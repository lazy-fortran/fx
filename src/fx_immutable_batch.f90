module fx_immutable_batch
    !! Batched blob publication with one durability barrier per batch.
    !!
    !! Each input is streamed once from its descriptor into owned staging while
    !! it is hashed; there is no temporary payload copy and no read-back. A
    !! digest that differs from the expected id, or an input whose identity,
    !! size or times change during the copy, is rejected as IMMUTABLE_INVALID.
    !!
    !! Durability: staged payloads are not synced individually. On Linux,
    !! immutable_batch_commit syncs the store file system once, publishes every
    !! payload under its object name and syncs once more, so a batch costs two
    !! barriers whatever its size. A published blob name therefore never refers
    !! to unsynced bytes, and callers must publish trees or roots that reference
    !! batch objects only after a successful commit. A crash before commit
    !! leaves only staging directories, which the stale-temp collector removes.
    !! Platforms without syncfs publish each payload through the synchronized
    !! single-object path instead (same invariant, one sync set per blob).
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_ptr, &
        c_null_ptr, c_null_char, c_associated
    use fx_hash, only: sha256_init, sha256_update, sha256_final, sha256_state_t
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_INVALID, IMMUTABLE_MISSING
    use fx_immutable_manifest, only: immutable_id_valid
    use fx_immutable_owned, only: owned_close
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_store_blob_path, immutable_store_verify_blob, &
        immutable_store_freshen, immutable_lease_covers_blob
    implicit none
    private

    integer, parameter :: HASH_LEN = 64
    integer, parameter :: COPY_BLOCK = 1048576

    type, public :: immutable_batch_t
        private
        type(c_ptr) :: handle = c_null_ptr
        !! Every id staged, retained or verified in this batch.
        character(len=HASH_LEN), allocatable :: ids(:)
        integer, allocatable :: slots(:)
        !! Ids of deferred payloads in the native staging order.
        character(len=HASH_LEN), allocatable :: staged_order(:)
        integer :: count = 0
        integer :: n_staged = 0
        logical :: committed = .false.
    end type immutable_batch_t

    public :: immutable_batch_begin, immutable_batch_put_fd
    public :: immutable_batch_put_file, immutable_batch_commit
    public :: immutable_batch_end, immutable_batch_covers

    interface
        function c_begin(root) bind(C, name='fx_batch_begin') result(handle)
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: root(*)
            type(c_ptr) :: handle
        end function c_begin
        integer(c_int) function c_count(handle) bind(C, name='fx_batch_count')
            import :: c_int, c_ptr
            type(c_ptr), value :: handle
        end function c_count
        integer(c_int) function c_open_file(path) bind(C, name='fx_batch_open_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_open_file
        function c_stage(handle, blob_path, input) bind(C, name='fx_batch_stage') &
                result(capture)
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: blob_path(*)
            integer(c_int), value :: input
            type(c_ptr) :: capture
        end function c_stage
        integer(c_int) function c_write(capture, bytes, count) &
                bind(C, name='fx_batch_write')
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: capture
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), value :: count
        end function c_write
        integer(c_int) function c_stage_end(handle, capture, input, id, ok) &
                bind(C, name='fx_batch_stage_end')
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: handle, capture
            integer(c_int), value :: input, ok
            character(kind=c_char), intent(in) :: id(*)
        end function c_stage_end
        integer(c_int) function c_commit(handle, retained) &
                bind(C, name='fx_batch_commit')
            import :: c_int, c_ptr
            type(c_ptr), value :: handle
            integer(c_int), intent(out) :: retained(*)
        end function c_commit
        subroutine c_end(handle) bind(C, name='fx_batch_end')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine c_end
        integer(c_long_long) function c_read(fd, offset, bytes, count) &
                bind(C, name='fx_owned_read')
            import :: c_int, c_long_long, c_char
            integer(c_int), value :: fd, count
            integer(c_long_long), value :: offset
            character(kind=c_char), intent(out) :: bytes(*)
        end function c_read
    end interface

contains

    subroutine immutable_batch_begin(store, batch, ierr)
        type(immutable_store_t), intent(in) :: store
        type(immutable_batch_t), intent(inout) :: batch
        integer, intent(out) :: ierr

        call immutable_batch_end(batch)
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        batch%handle = c_begin(store%root_dir//c_null_char)
        ierr = IMMUTABLE_IO_ERROR
        if (.not. c_associated(batch%handle)) return
        allocate (batch%ids(64), batch%slots(128))
        batch%slots = 0
        ierr = IMMUTABLE_OK
    end subroutine immutable_batch_begin

    !! Add the bytes readable from fd under object_id. An object the store
    !! already holds is verified and freshened without reading fd. The
    !! optional publication lease must cover object_id, as for put_blob.
    subroutine immutable_batch_put_fd(store, batch, fd, object_id, ierr, &
            protected_by)
        type(immutable_store_t), intent(in) :: store
        type(immutable_batch_t), intent(inout) :: batch
        integer(c_int), intent(in) :: fd
        character(len=*), intent(in) :: object_id
        integer, intent(out) :: ierr
        type(immutable_lease_t), intent(in), optional :: protected_by
        integer :: status

        ierr = IMMUTABLE_INVALID
        if (.not. c_associated(batch%handle) .or. batch%committed) return
        if (.not. immutable_id_valid(object_id)) return
        if (present(protected_by)) then
            if (.not. immutable_lease_covers_blob(store, protected_by, &
                object_id)) return
        end if
        if (find_slot(batch, object_id) > 0) then
            ierr = IMMUTABLE_OK
            return
        end if
        call immutable_store_verify_blob(store, object_id, status)
        if (status == IMMUTABLE_OK) then
            call immutable_store_freshen(store, 1, object_id, status)
            if (status == IMMUTABLE_OK) call remember(batch, object_id)
            if (status /= IMMUTABLE_MISSING) then
                ierr = status
                return
            end if
        end if
        if (status /= IMMUTABLE_MISSING) then
            ierr = status
            return
        end if
        call stage_fd(store, batch, fd, object_id, ierr)
        if (ierr == IMMUTABLE_OK) call remember(batch, object_id)
    end subroutine immutable_batch_put_fd

    subroutine immutable_batch_put_file(store, batch, path, object_id, ierr, &
            protected_by)
        type(immutable_store_t), intent(in) :: store
        type(immutable_batch_t), intent(inout) :: batch
        character(len=*), intent(in) :: path, object_id
        integer, intent(out) :: ierr
        type(immutable_lease_t), intent(in), optional :: protected_by
        integer(c_int) :: fd, cleanup

        ierr = IMMUTABLE_IO_ERROR
        fd = c_open_file(trim(path)//c_null_char)
        if (fd < 0) return
        call immutable_batch_put_fd(store, batch, fd, object_id, ierr, protected_by)
        cleanup = owned_close(fd)
    end subroutine immutable_batch_put_file

    !! Publish all staged objects behind the batch durability barriers. Only
    !! after IMMUTABLE_OK may callers reference batch objects from a tree.
    subroutine immutable_batch_commit(store, batch, ierr)
        type(immutable_store_t), intent(in) :: store
        type(immutable_batch_t), intent(inout) :: batch
        integer, intent(out) :: ierr
        integer(c_int), allocatable :: retained(:)
        integer :: n, i, status

        ierr = IMMUTABLE_INVALID
        if (.not. c_associated(batch%handle) .or. batch%committed) return
        n = int(c_count(batch%handle))
        allocate (retained(max(1, n)))
        retained = 0_c_int
        ierr = IMMUTABLE_IO_ERROR
        if (c_commit(batch%handle, retained) /= 0_c_int) return
        ierr = IMMUTABLE_OK
        do i = 1, n
            if (retained(i) == 0_c_int) cycle
            ! An existing name was kept: its bytes were not written by us.
            call immutable_store_verify_blob(store, staged_id(batch, i), status)
            if (status /= IMMUTABLE_OK) ierr = status
        end do
        batch%committed = ierr == IMMUTABLE_OK
    end subroutine immutable_batch_commit

    !! Release the batch. Staged payloads of an uncommitted batch are removed.
    subroutine immutable_batch_end(batch)
        type(immutable_batch_t), intent(inout) :: batch

        if (c_associated(batch%handle)) call c_end(batch%handle)
        batch%handle = c_null_ptr
        if (allocated(batch%ids)) deallocate (batch%ids)
        if (allocated(batch%slots)) deallocate (batch%slots)
        if (allocated(batch%staged_order)) deallocate (batch%staged_order)
        batch%count = 0
        batch%n_staged = 0
        batch%committed = .false.
    end subroutine immutable_batch_end

    !! True when a successfully committed batch published or verified
    !! object_id, so that it is durable and needs no further verification.
    logical function immutable_batch_covers(batch, object_id) result(covers)
        type(immutable_batch_t), intent(in) :: batch
        character(len=*), intent(in) :: object_id

        covers = .false.
        if (.not. batch%committed .or. .not. allocated(batch%slots)) return
        covers = find_slot(batch, object_id) > 0
    end function immutable_batch_covers

    subroutine stage_fd(store, batch, fd, object_id, ierr)
        type(immutable_store_t), intent(in) :: store
        type(immutable_batch_t), intent(inout) :: batch
        integer(c_int), intent(in) :: fd
        character(len=*), intent(in) :: object_id
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: bytes(:)
        character(len=HASH_LEN) :: digest
        type(sha256_state_t) :: state
        type(c_ptr) :: capture
        integer(c_long_long) :: offset, n
        integer(c_int) :: rc, ok
        logical :: copied

        ierr = IMMUTABLE_IO_ERROR
        capture = c_stage(batch%handle, &
            immutable_store_blob_path(store, object_id)//c_null_char, fd)
        if (.not. c_associated(capture)) return
        allocate (bytes(COPY_BLOCK))
        call sha256_init(state)
        offset = 0_c_long_long
        copied = .true.
        do
            n = c_read(fd, offset, bytes, int(COPY_BLOCK, c_int))
            if (n == 0_c_long_long) exit
            if (n < 0_c_long_long) then
                copied = .false.
                exit
            end if
            call sha256_update(state, bytes, int(n))
            if (c_write(capture, bytes, int(n, c_int)) /= 0_c_int) then
                copied = .false.
                exit
            end if
            offset = offset + n
        end do
        digest = ''
        if (copied) digest = sha256_final(state)
        ok = 0_c_int
        if (copied .and. digest == object_id) ok = 1_c_int
        rc = c_stage_end(batch%handle, capture, fd, object_id//c_null_char, ok)
        if (rc == -3_c_int .or. (copied .and. digest /= object_id)) then
            ierr = IMMUTABLE_INVALID
            return
        end if
        if (rc == 1_c_int) then
            call immutable_store_verify_blob(store, object_id, ierr)
            return
        end if
        if (rc /= 0_c_int) return
        if (c_count(batch%handle) > batch%n_staged) call note_staged(batch, object_id)
        ierr = IMMUTABLE_OK
    end subroutine stage_fd

    subroutine note_staged(batch, object_id)
        type(immutable_batch_t), intent(inout) :: batch
        character(len=*), intent(in) :: object_id
        character(len=HASH_LEN), allocatable :: grown(:)

        if (.not. allocated(batch%staged_order)) allocate (batch%staged_order(64))
        if (batch%n_staged == size(batch%staged_order)) then
            allocate (grown(2 * size(batch%staged_order)))
            grown(:batch%n_staged) = batch%staged_order(:batch%n_staged)
            call move_alloc(grown, batch%staged_order)
        end if
        batch%n_staged = batch%n_staged + 1
        batch%staged_order(batch%n_staged) = object_id
    end subroutine note_staged

    function staged_id(batch, index) result(id)
        type(immutable_batch_t), intent(in) :: batch
        integer, intent(in) :: index
        character(len=HASH_LEN) :: id

        id = batch%staged_order(index)
    end function staged_id

    !! Open-addressed set of ids keyed by their leading hex digits.
    subroutine remember(batch, object_id)
        type(immutable_batch_t), intent(inout) :: batch
        character(len=*), intent(in) :: object_id
        character(len=HASH_LEN), allocatable :: grown(:)
        integer :: i

        if (find_slot(batch, object_id) > 0) return
        if (batch%count == size(batch%ids)) then
            allocate (grown(2 * size(batch%ids)))
            grown(:batch%count) = batch%ids(:batch%count)
            call move_alloc(grown, batch%ids)
        end if
        batch%count = batch%count + 1
        batch%ids(batch%count) = object_id
        if (2 * batch%count > size(batch%slots)) then
            deallocate (batch%slots)
            allocate (batch%slots(4 * size(batch%ids)))
            batch%slots = 0
            do i = 1, batch%count
                call insert_slot(batch, i)
            end do
        else
            call insert_slot(batch, batch%count)
        end if
    end subroutine remember

    subroutine insert_slot(batch, index)
        type(immutable_batch_t), intent(inout) :: batch
        integer, intent(in) :: index
        integer :: slot

        slot = home_slot(batch%ids(index), size(batch%slots))
        do while (batch%slots(slot) /= 0)
            slot = modulo(slot, size(batch%slots)) + 1
        end do
        batch%slots(slot) = index
    end subroutine insert_slot

    integer function find_slot(batch, object_id) result(found)
        type(immutable_batch_t), intent(in) :: batch
        character(len=*), intent(in) :: object_id
        integer :: slot

        found = 0
        if (.not. allocated(batch%slots)) return
        if (size(batch%slots) == 0) return
        slot = home_slot(object_id, size(batch%slots))
        do while (batch%slots(slot) /= 0)
            if (batch%ids(batch%slots(slot)) == object_id) then
                found = batch%slots(slot)
                return
            end if
            slot = modulo(slot, size(batch%slots)) + 1
        end do
    end function find_slot

    pure integer function home_slot(object_id, n) result(slot)
        character(len=*), intent(in) :: object_id
        integer, intent(in) :: n
        integer :: i, value, digit

        value = 0
        do i = 1, min(7, len(object_id))
            digit = index('0123456789abcdef', object_id(i:i)) - 1
            if (digit < 0) digit = 0
            value = 16 * value + digit
        end do
        slot = modulo(value, n) + 1
    end function home_slot

end module fx_immutable_batch
