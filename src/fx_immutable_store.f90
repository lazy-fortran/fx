module fx_immutable_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_size_t, &
        c_null_char, c_ptr, c_associated
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_path, only: path_normalize
    use fx_hash, only: sha256_init, sha256_update, sha256_final, sha256_state_t
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_close, owned_begin_path, owned_dispose, owned_materialize_blob, &
        owned_begin_path_ephemeral, owned_materialize_blob_ephemeral, &
        owned_file_info, owned_fd, owned_hash_fd, &
        owned_copy_source, owned_publish, owned_reject
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_INVALID, IMMUTABLE_MISSING, IMMUTABLE_CORRUPT, &
        IMMUTABLE_UNSUPPORTED
    use fx_immutable_manifest, only: immutable_id_valid, &
        immutable_tree_entry_t, IMMUTABLE_BLOB, IMMUTABLE_TREE
    implicit none
    private

    integer, parameter, public :: IMMUTABLE_MATERIALIZE_AUTO = 0
    integer, parameter, public :: IMMUTABLE_MATERIALIZE_COPY = 1
    integer, parameter, public :: IMMUTABLE_MATERIALIZE_CLONE = 2
    integer, parameter :: PATH_LIMIT = 4096
    integer, parameter :: HASH_LEN = 64
    integer, parameter :: HASH_BLOCK = 65536
    integer, parameter :: DEFAULT_DIR_MODE = 493

    type, public :: immutable_store_t
        character(len=:), allocatable :: root_dir
        character(len=:), allocatable :: writer_start
        logical :: initialized = .false.
    end type immutable_store_t

    type, public :: immutable_lease_t
        character(len=:), allocatable :: owner
        character(len=:), allocatable :: owner_start
        character(len=:), allocatable :: token
        character(len=:), allocatable :: store_root
        character(len=:), allocatable :: protected_kinds(:)
        character(len=:), allocatable :: protected_ids(:)
        logical :: active = .false.
    end type immutable_lease_t


    interface
        integer(c_int) function c_lease_update(root, operation, owner, start, &
                reason, token, kind, object_id, roots, out_token, &
                out_capacity, epoch) bind(C, name='fx_immutable_lease_update')
            import :: c_char, c_int, c_long_long, c_size_t
            character(kind=c_char), intent(in) :: root(*), owner(*), start(*)
            integer(c_int), value :: operation
            character(kind=c_char), intent(in) :: reason(*), token(*)
            character(kind=c_char), intent(in) :: kind(*), object_id(*), roots(*)
            character(kind=c_char), intent(out) :: out_token(*)
            integer(c_size_t), value :: out_capacity
            integer(c_long_long), intent(out) :: epoch
        end function c_lease_update
        integer(c_int) function c_mkdirs(path) &
                bind(C, name='fx_immutable_mkdirs_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdirs
        integer(c_int) function c_resolve_root(path, resolved, capacity) &
                bind(C, name='fx_immutable_resolve_root')
            import :: c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: resolved(*)
            integer(c_size_t), value :: capacity
        end function c_resolve_root
        integer(c_int) function c_getpid() bind(C, name='getpid')
            import :: c_int
        end function c_getpid

    end interface

    public :: immutable_store_init, immutable_store_blob_path
    public :: immutable_store_tree_path, immutable_store_put_blob
    public :: immutable_store_verify_blob
    public :: immutable_store_materialize_blob
    public :: immutable_store_materialize_blob_ephemeral
    public :: immutable_store_root_set
    public :: immutable_store_reason_release
    public :: immutable_store_read_lease_acquire
    public :: immutable_store_graph_read_lease_acquire
    public :: immutable_store_publication_lease_acquire
    public :: immutable_store_publication_commit
    public :: immutable_store_lease_release
    public :: immutable_store_file_info
    public :: immutable_store_hash_file
    public :: IMMUTABLE_OK, IMMUTABLE_IO_ERROR, IMMUTABLE_INVALID
    public :: IMMUTABLE_MISSING, IMMUTABLE_CORRUPT, IMMUTABLE_UNSUPPORTED
    public :: immutable_tree_entry_t, IMMUTABLE_BLOB, IMMUTABLE_TREE

contains

    subroutine immutable_store_init(store, root_dir, ierr)
        type(immutable_store_t), intent(out) :: store
        character(len=*), intent(in) :: root_dir
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_root(:)
        character(kind=c_char) :: c_resolved(PATH_LIMIT)
        character(len=:), allocatable :: clean
        integer(int64) :: start_tick
        integer(c_int) :: pid
        character(len=64) :: identity

        ierr = IMMUTABLE_INVALID
        clean = trim(root_dir)
        if (len(clean) == 0 .or. len(clean) >= PATH_LIMIT) return
        call to_c_text(clean, c_root)
        if (c_resolve_root(c_root, c_resolved, int(PATH_LIMIT, c_size_t)) /= &
                0_c_int) then
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        clean = path_normalize(from_c_text(c_resolved))
        if (len(clean) == 0 .or. len(clean) >= PATH_LIMIT) return
        call to_c_text(clean, c_root)
        if (c_mkdirs(c_root) /= 0_c_int) then
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        store%root_dir = clean
        pid = c_getpid()
        call system_clock(start_tick)
        write(identity, '(A,I0,A,I0)') 'p', pid, 'c', start_tick
        store%writer_start = trim(identity)
        store%initialized = .true.
        ierr = IMMUTABLE_OK
    end subroutine immutable_store_init

    function immutable_store_blob_path(store, object_id) result(path)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id
        character(len=:), allocatable :: path

        path = object_path(store, 'blobs', object_id)
    end function immutable_store_blob_path

    function immutable_store_tree_path(store, object_id) result(path)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id
        character(len=:), allocatable :: path

        path = object_path(store, 'trees', object_id)
    end function immutable_store_tree_path

    !! The optional publication lease must cover this blob and remain active
    !! until the caller commits or releases that lease.
    subroutine immutable_store_put_blob(store, source_path, object_id, ierr, &
            protected_by)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: source_path
        character(len=HASH_LEN), intent(out) :: object_id
        integer, intent(out) :: ierr
        type(immutable_lease_t), intent(in), optional :: protected_by
        integer :: status
        type(immutable_lease_t) :: publication
        character(len=4) :: kinds(1) = ['blob']
        character(len=HASH_LEN) :: ids(1)

        object_id = ''
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        call immutable_store_hash_file(source_path, object_id, ierr)
        if (ierr /= IMMUTABLE_OK) return
        if (present(protected_by)) then
            if (.not. lease_covers_blob(store, protected_by, object_id)) then
                ierr = IMMUTABLE_INVALID
                return
            end if
            call immutable_store_verify_blob(store, object_id, status)
            if (status == IMMUTABLE_OK) then
                ierr = IMMUTABLE_OK
                return
            end if
            if (status /= IMMUTABLE_MISSING) then
                ierr = status
                return
            end if
            call publish_blob_capture(store, source_path, object_id, ierr)
            return
        end if

        ids(1) = object_id
        call immutable_store_publication_lease_acquire(store, 'fx-publisher', &
            store%writer_start, 'blob', kinds, ids, publication, status)
        if (status /= IMMUTABLE_OK) then
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        call immutable_store_verify_blob(store, object_id, status)
        if (status == IMMUTABLE_OK) then
            call immutable_store_lease_release(store, publication, ierr)
            return
        end if
        if (status /= IMMUTABLE_MISSING) then
            call immutable_store_lease_release(store, publication, ierr)
            ierr = status
            return
        end if
        call publish_blob_capture(store, source_path, object_id, ierr)
        call immutable_store_lease_release(store, publication, status)
        if (ierr == IMMUTABLE_OK .and. status /= IMMUTABLE_OK) &
            ierr = IMMUTABLE_IO_ERROR
    end subroutine immutable_store_put_blob

    subroutine publish_blob_capture(store, source, id, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: source, id
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: actual
        type(c_ptr) :: capture
        integer(c_int) :: status, cleanup

        ierr = IMMUTABLE_IO_ERROR
        capture = owned_begin_path(immutable_store_blob_path(store, id)//c_null_char, &
            0_c_int)
        if (.not. c_associated(capture)) return
        status = owned_copy_source(capture, trim(source)//c_null_char)
        if (status == 0_c_int) then
            call owned_hash_fd(owned_fd(capture), actual, ierr)
            if (ierr == IMMUTABLE_OK) then
                ierr = IMMUTABLE_INVALID
                if (actual == id) then
                    status = owned_publish(capture)
                    ierr = IMMUTABLE_IO_ERROR
                    if (status == 2_c_int) ierr = IMMUTABLE_UNSUPPORTED
                    if (status == 0_c_int .or. status == 1_c_int) then
                        call immutable_store_verify_blob(store, id, ierr)
                    end if
                end if
            end if
        end if
        if (ierr /= IMMUTABLE_OK) cleanup = owned_reject(capture)
        call owned_dispose(capture)
    end subroutine publish_blob_capture

    subroutine immutable_store_verify_blob(store, object_id, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id
        integer, intent(out) :: ierr
        integer(c_int) :: root, fd, cleanup

        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        if (.not. immutable_id_valid(object_id)) return
        root = owned_open_store(store%root_dir//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (root < 0) return
        call owned_open_verified(root, 1_c_int, object_id, fd, ierr)
        if (fd >= 0) cleanup = owned_close(fd)
        cleanup = owned_close(root)
    end subroutine immutable_store_verify_blob

    subroutine immutable_store_materialize_blob(store, object_id, dest_path, &
            mode, strategy, used_clone, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id, dest_path
        integer, intent(in) :: mode, strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        call materialize_blob(store, object_id, dest_path, mode, strategy, &
            .false., used_clone, ierr)
    end subroutine immutable_store_materialize_blob

    subroutine immutable_store_materialize_blob_ephemeral(store, object_id, &
            dest_path, mode, strategy, used_clone, ierr)
        !! Materialize verified CAS bytes for rebuildable outputs without fsync.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id, dest_path
        integer, intent(in) :: mode, strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr

        call materialize_blob(store, object_id, dest_path, mode, strategy, &
            .true., used_clone, ierr)
    end subroutine immutable_store_materialize_blob_ephemeral

    subroutine materialize_blob(store, object_id, dest_path, mode, strategy, &
            ephemeral, used_clone, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id, dest_path
        integer, intent(in) :: mode, strategy
        logical, intent(in) :: ephemeral
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        integer(c_int) :: root, cleanup
        type(c_ptr) :: transaction

        used_clone = .false.
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        if (.not. immutable_id_valid(object_id)) return
        if (mode < 0 .or. mode > 511 .or. strategy < &
            IMMUTABLE_MATERIALIZE_AUTO .or. strategy > &
            IMMUTABLE_MATERIALIZE_CLONE) return
        root = owned_open_store(store%root_dir//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (root < 0) return
        if (ephemeral) then
            transaction = owned_begin_path_ephemeral( &
                trim(dest_path)//c_null_char, 0_c_int)
        else
            transaction = owned_begin_path(trim(dest_path)//c_null_char, 0_c_int)
        end if
        ierr = IMMUTABLE_IO_ERROR
        if (c_associated(transaction)) then
            if (ephemeral) then
                call owned_materialize_blob_ephemeral(transaction, root, &
                    object_id, mode, strategy, used_clone, ierr)
            else
                call owned_materialize_blob(transaction, root, object_id, &
                    mode, strategy, used_clone, ierr)
            end if
            call owned_dispose(transaction)
        end if
        cleanup = owned_close(root)
    end subroutine materialize_blob

    subroutine immutable_store_root_set(store, owner, owner_start, reason, &
            kinds, ids, ierr, epoch)
        !! Replace one owner's reason roots atomically; repeating equal roots is a no-op.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: owner, owner_start, reason, kinds(:), ids(:)
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        integer(c_long_long) :: current_epoch
        character(kind=c_char) :: ignored(256)
        character(len=:), allocatable :: roots

        current_epoch = 0_c_long_long
        call build_root_rows(kinds, ids, roots, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call lease_update(store, 1, owner, owner_start, reason, '', '', '', &
            roots, ignored, current_epoch, ierr)
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine immutable_store_root_set

    subroutine immutable_store_reason_release(store, owner, owner_start, reason, &
            ierr, epoch)
        !! Release only roots with this exact owner incarnation and reason.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: owner, owner_start, reason
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        integer(c_long_long) :: current_epoch
        character(kind=c_char) :: ignored(1)

        current_epoch = 0_c_long_long
        call lease_update(store, 2, owner, owner_start, reason, '', '', '', '', &
            ignored, current_epoch, ierr)
        if (ierr == IMMUTABLE_MISSING) ierr = IMMUTABLE_OK
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine immutable_store_reason_release

    subroutine immutable_store_read_lease_acquire(store, owner, owner_start, &
            reason, kind, object_id, lease, ierr, epoch)
        !! Acquire before opening/materializing an object and release this token later.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: owner, owner_start, reason, kind, object_id
        type(immutable_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch

        call acquire_lease(store, 3, owner, owner_start, reason, kind, object_id, &
            lease, ierr, epoch)
    end subroutine immutable_store_read_lease_acquire

    subroutine immutable_store_graph_read_lease_acquire(store, owner, &
            owner_start, reason, lease, ierr, epoch)
        !! Lease every published or pending root owned by this graph identity.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: owner, owner_start, reason
        type(immutable_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch

        call acquire_lease(store, 7, owner, owner_start, reason, '', '', &
            lease, ierr, epoch)
    end subroutine immutable_store_graph_read_lease_acquire

    subroutine immutable_store_publication_lease_acquire(store, owner, &
            owner_start, reason, kinds, ids, lease, ierr, epoch)
        !! Protect anticipated graph roots while payload bytes are copied outside the lock.
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: owner, owner_start, reason
        character(len=*), intent(in) :: kinds(:), ids(:)
        type(immutable_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        character(kind=c_char) :: ignored(256)
        integer(c_long_long) :: current_epoch
        character(len=:), allocatable :: roots

        lease%active = .false.
        current_epoch = 0_c_long_long
        call build_root_rows(kinds, ids, roots, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call lease_update(store, 5, owner, owner_start, reason, '', '', '', &
            roots, ignored, current_epoch, ierr)
        if (ierr == IMMUTABLE_OK) then
            lease%owner = trim(owner)
            lease%owner_start = trim(owner_start)
            lease%token = from_c_text(ignored)
            lease%store_root = store%root_dir
            lease%protected_kinds = kinds
            lease%protected_ids = ids
            lease%active = .true.
        end if
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine immutable_store_publication_lease_acquire

    subroutine immutable_store_publication_commit(store, lease, reason, kinds, &
            ids, ierr, epoch)
        !! Atomically replace this publication lease with durable owner/reason roots.
        type(immutable_store_t), intent(in) :: store
        type(immutable_lease_t), intent(inout) :: lease
        character(len=*), intent(in) :: reason, kinds(:), ids(:)
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        integer(c_long_long) :: current_epoch
        character(kind=c_char) :: ignored(1)
        character(len=:), allocatable :: roots

        ierr = IMMUTABLE_INVALID
        if (.not. lease%active) return
        call build_root_rows(kinds, ids, roots, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call lease_update(store, 6, lease%owner, lease%owner_start, reason, &
            lease%token, '', '', roots, ignored, current_epoch, ierr)
        if (ierr == IMMUTABLE_OK) then
            lease%active = .false.
            call clear_lease_scope(lease)
        end if
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine immutable_store_publication_commit

    subroutine immutable_store_lease_release(store, lease, ierr, epoch)
        type(immutable_store_t), intent(in) :: store
        type(immutable_lease_t), intent(inout) :: lease
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        integer(c_long_long) :: current_epoch
        character(kind=c_char) :: ignored(1)

        ierr = IMMUTABLE_INVALID
        if (.not. lease%active) return
        call lease_update(store, 4, lease%owner, lease%owner_start, '', &
            lease%token, '', '', '', ignored, current_epoch, ierr)
        if (ierr == IMMUTABLE_OK) then
            lease%active = .false.
            call clear_lease_scope(lease)
        end if
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine immutable_store_lease_release

    logical function lease_covers_blob(store, lease, object_id) result(covers)
        type(immutable_store_t), intent(in) :: store
        type(immutable_lease_t), intent(in) :: lease
        character(len=*), intent(in) :: object_id
        integer :: i

        covers = .false.
        if (.not. lease%active) return
        if (.not. allocated(lease%store_root)) return
        if (trim(lease%store_root) /= trim(store%root_dir)) return
        if (.not. allocated(lease%protected_kinds)) return
        if (.not. allocated(lease%protected_ids)) return
        if (size(lease%protected_kinds) /= size(lease%protected_ids)) return
        do i = 1, size(lease%protected_ids)
            if (trim(lease%protected_kinds(i)) /= 'blob') cycle
            if (trim(lease%protected_ids(i)) /= trim(object_id)) cycle
            covers = .true.
            return
        end do
    end function lease_covers_blob

    subroutine clear_lease_scope(lease)
        type(immutable_lease_t), intent(inout) :: lease

        if (allocated(lease%store_root)) deallocate(lease%store_root)
        if (allocated(lease%protected_kinds)) deallocate(lease%protected_kinds)
        if (allocated(lease%protected_ids)) deallocate(lease%protected_ids)
    end subroutine clear_lease_scope

    subroutine acquire_lease(store, operation, owner, owner_start, reason, &
            kind, object_id, lease, ierr, epoch)
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: operation
        character(len=*), intent(in) :: owner, owner_start, reason, kind, object_id
        type(immutable_lease_t), intent(out) :: lease
        integer, intent(out) :: ierr
        integer(int64), intent(out), optional :: epoch
        integer(c_long_long) :: current_epoch
        character(kind=c_char) :: token(256)

        lease%active = .false.
        call lease_update(store, operation, owner, owner_start, reason, '', kind, &
            object_id, '', token, current_epoch, ierr)
        if (ierr == IMMUTABLE_OK) then
            lease%owner = trim(owner)
            lease%owner_start = trim(owner_start)
            lease%token = from_c_text(token)
            lease%active = .true.
        end if
        if (present(epoch)) epoch = int(current_epoch, int64)
    end subroutine acquire_lease

    subroutine lease_update(store, operation, owner, owner_start, reason, token, &
            kind, object_id, roots, out_token, epoch, ierr)
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: operation
        character(len=*), intent(in) :: owner, owner_start, reason, token
        character(len=*), intent(in) :: kind, object_id, roots
        character(kind=c_char), intent(out) :: out_token(*)
        integer(c_long_long), intent(out) :: epoch
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_root(:), c_owner(:), c_start(:)
        character(kind=c_char), allocatable :: c_reason(:), c_token(:), c_kind(:)
        character(kind=c_char), allocatable :: c_id(:), c_roots(:)
        integer(c_int) :: status

        ierr = IMMUTABLE_INVALID
        epoch = 0_c_long_long
        if (.not. store%initialized) return
        call to_c_text(store%root_dir, c_root)
        call to_c_text(owner, c_owner)
        call to_c_text(owner_start, c_start)
        call to_c_text(reason, c_reason)
        call to_c_text(token, c_token)
        call to_c_text(kind, c_kind)
        call to_c_text(object_id, c_id)
        call to_c_text(roots, c_roots)
        status = c_lease_update(c_root, int(operation, c_int), c_owner, c_start, &
            c_reason, c_token, c_kind, c_id, c_roots, out_token, &
            int(256, c_size_t), epoch)
        ierr = IMMUTABLE_IO_ERROR
        if (status == 0_c_int) ierr = IMMUTABLE_OK
        if (status == 1_c_int) ierr = IMMUTABLE_MISSING
    end subroutine lease_update

    subroutine build_root_rows(kinds, ids, roots, ierr)
        character(len=*), intent(in) :: kinds(:), ids(:)
        character(len=:), allocatable, intent(out) :: roots
        integer, intent(out) :: ierr
        integer :: i

        roots = ''
        ierr = IMMUTABLE_INVALID
        if (size(kinds) /= size(ids)) return
        do i = 1, size(kinds)
            if (len_trim(kinds(i)) == 0) return
            if (len_trim(kinds(i)) > 255) return
            if (.not. immutable_id_valid(ids(i))) return
            roots = roots//trim(kinds(i))//':'//trim(ids(i))//achar(10)
        end do
        if (len(roots) == 0) return
        ierr = IMMUTABLE_OK
    end subroutine build_root_rows

    function from_c_text(bytes) result(text)
        character(kind=c_char), intent(in) :: bytes(:)
        character(len=:), allocatable :: text
        integer :: i, n

        n = 0
        do i = 1, size(bytes)
            if (bytes(i) == c_null_char) exit
            n = n + 1
        end do
        allocate(character(len=n) :: text)
        do i = 1, n
            text(i:i) = bytes(i)
        end do
    end function from_c_text

    subroutine immutable_store_file_info(store, object_id, size_bytes, &
            mtime_ns, inode, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: object_id
        integer(int64), intent(out) :: size_bytes, mtime_ns, inode
        integer, intent(out) :: ierr
        integer(c_int) :: root, fd, cleanup, status
        integer(c_long_long) :: c_size, c_mtime, c_inode

        size_bytes = 0_int64
        mtime_ns = 0_int64
        inode = 0_int64
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        root = owned_open_store(store%root_dir//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (root < 0) return
        call owned_open_verified(root, 1_c_int, object_id, fd, ierr)
        if (ierr == IMMUTABLE_OK) then
            status = owned_file_info(fd, c_size, c_mtime, c_inode)
            ierr = IMMUTABLE_IO_ERROR
            if (status == 0_c_int) then
                size_bytes = int(c_size, int64)
                mtime_ns = int(c_mtime, int64)
                inode = int(c_inode, int64)
                ierr = IMMUTABLE_OK
            end if
        end if
        if (fd >= 0) cleanup = owned_close(fd)
        cleanup = owned_close(root)
    end subroutine immutable_store_file_info

    function object_path(store, class_name, object_id) result(path)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: class_name, object_id
        character(len=:), allocatable :: path

        path = ''
        if (.not. store%initialized .or. .not. immutable_id_valid(object_id)) return
        path = trim(store%root_dir)//'/'//trim(class_name)//'/sha256/'// &
            object_id(1:2)//'/'//trim(object_id)
    end function object_path

    subroutine immutable_store_hash_file(path, digest, ierr)
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: digest
        integer, intent(out) :: ierr
        type(sha256_state_t) :: state
        character(len=1), allocatable :: bytes(:)
        integer(int64) :: remaining
        integer :: unit, ios, chunk

        digest = ''
        inquire(file=path, size=remaining, iostat=ios)
        ierr = IMMUTABLE_IO_ERROR
        if (ios /= 0 .or. remaining < 0_int64) return
        allocate(bytes(HASH_BLOCK))
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=ios)
        if (ios /= 0) return
        call sha256_init(state)
        do while (remaining > 0_int64)
            chunk = int(min(int(HASH_BLOCK, int64), remaining))
            read(unit, iostat=ios) bytes(1:chunk)
            if (ios /= 0) exit
            call sha256_update(state, bytes, chunk)
            remaining = remaining - int(chunk, int64)
        end do
        close(unit)
        if (ios /= 0) return
        digest = sha256_final(state)
        ierr = IMMUTABLE_OK
    end subroutine immutable_store_hash_file


    subroutine to_c_text(text, c_string)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable, intent(out) :: c_string(:)
        integer :: i, n
        n = len_trim(text)
        allocate(c_string(n + 1))
        do i = 1, n
            c_string(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_string(n + 1) = c_null_char
    end subroutine to_c_text

end module fx_immutable_store
