module fx_immutable_tree
    use, intrinsic :: iso_c_binding, only: c_int, c_null_char, &
        c_ptr, c_associated
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        IMMUTABLE_IO_ERROR, IMMUTABLE_INVALID, IMMUTABLE_MISSING, &
        IMMUTABLE_CORRUPT, IMMUTABLE_UNSUPPORTED, &
        IMMUTABLE_MATERIALIZE_AUTO, IMMUTABLE_MATERIALIZE_COPY, &
        IMMUTABLE_MATERIALIZE_CLONE, &
        immutable_store_tree_path, immutable_store_verify_blob
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_read_manifest, owned_begin_path, owned_begin_at, owned_fd, &
        owned_pause, owned_materialize_blob, owned_finish, owned_dispose, owned_close, &
        owned_write, owned_hash_fd, owned_publish, owned_reject
    use fx_immutable_manifest, only: immutable_tree_entry_t, IMMUTABLE_BLOB, &
        IMMUTABLE_TREE, immutable_entries_canonical, &
        immutable_manifest_serialize, immutable_manifest_parse, &
        immutable_id_valid
    implicit none
    private

    integer, parameter :: PATH_LIMIT = 4096
    integer, parameter :: HASH_LEN = 64
    integer, parameter :: DEFAULT_DIR_MODE = 493

    public :: immutable_store_put_tree, immutable_store_verify_tree
    public :: immutable_store_materialize_tree

contains

    subroutine immutable_store_put_tree(store, entries, tree_id, ierr)
        type(immutable_store_t), intent(in) :: store
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=HASH_LEN), intent(out) :: tree_id
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: sorted(:)
        character(len=:), allocatable :: manifest
        integer :: i, status

        tree_id = ''
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        call immutable_entries_canonical(entries, sorted, ierr)
        if (ierr /= IMMUTABLE_OK) return
        do i = 1, size(sorted)
            if (sorted(i)%kind == IMMUTABLE_BLOB) then
                call immutable_store_verify_blob(store, sorted(i)%object_id, ierr)
            else
                call verify_tree_depth(store, sorted(i)%object_id, 0, ierr)
            end if
            if (ierr /= IMMUTABLE_OK) return
        end do
        manifest = immutable_manifest_serialize(sorted)
        tree_id = sha256_string(manifest)
        call immutable_store_verify_tree(store, tree_id, status)
        if (status == IMMUTABLE_OK) then
            ierr = IMMUTABLE_OK
            return
        else if (status /= IMMUTABLE_MISSING) then
            ierr = status
            return
        end if
        call publish_tree_capture(store, tree_id, manifest, ierr)
    end subroutine immutable_store_put_tree

    subroutine publish_tree_capture(store, id, manifest, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: id, manifest
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: actual
        type(c_ptr) :: capture
        integer(c_int) :: status, cleanup

        ierr = IMMUTABLE_IO_ERROR
        capture = owned_begin_path(immutable_store_tree_path(store, id)//c_null_char, &
            0_c_int)
        if (.not. c_associated(capture)) return
        call owned_pause(capture, 5_c_int)
        status = owned_write(capture, manifest, int(len(manifest), c_int))
        if (status == 0_c_int) then
            call owned_hash_fd(owned_fd(capture), actual, ierr)
            if (ierr == IMMUTABLE_OK) then
                ierr = IMMUTABLE_CORRUPT
                if (actual == id) then
                    status = owned_publish(capture)
                    ierr = IMMUTABLE_IO_ERROR
                    if (status == 0_c_int .or. status == 1_c_int) then
                        call immutable_store_verify_tree(store, id, ierr)
                        if (ierr /= IMMUTABLE_OK) cleanup = owned_reject(capture)
                    end if
                end if
            end if
        end if
        call owned_dispose(capture)
    end subroutine publish_tree_capture

    subroutine immutable_store_verify_tree(store, tree_id, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id
        integer, intent(out) :: ierr

        call verify_tree_depth(store, tree_id, 0, ierr)
    end subroutine immutable_store_verify_tree

    subroutine verify_tree_depth(store, tree_id, depth, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id
        integer, intent(in) :: depth
        integer, intent(out) :: ierr
        integer(c_int) :: root, cleanup

        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        root = owned_open_store(store%root_dir//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (root < 0) return
        call verify_owned_depth(root, tree_id, depth, ierr)
        cleanup = owned_close(root)
    end subroutine verify_tree_depth

    recursive subroutine verify_owned_depth(root, tree_id, depth, ierr)
        integer(c_int), intent(in) :: root
        character(len=*), intent(in) :: tree_id
        integer, intent(in) :: depth
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: raw
        integer :: i
        integer(c_int) :: fd, cleanup

        ierr = IMMUTABLE_INVALID
        if (depth > 128) return
        call owned_read_manifest(root, tree_id, raw, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call immutable_manifest_parse(raw, entries, ierr)
        if (ierr /= IMMUTABLE_OK) return
        do i = 1, size(entries)
            if (entries(i)%kind == IMMUTABLE_BLOB) then
                call owned_open_verified(root, 1_c_int, entries(i)%object_id, fd, ierr)
                if (fd >= 0) cleanup = owned_close(fd)
            else
                call verify_owned_depth(root, entries(i)%object_id, depth + 1, ierr)
            end if
            if (ierr /= IMMUTABLE_OK) return
        end do
    end subroutine verify_owned_depth

    subroutine immutable_store_materialize_tree(store, tree_id, dest_path, &
            strategy, used_clone, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id, dest_path
        integer, intent(in) :: strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        integer(c_int) :: root, cleanup
        type(c_ptr) :: transaction

        used_clone = .false.
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        if (.not. immutable_id_valid(tree_id)) return
        if (strategy < IMMUTABLE_MATERIALIZE_AUTO .or. &
            strategy > IMMUTABLE_MATERIALIZE_CLONE) return
        root = owned_open_store(store%root_dir//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (root < 0) return
        transaction = owned_begin_path(trim(dest_path)//c_null_char, 1_c_int)
        ierr = IMMUTABLE_IO_ERROR
        if (c_associated(transaction)) then
            call owned_pause(transaction, 4_c_int)
            call materialize_tree_contents(root, tree_id, transaction, &
                DEFAULT_DIR_MODE, strategy, 0, used_clone, ierr)
            call owned_dispose(transaction)
        end if
        cleanup = owned_close(root)
    end subroutine immutable_store_materialize_tree

    recursive subroutine materialize_tree_contents(root, tree_id, transaction, &
            mode, strategy, depth, used_clone, ierr)
        integer(c_int), intent(in) :: root
        character(len=*), intent(in) :: tree_id
        type(c_ptr), intent(in) :: transaction
        integer, intent(in) :: mode, strategy, depth
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: manifest
        type(c_ptr) :: child
        logical :: child_clone
        integer(c_int) :: status, parent
        integer :: i, child_kind

        used_clone = .false.
        ierr = IMMUTABLE_INVALID
        if (depth > 128) return
        call owned_read_manifest(root, tree_id, manifest, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call owned_pause(transaction, 3_c_int)
        call immutable_manifest_parse(manifest, entries, ierr)
        if (ierr /= IMMUTABLE_OK) return
        parent = owned_fd(transaction)
        do i = 1, size(entries)
            child_kind = 0
            if (entries(i)%kind == IMMUTABLE_TREE) child_kind = 1
            child = owned_begin_at(parent, entries(i)%path//c_null_char, &
                int(child_kind, c_int))
            ierr = IMMUTABLE_IO_ERROR
            if (.not. c_associated(child)) return
            if (child_kind == 0) then
                call owned_materialize_blob(child, root, entries(i)%object_id, &
                    entries(i)%mode, strategy, child_clone, ierr)
            else
                call materialize_tree_contents(root, entries(i)%object_id, child, &
                    entries(i)%mode, strategy, depth + 1, child_clone, ierr)
            end if
            call owned_dispose(child)
            if (ierr /= IMMUTABLE_OK) return
            used_clone = used_clone .or. child_clone
        end do
        status = owned_finish(transaction, int(mode, c_int))
        ierr = IMMUTABLE_IO_ERROR
        if (status == 0_c_int) ierr = IMMUTABLE_OK
        if (status == 2_c_int) ierr = IMMUTABLE_UNSUPPORTED
    end subroutine materialize_tree_contents

end module fx_immutable_tree
