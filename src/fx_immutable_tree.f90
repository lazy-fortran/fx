module fx_immutable_tree
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, &
        c_ptr, c_associated
    use fx_path, only: path_dirname
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        IMMUTABLE_IO_ERROR, IMMUTABLE_INVALID, IMMUTABLE_MISSING, &
        IMMUTABLE_CORRUPT, IMMUTABLE_UNSUPPORTED, &
        IMMUTABLE_MATERIALIZE_AUTO, IMMUTABLE_MATERIALIZE_COPY, &
        IMMUTABLE_MATERIALIZE_CLONE, &
        immutable_store_tree_path, immutable_store_verify_blob
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_read_manifest, owned_begin_path, owned_begin_at, owned_fd, &
        owned_pause, owned_materialize_blob, owned_finish, owned_dispose, owned_close
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

    interface
        integer(c_int) function c_mkdirs(path) &
                bind(C, name='fx_immutable_mkdirs_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdirs

        integer(c_int) function c_tempfile(dir, out, cap) &
                bind(C, name='fx_immutable_tempfile')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*)
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function c_tempfile

        integer(c_int) function c_fsync_file(path) &
                bind(C, name='fx_immutable_fsync_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_fsync_file

        integer(c_int) function c_seal_file(path) &
                bind(C, name='fx_immutable_seal_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_seal_file

        integer(c_int) function c_publish_file(src, dst) &
                bind(C, name='fx_immutable_publish_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function c_publish_file

        integer(c_int) function c_unlink(path) bind(C, name='fx_c_unlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink

    end interface

contains

    subroutine immutable_store_put_tree(store, entries, tree_id, ierr)
        type(immutable_store_t), intent(in) :: store
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=HASH_LEN), intent(out) :: tree_id
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: sorted(:)
        character(len=:), allocatable :: manifest, file_path, dir, temp_path
        character(kind=c_char), allocatable :: c_temp(:), c_final(:)
        integer :: i, status, cleanup

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
        file_path = immutable_store_tree_path(store, tree_id)
        call immutable_store_verify_tree(store, tree_id, status)
        if (status == IMMUTABLE_OK) then
            ierr = IMMUTABLE_OK
            return
        else if (status /= IMMUTABLE_MISSING) then
            ierr = status
            return
        end if
        dir = path_dirname(file_path)
        call ensure_dir(dir, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call make_tempfile(dir, temp_path, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call write_text_file(temp_path, manifest, ierr)
        if (ierr /= IMMUTABLE_OK) then
            call unlink_path(temp_path, cleanup)
            return
        end if
        call to_c_text(temp_path, c_temp)
        if (c_seal_file(c_temp) /= 0_c_int) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        call to_c_text(temp_path, c_temp)
        call to_c_text(file_path, c_final)
        status = c_publish_file(c_temp, c_final)
        if (status == 1_c_int) then
            call unlink_path(temp_path, cleanup)
            call immutable_store_verify_tree(store, tree_id, ierr)
        else if (status /= 0_c_int) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_IO_ERROR
        else
            ierr = IMMUTABLE_OK
        end if
    end subroutine immutable_store_put_tree

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

    subroutine make_tempfile(dir, path, ierr)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable, intent(out) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_out(PATH_LIMIT)
        character(kind=c_char), allocatable :: c_dir(:)
        integer(c_int) :: status
        c_out = c_null_char
        call to_c_text(dir, c_dir)
        status = c_tempfile(c_dir, c_out, int(PATH_LIMIT, c_int))
        ierr = IMMUTABLE_IO_ERROR
        if (status /= 0_c_int) return
        path = from_c_text(c_out)
        ierr = IMMUTABLE_OK
    end subroutine make_tempfile

    subroutine unlink_path(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_path(:)
        call to_c_text(path, c_path)
        ierr = int(c_unlink(c_path))
    end subroutine unlink_path
    subroutine write_text_file(path, text, ierr)
        character(len=*), intent(in) :: path, text
        integer, intent(out) :: ierr
        integer :: unit, ios, close_ios
        integer(c_int) :: status
        character(kind=c_char), allocatable :: c_path(:)

        ierr = IMMUTABLE_IO_ERROR
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='write', position='rewind', iostat=ios)
        if (ios /= 0) return
        write(unit, iostat=ios) text
        if (ios == 0) flush(unit, iostat=ios)
        close(unit, iostat=close_ios)
        if (ios /= 0 .or. close_ios /= 0) return
        call to_c_text(path, c_path)
        status = c_fsync_file(c_path)
        if (status == 0_c_int) ierr = IMMUTABLE_OK
    end subroutine write_text_file

    subroutine ensure_dir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_path(:)
        call to_c_text(path, c_path)
        ierr = IMMUTABLE_IO_ERROR
        if (c_mkdirs(c_path) == 0_c_int) ierr = IMMUTABLE_OK
    end subroutine ensure_dir

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

    function from_c_text(text) result(value)
        character(kind=c_char), intent(in) :: text(:)
        character(len=:), allocatable :: value
        integer :: i, n
        n = 0
        do i = 1, size(text)
            if (text(i) == c_null_char) exit
            n = n + 1
        end do
        allocate(character(len=n) :: value)
        do i = 1, n
            value(i:i) = text(i)
        end do
    end function from_c_text

end module fx_immutable_tree
