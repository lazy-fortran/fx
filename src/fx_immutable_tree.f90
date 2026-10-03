module fx_immutable_tree
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_path, only: path_dirname, path_join
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        IMMUTABLE_IO_ERROR, IMMUTABLE_INVALID, IMMUTABLE_MISSING, &
        IMMUTABLE_CORRUPT, IMMUTABLE_UNSUPPORTED, &
        IMMUTABLE_MATERIALIZE_AUTO, IMMUTABLE_MATERIALIZE_COPY, &
        IMMUTABLE_MATERIALIZE_CLONE, immutable_store_blob_path, &
        immutable_store_tree_path, immutable_store_verify_blob, &
        immutable_store_materialize_blob, immutable_store_hash_file
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

        integer(c_int) function c_tempdir(dir, out, cap) &
                bind(C, name='fx_immutable_tempdir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: dir(*)
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function c_tempdir

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

        integer(c_int) function c_mkdir_mode(path, mode) &
                bind(C, name='fx_immutable_mkdir_mode')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_mkdir_mode

        integer(c_int) function c_chmod_sync(path, mode) &
                bind(C, name='fx_immutable_chmod_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod_sync

        integer(c_int) function c_fsync_dir(path) &
                bind(C, name='fx_immutable_fsync_dir')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_fsync_dir

        integer(c_int) function c_publish_tree(src, dst) &
                bind(C, name='fx_immutable_publish_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function c_publish_tree

        integer(c_int) function c_file_info(path, size_bytes, mtime_ns, &
                inode) bind(C, name='fx_immutable_file_info')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: size_bytes, mtime_ns, inode
        end function c_file_info

        integer(c_int) function c_remove_tree(path) &
                bind(C, name='fx_immutable_remove_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_remove_tree
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

    recursive subroutine verify_tree_depth(store, tree_id, depth, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id
        integer, intent(in) :: depth
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: raw, file_path
        character(len=HASH_LEN) :: actual
        logical :: exists
        integer :: i
        integer(c_int) :: status
        integer(c_long_long) :: c_size, c_mtime, c_inode
        character(kind=c_char), allocatable :: c_file(:)

        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized .or. .not. immutable_id_valid(tree_id)) return
        if (depth > 128) return
        file_path = immutable_store_tree_path(store, tree_id)
        inquire(file=file_path, exist=exists)
        if (.not. exists) then
            ierr = IMMUTABLE_MISSING
            return
        end if
        call to_c_text(file_path, c_file)
        status = c_file_info(c_file, c_size, c_mtime, c_inode)
        if (status /= 0_c_int) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        call immutable_store_hash_file(file_path, actual, ierr)
        if (ierr /= IMMUTABLE_OK) return
        if (actual /= tree_id) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        call read_text_file(file_path, raw, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call immutable_manifest_parse(raw, entries, ierr)
        if (ierr /= IMMUTABLE_OK) return
        if (.not. allocated(entries)) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        do i = 1, size(entries)
            if (entries(i)%kind == IMMUTABLE_BLOB) then
                call immutable_store_verify_blob(store, entries(i)%object_id, ierr)
            else
                call verify_tree_depth(store, entries(i)%object_id, depth + 1, ierr)
            end if
            if (ierr /= IMMUTABLE_OK) return
        end do
        ierr = IMMUTABLE_OK
    end subroutine verify_tree_depth

    subroutine immutable_store_materialize_tree(store, tree_id, dest_path, &
            strategy, used_clone, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id, dest_path
        integer, intent(in) :: strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        character(len=:), allocatable :: parent, temp_root
        logical :: any_clone
        integer :: cleanup
        integer(c_int) :: status
        character(kind=c_char), allocatable :: c_temp(:), c_dest(:)

        used_clone = .false.
        if (strategy < IMMUTABLE_MATERIALIZE_AUTO .or. &
            strategy > IMMUTABLE_MATERIALIZE_CLONE) then
            ierr = IMMUTABLE_INVALID
            return
        end if
        call immutable_store_verify_tree(store, tree_id, ierr)
        if (ierr /= IMMUTABLE_OK) return
        parent = path_dirname(dest_path)
        call ensure_dir(parent, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call make_tempdir(parent, temp_root, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call materialize_tree_contents(store, tree_id, temp_root, strategy, &
            any_clone, ierr)
        if (ierr == IMMUTABLE_OK) then
            call to_c_text(temp_root, c_temp)
            status = c_chmod_sync(c_temp, int(DEFAULT_DIR_MODE, c_int))
            if (status /= 0_c_int) ierr = IMMUTABLE_IO_ERROR
        end if
        if (ierr == IMMUTABLE_OK) then
            call to_c_text(temp_root, c_temp)
            call to_c_text(dest_path, c_dest)
            status = c_publish_tree(c_temp, c_dest)
            if (status == 1_c_int) then
                ierr = IMMUTABLE_IO_ERROR
            else if (status == 2_c_int) then
                ierr = IMMUTABLE_UNSUPPORTED
            else if (status /= 0_c_int) then
                ierr = IMMUTABLE_IO_ERROR
            end if
        end if
        if (ierr /= IMMUTABLE_OK) then
            call to_c_text(temp_root, c_temp)
            status = c_remove_tree(c_temp)
            cleanup = int(status)
        else
            used_clone = any_clone
        end if
    end subroutine immutable_store_materialize_tree

    recursive subroutine materialize_tree_contents(store, tree_id, dir_path, &
            strategy, used_clone, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: tree_id, dir_path
        integer, intent(in) :: strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: manifest, tree_path, child
        logical :: child_clone
        integer(c_int) :: status
        integer :: i, read_err
        character(kind=c_char), allocatable :: c_child(:)

        used_clone = .false.
        tree_path = immutable_store_tree_path(store, tree_id)
        call read_text_file(tree_path, manifest, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call immutable_manifest_parse(manifest, entries, ierr)
        if (ierr /= IMMUTABLE_OK) return
        do i = 1, size(entries)
            child = path_join(dir_path, entries(i)%path)
            if (entries(i)%kind == IMMUTABLE_BLOB) then
                call immutable_store_materialize_blob(store, &
                    entries(i)%object_id, child, entries(i)%mode, strategy, &
                    child_clone, ierr)
                used_clone = used_clone .or. child_clone
            else
                call to_c_text(child, c_child)
                status = c_mkdir_mode(c_child, int(448, c_int))
                if (status /= 0_c_int) then
                    ierr = IMMUTABLE_IO_ERROR
                    return
                end if
                call materialize_tree_contents(store, entries(i)%object_id, &
                    child, strategy, child_clone, ierr)
                if (ierr /= IMMUTABLE_OK) return
                used_clone = used_clone .or. child_clone
                call to_c_text(child, c_child)
                status = c_chmod_sync(c_child, int(entries(i)%mode, c_int))
                if (status /= 0_c_int) then
                    ierr = IMMUTABLE_IO_ERROR
                    return
                end if
            end if
            if (ierr /= IMMUTABLE_OK) return
        end do
        read_err = sync_directory(dir_path)
        if (read_err /= 0) ierr = IMMUTABLE_IO_ERROR
    end subroutine materialize_tree_contents

    subroutine make_tempdir(dir, path, ierr)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable, intent(out) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_out(PATH_LIMIT)
        character(kind=c_char), allocatable :: c_dir(:)
        integer(c_int) :: status
        c_out = c_null_char
        call to_c_text(dir, c_dir)
        status = c_tempdir(c_dir, c_out, int(PATH_LIMIT, c_int))
        ierr = IMMUTABLE_IO_ERROR
        if (status /= 0_c_int) return
        path = from_c_text(c_out)
        ierr = IMMUTABLE_OK
    end subroutine make_tempdir

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
    subroutine read_text_file(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ierr
        integer(int64) :: n_bytes
        integer :: unit, ios
        character(len=1), allocatable :: bytes(:)

        inquire(file=path, size=n_bytes, iostat=ios)
        ierr = IMMUTABLE_IO_ERROR
        if (ios /= 0 .or. n_bytes < 0_int64 .or. &
            n_bytes > int(huge(0), int64)) return
        allocate(bytes(int(n_bytes)))
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=ios)
        if (ios /= 0) return
        if (n_bytes > 0) read(unit, iostat=ios) bytes
        close(unit)
        if (ios /= 0) return
        allocate(character(len=int(n_bytes)) :: text)
        if (n_bytes > 0) text = transfer(bytes, text)
        ierr = IMMUTABLE_OK
    end subroutine read_text_file

    subroutine ensure_dir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_path(:)
        call to_c_text(path, c_path)
        ierr = IMMUTABLE_IO_ERROR
        if (c_mkdirs(c_path) == 0_c_int) ierr = IMMUTABLE_OK
    end subroutine ensure_dir

    integer function sync_directory(path)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable :: c_path(:)
        call to_c_text(path, c_path)
        sync_directory = int(c_fsync_dir(c_path))
    end function sync_directory

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
