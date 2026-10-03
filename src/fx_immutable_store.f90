module fx_immutable_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char, c_ptr, c_associated
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_init, sha256_update, sha256_final, sha256_state_t
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_close, owned_begin_path, owned_dispose, owned_materialize_blob, &
        owned_file_info
    use fx_path, only: path_dirname
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
        logical :: initialized = .false.
    end type immutable_store_t


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

        integer(c_int) function c_copy_sync(src, dst) &
                bind(C, name='fx_immutable_copy_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function c_copy_sync

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

        integer(c_int) function c_materialize(src, dst, mode, strategy, &
                used_clone) bind(C, name='fx_immutable_materialize')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
            integer(c_int), value :: mode, strategy
            integer(c_int), intent(out) :: used_clone
        end function c_materialize

        integer(c_int) function c_file_info(path, size_bytes, mtime_ns, &
                inode) bind(C, name='fx_immutable_file_info')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: size_bytes, mtime_ns, inode
        end function c_file_info

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

        integer(c_int) function c_remove_tree(path) &
                bind(C, name='fx_immutable_remove_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_remove_tree
    end interface

    public :: immutable_store_init, immutable_store_blob_path
    public :: immutable_store_tree_path, immutable_store_put_blob
    public :: immutable_store_verify_blob
    public :: immutable_store_materialize_blob
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
        character(len=:), allocatable :: clean

        ierr = IMMUTABLE_INVALID
        clean = trim(root_dir)
        if (len(clean) == 0 .or. len(clean) >= PATH_LIMIT) return
        call to_c_text(clean, c_root)
        if (c_mkdirs(c_root) /= 0_c_int) then
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        store%root_dir = clean
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

    subroutine immutable_store_put_blob(store, source_path, object_id, ierr)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: source_path
        character(len=HASH_LEN), intent(out) :: object_id
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: copied_id
        character(len=:), allocatable :: final_path, dir, temp_path
        character(kind=c_char), allocatable :: c_source(:), c_temp(:), c_final(:)
        integer :: status, cleanup

        object_id = ''
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        call immutable_store_hash_file(source_path, object_id, ierr)
        if (ierr /= IMMUTABLE_OK) return
        final_path = immutable_store_blob_path(store, object_id)
        call immutable_store_verify_blob(store, object_id, status)
        if (status == IMMUTABLE_OK) then
            ierr = IMMUTABLE_OK
            return
        else if (status /= IMMUTABLE_MISSING) then
            ierr = status
            return
        end if

        dir = path_dirname(final_path)
        call ensure_dir(dir, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call make_tempfile(dir, temp_path, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call to_c_text(source_path, c_source)
        call to_c_text(temp_path, c_temp)
        status = c_copy_sync(c_source, c_temp)
        if (status /= 0_c_int) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        call immutable_store_hash_file(temp_path, copied_id, ierr)
        if (ierr /= IMMUTABLE_OK .or. copied_id /= object_id) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_INVALID
            return
        end if
        call to_c_text(temp_path, c_temp)
        if (c_seal_file(c_temp) /= 0_c_int) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_IO_ERROR
            return
        end if
        call to_c_text(temp_path, c_temp)
        call to_c_text(final_path, c_final)
        status = c_publish_file(c_temp, c_final)
        if (status == 1_c_int) then
            call unlink_path(temp_path, cleanup)
            call immutable_store_verify_blob(store, object_id, ierr)
        else if (status /= 0_c_int) then
            call unlink_path(temp_path, cleanup)
            ierr = IMMUTABLE_IO_ERROR
        else
            ierr = IMMUTABLE_OK
        end if
    end subroutine immutable_store_put_blob

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
        transaction = owned_begin_path(trim(dest_path)//c_null_char, 0_c_int)
        ierr = IMMUTABLE_IO_ERROR
        if (c_associated(transaction)) then
            call owned_materialize_blob(transaction, root, object_id, mode, &
                strategy, used_clone, ierr)
            call owned_dispose(transaction)
        end if
        cleanup = owned_close(root)
    end subroutine immutable_store_materialize_blob

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

    subroutine ensure_dir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        integer(c_int) :: status
        character(kind=c_char), allocatable :: c_path(:)
        call to_c_text(path, c_path)
        status = c_mkdirs(c_path)
        ierr = IMMUTABLE_IO_ERROR
        if (status == 0_c_int) ierr = IMMUTABLE_OK
    end subroutine ensure_dir

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

end module fx_immutable_store
