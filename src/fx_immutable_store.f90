module fx_immutable_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char, c_ptr, c_associated
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_init, sha256_update, sha256_final, sha256_state_t
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_close, owned_begin_path, owned_dispose, owned_materialize_blob, &
        owned_file_info, owned_fd, owned_hash_fd, owned_pause, &
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
        logical :: initialized = .false.
    end type immutable_store_t


    interface
        integer(c_int) function c_mkdirs(path) &
                bind(C, name='fx_immutable_mkdirs_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdirs

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
        integer :: status

        object_id = ''
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        call immutable_store_hash_file(source_path, object_id, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call immutable_store_verify_blob(store, object_id, status)
        if (status == IMMUTABLE_OK) then
            ierr = IMMUTABLE_OK
            return
        else if (status /= IMMUTABLE_MISSING) then
            ierr = status
            return
        end if

        call publish_blob_capture(store, source_path, object_id, ierr)
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
        call owned_pause(capture, 5_c_int)
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
