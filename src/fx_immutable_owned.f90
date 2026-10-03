module fx_immutable_owned
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_ptr, &
        c_null_char
    use fx_hash, only: sha256_init, sha256_update, sha256_final, sha256_state_t, &
        sha256_string
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_INVALID, IMMUTABLE_MISSING, IMMUTABLE_CORRUPT, &
        IMMUTABLE_UNSUPPORTED
    use fx_immutable_manifest, only: immutable_id_valid
    implicit none
    private
    public :: owned_open_store, owned_begin_path, owned_begin_at, owned_fd
    public :: owned_finish, owned_dispose, owned_close, owned_sync, owned_pause
    public :: owned_open_verified, owned_hash_fd, owned_read_manifest
    public :: owned_materialize_blob, owned_file_info
    integer, parameter :: HASH_BLOCK = 65536
    interface
        integer(c_int) function owned_open_store(path) &
                bind(C, name='fx_owned_open_store')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function owned_open_store
        function owned_begin_path(path, tree) bind(C, name='fx_owned_begin_path') &
                result(handle)
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: tree
            type(c_ptr) :: handle
        end function owned_begin_path
        function owned_begin_at(parent, name, tree) &
                bind(C, name='fx_owned_begin_at') result(handle)
            import :: c_char, c_int, c_ptr
            integer(c_int), value :: parent, tree
            character(kind=c_char), intent(in) :: name(*)
            type(c_ptr) :: handle
        end function owned_begin_at
        integer(c_int) function owned_fd(handle) bind(C, name='fx_owned_fd')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function owned_fd
        integer(c_int) function owned_finish(handle, mode) &
                bind(C, name='fx_owned_finish')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value :: mode
        end function owned_finish
        subroutine owned_dispose(handle) bind(C, name='fx_owned_dispose')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine owned_dispose
        subroutine owned_pause(handle, phase) bind(C, name='fx_owned_pause')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value :: phase
        end subroutine owned_pause
        integer(c_int) function owned_close(fd) bind(C, name='fx_owned_close')
            import :: c_int
            integer(c_int), value :: fd
        end function owned_close
        integer(c_int) function owned_sync(fd) bind(C, name='fx_owned_sync')
            import :: c_int
            integer(c_int), value :: fd
        end function owned_sync
        integer(c_int) function open_object(root, kind, id) &
                bind(C, name='fx_owned_open_object')
            import :: c_char, c_int
            integer(c_int), value :: root, kind
            character(kind=c_char), intent(in) :: id(*)
        end function open_object
        integer(c_int) function fd_size(fd, size_bytes) &
                bind(C, name='fx_owned_fd_size')
            import :: c_int, c_long_long
            integer(c_int), value :: fd
            integer(c_long_long), intent(out) :: size_bytes
        end function fd_size
        integer(c_long_long) function read_at(fd, offset, bytes, count) &
                bind(C, name='fx_owned_read')
            import :: c_int, c_long_long, c_char
            integer(c_int), value :: fd, count
            integer(c_long_long), value :: offset
            character(kind=c_char), intent(out) :: bytes(*)
        end function read_at
        integer(c_int) function owned_file_info(fd, size_bytes, mtime, inode) &
                bind(C, name='fx_owned_file_info')
            import :: c_int, c_long_long
            integer(c_int), value :: fd
            integer(c_long_long), intent(out) :: size_bytes, mtime, inode
        end function owned_file_info
        integer(c_int) function fill_blob(handle, source, strategy, cloned) &
                bind(C, name='fx_owned_fill')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value :: source, strategy
            integer(c_int), intent(out) :: cloned
        end function fill_blob
    end interface
contains
    subroutine owned_hash_fd(fd, digest, ierr)
        integer(c_int), intent(in) :: fd
        character(len=64), intent(out) :: digest
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: bytes(:)
        integer(c_long_long) :: size_bytes, offset, n, after
        integer :: chunk
        type(sha256_state_t) :: state

        digest = ''
        ierr = IMMUTABLE_IO_ERROR
        if (fd_size(fd, size_bytes) /= 0) return
        offset = 0_c_long_long
        allocate (bytes(HASH_BLOCK))
        call sha256_init(state)
        do while (offset < size_bytes)
            chunk = int(min(int(HASH_BLOCK, c_long_long), size_bytes - offset))
            n = read_at(fd, offset, bytes, int(chunk, c_int))
            if (n <= 0) return
            call sha256_update(state, bytes, int(n))
            offset = offset + n
        end do
        if (fd_size(fd, after) /= 0) return
        if (after /= size_bytes) return
        digest = sha256_final(state)
        ierr = IMMUTABLE_OK
    end subroutine owned_hash_fd

    subroutine owned_open_verified(root, kind, id, fd, ierr)
        integer(c_int), intent(in) :: root, kind
        character(len=*), intent(in) :: id
        integer(c_int), intent(out) :: fd
        integer, intent(out) :: ierr
        integer(c_int) :: cleanup
        character(len=64) :: actual

        ierr = IMMUTABLE_INVALID
        fd = -1_c_int
        if (.not. immutable_id_valid(id)) return
        fd = open_object(root, kind, id//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (fd == -2_c_int) ierr = IMMUTABLE_MISSING
        if (fd < 0) return
        call owned_hash_fd(fd, actual, ierr)
        if (ierr == IMMUTABLE_OK) then
            if (actual /= id) ierr = IMMUTABLE_CORRUPT
        end if
        if (ierr /= IMMUTABLE_OK) then
            cleanup = owned_close(fd)
            fd = -1_c_int
        end if
    end subroutine owned_open_verified

    subroutine owned_read_manifest(root, id, text, ierr)
        integer(c_int), intent(in) :: root
        character(len=*), intent(in) :: id
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ierr
        integer(c_int) :: fd, cleanup
        integer(c_long_long) :: count, offset, n

        ierr = IMMUTABLE_INVALID
        if (.not. immutable_id_valid(id)) return
        fd = open_object(root, 2_c_int, id//c_null_char)
        ierr = IMMUTABLE_CORRUPT
        if (fd == -2_c_int) ierr = IMMUTABLE_MISSING
        if (fd < 0) return
        ierr = IMMUTABLE_IO_ERROR
        if (fd_size(fd, count) == 0) then
            if (count <= int(huge(0), c_long_long)) then
                allocate (character(len=int(count)) :: text)
                offset = 0_c_long_long
                do while (offset < count)
                    n = read_at(fd, offset, text(int(offset) + 1:), &
                        int(min(int(HASH_BLOCK, c_long_long), count - offset), c_int))
                    if (n <= 0) exit
                    offset = offset + n
                end do
                if (offset == count) then
                    ierr = IMMUTABLE_OK
                    if (sha256_string(text) /= id) ierr = IMMUTABLE_CORRUPT
                end if
            end if
        end if
        cleanup = owned_close(fd)
    end subroutine owned_read_manifest

    subroutine owned_materialize_blob(handle, root, id, mode, strategy, &
            used_clone, ierr)
        type(c_ptr), intent(in) :: handle
        integer(c_int), intent(in) :: root
        character(len=*), intent(in) :: id
        integer, intent(in) :: mode, strategy
        logical, intent(out) :: used_clone
        integer, intent(out) :: ierr
        integer(c_int) :: fd, cloned, status, cleanup
        character(len=64) :: actual

        used_clone = .false.
        call owned_open_verified(root, 1_c_int, id, fd, ierr)
        if (ierr /= IMMUTABLE_OK) return
        call owned_pause(handle, 1_c_int)
        status = fill_blob(handle, fd, int(strategy, c_int), cloned)
        cleanup = owned_close(fd)
        ierr = IMMUTABLE_IO_ERROR
        if (status == 2_c_int) ierr = IMMUTABLE_UNSUPPORTED
        if (status /= 0_c_int) return
        call owned_hash_fd(owned_fd(handle), actual, ierr)
        if (ierr /= IMMUTABLE_OK) return
        if (actual /= id) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        call owned_pause(handle, 2_c_int)
        status = owned_finish(handle, int(mode, c_int))
        ierr = IMMUTABLE_IO_ERROR
        if (status /= 0_c_int) return
        used_clone = cloned /= 0_c_int
        ierr = IMMUTABLE_OK
    end subroutine owned_materialize_blob
end module fx_immutable_owned
