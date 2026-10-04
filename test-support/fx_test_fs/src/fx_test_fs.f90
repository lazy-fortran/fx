module fx_test_fs
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    implicit none
    private
    public :: fx_test_mkdir_p, fx_test_remove_tree, fx_test_rename
    public :: fx_test_symlink, fx_test_chmod, fx_test_sleep_ms
    public :: fx_test_lock, fx_test_unlock
    public :: fx_test_descriptor_count
    public :: fx_test_source_files

    integer, parameter :: PATH_MAX_LEN = 4096

    interface
        integer(c_int) function c_lock(path) bind(C, name='fx_test_fs_lock')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_lock
        integer(c_int) function c_unlock(fd) bind(C, name='fx_test_fs_unlock')
            import :: c_int
            integer(c_int), value :: fd
        end function c_unlock
        integer(c_int) function c_descriptor_count() &
                bind(C, name='fx_test_fs_descriptor_count')
            import :: c_int
        end function c_descriptor_count
        integer(c_int) function c_mkdir_p(path) &
                bind(C, name='fx_test_fs_mkdir_p')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdir_p
        integer(c_int) function c_remove_tree(path) &
                bind(C, name='fx_test_fs_remove_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_remove_tree
        integer(c_int) function c_rename(source, destination) &
                bind(C, name='fx_test_fs_rename')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: source(*)
            character(kind=c_char), intent(in) :: destination(*)
        end function c_rename
        integer(c_int) function c_symlink(target, link_path) &
                bind(C, name='fx_test_fs_symlink')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: target(*)
            character(kind=c_char), intent(in) :: link_path(*)
        end function c_symlink
        integer(c_int) function c_chmod(path, mode) &
                bind(C, name='fx_test_fs_chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function c_chmod
        integer(c_int) function c_sleep_ms(milliseconds) &
                bind(C, name='fx_test_fs_sleep_ms')
            import :: c_int, c_int64_t
            integer(c_int64_t), value :: milliseconds
        end function c_sleep_ms
        integer(c_int) function c_source_count(root, n_paths) &
                bind(C, name='fx_test_fs_source_count')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: n_paths
        end function c_source_count
        integer(c_int) function c_source_collect(root, paths, slot_len, &
                max_paths, n_paths) bind(C, name='fx_test_fs_source_collect')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: paths(*)
            integer(c_int), intent(in), value :: slot_len, max_paths
            integer(c_int), intent(out) :: n_paths
        end function c_source_collect
    end interface

contains

    subroutine fx_test_source_files(root, files, ierr)
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: files(:)
        integer, intent(out) :: ierr

        character(kind=c_char) :: c_root(PATH_MAX_LEN)
        character(kind=c_char), allocatable :: c_paths(:)
        integer(c_int) :: c_count, c_collected, c_status
        integer :: i, slot

        if (len_trim(root) == 0 .or. len_trim(root) >= PATH_MAX_LEN) then
            ierr = 1
            allocate(character(len=PATH_MAX_LEN) :: files(0))
            return
        end if
        c_root = c_null_char
        do i = 1, len_trim(root)
            c_root(i) = char(iachar(root(i:i)), kind=c_char)
        end do
        c_count = 0_c_int
        c_status = c_source_count(c_root, c_count)
        if (c_status /= 0_c_int .or. c_count < 0_c_int) then
            ierr = 1
            allocate(character(len=PATH_MAX_LEN) :: files(0))
            return
        end if
        if (c_count > int(huge(0) / PATH_MAX_LEN, c_int)) then
            ierr = 1
            allocate(character(len=PATH_MAX_LEN) :: files(0))
            return
        end if

        allocate(character(len=PATH_MAX_LEN) :: files(int(c_count)))
        if (c_count == 0_c_int) then
            ierr = 0
            return
        end if

        allocate(c_paths(int(c_count) * PATH_MAX_LEN))
        c_paths = c_null_char
        c_collected = 0_c_int
        c_status = c_source_collect(c_root, c_paths, &
            int(PATH_MAX_LEN, c_int), c_count, c_collected)
        if (c_status /= 0_c_int) then
            ierr = 1
            return
        end if
        if (c_collected /= c_count) then
            ierr = 1
            return
        end if
        do i = 1, int(c_collected)
            slot = (i - 1) * PATH_MAX_LEN + 1
            files(i) = chars_to_text(c_paths(slot:slot + PATH_MAX_LEN - 1))
        end do
        ierr = 0
    end subroutine fx_test_source_files

    function chars_to_text(chars) result(text)
        character(kind=c_char), intent(in) :: chars(:)
        character(len=:), allocatable :: text
        integer :: i, n

        n = size(chars)
        do i = 1, size(chars)
            if (chars(i) == c_null_char) then
                n = i - 1
                exit
            end if
        end do
        allocate(character(len=n) :: text)
        do i = 1, n
            text(i:i) = char(iachar(chars(i)))
        end do
    end function chars_to_text

    integer function fx_test_lock(path) result(fd)
        character(len=*), intent(in) :: path
        fd = int(c_lock(trim(path)//c_null_char))
    end function fx_test_lock

    integer function fx_test_unlock(fd) result(ierr)
        integer, intent(inout) :: fd
        ierr = int(c_unlock(int(fd, c_int)))
        fd = -1
    end function fx_test_unlock

    integer function fx_test_descriptor_count() result(count)
        count = c_descriptor_count()
    end function fx_test_descriptor_count

    integer function fx_test_mkdir_p(path) result(ierr)
        character(len=*), intent(in) :: path
        ierr = int(c_mkdir_p(trim(path)//c_null_char))
    end function fx_test_mkdir_p

    integer function fx_test_remove_tree(path) result(ierr)
        character(len=*), intent(in) :: path
        ierr = int(c_remove_tree(trim(path)//c_null_char))
    end function fx_test_remove_tree

    integer function fx_test_rename(source, destination) result(ierr)
        character(len=*), intent(in) :: source, destination
        ierr = int(c_rename(trim(source)//c_null_char, &
            trim(destination)//c_null_char))
    end function fx_test_rename

    integer function fx_test_symlink(target, link_path) result(ierr)
        character(len=*), intent(in) :: target, link_path
        ierr = int(c_symlink(trim(target)//c_null_char, &
            trim(link_path)//c_null_char))
    end function fx_test_symlink

    integer function fx_test_chmod(path, mode) result(ierr)
        character(len=*), intent(in) :: path
        integer, intent(in) :: mode
        ierr = int(c_chmod(trim(path)//c_null_char, int(mode, c_int)))
    end function fx_test_chmod

    integer function fx_test_sleep_ms(milliseconds) result(ierr)
        integer(c_int64_t), intent(in) :: milliseconds
        ierr = int(c_sleep_ms(milliseconds))
    end function fx_test_sleep_ms

end module fx_test_fs
