module fx_test_fs
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    implicit none
    private
    public :: fx_test_mkdir_p, fx_test_remove_tree, fx_test_rename
    public :: fx_test_symlink, fx_test_sleep_ms

    interface
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
        integer(c_int) function c_sleep_ms(milliseconds) &
                bind(C, name='fx_test_fs_sleep_ms')
            import :: c_int, c_int64_t
            integer(c_int64_t), value :: milliseconds
        end function c_sleep_ms
    end interface

contains

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

    integer function fx_test_sleep_ms(milliseconds) result(ierr)
        integer(c_int64_t), intent(in) :: milliseconds
        ierr = int(c_sleep_ms(milliseconds))
    end function fx_test_sleep_ms

end module fx_test_fs
