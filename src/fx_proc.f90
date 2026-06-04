module fx_proc
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, &
                                           c_null_char, c_null_ptr
    implicit none
    private

    type, public :: proc_result_t
        integer :: exit_code = -1
        character(len=:), allocatable :: stdout_text
        character(len=:), allocatable :: stderr_text
    end type proc_result_t

    ! C function interfaces
    interface
        integer(c_int) function fx_c_exec(argv, n_argv, stdout_buf, &
                stdout_len, stderr_buf, stderr_len) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: argv(*)
            integer(c_int), intent(in), value :: n_argv
            character(kind=c_char), intent(out) :: stdout_buf(*)
            integer(c_int), intent(inout) :: stdout_len
            character(kind=c_char), intent(out) :: stderr_buf(*)
            integer(c_int), intent(inout) :: stderr_len
        end function fx_c_exec

        integer(c_int) function fx_c_exec_silent(argv, n_argv) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: argv(*)
            integer(c_int), intent(in), value :: n_argv
        end function fx_c_exec_silent

        integer(c_int) function fx_c_scan_dir(root, extensions, &
                n_ext, files, n_files, max_files) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(in) :: extensions(*)
            integer(c_int), intent(in), value :: n_ext
            character(kind=c_char), intent(out) :: files(*)
            integer(c_int), intent(out) :: n_files
            integer(c_int), intent(in), value :: max_files
        end function fx_c_scan_dir

        integer(c_int) function fx_c_file_read(path, content, &
                n_bytes) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(out) :: content(*)
            integer(c_int), intent(inout) :: n_bytes
        end function fx_c_file_read

        integer(c_int) function fx_c_file_write(path, content, &
                n_bytes) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            character(kind=c_char), intent(in) :: content(*)
            integer(c_int), intent(in), value :: n_bytes
        end function fx_c_file_write

        subroutine fx_c_tmpfile(prefix, path, path_len) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: prefix(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), intent(out) :: path_len
        end subroutine fx_c_tmpfile

        integer(c_int) function fx_c_pid() bind(C)
            import :: c_int
        end function fx_c_pid

        integer(c_int) function fx_c_kill(pid, signal) bind(C)
            import :: c_int
            integer(c_int), intent(in), value :: pid
            integer(c_int), intent(in), value :: signal
        end function fx_c_kill
    end interface

    public :: proc_exec, proc_exec_silent, proc_scan_dir
    public :: proc_file_read, proc_file_write
    public :: proc_tmpfile, proc_pid, proc_kill

contains

    subroutine proc_exec(argv, n_argv, result)
        character(len=*), intent(in) :: argv(:)
        integer, intent(in) :: n_argv
        type(proc_result_t), intent(out) :: result
        error stop "fx_proc:proc_exec not implemented"
    end subroutine proc_exec

    subroutine proc_exec_silent(argv, n_argv, exit_code)
        character(len=*), intent(in) :: argv(:)
        integer, intent(in) :: n_argv
        integer, intent(out) :: exit_code
        error stop "fx_proc:proc_exec_silent not implemented"
    end subroutine proc_exec_silent

    subroutine proc_scan_dir(root, extensions, n_ext, files, &
            n_files, max_files)
        character(len=*), intent(in) :: root
        character(len=*), intent(in) :: extensions(:)
        integer, intent(in) :: n_ext
        character(len=512), intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer, intent(in) :: max_files
        error stop "fx_proc:proc_scan_dir not implemented"
    end subroutine proc_scan_dir

    subroutine proc_file_read(path, content, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: content
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr
        error stop "fx_proc:proc_file_read not implemented"
    end subroutine proc_file_read

    subroutine proc_file_write(path, content, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: content
        integer, intent(in) :: n_bytes
        integer, intent(out) :: ierr
        error stop "fx_proc:proc_file_write not implemented"
    end subroutine proc_file_write

    subroutine proc_tmpfile(prefix, path)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable, intent(out) :: path
        error stop "fx_proc:proc_tmpfile not implemented"
    end subroutine proc_tmpfile

    integer function proc_pid()
        error stop "fx_proc:proc_pid not implemented"
    end function proc_pid

    subroutine proc_kill(pid, signal, ierr)
        integer, intent(in) :: pid
        integer, intent(in) :: signal
        integer, intent(out) :: ierr
        error stop "fx_proc:proc_kill not implemented"
    end subroutine proc_kill

end module fx_proc
