module fx_proc
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, &
                                           c_null_char, c_null_ptr
    implicit none
    private

    integer, parameter :: PATH_MAX_LEN = 4096

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

        integer(c_int) function fx_c_inotify_init() bind(C)
            import :: c_int
        end function fx_c_inotify_init

        integer(c_int) function fx_c_inotify_add_watch(fd, path, mask) &
                bind(C)
            import :: c_int, c_char
            integer(c_int), intent(in), value :: fd
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), intent(in), value :: mask
        end function fx_c_inotify_add_watch

        integer(c_int) function fx_c_inotify_rm_watch(fd, wd) bind(C)
            import :: c_int
            integer(c_int), intent(in), value :: fd
            integer(c_int), intent(in), value :: wd
        end function fx_c_inotify_rm_watch

        integer(c_int) function fx_c_inotify_poll(fd, path_buf, path_len, &
                event_type, timeout_ms) bind(C)
            import :: c_int, c_char
            integer(c_int), intent(in), value :: fd
            character(kind=c_char), intent(out) :: path_buf(*)
            integer(c_int), intent(in), value :: path_len
            integer(c_int), intent(out) :: event_type
            integer(c_int), intent(in), value :: timeout_ms
        end function fx_c_inotify_poll

        integer(c_int) function fx_c_inotify_close(fd) bind(C)
            import :: c_int
            integer(c_int), intent(in), value :: fd
        end function fx_c_inotify_close

        integer(c_int) function fx_c_count_dirs(root, n_dirs) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: n_dirs
        end function fx_c_count_dirs

        integer(c_int) function fx_c_collect_dirs(root, dirs, dir_len, &
                n_dirs, max_dirs) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: dirs(*)
            integer(c_int), intent(in), value :: dir_len
            integer(c_int), intent(out) :: n_dirs
            integer(c_int), intent(in), value :: max_dirs
        end function fx_c_collect_dirs

        integer(c_int) function fx_c_count_files(root, n_files) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: n_files
        end function fx_c_count_files

        integer(c_int) function fx_c_collect_files(root, files, file_len, &
                n_files, max_files) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: root(*)
            character(kind=c_char), intent(out) :: files(*)
            integer(c_int), intent(in), value :: file_len
            integer(c_int), intent(out) :: n_files
            integer(c_int), intent(in), value :: max_files
        end function fx_c_collect_files

        integer(c_int) function fx_c_path_is_dir(path) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function fx_c_path_is_dir
    end interface

    public :: proc_exec, proc_exec_silent, proc_scan_dir
    public :: proc_file_read, proc_file_write
    public :: proc_tmpfile, proc_pid, proc_kill
    public :: proc_watch_init, proc_watch_add, proc_watch_rm
    public :: proc_watch_poll, proc_watch_close
    public :: proc_scan_dirs, proc_scan_files, proc_path_is_dir

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

    subroutine proc_watch_init(fd, ierr)
        integer, intent(out) :: fd
        integer, intent(out) :: ierr

        integer(c_int) :: c_fd

        c_fd = fx_c_inotify_init()
        fd = int(c_fd)
        if (fd >= 0) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine proc_watch_init

    subroutine proc_watch_add(fd, path, mask, wd, ierr)
        integer, intent(in) :: fd
        character(len=*), intent(in) :: path
        integer, intent(in) :: mask
        integer, intent(out) :: wd
        integer, intent(out) :: ierr

        character(kind=c_char) :: c_path(PATH_MAX_LEN)
        integer(c_int) :: c_wd

        call to_c_string(path, c_path)
        c_wd = fx_c_inotify_add_watch(int(fd, c_int), c_path, &
                                      int(mask, c_int))
        wd = int(c_wd)
        if (wd >= 0) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine proc_watch_add

    subroutine proc_watch_rm(fd, wd, ierr)
        integer, intent(in) :: fd
        integer, intent(in) :: wd
        integer, intent(out) :: ierr

        integer(c_int) :: c_status

        c_status = fx_c_inotify_rm_watch(int(fd, c_int), int(wd, c_int))
        if (c_status == 0_c_int) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine proc_watch_rm

    subroutine proc_watch_poll(fd, path, event_type, timeout_ms, got_event)
        integer, intent(in) :: fd
        character(len=*), intent(out) :: path
        integer, intent(out) :: event_type
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: got_event

        character(kind=c_char) :: c_path(PATH_MAX_LEN)
        integer(c_int) :: c_event_type
        integer(c_int) :: c_got

        path = ''
        event_type = 0
        c_event_type = 0_c_int
        c_got = fx_c_inotify_poll(int(fd, c_int), c_path, &
                                  int(PATH_MAX_LEN, c_int), c_event_type, &
                                  int(timeout_ms, c_int))
        got_event = (c_got > 0_c_int)
        if (got_event) then
            path = c_string_from_chars(c_path)
            event_type = int(c_event_type)
        end if
    end subroutine proc_watch_poll

    subroutine proc_watch_close(fd, ierr)
        integer, intent(in) :: fd
        integer, intent(out) :: ierr

        integer(c_int) :: c_status

        c_status = fx_c_inotify_close(int(fd, c_int))
        if (c_status == 0_c_int) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine proc_watch_close

    subroutine proc_scan_dirs(root, dirs, n_dirs, ierr)
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: dirs(:)
        integer, intent(out) :: n_dirs
        integer, intent(out) :: ierr

        character(kind=c_char) :: c_root(PATH_MAX_LEN)
        character(kind=c_char), allocatable :: c_dirs(:)
        integer(c_int) :: c_count
        integer(c_int) :: c_status
        integer :: i
        integer :: slot

        call to_c_string(root, c_root)

        c_count = 0_c_int
        c_status = fx_c_count_dirs(c_root, c_count)
        if (c_status /= 0_c_int .or. c_count < 0_c_int) then
            ierr = 1
            n_dirs = 0
            allocate(character(len=PATH_MAX_LEN) :: dirs(0))
            return
        end if

        n_dirs = int(c_count)
        allocate(character(len=PATH_MAX_LEN) :: dirs(n_dirs))
        if (n_dirs == 0) then
            ierr = 0
            return
        end if

        allocate(c_dirs(n_dirs * PATH_MAX_LEN))
        c_dirs = c_null_char
        c_status = fx_c_collect_dirs(c_root, c_dirs, &
                                     int(PATH_MAX_LEN, c_int), c_count, &
                                     int(n_dirs, c_int))
        if (c_status /= 0_c_int) then
            ierr = 1
            deallocate(c_dirs)
            return
        end if

        n_dirs = int(c_count)

        do i = 1, n_dirs
            slot = (i - 1) * PATH_MAX_LEN + 1
            dirs(i) = c_string_from_chars(c_dirs(slot:slot + PATH_MAX_LEN - 1))
        end do

        ierr = 0
        deallocate(c_dirs)
    end subroutine proc_scan_dirs

    subroutine proc_scan_files(root, files, n_files, ierr)
        character(len=*), intent(in) :: root
        character(len=:), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer, intent(out) :: ierr

        character(kind=c_char) :: c_root(PATH_MAX_LEN)
        character(kind=c_char), allocatable :: c_files(:)
        integer(c_int) :: c_count
        integer(c_int) :: c_status
        integer :: i
        integer :: slot

        call to_c_string(root, c_root)

        c_count = 0_c_int
        c_status = fx_c_count_files(c_root, c_count)
        if (c_status /= 0_c_int .or. c_count < 0_c_int) then
            ierr = 1
            n_files = 0
            allocate(character(len=PATH_MAX_LEN) :: files(0))
            return
        end if

        n_files = int(c_count)
        allocate(character(len=PATH_MAX_LEN) :: files(n_files))
        if (n_files == 0) then
            ierr = 0
            return
        end if

        allocate(c_files(n_files * PATH_MAX_LEN))
        c_files = c_null_char
        c_status = fx_c_collect_files(c_root, c_files, &
                                      int(PATH_MAX_LEN, c_int), c_count, &
                                      int(n_files, c_int))
        if (c_status /= 0_c_int) then
            ierr = 1
            deallocate(c_files)
            return
        end if

        n_files = int(c_count)
        do i = 1, n_files
            slot = (i - 1) * PATH_MAX_LEN + 1
            files(i) = c_string_from_chars(c_files(slot:slot + PATH_MAX_LEN - 1))
        end do

        ierr = 0
        deallocate(c_files)
    end subroutine proc_scan_files

    logical function proc_path_is_dir(path)
        character(len=*), intent(in) :: path

        character(kind=c_char) :: c_path(PATH_MAX_LEN)
        integer(c_int) :: c_is_dir

        call to_c_string(path, c_path)
        c_is_dir = fx_c_path_is_dir(c_path)
        proc_path_is_dir = (c_is_dir /= 0_c_int)
    end function proc_path_is_dir

    subroutine to_c_string(text, c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), intent(out) :: c_text(:)

        integer :: i, n

        c_text = c_null_char
        n = min(len_trim(text), size(c_text) - 1)
        do i = 1, n
            c_text(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_text(n + 1) = c_null_char
    end subroutine to_c_string

    function c_string_from_chars(chars) result(text)
        character(kind=c_char), intent(in) :: chars(:)
        character(len=:), allocatable :: text
        integer :: n
        integer :: i

        n = 0
        do i = 1, size(chars)
            if (chars(i) == c_null_char) exit
            n = n + 1
        end do

        allocate(character(len=n) :: text)
        do i = 1, n
            text(i:i) = char(iachar(chars(i)))
        end do
    end function c_string_from_chars

end module fx_proc
