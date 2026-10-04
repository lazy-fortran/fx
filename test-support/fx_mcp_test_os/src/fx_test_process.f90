module fx_test_process
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    implicit none
    private
    public :: test_process_spawn, test_process_spawn_piped, test_process_read
    public :: test_process_write, test_process_close, test_process_is_executable
    public :: test_process_wait_once, test_process_signal, test_process_identity
    public :: test_process_clock_ms, test_process_sleep_ms

    type, public :: test_process_t
        integer :: pid = -1
        integer :: input_fd = -1
        integer :: output_fd = -1
    end type test_process_t

    interface
        integer(c_int) function c_spawn(bytes, offsets, count, pid) &
                bind(C, name='fx_test_process_spawn')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), intent(in) :: offsets(*)
            integer(c_int), value :: count
            integer(c_int), intent(out) :: pid
        end function c_spawn
        integer(c_int) function c_spawn_piped(bytes, offsets, count, capture_stderr, &
                pid, input_fd, output_fd) bind(C, name='fx_test_process_spawn_piped')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), intent(in) :: offsets(*)
            integer(c_int), value :: count, capture_stderr
            integer(c_int), intent(out) :: pid, input_fd, output_fd
        end function c_spawn_piped
        integer(c_int) function c_pipe_read(fd, bytes, capacity, timeout) &
                bind(C, name='fx_test_process_pipe_read')
            import :: c_char, c_int
            integer(c_int), value :: fd, capacity, timeout
            character(kind=c_char), intent(out) :: bytes(*)
        end function c_pipe_read
        integer(c_int) function c_pipe_write(fd, bytes, count) &
                bind(C, name='fx_test_process_pipe_write')
            import :: c_char, c_int
            integer(c_int), value :: fd, count
            character(kind=c_char), intent(in) :: bytes(*)
        end function c_pipe_write
        integer(c_int) function c_close_fd(fd) &
                bind(C, name='fx_test_process_close_fd')
            import :: c_int
            integer(c_int), value :: fd
        end function c_close_fd
        integer(c_int) function c_is_executable(path) &
                bind(C, name='fx_test_process_is_executable')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_is_executable
        integer(c_int) function c_wait(pid, status) &
                bind(C, name='fx_test_process_wait_once')
            import :: c_int
            integer(c_int), value :: pid
            integer(c_int), intent(out) :: status
        end function c_wait
        integer(c_int) function c_signal(pid, signal_number) &
                bind(C, name='fx_test_process_signal')
            import :: c_int
            integer(c_int), value :: pid, signal_number
        end function c_signal
        integer(c_int) function c_identity(pid, start, parent, path, capacity) &
                bind(C, name='fx_test_process_identity')
            import :: c_char, c_int, c_int64_t
            integer(c_int), value :: pid
            integer(c_int64_t), intent(out) :: start
            integer(c_int), intent(out) :: parent
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: capacity
        end function c_identity
        integer(c_int64_t) function c_clock_ms() &
                bind(C, name='fx_test_process_clock_ms')
            import :: c_int64_t
        end function c_clock_ms
        subroutine c_sleep_ms(milliseconds) &
                bind(C, name='fx_test_process_sleep_ms')
            import :: c_int
            integer(c_int), value :: milliseconds
        end subroutine c_sleep_ms
    end interface

contains

    subroutine test_process_spawn(arguments, pid, ierr)
        character(len=*), intent(in) :: arguments(:)
        integer, intent(out) :: pid, ierr
        integer(c_int) :: child_pid
        character(kind=c_char), allocatable :: bytes(:)
        integer(c_int), allocatable :: offsets(:)
        integer :: total

        if (size(arguments) < 1) then
            pid = -1
            ierr = -1
            return
        end if
        total = sum(len_trim(arguments)) + size(arguments)
        allocate(bytes(total), offsets(size(arguments)))
        child_pid = -1_c_int
        call pack_arguments(arguments, bytes, offsets)
        ierr = int(c_spawn(bytes, offsets, int(size(arguments), c_int), &
            child_pid))
        pid = int(child_pid)
    end subroutine test_process_spawn

    subroutine test_process_spawn_piped(arguments, process, capture_stderr, ierr)
        character(len=*), intent(in) :: arguments(:)
        type(test_process_t), intent(out) :: process
        logical, intent(in) :: capture_stderr
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: bytes(:)
        integer(c_int), allocatable :: offsets(:)
        integer(c_int) :: child_pid, input_fd, output_fd
        integer :: total

        process = test_process_t()
        if (size(arguments) < 1) then
            ierr = -1
            return
        end if
        total = sum(len_trim(arguments)) + size(arguments)
        allocate(bytes(total), offsets(size(arguments)))
        call pack_arguments(arguments, bytes, offsets)
        child_pid = -1_c_int
        input_fd = -1_c_int
        output_fd = -1_c_int
        ierr = int(c_spawn_piped(bytes, offsets, int(size(arguments), c_int), &
            merge(1_c_int, 0_c_int, capture_stderr), child_pid, input_fd, output_fd))
        if (ierr == 0) then
            process%pid = int(child_pid)
            process%input_fd = int(input_fd)
            process%output_fd = int(output_fd)
        end if
    end subroutine test_process_spawn_piped

    integer function test_process_read(fd, bytes, timeout_ms)
        integer, intent(in) :: fd, timeout_ms
        character(kind=c_char), intent(out) :: bytes(:)
        test_process_read = int(c_pipe_read(int(fd, c_int), bytes, &
            int(size(bytes), c_int), int(timeout_ms, c_int)))
    end function test_process_read

    integer function test_process_write(fd, bytes)
        integer, intent(in) :: fd
        character(len=*), intent(in) :: bytes
        test_process_write = int(c_pipe_write(int(fd, c_int), bytes, &
            int(len(bytes), c_int)))
    end function test_process_write

    integer function test_process_is_executable(path)
        character(len=*), intent(in) :: path
        test_process_is_executable = int(c_is_executable(trim(path)//c_null_char))
    end function test_process_is_executable

    subroutine test_process_close(process, timeout_ms, status, timed_out)
        type(test_process_t), intent(inout) :: process
        integer, intent(in) :: timeout_ms
        integer, intent(out) :: status
        logical, intent(out) :: timed_out
        integer :: state, signal_error
        integer(c_int64_t) :: start_ms, now_ms

        if (process%input_fd >= 0) then
            block
                integer(c_int) :: ignored
                ignored = c_close_fd(int(process%input_fd, c_int))
            end block
        end if
        process%input_fd = -1
        start_ms = test_process_clock_ms()
        timed_out = .false.
        status = -1
        do
            call test_process_wait_once(process%pid, status, state)
            if (state == 1) exit
            if (state < 0) exit
            now_ms = test_process_clock_ms()
            if (now_ms < 0_c_int64_t .or. &
                now_ms - start_ms >= int(timeout_ms, c_int64_t)) then
                call test_process_signal(process%pid, 9, signal_error)
                timed_out = .true.
                do
                    call test_process_wait_once(process%pid, status, state)
                    if (state /= 0) exit
                    call test_process_sleep_ms(10)
                end do
                status = -2
                exit
            end if
            call test_process_sleep_ms(10)
        end do
        if (process%output_fd >= 0) then
            block
                integer(c_int) :: ignored
                ignored = c_close_fd(int(process%output_fd, c_int))
            end block
        end if
        process%pid = -1
        process%input_fd = -1
        process%output_fd = -1
    end subroutine test_process_close

    subroutine pack_arguments(arguments, bytes, offsets)
        character(len=*), intent(in) :: arguments(:)
        character(kind=c_char), intent(out) :: bytes(:)
        integer(c_int), intent(out) :: offsets(:)
        integer :: i, j, position

        position = 1
        do i = 1, size(arguments)
            offsets(i) = int(position, c_int)
            do j = 1, len_trim(arguments(i))
                bytes(position) = arguments(i)(j:j)
                position = position + 1
            end do
            bytes(position) = c_null_char
            position = position + 1
        end do
    end subroutine pack_arguments

    subroutine test_process_wait_once(pid, status, state)
        integer, intent(in) :: pid
        integer, intent(out) :: status, state
        integer(c_int) :: c_status
        if (pid <= 0) then
            status = -1
            state = -1
            return
        end if
        state = int(c_wait(int(pid, c_int), c_status))
        status = int(c_status)
    end subroutine test_process_wait_once

    subroutine test_process_signal(pid, signal_number, ierr)
        integer, intent(in) :: pid, signal_number
        integer, intent(out) :: ierr
        if (pid <= 0) then
            ierr = -1
            return
        end if
        ierr = int(c_signal(int(pid, c_int), int(signal_number, c_int)))
    end subroutine test_process_signal

    subroutine test_process_identity(pid, start, parent, path, ierr)
        integer, intent(in) :: pid
        integer(c_int64_t), intent(out) :: start
        integer, intent(out) :: parent
        character(len=*), intent(out) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(len(path))
        integer(c_int) :: c_parent
        integer :: i
        c_path = c_null_char
        c_parent = -1_c_int
        start = -1_c_int64_t
        ierr = int(c_identity(int(pid, c_int), start, c_parent, c_path, &
            int(len(path), c_int)))
        parent = int(c_parent)
        path = ''
        do i = 1, len(path)
            if (c_path(i) == c_null_char) exit
            path(i:i) = c_path(i)
        end do
    end subroutine test_process_identity

    integer(c_int64_t) function test_process_clock_ms()
        test_process_clock_ms = c_clock_ms()
    end function test_process_clock_ms

    subroutine test_process_sleep_ms(milliseconds)
        integer, intent(in) :: milliseconds
        call c_sleep_ms(int(milliseconds, c_int))
    end subroutine test_process_sleep_ms

end module fx_test_process
