module fx_test_process
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_null_char
    implicit none
    private
    public :: test_process_spawn, test_process_wait_once, test_process_signal
    public :: test_process_identity, test_process_clock_ms, test_process_sleep_ms

    interface
        integer(c_int) function c_spawn(bytes, offsets, count, pid) &
                bind(C, name='fx_test_process_spawn')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), intent(in) :: offsets(*)
            integer(c_int), value :: count
            integer(c_int), intent(out) :: pid
        end function c_spawn
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
        integer :: i, j, position, total

        total = sum(len_trim(arguments)) + size(arguments)
        allocate(bytes(total), offsets(size(arguments)))
        child_pid = -1_c_int
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
        ierr = int(c_spawn(bytes, offsets, int(size(arguments), c_int), &
            child_pid))
        pid = int(child_pid)
    end subroutine test_process_spawn

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
