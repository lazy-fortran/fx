module fx_test
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, &
                                             real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private :: json_escape

    integer, public, parameter :: initial_test_capacity = 64

    type, public :: test_result_t
        character(len=128) :: name = ''
        character(len=256) :: message = ''
        logical :: status = .false.
    end type test_result_t

    type, public :: test_suite_t
        character(len=128) :: name = ''
        integer :: n_pass = 0
        integer :: n_fail = 0
        integer :: n_tests = 0
        integer :: max_tests = initial_test_capacity
        type(test_result_t), allocatable, public :: tests(:)
    end type test_suite_t

    public :: test_suite_init, test_assert
    public :: test_assert_equal_int, test_assert_equal_str
    public :: test_assert_equal_real
    public :: test_suite_summary, test_suite_exit
    public :: test_suite_to_json
    public :: test_record

contains

    subroutine test_suite_init(s, name)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: name

        s%name = trim(name)
        s%n_pass = 0
        s%n_fail = 0
        s%n_tests = 0
        if (allocated(s%tests)) deallocate(s%tests)
        allocate(s%tests(initial_test_capacity))
    end subroutine test_suite_init

    subroutine test_assert(s, condition, message)
        type(test_suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        if (condition) then
            s%n_pass = s%n_pass + 1
            write(output_unit, '(A)') '  PASS: ' // trim(message)
        else
            s%n_fail = s%n_fail + 1
            write(error_unit, '(A)') '  FAIL: ' // trim(message)
        end if
    end subroutine test_assert

    subroutine test_assert_equal_int(s, expected, actual, label)
        type(test_suite_t), intent(inout) :: s
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: label

        character(len=128) :: msg

        if (expected == actual) then
            call test_assert(s, .true., trim(label))
        else
            write(msg, '(A,I0,A,I0,A)') trim(label), &
                ' (expected ', expected, ', got ', actual, ')'
            call test_assert(s, .false., msg)
        end if
    end subroutine test_assert_equal_int

    subroutine test_assert_equal_str(s, expected, actual, label)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: label

        if (trim(expected) == trim(actual)) then
            call test_assert(s, .true., trim(label))
        else
            call test_assert(s, .false., &
                trim(label) // &
                ' (expected "' // trim(expected) // '"' // &
                ', got "' // trim(actual) // '")')
        end if
    end subroutine test_assert_equal_str

    subroutine test_assert_equal_real(s, expected, actual, label, tol)
        type(test_suite_t), intent(inout) :: s
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: actual
        character(len=*), intent(in) :: label
        real(real64), intent(in), optional :: tol

        real(real64) :: tolerance
        real(real64) :: diff
        logical :: pass
        logical :: tol_provided
        character(len=256) :: msg
        character(len=32) :: exp_str, act_str, diff_str, tol_str

        tol_provided = present(tol)
        if (tol_provided) then
            tolerance = tol
        else
            tolerance = 1.0e-10_real64
        end if

        diff = abs(expected - actual)
        pass = (diff < tolerance)

        if (pass) then
            call test_assert(s, .true., trim(label))
        else
            if (ieee_is_nan(expected)) then
                exp_str = 'NaN'
            else
                write(exp_str, '(G0)') expected
                exp_str = adjustl(trim(exp_str))
            end if

            if (ieee_is_nan(actual)) then
                act_str = 'NaN'
            else
                write(act_str, '(G0)') actual
                act_str = adjustl(trim(act_str))
            end if

            if (diff == 0.0_real64) then
                diff_str = '0.0'
            elseif (ieee_is_nan(diff)) then
                diff_str = 'NaN'
            else
                write(diff_str, '(G0)') diff
                diff_str = adjustl(trim(diff_str))
            end if

            if (tol_provided) then
                write(tol_str, '(G0)') tolerance
                tol_str = adjustl(trim(tol_str))
                write(msg, '(A,A,G0,A,G0,A,G0,A,G0,A,G0,A)') &
                    trim(label), ' (expected ', expected, &
                    ', got ', actual, ', diff ', diff, &
                    ', tol ', tolerance, ')'
            else
                write(msg, '(A,A,G0,A,G0,A,G0,A)') &
                    trim(label), ' (expected ', expected, &
                    ', got ', actual, ', diff ', diff, ')'
            end if
            call test_assert(s, .false., msg)
        end if
    end subroutine test_assert_equal_real

    subroutine test_suite_summary(s)
        type(test_suite_t), intent(in) :: s

        write(output_unit, '(A,I0,A,I0,A,I0,A)') &
            trim(s%name) // ': ', s%n_pass, ' pass, ', &
            s%n_fail, ' fail, ', s%n_pass + s%n_fail, ' total'
    end subroutine test_suite_summary

    subroutine test_suite_exit(s)
        type(test_suite_t), intent(in) :: s

        if (s%n_fail > 0) then
            stop 1
        end if
    end subroutine test_suite_exit

    subroutine test_record(s, name, message, status)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: message
        logical, intent(in) :: status
        type(test_result_t), allocatable :: temp_tests(:)

        if (.not. allocated(s%tests)) then
            allocate(s%tests(initial_test_capacity))
            s%max_tests = initial_test_capacity
        end if

        if (s%n_tests >= s%max_tests) then
            allocate(temp_tests(s%max_tests))
            temp_tests = s%tests
            s%max_tests = s%max_tests * 2
            deallocate(s%tests)
            allocate(s%tests(s%max_tests))
            s%tests(1:s%n_tests) = temp_tests
            deallocate(temp_tests)
        end if

        s%n_tests = s%n_tests + 1
        s%tests(s%n_tests)%name = trim(name)
        s%tests(s%n_tests)%message = trim(message)
        s%tests(s%n_tests)%status = status
    end subroutine test_record

    subroutine test_suite_to_json(s, output)
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: output

        character(len=32) :: pass_str, fail_str, total_str
        integer :: i
        character(len=128) :: test_name
        character(len=10) :: test_status
        character(len=256) :: test_json
        character(len=256) :: escaped_msg

        write(pass_str, '(I0)') s%n_pass
        write(fail_str, '(I0)') s%n_fail
        write(total_str, '(I0)') s%n_pass + s%n_fail
        pass_str = adjustl(trim(pass_str))
        fail_str = adjustl(trim(fail_str))
        total_str = adjustl(trim(total_str))

        output = '{"suite":"' // trim(s%name) // '",'
        output = output // '"pass":' // pass_str
        output = output // ',"fail":' // fail_str
        output = output // ',"total":' // total_str
        output = output // ',"tests":['

        do i = 1, s%n_tests
            if (i > 1) output = output // ','
            test_name = trim(s%tests(i)%name)
            if (s%tests(i)%status) then
                test_status = '"pass"'
            else
                test_status = '"fail"'
            end if

            if (trim(s%tests(i)%message) == '') then
                write(test_json, '(A,A,A,A,A)') &
                    '{"name":"', test_name, '","status":', &
                    test_status, '}'
            else
                escaped_msg = json_escape( &
                    trim(s%tests(i)%message))
                write(test_json, '(A,A,A,A,A,A,A,A,A)') &
                    '{"name":"', test_name, '","status":', &
                    test_status, ',"message":"', &
                    escaped_msg, '"}'
            end if

            output = output // test_json
        end do

        output = output // ']}'
    end subroutine test_suite_to_json

    function json_escape(s) result(escaped)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: escaped
        integer :: i, j
        character(len=2) :: hex
        character(len=1) :: c

        escaped = s
        j = 1
        do i = 1, len(s)
            c = s(i:i)
            if (c == '"') then
                escaped(j:j+1) = '\"'
                j = j + 2
            elseif (c == '\') then
                escaped(j:j+1) = '\\'
                j = j + 2
            else
                escaped(j:j) = c
                j = j + 1
            end if
        end do
        if (j <= len(escaped)) then
            escaped = adjustl(trim(escaped))
        end if
    end function json_escape

end module fx_test
