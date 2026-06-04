module fx_test
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, &
                                             real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private

    type, public :: test_suite_t
        character(len=128) :: name = ''
        integer :: n_pass = 0
        integer :: n_fail = 0
    end type test_suite_t

    public :: test_suite_init, test_assert
    public :: test_assert_equal_int, test_assert_equal_str
    public :: test_assert_equal_real
    public :: test_suite_summary, test_suite_exit
    public :: test_suite_to_json

contains

    subroutine test_suite_init(s, name)
        type(test_suite_t), intent(out) :: s
        character(len=*), intent(in) :: name

        s%name = trim(name)
        s%n_pass = 0
        s%n_fail = 0
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
        character(len=16) :: exp_str, act_str

        write(exp_str, '(I0)') expected
        write(act_str, '(I0)') actual
        exp_str = adjustl(trim(exp_str))
        act_str = adjustl(trim(act_str))

        if (expected == actual) then
            call test_assert(s, .true., trim(label))
        else
       write(msg, '(A,A,I0,A,I0,A)') &
            trim(label), ' (expected ', expected, &
            ', got ', actual, ')'
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
                write(msg, '(A,A,A,G0,A,G0,A,G0,A,G0,A)') &
                    trim(label), ' (expected ', expected, &
                    ', got ', actual, ', diff ', diff, &
                    ', tol ', tolerance, ')'
            else
                write(msg, '(A,A,A,G0,A,G0,A,G0,A)') &
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

    subroutine test_suite_to_json(s, output)
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: output

        character(len=32) :: pass_str, fail_str, total_str

        write(pass_str, '(I0)') s%n_pass
        write(fail_str, '(I0)') s%n_fail
        write(total_str, '(I0)') s%n_pass + s%n_fail

        pass_str = adjustl(trim(pass_str))
        fail_str = adjustl(trim(fail_str))
        total_str = adjustl(trim(total_str))

        output = '{"name":"' // trim(s%name) // &
                 '","pass":' // pass_str // &
                 ',"fail":' // fail_str // &
                 ',"total":' // total_str // &
                 '}'
    end subroutine test_suite_to_json

end module fx_test
