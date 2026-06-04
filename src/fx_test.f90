module fx_test
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, &
                                             real64
    implicit none
    private

    type, public :: test_suite_t
        character(len=128) :: name = ' '
        integer :: n_pass = 0
        integer :: n_fail = 0
        integer :: n_total = 0
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
        s%n_total = 0
    end subroutine test_suite_init

    subroutine test_assert(s, condition, message)
        type(test_suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message

        s%n_total = s%n_total + 1
        if (condition) then
            s%n_pass = s%n_pass + 1
            write(output_unit, '(A)') '  PASS: ' // trim(message)
        else
            s%n_fail = s%n_fail + 1
            write(error_unit, '(A)') '  FAIL: ' // trim(message)
        end if
    end subroutine test_assert

    subroutine test_assert_equal_int(s, expected, actual, message)
        type(test_suite_t), intent(inout) :: s
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message

        call test_assert(s, expected == actual, &
                         trim(message) // ' (expected ' // &
                         trim(adjustl(transfer(expected, '        '))) // &
                         ', got ' // &
                         trim(adjustl(transfer(actual, '        '))) // ')')
    end subroutine test_assert_equal_int

    subroutine test_assert_equal_str(s, expected, actual, message)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message

        call test_assert(s, trim(expected) == trim(actual), &
                         trim(message) // &
                         ' (expected "' // trim(expected) // '"' // &
                         ', got "' // trim(actual) // '")')
    end subroutine test_assert_equal_str

    subroutine test_assert_equal_real(s, expected, actual, tol, &
            message)
        type(test_suite_t), intent(inout) :: s
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: actual
        real(real64), intent(in) :: tol
        character(len=*), intent(in) :: message

        call test_assert(s, abs(expected - actual) <= tol, &
                         trim(message) // &
                         ' (expected ' // trim(adjustl(transfer(expected, '            '))) // &
                         ', got ' // trim(adjustl(transfer(actual, '            '))) // &
                         ', tol ' // trim(adjustl(transfer(tol, '            '))) // ')')
    end subroutine test_assert_equal_real

    subroutine test_suite_summary(s)
        type(test_suite_t), intent(in) :: s

        write(output_unit, '(A)') ''
        write(output_unit, '(A,I0,A)') 'Test suite: ' // trim(s%name)
        write(output_unit, '(A,I0,A,I0,A,I0,A)') &
            '  Results: ', s%n_pass, ' passed, ', &
            s%n_fail, ' failed, ', s%n_total, ' total'
    end subroutine test_suite_summary

    subroutine test_suite_exit(s)
        type(test_suite_t), intent(in) :: s

        if (s%n_fail > 0) then
            write(error_unit, '(A)') 'TEST FAILURE'
            error stop 'test_suite_exit: ' // trim(s%name) // ' had failures'
        end if
        write(output_unit, '(A)') 'All tests passed.'
    end subroutine test_suite_exit

    subroutine test_suite_to_json(s, output)
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: output

        output = '{"name":"' // trim(s%name) // &
                 '","passed":' // trim(adjustl(transfer(s%n_pass, '        '))) // &
                 ',"failed":' // trim(adjustl(transfer(s%n_fail, '        '))) // &
                 ',"total":' // trim(adjustl(transfer(s%n_total, '        '))) // &
                 '}'
    end subroutine test_suite_to_json

end module fx_test
