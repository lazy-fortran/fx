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
        error stop "fx_test:test_suite_init not implemented"
    end subroutine test_suite_init

    subroutine test_assert(s, condition, message)
        type(test_suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        error stop "fx_test:test_assert not implemented"
    end subroutine test_assert

    subroutine test_assert_equal_int(s, expected, actual, message)
        type(test_suite_t), intent(inout) :: s
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: message
        error stop "fx_test:test_assert_equal_int not implemented"
    end subroutine test_assert_equal_int

    subroutine test_assert_equal_str(s, expected, actual, message)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: message
        error stop "fx_test:test_assert_equal_str not implemented"
    end subroutine test_assert_equal_str

    subroutine test_assert_equal_real(s, expected, actual, tol, &
            message)
        type(test_suite_t), intent(inout) :: s
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: actual
        real(real64), intent(in) :: tol
        character(len=*), intent(in) :: message
        error stop "fx_test:test_assert_equal_real not implemented"
    end subroutine test_assert_equal_real

    subroutine test_suite_summary(s)
        type(test_suite_t), intent(in) :: s
        error stop "fx_test:test_suite_summary not implemented"
    end subroutine test_suite_summary

    subroutine test_suite_exit(s)
        type(test_suite_t), intent(in) :: s
        error stop "fx_test:test_suite_exit not implemented"
    end subroutine test_suite_exit

    subroutine test_suite_to_json(s, output)
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: output
        error stop "fx_test:test_suite_to_json not implemented"
    end subroutine test_suite_to_json

end module fx_test
