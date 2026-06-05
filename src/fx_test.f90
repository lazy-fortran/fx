module fx_test
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, &
                                             real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private :: json_escape, int_to_string, real_to_string

    integer, public, parameter :: initial_test_capacity = 64

    type, public :: test_result_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: message
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
        s%max_tests = initial_test_capacity
        if (allocated(s%tests)) deallocate(s%tests)
        allocate(s%tests(initial_test_capacity))
    end subroutine test_suite_init

    subroutine test_assert(s, condition, name, detail, file, line)
        type(test_suite_t), intent(inout) :: s
        logical, intent(in) :: condition
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: detail
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: message
        character(len=:), allocatable :: location

        message = ''
        if (present(detail)) then
            message = trim(detail)
        end if

        if (condition) then
            s%n_pass = s%n_pass + 1
            call test_record(s, name, '', .true.)
        else
            s%n_fail = s%n_fail + 1
            location = ''
            if (present(file) .and. present(line)) then
                location = ' (' // trim(file) // ':' // int_to_string(line) // ')'
            else if (present(file)) then
                location = ' (' // trim(file) // ')'
            else if (present(line)) then
                location = ' (' // int_to_string(line) // ')'
            end if
            write(error_unit, '(A)') 'FAIL: ' // trim(name) // location
            if (len_trim(message) > 0) then
                write(error_unit, '(A)') '  ' // trim(message)
            end if
            call test_record(s, name, message, .false.)
        end if
    end subroutine test_assert

    subroutine test_assert_equal_int(s, expected, actual, label, file, line)
        type(test_suite_t), intent(inout) :: s
        integer, intent(in) :: expected
        integer, intent(in) :: actual
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        character(len=:), allocatable :: msg

        msg = 'expected ' // int_to_string(expected) // &
              ', got ' // int_to_string(actual)
        call test_assert(s, expected == actual, trim(label), msg, file, line)
    end subroutine test_assert_equal_int

    subroutine test_assert_equal_str(s, expected, actual, label, file, line)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: expected
        character(len=*), intent(in) :: actual
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line
        character(len=:), allocatable :: msg

        msg = 'expected "' // trim(expected) // '", got "' // &
              trim(actual) // '"'
        call test_assert(s, trim(expected) == trim(actual), trim(label), msg, &
                         file, line)
    end subroutine test_assert_equal_str

    subroutine test_assert_equal_real(s, expected, actual, label, tol, file, &
                                      line)
        type(test_suite_t), intent(inout) :: s
        real(real64), intent(in) :: expected
        real(real64), intent(in) :: actual
        character(len=*), intent(in) :: label
        real(real64), intent(in), optional :: tol
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line

        real(real64) :: tolerance
        real(real64) :: diff
        logical :: pass
        logical :: tol_provided
        character(len=:), allocatable :: msg
        character(len=:), allocatable :: exp_str, act_str, diff_str, tol_str

        tol_provided = present(tol)
        if (tol_provided) then
            tolerance = tol
        else
            tolerance = 1.0e-10_real64
        end if

        diff = abs(expected - actual)
        pass = (diff < tolerance)

        exp_str = real_to_string(expected)
        act_str = real_to_string(actual)
        diff_str = real_to_string(diff)
        if (tol_provided) then
            tol_str = real_to_string(tolerance)
            msg = 'expected ' // exp_str // ', got ' // act_str // &
                  ', diff ' // diff_str // ', tol ' // tol_str
        else
            msg = 'expected ' // exp_str // ', got ' // act_str // &
                  ', diff ' // diff_str
        end if
        call test_assert(s, pass, trim(label), msg, file, line)
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

        character(len=:), allocatable :: pass_str, fail_str, total_str
        integer :: i
        character(len=:), allocatable :: test_json
        character(len=:), allocatable :: escaped_name
        character(len=:), allocatable :: escaped_msg
        character(len=:), allocatable :: test_status

        pass_str = int_to_string(s%n_pass)
        fail_str = int_to_string(s%n_fail)
        total_str = int_to_string(s%n_pass + s%n_fail)

        output = '{"suite":"' // json_escape(trim(s%name)) // '",'
        output = output // '"pass":' // pass_str
        output = output // ',"fail":' // fail_str
        output = output // ',"total":' // total_str
        output = output // ',"tests":['

        do i = 1, s%n_tests
            if (i > 1) output = output // ','
            if (s%tests(i)%status) then
                test_status = 'pass'
            else
                test_status = 'fail'
            end if

            escaped_name = json_escape(trim(s%tests(i)%name))
            if (trim(s%tests(i)%message) == '') then
                test_json = '{"name":"' // escaped_name // '","status":"' // &
                            test_status // '"}'
            else
                escaped_msg = json_escape( &
                    trim(s%tests(i)%message))
                test_json = '{"name":"' // escaped_name // '","status":"' // &
                            test_status // '","message":"' // escaped_msg // '"}'
            end if

            output = output // test_json
        end do

        output = output // ']}'
    end subroutine test_suite_to_json

    function json_escape(s) result(escaped)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: escaped
        integer :: i
        character(len=1) :: c

        escaped = ''
        do i = 1, len(s)
            c = s(i:i)
            if (c == '"') then
                escaped = escaped // '\"'
            elseif (c == '\') then
                escaped = escaped // '\\'
            else
                escaped = escaped // c
            end if
        end do
    end function json_escape

    function int_to_string(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buffer

        write(buffer, '(I0)') value
        text = trim(adjustl(buffer))
    end function int_to_string

    function real_to_string(value) result(text)
        real(real64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=64) :: buffer

        if (ieee_is_nan(value)) then
            text = 'NaN'
        else
            write(buffer, '(G0.17)') value
            text = trim(adjustl(buffer))
        end if
    end function real_to_string

end module fx_test
