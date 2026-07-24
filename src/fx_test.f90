module fx_test
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit, &
        real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    private :: json_escape, int_to_string, real_to_string, &
        real_scientific_to_string, location_suffix, &
        write_prefixed_lines

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
        logical :: is_fixture = .false.
        type(test_result_t), allocatable, public :: tests(:)
    end type test_suite_t

    public :: test_suite_init, test_assert
    public :: test_assert_equal_int, test_assert_equal_str
    public :: test_assert_equal_real
    public :: test_suite_summary, test_suite_summary_line, test_suite_exit
    public :: test_suite_to_json
    public :: test_record

contains

    subroutine test_suite_init(s, name, fixture)
        !! A fixture suite is one driven by a test as data, not one whose
        !! outcome is the test's own. It records results exactly like a normal
        !! suite so the caller can inspect them, but emits no failure lines and
        !! no summary line, so its deliberate failures stay out of the reported
        !! counts that tooling parses.
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: name
        logical, intent(in), optional :: fixture

        s%name = trim(name)
        s%n_pass = 0
        s%n_fail = 0
        s%n_tests = 0
        s%max_tests = initial_test_capacity
        s%is_fixture = .false.
        if (present(fixture)) s%is_fixture = fixture
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
            if (.not. s%is_fixture) then
                location = location_suffix(file, line)
                write(error_unit, '(A)') 'FAIL: ' // trim(name) // location
                if (len_trim(message) > 0) then
                    call write_prefixed_lines(error_unit, message)
                end if
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

        msg = 'expected: ' // int_to_string(expected) // new_line('a') // &
            'actual:   ' // int_to_string(actual)
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

        msg = 'expected: "' // trim(expected) // '"' // new_line('a') // &
            'actual:   "' // trim(actual) // '"'
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
        diff_str = real_scientific_to_string(diff)
        tol_str = real_scientific_to_string(tolerance)
        msg = 'expected: ' // exp_str // new_line('a') // &
            'actual:   ' // act_str // new_line('a') // &
            'diff:     ' // diff_str // ' (tol: ' // tol_str // ')'
        call test_assert(s, pass, trim(label), msg, file, line)
    end subroutine test_assert_equal_real

    subroutine test_suite_summary(s)
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable :: line

        if (s%is_fixture) return
        call test_suite_summary_line(s, line)
        write(output_unit, '(A)') line
    end subroutine test_suite_summary

    subroutine test_suite_summary_line(s, line)
        !! Render the summary line without emitting it, so the exact text that
        !! downstream tooling parses can be asserted on in a test.
        type(test_suite_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: line

        line = trim(s%name) // ': ' // int_to_string(s%n_pass) // ' pass, ' // &
            int_to_string(s%n_fail) // ' fail, ' // &
            int_to_string(s%n_pass + s%n_fail) // ' total'
    end subroutine test_suite_summary_line

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

    function location_suffix(file, line) result(location)
        character(len=*), intent(in), optional :: file
        integer, intent(in), optional :: line
        character(len=:), allocatable :: location

        location = ''
        if (present(file) .and. present(line)) then
            location = ' (' // trim(file) // ':' // int_to_string(line) // ')'
        else if (present(file)) then
            location = ' (' // trim(file) // ')'
        else if (present(line)) then
            location = ' (' // int_to_string(line) // ')'
        end if
    end function location_suffix

    subroutine write_prefixed_lines(unit, text)
        integer, intent(in) :: unit
        character(len=*), intent(in) :: text
        integer :: start
        integer :: newline_pos
        integer :: line_end

        start = 1
        do while (start <= len(text))
            newline_pos = index(text(start:), new_line('a'))
            if (newline_pos == 0) then
                write(unit, '(A)') '  ' // text(start:)
                exit
            end if

            line_end = start + newline_pos - 2
            if (line_end >= start) then
                write(unit, '(A)') '  ' // text(start:line_end)
            else
                write(unit, '(A)') '  '
            end if
            start = line_end + 2
        end do
    end subroutine write_prefixed_lines

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
            elseif (c == achar(10)) then
                escaped = escaped // '\n'
            elseif (c == achar(13)) then
                escaped = escaped // '\r'
            elseif (c == achar(9)) then
                escaped = escaped // '\t'
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
            write(buffer, '(F0.14)') value
            text = trim(adjustl(buffer))
        end if
    end function real_to_string

    function real_scientific_to_string(value) result(text)
        real(real64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=64) :: buffer

        if (ieee_is_nan(value)) then
            text = 'NaN'
        else
            write(buffer, '(ES12.2E2)') value
            text = trim(adjustl(buffer))
        end if
    end function real_scientific_to_string

end module fx_test
