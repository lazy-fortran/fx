program test_fx_test
    use, intrinsic :: iso_fortran_env, only: real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_assert, test_assert_equal_int, &
                       test_assert_equal_str, test_assert_equal_real, &
                       test_suite_summary, test_suite_exit, &
                       test_suite_to_json, initial_test_capacity
    implicit none

    character(len=256) :: mode

    mode = ''
    call get_command_argument(1, mode)
    if (trim(mode) == '--exit-fail') then
        block
            type(test_suite_t) :: exit_suite

            call test_suite_init(exit_suite, 'exit_failure')
            exit_suite%n_fail = 1
            call test_suite_exit(exit_suite)
        end block
    else
        call run_all_tests()
    end if

contains

    subroutine run_all_tests()
        type(test_suite_t) :: suite

        allocate(suite%tests(initial_test_capacity))
        deallocate(suite%tests)
        call test_suite_init(suite, 'fx_test')
        call test_init_zeros(suite)
        call test_assert_pass(suite)
        call test_assert_fail(suite)
        call test_suite_to_json_records_tests(suite)
        call test_suite_to_json_long_message(suite)
        call test_suite_to_json_nan_message(suite)
        call test_suite_record_growth(suite)
        call test_assert_equal_int_match(suite)
        call test_assert_equal_int_mismatch(suite)
        call test_assert_equal_int_negative(suite)
        call test_assert_equal_int_zero(suite)
        call test_assert_equal_int_message_format(suite)
        call test_assert_equal_str_match(suite)
        call test_assert_equal_str_mismatch(suite)
        call test_assert_equal_str_trailing_space(suite)
        call test_assert_equal_str_empty(suite)
        call test_assert_equal_str_message_format(suite)
        call test_assert_equal_real_match(suite)
        call test_assert_equal_real_default_tol(suite)
        call test_assert_equal_real_custom_tol(suite)
        call test_assert_equal_real_nan_expected(suite)
        call test_assert_equal_real_nan_actual(suite)
        call test_assert_equal_real_nan_both(suite)
        call test_assert_equal_real_negative(suite)
        call test_assert_equal_real_message_format(suite)
        call test_suite_summary(suite)
        call test_suite_to_json_valid(suite)
        call test_suite_to_json_empty(suite)
        call test_suite_to_json_format(suite)
        call test_no_assertions(suite)
        call test_suite_exit_failure_path(suite)
    end subroutine run_all_tests

    subroutine test_init_zeros(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s

        call test_suite_init(s, 'init_test')
        call test_assert(suite, s%n_pass == 0, 'n_pass is zero after init')
        call test_assert(suite, s%n_fail == 0, 'n_fail is zero after init')
    end subroutine test_init_zeros

    subroutine test_assert_pass(suite)
        type(test_suite_t), intent(inout) :: suite
        integer :: pass_before, fail_before

        pass_before = suite%n_pass
        fail_before = suite%n_fail

        call test_assert(suite, .true., 'assert_pass')

        call test_assert(suite, suite%n_pass == pass_before + 1, &
                         'n_pass incremented on true')
        call test_assert(suite, suite%n_fail == fail_before, &
                         'n_fail not incremented on true')
    end subroutine test_assert_pass

    subroutine test_assert_fail(suite)
        type(test_suite_t), intent(inout) :: suite
        integer :: pass_before, fail_before

        pass_before = suite%n_pass
        fail_before = suite%n_fail

        call test_assert(suite, .false., 'assert_fail')

        call test_assert(suite, suite%n_pass == pass_before, &
                         'n_pass not incremented on false')
        call test_assert(suite, suite%n_fail == fail_before + 1, &
                         'n_fail incremented on false')
    end subroutine test_assert_fail

    subroutine test_assert_equal_int_match(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, 42, 42, 'int_match')
        call test_assert_equal_int(suite, 0, 0, 'int_match_zero')
    end subroutine test_assert_equal_int_match

    subroutine test_assert_equal_int_mismatch(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, 42, 43, 'int_mismatch')
        call test_assert_equal_int(suite, 100, 0, 'int_mismatch_large_gap')
    end subroutine test_assert_equal_int_mismatch

    subroutine test_assert_equal_int_negative(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, -1, -1, 'int_negative_match')
        call test_assert_equal_int(suite, -42, -43, 'int_negative_mismatch')
        call test_assert_equal_int(suite, -1000000, -999999, &
                                   'int_large_negative_mismatch')
    end subroutine test_assert_equal_int_negative

    subroutine test_assert_equal_int_zero(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, 0, 0, 'int_zero_match')
        call test_assert_equal_int(suite, 0, 1, 'int_zero_mismatch')
    end subroutine test_assert_equal_int_zero

    subroutine test_assert_equal_int_message_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: expected_message

        call test_suite_init(s, 'int_message')
        call test_assert_equal_int(s, 42, 43, 'int_mismatch')

        expected_message = 'expected: 42' // new_line('a') // &
                           'actual:   43'
        call test_assert(suite, s%n_tests == 1, 'int_message_recorded')
        call test_assert(suite, s%tests(1)%message == expected_message, &
                         'int_message_exact')
    end subroutine test_assert_equal_int_message_format

    subroutine test_assert_equal_str_match(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'hello', 'hello', 'str_match')
        call test_assert_equal_str(suite, '', '', 'str_match_empty')
    end subroutine test_assert_equal_str_match

    subroutine test_assert_equal_str_mismatch(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'foo', 'bar', 'str_mismatch')
        call test_assert_equal_str(suite, 'hello world', 'hello there', &
                                   'str_mismatch_partial')
    end subroutine test_assert_equal_str_mismatch

    subroutine test_assert_equal_str_trailing_space(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'hello', 'hello   ', &
                                   'str_trailing_space_pass')
        call test_assert_equal_str(suite, 'hello   ', 'hello', &
                                   'str_trailing_space_pass_rev')
    end subroutine test_assert_equal_str_trailing_space

    subroutine test_assert_equal_str_empty(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, '', '   ', 'str_empty_vs_spaces')
        call test_assert_equal_str(suite, '  ', '', 'str_spaces_vs_empty')
    end subroutine test_assert_equal_str_empty

    subroutine test_assert_equal_str_message_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: expected_message

        call test_suite_init(s, 'str_message')
        call test_assert_equal_str(s, 'foo', 'bar', 'str_mismatch')

        expected_message = 'expected: "foo"' // new_line('a') // &
                           'actual:   "bar"'
        call test_assert(suite, s%n_tests == 1, 'str_message_recorded')
        call test_assert(suite, s%tests(1)%message == expected_message, &
                         'str_message_exact')
    end subroutine test_assert_equal_str_message_format

    subroutine test_assert_equal_real_match(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_real(suite, 1.0_real64, 1.0_real64, &
                                   'real_match', 1.0e-10_real64)
        call test_assert_equal_real(suite, 0.0_real64, 0.0_real64, &
                                   'real_match_zero', 1.0e-10_real64)
        call test_assert_equal_real(suite, -3.14_real64, -3.14_real64, &
                                   'real_match_negative', 1.0e-10_real64)
    end subroutine test_assert_equal_real_match

    subroutine test_assert_equal_real_default_tol(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_real(suite, 1.0_real64, &
                                   1.0_real64 + 1.0e-11_real64, &
                                   'real_default_tol_pass')
        call test_assert_equal_real(suite, 1.0_real64, &
                                   1.0_real64 + 1.0e-9_real64, &
                                   'real_default_tol_fail')
    end subroutine test_assert_equal_real_default_tol

    subroutine test_assert_equal_real_custom_tol(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_real(suite, 1.0_real64, &
                                   1.0_real64 + 1.0e-8_real64, &
                                   'real_custom_tol_pass', 1.0e-7_real64)
        call test_assert_equal_real(suite, 1.0_real64, &
                                   1.0_real64 + 1.0e-7_real64, &
                                   'real_custom_tol_fail', 1.0e-8_real64)
    end subroutine test_assert_equal_real_custom_tol

    subroutine test_assert_equal_real_nan_expected(suite)
        type(test_suite_t), intent(inout) :: suite
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_assert_equal_real(suite, nan_val, 1.0_real64, &
                                   'real_nan_expected')
    end subroutine test_assert_equal_real_nan_expected

    subroutine test_assert_equal_real_nan_actual(suite)
        type(test_suite_t), intent(inout) :: suite
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_assert_equal_real(suite, 1.0_real64, nan_val, &
                                   'real_nan_actual')
    end subroutine test_assert_equal_real_nan_actual

    subroutine test_assert_equal_real_nan_both(suite)
        type(test_suite_t), intent(inout) :: suite
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_assert_equal_real(suite, nan_val, nan_val, &
                                   'real_nan_both')
    end subroutine test_assert_equal_real_nan_both

    subroutine test_assert_equal_real_negative(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_real(suite, -1.0_real64, -1.0_real64, &
                                   'real_negative_match', 1.0e-10_real64)
        call test_assert_equal_real(suite, -1.0_real64, -2.0_real64, &
                                   'real_negative_mismatch', 1.0e-10_real64)
    end subroutine test_assert_equal_real_negative

    subroutine test_assert_equal_real_message_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: expected_message

        call test_suite_init(s, 'real_message')
        call test_assert_equal_real(s, 1.5_real64, 1.25_real64, &
                                    'real_mismatch')

        expected_message = 'expected: 1.50000000000000' // new_line('a') // &
                           'actual:   1.25000000000000' // new_line('a') // &
                           'diff:     2.50E-01 (tol: 1.00E-10)'
        call test_assert(suite, s%n_tests == 1, 'real_message_recorded')
        call test_assert(suite, s%tests(1)%message == expected_message, &
                         'real_message_exact')
    end subroutine test_assert_equal_real_message_format

    subroutine test_suite_to_json_valid(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: json

        call test_suite_init(suite, 'json_test')
        call test_assert(suite, .true., 'json_pass_1')
        call test_assert(suite, .true., 'json_pass_2')
        call test_assert(suite, .false., 'json_fail_1')
        call test_suite_to_json(suite, json)

        call test_assert(suite, index(json, '"suite":"json_test"') > 0, &
                         'json_contains_suite')
    end subroutine test_suite_to_json_valid

    subroutine test_suite_to_json_empty(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: json

        call test_suite_init(suite, '')
        call test_suite_to_json(suite, json)

        call test_assert(suite, index(json, '""') > 0, &
                         'json_empty_name_valid')
    end subroutine test_suite_to_json_empty

    subroutine test_suite_to_json_format(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: json

        call test_suite_init(suite, 'fmt_test')
        call test_assert(suite, .true., 'fmt_pass')
        call test_assert(suite, .false., 'fmt_fail')
        call test_suite_to_json(suite, json)

        call test_assert(suite, index(json, '"pass":') > 0, &
                         'json_has_pass_key')
        call test_assert(suite, index(json, '"fail":') > 0, &
                         'json_has_fail_key')
        call test_assert(suite, index(json, '"total":') > 0, &
                         'json_has_total_key')
        call test_assert(suite, index(json, '"passed"') == 0, &
                         'json_no_passed_key')
        call test_assert(suite, index(json, '"failed"') == 0, &
                         'json_no_failed_key')
    end subroutine test_suite_to_json_format

    subroutine test_suite_to_json_records_tests(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: json

        call test_suite_init(s, 'json_recording')
        call test_assert(s, .true., 'json_pass')
        call test_assert(s, .false., 'json_fail')
        call test_assert_equal_int(s, 5, 3, 'json_int_fail')
        call test_suite_to_json(s, json)

        call test_assert(suite, s%n_tests == 3, 'json_records_test_count')
        call test_assert(suite, index(json, '"tests":[') > 0, &
                         'json_has_tests_array')
        call test_assert(suite, index(json, &
                         '"name":"json_pass","status":"pass"') > 0, &
                         'json_records_pass_case')
        call test_assert(suite, index(json, &
                         '"name":"json_fail","status":"fail"') > 0, &
                         'json_records_fail_case')
        call test_assert(suite, index(json, &
                         '"message":"expected: 5\nactual:   3"') > 0, &
                         'json_records_int_message')
    end subroutine test_suite_to_json_records_tests

    subroutine test_suite_to_json_long_message(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: json
        character(len=:), allocatable :: expected
        character(len=:), allocatable :: actual
        character(len=*), parameter :: chunk = 'abc"def\ghi'

        expected = repeat(chunk, 24)
        actual = expected // 'x'

        call test_suite_init(s, 'long_message')
        call test_assert_equal_str(s, expected, actual, 'long_message_case')
        call test_suite_to_json(s, json)

        call test_assert(suite, s%n_tests == 1, 'long_message_recorded')
        call test_assert(suite, len_trim(s%tests(1)%message) > 256, &
                         'long_message_not_truncated')
        call test_assert(suite, index(json, '\"') > 0, &
                         'json_escapes_quotes')
        call test_assert(suite, index(json, '\\') > 0, &
                         'json_escapes_backslashes')
        call test_assert(suite, index(json, '\n') > 0, &
                         'json_escapes_newlines')
    end subroutine test_suite_to_json_long_message

    subroutine test_suite_to_json_nan_message(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: json
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_suite_init(s, 'nan_message')
        call test_assert_equal_real(s, nan_val, 1.0_real64, &
                                   'nan_message_case')
        call test_suite_to_json(s, json)

        call test_assert(suite, index(json, 'NaN') > 0, &
                         'json_records_nan_literal')
    end subroutine test_suite_to_json_nan_message

    subroutine test_suite_record_growth(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        integer :: i
        character(len=32) :: label

        call test_suite_init(s, 'growth_test')
        do i = 1, initial_test_capacity + 6
            write(label, '(A,I0)') 'grow_', i
            call test_assert(s, mod(i, 2) == 0, trim(label))
        end do

        call test_assert(suite, s%n_tests == initial_test_capacity + 6, &
                         'record_growth_count')
        call test_assert(suite, size(s%tests) >= s%n_tests, &
                         'record_growth_capacity')
        call test_assert(suite, s%tests(1)%name == 'grow_1', &
                         'record_growth_first_name')
        call test_assert(suite, s%tests(initial_test_capacity + 6)%name == &
                         'grow_70', 'record_growth_last_name')
    end subroutine test_suite_record_growth

    subroutine test_no_assertions(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: json

        call test_suite_init(s, 'empty_suite')
        call test_suite_summary(s)
        call test_suite_to_json(s, json)
        call test_suite_exit(s)

        call test_assert(suite, s%n_pass == 0, 'empty_suite_n_pass')
        call test_assert(suite, s%n_fail == 0, 'empty_suite_n_fail')
    end subroutine test_no_assertions

    subroutine test_suite_exit_failure_path(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=256) :: exe
        integer :: exit_code
        integer :: cmdstat
        character(len=256) :: cmdmsg

        exe = ''
        call get_command_argument(0, exe)
        call execute_command_line(trim(exe) // ' --exit-fail', &
                                  exitstat=exit_code, cmdstat=cmdstat, &
                                  cmdmsg=cmdmsg)
        call test_assert(suite, cmdstat == 0, 'exit_failure_spawn_command', &
                         trim(cmdmsg))
        if (cmdstat == 0) then
            call test_assert(suite, exit_code == 1, 'exit_failure_exit_code')
        end if
    end subroutine test_suite_exit_failure_path

end program test_fx_test
