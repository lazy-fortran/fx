program test_fx_test
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, &
        c_null_char, c_associated
    use, intrinsic :: iso_fortran_env, only: real64, error_unit, output_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fx_mcp_test_os, only: fx_test_spawn_argv, fx_test_read, fx_test_close
    use fx_test, only: test_suite_t, test_suite_init, &
        test_assert, test_assert_equal_int, &
        test_assert_equal_str, test_assert_equal_real, &
        test_suite_summary, test_suite_summary_line, test_suite_exit, &
        test_suite_to_json, initial_test_capacity
    implicit none

    character(len=256) :: mode

    mode = ''
    call get_command_argument(1, mode)
    select case (trim(mode))
    case ('--exit-fail')
        block
            type(test_suite_t) :: exit_suite

            call test_suite_init(exit_suite, 'exit_failure')
            exit_suite%n_fail = 1
            call test_suite_exit(exit_suite)
        end block
    case ('--emit-fixture')
        call emit_noise(.true.)
    case ('--emit-normal')
        call emit_noise(.false.)
    case default
        call run_all_tests()
    end select

contains

    subroutine emit_noise(fixture)
        !! Drive one failing assertion and a summary through a suite of the
        !! requested kind. The parent process captures both output streams.
        logical, intent(in) :: fixture
        type(test_suite_t) :: s

        call test_suite_init(s, 'noise_suite', fixture=fixture)
        call test_assert(s, .false., 'noise_fail')
        call test_suite_summary(s)
    end subroutine emit_noise

    subroutine run_all_tests()
        type(test_suite_t) :: suite
        integer :: bootstrap_fail

        call run_bootstrap_checks(bootstrap_fail)

        allocate(suite%tests(initial_test_capacity))
        deallocate(suite%tests)
        call test_suite_init(suite, 'fx_test')
        call test_init_zeros(suite)
        call test_assert_pass(suite)
        call test_assert_fail(suite)
        call test_fixture_stays_out_of_outer_counts(suite)
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
        call test_summary_line_text(suite)
        call test_suite_to_json_valid(suite)
        call test_suite_to_json_empty(suite)
        call test_suite_to_json_format(suite)
        call test_no_assertions(suite)
        call test_fixture_emits_nothing(suite)
        call test_normal_suite_emits_failure(suite)
        call test_suite_exit_failure_path(suite)
        call test_suite_summary(suite)
        call report_bootstrap_failures(bootstrap_fail)
        call test_suite_exit(suite)
    end subroutine run_all_tests

    subroutine run_bootstrap_checks(n_fail)
        !! Everything below this point asserts through test_assert, so it can
        !! only report a defect that test_assert itself does not swallow: a
        !! test_assert that never records a failure would silently turn every
        !! check of it into a pass. These few checks therefore compare suite
        !! state with plain Fortran and keep their own counter, giving the
        !! self-test an oracle independent of the code under test.
        integer, intent(out) :: n_fail
        type(test_suite_t) :: f

        n_fail = 0

        call test_suite_init(f, 'bootstrap_fixture', fixture=.true.)

        call test_assert(f, .true., 'bootstrap_true')
        call bootstrap_check(f%n_pass == 1, 'true assertion counted as pass', &
            n_fail)
        call bootstrap_check(f%n_fail == 0, 'true assertion not counted as fail', &
            n_fail)

        call test_assert(f, .false., 'bootstrap_false')
        call bootstrap_check(f%n_fail == 1, 'false assertion counted as fail', &
            n_fail)
        call bootstrap_check(f%n_pass == 1, 'false assertion not counted as pass', &
            n_fail)
        call bootstrap_check(f%n_tests == 2, 'both assertions recorded', n_fail)
        if (f%n_tests == 2) then
            call bootstrap_check(.not. f%tests(2)%status, &
                'false assertion recorded with fail status', n_fail)
        end if
    end subroutine run_bootstrap_checks

    subroutine bootstrap_check(ok, label, n_fail)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: label
        integer, intent(inout) :: n_fail

        if (.not. ok) then
            n_fail = n_fail + 1
            write (error_unit, '(A)') 'BOOTSTRAP FAIL: ' // label
        end if
    end subroutine bootstrap_check

    subroutine report_bootstrap_failures(n_fail)
        !! Report bootstrap failures in the same parseable shape as a suite so
        !! the build tool sees them, and stop before the normal exit path,
        !! which relies on the very counters a bootstrap failure discredits.
        integer, intent(in) :: n_fail
        character(len=64) :: line

        if (n_fail <= 0) return
        write (line, '(A,I0,A,I0,A)') 'fx_test_bootstrap: 0 pass, ', n_fail, &
            ' fail, ', n_fail, ' total'
        write (output_unit, '(A)') trim(line)
        stop 1
    end subroutine report_bootstrap_failures

    function last_failed(f) result(ok)
        !! True when the newest result recorded by fixture f is a failure.
        type(test_suite_t), intent(in) :: f
        logical :: ok

        ok = .false.
        if (f%n_tests >= 1) then
            ok = .not. f%tests(f%n_tests)%status
        end if
    end function last_failed

    function last_passed(f) result(ok)
        type(test_suite_t), intent(in) :: f
        logical :: ok

        ok = .false.
        if (f%n_tests >= 1) then
            ok = f%tests(f%n_tests)%status
        end if
    end function last_passed

    subroutine test_init_zeros(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s

        call test_suite_init(s, 'init_test')
        call test_assert(suite, s%n_pass == 0, 'n_pass is zero after init')
        call test_assert(suite, s%n_fail == 0, 'n_fail is zero after init')
        call test_assert(suite, .not. s%is_fixture, &
            'suite is not a fixture by default')
        call test_suite_init(s, 'init_fixture', fixture=.true.)
        call test_assert(suite, s%is_fixture, 'fixture flag set by init')
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
        type(test_suite_t) :: f

        call test_suite_init(f, 'assert_fail_fixture', fixture=.true.)
        call test_assert(f, .false., 'assert_fail')

        call test_assert(suite, f%n_fail == 1, 'n_fail incremented on false')
        call test_assert(suite, f%n_pass == 0, 'n_pass not incremented on false')
        call test_assert(suite, f%n_tests == 1, 'failure recorded as a result')
        call test_assert(suite, last_failed(f), &
            'failure recorded with status fail')
        if (f%n_tests == 1) then
            call test_assert(suite, f%tests(1)%name == 'assert_fail', &
                'failure recorded under its name')
        end if
    end subroutine test_assert_fail

    subroutine test_fixture_stays_out_of_outer_counts(suite)
        !! The point of the fixture flag: failures driven into a fixture suite
        !! are counted there and nowhere else, so the outer suite's reported
        !! numbers stay a truthful signal.
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        integer :: pass_before, fail_before, tests_before

        pass_before = suite%n_pass
        fail_before = suite%n_fail
        tests_before = suite%n_tests

        call test_suite_init(f, 'isolation_fixture', fixture=.true.)
        call test_assert(f, .false., 'isolated_fail_1')
        call test_assert_equal_int(f, 1, 2, 'isolated_fail_2')

        call test_assert(suite, f%n_fail == 2, 'fixture counts its own failures')
        call test_assert(suite, suite%n_fail == fail_before, &
            'outer fail count untouched by fixture')
        call test_assert(suite, suite%n_pass == pass_before + 2, &
            'outer pass count advanced only by outer asserts')
        call test_assert(suite, suite%n_tests == tests_before + 3, &
            'outer results advanced only by outer asserts')
    end subroutine test_fixture_stays_out_of_outer_counts

    subroutine test_assert_equal_int_match(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, 42, 42, 'int_match')
        call test_assert_equal_int(suite, 0, 0, 'int_match_zero')
    end subroutine test_assert_equal_int_match

    subroutine test_assert_equal_int_mismatch(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f

        call test_suite_init(f, 'int_mismatch_fixture', fixture=.true.)
        call test_assert_equal_int(f, 42, 43, 'int_mismatch')
        call test_assert(suite, last_failed(f), 'int_mismatch_detected')
        call test_assert_equal_int(f, 100, 0, 'int_mismatch_large_gap')
        call test_assert(suite, last_failed(f), 'int_mismatch_large_gap_detected')
        call test_assert(suite, f%n_fail == 2, 'int_mismatch_fail_count')
        call test_assert(suite, f%n_pass == 0, 'int_mismatch_pass_count')
    end subroutine test_assert_equal_int_mismatch

    subroutine test_assert_equal_int_negative(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f

        call test_assert_equal_int(suite, -1, -1, 'int_negative_match')

        call test_suite_init(f, 'int_negative_fixture', fixture=.true.)
        call test_assert_equal_int(f, -42, -43, 'int_negative_mismatch')
        call test_assert(suite, last_failed(f), 'int_negative_mismatch_detected')
        call test_assert_equal_int(f, -1000000, -999999, &
            'int_large_negative_mismatch')
        call test_assert(suite, last_failed(f), &
            'int_large_negative_mismatch_detected')
        call test_assert(suite, f%n_fail == 2, 'int_negative_fail_count')
    end subroutine test_assert_equal_int_negative

    subroutine test_assert_equal_int_zero(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f

        call test_assert_equal_int(suite, 0, 0, 'int_zero_match')

        call test_suite_init(f, 'int_zero_fixture', fixture=.true.)
        call test_assert_equal_int(f, 0, 1, 'int_zero_mismatch')
        call test_assert(suite, last_failed(f), 'int_zero_mismatch_detected')
        call test_assert(suite, f%n_fail == 1, 'int_zero_fail_count')
    end subroutine test_assert_equal_int_zero

    subroutine test_assert_equal_int_message_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: expected_message

        call test_suite_init(s, 'int_message', fixture=.true.)
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
        type(test_suite_t) :: f

        call test_suite_init(f, 'str_mismatch_fixture', fixture=.true.)
        call test_assert_equal_str(f, 'foo', 'bar', 'str_mismatch')
        call test_assert(suite, last_failed(f), 'str_mismatch_detected')
        call test_assert_equal_str(f, 'hello world', 'hello there', &
            'str_mismatch_partial')
        call test_assert(suite, last_failed(f), 'str_mismatch_partial_detected')
        call test_assert(suite, f%n_fail == 2, 'str_mismatch_fail_count')
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

        call test_suite_init(s, 'str_message', fixture=.true.)
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
        type(test_suite_t) :: f

        call test_suite_init(f, 'real_default_tol_fixture', fixture=.true.)
        call test_assert_equal_real(f, 1.0_real64, &
            1.0_real64 + 1.0e-11_real64, &
            'real_default_tol_pass')
        call test_assert(suite, last_passed(f), 'real_default_tol_pass_detected')
        call test_assert_equal_real(f, 1.0_real64, &
            1.0_real64 + 1.0e-9_real64, &
            'real_default_tol_fail')
        call test_assert(suite, last_failed(f), 'real_default_tol_fail_detected')
    end subroutine test_assert_equal_real_default_tol

    subroutine test_assert_equal_real_custom_tol(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f

        call test_suite_init(f, 'real_custom_tol_fixture', fixture=.true.)
        call test_assert_equal_real(f, 1.0_real64, &
            1.0_real64 + 1.0e-8_real64, &
            'real_custom_tol_pass', 1.0e-7_real64)
        call test_assert(suite, last_passed(f), 'real_custom_tol_pass_detected')
        call test_assert_equal_real(f, 1.0_real64, &
            1.0_real64 + 1.0e-7_real64, &
            'real_custom_tol_fail', 1.0e-8_real64)
        call test_assert(suite, last_failed(f), 'real_custom_tol_fail_detected')
    end subroutine test_assert_equal_real_custom_tol

    subroutine test_assert_equal_real_nan_expected(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_suite_init(f, 'real_nan_expected_fixture', fixture=.true.)
        call test_assert_equal_real(f, nan_val, 1.0_real64, &
            'real_nan_expected')
        call test_assert(suite, last_failed(f), 'real_nan_expected_detected')
    end subroutine test_assert_equal_real_nan_expected

    subroutine test_assert_equal_real_nan_actual(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_suite_init(f, 'real_nan_actual_fixture', fixture=.true.)
        call test_assert_equal_real(f, 1.0_real64, nan_val, &
            'real_nan_actual')
        call test_assert(suite, last_failed(f), 'real_nan_actual_detected')
    end subroutine test_assert_equal_real_nan_actual

    subroutine test_assert_equal_real_nan_both(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        real(real64) :: nan_val

        nan_val = ieee_value(nan_val, ieee_quiet_nan)
        call test_suite_init(f, 'real_nan_both_fixture', fixture=.true.)
        call test_assert_equal_real(f, nan_val, nan_val, &
            'real_nan_both')
        call test_assert(suite, last_failed(f), 'real_nan_both_detected')
    end subroutine test_assert_equal_real_nan_both

    subroutine test_assert_equal_real_negative(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f

        call test_assert_equal_real(suite, -1.0_real64, -1.0_real64, &
            'real_negative_match', 1.0e-10_real64)

        call test_suite_init(f, 'real_negative_fixture', fixture=.true.)
        call test_assert_equal_real(f, -1.0_real64, -2.0_real64, &
            'real_negative_mismatch', 1.0e-10_real64)
        call test_assert(suite, last_failed(f), 'real_negative_mismatch_detected')
    end subroutine test_assert_equal_real_negative

    subroutine test_assert_equal_real_message_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: expected_message

        call test_suite_init(s, 'real_message', fixture=.true.)
        call test_assert_equal_real(s, 1.5_real64, 1.25_real64, &
            'real_mismatch')

        expected_message = 'expected: 1.50000000000000' // new_line('a') // &
            'actual:   1.25000000000000' // new_line('a') // &
            'diff:     2.50E-01 (tol: 1.00E-10)'
        call test_assert(suite, s%n_tests == 1, 'real_message_recorded')
        call test_assert(suite, s%tests(1)%message == expected_message, &
            'real_message_exact')
    end subroutine test_assert_equal_real_message_format

    subroutine test_summary_line_text(suite)
        !! The summary line is the machine-readable contract with the build
        !! tool, so assert its exact text and not merely that it prints.
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        character(len=:), allocatable :: line

        call test_suite_init(f, 'summary_fixture', fixture=.true.)
        call test_assert(f, .true., 'summary_pass')
        call test_assert(f, .false., 'summary_fail')
        call test_suite_summary_line(f, line)
        call test_assert(suite, &
            line == 'summary_fixture: 1 pass, 1 fail, 2 total', &
            'summary_line_exact', 'actual: ' // line)
    end subroutine test_summary_line_text

    subroutine test_suite_to_json_valid(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        character(len=:), allocatable :: json

        call test_suite_init(f, 'json_test', fixture=.true.)
        call test_assert(f, .true., 'json_pass_1')
        call test_assert(f, .true., 'json_pass_2')
        call test_assert(f, .false., 'json_fail_1')
        call test_suite_to_json(f, json)

        call test_assert(suite, index(json, '"suite":"json_test"') > 0, &
            'json_contains_suite')
        call test_assert(suite, index(json, '"pass":2') > 0, &
            'json_reports_pass_count')
        call test_assert(suite, index(json, '"fail":1') > 0, &
            'json_reports_fail_count')
    end subroutine test_suite_to_json_valid

    subroutine test_suite_to_json_empty(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        character(len=:), allocatable :: json

        call test_suite_init(f, '', fixture=.true.)
        call test_suite_to_json(f, json)

        call test_assert(suite, index(json, '""') > 0, &
            'json_empty_name_valid')
    end subroutine test_suite_to_json_empty

    subroutine test_suite_to_json_format(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: f
        character(len=:), allocatable :: json

        call test_suite_init(f, 'fmt_test', fixture=.true.)
        call test_assert(f, .true., 'fmt_pass')
        call test_assert(f, .false., 'fmt_fail')
        call test_suite_to_json(f, json)

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

        call test_suite_init(s, 'json_recording', fixture=.true.)
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

        call test_suite_init(s, 'long_message', fixture=.true.)
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
        call test_suite_init(s, 'nan_message', fixture=.true.)
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

        call test_suite_init(s, 'growth_test', fixture=.true.)
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
        character(len=:), allocatable :: line

        call test_suite_init(s, 'empty_suite', fixture=.true.)
        call test_suite_summary(s)
        call test_suite_summary_line(s, line)
        call test_suite_to_json(s, json)
        call test_suite_exit(s)

        call test_assert(suite, s%n_pass == 0, 'empty_suite_n_pass')
        call test_assert(suite, s%n_fail == 0, 'empty_suite_n_fail')
        call test_assert(suite, line == 'empty_suite: 0 pass, 0 fail, 0 total', &
            'empty_suite_summary_line')
    end subroutine test_no_assertions

    subroutine test_fixture_emits_nothing(suite)
        !! A fixture suite must stay silent on both streams: a printed FAIL
        !! line or summary line would land in the log the build tool parses.
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: captured
        logical :: ok

        call run_capture('--emit-fixture', captured, ok)
        call test_assert(suite, ok, 'fixture_emit_spawn')
        if (ok) then
            call test_assert(suite, len_trim(captured) == 0, &
                'fixture_emits_no_output', 'captured: ' // captured)
        end if
    end subroutine test_fixture_emits_nothing

    subroutine test_normal_suite_emits_failure(suite)
        !! Control for the test above: the same code path on a normal suite
        !! must still print the failure and the summary line.
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: captured
        logical :: ok

        call run_capture('--emit-normal', captured, ok)
        call test_assert(suite, ok, 'normal_emit_spawn')
        if (ok) then
            call test_assert(suite, index(captured, 'FAIL: noise_fail') > 0, &
                'normal_emits_fail_line', 'captured: ' // captured)
            call test_assert(suite, &
                index(captured, 'noise_suite: 0 pass, 1 fail, 1 total') > 0, &
                'normal_emits_summary_line', 'captured: ' // captured)
        end if
    end subroutine test_normal_suite_emits_failure

    subroutine run_capture(mode_arg, captured, ok)
        !! Re-run this program in the given mode and capture both output streams.
        character(len=*), intent(in) :: mode_arg
        character(len=:), allocatable, intent(out) :: captured
        logical, intent(out) :: ok
        integer :: exit_code

        call run_child(mode_arg, captured, exit_code, ok)
        if (ok) ok = exit_code == 0
    end subroutine run_capture

    subroutine run_child(mode_arg, captured, exit_code, ok)
        character(len=*), intent(in) :: mode_arg
        character(len=:), allocatable, intent(out) :: captured
        integer, intent(out) :: exit_code
        logical, intent(out) :: ok
        character(len=512) :: executable
        character(kind=c_char), allocatable :: arguments(:)
        character(kind=c_char) :: bytes(4096)
        type(c_ptr) :: handle
        integer :: argument_length, argument_status, position, i
        integer(c_int) :: nread

        captured = ''
        exit_code = -1
        ok = .false.
        executable = ''
        call get_command_argument(0, executable, length=argument_length, &
            status=argument_status)
        if (argument_status /= 0 .or. argument_length > len(executable)) return
        allocate(arguments(argument_length + len_trim(mode_arg) + 2))
        position = 1
        call append_argument(arguments, position, executable(:argument_length))
        call append_argument(arguments, position, trim(mode_arg))
        handle = fx_test_spawn_argv(arguments, 2_c_int, 1_c_int)
        if (.not. c_associated(handle)) return
        do
            nread = fx_test_read(handle, bytes, int(size(bytes), c_int), 10000_c_int)
            if (nread <= 0_c_int) exit
            do i = 1, int(nread)
                captured = captured // achar(iachar(bytes(i)))
            end do
        end do
        exit_code = int(fx_test_close(handle, 5000_c_int))
        ok = exit_code >= 0
    end subroutine run_child

    subroutine append_argument(buffer, position, argument)
        character(kind=c_char), intent(inout) :: buffer(:)
        integer, intent(inout) :: position
        character(len=*), intent(in) :: argument
        integer :: i

        do i = 1, len(argument)
            buffer(position) = argument(i:i)
            position = position + 1
        end do
        buffer(position) = c_null_char
        position = position + 1
    end subroutine append_argument

    subroutine test_suite_exit_failure_path(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: captured
        integer :: exit_code
        logical :: ok

        call run_child('--exit-fail', captured, exit_code, ok)
        call test_assert(suite, ok, 'exit_failure_spawn_command')
        if (ok) then
            call test_assert(suite, exit_code == 1, 'exit_failure_exit_code')
        end if
    end subroutine test_suite_exit_failure_path

end program test_fx_test
