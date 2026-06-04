program test_fx_test
    use, intrinsic :: iso_fortran_env, only: real64, error_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_assert, test_assert_equal_int, &
                       test_assert_equal_str, test_assert_equal_real, &
                       test_suite_summary, test_suite_exit, &
                       test_suite_to_json
    implicit none

    call run_all_tests()

contains

    subroutine run_all_tests()
        type(test_suite_t) :: suite

        call test_suite_init(suite, 'fx_test')
        call test_init_zeros(suite)
        call test_assert_pass(suite)
        call test_assert_fail(suite)
        call test_assert_equal_int_match(suite)
        call test_assert_equal_int_mismatch(suite)
        call test_assert_equal_int_negative(suite)
        call test_assert_equal_int_zero(suite)
        call test_assert_equal_str_match(suite)
        call test_assert_equal_str_mismatch(suite)
        call test_assert_equal_str_trailing_space(suite)
        call test_assert_equal_str_empty(suite)
        call test_assert_equal_real_match(suite)
        call test_assert_equal_real_default_tol(suite)
        call test_assert_equal_real_custom_tol(suite)
        call test_assert_equal_real_nan_expected(suite)
        call test_assert_equal_real_nan_actual(suite)
        call test_assert_equal_real_nan_both(suite)
        call test_assert_equal_real_negative(suite)
        call test_suite_summary(suite)
        call test_suite_to_json_valid(suite)
        call test_suite_to_json_empty(suite)
        call test_suite_to_json_format(suite)
        call test_no_assertions(suite)
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

    subroutine test_suite_to_json_valid(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: json

        call test_suite_init(suite, 'json_test')
        call test_assert(suite, .true., 'json_pass_1')
        call test_assert(suite, .true., 'json_pass_2')
        call test_assert(suite, .false., 'json_fail_1')
        call test_suite_to_json(suite, json)

        call test_assert(suite, index(json, '"name":"json_test"') > 0, &
                         'json_contains_name')
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

    subroutine test_no_assertions(suite)
        type(test_suite_t), intent(inout) :: suite
        type(test_suite_t) :: s
        character(len=:), allocatable :: json

        call test_suite_init(s, 'empty_suite')
        call test_suite_summary(s)
        call test_suite_to_json(s, json)

        call test_assert(suite, s%n_pass == 0, 'empty_suite_n_pass')
        call test_assert(suite, s%n_fail == 0, 'empty_suite_n_fail')
    end subroutine test_no_assertions

end program test_fx_test
