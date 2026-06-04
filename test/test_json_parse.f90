program test_json_parse
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_json_parse')
    call test_parse_empty_object(suite)
    call test_parse_nested(suite)
    call test_parse_array(suite)
    call test_parse_escaped_strings(suite)
    call test_parse_numbers(suite)
    call test_parse_booleans_null(suite)
    call test_extract_string_path(suite)
    call test_extract_int_path(suite)
    call test_parse_malformed(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_parse_empty_object(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_empty_object not implemented"
    end subroutine test_parse_empty_object

    subroutine test_parse_nested(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_nested not implemented"
    end subroutine test_parse_nested

    subroutine test_parse_array(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_array not implemented"
    end subroutine test_parse_array

    subroutine test_parse_escaped_strings(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_escaped_strings not implemented"
    end subroutine test_parse_escaped_strings

    subroutine test_parse_numbers(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_numbers not implemented"
    end subroutine test_parse_numbers

    subroutine test_parse_booleans_null(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_booleans_null not implemented"
    end subroutine test_parse_booleans_null

    subroutine test_extract_string_path(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_extract_string_path not implemented"
    end subroutine test_extract_string_path

    subroutine test_extract_int_path(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_extract_int_path not implemented"
    end subroutine test_extract_int_path

    subroutine test_parse_malformed(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_parse_malformed not implemented"
    end subroutine test_parse_malformed

end program test_json_parse
