program test_json_build
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_json_build')
    call test_json_empty_object(suite)
    call test_json_nested_object(suite)
    call test_json_array(suite)
    call test_json_escape_special_chars(suite)
    call test_json_key_value_shortcuts(suite)
    call test_json_large_output(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_json_empty_object(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_empty_object not implemented"
    end subroutine test_json_empty_object

    subroutine test_json_nested_object(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_nested_object not implemented"
    end subroutine test_json_nested_object

    subroutine test_json_array(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_array not implemented"
    end subroutine test_json_array

    subroutine test_json_escape_special_chars(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_escape_special_chars not implemented"
    end subroutine test_json_escape_special_chars

    subroutine test_json_key_value_shortcuts(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_key_value_shortcuts not implemented"
    end subroutine test_json_key_value_shortcuts

    subroutine test_json_large_output(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_json_large_output not implemented"
    end subroutine test_json_large_output

end program test_json_build
