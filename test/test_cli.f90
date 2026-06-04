program test_cli
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_cli')
    call test_cli_has_flag(suite)
    call test_cli_get_value(suite)
    call test_cli_positional(suite)
    call test_cli_command(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_cli_has_flag(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cli_has_flag not implemented"
    end subroutine test_cli_has_flag

    subroutine test_cli_get_value(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cli_get_value not implemented"
    end subroutine test_cli_get_value

    subroutine test_cli_positional(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cli_positional not implemented"
    end subroutine test_cli_positional

    subroutine test_cli_command(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cli_command not implemented"
    end subroutine test_cli_command

end program test_cli
