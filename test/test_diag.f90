program test_diag
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_diag')
    call test_diag_new(suite)
    call test_diag_to_string(suite)
    call test_diag_to_json(suite)
    call test_diag_strip_prefix(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_diag_new(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_diag_new not implemented"
    end subroutine test_diag_new

    subroutine test_diag_to_string(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_diag_to_string not implemented"
    end subroutine test_diag_to_string

    subroutine test_diag_to_json(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_diag_to_json not implemented"
    end subroutine test_diag_to_json

    subroutine test_diag_strip_prefix(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_diag_strip_prefix not implemented"
    end subroutine test_diag_strip_prefix

end program test_diag
