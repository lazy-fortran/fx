program test_path
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_path')
    call test_path_join(suite)
    call test_path_dirname_basename(suite)
    call test_path_extension_stem(suite)
    call test_path_strip_prefix(suite)
    call test_path_normalize(suite)
    call test_path_is_absolute(suite)
    call test_path_relative(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_path_join(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_join not implemented"
    end subroutine test_path_join

    subroutine test_path_dirname_basename(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_dirname_basename not implemented"
    end subroutine test_path_dirname_basename

    subroutine test_path_extension_stem(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_extension_stem not implemented"
    end subroutine test_path_extension_stem

    subroutine test_path_strip_prefix(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_strip_prefix not implemented"
    end subroutine test_path_strip_prefix

    subroutine test_path_normalize(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_normalize not implemented"
    end subroutine test_path_normalize

    subroutine test_path_is_absolute(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_is_absolute not implemented"
    end subroutine test_path_is_absolute

    subroutine test_path_relative(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_path_relative not implemented"
    end subroutine test_path_relative

end program test_path
