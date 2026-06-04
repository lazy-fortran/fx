program test_string
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_string')
    call test_str_constructor(suite)
    call test_builder_append(suite)
    call test_builder_geometric_growth(suite)
    call test_to_lower_upper(suite)
    call test_split_join(suite)
    call test_starts_ends_with(suite)
    call test_contains_find(suite)
    call test_replace(suite)
    call test_strip(suite)
    call test_utf8_len(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_str_constructor(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_str_constructor not implemented"
    end subroutine test_str_constructor

    subroutine test_builder_append(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_builder_append not implemented"
    end subroutine test_builder_append

    subroutine test_builder_geometric_growth(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_builder_geometric_growth not implemented"
    end subroutine test_builder_geometric_growth

    subroutine test_to_lower_upper(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_to_lower_upper not implemented"
    end subroutine test_to_lower_upper

    subroutine test_split_join(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_split_join not implemented"
    end subroutine test_split_join

    subroutine test_starts_ends_with(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_starts_ends_with not implemented"
    end subroutine test_starts_ends_with

    subroutine test_contains_find(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_contains_find not implemented"
    end subroutine test_contains_find

    subroutine test_replace(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_replace not implemented"
    end subroutine test_replace

    subroutine test_strip(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_strip not implemented"
    end subroutine test_strip

    subroutine test_utf8_len(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_utf8_len not implemented"
    end subroutine test_utf8_len

end program test_string
