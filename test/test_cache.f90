program test_cache
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_cache')
    call test_cache_init(suite)
    call test_cache_store_restore(suite)
    call test_cache_has(suite)
    call test_cache_evict(suite)
    call test_cache_gc(suite)
    call test_cache_key_deterministic(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_cache_init(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_init not implemented"
    end subroutine test_cache_init

    subroutine test_cache_store_restore(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_store_restore not implemented"
    end subroutine test_cache_store_restore

    subroutine test_cache_has(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_has not implemented"
    end subroutine test_cache_has

    subroutine test_cache_evict(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_evict not implemented"
    end subroutine test_cache_evict

    subroutine test_cache_gc(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_gc not implemented"
    end subroutine test_cache_gc

    subroutine test_cache_key_deterministic(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_cache_key_deterministic not implemented"
    end subroutine test_cache_key_deterministic

end program test_cache
