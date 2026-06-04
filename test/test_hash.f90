program test_hash
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_hash')
    call test_fnv1a_known_values(suite)
    call test_fnv1a_file(suite)
    call test_xxhash64(suite)
    call test_hash_combine(suite)
    call test_hash_to_hex(suite)
    call test_incremental_hash(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_fnv1a_known_values(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_fnv1a_known_values not implemented"
    end subroutine test_fnv1a_known_values

    subroutine test_fnv1a_file(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_fnv1a_file not implemented"
    end subroutine test_fnv1a_file

    subroutine test_xxhash64(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_xxhash64 not implemented"
    end subroutine test_xxhash64

    subroutine test_hash_combine(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_hash_combine not implemented"
    end subroutine test_hash_combine

    subroutine test_hash_to_hex(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_hash_to_hex not implemented"
    end subroutine test_hash_to_hex

    subroutine test_incremental_hash(suite)
        type(test_suite_t), intent(inout) :: suite
        error stop "test_incremental_hash not implemented"
    end subroutine test_incremental_hash

end program test_hash
