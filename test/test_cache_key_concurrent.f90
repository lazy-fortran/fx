program test_cache_key_concurrent
    !! OpenMP stress regression test for lazy-fortran/fx #36.
    !!
    !! fo's link_binary calls fx_cache_key::cache_file_digest from inside an
    !! OpenMP parallel region (compile_and_run_tests._omp_fn.0). digest_parts
    !! used to grow a message with `text = text // ...` (a reallocating
    !! allocatable) before hashing; under concurrent builds that realloc was
    !! handed an inconsistent pointer and segfaulted. The fix streams each
    !! part into a block-buffered incremental SHA-256 with no growing buffer,
    !! so the digest path has no shared allocation to race on.
    !!
    !! This test hammers cache_file_digest / cache_digest / cache_key_for /
    !! cache_source_tree_hash from many OpenMP threads on varied inputs and
    !! verifies (a) the process survives and (b) every thread's digest matches
    !! a single-threaded reference. It is a real stress test only when built
    !! with OpenMP (-fopenmp); without it the parallel region collapses to one
    !! thread and the digest-correctness assertions still run.
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_str, test_suite_summary, test_suite_exit
    use fx_cache_key, only: HASH_LEN, cache_digest, cache_file_digest, &
        cache_key_for, cache_source_tree_hash
    implicit none

    type(test_suite_t) :: suite
    integer, parameter :: NTHREADS = 8
    integer, parameter :: NITER = 200
    integer :: ierr

    call test_suite_init(suite, 'fx_cache_key_concurrent')
    call setup_files(ierr)
    call test_concurrent_digest_correctness(suite)
    call test_concurrent_file_digest_correctness(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine setup_files(ierr)
        integer, intent(out) :: ierr
        integer :: u, i

        ierr = 0
        open (newunit=u, file='/tmp/fx_cache_key_concurrent_a.f90', &
            status='replace', action='write', iostat=ierr)
        if (ierr /= 0) return
        write (u, '(A)') 'module alpha'
        write (u, '(A)') '  include "fx_cache_key_concurrent_inc.inc"'
        write (u, '(A)') 'end module alpha'
        close (u)

        open (newunit=u, file='/tmp/fx_cache_key_concurrent_inc.inc', &
            status='replace', action='write', iostat=ierr)
        if (ierr /= 0) return
        write (u, '(A)') 'integer, parameter :: V = 42'
        close (u)

        open (newunit=u, file='/tmp/fx_cache_key_concurrent_b.o', &
            status='replace', access='stream', form='unformatted', &
            action='write', iostat=ierr)
        if (ierr /= 0) return
        do i = 1, 4096
            write (u) achar(mod(i, 251))
        end do
        close (u)
    end subroutine setup_files

    subroutine test_concurrent_digest_correctness(suite)
        type(test_suite_t), intent(inout) :: suite

        character(len=512) :: parts_a(3) = &
            [character(len=512) :: 'fx-output-schema-1', &
            repeat('a', 64), 'module_alpha']
        character(len=512) :: parts_b(3) = &
            [character(len=512) :: 'fx-output-schema-1', &
            repeat('b', 64), 'module_beta']
        character(len=64) :: ref_a, ref_b
        integer :: t, it

        ref_a = cache_digest(parts_a, 3)
        ref_b = cache_digest(parts_b, 3)
        call test_assert(suite, trim(ref_a) /= trim(ref_b), &
            'reference digests differ for distinct inputs')

        !$omp parallel num_threads(NTHREADS) private(t, it)
        do t = 1, NTHREADS
            do it = 1, NITER
                call check_digest_match(trim(ref_a), parts_a, 3, suite)
                call check_digest_match(trim(ref_b), parts_b, 3, suite)
            end do
        end do
        !$omp end parallel
    end subroutine test_concurrent_digest_correctness

    subroutine test_concurrent_file_digest_correctness(suite)
        type(test_suite_t), intent(inout) :: suite

        character(len=HASH_LEN) :: ref, got
        integer :: t, it

        call cache_file_digest('/tmp/fx_cache_key_concurrent_b.o', ref)
        call test_assert(suite, len_trim(ref) == HASH_LEN, &
            'reference file digest is full length')

        !$omp parallel num_threads(NTHREADS) private(t, it, got)
        do t = 1, NTHREADS
            do it = 1, NITER
                call cache_file_digest('/tmp/fx_cache_key_concurrent_b.o', got)
                !$omp critical
                call test_assert_equal_str(suite, trim(ref), trim(got), &
                    'concurrent file digest matches reference')
                !$omp end critical
            end do
        end do
        !$omp end parallel
    end subroutine test_concurrent_file_digest_correctness

    subroutine check_digest_match(ref, parts, n_parts, suite)
        character(len=*), intent(in) :: ref
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        type(test_suite_t), intent(inout) :: suite

        character(len=HASH_LEN) :: got

        got = cache_digest(parts, n_parts)
        !$omp critical
        call test_assert_equal_str(suite, ref, trim(got), &
            'concurrent digest matches reference')
        !$omp end critical
    end subroutine check_digest_match

end program test_cache_key_concurrent
