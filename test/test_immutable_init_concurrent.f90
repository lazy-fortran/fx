program test_immutable_init_concurrent
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        IMMUTABLE_OK
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_temp_root, fx_test_remove_tree
    implicit none

    integer, parameter :: workers = 8, repetitions = 128
    character(kind=c_char, len=512) :: scratch
    character(len=512) :: root, paths(workers), expected(workers)
    character(len=32) :: pid
    type(immutable_store_t) :: store
    integer :: i, j, ierr, failures(workers), n

    ierr = fx_test_temp_root(scratch, len(scratch))
    if (ierr /= 0) error stop 'cannot resolve scratch directory'
    n = index(scratch, c_null_char) - 1
    write(pid, '(I0)') proc_pid()
    root = scratch(:n)//'/fx-init-parallel-'//trim(pid)
    do i = 1, workers
        paths(i) = trim(root)//'/'//repeat('s', i*19)
        call immutable_store_init(store, trim(paths(i)), ierr)
        if (ierr /= IMMUTABLE_OK) error stop 'cannot seed store'
        expected(i) = store%root_dir
    end do
    failures = 0
    ! Vary descriptor lengths and check actual canonical roots, not merely
    ! survival. Without OpenMP this remains the serial correctness control.
    !$omp parallel do num_threads(workers) private(j) schedule(static)
    do i = 1, workers
        do j = 1, repetitions
            call check_root(trim(paths(i)), trim(expected(i)), failures(i))
        end do
    end do
    !$omp end parallel do
    ierr = fx_test_remove_tree(trim(root))
    if (ierr /= 0) error stop 'cannot clean owned scratch'
    if (sum(failures) /= 0) error stop 'concurrent initialization changed a root'
    print '(A,I0)', 'PASS immutable initializations: ', workers*repetitions
contains
    recursive subroutine check_root(path, expected_root, failures)
        character(len=*), intent(in) :: path, expected_root
        integer, intent(inout) :: failures
        type(immutable_store_t) :: local_store
        integer :: status

        call immutable_store_init(local_store, path, status)
        if (status /= IMMUTABLE_OK) then
            failures = failures + 1
            return
        end if
        if (.not. local_store%initialized) failures = failures + 1
        if (local_store%root_dir /= expected_root) failures = failures + 1
    end subroutine check_root
end program test_immutable_init_concurrent
