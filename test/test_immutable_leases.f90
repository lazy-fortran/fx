program test_immutable_leases
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        IMMUTABLE_OK, immutable_store_init, immutable_store_put_blob, &
        immutable_store_blob_path, immutable_store_root_set, &
        immutable_store_reason_release, &
        immutable_store_publication_lease_acquire, &
        immutable_store_publication_commit, immutable_store_read_lease_acquire, &
        immutable_store_lease_release
    use fx_proc, only: proc_pid
    use fx_test_process, only: test_process_spawn, test_process_wait_once, &
        test_process_signal, test_process_clock_ms, test_process_sleep_ms
    implicit none

    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_lease_t) :: publication, reader
    character(len=512) :: root, source, executable, marker, argument
    character(len=512) :: child_args(5)
    character(len=64) :: object_id, ids(1)
    character(len=8) :: kinds(1) = ['blob    ']
    character(len=1) :: bytes(8) = ['l', 'e', 'a', 's', 'e', 's', '!', char(10)]
    integer(kind=8) :: epoch1, epoch2, epoch3
    integer :: ierr, status, child_pid
    logical :: exists, completed, child_exited

    call get_command_argument(1, argument)
    if (trim(argument) == '--barrier-worker') then
        call barrier_worker()
        stop 0
    end if
    if (trim(argument) == '--crash-worker') then
        call crash_worker()
        stop 0
    end if
    call test_suite_init(suite, 'fx_immutable_leases')
    write(root, '(A,I0)') '/var/tmp/fx_leases_', proc_pid()
    call immutable_store_init(store, trim(root), ierr)
    if (ierr /= IMMUTABLE_OK) write (*, '(A,I0,2A)') &
        'lease init error=', ierr, ' root=', trim(root)
    if (ierr == IMMUTABLE_OK) then
        if (store%root_dir /= trim(root)) &
            write (*, '(2A)') 'resolved lease root=', store%root_dir
    end if
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'lease store initializes')
    source = trim(root)//'/source'
    call write_bytes(trim(source), bytes, ierr)
    call immutable_store_put_blob(store, trim(source), object_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'lease fixture blob publishes')
    ids(1) = object_id

    call immutable_store_root_set(store, 'owner_a', 'start_1', 'result', &
        kinds, ids, ierr, epoch1)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'root owner A registers')
    call immutable_store_root_set(store, 'owner_a', 'start_1', 'result', &
        kinds, ids, ierr, epoch2)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'equal root publication succeeds')
    call test_assert(suite, epoch1 == epoch2, 'equal root publication is idempotent')
    call immutable_store_root_set(store, 'owner_b', 'start_2', 'result', &
        kinds, ids, ierr, epoch2)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'root owner B registers')
    call immutable_store_reason_release(store, 'owner_a', 'stale_start', 'result', &
        ierr, epoch3)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'stale release is harmless')
    call test_assert(suite, epoch2 == epoch3, 'stale owner identity changes no roots')
    call immutable_store_reason_release(store, 'owner_a', 'start_1', 'result', &
        ierr, epoch3)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'owner A releases its reason')
    call test_assert(suite, epoch3 > epoch2, 'root release advances the epoch')
    call assert_metadata_has(suite, trim(root)//'/.fx-metadata/leases', &
        'R||owner_b|start_2|result|blob|'//trim(object_id), &
        'owner B root remains after A release')

    call immutable_store_publication_lease_acquire(store, 'owner_c', 'start_3', &
        'publish', kinds, ids, publication, ierr, epoch1)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'publication lease acquired')
    call immutable_store_publication_commit(store, publication, 'result', kinds, &
        ids, ierr, epoch2)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'publication commits atomically')
    call test_assert(suite, epoch2 > epoch1, 'publication commit advances epoch')
    call immutable_store_read_lease_acquire(store, 'reader_a', 'reader_start', &
        'materialize', 'blob', object_id, reader, ierr, epoch1)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'read lease acquired')
    call immutable_store_lease_release(store, reader, ierr, epoch2)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'read lease releases')

    call test_concurrent_publication_barrier(suite, trim(root), trim(object_id))
    if (suite%n_fail > 0) then
        call test_suite_summary(suite)
        call test_suite_exit(suite)
        stop
    end if

    call get_command_argument(0, executable)
    marker = trim(root)//'/crash-ready'
    child_args = [character(len=512) :: trim(executable), '--crash-worker', &
        trim(root), trim(object_id), trim(marker)]
    call test_process_spawn(child_args, child_pid, ierr)
    call test_assert_equal_int(suite, 0, ierr, &
        'crash worker starts as a separate process')
    call wait_for_file(trim(marker), exists, child_pid, status, child_exited)
    if (child_exited) then
        write (*, '(A,I0,2A)') 'crash worker exited=', status, &
            ' store root=', trim(root)
        call test_assert(suite, .false., &
            'crash worker exited before durable root marker')
        call test_suite_summary(suite)
        call test_suite_exit(suite)
        stop
    end if
    call test_assert(suite, exists, 'crash worker durably commits its root')
    call test_process_signal(child_pid, 9, ierr)
    call test_assert(suite, ierr == 0, 'committed-root worker is killed')
    call wait_for_child(child_pid, 5000, status, completed)
    call test_assert(suite, completed, 'killed worker is reaped')
    call read_metadata_epoch(trim(root)//'/.fx-metadata/leases', epoch3, ierr)
    call test_assert_equal_int(suite, 0, ierr, &
        'restarted reader opens committed root metadata')
    call test_assert(suite, epoch3 >= epoch2, 'recovered epoch is monotonic')
    call assert_metadata_has(suite, trim(root)//'/.fx-metadata/leases', &
        'R||crash_owner|crash_start|recovered|blob|'//trim(object_id), &
        'crash recovery preserves committed root')

    inquire(file=immutable_store_blob_path(store, object_id), exist=exists)
    call test_assert(suite, exists, 'lease updates and releases delete no blob')
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine crash_worker()
        type(immutable_store_t) :: child_store
        character(len=512) :: child_root, ready
        character(len=64) :: child_id
        character(len=64) :: child_ids(1)
        character(len=8) :: child_kinds(1) = ['blob    ']
        integer :: local_err, unit

        call get_command_argument(2, child_root)
        call get_command_argument(3, child_id)
        call get_command_argument(4, ready)
        call immutable_store_init(child_store, trim(child_root), local_err)
        if (local_err /= IMMUTABLE_OK) then
            write (*, '(A,I0,2A)') 'crash worker store init error=', &
                local_err, ' root=', trim(child_root)
            stop 41
        end if
        child_ids(1) = child_id
        call immutable_store_root_set(child_store, 'crash_owner', 'crash_start', &
            'recovered', child_kinds, child_ids, local_err)
        if (local_err /= IMMUTABLE_OK) stop 41
        open(newunit=unit, file=trim(ready), status='replace', action='write')
        write(unit, '(A)') 'ready'
        close(unit)
        do
        end do
    end subroutine crash_worker

    subroutine barrier_worker()
        type(immutable_store_t) :: child_store
        type(immutable_lease_t) :: child_lease
        character(len=512) :: child_root, owner, owner_start
        character(len=512) :: ready, release, done
        character(len=64) :: child_id, child_ids(1)
        character(len=8) :: child_kinds(1) = ['blob    ']
        integer :: local_err, unit

        call get_command_argument(2, child_root)
        call get_command_argument(3, owner)
        call get_command_argument(4, owner_start)
        call get_command_argument(5, child_id)
        call get_command_argument(6, ready)
        call get_command_argument(7, release)
        call get_command_argument(8, done)
        call immutable_store_init(child_store, trim(child_root), local_err)
        if (local_err /= IMMUTABLE_OK) write (*, '(A,I0,2A)') &
            'barrier store init error=', local_err, ' root=', trim(child_root)
        child_ids(1) = child_id
        call immutable_store_publication_lease_acquire(child_store, trim(owner), &
            trim(owner_start), 'parallel', child_kinds, child_ids, child_lease, &
            local_err)
        if (local_err /= IMMUTABLE_OK) then
            write (*, '(A,I0,2A)') 'publication lease error=', local_err, &
                ' root=', trim(child_root)
            stop 42
        end if
        open(newunit=unit, file=trim(ready), status='replace', action='write')
        write(unit, '(A)') 'ready'
        close(unit)
        call wait_for_file(trim(release), exists)
        if (.not. exists) stop 43
        call immutable_store_publication_commit(child_store, child_lease, &
            'parallel', child_kinds, child_ids, local_err)
        if (local_err /= IMMUTABLE_OK) stop 44
        open(newunit=unit, file=trim(done), status='replace', action='write')
        write(unit, '(A)') 'committed'
        close(unit)
    end subroutine barrier_worker

    subroutine test_concurrent_publication_barrier(s, base, id)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: base, id
        character(len=512) :: child, ready_a, ready_b, done_a, done_b
        character(len=512) :: release, arguments(9)
        integer :: launch_error, pid_a, pid_b, exit_a, exit_b, signal_error
        logical :: ready, complete_a, complete_b, exited

        call get_command_argument(0, child)
        ready_a = trim(base)//'/parallel-a-ready'
        ready_b = trim(base)//'/parallel-b-ready'
        done_a = trim(base)//'/parallel-a-done'
        done_b = trim(base)//'/parallel-b-done'
        release = trim(base)//'/parallel-release'
        arguments = [character(len=512) :: trim(child), '--barrier-worker', &
            trim(base), 'owner_d', 'start_4', trim(id), trim(ready_a), &
            trim(release), trim(done_a)]
        call test_process_spawn(arguments, pid_a, launch_error)
        call test_assert_equal_int(s, 0, launch_error, 'publisher A launches')
        arguments(4) = 'owner_e'
        arguments(5) = 'start_5'
        arguments(7) = trim(ready_b)
        arguments(9) = trim(done_b)
        call test_process_spawn(arguments, pid_b, launch_error)
        call test_assert_equal_int(s, 0, launch_error, 'publisher B launches')
        call wait_for_file(trim(ready_a), ready, pid_a, exit_a, exited)
        if (exited) then
            call test_assert(s, .false., &
                'publisher A exited before its readiness marker')
            call test_process_signal(pid_b, 9, signal_error)
            call wait_for_child(pid_b, 1000, exit_b, complete_b)
            return
        end if
        call test_assert(s, ready, 'publisher A holds its lease at barrier')
        call wait_for_file(trim(ready_b), ready, pid_b, exit_b, exited)
        if (exited) then
            call test_assert(s, .false., &
                'publisher B exited before its readiness marker')
            call test_process_signal(pid_a, 9, signal_error)
            call wait_for_child(pid_a, 1000, exit_a, complete_a)
            return
        end if
        call test_assert(s, ready, 'publisher B holds its lease at barrier')
        call create_marker(trim(release))
        call test_assert_equal_int(s, 0, launch_error, 'barrier workers launch')
        call wait_for_file(trim(done_a), ready)
        call test_assert(s, ready, 'publisher A commits after the barrier')
        call wait_for_file(trim(done_b), ready)
        call test_assert(s, ready, 'publisher B commits after the barrier')
        call wait_for_child(pid_a, 15000, exit_a, complete_a)
        call wait_for_child(pid_b, 15000, exit_b, complete_b)
        call test_assert(s, complete_a, 'publisher A process is reaped')
        call test_assert(s, complete_b, 'publisher B process is reaped')
        call test_assert_equal_int(s, 0, exit_a, 'publisher A exits successfully')
        call test_assert_equal_int(s, 0, exit_b, 'publisher B exits successfully')
        call assert_metadata_has(s, trim(base)//'/.fx-metadata/leases', &
            'R||owner_d|start_4|parallel|blob|'//id, &
            'concurrent publisher A root is retained')
        call assert_metadata_has(s, trim(base)//'/.fx-metadata/leases', &
            'R||owner_e|start_5|parallel|blob|'//id, &
            'concurrent publisher B root is retained')
    end subroutine test_concurrent_publication_barrier

    subroutine write_bytes(path, content, local_err)
        character(len=*), intent(in) :: path
        character(len=1), intent(in) :: content(:)
        integer, intent(out) :: local_err
        integer :: unit, i

        open(newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', iostat=local_err)
        if (local_err /= 0) return
        do i = 1, size(content)
            write(unit, iostat=local_err) content(i)
            if (local_err /= 0) exit
        end do
        close(unit)
    end subroutine write_bytes

    subroutine create_marker(path)
        character(len=*), intent(in) :: path
        integer :: unit

        open(newunit=unit, file=path, status='replace', action='write')
        write(unit, '(A)') 'release'
        close(unit)
    end subroutine create_marker

    subroutine wait_for_file(path, found, child_pid, child_status, child_exited)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer, intent(in), optional :: child_pid
        integer, intent(out), optional :: child_status
        logical, intent(out), optional :: child_exited
        integer :: i, state, status

        found = .false.
        if (present(child_exited)) child_exited = .false.
        do i = 1, 500
            inquire(file=path, exist=found)
            if (found) return
            if (present(child_pid)) then
                call test_process_wait_once(child_pid, status, state)
                if (state /= 0) then
                    if (present(child_status)) child_status = status
                    if (present(child_exited)) child_exited = .true.
                    return
                end if
            end if
            call test_process_sleep_ms(10)
        end do
    end subroutine wait_for_file

    subroutine wait_for_child(pid, timeout_ms, exit_status, completed)
        integer, intent(in) :: pid, timeout_ms
        integer, intent(out) :: exit_status
        logical, intent(out) :: completed
        integer :: child_state, signal_error
        integer(int64) :: deadline

        completed = .false.
        exit_status = -1
        deadline = test_process_clock_ms() + int(timeout_ms, int64)
        do
            call test_process_wait_once(pid, exit_status, child_state)
            if (child_state == 1) then
                completed = .true.
                return
            end if
            if (child_state < 0) exit
            if (test_process_clock_ms() >= deadline) exit
    call test_process_sleep_ms(5)
    end do
        call test_process_signal(pid, 9, signal_error)
        deadline = test_process_clock_ms() + 5000_int64
        do
            call test_process_wait_once(pid, exit_status, child_state)
            if (child_state == 1) then
                completed = .true.
                return
            end if
            if (child_state < 0) exit
            if (test_process_clock_ms() >= deadline) exit
            call test_process_sleep_ms(5)
        end do
    end subroutine wait_for_child

    subroutine assert_metadata_has(s, path, needle, label)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: path, needle, label
        character(len=2048) :: line
        integer :: unit, local_err
        logical :: found

        found = .false.
        open(newunit=unit, file=path, status='old', action='read', &
            iostat=local_err)
        if (local_err == 0) then
            do
                read(unit, '(A)', iostat=local_err) line
                if (local_err /= 0) exit
                if (index(line, needle) > 0) found = .true.
            end do
            close(unit)
        end if
        call test_assert(s, found, label)
    end subroutine assert_metadata_has

    subroutine read_metadata_epoch(path, value, local_err)
        character(len=*), intent(in) :: path
        integer(kind=8), intent(out) :: value
        integer, intent(out) :: local_err
        character(len=128) :: line
        integer :: unit, sep

        value = -1
        open(newunit=unit, file=path, status='old', action='read', &
            iostat=local_err)
        if (local_err /= 0) return
        read(unit, '(A)', iostat=local_err) line
        close(unit)
        if (local_err /= 0) return
        sep = index(line, '|')
        if (sep <= 0) then
            local_err = 1
            return
        end if
        read(line(sep + 1:), *, iostat=local_err) value
    end subroutine read_metadata_epoch

end program test_immutable_leases
