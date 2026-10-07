program test_action_result_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_proc, only: proc_pid, proc_exec_silent
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, fx_test_chmod, &
        fx_test_temp_root
    use fx_test_process, only: test_process_is_executable
    use fx_test_process, only: test_process_wait_once, test_process_signal, &
        test_process_sleep_ms
    use action_publication_oracle, only: publication_probe_t, &
        publication_probe_lock, publication_probe_observe, publication_probe_conflict, &
        publication_probe_unlock
    use fx_immutable_store, only: immutable_tree_entry_t, &
        immutable_store_blob_path
    use fx_immutable_tree, only: immutable_store_verify_tree
    use fx_immutable_constants, only: IMMUTABLE_OK
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_publish_files, &
        action_result_lookup, action_result_conflicts, &
        action_result_retire, action_result_retire_key, action_result_read_t, &
        action_result_read_acquire, action_result_read_release, &
        action_result_put_blob, action_result_publish, &
        action_result_materialize_blob, ACTION_RESULT_OK, &
        ACTION_RESULT_CONFLICT, ACTION_RESULT_QUARANTINED, &
        ACTION_RESULT_MISSING, ACTION_RESULT_CORRUPT, &
        action_result_action_key, action_result_action_key_parts, &
        action_result_compile_action_key
    implicit none

    interface
        integer(c_int) function fork_process() bind(C, name='fork')
            import c_int
        end function fork_process
        integer(c_int) function kill_child(pid, signal) bind(C, name='kill')
            import c_int
            integer(c_int), value :: pid, signal
        end function kill_child
        integer(c_int) function sleep_us(usec) bind(C, name='usleep')
            import c_int
            integer(c_int), value :: usec
        end function sleep_us
        subroutine exit_child(status) bind(C, name='_exit')
            import c_int
            integer(c_int), value :: status
        end subroutine exit_child
        integer(c_int) function raw_action_write(root_dir, action_key, &
                bytes, count) bind(C, name='fx_action_result_write')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root_dir(*), action_key(*), &
                bytes(*)
            integer(c_int), value :: count
        end function raw_action_write
        integer(c_int) function set_mtime(path, mtime) &
                bind(C, name='fx_c_set_mtime')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), value :: mtime
        end function set_mtime
        integer(c_long_long) function unix_time() bind(C, name='fx_c_unix_time')
            import c_long_long
        end function unix_time
    end interface

    type(test_suite_t) :: suite
    type(action_result_store_t) :: store
    type(immutable_tree_entry_t) :: files(4), conflict_files(1), later_files(1)
    type(immutable_tree_entry_t) :: missing_files(2), corrupt_files(1)
    type(immutable_tree_entry_t), allocatable :: restored(:)
    character(len=512, kind=c_char) :: scratch
    character(len=512) :: root
    character(len=64) :: first_result, repeated_result, second_result, later_result
    character(len=64) :: conflict_ids(2), corrupt_result
    character(len=:), allocatable :: result_path
    integer :: ierr, end_path
    character(len=512) :: race_source_a, race_source_b
    type(publication_probe_t) :: race_probe
    type(immutable_tree_entry_t) :: race_entry_a(1), race_entry_b(1)
    character(len=64) :: race_result_a, race_result_b

    call test_suite_init(suite, 'fx_action_result_store')
    scratch = c_null_char
    ierr = fx_test_temp_root(scratch, 512)
    call test_assert_equal_int(suite, 0, ierr, 'physical test root resolves')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-action43-', proc_pid()
    ierr = fx_test_mkdir_p(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'test root directory is created')
    call action_result_store_init(store, trim(root)//'/store/v2', ierr)
    call test_assert_equal_int(suite, 0, ierr, 'versioned action store initializes')

    call test_complete_outputs()
    call test_action_key_completeness()
    call test_manifest_metadata_mutants()
    call test_conflicting_publication()
    call test_missing_companion()
    call test_corrupt_companion()
    call test_equal_concurrent_publishers()
    call test_conflicting_concurrent_publishers()
    call test_crash_boundaries()
    call test_retirement_boundary()

    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'test root directory is removed')
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_retirement_boundary()
        type(action_result_read_t) :: active_read
        type(action_result_store_t) :: reopened
        type(immutable_tree_entry_t), allocatable :: found_entries(:)
        character(len=64) :: result_id, found_id, key
        character(len=:), allocatable :: marker
        character(len=512) :: source(1), record_path
        type(immutable_tree_entry_t) :: entry(1)
        logical :: retired
        integer :: status
        integer(c_int) :: child, child_status, waited

        source(1) = trim(root)//'/retire-source'
        call write_text(trim(source(1)), 'retirement bytes')
        entry(1) = output_entry('retired.bin', 'runtime-companion', 420)
        call action_result_publish_files(store, 'retire-action', source, entry, &
            result_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'retirement fixture publishes')
        key = action_result_action_key('retire-action')
        record_path = trim(store%root_dir)//'/actions/sha256/'// &
            key(1:2)//'/'//trim(key)
        status = set_mtime(trim(record_path)//c_null_char, &
            unix_time() - 172800_c_long_long)
        call test_assert_equal_int(suite, 0, status, &
            'fixture ages action record before hot lookup')
        call action_result_lookup(store, 'retire-action', found_entries, &
            found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'bound lookup refreshes old record age')
        call action_result_retire(store, 'retire-action', 86400, retired, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'recently used binding is ineligible')
        call test_assert(suite, .not. retired, 'hot binding survives age guard')
        call action_result_read_acquire(store, 'retire-action', active_read, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'reader leases old result before retirement')
        call action_result_retire(store, 'retire-action', 86400, retired, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'young binding is ineligible')
        call test_assert(suite, .not. retired, 'age guard leaves binding intact')
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, &
            'retirement worker forks alongside live reader')
        if (child == 0_c_int) then
            call action_result_retire_key(store, key, 0, retired, status)
            if (status == ACTION_RESULT_OK .and. retired) &
                call exit_child(0_c_int)
            call exit_child(1_c_int)
        end if
        if (child > 0_c_int) then
            call bounded_child_wait(child, child_status, waited)
            call test_assert_equal_int(suite, int(child), int(waited), &
                'retirement worker is reaped')
            call test_assert_equal_int(suite, 0, int(child_status), &
                'concurrent retirement reports removed binding')
        end if
        call assert_action_root('retire-action', 'bound', result_id, .false.)
        call action_result_lookup(store, 'retire-action', found_entries, &
            found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_MISSING, status, &
            'lookup safely misses after retirement')
        call action_result_read_release(store, active_read, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'live reader releases its independent graph lease')
        call action_result_retire(store, 'conflict-race-action', 0, retired, status)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, status, &
            'conflict evidence is protected from retirement')
        call test_assert(suite, .not. retired, 'conflict is not retired')

        ! Model termination immediately after the marker rename and reopen.
        call action_result_publish_files(store, 'retire-crash-action', source, &
            entry, result_id, status)
        key = action_result_action_key('retire-crash-action')
        marker = 'FXACTION2'//achar(10)//trim(key)//achar(10)// &
            'RETIRED'//achar(10)
        status = raw_action_write(trim(store%root_dir)//c_null_char, &
            trim(key)//c_null_char, marker//c_null_char, int(len(marker), c_int))
        call test_assert_equal_int(suite, 0, status, &
            'simulated crash leaves durable retired marker')
        call action_result_store_init(reopened, trim(root)//'/store/v2', status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'retirement store reopens after crash')
        call action_result_lookup(reopened, 'retire-crash-action', found_entries, &
            found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_MISSING, status, &
            'durable marker is a safe miss after restart')
        call action_result_retire(reopened, 'retire-crash-action', 86400, &
            retired, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'restart finishes marker cleanup regardless of age')
        call test_assert(suite, retired, 'interrupted retirement completes')
        call assert_action_root('retire-crash-action', 'bound', result_id, &
            .false.)
        call action_result_publish_files(reopened, 'retire-crash-action', &
            source, entry, found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'fresh publication can reuse retired action key')

        call action_result_publish_files(reopened, 'retire-rebind-action', &
            source, entry, result_id, status)
        key = action_result_action_key('retire-rebind-action')
        marker = 'FXACTION2'//achar(10)//trim(key)//achar(10)// &
            'RETIRED'//achar(10)
        status = raw_action_write(trim(store%root_dir)//c_null_char, &
            trim(key)//c_null_char, marker//c_null_char, int(len(marker), c_int))
        call test_assert_equal_int(suite, 0, status, &
            'second interrupted retirement marker is durable')
        call action_result_publish_files(reopened, 'retire-rebind-action', &
            source, entry, found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'publisher reconciles interrupted retirement before rebinding')
        call action_result_lookup(reopened, 'retire-rebind-action', &
            found_entries, found_id, status)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, status, &
            'rebound action remains readable')
        call test_assert_equal_str(suite, trim(result_id), trim(found_id), &
            'rebound action retains exact result graph')
    end subroutine test_retirement_boundary

    subroutine test_action_key_completeness()
        character(len=96) :: source_key, flags, toolchain, runtime, oracle
        character(len=64) :: baseline, changed
        character(len=96) :: collision_parts(2)

        source_key = 'source-tree:sha256:source-1'
        flags = '-O2 -fopenmp'
        toolchain = 'gfortran-16.2.1'
        runtime = 'libgfortran-16'
        oracle = 'fx-test-schema-43'
        baseline = action_result_compile_action_key(source_key, flags, &
            toolchain, runtime, oracle)
        changed = action_result_compile_action_key(source_key, '-O0 -fopenmp', &
            toolchain, runtime, oracle)
        call test_assert(suite, changed /= baseline, &
            'action key changes when compiler flags change')
        changed = action_result_compile_action_key(source_key, flags, &
            'gfortran-17.0.0', runtime, oracle)
        call test_assert(suite, changed /= baseline, &
            'action key changes when toolchain changes')
        changed = action_result_compile_action_key(source_key, flags, toolchain, &
            'libgfortran-17', oracle)
        call test_assert(suite, changed /= baseline, &
            'action key changes when runtime changes')
        changed = action_result_compile_action_key(source_key, flags, toolchain, &
            runtime, 'fx-test-schema-44')
        call test_assert(suite, changed /= baseline, &
            'action key changes when output oracle changes')
        changed = action_result_compile_action_key(source_key, flags, toolchain, '', &
            oracle)
        call test_assert(suite, len_trim(changed) == 0, &
            'action key refuses a missing runtime identity')
        changed = action_result_compile_action_key('source-tree:source-2', flags, &
            toolchain, runtime, oracle)
        call test_assert(suite, changed /= baseline, &
            'action key changes when the source tree changes')
        collision_parts = ''
        collision_parts(1) = 'ab'
        collision_parts(2) = 'c'
        baseline = action_result_action_key_parts(collision_parts, 2)
        collision_parts(1) = 'a'
        collision_parts(2) = 'bc'
        changed = action_result_action_key_parts(collision_parts, 2)
        call test_assert(suite, changed /= baseline, &
            'length-prefixed action components cannot alias by concatenation')
    end subroutine test_action_key_completeness

    subroutine test_manifest_metadata_mutants()
        character(len=512) :: source
        type(immutable_tree_entry_t) :: baseline_entry(1), mutant_entry(1)
        character(len=64) :: baseline_id, mutant_id

        source = trim(root)//'/manifest-mutant'
        call write_text(trim(source), 'same bytes, distinct manifest')
        baseline_entry(1) = output_entry('program', 'executable', 493)
        call action_result_publish_files(store, 'metadata-baseline', [source], &
            baseline_entry, baseline_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'baseline manifest publishes')

        mutant_entry = baseline_entry
        mutant_entry(1)%mode = 420
        call action_result_publish(store, 'metadata-mode-mutant', mutant_entry, &
            mutant_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'mode mutant manifest publishes')
        call test_assert(suite, mutant_id /= baseline_id, &
            'result identity includes executable mode')

        mutant_entry = baseline_entry
        mutant_entry(1)%role = 'runtime-companion'
        call action_result_publish(store, 'metadata-role-mutant', mutant_entry, &
            mutant_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'role mutant manifest publishes')
        call test_assert(suite, mutant_id /= baseline_id, &
            'result identity includes output role')

        mutant_entry = baseline_entry
        mutant_entry(1)%path = 'renamed-program'
        call action_result_publish(store, 'metadata-path-mutant', mutant_entry, &
            mutant_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'path mutant manifest publishes')
        call test_assert(suite, mutant_id /= baseline_id, &
            'result identity includes companion path')
    end subroutine test_manifest_metadata_mutants

    subroutine prepare_race_entries(action_tag)
        character(len=*), intent(in) :: action_tag

        race_source_a = trim(root)//'/'//trim(action_tag)//'-a'
        race_source_b = trim(root)//'/'//trim(action_tag)//'-b'
        call write_text(trim(race_source_a), trim(action_tag)//' RESULT A')
        call write_text(trim(race_source_b), trim(action_tag)//' RESULT B')
        race_entry_a(1) = output_entry('program', 'executable', 493)
        race_entry_b(1) = output_entry('program', 'executable', 493)
        call action_result_put_blob(store, trim(race_source_a), &
            race_entry_a(1)%object_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'first race result object is durable before publication')
        call action_result_put_blob(store, trim(race_source_b), &
            race_entry_b(1)%object_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'second race result object is durable before publication')
        call action_result_publish(store, trim(action_tag)//'-id-a', &
            race_entry_a, race_result_a, ierr)
        call action_result_publish(store, trim(action_tag)//'-id-b', &
            race_entry_b, race_result_b, ierr)
    end subroutine prepare_race_entries

    subroutine concurrent_publish_child(action_id, entries)
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=64) :: result_id
        integer :: status

        call action_result_publish(store, action_id, entries, result_id, status)
        if (status == ACTION_RESULT_OK .or. status == ACTION_RESULT_CONFLICT) then
            call exit_child(0_c_int)
        end if
        call exit_child(1_c_int)
    end subroutine concurrent_publish_child

    subroutine synchronized_publish_child(action_id, entries, ready_path, &
            release_path)
        character(len=*), intent(in) :: action_id, ready_path, release_path
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        logical :: ready

        call write_text(ready_path, 'ready')
        call wait_for_file(release_path, ready)
        if (.not. ready) call exit_child(2_c_int)
        call concurrent_publish_child(action_id, entries)
    end subroutine synchronized_publish_child

    subroutine start_paused_conflict(phase, tag, entries, child)
        integer(c_int), intent(in) :: phase
        character(len=*), intent(in) :: tag
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        integer(c_int), intent(out) :: child
        integer :: local_err
        character(len=64) :: pending_id

        child = -1_c_int
        call publication_probe_lock(trim(root)//'/store/v2', &
            trim(tag)//'-action', race_probe, local_err)
        call test_assert_equal_int(suite, 0, local_err, &
            'external action lock establishes publication boundary')
        if (local_err /= 0) return
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'crash publisher forks')
        if (child == 0_c_int) &
            call concurrent_publish_child(trim(tag)//'-action', entries)
        if (child < 0_c_int) then
            call publication_probe_unlock(race_probe)
            return
        end if
        call publication_probe_observe(race_probe, int(child), pending_id, local_err)
        call test_assert_equal_int(suite, 0, local_err, &
            'publisher waits after external durable P observation')
        if (phase == 2_c_int) then
            call publication_probe_conflict(race_probe, local_err)
            call test_assert_equal_int(suite, 0, local_err, &
                'publisher waits after external conflict-binding observation')
        end if
    end subroutine start_paused_conflict

    subroutine wait_for_file(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out), optional :: found
        logical :: exists
        integer :: attempt

        exists = .false.
        if (present(found)) found = .false.
        do attempt = 1, 1000
            inquire(file=trim(path), exist=exists)
            if (exists) then
                if (present(found)) found = .true.
                return
            end if
            ierr = sleep_us(10000_c_int)
        end do
        call test_assert(suite, .false., 'publisher reaches the native race barrier')
    end subroutine wait_for_file

    subroutine bounded_child_wait(pid, status, waited)
        integer(c_int), intent(in) :: pid
        integer(c_int), intent(out) :: status, waited
        integer :: local_status, state, local_err, attempt, phase

        status = -1_c_int
        waited = -1_c_int
        if (pid <= 0_c_int) return
        do phase = 1, 2
            do attempt = 1, 1500
                call test_process_wait_once(int(pid), local_status, state)
                if (state == 1) then
                    status = int(local_status, c_int)
                    waited = pid
                    return
                end if
                if (state < 0) return
                call test_process_sleep_ms(10)
            end do
            if (phase == 1) call test_process_signal(int(pid), 9, local_err)
        end do
    end subroutine bounded_child_wait

    subroutine test_equal_concurrent_publishers()
        integer(c_int) :: child, wait_status, wait_rc
        character(len=64) :: result_id
        character(len=512) :: ready_path, release_path
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('equal-race')
        ready_path = trim(root)//'/equal-race-ready'
        release_path = trim(root)//'/equal-race-release'
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'equal publication worker forks')
        if (child == 0_c_int) call synchronized_publish_child( &
            'equal-race-action', race_entry_a, ready_path, release_path)
        call wait_for_file(trim(ready_path))
        call write_text(trim(release_path), 'go')
        call action_result_publish(store, 'equal-race-action', race_entry_a, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'parent equal producer publishes successfully')
        wait_status = 0_c_int
        call bounded_child_wait(child, wait_status, wait_rc)
        call test_assert_equal_int(suite, 0, int(wait_status), &
            'concurrent equal producer succeeds')
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'equal producer process is reaped')
        call action_result_lookup(store, 'equal-race-action', restored_entries, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'equal concurrent publication remains reusable')
        call test_assert_equal_str(suite, trim(race_result_a), trim(result_id), &
            'equal producers publish one canonical result ID')
    end subroutine test_equal_concurrent_publishers

    subroutine test_conflicting_concurrent_publishers()
        integer(c_int) :: child, wait_status, wait_rc
        character(len=64) :: result_id, ids(2)
        character(len=512) :: ready_path, release_path
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('conflict-race')
        ready_path = trim(root)//'/conflict-race-ready'
        release_path = trim(root)//'/conflict-race-release'
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'conflicting publisher forks')
        if (child == 0_c_int) call synchronized_publish_child( &
            'conflict-race-action', race_entry_b, ready_path, release_path)
        call wait_for_file(trim(ready_path))
        call write_text(trim(release_path), 'go')
        call action_result_publish(store, 'conflict-race-action', race_entry_a, &
            result_id, ierr)
        call test_assert(suite, ierr == ACTION_RESULT_OK .or. &
            ierr == ACTION_RESULT_CONFLICT, 'parent race producer linearizes')
        wait_status = 0_c_int
        call bounded_child_wait(child, wait_status, wait_rc)
        call test_assert_equal_int(suite, 0, int(wait_status), &
            'competing producer completes after the action lock releases')
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'conflicting producer process is reaped')
        call action_result_lookup(store, 'conflict-race-action', restored_entries, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'concurrent different results leave no reusable first-writer hit')
        call action_result_conflicts(store, 'conflict-race-action', ids, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'concurrent conflict evidence is durable')
        call test_assert(suite, includes_id(ids, race_result_a) .and. &
            includes_id(ids, race_result_b), 'conflict retains both result IDs')
        call assert_action_root('conflict-race-action', 'conflict', race_result_a)
        call assert_action_root('conflict-race-action', 'conflict', race_result_b)
    end subroutine test_conflicting_concurrent_publishers

    subroutine assert_action_root(action_id, reason, object_id, expected)
        character(len=*), intent(in) :: action_id, reason, object_id
        logical, intent(in), optional :: expected
        character(len=64) :: owner
        character(len=2048) :: line
        character(len=4096) :: metadata
        integer :: unit, local_err
        logical :: found

        owner = action_result_action_key(action_id)
        metadata = trim(root)//'/store/v2/.fx-metadata/leases'
        found = .false.
        open(newunit=unit, file=trim(metadata), status='old', action='read', &
            iostat=local_err)
        if (local_err == 0) then
            do
                read(unit, '(A)', iostat=local_err) line
                if (local_err /= 0) exit
                if (index(line, 'R||'//trim(owner)//'|fx-action-v1|'// &
                    trim(reason)//'|tree|'//trim(object_id)) > 0) found = .true.
            end do
            close(unit)
        end if
        if (present(expected)) then
            call test_assert(suite, found .eqv. expected, &
                'action result graph root matches retirement state')
        else
            call test_assert(suite, found, 'action result graph has a durable root')
        end if
    end subroutine assert_action_root

    subroutine test_crash_boundaries()
        call test_crash_before_conflict_rename()
        call test_crash_after_conflict_rename()
    end subroutine test_crash_boundaries

    subroutine test_crash_before_conflict_rename()
        integer(c_int) :: child, wait_status, killed, wait_rc
        character(len=64) :: result_id
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('crash-before')
        call action_result_publish(store, 'crash-before-action', race_entry_a, &
            result_id, ierr)
        call start_paused_conflict(1_c_int, 'crash-before', race_entry_b, child)
        if (child <= 0_c_int) return
        killed = kill_child(child, 9_c_int)
        wait_status = 0_c_int
        call bounded_child_wait(child, wait_status, wait_rc)
        call publication_probe_unlock(race_probe)
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'pre-rename crashed process is reaped')
        call test_assert(suite, wait_status /= 0_c_int, &
            'pre-rename process records forced termination')
        call test_assert_equal_int(suite, 0, int(killed), &
            'pre-rename publisher is killed at the crash barrier')
        call action_result_lookup(store, 'crash-before-action', restored_entries, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'crash before rename leaves the prior complete binding')
        call test_assert_equal_str(suite, trim(race_result_a), trim(result_id), &
            'pre-rename recovery retains the old result')
    end subroutine test_crash_before_conflict_rename

    subroutine test_crash_after_conflict_rename()
        integer(c_int) :: child, wait_status, killed, wait_rc
        character(len=64) :: result_id, ids(2)
        type(action_result_store_t) :: recovered_store
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)
        integer :: init_status, tree_status

        call prepare_race_entries('crash-after')
        call action_result_publish(store, 'crash-after-action', race_entry_a, &
            result_id, ierr)
        call start_paused_conflict(2_c_int, 'crash-after', race_entry_b, child)
        if (child <= 0_c_int) return
        killed = kill_child(child, 9_c_int)
        wait_status = 0_c_int
        call bounded_child_wait(child, wait_status, wait_rc)
        call publication_probe_unlock(race_probe)
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'post-rename crashed process is reaped')
        call test_assert(suite, wait_status /= 0_c_int, &
            'post-rename process records forced termination')
        call test_assert_equal_int(suite, 0, int(killed), &
            'post-rename publisher is killed at the crash barrier')
        call action_result_store_init(recovered_store, trim(root)//'/store/v2', &
            init_status)
        call test_assert_equal_int(suite, 0, init_status, &
            'result store reopens after process termination')
        call action_result_lookup(recovered_store, 'crash-after-action', &
            restored_entries, result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'crash after conflict rename recovers as quarantined')
        call action_result_conflicts(recovered_store, 'crash-after-action', &
            ids, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'recovered conflict record exposes both result IDs')
        call test_assert(suite, includes_id(ids, race_result_a), &
            'recovery retains the exact original result ID')
        call test_assert(suite, includes_id(ids, race_result_b), &
            'recovery retains the exact conflicting result ID')
        call immutable_store_verify_tree(recovered_store%objects, race_result_a, &
            tree_status)
        call test_assert_equal_int(suite, IMMUTABLE_OK, tree_status, &
            'original manifest remains verifiable after conflict recovery')
        call immutable_store_verify_tree(recovered_store%objects, race_result_b, &
            tree_status)
        call test_assert_equal_int(suite, IMMUTABLE_OK, tree_status, &
            'conflicting manifest remains verifiable after conflict recovery')
    end subroutine test_crash_after_conflict_rename

    subroutine test_complete_outputs()
        character(len=512) :: sources(4), destinations(4), destination
        character(len=512) :: c_source, object_file, consumer_source
        character(len=512) :: producer_source
        integer :: command_status, command_status_run, executable_index
        integer :: archive_index, shared_index, runtime_index, i

        c_source = trim(root)//'/demo.f90'
        producer_source = trim(root)//'/program.f90'
        object_file = trim(root)//'/demo.o'
        sources(1) = trim(root)//'/program'
        call write_text(trim(producer_source), &
            'program action43'//achar(10)// &
            'print *, "ACTION43"'//achar(10)//'end program'//achar(10))
        call run_native(command_status, 'gfortran', trim(producer_source), '-o', &
            trim(sources(1)))
        call test_assert_equal_int(suite, 0, command_status, &
            'producer creates a native executable output')
        call write_text(trim(c_source), &
            'integer function fx_demo()'//achar(10)// &
            'fx_demo = 43'//achar(10)//'end function'//achar(10))
        sources(2) = trim(root)//'/libdemo.a'
        sources(3) = trim(root)//'/libdemo.so'
        call run_native(command_status, 'gfortran', '-fPIC', '-c', trim(c_source), &
            '-o', trim(object_file))
        if (command_status == 0) call run_native(command_status, 'ar', 'rcs', &
            trim(sources(2)), trim(object_file))
        if (command_status == 0) call run_native(command_status, 'gfortran', &
            '-shared', trim(object_file), '-o', trim(sources(3)))
        call test_assert_equal_int(suite, 0, command_status, &
            'producer creates a real archive and shared library')
        sources(4) = trim(root)//'/runtime.dat'
        call write_text(trim(sources(4)), 'runtime companion data')
        files(1) = output_entry('program', 'executable', 493)
        files(2) = output_entry('libdemo.a', 'archive', 420)
        files(3) = output_entry('libdemo.so', 'shared-library', 493)
        files(4) = output_entry('runtime.dat', 'runtime-companion', 420)
        call action_result_publish_files(store, 'complete-action', sources, &
            files, first_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'complete executable/archive/shared/runtime result publishes')
        call action_result_publish_files(store, 'complete-action', sources, &
            files, repeated_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'equal result publication is idempotent')
        call test_assert_equal_str(suite, trim(first_result), trim(repeated_result), &
            'equal publications keep the canonical result ID')

        call action_result_lookup(store, 'complete-action', restored, &
            second_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'complete result lookup validates every companion')
        call test_assert_equal_str(suite, trim(first_result), trim(second_result), &
            'lookup returns the published result ID')
        call test_assert_equal_int(suite, size(files), size(restored), &
            'result manifest retains every output')
        call test_assert(suite, has_role(restored, 'runtime-companion'), &
            'runtime companion is part of the result manifest')
        call test_assert(suite, has_role(restored, 'archive'), &
            'archive output is part of the result manifest')
        call test_assert(suite, has_role(restored, 'shared-library'), &
            'shared-library output is part of the result manifest')

        executable_index = role_index(restored, 'executable')
        archive_index = role_index(restored, 'archive')
        shared_index = role_index(restored, 'shared-library')
        runtime_index = role_index(restored, 'runtime-companion')
        do i = 1, size(restored)
            destinations(i) = trim(root)//'/restored-'//trim(restored(i)%path)
            call action_result_materialize_blob(store, restored(i)%object_id, &
                trim(destinations(i)), restored(i)%mode, ierr)
            call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
                'every manifest companion materializes for its consumer')
        end do
        destination = destinations(executable_index)
        call test_assert(suite, test_process_is_executable(trim(destination)) == 1, &
            'materialized native program retains executable mode')
        call run_native(command_status_run, trim(destination))
        call test_assert_equal_int(suite, 0, command_status_run, &
            'materialized native executable returns success')

        consumer_source = trim(root)//'/consumer.f90'
        call write_text(trim(consumer_source), &
            'program consumer'//achar(10)// &
            'integer, external :: fx_demo'//achar(10)// &
            'if (fx_demo() /= 43) stop 1'//achar(10)//'end program'//achar(10))
        call run_native(command_status, 'gfortran', trim(consumer_source), &
            trim(destinations(archive_index)), '-o', trim(root)//'/use-archive')
        if (command_status == 0) call run_native(command_status, &
            trim(root)//'/use-archive')
        call test_assert_equal_int(suite, 0, command_status, &
            'materialized archive links and runs in a real consumer')
        call run_native(command_status, 'gfortran', trim(consumer_source), &
            trim(destinations(shared_index)), '-Wl,-rpath,'//trim(root), '-o', &
            trim(root)//'/use-shared')
        if (command_status == 0) call run_native(command_status, &
            trim(root)//'/use-shared')
        call test_assert_equal_int(suite, 0, command_status, &
            'materialized shared library links and runs in a real consumer')
        call test_assert(suite, file_has_bytes(trim(destinations(runtime_index)), &
            'runtime companion data'), &
            'materialized runtime companion bytes reach the consumer')
    end subroutine test_complete_outputs

    subroutine test_conflicting_publication()
        character(len=512) :: source
        type(immutable_tree_entry_t), allocatable :: ignored(:)

        source = trim(root)//'/conflicting-output'
        call write_text(trim(source), 'DIFFERENT OUTPUT')
        conflict_files(1) = output_entry('program', 'executable', 493)
        call action_result_publish_files(store, 'complete-action', [source], &
            conflict_files, second_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_CONFLICT, ierr, &
            'different result IDs atomically report nondeterminism')
        call test_assert(suite, first_result /= second_result, &
            'conflicting output receives a distinct result ID')

        call action_result_lookup(store, 'complete-action', ignored, &
            later_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'conflicted action can never be restored as a hit')
        call action_result_conflicts(store, 'complete-action', conflict_ids, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'durable conflict evidence is readable')
        call test_assert(suite, includes_id(conflict_ids, first_result), &
            'conflict evidence retains the first result')
        call test_assert(suite, includes_id(conflict_ids, second_result), &
            'conflict evidence retains the second result')

        source = trim(root)//'/later-output'
        call write_text(trim(source), 'THIRD OUTPUT')
        later_files(1) = output_entry('program', 'executable', 493)
        call action_result_publish_files(store, 'complete-action', [source], &
            later_files, later_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'later producer cannot clear or replace quarantine')
        call action_result_lookup(store, 'complete-action', ignored, &
            later_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'later lookup remains quarantined')
    end subroutine test_conflicting_publication

    subroutine test_missing_companion()
        character(len=512) :: sources(2)
        type(immutable_tree_entry_t), allocatable :: ignored(:)
        character(len=64) :: result_id, missing_blob
        character(len=512) :: blob_source

        sources(1) = trim(root)//'/output-1'
        sources(2) = trim(root)//'/does-not-exist'
        missing_files(1) = output_entry('program', 'executable', 493)
        missing_files(2) = output_entry('runtime.dat', 'runtime-companion', 420)
        call action_result_publish_files(store, 'missing-companion', sources, &
            missing_files, result_id, ierr)
        call test_assert(suite, ierr /= ACTION_RESULT_OK, &
            'missing required runtime companion prevents publication')
        call action_result_lookup(store, 'missing-companion', ignored, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_MISSING, ierr, &
            'failed incomplete output set leaves no binding')

        blob_source = trim(root)//'/published-then-removed'
        call write_text(trim(blob_source), 'PUBLISHED COMPANION')
        call action_result_put_blob(store, trim(blob_source), missing_blob, ierr)
        missing_files(1) = output_entry('runtime.dat', 'runtime-companion', 420)
        missing_files(1)%object_id = missing_blob
        call action_result_publish(store, 'removed-companion', missing_files(1:1), &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'complete companion publishes before removal')
        result_path = immutable_store_blob_path(store%objects, missing_blob)
        ierr = fx_test_remove_tree(result_path)
        call test_assert_equal_int(suite, 0, ierr, 'companion file is removed')
        call action_result_lookup(store, 'removed-companion', ignored, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_CORRUPT, ierr, &
            'missing referenced companion invalidates an established result')
    end subroutine test_missing_companion

    subroutine test_corrupt_companion()
        character(len=512) :: source
        character(len=64) :: result_id
        type(immutable_tree_entry_t), allocatable :: ignored(:)
        integer :: unit

        source = trim(root)//'/corrupt-source'
        call write_text(trim(source), 'VALID BEFORE CORRUPTION')
        corrupt_files(1) = output_entry('libdemo.so', 'shared-library', 493)
        call action_result_publish_files(store, 'corrupt-companion', [source], &
            corrupt_files, corrupt_result, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'shared output publishes before corruption test')
        result_path = immutable_store_blob_path(store%objects, &
            corrupt_files(1)%object_id)
        ierr = fx_test_chmod(result_path, 420)
        call test_assert_equal_int(suite, 0, ierr, 'companion is writable for corruption')
        open (newunit=unit, file=result_path, status='replace', &
            access='stream', form='unformatted')
        write (unit) 'CORRUPTED'
        close (unit)
        call action_result_lookup(store, 'corrupt-companion', ignored, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_CORRUPT, ierr, &
            'corrupt required companion invalidates result lookup')
    end subroutine test_corrupt_companion

    function output_entry(path, role, mode) result(entry)
        character(len=*), intent(in) :: path, role
        integer, intent(in) :: mode
        type(immutable_tree_entry_t) :: entry

        entry%path = path
        entry%role = role
        entry%mode = mode
    end function output_entry

    function digit(value) result(text)
        integer, intent(in) :: value
        character(len=1) :: text
        write (text, '(i1)') value
    end function digit

    logical function has_role(entries, role)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: role
        integer :: i

        has_role = .false.
        do i = 1, size(entries)
            if (entries(i)%role == role) has_role = .true.
        end do
    end function has_role

    integer function role_index(entries, role)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: role
        integer :: i

        role_index = 0
        do i = 1, size(entries)
            if (entries(i)%role == role) role_index = i
        end do
    end function role_index

    logical function includes_id(ids, id)
        character(len=64), intent(in) :: ids(:)
        character(len=*), intent(in) :: id

        includes_id = any(ids == id)
    end function includes_id

    subroutine run_native(exit_status, arg1, arg2, arg3, arg4, arg5, arg6)
        integer, intent(out) :: exit_status
        character(len=*), intent(in) :: arg1
        character(len=*), intent(in), optional :: arg2, arg3, arg4, arg5, arg6
        character(len=512) :: arguments(6)
        integer :: count
        arguments = ''
        arguments(1) = arg1
        count = 1
        if (present(arg2)) then
            arguments(2) = arg2
            count = 2
        end if
        if (present(arg3)) then
            arguments(3) = arg3
            count = 3
        end if
        if (present(arg4)) then
            arguments(4) = arg4
            count = 4
        end if
        if (present(arg5)) then
            arguments(5) = arg5
            count = 5
        end if
        if (present(arg6)) then
            arguments(6) = arg6
            count = 6
        end if
        call proc_exec_silent(arguments, count, exit_status)
    end subroutine run_native

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit

        open (newunit=unit, file=trim(path), status='replace', &
            access='stream', form='unformatted')
        write (unit) text
        close (unit)
    end subroutine write_text

    logical function file_has_bytes(path, expected)
        character(len=*), intent(in) :: path, expected
        character(len=len(expected)) :: actual
        integer :: unit, size_bytes, status

        file_has_bytes = .false.
        inquire(file=trim(path), size=size_bytes)
        if (size_bytes /= len(expected)) return
        open (newunit=unit, file=trim(path), status='old', &
            access='stream', form='unformatted', iostat=status)
        if (status /= 0) return
        read (unit, iostat=status) actual
        close (unit)
        if (status == 0) file_has_bytes = actual == expected
    end function file_has_bytes

end program test_action_result_store
