program test_action_result_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_proc, only: proc_pid
    use fx_immutable_store, only: immutable_tree_entry_t, &
        immutable_store_blob_path
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_publish_files, &
        action_result_lookup, action_result_conflicts, &
        action_result_put_blob, action_result_publish, &
        action_result_materialize_blob, ACTION_RESULT_OK, &
        ACTION_RESULT_CONFLICT, ACTION_RESULT_QUARANTINED, &
        ACTION_RESULT_MISSING, ACTION_RESULT_CORRUPT
    implicit none

    interface
        integer(c_int) function fork_process() bind(C, name='fork')
            import c_int
        end function fork_process
        integer(c_int) function wait_child(pid, status, options) bind(C, name='waitpid')
            import c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
        end function wait_child
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
        integer(c_int) function configure_barrier(phase, ready, release) &
                bind(C, name='fx_action_result_test_configure')
            import c_char, c_int
            integer(c_int), value :: phase
            character(kind=c_char), intent(in) :: ready(*), release(*)
        end function configure_barrier
        integer(c_int) function tmp_root(out, cap) &
                bind(C, name='fx_immutable_test_tmp_root')
            import c_char, c_int
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function tmp_root
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
    character(len=512) :: race_ready, race_release
    type(immutable_tree_entry_t) :: race_entry_a(1), race_entry_b(1)
    character(len=64) :: race_result_a, race_result_b

    call test_suite_init(suite, 'fx_action_result_store')
    scratch = c_null_char
    ierr = tmp_root(scratch, 512_c_int)
    call test_assert_equal_int(suite, 0, ierr, 'physical test root resolves')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-action43-', proc_pid()
    call execute_command_line('mkdir -p -- '//trim(root))
    call action_result_store_init(store, trim(root)//'/store/v2', ierr)
    call test_assert_equal_int(suite, 0, ierr, 'versioned action store initializes')

    call test_complete_outputs()
    call test_conflicting_publication()
    call test_missing_companion()
    call test_corrupt_companion()
    call test_equal_concurrent_publishers()
    call test_conflicting_concurrent_publishers()
    call test_crash_boundaries()

    call execute_command_line('rm -rf -- '//trim(root))
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

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

    subroutine start_paused_conflict(phase, tag, entries, child)
        integer(c_int), intent(in) :: phase
        character(len=*), intent(in) :: tag
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        integer(c_int), intent(out) :: child
        integer(c_int) :: configured
        character(len=512) :: ready_path, release_path

        ready_path = trim(root)//'/'//trim(tag)//'-ready'
        release_path = trim(root)//'/'//trim(tag)//'-release'
        race_ready = ready_path
        race_release = release_path
        configured = configure_barrier(phase, trim(ready_path)//c_null_char, &
            trim(release_path)//c_null_char)
        call test_assert_equal_int(suite, 0, int(configured), &
            'crash barrier configuration succeeds')
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'crash publisher forks')
        if (child == 0_c_int) &
            call concurrent_publish_child(trim(tag)//'-action', entries)
    end subroutine start_paused_conflict

    subroutine wait_for_file(path)
        character(len=*), intent(in) :: path
        logical :: exists
        integer :: attempt

        exists = .false.
        do attempt = 1, 1000
            inquire(file=trim(path), exist=exists)
            if (exists) return
            ierr = sleep_us(10000_c_int)
        end do
        call test_assert(suite, .false., 'publisher reaches the configured crash barrier')
    end subroutine wait_for_file

    subroutine test_equal_concurrent_publishers()
        integer(c_int) :: child, wait_status, wait_rc
        character(len=64) :: result_id
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('equal-race')
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'equal publication worker forks')
        if (child == 0_c_int) &
            call concurrent_publish_child('equal-race-action', race_entry_a)
        call action_result_publish(store, 'equal-race-action', race_entry_a, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'parent equal producer publishes successfully')
        wait_status = 0_c_int
        wait_rc = wait_child(child, wait_status, 0_c_int)
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
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('conflict-race')
        child = fork_process()
        call test_assert(suite, child >= 0_c_int, 'conflicting publisher forks')
        if (child == 0_c_int) &
            call concurrent_publish_child('conflict-race-action', race_entry_b)
        call action_result_publish(store, 'conflict-race-action', race_entry_a, &
            result_id, ierr)
        call test_assert(suite, ierr == ACTION_RESULT_OK .or. &
            ierr == ACTION_RESULT_CONFLICT, 'parent race producer linearizes')
        wait_status = 0_c_int
        wait_rc = wait_child(child, wait_status, 0_c_int)
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
    end subroutine test_conflicting_concurrent_publishers

    subroutine test_crash_boundaries()
        call test_crash_before_conflict_rename()
        call test_crash_after_conflict_rename()
    end subroutine test_crash_boundaries

    subroutine test_crash_before_conflict_rename()
        integer(c_int) :: child, wait_status, configured, killed, wait_rc
        character(len=64) :: result_id
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)

        call prepare_race_entries('crash-before')
        call action_result_publish(store, 'crash-before-action', race_entry_a, &
            result_id, ierr)
        call start_paused_conflict(1_c_int, 'crash-before', race_entry_b, child)
        if (child <= 0_c_int) return
        call wait_for_file(trim(race_ready))
        killed = kill_child(child, 9_c_int)
        wait_status = 0_c_int
        wait_rc = wait_child(child, wait_status, 0_c_int)
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'pre-rename crashed process is reaped')
        call test_assert(suite, wait_status /= 0_c_int, &
            'pre-rename process records forced termination')
        configured = configure_barrier(0_c_int, ''//c_null_char, ''//c_null_char)
        call test_assert_equal_int(suite, 0, int(killed), &
            'pre-rename publisher is killed at the crash barrier')
        call test_assert_equal_int(suite, 0, int(configured), &
            'pre-rename barrier is disabled after recovery')
        call action_result_lookup(store, 'crash-before-action', restored_entries, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'crash before rename leaves the prior complete binding')
        call test_assert_equal_str(suite, trim(race_result_a), trim(result_id), &
            'pre-rename recovery retains the old result')
    end subroutine test_crash_before_conflict_rename

    subroutine test_crash_after_conflict_rename()
        integer(c_int) :: child, wait_status, configured, killed, wait_rc
        character(len=64) :: result_id
        type(action_result_store_t) :: recovered_store
        type(immutable_tree_entry_t), allocatable :: restored_entries(:)
        integer :: init_status

        call prepare_race_entries('crash-after')
        call action_result_publish(store, 'crash-after-action', race_entry_a, &
            result_id, ierr)
        call start_paused_conflict(2_c_int, 'crash-after', race_entry_b, child)
        if (child <= 0_c_int) return
        call wait_for_file(trim(race_ready))
        killed = kill_child(child, 9_c_int)
        wait_status = 0_c_int
        wait_rc = wait_child(child, wait_status, 0_c_int)
        call test_assert_equal_int(suite, int(child), int(wait_rc), &
            'post-rename crashed process is reaped')
        call test_assert(suite, wait_status /= 0_c_int, &
            'post-rename process records forced termination')
        configured = configure_barrier(0_c_int, ''//c_null_char, ''//c_null_char)
        call test_assert_equal_int(suite, 0, int(killed), &
            'post-rename publisher is killed at the crash barrier')
        call test_assert_equal_int(suite, 0, int(configured), &
            'post-rename barrier is disabled after recovery')
        call action_result_store_init(recovered_store, trim(root)//'/store/v2', &
            init_status)
        call test_assert_equal_int(suite, 0, init_status, &
            'result store reopens after process termination')
        call action_result_lookup(recovered_store, 'crash-after-action', &
            restored_entries, result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_QUARANTINED, ierr, &
            'crash after conflict rename recovers as quarantined')
    end subroutine test_crash_after_conflict_rename

    subroutine test_complete_outputs()
        character(len=512) :: sources(4), destination
        integer :: i

        character(len=512) :: c_source, object_file
        integer :: command_status, command_status_run, executable_index

        c_source = trim(root)//'/demo.c'
        object_file = trim(root)//'/demo.o'
        sources(1) = trim(root)//'/program'
        call write_text(trim(sources(1)), '#!/bin/sh'//achar(10)// &
            'printf ACTION43'//achar(10))
        call execute_command_line('chmod 755 -- '//trim(sources(1)), &
            exitstat=command_status)
        call test_assert_equal_int(suite, 0, command_status, &
            'producer creates an executable output')
        call write_text(trim(c_source), 'int fx_demo(void) { return 43; }'//achar(10))
        sources(2) = trim(root)//'/libdemo.a'
        sources(3) = trim(root)//'/libdemo.so'
        call execute_command_line('cc -fPIC -c '//trim(c_source)//' -o '// &
            trim(object_file)//' && ar rcs '//trim(sources(2))//' '// &
            trim(object_file)//' && cc -shared '//trim(object_file)//' -o '// &
            trim(sources(3)), exitstat=command_status)
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

        executable_index = role_index(restored, 'executable')
        destination = trim(root)//'/restored-program'
        call action_result_materialize_blob(store, &
            restored(executable_index)%object_id, trim(destination), &
            restored(executable_index)%mode, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'real executable payload materializes')
        call test_assert(suite, file_has_bytes(trim(destination), &
            '#!/bin/sh'//achar(10)//'printf ACTION43'//achar(10)), &
            'materialized executable bytes match producer output')
        call execute_command_line(trim(destination), exitstat=command_status_run, &
            cmdstat=command_status)
        call test_assert_equal_int(suite, 0, command_status, &
            'materialized executable starts successfully')
        call test_assert_equal_int(suite, 0, command_status_run, &
            'materialized executable returns success')
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
        call execute_command_line('rm -f -- '//result_path)
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
        call execute_command_line('chmod u+w -- '//result_path)
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
