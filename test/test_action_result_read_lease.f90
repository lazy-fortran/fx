program test_action_result_read_lease
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree
    use fx_test_process, only: test_process_spawn, test_process_wait_once, &
        test_process_signal, test_process_sleep_ms
    use fx_immutable_manifest, only: immutable_tree_entry_t
    use fx_action_result_store, only: action_result_store_t, &
        action_result_read_t, action_result_store_init, &
        action_result_read_acquire, action_result_read_lookup, &
        action_result_read_release, action_result_publish_files, &
        action_result_materialize_blob, action_result_action_key, &
        ACTION_RESULT_OK, ACTION_RESULT_CONFLICT, ACTION_RESULT_QUARANTINED
    implicit none

    type(test_suite_t) :: suite
    type(action_result_store_t) :: store
    character(len=512) :: root, store_root, executable, args(10)
    character(len=512) :: sources(2), dest_dir, ready, release, done
    character(len=512) :: finish, released
    character(len=512) :: conflict_source(1)
    character(len=64) :: old_id, conflict_id, action_id
    type(immutable_tree_entry_t) :: entries(2), conflict_entry(1)
    integer :: ierr, child, status, cleanup
    logical :: completed

    call get_command_argument(1, executable)
    if (trim(executable) == '--prelookup-reader') then
        call run_prelookup_reader()
        stop 0
    end if
    if (trim(executable) == '--restore-reader') then
        call run_restore_reader()
        stop 0
    end if

    call test_suite_init(suite, 'fx_action_result_read_lease')
    write(root, '(A,I0)') '/var/tmp/fx_action_read_', proc_pid()
    cleanup = fx_test_remove_tree(trim(root))
    cleanup = fx_test_mkdir_p(trim(root))
    call test_assert_equal_int(suite, 0, cleanup, 'isolated fixture root created')
    store_root = trim(root)//'/store/v2'
    call action_result_store_init(store, trim(store_root), ierr)
    call test_assert_equal_int(suite, 0, ierr, 'action result store initializes')

    sources = [character(len=512) :: trim(root)//'/source-a', &
        trim(root)//'/source-b']
    call write_text(trim(sources(1)), 'first companion bytes')
    call write_text(trim(sources(2)), 'second companion bytes')
    entries(1) = result_entry('one.bin', 'runtime-companion', 420)
    entries(2) = result_entry('two.bin', 'runtime-companion', 420)
    conflict_source(1) = trim(root)//'/conflict-source'
    call write_text(trim(conflict_source(1)), 'conflicting graph bytes')
    conflict_entry(1) = result_entry('other.bin', 'runtime-companion', 420)
    call get_command_argument(0, executable)

    action_id = 'prelookup-race'
    call action_result_publish_files(store, trim(action_id), sources, entries, &
        old_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
        'pre-lookup graph publishes')
    ready = trim(root)//'/prelookup-ready'
    release = trim(root)//'/prelookup-release'
    done = trim(root)//'/prelookup-done'
    args(1:7) = [character(len=512) :: trim(executable), &
        '--prelookup-reader', trim(store_root), trim(action_id), trim(ready), &
        trim(release), trim(done)]
    call test_process_spawn(args(1:7), child, ierr)
    call test_assert_equal_int(suite, 0, ierr, &
        'native reader starts before action binding lookup')
    call wait_for_file(trim(ready), completed)
    call test_assert(suite, completed, 'reader pauses after graph lease acquire')
    call assert_graph_lease(trim(action_id), trim(old_id), .true., &
        'pre-lookup lease protects the bound graph')
    call action_result_publish_files(store, trim(action_id), conflict_source, &
        conflict_entry, conflict_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_CONFLICT, ierr, &
        'concurrent publisher changes binding to conflict')
    call assert_graph_lease(trim(action_id), trim(old_id), .true., &
        'pre-lookup lease survives binding conflict publication')
    call write_text(trim(release), 'continue')
    call wait_for_child(child, 15000, status, completed)
    call test_assert(suite, completed, 'pre-lookup reader exits and is reaped')
    call test_assert_equal_int(suite, 0, status, &
        'pre-lookup reader reports the durable conflict')
    call assert_graph_lease(trim(action_id), trim(old_id), .false., &
        'pre-lookup reader releases its own graph lease')

    action_id = 'restore-lifetime'
    call action_result_publish_files(store, trim(action_id), sources, entries, &
        old_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
        'restoration graph publishes')
    ready = trim(root)//'/restore-ready'
    release = trim(root)//'/restore-release'
    done = trim(root)//'/restore-done'
    finish = trim(root)//'/restore-finish'
    released = trim(root)//'/restore-released'
    dest_dir = trim(root)//'/restored'
    cleanup = fx_test_mkdir_p(trim(dest_dir))
    call test_assert_equal_int(suite, 0, cleanup, 'restore destination created')
    args(1:9) = [character(len=512) :: trim(executable), &
        '--restore-reader', trim(store_root), trim(action_id), trim(ready), &
        trim(release), trim(done), trim(dest_dir), trim(finish)]
    args(10) = trim(released)
    call test_process_spawn(args, child, ierr)
    call test_assert_equal_int(suite, 0, ierr, 'native restore reader starts')
    call wait_for_file(trim(ready), completed)
    call test_assert(suite, completed, 'reader pauses after loading result manifest')
    call assert_graph_lease(trim(action_id), trim(old_id), .true., &
        'read lease remains active during companion restoration')
    call action_result_publish_files(store, trim(action_id), conflict_source, &
        conflict_entry, conflict_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_CONFLICT, ierr, &
        'restoration races with conflicting publication')
    call assert_graph_lease(trim(action_id), trim(old_id), .true., &
        'old graph remains leased after conflict replacement')
    call write_text(trim(release), 'continue')
    call wait_for_file(trim(done), completed)
    call test_assert(suite, completed, 'all companions replace before release')
    call assert_graph_lease(trim(action_id), trim(old_id), .true., &
        'graph lease remains through every output replacement')
    call write_text(trim(finish), 'release-lease')
    call wait_for_file(trim(released), completed)
    call test_assert(suite, completed, 'reader releases after output commit')
    call wait_for_child(child, 15000, status, completed)
    call test_assert(suite, completed, 'restore reader exits and is reaped')
    call test_assert_equal_int(suite, 0, status, &
        'restore reader materializes every companion successfully')
    call test_assert(suite, file_has_bytes(trim(dest_dir)//'/one.bin', &
        'first companion bytes'), 'first companion restored from leased graph')
    call test_assert(suite, file_has_bytes(trim(dest_dir)//'/two.bin', &
        'second companion bytes'), 'second companion restored from leased graph')
    call assert_graph_lease(trim(action_id), trim(old_id), .false., &
        'complete restore releases its graph lease')
    cleanup = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, cleanup, 'fixture tree is removed')
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine run_prelookup_reader()
        type(action_result_store_t) :: child_store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: restored(:)
        character(len=512) :: child_root, child_action, child_ready
        character(len=512) :: child_release, child_done
        character(len=64) :: result_id
        integer :: local_err, release_err

        call get_command_argument(2, child_root)
        call get_command_argument(3, child_action)
        call get_command_argument(4, child_ready)
        call get_command_argument(5, child_release)
        call get_command_argument(6, child_done)
        call action_result_store_init(child_store, trim(child_root), local_err)
        if (local_err /= 0) stop 51
        call action_result_read_acquire(child_store, trim(child_action), read, &
            local_err)
        if (local_err /= ACTION_RESULT_OK) stop 52
        call write_text(trim(child_ready), 'leased-before-lookup')
        call wait_for_file(trim(child_release), completed)
        if (.not. completed) stop 53
        call action_result_read_lookup(child_store, read, restored, result_id, &
            local_err)
        if (local_err /= ACTION_RESULT_QUARANTINED) stop 54
        call action_result_read_release(child_store, read, release_err)
        if (release_err /= ACTION_RESULT_OK) stop 55
        call write_text(trim(child_done), 'conflict-observed')
    end subroutine run_prelookup_reader

    subroutine run_restore_reader()
        type(action_result_store_t) :: child_store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: restored(:)
        character(len=512) :: child_root, child_action, child_ready
        character(len=512) :: child_release, child_done, child_dest
        character(len=512) :: child_finish, child_released
        character(len=64) :: result_id
        integer :: local_err, release_err, i

        call get_command_argument(2, child_root)
        call get_command_argument(3, child_action)
        call get_command_argument(4, child_ready)
        call get_command_argument(5, child_release)
        call get_command_argument(6, child_done)
        call get_command_argument(7, child_dest)
        call get_command_argument(8, child_finish)
        call get_command_argument(9, child_released)
        call action_result_store_init(child_store, trim(child_root), local_err)
        if (local_err /= 0) stop 61
        call action_result_read_acquire(child_store, trim(child_action), read, &
            local_err)
        if (local_err /= ACTION_RESULT_OK) stop 62
        call action_result_read_lookup(child_store, read, restored, result_id, &
            local_err)
        if (local_err /= ACTION_RESULT_OK) stop 63
        call write_text(trim(child_ready), 'manifest-loaded')
        call wait_for_file(trim(child_release), completed)
        if (.not. completed) stop 64
        do i = 1, size(restored)
            call action_result_materialize_blob(child_store, &
                restored(i)%object_id, trim(child_dest)//'/'// &
                trim(restored(i)%path), restored(i)%mode, local_err)
            if (local_err /= ACTION_RESULT_OK) stop 65
        end do
        call write_text(trim(child_done), 'all-replaced')
        call wait_for_file(trim(child_finish), completed)
        if (.not. completed) stop 66
        call action_result_read_release(child_store, read, release_err)
        if (release_err /= ACTION_RESULT_OK) stop 67
        call write_text(trim(child_released), 'lease-released')
    end subroutine run_restore_reader

    function result_entry(path, role, mode) result(entry)
        character(len=*), intent(in) :: path, role
        integer, intent(in) :: mode
        type(immutable_tree_entry_t) :: entry

        entry%path = path
        entry%role = role
        entry%mode = mode
    end function result_entry

    subroutine assert_graph_lease(action, object_id, expected, label)
        character(len=*), intent(in) :: action, object_id, label
        logical, intent(in) :: expected
        character(len=64) :: owner
        character(len=2048) :: line
        character(len=4096) :: metadata, needle
        integer :: unit, local_err
        logical :: found

        owner = action_result_action_key(action)
        metadata = trim(store_root)//'/.fx-metadata/leases'
        needle = '|'//trim(owner)//'|fx-action-v1|action-read|tree|'// &
            trim(object_id)
        found = .false.
        open(newunit=unit, file=trim(metadata), status='old', action='read', &
            iostat=local_err)
        if (local_err == 0) then
            do
                read(unit, '(A)', iostat=local_err) line
                if (local_err /= 0) exit
                if (line(1:2) == 'L|' .and. index(line, trim(needle)) > 0) &
                    found = .true.
            end do
            close(unit)
        end if
        call test_assert(suite, found .eqv. expected, label)
    end subroutine assert_graph_lease

    subroutine wait_for_file(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer :: i

        found = .false.
        do i = 1, 1500
            inquire(file=trim(path), exist=found)
            if (found) return
            call test_process_sleep_ms(10)
        end do
    end subroutine wait_for_file

    subroutine wait_for_child(pid, timeout_ms, exit_status, reaped)
        integer, intent(in) :: pid, timeout_ms
        integer, intent(out) :: exit_status
        logical, intent(out) :: reaped
        integer :: child_state, signal_err, i

        reaped = .false.
        exit_status = -1
        do i = 1, timeout_ms / 5
            call test_process_wait_once(pid, exit_status, child_state)
            if (child_state == 1) then
                reaped = .true.
                return
            end if
            if (child_state < 0) exit
            call test_process_sleep_ms(5)
        end do
        call test_process_signal(pid, 9, signal_err)
        do i = 1, 1000
            call test_process_wait_once(pid, exit_status, child_state)
            if (child_state == 1) then
                reaped = .true.
                return
            end if
            if (child_state < 0) return
            call test_process_sleep_ms(5)
        end do
    end subroutine wait_for_child

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, local_err

        open(newunit=unit, file=trim(path), status='replace', &
            access='stream', form='unformatted', iostat=local_err)
        if (local_err /= 0) return
        write(unit, iostat=local_err) text
        close(unit)
    end subroutine write_text

    logical function file_has_bytes(path, expected)
        character(len=*), intent(in) :: path, expected
        character(len=len(expected)) :: actual
        integer :: unit, size_bytes, local_err

        file_has_bytes = .false.
        inquire(file=trim(path), size=size_bytes)
        if (size_bytes /= len(expected)) return
        open(newunit=unit, file=trim(path), status='old', &
            access='stream', form='unformatted', iostat=local_err)
        if (local_err /= 0) return
        read(unit, iostat=local_err) actual
        close(unit)
        if (local_err == 0) file_has_bytes = actual == expected
    end function file_has_bytes

end program test_action_result_read_lease
