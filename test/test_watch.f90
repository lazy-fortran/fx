program test_watch
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
                       test_assert_equal_int, test_assert_equal_str, &
                       test_suite_summary, test_suite_exit
    use fx_watch, only: WATCH_CREATE, WATCH_DELETE, WATCH_MODIFY, &
                        watcher_t, watcher_init, watcher_add, &
                        watcher_remove, watcher_poll, &
                        watcher_mark_self_written, watcher_close
    implicit none

    interface
        subroutine fx_c_inotify_test_force_enospc_once() bind(C)
        end subroutine fx_c_inotify_test_force_enospc_once

        integer(c_int) function fx_c_stderr_redirect(path) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fx_c_stderr_redirect

        integer(c_int) function fx_c_stderr_restore(saved_fd) bind(C)
            import :: c_int
            integer(c_int), intent(in), value :: saved_fd
        end function fx_c_stderr_restore
    end interface

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_watch')
    call test_watch_init_close(suite)
    call test_watch_recursive_existing_dir(suite)
    call test_watch_recursive_new_dir(suite)
    call test_watch_self_write_window(suite)
    call test_watch_delete_and_close(suite)
    call test_watch_moved_to_is_create(suite)
    call test_watch_symlink_directory_limit(suite)
    call test_watch_enospc_warning(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_watch_init_close(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        integer :: ierr

        call watcher_init(w, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'watcher_init ierr')
        call test_assert(suite, w%fd > 0, 'watcher_init returns fd')

        call watcher_close(w)
        call test_assert_equal_int(suite, -1, w%fd, 'watcher_close resets fd')
        call test_assert_equal_int(suite, 0, w%n_watches, &
                                   'watcher_close clears watches')
        call test_assert_equal_int(suite, 0, w%n_self_written, &
                                   'watcher_close clears self writes')
    end subroutine test_watch_init_close

    subroutine test_watch_recursive_existing_dir(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: nested
        character(len=:), allocatable :: file_path
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('existing')
        nested = join_path(root, 'src/nested')
        file_path = join_path(nested, 'module.f90')

        call run_cmd('mkdir -p -- ' // trim(nested))
        call write_file(file_path, 'module a')

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'recursive add ierr')

        call write_file(file_path, 'module b')
        call watcher_poll(w, changed_path, event_type, 1000, got_event)

        call test_assert(suite, got_event, 'recursive existing dir got event')
        call test_assert_equal_int(suite, WATCH_MODIFY, event_type, &
                                   'recursive existing dir event type')
        call test_assert_equal_str(suite, trim(file_path), trim(changed_path), &
                                   'recursive existing dir path')

        call watcher_close(w)
        call cleanup_tree(root)
    end subroutine test_watch_recursive_existing_dir

    subroutine test_watch_recursive_new_dir(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: new_dir
        character(len=:), allocatable :: preexisting_file
        character(len=:), allocatable :: later_file
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('newdir')
        new_dir = join_path(root, 'generated')
        preexisting_file = join_path(new_dir, 'preexisting.f90')
        later_file = join_path(new_dir, 'later.f90')

        call run_cmd('mkdir -p -- ' // trim(root))

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'new dir add ierr')

        call run_cmd('mkdir -p -- ' // trim(new_dir))
        call write_file(preexisting_file, 'module preexisting')
        call watcher_poll(w, changed_path, event_type, 1000, got_event)
        call test_assert(suite, got_event, 'new dir create got event')
        call test_assert_equal_int(suite, WATCH_CREATE, event_type, &
                                   'new dir create event type')
        call test_assert_equal_str(suite, trim(new_dir), trim(changed_path), &
                                   'new dir create path')

        call watcher_poll(w, changed_path, event_type, 1000, got_event)
        call test_assert(suite, got_event, 'preexisting file reported')
        call test_assert_equal_int(suite, WATCH_CREATE, event_type, &
                                   'preexisting file event type')
        call test_assert_equal_str(suite, trim(preexisting_file), &
                                   trim(changed_path), &
                                   'preexisting file path')

        call write_file(later_file, 'module later')
        call watcher_poll(w, changed_path, event_type, 1000, got_event)
        call test_assert(suite, got_event, 'new dir child got event')
        call test_assert_equal_int(suite, WATCH_CREATE, event_type, &
                                   'new dir child event type')
        call test_assert_equal_str(suite, trim(later_file), trim(changed_path), &
                                   'new dir child path')

        call watcher_close(w)
        call cleanup_tree(root)
    end subroutine test_watch_recursive_new_dir

    subroutine test_watch_self_write_window(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: file_path
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('selfwrite')
        file_path = join_path(root, 'self.f90')

        call run_cmd('mkdir -p -- ' // trim(root))
        call write_file(file_path, 'module self')

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'self write add ierr')

        call watcher_mark_self_written(w, file_path)
        call append_file(file_path, '!')
        call watcher_poll(w, changed_path, event_type, 250, got_event)
        call test_assert(suite, .not. got_event, 'self write suppressed')

        call watcher_mark_self_written(w, file_path)
        call wait_ms(600)
        call append_file(file_path, '?')
        call watcher_poll(w, changed_path, event_type, 1000, got_event)
        call test_assert(suite, got_event, 'expired self write reported')
        call test_assert_equal_int(suite, WATCH_MODIFY, event_type, &
                                   'expired self write event type')
        call test_assert_equal_str(suite, trim(file_path), trim(changed_path), &
                                   'expired self write path')

        call watcher_close(w)
        call cleanup_tree(root)
    end subroutine test_watch_self_write_window

    subroutine test_watch_delete_and_close(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: child_dir
        character(len=:), allocatable :: child_file
        character(len=:), allocatable :: sibling_file
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('delete')
        child_dir = join_path(root, 'gone')
        child_file = join_path(child_dir, 'gone.f90')
        sibling_file = join_path(root, 'survivor.f90')

        call run_cmd('mkdir -p -- ' // trim(child_dir))
        call write_file(child_file, 'module gone')
        call write_file(sibling_file, 'module survive')

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'delete add ierr')

        call run_cmd('rm -rf -- ' // trim(child_dir))
        call poll_until_match(w, child_dir, WATCH_DELETE, 200, 10, &
                              got_event, changed_path, event_type)
        call test_assert(suite, got_event, 'delete event received')
        call test_assert_equal_int(suite, WATCH_DELETE, event_type, &
                                   'delete event type')
        call test_assert_equal_str(suite, trim(child_dir), trim(changed_path), &
                                   'delete event path')

        call append_file(sibling_file, '!')
        call poll_until_match(w, sibling_file, WATCH_MODIFY, 1000, 10, &
                              got_event, changed_path, event_type)
        call test_assert(suite, got_event, 'sibling event after delete')
        call test_assert_equal_int(suite, WATCH_MODIFY, event_type, &
                                   'sibling event type after delete')
        call test_assert_equal_str(suite, trim(sibling_file), &
                                   trim(changed_path), &
                                   'sibling event path after delete')

        call watcher_close(w)
        call test_assert_equal_int(suite, -1, w%fd, 'close after delete resets fd')

        call cleanup_tree(root)
    end subroutine test_watch_delete_and_close

    subroutine test_watch_moved_to_is_create(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: staging
        character(len=:), allocatable :: source_file
        character(len=:), allocatable :: moved_file
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('moved')
        staging = join_path(root, 'staging')
        source_file = join_path(staging, 'incoming.f90')
        moved_file = join_path(root, 'arrived.f90')

        call run_cmd('mkdir -p -- ' // trim(staging))
        call write_file(source_file, 'module incoming')

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'moved add ierr')

        call run_cmd('mv -- ' // trim(source_file) // ' ' // trim(moved_file))
        call poll_until_match(w, moved_file, WATCH_CREATE, 200, 10, &
                              got_event, changed_path, event_type)
        call test_assert(suite, got_event, 'moved-to event received')
        call test_assert_equal_int(suite, WATCH_CREATE, event_type, &
                                   'moved-to maps to create')
        call test_assert_equal_str(suite, trim(moved_file), trim(changed_path), &
                                   'moved-to path')

        call watcher_close(w)
        call cleanup_tree(root)
    end subroutine test_watch_moved_to_is_create

    subroutine test_watch_symlink_directory_limit(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: external_root
        character(len=:), allocatable :: target_dir
        character(len=:), allocatable :: link_dir
        character(len=:), allocatable :: target_file
        character(len=4096) :: changed_path
        integer :: event_type
        integer :: ierr
        logical :: got_event

        root = temp_root('symlink')
        external_root = temp_root('symlink-outside')
        target_dir = join_path(external_root, 'target')
        link_dir = join_path(root, 'link')
        target_file = join_path(target_dir, 'linked.f90')

        call run_cmd('mkdir -p -- ' // trim(root))
        call run_cmd('mkdir -p -- ' // trim(target_dir))
        call write_file(target_file, 'module target')
        call run_cmd('ln -s -- ' // trim(target_dir) // ' ' // trim(link_dir))

        call watcher_init(w, ierr)
        call watcher_add(w, root, .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'symlink add ierr')

        call append_file(target_file, '!')
        call watcher_poll(w, changed_path, event_type, 250, got_event)
        call test_assert(suite, .not. got_event, 'symlink target ignored')

        call watcher_close(w)
        call cleanup_tree(root)
        call cleanup_tree(external_root)
    end subroutine test_watch_symlink_directory_limit

    subroutine test_watch_enospc_warning(suite)
        type(test_suite_t), intent(inout) :: suite
        type(watcher_t) :: w
        character(len=:), allocatable :: root
        character(len=:), allocatable :: stderr_path
        character(kind=c_char) :: c_stderr_path(4096)
        character(len=4096) :: stderr_text
        integer :: ierr
        integer :: saved_fd
        integer :: read_err

        root = temp_root('enospc')
        stderr_path = join_path(root, 'stderr.log')

        call run_cmd('mkdir -p -- ' // trim(root))
        call watcher_init(w, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'enospc init ierr')

        call to_c_string(stderr_path, c_stderr_path)
        saved_fd = fx_c_stderr_redirect(c_stderr_path)
        call test_assert(suite, saved_fd >= 0, 'stderr redirect started')
        if (saved_fd >= 0) then
            call fx_c_inotify_test_force_enospc_once()
            call watcher_add(w, root, .true., ierr)
            call test_assert(suite, ierr /= 0, 'enospc add fails')
            ierr = fx_c_stderr_restore(saved_fd)
            call test_assert_equal_int(suite, 0, ierr, 'stderr redirect restored')
            call read_text_file(stderr_path, stderr_text, read_err)
            call test_assert_equal_int(suite, 0, read_err, 'enospc stderr read')
            call test_assert(suite, index(stderr_text, &
                               'fx_watch: inotify watch limit reached for') > 0, &
                               'enospc warning emitted')
            call test_assert(suite, index(stderr_text, trim(root)) > 0, &
                               'enospc warning includes path')
        end if

        call watcher_close(w)
        call cleanup_tree(root)
    end subroutine test_watch_enospc_warning

    subroutine run_cmd(cmd)
        character(len=*), intent(in) :: cmd
        integer :: exitstat
        integer :: cmdstat

        call execute_command_line(trim(cmd), exitstat=exitstat, cmdstat=cmdstat, &
                                  wait=.true.)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            error stop 'command failed: ' // trim(cmd)
        end if
    end subroutine run_cmd

    subroutine write_file(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: text
        integer :: unit
        integer :: ios

        open(newunit=unit, file=trim(path), status='replace', action='write', &
             iostat=ios)
        if (ios /= 0) error stop 'write_file open failed: ' // trim(path)
        write(unit, '(A)', iostat=ios) trim(text)
        if (ios /= 0) error stop 'write_file write failed: ' // trim(path)
        close(unit)
    end subroutine write_file

    subroutine append_file(path, text)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: text
        integer :: unit
        integer :: ios

        open(newunit=unit, file=trim(path), status='old', action='write', &
             position='append', iostat=ios)
        if (ios /= 0) error stop 'append_file open failed: ' // trim(path)
        write(unit, '(A)', iostat=ios) trim(text)
        if (ios /= 0) error stop 'append_file write failed: ' // trim(path)
        close(unit)
    end subroutine append_file

    subroutine wait_ms(ms)
        integer, intent(in) :: ms
        integer :: start
        integer :: current
        integer :: rate
        integer :: elapsed

        call system_clock(count_rate=rate)
        call system_clock(count=start)
        do
            call system_clock(count=current)
            elapsed = (current - start) * 1000 / max(1, rate)
            if (elapsed >= ms) exit
        end do
    end subroutine wait_ms

    function temp_root(tag) result(path)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: path
        integer, save :: serial = 0
        integer :: count
        character(len=32) :: count_buf
        character(len=32) :: serial_buf

        serial = serial + 1
        call system_clock(count=count)
        write(count_buf, '(i0)') count
        write(serial_buf, '(i0)') serial
        path = '/tmp/fx-watch-' // trim(tag) // '-' // trim(count_buf) // &
            '-' // trim(serial_buf)
    end function temp_root

    function join_path(a, b) result(path)
        character(len=*), intent(in) :: a
        character(len=*), intent(in) :: b
        character(len=:), allocatable :: path

        if (len_trim(a) == 0) then
            path = trim(b)
        else if (a(len_trim(a):len_trim(a)) == '/') then
            path = trim(a) // trim(b)
        else
            path = trim(a) // '/' // trim(b)
        end if
    end function join_path

    subroutine cleanup_tree(path)
        character(len=*), intent(in) :: path

        if (len_trim(path) == 0) return
        call run_cmd('rm -rf -- ' // trim(path))
    end subroutine cleanup_tree

    subroutine read_text_file(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text
        integer, intent(out) :: ierr

        integer :: unit
        integer :: ios

        text = ''
        ierr = 0
        open(newunit=unit, file=trim(path), action='read', status='old', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        read(unit, '(A)', iostat=ios) text
        if (ios > 0) then
            ierr = 1
        end if
        close(unit)
    end subroutine read_text_file

    subroutine to_c_string(text, c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), intent(out) :: c_text(:)

        integer :: i
        integer :: n

        c_text = c_null_char
        n = min(len_trim(text), size(c_text) - 1)
        do i = 1, n
            c_text(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_text(n + 1) = c_null_char
    end subroutine to_c_string

    subroutine poll_until_match(w, expected_path, expected_type, timeout_ms, &
                                max_tries, matched, actual_path, actual_type)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: expected_path
        integer, intent(in) :: expected_type
        integer, intent(in) :: timeout_ms
        integer, intent(in) :: max_tries
        logical, intent(out) :: matched
        character(len=*), intent(out) :: actual_path
        integer, intent(out) :: actual_type

        integer :: i
        logical :: got_event
        character(len=4096) :: path
        integer :: event_type

        matched = .false.
        actual_path = ''
        actual_type = 0

        do i = 1, max_tries
            call watcher_poll(w, path, event_type, timeout_ms, got_event)
            if (.not. got_event) cycle
            if (trim(path) == trim(expected_path) .and. &
                event_type == expected_type) then
                matched = .true.
                actual_path = path
                actual_type = event_type
                return
            end if
        end do
    end subroutine poll_until_match

end program test_watch
