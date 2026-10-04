program test_watch_lifecycle
    use, intrinsic :: iso_c_binding, only: c_int
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, fx_test_rename, &
        fx_test_descriptor_count
    use fx_watch, only: watcher_t, watcher_init, watcher_add, watcher_remove, &
        watcher_poll, watcher_close
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, test_suite_exit
    implicit none
    interface
        integer(c_int) function close_descriptor(fd) bind(C, name='close')
            import :: c_int
            integer(c_int), value :: fd
        end function close_descriptor
        integer(c_int) function getpid() bind(C, name='getpid')
            import :: c_int
        end function getpid
    end interface
    type(test_suite_t) :: suite
    character(len=256) :: root
    integer :: pid

    pid = getpid()
    write (root, '(a,i0)') '/var/tmp/fx watch;$(fixture)-lifecycle-', pid
    call test_suite_init(suite, 'watch lifecycle')
    call test_repeated_add()
    call test_repeated_start_stop()
    call test_replaced_tree()
    call test_file_directory_replacement()
    call test_registration_error()
    call test_poll_outcomes()
    call remove_tree(trim(root))
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    integer function descriptor_count() result(count)
        count = fx_test_descriptor_count()
        if (count < 0) error stop 'cannot enumerate process descriptors'
    end function descriptor_count

    subroutine make_dir(path)
        character(len=*), intent(in) :: path
        integer :: ierr

        ierr = fx_test_mkdir_p(path)
        if (ierr /= 0) error stop 'fixture directory creation failed'
    end subroutine make_dir

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path
        integer :: ierr

        ierr = fx_test_remove_tree(path)
        if (ierr /= 0) error stop 'fixture tree removal failed'
    end subroutine remove_tree

    subroutine rename_path(source, destination)
        character(len=*), intent(in) :: source, destination
        integer :: ierr

        ierr = fx_test_rename(source, destination)
        if (ierr /= 0) error stop 'fixture rename failed'
    end subroutine rename_path

    subroutine write_input()
        integer :: unit

        open (newunit=unit, file=trim(root)//'/input.f90', status='replace')
        write (unit, '(a)') 'module input'
        close (unit)
    end subroutine write_input

    subroutine test_repeated_add()
        type(watcher_t) :: watch
        integer :: before, subscribed, ierr, i

        call make_dir(trim(root))
        call write_input()
        before = descriptor_count()
        call watcher_init(watch, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'initialize watcher')
        call watcher_add(watch, trim(root), .true., ierr)
        subscribed = descriptor_count()
        do i = 1, 32
            call watcher_add(watch, trim(root), .true., ierr)
            call test_assert_equal_int(suite, 0, ierr, 'repeated add succeeds')
        end do
        call test_assert_equal_int(suite, subscribed, descriptor_count(), &
            'repeated add retains a bounded descriptor set')
        call watcher_remove(watch, trim(root), ierr)
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'remove and close restore the independent OS descriptor baseline')
    end subroutine test_repeated_add

    subroutine test_repeated_start_stop()
        type(watcher_t) :: watch
        integer :: before, ierr, i

        before = descriptor_count()
        do i = 1, 32
            call watcher_init(watch, ierr)
            call test_assert_equal_int(suite, 0, ierr, 'repeated start succeeds')
            call watcher_add(watch, trim(root), .true., ierr)
            call test_assert_equal_int(suite, 0, ierr, 'repeated subscription succeeds')
            call watcher_close(watch)
            call test_assert_equal_int(suite, before, descriptor_count(), &
                'repeated start and stop returns to descriptor baseline')
        end do
    end subroutine test_repeated_start_stop

    subroutine test_replaced_tree()
        type(watcher_t) :: watch
        character(len=4096) :: changed
        integer :: before, subscribed, ierr, i, kind
        logical :: got_event

        before = descriptor_count()
        call watcher_init(watch, ierr)
        call watcher_add(watch, trim(root), .true., ierr)
        subscribed = descriptor_count()
        do i = 1, 16
            call rename_path(trim(root), trim(root)//'-retired')
            call make_dir(trim(root))
            call write_input()
            call watcher_add(watch, trim(root), .true., ierr)
            call test_assert_equal_int(suite, 0, ierr, 'replacement refresh succeeds')
            call test_assert_equal_int(suite, subscribed, descriptor_count(), &
                'replacement retires the superseded descriptors')
            call remove_tree(trim(root)//'-retired')
        end do
        call write_input()
        do i = 1, 64
            call watcher_poll(watch, changed, kind, 20, got_event, ierr)
            if (got_event .and. trim(changed) == trim(root)//'/input.f90') exit
        end do
        call test_assert_equal_int(suite, 0, ierr, 'replacement watcher poll succeeds')
        call test_assert(suite, got_event, 'replacement source edit is delivered')
        call test_assert_equal_str(suite, trim(root)//'/input.f90', trim(changed), &
            'replacement source event belongs to the current file')
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'replacement cleanup restores the descriptor baseline')
    end subroutine test_replaced_tree

    subroutine test_file_directory_replacement()
        type(watcher_t) :: watch
        character(len=4096) :: changed
        integer :: before, ierr, kind, unit, i
        logical :: got_event, matched

        before = descriptor_count()
        call watcher_init(watch, ierr)
        call watcher_add(watch, trim(root), .true., ierr)
        call remove_tree(trim(root)//'/input.f90')
        call make_dir(trim(root)//'/input.f90')
        open (newunit=unit, file=trim(root)//'/input.f90/child', status='replace')
        write (unit, '(a)') 'before subscription'
        close (unit)
        do i = 1, 32
            call watcher_poll(watch, changed, kind, 0, got_event, ierr)
            if (.not. got_event) exit
        end do
        open (newunit=unit, file=trim(root)//'/input.f90/child', status='replace')
        write (unit, '(a)') 'after subscription'
        close (unit)
        matched = .false.
        do i = 1, 32
            call watcher_poll(watch, changed, kind, 20, got_event, ierr)
            if (trim(changed) == trim(root)//'/input.f90/child') matched = .true.
            if (matched) exit
        end do
        call test_assert(suite, matched, 'file to directory replacement observes nested edit')
        call remove_tree(trim(root)//'/input.f90')
        call write_input()
        do i = 1, 32
            call watcher_poll(watch, changed, kind, 0, got_event, ierr)
            if (.not. got_event) exit
        end do
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'file-directory-file replacement restores descriptor baseline')
    end subroutine test_file_directory_replacement

    subroutine test_registration_error()
        type(watcher_t) :: watch
        character(len=4096) :: changed
        character(len=320) :: missing_root
        integer :: before, initialized, subscribed, ierr, kind, i
        logical :: got_event

        write (missing_root, '(a,i0)') trim(root)//'-missing-', pid
        before = descriptor_count()
        call watcher_init(watch, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'initialize watcher before invalid root')
        initialized = descriptor_count()
        call watcher_add(watch, trim(missing_root), .true., ierr)
        call test_assert(suite, ierr /= 0, 'missing root registration error propagates')
        call test_assert_equal_int(suite, initialized, descriptor_count(), &
            'missing root admission retains no resource')
        call watcher_add(watch, trim(root), .true., ierr)
        subscribed = descriptor_count()
        call write_input()
        got_event = .false.
        do i = 1, 32
            call watcher_poll(watch, changed, kind, 20, got_event, ierr)
            if (got_event) then
                if (trim(changed) == trim(root)//'/input.f90') exit
            end if
        end do
        call test_assert_equal_int(suite, 0, ierr, 'watcher remains usable after root error')
        call test_assert(suite, got_event, 'valid root delivers event after registration error')
        call test_assert_equal_str(suite, trim(root)//'/input.f90', trim(changed), &
            'recovered watcher reports the expected source')
        call test_assert_equal_int(suite, subscribed, descriptor_count(), &
            'recovered watcher retains a bounded descriptor set')
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'registration error and subsequent close restore descriptor baseline')
    end subroutine test_registration_error

    subroutine test_poll_outcomes()
        type(watcher_t) :: watch
        character(len=4096) :: changed
        integer :: ierr, kind, ignored, i
        logical :: got_event

        call watcher_init(watch, ierr)
        call watcher_add(watch, trim(root), .true., ierr)
        call watcher_poll(watch, changed, kind, 20, got_event, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'timeout is not an error')
        call test_assert(suite, .not. got_event, 'timeout has no event')
        call write_input()
        call watcher_poll(watch, changed, kind, 1000, got_event, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'real event is not an error')
        call test_assert(suite, got_event, 'real event follows idle timeout')
        do i = 1, 16
            call watcher_poll(watch, changed, kind, 0, got_event, ierr)
            if (.not. got_event) exit
        end do
        ignored = close_descriptor(watch%fd)
        call test_assert_equal_int(suite, 0, ignored, 'induce backend descriptor failure')
        call watcher_poll(watch, changed, kind, 0, got_event, ierr)
        call test_assert(suite, ierr /= 0, 'public watcher API propagates backend failure')
        call test_assert(suite, .not. got_event, 'backend failure has no event')
        call watcher_close(watch)
    end subroutine test_poll_outcomes
end program test_watch_lifecycle
