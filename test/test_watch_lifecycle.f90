program test_watch_lifecycle
    use, intrinsic :: iso_c_binding, only: c_int
    use fx_watch, only: watcher_t, watcher_init, watcher_add, watcher_remove, &
        watcher_poll, watcher_close
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, test_suite_exit
    implicit none
    interface
        integer(c_int) function descriptor_count_c() &
                bind(C, name='fx_c_watch_test_descriptor_count')
            import :: c_int
        end function descriptor_count_c
        integer(c_int) function fail_registration_after(count) &
                bind(C, name='fx_c_watch_test_fail_registration_after')
            import :: c_int
            integer(c_int), value :: count
        end function fail_registration_after
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
    write (root, '(a,i0)') '/var/tmp/fx-watch-lifecycle-', pid
    call test_suite_init(suite, 'watch lifecycle')
    call test_repeated_add()
    call test_replaced_tree()
    call test_file_directory_replacement()
    call test_registration_failure()
    call test_poll_outcomes()
    call command('rm -rf -- '//trim(root))
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    integer function descriptor_count() result(count)
        count = descriptor_count_c()
        if (count < 0) error stop 'cannot enumerate process descriptors'
    end function descriptor_count

    subroutine command(text)
        character(len=*), intent(in) :: text
        integer :: status

        call execute_command_line(text, exitstat=status)
        if (status /= 0) error stop 'fixture command failed'
    end subroutine command

    subroutine write_input()
        integer :: unit

        open (newunit=unit, file=trim(root)//'/input.f90', status='replace')
        write (unit, '(a)') 'module input'
        close (unit)
    end subroutine write_input

    subroutine test_repeated_add()
        type(watcher_t) :: watch
        integer :: before, subscribed, ierr, i

        call command('mkdir -p -- '//trim(root))
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
            call command('mv -- '//trim(root)//' '//trim(root)//'-retired')
            call command('mkdir -p -- '//trim(root))
            call write_input()
            call watcher_add(watch, trim(root), .true., ierr)
            call test_assert_equal_int(suite, 0, ierr, 'replacement refresh succeeds')
            call test_assert_equal_int(suite, subscribed, descriptor_count(), &
                'replacement retires the superseded descriptors')
            call command('rm -rf -- '//trim(root)//'-retired')
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
        call command('rm -- '//trim(root)//'/input.f90')
        call command('mkdir -- '//trim(root)//'/input.f90')
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
        call command('rm -rf -- '//trim(root)//'/input.f90')
        call write_input()
        do i = 1, 32
            call watcher_poll(watch, changed, kind, 0, got_event, ierr)
            if (.not. got_event) exit
        end do
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'file-directory-file replacement restores descriptor baseline')
    end subroutine test_file_directory_replacement

    subroutine test_registration_failure()
        type(watcher_t) :: watch
        character(len=4096) :: changed
        integer :: before, initialized, subscribed, ierr, kind, ignored, unit
        logical :: got_event

        if (fail_registration_after(-1) == 0) return
        before = descriptor_count()
        call watcher_init(watch, ierr)
        initialized = descriptor_count()
        ! Directory registration succeeds, then the first child fails.
        ignored = fail_registration_after(1)
        call watcher_add(watch, trim(root), .true., ierr)
        call test_assert(suite, ierr /= 0, 'initial child registration failure propagates')
        call test_assert_equal_int(suite, initialized, descriptor_count(), &
            'failed initial admission closes directory and child descriptors')
        call watcher_add(watch, trim(root), .true., ierr)
        subscribed = descriptor_count()
        open (newunit=unit, file=trim(root)//'-replacement', status='replace')
        write (unit, '(a)') 'atomic replacement'
        close (unit)
        call command('mv -- '//trim(root)//'-replacement '//trim(root)//'/input.f90')
        ignored = fail_registration_after(0)
        call watcher_poll(watch, changed, kind, 1000, got_event, ierr)
        call test_assert(suite, ierr /= 0, 'existing child registration failure propagates')
        call test_assert_equal_int(suite, subscribed - 1, descriptor_count(), &
            'failed refresh closes and resets the replaced child descriptor')
        call watcher_add(watch, trim(root), .true., ierr)
        call test_assert_equal_int(suite, 0, ierr, 'explicit refresh retries failed child')
        call test_assert_equal_int(suite, subscribed, descriptor_count(), &
            'successful retry restores the bounded subscription descriptors')
        call write_input()
        call watcher_poll(watch, changed, kind, 1000, got_event, ierr)
        call test_assert(suite, got_event, 'retry restores real event delivery')
        call test_assert_equal_str(suite, trim(root)//'/input.f90', trim(changed), &
            'retried registration observes the replacement file')
        call watcher_close(watch)
        call test_assert_equal_int(suite, before, descriptor_count(), &
            'registration failures and retries restore baseline after close')
    end subroutine test_registration_failure

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
