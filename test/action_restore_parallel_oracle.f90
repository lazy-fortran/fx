program action_restore_parallel_oracle
    use, intrinsic :: iso_c_binding, only: c_int
    use fx_action_cache, only: cache_t, cache_store_action, &
        cache_restore_action
    use fx_cache, only: cache_init
    implicit none

    interface
        function c_getpid() bind(c, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid
    end interface

    integer, parameter :: NTHREADS = 8, NITER = 80
    character(len=256) :: root
    type(cache_t) :: writer
    character(len=512) :: source_dir, object_path, module_path
    character(len=512) :: output_id
    integer :: ierr, thread, iteration, failures
    logical :: restored

    write (root, '(A,I0)') '/var/tmp/fx_action_restore_parallel_oracle-', &
        c_getpid()
    call execute_command_line('rm -rf '//trim(root)//'; mkdir -p '// &
        trim(root)//'/src')
    source_dir = trim(root)//'/src'
    object_path = trim(source_dir)//'/unit.o'
    module_path = trim(source_dir)//'/unit.mod'
    call write_file(trim(object_path), 'oracle object bytes')
    call write_file(trim(module_path), 'oracle module bytes')
    call cache_init(writer, trim(root)//'/cache')
    call cache_store_action(writer, 'parallel-restore', trim(object_path), &
        trim(source_dir), 'unit', output_id, ierr)
    if (ierr /= 0) error stop 'could not seed action result'

    failures = 0
    do thread = 1, NTHREADS
        do iteration = 1, NITER
            call make_destination(thread, iteration)
        end do
    end do
    !$omp parallel do num_threads(NTHREADS) private(thread, iteration, restored) &
    !$omp reduction(+:failures) schedule(dynamic)
    do thread = 1, NTHREADS
        do iteration = 1, NITER
            call restore_once(thread, iteration, restored)
            if (.not. restored) failures = failures + 1
        end do
    end do
    !$omp end parallel do
    if (failures /= 0) then
        write (*, '(A,I0)') 'parallel restore failures: ', failures
        error stop 1
    end if
    call execute_command_line('rm -rf '//trim(root))
    print '(A,I0,A)', 'parallel restore passed: ', NTHREADS*NITER, ' restores'

contains

    subroutine restore_once(thread, iteration, ok)
        integer, intent(in) :: thread, iteration
        logical, intent(out) :: ok
        type(cache_t) :: reader
        character(len=512) :: destination
        character(len=32) :: t, n

        write (t, '(I0)') thread
        write (n, '(I0)') iteration
        destination = trim(root)//'/dst-'//trim(t)//'-'//trim(n)
        call cache_init(reader, trim(root)//'/cache')
        call cache_restore_action(reader, 'parallel-restore', &
            trim(destination)//'/unit.o', trim(destination), ok)
        if (ok) then
            ok = file_equals(trim(destination)//'/unit.o', 'oracle object bytes')
            ok = ok .and. file_equals(trim(destination)//'/unit.mod', &
                'oracle module bytes')
        end if
    end subroutine restore_once

    subroutine make_destination(thread, iteration)
        integer, intent(in) :: thread, iteration
        character(len=32) :: t, n

        write (t, '(I0)') thread
        write (n, '(I0)') iteration
        call execute_command_line('mkdir -p '//trim(root)//'/dst-'//trim(t)// &
            '-'//trim(n))
    end subroutine make_destination

    function file_equals(path, expected) result(matches)
        character(len=*), intent(in) :: path, expected
        logical :: matches
        character(len=128) :: contents
        integer :: unit, status

        matches = .false.
        open (newunit=unit, file=path, status='old', action='read', &
            iostat=status)
        if (status /= 0) return
        read (unit, '(A)', iostat=status) contents
        close (unit)
        if (status /= 0) return
        matches = trim(contents) == expected
    end function file_equals

    subroutine write_file(path, contents)
        character(len=*), intent(in) :: path, contents
        integer :: unit

        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(A)') contents
        close (unit)
    end subroutine write_file

end program action_restore_parallel_oracle
