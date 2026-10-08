program action_restore_parallel_oracle
    use, intrinsic :: iso_c_binding, only: c_int
    use fx_action_cache, only: cache_t, cache_store_action, &
        cache_restore_action
    use fx_cache, only: cache_init
    use fx_cache_fs, only: cache_ensure_dir
    implicit none

    interface
        function c_getpid() bind(c, name='getpid') result(pid)
            import :: c_int
            integer(c_int) :: pid
        end function c_getpid
    end interface

    integer, parameter :: NTHREADS = 8, NITER = 80
    character(len=512) :: root, temporary_root
    character(len=32) :: pid_text, action_ids(NTHREADS)
    character(len=128) :: module_names(NTHREADS), object_names(NTHREADS)
    character(len=256) :: object_bytes(NTHREADS), module_bytes(NTHREADS)
    type(cache_t) :: writer
    character(len=512) :: source_dir, object_path, module_path
    character(len=512) :: output_id
    integer :: ierr, thread, iteration, failures
    logical :: restored

    temporary_root = ''
    call get_environment_variable('TMPDIR', temporary_root, status=ierr)
    if (ierr /= 0 .or. len_trim(temporary_root) == 0) temporary_root = '/var/tmp'
    write (pid_text, '(I0)') c_getpid()
    root = trim(temporary_root)//'/fx_action_restore_parallel_oracle-'// &
        trim(pid_text)
    call cache_ensure_dir(trim(root)//'/src', ierr)
    if (ierr /= 0) error stop 'could not create oracle source directory'
    source_dir = trim(root)//'/src'
    call cache_init(writer, trim(root)//'/cache')
    do thread = 1, NTHREADS
        write (action_ids(thread), '(A,I0)') 'parallel-restore-', thread
        ! Different payload and path lengths must never borrow another lane's
        ! deferred-character length or restore another action's bytes.
        object_names(thread) = 'object_'//repeat('o', 9*thread)//'.o'
        module_names(thread) = 'module_'//repeat('m', 7*thread)
        object_bytes(thread) = 'object '//trim(action_ids(thread))// &
            repeat('x', 11*thread)
        module_bytes(thread) = 'module '//trim(action_ids(thread))// &
            repeat('y', 13*thread)
        object_path = trim(source_dir)//'/'//trim(object_names(thread))
        module_path = trim(source_dir)//'/'//trim(module_names(thread))//'.mod'
        call write_file(trim(object_path), trim(object_bytes(thread)))
        call write_file(trim(module_path), trim(module_bytes(thread)))
        call cache_store_action(writer, trim(action_ids(thread)), &
            trim(object_path), trim(source_dir), [trim(module_names(thread))], &
            output_id, ierr)
        if (ierr /= 0) error stop 'could not seed action result'
    end do

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
    call execute_command_line('rm -rf "'//trim(root)//'"')
    print '(A,I0,A)', 'parallel restore passed: ', NTHREADS*NITER, ' cold/warm pairs'

contains

    subroutine restore_once(thread, iteration, ok)
        integer, intent(in) :: thread, iteration
        logical, intent(out) :: ok
        type(cache_t) :: reader
        character(len=512) :: destination, object, module
        logical :: warm

        call destination_path(thread, iteration, destination)
        object = trim(destination)//'/'//trim(object_names(thread))
        module = trim(destination)//'/'//trim(module_names(thread))//'.mod'
        call cache_init(reader, trim(root)//'/cache')
        call cache_restore_action(reader, trim(action_ids(thread)), &
            trim(object), trim(destination), ok)
        if (.not. ok) return
        ok = file_equals(trim(object), trim(object_bytes(thread)))
        if (.not. ok) return
        ok = file_equals(trim(module), trim(module_bytes(thread)))
        if (.not. ok) return
        call cache_restore_action(reader, trim(action_ids(thread)), &
            trim(object), trim(destination), warm)
        if (.not. warm) then
            ok = .false.
            return
        end if
        ok = file_equals(trim(object), trim(object_bytes(thread)))
        if (.not. ok) return
        ok = file_equals(trim(module), trim(module_bytes(thread)))
    end subroutine restore_once

    subroutine destination_path(thread, iteration, destination)
        integer, intent(in) :: thread, iteration
        character(len=*), intent(out) :: destination
        character(len=32) :: t, n

        write (t, '(I0)') thread
        write (n, '(I0)') iteration
        destination = trim(root)//'/dst-'//trim(t)//'-'// &
            repeat('d', 13*thread)//'-'//trim(n)
    end subroutine destination_path

    subroutine make_destination(thread, iteration)
        integer, intent(in) :: thread, iteration
        character(len=512) :: destination
        integer :: status

        call destination_path(thread, iteration, destination)
        call cache_ensure_dir(trim(destination), status)
        if (status /= 0) error stop 'could not create oracle destination'
    end subroutine make_destination

    function file_equals(path, expected) result(matches)
        character(len=*), intent(in) :: path, expected
        logical :: matches
        character(len=256) :: contents
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
