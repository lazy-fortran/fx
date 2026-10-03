program test_immutable_publication
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_loc, &
        c_null_char, c_null_ptr
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_path, only: path_dirname
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        immutable_store_init, immutable_store_put_blob, immutable_store_blob_path, &
        immutable_store_tree_path, IMMUTABLE_OK, IMMUTABLE_BLOB
    use fx_immutable_tree, only: immutable_store_put_tree
    use fx_immutable_manifest, only: immutable_manifest_serialize
    implicit none
    interface
        integer(c_int) function tmp_root(out, cap) &
                bind(C, name='fx_immutable_test_tmp_root')
            import :: c_int, c_char
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function tmp_root
        subroutine configure(phase, ready, release) &
                bind(C, name='fx_immutable_owned_test_configure')
            import :: c_int, c_char
            integer(c_int), value :: phase
            character(kind=c_char), intent(in) :: ready(*), release(*)
        end subroutine configure
        integer(c_int) function rename_path(old, new) bind(C, name='rename')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: old(*), new(*)
        end function rename_path
        integer(c_int) function symlink_path(target, link) bind(C, name='symlink')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: target(*), link(*)
        end function symlink_path
        integer(c_int) function path_mode(path, mode) bind(C, name='fx_immutable_path_mode')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), intent(out) :: mode
        end function path_mode
        integer(c_int) function chmod_path(path, mode) &
                bind(C, name='fx_immutable_chmod_sync')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function chmod_path
        integer(c_int) function fork_process() bind(C, name='fork')
            import :: c_int
        end function fork_process
        integer(c_int) function exec_child(path, arguments) bind(C, name='execv')
            import :: c_int, c_char, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), intent(in) :: arguments(*)
        end function exec_child
        subroutine exit_child(status) bind(C, name='_exit')
            import :: c_int
            integer(c_int), value :: status
        end subroutine exit_child
        integer(c_int) function wait_child(pid, status, options) bind(C, name='waitpid')
            import :: c_int
            integer(c_int), value :: pid, options
            integer(c_int), intent(out) :: status
        end function wait_child
        integer(c_int) function kill_child(pid, signal) bind(C, name='kill')
            import :: c_int
            integer(c_int), value :: pid, signal
        end function kill_child
        integer(c_int) function sleep_us(time) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: time
        end function sleep_us
    end interface
    character(len=*), parameter :: PAYLOAD = 'captured publication payload'
    type(test_suite_t) :: suite
    character(len=512) :: root, argument
    character(len=512, kind=c_char) :: scratch
    integer(c_int) :: status
    integer :: kind, phase, attack, trial, end_path

    call get_command_argument(1, argument)
    if (trim(argument) == '--publish-worker') call run_worker()
    call test_suite_init(suite, 'immutable_publication')
    scratch = c_null_char
    status = tmp_root(scratch, 512_c_int)
    call test_assert_equal_int(suite, 0, int(status), 'physical system scratch resolves')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-publish42-', proc_pid()
    trial = 0
    do kind = 1, 2
        do phase = 5, 7
            do attack = 1, 2
                if (phase == 7 .and. attack == 2) cycle
                trial = trial + 1
                call test_capture(kind, phase, attack, trial)
            end do
        end do
    end do
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    function number(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=20) :: buffer
        write (buffer, '(i0)') value
        text = trim(buffer)
    end function number

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, ios, closed
        open (newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
        if (ios /= 0) stop 21
        write (unit, iostat=ios) text
        close (unit, iostat=closed)
        if (ios /= 0 .or. closed /= 0) stop 22
    end subroutine write_text

    subroutine poison_existing(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        open (newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='write', iostat=ios)
        if (ios /= 0) stop 26
        write (unit, iostat=ios) 'CORRUPTED AFTER PUBLICATION'
        close (unit)
        if (ios /= 0) stop 27
    end subroutine poison_existing

    subroutine read_text(path, text, ios)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ios
        integer :: unit, count
        text = ''
        count = -1
        inquire (file=path, size=count, iostat=ios)
        if (ios /= 0 .or. count < 0) return
        text = repeat(' ', count)
        open (newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=ios)
        if (ios /= 0) return
        read (unit, iostat=ios) text
        close (unit)
    end subroutine read_text

    subroutine fixture(base, kind, store, id, expected)
        character(len=*), intent(in) :: base
        integer, intent(in) :: kind
        type(immutable_store_t), intent(out) :: store
        character(len=64), intent(out) :: id
        character(len=:), allocatable, intent(out) :: expected
        type(immutable_tree_entry_t) :: entry(1)
        integer :: ierr
        call immutable_store_init(store, base//'/store', ierr)
        if (ierr /= IMMUTABLE_OK) stop 23
        call write_text(base//'/source', PAYLOAD)
        expected = PAYLOAD
        id = sha256_string(expected)
        if (kind == 1) return
        call immutable_store_put_blob(store, base//'/source', id, ierr)
        if (ierr /= IMMUTABLE_OK) stop 24
        entry(1) = tree_entry(id)
        expected = immutable_manifest_serialize(entry)
        id = sha256_string(expected)
    end subroutine fixture

    function tree_entry(blob) result(entry)
        character(len=*), intent(in) :: blob
        type(immutable_tree_entry_t) :: entry
        entry%path = 'payload.bin'
        entry%role = 'source'
        entry%object_id = blob
        entry%kind = IMMUTABLE_BLOB
        entry%mode = 420
    end function tree_entry

    subroutine test_capture(kind, phase, attack, trial)
        integer, intent(in) :: kind, phase, attack, trial
        type(immutable_store_t) :: store
        character(len=64) :: id
        character(len=:), allocatable :: base, expected, observed, final, temp
        integer(c_int) :: pid, child_status, actual_mode, checked
        integer :: result, ios
        logical :: found, exists
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        call write_text(base//'/sentinel', 'OUTSIDE SENTINEL')
        final = immutable_store_blob_path(store, id)
        if (kind == 2) final = immutable_store_tree_path(store, id)
        pid = spawn_worker(base, kind, phase)
        call test_assert(suite, pid > 0, 'independent publication worker starts')
        if (pid <= 0) return
        call wait_marker(base//'/ready', found)
        call test_assert(suite, found, 'worker reaches exact capture/publication boundary')
        temp = ''
        if (found) then
            call read_text(base//'/ready', temp, ios)
            call test_assert_equal_int(suite, 0, ios, 'boundary records owned temporary')
            ios = index(temp, achar(10))
            if (ios > 0) temp = temp(:ios - 1)
            temp = trim(temp)
            call replace_entry(base, temp, phase, attack)
        end if
        call write_text(base//'/release', 'release')
        call wait_worker(pid, child_status)
        call test_assert_equal_int(suite, 0, int(child_status), 'publication worker exits normally')
        call read_text(base//'/result', observed, ios)
        result = IMMUTABLE_OK
        if (ios == 0) read (observed, *, iostat=ios) result
        call test_assert_equal_int(suite, 0, ios, 'worker reports real API status')
        call test_assert(suite, result /= IMMUTABLE_OK, 'substitution/corruption never reports success')
        inquire (file=final, exist=exists)
        call test_assert(suite, .not. exists, 'arbitrary bytes are absent under requested object ID')
        call read_text(base//'/sentinel', observed, ios)
        call test_assert(suite, ios == 0 .and. observed == 'OUTSIDE SENTINEL', &
            'outside sentinel bytes survive capture and publication')
        actual_mode = -1_c_int
        checked = path_mode((base//'/sentinel')//c_null_char, actual_mode)
        call test_assert(suite, checked == 0 .and. actual_mode == 420, &
            'outside sentinel permissions survive')
        if (found) then
            inquire (file=path_dirname(temp), exist=exists)
            call test_assert(suite, .not. exists, 'owned temporary root and all entries are cleaned')
        end if
    end subroutine test_capture

    subroutine replace_entry(base, temp, phase, attack)
        character(len=*), intent(in) :: base, temp
        integer, intent(in) :: phase, attack
        integer(c_int) :: renamed, linked
        if (phase == 7) then
            linked = chmod_path(temp//c_null_char, 420_c_int)
            call test_assert_equal_int(suite, 0, int(linked), 'published captured inode becomes writable')
            call poison_existing(temp)
            return
        end if
        renamed = rename_path(temp//c_null_char, (temp//'.held')//c_null_char)
        call test_assert_equal_int(suite, 0, int(renamed), 'captured entry is renamed before release')
        if (attack == 1) then
            linked = symlink_path((base//'/sentinel')//c_null_char, temp//c_null_char)
            call test_assert_equal_int(suite, 0, int(linked), 'captured entry substitutes outside symlink')
        else
            call write_text(temp, 'ARBITRARY UNVERIFIED BYTES')
        end if
    end subroutine replace_entry

    function spawn_worker(base, kind, phase) result(pid)
        character(len=*), intent(in) :: base
        integer, intent(in) :: kind, phase
        integer(c_int) :: pid, exec_status
        character(kind=c_char), target :: args(1024, 5)
        type(c_ptr) :: argv(6)
        character(len=1024) :: executable, text(5)
        integer :: i, j
        call get_command_argument(0, executable)
        text = [character(len=1024) :: trim(executable), '--publish-worker', &
            base, number(kind), number(phase)]
        args = c_null_char
        do i = 1, 5
            do j = 1, len_trim(text(i))
                args(j, i) = text(i)(j:j)
            end do
            argv(i) = c_loc(args(1, i))
        end do
        argv(6) = c_null_ptr
        pid = fork_process()
        if (pid == 0) then
            exec_status = exec_child(args(:, 1), argv)
            call exit_child(127_c_int)
        end if
    end function spawn_worker

    subroutine run_worker()
        type(immutable_store_t) :: store
        type(immutable_tree_entry_t) :: entry(1)
        character(len=512) :: base, argument
        character(len=64) :: id
        integer :: kind, phase, ierr, unit
        call get_command_argument(2, base)
        call get_command_argument(3, argument)
        read (argument, *) kind
        call get_command_argument(4, argument)
        read (argument, *) phase
        call immutable_store_init(store, trim(base)//'/store', ierr)
        if (ierr /= IMMUTABLE_OK) call exit_child(25_c_int)
        call configure(int(phase, c_int), (trim(base)//'/ready')//c_null_char, &
            (trim(base)//'/release')//c_null_char)
        if (kind == 1) then
            call immutable_store_put_blob(store, trim(base)//'/source', id, ierr)
        else
            entry(1) = tree_entry(sha256_string(PAYLOAD))
            call immutable_store_put_tree(store, entry, id, ierr)
        end if
        open (newunit=unit, file=trim(base)//'/result', status='replace')
        write (unit, *) ierr
        close (unit)
        call exit_child(0_c_int)
    end subroutine run_worker

    subroutine wait_marker(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer :: i
        integer(c_int) :: waited
        found = .false.
        do i = 1, 1000
            inquire (file=path, exist=found)
            if (found) return
            waited = sleep_us(10000_c_int)
        end do
    end subroutine wait_marker

    subroutine wait_worker(pid, status)
        integer(c_int), intent(in) :: pid
        integer(c_int), intent(out) :: status
        integer :: i
        integer(c_int) :: waited, reaped
        do i = 1, 1000
            reaped = wait_child(pid, status, 1_c_int)
            if (reaped == pid) return
            if (reaped < 0) exit
            waited = sleep_us(10000_c_int)
        end do
        waited = kill_child(pid, 9_c_int)
        waited = wait_child(pid, status, 0_c_int)
        status = 999_c_int
    end subroutine wait_worker
end program test_immutable_publication
