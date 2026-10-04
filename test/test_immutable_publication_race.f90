program test_immutable_publication_race
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_loc, c_long_long, &
        c_null_char, c_null_ptr
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_path, only: path_dirname
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        immutable_store_init, immutable_store_put_blob, immutable_store_blob_path, &
        immutable_store_tree_path, IMMUTABLE_OK, IMMUTABLE_BLOB, IMMUTABLE_UNSUPPORTED
    use fx_immutable_tree, only: immutable_store_put_tree
    use fx_immutable_manifest, only: immutable_manifest_serialize
    implicit none
    interface
        subroutine chain(first, second, ready, release, after_ready, after_release) &
                bind(C, name='fx_immutable_owned_test_chain')
            import :: c_int, c_char
            integer(c_int), value :: first, second
            character(kind=c_char), intent(in) :: ready(*), release(*)
            character(kind=c_char), intent(in) :: after_ready(*), after_release(*)
        end subroutine chain
        subroutine no_atomic_clone(forced) bind(C, name='fx_owned_test_no_atomic_clone')
            import :: c_int
            integer(c_int), value :: forced
        end subroutine no_atomic_clone
        integer(c_int) function is_apfs(path) bind(C, name='fx_immutable_is_apfs')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function is_apfs
        integer(c_int) function file_info(path, bytes, mtime, inode) &
                bind(C, name='fx_immutable_file_info')
            import :: c_int, c_char, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: bytes, mtime, inode
        end function file_info
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
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-publish42-late-', proc_pid()
    trial = 0
    do kind = 1, 2
        do attack = 1, 2
            trial = trial + 1
            call test_after_check(kind, attack, trial)
        end do
        trial = trial + 1
        call test_cleanup_winner(kind, trial)
        if (is_apfs(trim(root)//c_null_char) == 1_c_int) then
            trial = trial + 1
            call test_atomic_unavailable(kind, trial)
        end if
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

    subroutine test_after_check(kind, attack, trial)
        integer, intent(in) :: kind, attack, trial
        type(immutable_store_t) :: store
        character(len=64) :: id
        character(len=:), allocatable :: base, expected, temp, final
        integer(c_int) :: pid, changed
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        call write_text(base//'/sentinel', 'OUTSIDE SENTINEL')
        final = canonical(store, kind, id)
        pid = spawn_worker(base, kind, 8)
        call test_assert(suite, pid > 0, 'late publication worker starts')
        if (pid <= 0) return
        call boundary(base//'/ready', temp)
        changed = rename_path(temp//c_null_char, (temp//'.held')//c_null_char)
        call test_assert_equal_int(suite, 0, int(changed), 'entry moves after final ownership check')
        if (attack == 1) then
            changed = symlink_path((base//'/sentinel')//c_null_char, temp//c_null_char)
            call test_assert_equal_int(suite, 0, int(changed), 'late entry becomes outside symlink')
        else
            call write_text(temp, 'ARBITRARY SUBSTITUTED BYTES')
        end if
        call write_text(base//'/release', 'release')
        call result_after_wait(pid, base, IMMUTABLE_OK)
        call check_bytes(final, expected, 'descriptor publication preserves original bytes')
        call check_outside_and_temps(base, temp)
        call retry(store, base, kind, id)
        call check_bytes(final, expected, 'retry keeps the correct canonical object')
    end subroutine test_after_check

    subroutine test_cleanup_winner(kind, trial)
        integer, intent(in) :: kind, trial
        type(immutable_store_t) :: store
        character(len=64) :: id
        character(len=:), allocatable :: base, expected, temp, final, marker
        integer(c_int) :: pid, changed
        integer(c_long_long) :: winner_inode
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        call write_text(base//'/sentinel', 'OUTSIDE SENTINEL')
        final = canonical(store, kind, id)
        pid = spawn_worker(base, kind, 9)
        call test_assert(suite, pid > 0, 'rejected publication worker starts')
        if (pid <= 0) return
        call boundary(base//'/ready', temp)
        changed = chmod_path(final//c_null_char, 420_c_int)
        call test_assert_equal_int(suite, 0, int(changed), 'actual published inode can be poisoned')
        call poison_existing(final)
        call write_text(base//'/release', 'release')
        call boundary(base//'/cleanup-ready', marker)
        changed = rename_path(final//c_null_char, (base//'/rejected-object')//c_null_char)
        call test_assert_equal_int(suite, 0, int(changed), 'rejected inode leaves canonical slot')
        call retry(store, base, kind, id)
        winner_inode = inode_of(final)
        call test_assert(suite, winner_inode > 0, 'concurrent valid winner has real inode')
        call write_text(base//'/cleanup-release', 'release')
        call result_after_wait(pid, base, -1)
        call test_assert(suite, inode_of(final) == winner_inode, &
            'rejection cleanup preserves the concurrent winner inode')
        call check_bytes(final, expected, 'concurrent winner retains actual valid bytes')
        call check_outside_and_temps(base, temp)
        call retry(store, base, kind, id)
    end subroutine test_cleanup_winner

    subroutine test_atomic_unavailable(kind, trial)
        integer, intent(in) :: kind, trial
        type(immutable_store_t) :: store
        character(len=64) :: id
        character(len=:), allocatable :: base, expected, final
        integer(c_int) :: pid
        logical :: exists
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        final = canonical(store, kind, id)
        pid = spawn_worker(base, kind, 10)
        if (pid <= 0) stop 29
        call result_after_wait(pid, base, IMMUTABLE_UNSUPPORTED)
        inquire (file=final, exist=exists)
        call test_assert(suite, .not. exists, 'no pathname fallback when atomic clone is unavailable')
        call retry(store, base, kind, id)
        call check_bytes(final, expected, 'retry after atomic clone failure publishes correct bytes')
    end subroutine test_atomic_unavailable

    function canonical(store, kind, id) result(path)
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: kind
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: path
        path = immutable_store_blob_path(store, id)
        if (kind == 2) path = immutable_store_tree_path(store, id)
    end function canonical

    subroutine boundary(ready, temp)
        character(len=*), intent(in) :: ready
        character(len=:), allocatable, intent(out) :: temp
        logical :: found
        integer :: ios
        call wait_marker(ready, found)
        call test_assert(suite, found, 'worker reaches the exact late boundary')
        if (.not. found) stop 28
        call read_text(ready, temp, ios)
        call test_assert_equal_int(suite, 0, ios, 'owned temporary is recorded')
        ios = index(temp, achar(10))
        if (ios > 0) temp = temp(:ios - 1)
        temp = trim(temp)
    end subroutine boundary

    subroutine result_after_wait(pid, base, expected)
        integer(c_int), intent(in) :: pid
        character(len=*), intent(in) :: base
        integer, intent(in) :: expected
        integer(c_int) :: child_status
        character(len=:), allocatable :: observed
        integer :: result, ios
        call wait_worker(pid, child_status)
        call test_assert_equal_int(suite, 0, int(child_status), 'worker exits normally')
        call read_text(base//'/result', observed, ios)
        if (ios == 0) read (observed, *, iostat=ios) result
        call test_assert_equal_int(suite, 0, ios, 'worker reports the actual library status')
        if (ios /= 0) return
        if (expected == -1) then
            call test_assert(suite, result /= IMMUTABLE_OK, 'poisoned publication fails verification')
        else
            call test_assert_equal_int(suite, expected, result, 'expected publication outcome')
        end if
    end subroutine result_after_wait

    subroutine check_bytes(path, expected, label)
        character(len=*), intent(in) :: path, expected, label
        character(len=:), allocatable :: observed
        integer :: ios
        call read_text(path, observed, ios)
        call test_assert(suite, ios == 0 .and. observed == expected, label)
    end subroutine check_bytes

    subroutine check_outside_and_temps(base, temp)
        character(len=*), intent(in) :: base, temp
        integer(c_int) :: checked, mode
        logical :: exists
        call check_bytes(base//'/sentinel', 'OUTSIDE SENTINEL', 'outside sentinel survives')
        mode = -1_c_int
        checked = path_mode((base//'/sentinel')//c_null_char, mode)
        call test_assert(suite, checked == 0 .and. mode == 420, 'outside sentinel mode survives')
        inquire (file=path_dirname(temp), exist=exists)
        call test_assert(suite, .not. exists, 'owned temporary root is completely cleaned')
    end subroutine check_outside_and_temps

    function inode_of(path) result(inode)
        character(len=*), intent(in) :: path
        integer(c_long_long) :: inode, bytes, mtime
        integer(c_int) :: checked
        inode = -1_c_long_long
        checked = file_info(path//c_null_char, bytes, mtime, inode)
        if (checked /= 0_c_int) inode = -1_c_long_long
    end function inode_of

    subroutine retry(store, base, kind, expected_id)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: base, expected_id
        integer, intent(in) :: kind
        type(immutable_tree_entry_t) :: entry(1)
        character(len=64) :: id
        integer :: ierr
        if (kind == 1) then
            call immutable_store_put_blob(store, base//'/source', id, ierr)
        else
            entry(1) = tree_entry(sha256_string(PAYLOAD))
            call immutable_store_put_tree(store, entry, id, ierr)
        end if
        call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'retry publishes/reuses correct object')
        call test_assert(suite, id == expected_id, 'retry returns the independently expected ID')
    end subroutine retry

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
        if (phase == 9) then
            call chain(7_c_int, 9_c_int, (trim(base)//'/ready')//c_null_char, &
                (trim(base)//'/release')//c_null_char, &
                (trim(base)//'/cleanup-ready')//c_null_char, &
                (trim(base)//'/cleanup-release')//c_null_char)
        else if (phase == 10) then
            call no_atomic_clone(1_c_int)
        else
            call configure(int(phase, c_int), (trim(base)//'/ready')//c_null_char, &
                (trim(base)//'/release')//c_null_char)
        end if
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
end program test_immutable_publication_race
