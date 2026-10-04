program test_immutable_publication_race
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_ptr, c_long_long, &
        c_null_char, c_associated
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
    use fx_immutable_owned, only: owned_begin_path, owned_write, owned_publish, &
        owned_reject, owned_dispose
    use fx_test_fs, only: fx_test_temp_root, fx_test_mkdir_p, &
        fx_test_lock_directory, fx_test_unlock
    use fx_test_process, only: test_process_spawn, test_process_wait_once, &
        test_process_signal, test_process_sleep_ms
    implicit none
    interface
        integer(c_int) function file_info(path, bytes, mtime, inode) &
                bind(C, name='fx_immutable_file_info')
            import :: c_int, c_char, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: bytes, mtime, inode
        end function file_info
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
    end interface
    character(len=*), parameter :: PAYLOAD = 'captured publication payload'
    type(test_suite_t) :: suite
    character(len=512) :: root, argument
    character(len=512, kind=c_char) :: scratch
    integer(c_int) :: status
    integer :: kind, phase, attack, trial, end_path

    call get_command_argument(1, argument)
    if (trim(argument) == '--publish-worker') then
        call run_worker()
        stop 0
    end if
    call test_suite_init(suite, 'immutable_publication')
    scratch = c_null_char
    status = int(fx_test_temp_root(scratch, 512))
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
        character(len=:), allocatable :: base, expected, temp, final, parent_dir
        integer :: pid
        integer(c_int) :: changed
        integer :: lock_fd, local_err
        integer :: child_status
        logical :: staged
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        call write_text(base//'/sentinel', 'OUTSIDE SENTINEL')
        final = canonical(store, kind, id)
        parent_dir = path_dirname(final)
        local_err = fx_test_mkdir_p(parent_dir)
        call test_assert_equal_int(suite, 0, local_err, &
            'object shard exists before external publication admission')
        lock_fd = fx_test_lock_directory(parent_dir)
        call test_assert(suite, lock_fd >= 0, &
            'oracle holds the real publication directory lock')
        if (lock_fd < 0) return
        pid = spawn_worker(base, kind, 8)
        call test_assert(suite, pid > 0, 'late publication worker starts')
        if (pid <= 0) then
            local_err = fx_test_unlock(lock_fd)
            return
        end if
        call wait_for_owned_stage(parent_dir, pid, len(expected), temp, staged)
        call test_assert(suite, staged, &
            'publisher reaches the real lock with a verified read-only temp file')
        if (.not. staged) then
            local_err = fx_test_unlock(lock_fd)
            call wait_worker(pid, child_status)
            return
        end if
        changed = rename_path(temp//c_null_char, (temp//'.held')//c_null_char)
        call test_assert_equal_int(suite, 0, int(changed), 'entry moves after final ownership check')
        if (attack == 1) then
            changed = symlink_path((base//'/sentinel')//c_null_char, temp//c_null_char)
            call test_assert_equal_int(suite, 0, int(changed), 'late entry becomes outside symlink')
        else
            call write_text(temp, 'ARBITRARY SUBSTITUTED BYTES')
        end if
        local_err = fx_test_unlock(lock_fd)
        call test_assert_equal_int(suite, 0, local_err, &
            'oracle releases the real publication directory lock')
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
        character(len=:), allocatable :: base, expected, temp, final, parent_dir
        integer :: pid
        integer(c_int) :: changed
        integer(c_long_long) :: winner_inode
        integer :: lock_fd, local_err
        integer :: child_status
        logical :: found
        base = trim(root)//'/case-'//number(trial)
        call fixture(base, kind, store, id, expected)
        call write_text(base//'/sentinel', 'OUTSIDE SENTINEL')
        final = canonical(store, kind, id)
        pid = spawn_worker(base, kind, 9)
        call test_assert(suite, pid > 0, 'rejected publication worker starts')
        if (pid <= 0) return
        call wait_marker(base//'/ready', found)
        call test_assert(suite, found, &
            'Fortran worker publishes its deliberately invalid candidate')
        if (.not. found) then
            call wait_worker(pid, child_status)
            return
        end if
        parent_dir = path_dirname(final)
        temp = parent_dir//'/.fx-owned-'//number(pid)//'-0/payload'
        changed = rename_path(final//c_null_char, (base//'/rejected-object')//c_null_char)
        call test_assert_equal_int(suite, 0, int(changed), 'rejected inode leaves canonical slot')
        call retry(store, base, kind, id)
        winner_inode = inode_of(final)
        call test_assert(suite, winner_inode > 0, 'concurrent valid winner has real inode')
        lock_fd = fx_test_lock_directory(parent_dir)
        call test_assert(suite, lock_fd >= 0, &
            'oracle holds the real cleanup admission lock')
        if (lock_fd < 0) then
            call write_text(base//'/release', 'release')
            call result_after_wait(pid, base, IMMUTABLE_OK)
            return
        end if
        call write_text(base//'/release', 'release')
        call check_bytes(final, expected, &
            'valid winner remains complete while cleanup waits for admission')
        local_err = fx_test_unlock(lock_fd)
        call test_assert_equal_int(suite, 0, local_err, &
            'oracle releases the real cleanup admission lock')
        call result_after_wait(pid, base, IMMUTABLE_OK)
        call test_assert(suite, inode_of(final) == winner_inode, &
            'rejection cleanup preserves the concurrent winner inode')
        call check_bytes(final, expected, 'concurrent winner retains actual valid bytes')
        call check_outside_and_temps(base, temp)
        call retry(store, base, kind, id)
    end subroutine test_cleanup_winner

    function canonical(store, kind, id) result(path)
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: kind
        character(len=*), intent(in) :: id
        character(len=:), allocatable :: path
        path = immutable_store_blob_path(store, id)
        if (kind == 2) path = immutable_store_tree_path(store, id)
    end function canonical

    subroutine result_after_wait(pid, base, expected)
        integer, intent(in) :: pid
        character(len=*), intent(in) :: base
        integer, intent(in) :: expected
        integer :: child_status
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
        integer :: pid, ierr
        character(len=1024) :: executable
        character(len=1024) :: worker_args(5)
        call get_command_argument(0, executable)
        worker_args = [character(len=1024) :: trim(executable), '--publish-worker', &
            base, number(kind), number(phase)]
        call test_process_spawn(worker_args, pid, ierr)
        if (ierr /= 0) pid = -1
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
        if (ierr /= IMMUTABLE_OK) stop 25
        if (phase == 9) then
            call run_cleanup_worker(store, base, kind)
            return
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
    end subroutine run_worker

    subroutine run_cleanup_worker(store, base, kind)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: base
        integer, intent(in) :: kind
        type(immutable_tree_entry_t) :: entry(1)
        type(c_ptr) :: capture
        character(len=64) :: id, blob_id
        character(len=:), allocatable :: manifest, final
        character(kind=c_char, len=*), parameter :: BAD = 'invalid candidate bytes'
        integer(c_int) :: status
        integer :: ierr, unit
        logical :: released

        blob_id = sha256_string(PAYLOAD)
        if (kind == 1) then
            id = blob_id
            final = immutable_store_blob_path(store, id)
        else
            entry(1) = tree_entry(blob_id)
            manifest = immutable_manifest_serialize(entry)
            id = sha256_string(manifest)
            final = immutable_store_tree_path(store, id)
        end if
        capture = owned_begin_path(trim(final)//c_null_char, 0_c_int)
        if (.not. c_associated(capture)) stop 26
        status = owned_write(capture, BAD, int(len(BAD), c_int))
        if (status == 0_c_int) status = owned_publish(capture)
        if (status /= 0_c_int) stop 27
        call write_text(base//'/ready', 'published')
        call wait_marker(base//'/release', released)
        if (.not. released) stop 28
        status = owned_reject(capture)
        open (newunit=unit, file=base//'/result', status='replace', iostat=ierr)
        if (ierr == 0) then
            write (unit, *, iostat=ierr) status
            close (unit)
        end if
        call owned_dispose(capture)
        if (ierr /= 0) stop 29
    end subroutine run_cleanup_worker

    subroutine wait_for_owned_stage(parent_dir, pid, expected_size, path, ready)
        character(len=*), intent(in) :: parent_dir
        integer, intent(in) :: pid
        integer, intent(in) :: expected_size
        character(len=:), allocatable, intent(out) :: path
        logical, intent(out) :: ready
        integer(c_long_long) :: bytes, mtime, inode
        integer(c_int) :: status
        integer :: attempt

        path = trim(parent_dir)//'/.fx-owned-'//number(int(pid))//'-0/payload'
        ready = .false.
        do attempt = 1, 3000
            bytes = -1_c_long_long
            mtime = -1_c_long_long
            inode = -1_c_long_long
            status = file_info(path//c_null_char, bytes, mtime, inode)
            if (status == 0_c_int .and. bytes == expected_size) then
                ready = .true.
                return
            end if
            call test_process_sleep_ms(10)
        end do
    end subroutine wait_for_owned_stage

    subroutine wait_marker(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer :: i
        found = .false.
        do i = 1, 1000
            inquire (file=path, exist=found)
            if (found) return
            call test_process_sleep_ms(10)
        end do
    end subroutine wait_marker

    subroutine wait_worker(pid, status)
        integer, intent(in) :: pid
        integer, intent(out) :: status
        integer :: i
        integer :: state, signal_err
        do i = 1, 1000
            call test_process_wait_once(pid, status, state)
            if (state == 1) return
            if (state < 0) exit
            call test_process_sleep_ms(10)
        end do
        call test_process_signal(pid, 9, signal_err)
        do i = 1, 500
            call test_process_wait_once(pid, status, state)
            if (state == 1) return
            if (state < 0) exit
            call test_process_sleep_ms(10)
        end do
        status = 999_c_int
    end subroutine wait_worker
end program test_immutable_publication_race
