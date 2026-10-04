program test_immutable_materialization
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, test_suite_exit
    use fx_hash, only: sha256_string, sha256_file
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_temp_root, fx_test_mkdir_p, fx_test_remove_tree
    use fx_test_process, only: test_process_spawn, test_process_wait_once, &
        test_process_signal, test_process_sleep_ms
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        immutable_store_init, immutable_store_put_blob, immutable_store_blob_path, &
        immutable_store_materialize_blob, &
        IMMUTABLE_OK, IMMUTABLE_INVALID, IMMUTABLE_BLOB, &
        IMMUTABLE_MISSING, IMMUTABLE_CORRUPT, &
        IMMUTABLE_MATERIALIZE_COPY, IMMUTABLE_MATERIALIZE_CLONE
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_materialize_tree, immutable_store_verify_tree
    use fx_immutable_manifest, only: immutable_entries_canonical, immutable_manifest_serialize
    implicit none
    interface
        integer(c_int) function probe_alias(dir, a, b) bind(C, name='fx_immutable_probe_alias')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: dir(*), a(*), b(*)
        end function probe_alias
        integer(c_int) function is_apfs(path) bind(C, name='fx_immutable_is_apfs')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function is_apfs
        integer(c_int) function clone_id(path, id) bind(C, name='fx_immutable_clone_id')
            import :: c_int, c_char, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: id
        end function clone_id
    end interface
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_tree_entry_t) :: entry(1)
    character(len=512) :: root, source
    character(len=64) :: blob, tree
    character(len=32) :: required
    character(len=*), parameter :: PAYLOAD = 'original immutable payload'
    integer :: ierr, end_path
    character(len=512, kind=c_char) :: scratch
    integer(c_int) :: status
    logical :: require_apfs

    call get_command_argument(1, required)
    if (trim(required) == '--materialize-crash-worker') then
        call run_crash_worker()
        stop 0
    end if
    call test_suite_init(suite, 'immutable_materialization')
    scratch = c_null_char
    status = int(fx_test_temp_root(scratch, 512))
    call test_assert_equal_int(suite, 0, int(status), 'system scratch resolves physically')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-owned42-', proc_pid()
    call immutable_store_init(store, trim(root)//'/store', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'private store opens')
    source = trim(root)//'/source'
    call write_text(trim(source), PAYLOAD)
    call immutable_store_put_blob(store, trim(source), blob, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'fixture blob publishes')
    call test_assert_equal_str(suite, sha256_string(PAYLOAD), blob, 'fixture ID has real bytes')
    entry(1)%path = 'payload.bin'
    entry(1)%role = 'source'
    entry(1)%object_id = blob
    entry(1)%kind = IMMUTABLE_BLOB
    entry(1)%mode = 420
    call immutable_store_put_tree(store, entry, tree, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'fixture tree publishes')
    required = ''
    call get_environment_variable('FX_IMMUTABLE_REQUIRE_APFS', required)
    require_apfs = trim(required) == '1'
    if (require_apfs) then
        status = is_apfs(trim(root)//c_null_char)
        call test_assert_equal_int(suite, 1, int(status), 'required APFS volume is real')
    end if
    call test_collisions()
    call test_clone_and_fallback()
    call test_materialization_contract()
    call test_failed_materialization_preserves_destination()
    call test_materialization_crash()
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: u, ios, close_ios
        open (newunit=u, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
        call test_assert_equal_int(suite, 0, ios, 'write fixture opens')
        if (ios /= 0) return
        write (u, iostat=ios) text
        close (u, iostat=close_ios)
        call test_assert(suite, ios == 0 .and. close_ios == 0, 'fixture bytes close')
    end subroutine write_text

    subroutine read_text(path, text, ios)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ios
        integer :: u, count
        text = ''
        count = -1
        inquire (file=path, size=count, iostat=ios)
        if (ios /= 0 .or. count < 0) return
        text = repeat(' ', count)
        open (newunit=u, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) text
        close (u)
    end subroutine read_text

    subroutine test_collisions()
        type(immutable_tree_entry_t) :: input(2)
        type(immutable_tree_entry_t), allocatable :: sorted(:)
        character(len=:), allocatable :: first, second, encoded
        integer :: pair, result
        integer(c_int) :: alias, mkdir_status
        input = [entry(1), entry(1)]
        mkdir_status = int(fx_test_mkdir_p(trim(root)//'/alias-probe'))
        call test_assert_equal_int(suite, 0, int(mkdir_status), 'alias oracle directory')
        do pair = 1, 2
            if (pair == 1) then
                first = 'File'
                second = 'file'
            else
                first = 'caf'//achar(195)//achar(169)
                second = 'cafe'//achar(204)//achar(129)
            end if
            input(1)%path = first
            input(2)%path = second
            alias = probe_alias((trim(root)//'/alias-probe')//c_null_char, &
                first//c_null_char, second//c_null_char)
            call test_assert(suite, alias >= 0, 'exclusive creates independently test aliasing')
            if (require_apfs) call test_assert_equal_int(suite, 1, int(alias), &
                'required APFS actually aliases case/normalization names')
            call immutable_entries_canonical(input, sorted, result)
            if (alias == 1 .or. pair == 1) then
                call test_assert_equal_int(suite, IMMUTABLE_INVALID, result, &
                    'canonical manifest rejects aliased distinct names')
            end if
            call immutable_entries_canonical(input(1:1), sorted, result)
            call test_assert_equal_int(suite, IMMUTABLE_OK, result, &
                'an unambiguous Unicode/case spelling remains supported')
            encoded = immutable_manifest_serialize(sorted)
            call test_assert(suite, index(encoded, first) > 0, &
                'manifest preserves original name bytes without rewriting')
        end do
    end subroutine test_collisions

    subroutine test_clone_and_fallback()
        character(len=512) :: clone, fallback, stored
        character(len=:), allocatable :: observed
        logical :: cloned
        integer :: result, ios
        integer(c_int) :: a_status, b_status
        integer(c_long_long) :: original_id, copied_id
        original_id = 0_c_long_long
        copied_id = -1_c_long_long
        clone = trim(root)//'/forced-clone'
        fallback = trim(root)//'/forced-fallback'
        stored = immutable_store_blob_path(store, blob)
        call immutable_store_materialize_blob(store, blob, trim(clone), 420, &
            IMMUTABLE_MATERIALIZE_CLONE, cloned, result)
        if (require_apfs) then
            call test_assert_equal_int(suite, IMMUTABLE_OK, result, 'APFS clone is mandatory')
            call test_assert(suite, cloned, 'mandatory clone reports clone')
            a_status = clone_id(trim(stored)//c_null_char, original_id)
            b_status = clone_id(trim(clone)//c_null_char, copied_id)
            call test_assert(suite, a_status == 0 .and. b_status == 0, &
                'kernel clone IDs are available for independent clone evidence')
            call test_assert(suite, original_id /= 0 .and. original_id == copied_id, &
                'source and forced clone share the kernel data-stream identity')
        end if
        call immutable_store_materialize_blob(store, blob, trim(fallback), 420, &
            IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        call test_assert_equal_int(suite, IMMUTABLE_OK, result, 'copy strategy succeeds')
        call test_assert(suite, .not. cloned, 'copy strategy uses byte-copy materialization')
        call read_text(trim(fallback), observed, ios)
        call test_assert(suite, ios == 0 .and. observed == PAYLOAD, 'fallback has actual bytes')
        if (require_apfs) then
            b_status = clone_id(trim(fallback)//c_null_char, copied_id)
            call test_assert(suite, b_status == 0 .and. copied_id /= original_id, &
                'byte copy has a different kernel data-stream identity')
        end if
        call write_text(trim(fallback), 'independent edited copy')
        call read_text(trim(stored), observed, ios)
        call test_assert(suite, ios == 0 .and. observed == PAYLOAD, 'copy editing preserves CAS')
    end subroutine test_clone_and_fallback

    subroutine test_materialization_contract()
        character(len=512) :: blob_destination, tree_destination, outside
        character(len=:), allocatable :: observed
        logical :: cloned
        integer :: result, ios, local_err

        local_err = fx_test_mkdir_p(trim(root)//'/materialization-outside')
        call test_assert_equal_int(suite, 0, local_err, &
            'outside materialization sentinel directory is created')
        outside = trim(root)//'/materialization-outside/sentinel'
        call write_text(trim(outside), 'OUTSIDE MATERIALIZATION SENTINEL')
        blob_destination = trim(root)//'/materialization-contract/blob'
        tree_destination = trim(root)//'/materialization-contract/tree'

        call immutable_store_materialize_blob(store, blob, &
            trim(blob_destination), 420, IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        call test_assert_equal_int(suite, IMMUTABLE_OK, result, &
            'blob materialization returns only after publication')
        call read_text(trim(blob_destination), observed, ios)
        call test_assert(suite, ios == 0 .and. observed == PAYLOAD, &
            'returned blob materialization contains every expected byte')

        call immutable_store_materialize_tree(store, tree, &
            trim(tree_destination), IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        call test_assert_equal_int(suite, IMMUTABLE_OK, result, &
            'tree materialization returns only after publication')
        call read_text(trim(tree_destination)//'/payload.bin', observed, ios)
        call test_assert(suite, ios == 0 .and. observed == PAYLOAD, &
            'returned tree materialization contains its complete source blob')
        call read_text(trim(outside), observed, ios)
        call test_assert(suite, ios == 0 .and. &
            observed == 'OUTSIDE MATERIALIZATION SENTINEL', &
            'materialization leaves the unrelated outside file unchanged')
    end subroutine test_materialization_contract

    subroutine test_failed_materialization_preserves_destination()
        character(len=512) :: destination
        character(len=:), allocatable :: stored, observed
        logical :: cloned
        integer :: result, ios, local_err

        destination = trim(root)//'/materialization-failure/destination'
        local_err = fx_test_mkdir_p(trim(root)//'/materialization-failure')
        call test_assert_equal_int(suite, 0, local_err, &
            'failed-materialization parent is created')
        call write_text(trim(destination), 'existing destination sentinel')
        stored = immutable_store_blob_path(store, blob)

        call write_text(stored, 'corrupt immutable bytes')
        call immutable_store_materialize_blob(store, blob, trim(destination), &
            420, IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        call test_assert_equal_int(suite, IMMUTABLE_CORRUPT, result, &
            'materialization rejects bytes that do not match the blob ID')
        call read_text(trim(destination), observed, ios)
        call test_assert(suite, ios == 0 .and. &
            observed == 'existing destination sentinel', &
            'corrupt input leaves the prior destination unchanged')

        local_err = fx_test_remove_tree(stored)
        call test_assert_equal_int(suite, 0, local_err, &
            'test removes the blob before materialization opens it')
        call immutable_store_materialize_blob(store, blob, trim(destination), &
            420, IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        call test_assert_equal_int(suite, IMMUTABLE_MISSING, result, &
            'unlink before opening reports a cache miss')
        call read_text(trim(destination), observed, ios)
        call test_assert(suite, ios == 0 .and. &
            observed == 'existing destination sentinel', &
            'a pre-open cache miss leaves the prior destination unchanged')
    end subroutine test_failed_materialization_preserves_destination

    subroutine test_materialization_crash()
        integer, parameter :: NBYTES = 64 * 1024 * 1024
        type(immutable_tree_entry_t) :: large_entry(1)
        character(len=512) :: source_path, outside, destination, result_path
        character(len=512) :: parent_dir, top_stage, stage_file, executable
        character(len=64) :: expected_blob, stored_blob, large_tree, actual_blob
        character(len=:), allocatable :: actual_text
        character(len=512) :: worker_args(6)
        integer(int64) :: staged_size
        logical :: found, completed, killed, exists
        logical :: cloned, partial_seen
        integer :: pid, process_status, state, signal_status
        integer :: ierr, result, io_status, attempt, unit

        source_path = trim(root)//'/large-materialization-source'
        call write_repeated(source_path, NBYTES)
        call sha256_file(trim(source_path), expected_blob, ierr)
        call test_assert_equal_int(suite, 0, ierr, &
            'large materialization source hashes independently')
        call immutable_store_put_blob(store, trim(source_path), stored_blob, ierr)
        call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
            'large materialization blob publishes')
        call test_assert_equal_str(suite, expected_blob, stored_blob, &
            'store publishes the independently computed blob ID')
        large_entry(1)%path = 'payload.bin'
        large_entry(1)%role = 'source'
        large_entry(1)%object_id = stored_blob
        large_entry(1)%kind = IMMUTABLE_BLOB
        large_entry(1)%mode = 420
        call immutable_store_put_tree(store, large_entry, large_tree, ierr)
        call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
            'large materialization tree publishes')

        parent_dir = trim(root)//'/materialization-crash-parent'
        ierr = fx_test_mkdir_p(parent_dir)
        call test_assert_equal_int(suite, 0, ierr, &
            'crash destination parent is created')
        outside = trim(root)//'/materialization-crash-outside'
        ierr = fx_test_mkdir_p(trim(outside))
        call test_assert_equal_int(suite, 0, ierr, &
            'crash outside directory is created')
        call write_text(trim(outside)//'/sentinel', 'CRASH OUTSIDE SENTINEL')
        destination = trim(parent_dir)//'/result'
        result_path = trim(root)//'/materialization-crash-result'
        call get_command_argument(0, executable)
        worker_args = [character(len=512) :: trim(executable), &
            '--materialize-crash-worker', trim(store%root_dir), large_tree, &
            trim(destination), trim(result_path)]
        call test_process_spawn(worker_args, pid, ierr)
        call test_assert_equal_int(suite, 0, ierr, &
            'native process API starts an exact materialization worker')
        if (ierr /= 0) return
        top_stage = trim(parent_dir)//'/.fx-owned-'//number(int(pid))//'-0'
        stage_file = trim(top_stage)//'/.fx-owned-'//number(int(pid))//'-1/payload'
        completed = .false.
        killed = .false.
        partial_seen = .false.
        process_status = -1_c_int
        do attempt = 1, 6000
            call test_process_wait_once(pid, process_status, state)
            if (state == 1_c_int) then
                completed = .true.
                exit
            end if
            if (state < 0_c_int) exit
            staged_size = -1_int64
            inquire(file=trim(stage_file), exist=found, size=staged_size)
            if (found .and. staged_size > 0_int64 .and. &
                    staged_size < int(NBYTES, int64)) then
                partial_seen = .true.
                call test_process_signal(pid, 9, signal_status)
                killed = signal_status == 0_c_int
                exit
            end if
            call test_process_sleep_ms(2)
        end do
        if (.not. completed) then
            do attempt = 1, 3000
                call test_process_wait_once(pid, process_status, state)
                if (state == 1) then
                    completed = .true.
                    exit
                end if
                if (state < 0_c_int) exit
                call test_process_sleep_ms(5)
            end do
        end if
        if (state /= 1) then
            call test_process_signal(pid, 9, signal_status)
            do attempt = 1, 3000
                call test_process_wait_once(pid, process_status, state)
                if (state == 1) exit
                if (state < 0) exit
                call test_process_sleep_ms(5)
            end do
        end if
        call test_assert(suite, state == 1, &
            'materialization worker exits or is reaped after exact-PID kill')
        if (killed) call test_assert_equal_int(suite, 137, process_status, &
            'SIGKILL targets only the exact materialization worker PID')
        if (killed) call test_assert(suite, partial_seen, &
            'crash was observed during an independently visible partial copy')

        inquire(file=trim(destination)//'/payload.bin', exist=exists)
        if (exists) then
            call sha256_file(trim(destination)//'/payload.bin', actual_blob, ierr)
            call test_assert_equal_int(suite, 0, ierr, &
                'visible materialized blob can be independently hashed')
            call test_assert_equal_str(suite, stored_blob, actual_blob, &
                'a visible materialization is complete even if the worker exited')
        else
            call test_assert(suite, killed, &
                'a missing destination follows only an interrupted transaction')
            call immutable_store_verify_tree(store, large_tree, ierr)
            call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
                'interrupted materialization leaves the immutable source graph valid')
        end if
        inquire(file=trim(result_path), exist=found)
        result = -1
        if (found) then
            open(newunit=unit, file=trim(result_path), status='old', &
                action='read', iostat=io_status)
            if (io_status == 0) then
                read(unit, *, iostat=io_status) result
                close(unit)
            end if
            call test_assert(suite, io_status == 0 .and. result == IMMUTABLE_OK, &
                'a worker that returned reports a complete materialization')
        end if
        call read_text(trim(outside)//'/sentinel', actual_text, ierr)
        call test_assert(suite, ierr == 0 .and. &
            actual_text == 'CRASH OUTSIDE SENTINEL', &
            'interrupted materialization leaves outside data unchanged')
        inquire(file=trim(top_stage), exist=exists)
        if (exists) then
            ierr = fx_test_remove_tree(trim(top_stage))
            call test_assert_equal_int(suite, 0, ierr, &
                'interrupted private materialization staging is removable')
        end if
    end subroutine test_materialization_crash

    subroutine write_repeated(path, byte_count)
        character(len=*), intent(in) :: path
        integer, intent(in) :: byte_count
        character(len=65536) :: chunk
        integer :: i, remaining, count, unit, ios, close_ios

        do i = 1, len(chunk)
            chunk(i:i) = char(mod(i, 251))
        end do
        open(newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ios)
        call test_assert_equal_int(suite, 0, ios, 'large source file opens')
        if (ios /= 0) return
        remaining = byte_count
        do while (remaining > 0)
            count = min(remaining, len(chunk))
            write(unit, iostat=ios) chunk(:count)
            if (ios /= 0) exit
            remaining = remaining - count
        end do
        close(unit, iostat=close_ios)
        call test_assert(suite, ios == 0 .and. close_ios == 0, &
            'large source file closes after all bytes are written')
    end subroutine write_repeated

    subroutine run_crash_worker()
        type(immutable_store_t) :: worker_store
        character(len=512) :: store_root, tree_id, destination, result_path
        integer :: worker_status, ios, unit
        logical :: cloned
        call get_command_argument(2, store_root)
        call get_command_argument(3, tree_id)
        call get_command_argument(4, destination)
        call get_command_argument(5, result_path)
        call immutable_store_init(worker_store, trim(store_root), worker_status)
        if (worker_status /= IMMUTABLE_OK) stop 31
        call immutable_store_materialize_tree(worker_store, trim(tree_id), &
            trim(destination), IMMUTABLE_MATERIALIZE_COPY, cloned, worker_status)
        open(newunit=unit, file=trim(result_path), status='replace', iostat=ios)
        if (ios == 0) then
            write(unit, *, iostat=ios) worker_status
            close(unit)
        end if
        if (ios /= 0) stop 32
    end subroutine run_crash_worker

    function number(i) result(text)
        integer, intent(in) :: i
        character(len=:), allocatable :: text
        character(len=24) :: buffer
        write (buffer, '(i0)') i
        text = trim(buffer)
    end function number
end program test_immutable_materialization
