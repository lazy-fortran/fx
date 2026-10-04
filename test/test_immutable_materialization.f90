program test_immutable_materialization
    use, intrinsic :: iso_c_binding, only: c_int, c_char, c_long_long, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, test_suite_exit
    use fx_hash, only: sha256_string
    use fx_proc, only: proc_pid
    use fx_path, only: path_dirname
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        immutable_store_init, immutable_store_put_blob, immutable_store_blob_path, &
        immutable_store_tree_path, immutable_store_materialize_blob, &
        IMMUTABLE_OK, IMMUTABLE_INVALID, IMMUTABLE_BLOB, &
        IMMUTABLE_MATERIALIZE_COPY, IMMUTABLE_MATERIALIZE_CLONE, IMMUTABLE_MATERIALIZE_AUTO
    use fx_immutable_tree, only: immutable_store_put_tree, immutable_store_materialize_tree
    use fx_immutable_manifest, only: immutable_entries_canonical, immutable_manifest_serialize
    use immutable_marker_oracle, only: marker_boundary_configure, probe_ready_marker
    implicit none
    interface
        integer(c_int) function tmp_root(out, cap) &
                bind(C, name='fx_immutable_test_tmp_root')
            import :: c_int, c_char
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function tmp_root
        integer(c_int) function mkdirs(path) bind(C, name='fx_immutable_mkdirs_sync')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function mkdirs
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
        integer(c_int) function unlink_path(path) bind(C, name='unlink')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function unlink_path
        integer(c_int) function sleep_us(time) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: time
        end function sleep_us
        integer(c_int) function fork_process() bind(C, name='fork')
            import :: c_int
        end function fork_process
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
        integer(c_int) function path_mode(path, mode) bind(C, name='fx_immutable_path_mode')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), intent(out) :: mode
        end function path_mode
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
        subroutine force_copy(value) bind(C, name='fx_owned_test_force_copy')
            import :: c_int
            integer(c_int), value :: value
        end subroutine force_copy
    end interface
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_tree_entry_t) :: entry(1)
    character(len=512) :: root, source
    character(len=64) :: blob, tree
    character(len=32) :: required
    character(len=*), parameter :: PAYLOAD = 'original immutable payload'
    integer :: ierr, mode, end_path
    character(len=512, kind=c_char) :: scratch
    integer(c_int) :: status
    logical :: require_apfs

    call get_command_argument(1, required)
    if (trim(required) == '--materialize-worker') then
        call run_worker()
    end if
    call test_suite_init(suite, 'immutable_materialization')
    scratch = c_null_char
    status = tmp_root(scratch, 512_c_int)
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
    do mode = 1, 6
        call test_substitution(mode)
    end do
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
        mkdir_status = mkdirs((trim(root)//'/alias-probe')//c_null_char)
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
        call force_copy(1_c_int)
        call immutable_store_materialize_blob(store, blob, trim(fallback), 420, &
            IMMUTABLE_MATERIALIZE_AUTO, cloned, result)
        call force_copy(0_c_int)
        call test_assert_equal_int(suite, IMMUTABLE_OK, result, 'forced fallback succeeds')
        call test_assert(suite, .not. cloned, 'automatic strategy used real byte-copy fallback')
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

    subroutine test_substitution(which)
        integer, intent(in) :: which
        character(len=512) :: destination, outside, ready, release, result_path, swapped
        integer(c_int) :: pid, process_status, rename_status, link_status, cleanup
        integer :: result, ios, phase
        logical :: found
        character(len=:), allocatable :: observed

        swapped = ''
        call prepare_race(which, destination, outside, ready, release, result_path)
        pid = spawn_worker(which)
        call test_assert(suite, pid > 0, 'independent materialization worker starts')
        if (pid <= 0) return
        if (which == 6) call probe_ready_marker(suite, trim(ready))
        call wait_marker(ready, found)
        call test_assert(suite, found, 'worker reaches the exact descriptor boundary')
        if (found) then
            call substitution_paths(which, destination, ready, swapped)
            rename_status = rename_path(trim(swapped)//c_null_char, &
                (trim(swapped)//'.held')//c_null_char)
            link_status = symlink_path(trim(outside)//c_null_char, trim(swapped)//c_null_char)
            call test_assert(suite, rename_status == 0 .and. link_status == 0, &
                'real rename and symlink substitution occurred before release')
        end if
        call write_text(trim(release), 'release')
        call wait_worker(pid, process_status)
        call test_assert_equal_int(suite, 0, int(process_status), 'worker exits normally')
        open (newunit=ios, file=trim(result_path), status='old', action='read', iostat=result)
        if (result == 0) then
            read (ios, *, iostat=result) phase
            close (ios)
            if (result == 0) call check_race_result(which, destination, swapped, phase)
        end if
        call test_assert_equal_int(suite, 0, result, 'worker reports the real library result')
        if (found) then
            cleanup = unlink_path(trim(swapped)//c_null_char)
            if (which == 1 .or. which == 2 .or. which == 5) then
                cleanup = rename_path((trim(swapped)//'.held')//c_null_char, &
                    trim(swapped)//c_null_char)
                call test_assert_equal_int(suite, 0, int(cleanup), 'source/parent fixture restores')
            end if
        end if
        call read_text(trim(root)//'/outside'//number(which)//'/sentinel', observed, ios)
        call test_assert(suite, ios == 0 .and. observed == 'SENTINEL', &
            'outside sentinel bytes are untouched')
    end subroutine test_substitution

    subroutine prepare_race(which, destination, outside, ready, release, result_path)
        integer, intent(in) :: which
        character(len=*), intent(out) :: destination, outside, ready, release, result_path
        type(immutable_tree_entry_t) :: forged(1)
        integer(c_int) :: created
        character(len=:), allocatable :: dir, text
        dir = trim(root)//'/outside'//number(which)
        created = mkdirs(dir//c_null_char)
        call test_assert_equal_int(suite, 0, int(created), 'outside fixture directory creates')
        call write_text(dir//'/sentinel', 'SENTINEL')
        destination = trim(root)//'/dest'//number(which)//'/result'
        created = mkdirs((trim(path_dirname(destination)))//c_null_char)
        ready = trim(root)//'/ready'//number(which)
        release = trim(root)//'/release'//number(which)
        result_path = trim(root)//'/result'//number(which)
        outside = dir
        if (which == 1) call write_text(dir//'/'//blob, 'wrong source bytes')
        if (which == 3 .or. which == 6) outside = dir//'/sentinel'
        if (which == 5) then
            forged(1) = entry(1)
            forged(1)%path = 'evil.bin'
            text = immutable_manifest_serialize(forged)
            outside = dir//'/forged-manifest'
            call write_text(trim(outside), text)
        end if
    end subroutine prepare_race

    subroutine substitution_paths(which, destination, ready, swapped)
        integer, intent(in) :: which
        character(len=*), intent(in) :: destination, ready
        character(len=*), intent(out) :: swapped
        integer :: u, ios
        select case (which)
        case (1)
            swapped = path_dirname(immutable_store_blob_path(store, blob))
        case (2)
            swapped = path_dirname(destination)
        case (3, 4, 6)
            open (newunit=u, file=trim(ready), status='old', iostat=ios)
            if (ios == 0) then
                read (u, '(a)', iostat=ios) swapped
                close (u)
            end if
            call test_assert_equal_int(suite, 0, ios, 'boundary exposes owned staging path')
        case (5)
            swapped = immutable_store_tree_path(store, tree)
        end select
    end subroutine substitution_paths

    subroutine check_race_result(which, destination, swapped, result)
        integer, intent(in) :: which, result
        character(len=*), intent(in) :: destination, swapped
        character(len=:), allocatable :: actual_path, observed
        logical :: exists
        integer :: ios
        integer(c_int) :: checked, actual_mode
        actual_mode = -1_c_int
        if (which == 3 .or. which == 4 .or. which == 6) then
            call test_assert(suite, result /= IMMUTABLE_OK, 'substituted staging entry fails closed')
            inquire (file=trim(destination), exist=exists)
            call test_assert(suite, .not. exists, 'invalid staging has no published destination')
        else
            call test_assert_equal_int(suite, IMMUTABLE_OK, result, &
                'held verified source/destination survives pathname substitution')
            actual_path = trim(destination)
            if (which == 2) actual_path = trim(swapped)//'.held/result'
            if (which == 5) actual_path = actual_path//'/payload.bin'
            call read_text(actual_path, observed, ios)
            call test_assert(suite, ios == 0 .and. observed == PAYLOAD, &
                'materialized bytes are independently the original payload')
        end if
        if (which == 2 .or. which == 4) then
            inquire (file=trim(root)//'/outside'//number(which)//'/result', exist=exists)
            call test_assert(suite, .not. exists, 'outside parent receives no destination')
            inquire (file=trim(root)//'/outside'//number(which)//'/payload.bin', exist=exists)
            call test_assert(suite, .not. exists, 'outside staging directory receives no payload')
        end if
        if (which == 5) then
            inquire (file=trim(destination)//'/evil.bin', exist=exists)
            call test_assert(suite, .not. exists, 'unverified replacement manifest is never parsed')
        end if
        checked = path_mode((trim(root)//'/outside'//number(which)//'/sentinel')// &
            c_null_char, actual_mode)
        call test_assert(suite, checked == 0 .and. actual_mode == 420, &
            'outside sentinel permission mode is untouched')
    end subroutine check_race_result

    function spawn_worker(which) result(pid)
        use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_null_ptr
        integer, intent(in) :: which
        integer(c_int) :: pid, exec_status
        character(kind=c_char), target :: args(1024, 6)
        type(c_ptr) :: argv(7)
        character(len=1024) :: executable, text(6)
        integer :: i, j
        interface
            integer(c_int) function exec_child(path, arguments) bind(C, name='execv')
                import :: c_int, c_char, c_ptr
                character(kind=c_char), intent(in) :: path(*)
                type(c_ptr), intent(in) :: arguments(*)
            end function exec_child
        end interface
        call get_command_argument(0, executable)
        text = [character(len=1024) :: trim(executable), '--materialize-worker', &
            trim(root), blob, tree, number(which)]
        args = c_null_char
        do i = 1, 6
            do j = 1, len_trim(text(i))
                args(j, i) = text(i)(j:j)
            end do
            argv(i) = c_loc(args(1, i))
        end do
        argv(7) = c_null_ptr
        pid = fork_process()
        if (pid == 0) then
            exec_status = exec_child(args(:, 1), argv)
            call exit_child(127_c_int)
        end if
    end function spawn_worker

    subroutine run_worker()
        character(len=512) :: destination, ready, release, result_path, which_text
        integer :: which, result, u, ios
        integer(c_int) :: phase
        logical :: cloned
        call get_command_argument(2, root)
        call get_command_argument(3, blob)
        call get_command_argument(4, tree)
        call get_command_argument(5, which_text)
        read (which_text, *) which
        call immutable_store_init(store, trim(root)//'/store', result)
        destination = trim(root)//'/dest'//number(which)//'/result'
        ready = trim(root)//'/ready'//number(which)
        release = trim(root)//'/release'//number(which)
        result_path = trim(root)//'/result'//number(which)
        phase = 1_c_int
        if (which == 3 .or. which == 6) phase = 2_c_int
        if (which == 4) phase = 4_c_int
        if (which == 5) phase = 3_c_int
        call configure(phase, trim(ready)//c_null_char, trim(release)//c_null_char)
        if (which == 6) call marker_boundary_configure( &
            (trim(ready)//'.open')//c_null_char, (trim(ready)//'.write')//c_null_char)
        if (which <= 3 .or. which == 6) then
            call immutable_store_materialize_blob(store, blob, trim(destination), &
                420, IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        else
            call immutable_store_materialize_tree(store, tree, trim(destination), &
                IMMUTABLE_MATERIALIZE_COPY, cloned, result)
        end if
        open (newunit=u, file=trim(result_path), status='replace', iostat=ios)
        if (ios == 0) then
            write (u, *) result
            close (u)
        end if
        call exit_child(0_c_int)
    end subroutine run_worker

    subroutine wait_marker(path, found)
        character(len=*), intent(in) :: path
        logical, intent(out) :: found
        integer :: attempt
        integer(c_int) :: ignored
        found = .false.
        do attempt = 1, 3000
            inquire (file=trim(path), exist=found)
            if (found) return
            ignored = sleep_us(10000_c_int)
        end do
    end subroutine wait_marker

    subroutine wait_worker(pid, result)
        integer(c_int), intent(in) :: pid
        integer(c_int), intent(out) :: result
        integer(c_int) :: found, ignored
        integer :: attempt
        do attempt = 1, 3000
            found = wait_child(pid, result, 1_c_int)
            if (found == pid) return
            ignored = sleep_us(10000_c_int)
        end do
        ignored = kill_child(pid, 9_c_int)
        ignored = wait_child(pid, result, 0_c_int)
    end subroutine wait_worker

    function number(i) result(text)
        integer, intent(in) :: i
        character(len=:), allocatable :: text
        character(len=24) :: buffer
        write (buffer, '(i0)') i
        text = trim(buffer)
    end function number
end program test_immutable_materialization
