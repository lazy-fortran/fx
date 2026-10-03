program test_immutable_store
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_hash, only: sha256_string
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        IMMUTABLE_OK, IMMUTABLE_MISSING, IMMUTABLE_CORRUPT, &
        IMMUTABLE_INVALID, IMMUTABLE_UNSUPPORTED, IMMUTABLE_BLOB, &
        IMMUTABLE_TREE, &
        IMMUTABLE_MATERIALIZE_AUTO, IMMUTABLE_MATERIALIZE_COPY, &
        IMMUTABLE_MATERIALIZE_CLONE, immutable_store_init, &
        immutable_store_put_blob, immutable_store_verify_blob, &
        immutable_store_blob_path, immutable_store_tree_path, &
        immutable_store_materialize_blob, immutable_store_file_info
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree, immutable_store_materialize_tree
    use fx_immutable_manifest, only: immutable_manifest_serialize
    use fx_proc, only: proc_pid, proc_exec_silent, &
        proc_watch_init, proc_watch_add, proc_watch_poll, proc_watch_close
    use fx_path, only: path_dirname
    implicit none

    interface
        integer(c_int) function tmp_root(out, cap) &
                bind(C, name='fx_immutable_test_tmp_root')
            import :: c_int, c_char
            character(kind=c_char), intent(out) :: out(*)
            integer(c_int), value :: cap
        end function tmp_root
        subroutine c_test_configure(ready, copy_release, publish_ready, &
                publish_release, eexist) &
                bind(C, name='fx_immutable_test_configure')
            import :: c_char
            character(kind=c_char), intent(in) :: ready(*), copy_release(*)
            character(kind=c_char), intent(in) :: publish_ready(*)
            character(kind=c_char), intent(in) :: publish_release(*), eexist(*)
        end subroutine c_test_configure

        subroutine c_test_mkdir_configure(path, ready, release, existing) &
                bind(C, name='fx_immutable_test_mkdir_configure')
            import :: c_char
            character(kind=c_char), intent(in) :: path(*), ready(*), release(*)
            character(kind=c_char), intent(in) :: existing(*)
        end subroutine c_test_mkdir_configure

        integer(c_int) function c_mkdirs(path) &
                bind(C, name='fx_immutable_mkdirs_sync')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_mkdirs

        integer(c_int) function c_publish_tree(src, dst) &
                bind(C, name='fx_immutable_publish_tree')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*), dst(*)
        end function c_publish_tree

        integer(c_int) function c_path_mode(path, mode) &
                bind(C, name='fx_immutable_path_mode')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), intent(out) :: mode
        end function c_path_mode
    end interface

    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    character(len=64) :: object_id, tree_id, sub_id, expected_id
    character(len=64) :: crash_id
    character(len=512) :: root, source, copy_path, clone_path
    character(len=512) :: fallback_path, executable
    character(len=512) :: arg
    character(len=1) :: payload(5)
    integer :: ierr, end_path
    character(len=512, kind=c_char) :: scratch

    call get_command_argument(1, arg)
    if (trim(arg) == '--put-blob') then
        call run_worker_put()
        stop 0
    end if
    if (trim(arg) == '--put-blob-barrier') then
        call run_worker_put_barrier()
        stop 0
    end if
    if (trim(arg) == '--mkdir-barrier') then
        call run_mkdir_worker()
        stop 0
    end if
    call test_suite_init(suite, 'fx_immutable_store')
    scratch = c_null_char
    ierr = tmp_root(scratch, 512_c_int)
    call test_assert_equal_int(suite, 0, ierr, 'system scratch resolves physically')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write(root, '(A,I0)') scratch(1:end_path - 1)//'/fx_store42_', proc_pid()
    call immutable_store_init(store, trim(root), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')

    payload = [char(0), 'A', char(255), 'z', char(10)]
    source = trim(root)//'/source.bin'
    call write_bytes(trim(source), payload, ierr)
    call test_assert_equal_int(suite, 0, ierr, 'source fixture is written')
    call immutable_store_put_blob(store, trim(source), object_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'blob publishes')
    expected_id = '7912661781b57f9a100bcd053d68f33f'// &
        '27af71eeaa98c4d321751be451e652fe'
    call test_assert_equal_str(suite, trim(expected_id), trim(object_id), &
        'blob ID hashes payload bytes only')
    call immutable_store_verify_blob(store, object_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'published blob verifies')

    call test_reuse_does_not_write(suite, store, source, object_id)
    call test_concurrent_publish(suite, store, root)
    call test_mkdir_race_sync(suite, root)
    call test_manifest_encoding(suite, object_id)
    call test_role_and_tree_manifests(suite, store, object_id, tree_id, sub_id)
    call test_materialization(suite, store, root, object_id, tree_id)
    call test_materialization_modes(suite, store, root, object_id)
    call test_tree_eexist_sync(suite, store, tree_id, root)
    call test_symlink_shard_rejected(suite, root)
    call test_killed_partial_is_hidden(suite, store, root)
    call test_corrupt_tree_is_reported(suite, store, object_id, tree_id, sub_id)
    call test_corruption_is_reported(suite, store, root)

    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_reuse_does_not_write(s, cache, source_path, id)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: source_path, id
        character(len=512) :: blob_path, watched_path
        integer(int64) :: bytes0, mtime0, ino0, bytes1, mtime1, ino1
        integer :: local_err, fd, wd_dir, wd_file, evt, poll_err
        logical :: event_seen, write_event

        blob_path = immutable_store_blob_path(cache, id)
        call immutable_store_file_info(cache, id, bytes0, mtime0, ino0, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'blob metadata is readable before reuse')
        fd = -1
        call proc_watch_init(fd, local_err)
        if (local_err == 0) then
            call proc_watch_add(fd, trim(path_dirname(blob_path)), 4095, wd_dir, &
                local_err)
        end if
        call test_assert_equal_int(s, 0, local_err, 'watch monitors blob shard')
        if (fd >= 0 .and. local_err == 0) then
            call proc_watch_add(fd, trim(blob_path), 14, wd_file, local_err)
        end if
        call test_assert_equal_int(s, 0, local_err, &
            'write-only watch monitors the canonical blob itself')
        if (fd >= 0 .and. local_err == 0) call drain_events(fd)
        call immutable_store_put_blob(cache, source_path, expected_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'identical payload reuses verified blob')
        call immutable_store_file_info(cache, id, bytes1, mtime1, ino1, &
            local_err)
        call test_assert(s, bytes0 == bytes1 .and. mtime0 == mtime1 .and. &
            ino0 == ino1, 'reuse preserves blob size, inode, and mtime')
        if (fd >= 0) then
            write_event = .false.
            call proc_watch_poll(fd, watched_path, evt, 30, event_seen, poll_err)
            if (event_seen) write_event = .true.
            do while (poll_err == 0 .and. event_seen)
                call proc_watch_poll(fd, watched_path, evt, 0, event_seen, &
                    poll_err)
                if (event_seen) write_event = .true.
            end do
            call test_assert(s, poll_err == 0 .and. .not. write_event, &
                'reuse emits no child-file write or directory event')
            call proc_watch_close(fd, local_err)
        end if
    end subroutine test_reuse_does_not_write

    subroutine test_concurrent_publish(s, cache, base)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base
        integer, parameter :: NWRITERS = 6, NBYTES = 262144
        character(len=512) :: paths(NWRITERS), ready(NWRITERS)
        character(len=512) :: publish_ready(NWRITERS), eexist(NWRITERS)
        character(len=64) :: ref_id
        character(len=512) :: child_exe, number, source_path
        character(len=8192) :: command
        character(len=8192) :: worker_argv(3)
        character(len=512) :: copy_release, publish_release
        character(len=1) :: bytes(NBYTES)
        integer :: t, local_err, file_err, child_status, eexist_count
        logical :: marker_exists

        do t = 1, size(bytes)
            bytes(t) = char(mod(t, 251))
        end do
        ref_id = sha256_string(transfer(bytes, repeat(' ', size(bytes))))
        do t = 1, NWRITERS
            write(paths(t), '(A,A,I0,A)') trim(base), '/writer-', t, '.bin'
            write(ready(t), '(A,A,I0,A)') trim(base), '/copy-ready-', t
            write(publish_ready(t), '(A,A,I0,A)') trim(base), '/pub-ready-', t
            write(eexist(t), '(A,A,I0,A)') trim(base), '/eexist-', t
            call write_bytes(trim(paths(t)), bytes, file_err)
            call test_assert_equal_int(s, 0, file_err, &
                'concurrent writer input created')
        end do
        write(source_path, '(A,A)') trim(base), '/writer-1.bin'
        copy_release = trim(base)//'/copy-release'
        publish_release = trim(base)//'/publish-release'
        call get_command_argument(0, child_exe)
        command = 'status=0; '
        do t = 1, NWRITERS
            write(number, '(I0)') t
            command = trim(command)//quote(trim(child_exe))// &
                ' --put-blob-barrier '//quote(trim(cache%root_dir))//' '// &
                quote(trim(paths(t)))//' '//quote(trim(ready(t)))//' '// &
                quote(trim(copy_release))//' '// &
                quote(trim(publish_ready(t)))//' '// &
                quote(trim(publish_release))//' '//quote(trim(eexist(t)))// &
                ' & p'//trim(number)//'=$!; '
        end do
        do t = 1, NWRITERS
            command = trim(command)//wait_for_marker(trim(ready(t)))
        end do
        command = trim(command)//'touch '//quote(trim(copy_release))//'; '
        do t = 1, NWRITERS
            command = trim(command)//wait_for_marker(trim(publish_ready(t)))
        end do
        command = trim(command)//'touch '//quote(trim(publish_release))//'; '
        do t = 1, NWRITERS
            write(number, '(I0)') t
            command = trim(command)//'wait $p'//trim(number)// &
                ' || status=1; '
        end do
        command = trim(command)//'exit $status'
        worker_argv(1) = 'sh'
        worker_argv(2) = '-c'
        worker_argv(3) = trim(command)
        call proc_exec_silent(worker_argv, 3, child_status)
        call test_assert_equal_int(s, 0, child_status, &
            'barrier-released identical publishers all succeed')
        eexist_count = 0
        do t = 1, NWRITERS
            inquire(file=trim(eexist(t)), exist=marker_exists)
            if (marker_exists) eexist_count = eexist_count + 1
        end do
        call test_assert_equal_int(s, NWRITERS - 1, eexist_count, &
            'five deterministic race losers sync the blob parent on EEXIST')
        call immutable_store_put_blob(cache, trim(source_path), ref_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'concurrent publisher returns the raw-byte ID')
        call immutable_store_verify_blob(cache, ref_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'concurrently published blob is complete and valid')
    end subroutine test_concurrent_publish

    subroutine test_mkdir_race_sync(s, base)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: base
        character(len=512) :: target, ready, release, existing, child_exe
        character(len=8192) :: command
        character(len=8192) :: worker_argv(3)
        character(len=17) :: observed
        integer :: child_status, local_err
        logical :: exists

        target = trim(base)//'/mkdir-race/shared/leaf'
        ready = trim(base)//'/mkdir-ready'
        release = trim(base)//'/mkdir-release'
        existing = trim(base)//'/mkdir-existing-sync'
        call get_command_argument(0, child_exe)
        command = quote(trim(child_exe))//' --mkdir-barrier '// &
            quote(trim(target))//' '//quote(trim(ready))//' '// &
            quote(trim(release))//' '//quote('')//' & creator=$!; '
        command = trim(command)//wait_for_marker(trim(ready))
        command = trim(command)//quote(trim(child_exe))// &
            ' --mkdir-barrier '//quote(trim(target))//' '//quote('')//' '// &
            quote('')//' '// &
            quote(trim(existing))//'; status=$?; '
        command = trim(command)//'touch '//quote(trim(release))//'; '
        command = trim(command)//'wait $creator; creator_status=$?; '
        command = trim(command)//'[ $status -eq 0 ] && [ $creator_status -eq 0 ]'
        worker_argv(1) = 'sh'
        worker_argv(2) = '-c'
        worker_argv(3) = trim(command)
        call proc_exec_silent(worker_argv, 3, child_status)
        call test_assert_equal_int(s, 0, child_status, &
            'concurrent existing directory use completes during creator pause')
        inquire(file=trim(existing), exist=exists)
        call test_assert(s, exists, &
            'existing shard user records its parent fsync before success')
        call read_line(trim(existing), observed, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'existing-directory durability receipt can be read')
        call test_assert_equal_str(s, 'EXIST_PARENT_SYNC', observed, &
            'EEXIST observer synced the still-uncommitted parent entry')
    end subroutine test_mkdir_race_sync

    subroutine test_materialization_modes(s, cache, base, blob_id)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base, blob_id
        type(immutable_tree_entry_t) :: empty(0), directories(3)
        character(len=512) :: file_path(3), tree_path, child_path
        character(len=64) :: empty_id, directory_tree_id
        character(len=12) :: names(3) = [character(len=12) :: &
            'mode-0444', 'mode-0555', 'mode-0000']
        integer :: modes(3) = [292, 365, 0]
        integer :: i, local_err, c_err, actual_mode
        logical :: cloned
        character(kind=c_char), allocatable :: c_path(:)

        do i = 1, 3
            write(file_path(i), '(A,A,I0,A)') trim(base), '/mode-file-', i, '.bin'
            call immutable_store_materialize_blob(cache, blob_id, &
                trim(file_path(i)), modes(i), IMMUTABLE_MATERIALIZE_COPY, &
                cloned, local_err)
            call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
                'file materialization supports restrictive mode')
            c_path = c_string(trim(file_path(i)))
            c_err = c_path_mode(c_path, actual_mode)
            call test_assert_equal_int(s, 0, int(c_err), &
                'materialized file mode can be inspected without opening it')
            call test_assert_equal_int(s, modes(i), actual_mode, &
                'file retains the requested mode including 000')
        end do

        call immutable_store_put_tree(cache, empty, empty_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'empty child tree is published for directory mode coverage')
        do i = 1, 3
            directories(i)%path = trim(names(i))
            directories(i)%role = 'source'
            directories(i)%object_id = empty_id
            directories(i)%mode = modes(i)
            directories(i)%kind = IMMUTABLE_TREE
        end do
        call immutable_store_put_tree(cache, directories, directory_tree_id, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'mode-bearing directory tree publishes')
        tree_path = trim(base)//'/mode-directory-tree'
        call immutable_store_materialize_tree(cache, directory_tree_id, &
            trim(tree_path), IMMUTABLE_MATERIALIZE_COPY, cloned, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'directory materialization supports restrictive modes')
        do i = 1, 3
            child_path = trim(tree_path)//'/'//trim(names(i))
            c_path = c_string(trim(child_path))
            c_err = c_path_mode(c_path, actual_mode)
            call test_assert_equal_int(s, 0, int(c_err), &
                'materialized directory mode can be read through parent')
            call test_assert_equal_int(s, modes(i), actual_mode, &
                'directory retains the requested mode including 000')
        end do
    end subroutine test_materialization_modes

    subroutine test_tree_eexist_sync(s, cache, tree_id, base)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: tree_id, base
        character(len=512) :: empty_dir, marker, destination, mkdir_argv(2)
        character(len=6) :: observed
        integer(c_int) :: c_err
        integer :: local_err
        logical :: exists
        character(kind=c_char), allocatable :: c_temp(:), c_dest(:)

        destination = immutable_store_tree_path(cache, tree_id)
        empty_dir = trim(path_dirname(destination))//'/eexist-test-dir'
        marker = trim(base)//'/tree-eexist-sync'
        mkdir_argv(1) = 'mkdir'
        mkdir_argv(2) = trim(empty_dir)
        call proc_exec_silent(mkdir_argv, 2, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'tree EEXIST contender owns a private temporary directory')
        call configure_test_hooks('', '', '', '', trim(marker))
        c_temp = c_string(trim(empty_dir))
        c_dest = c_string(trim(destination))
        c_err = c_publish_tree(c_temp, c_dest)
        call test_assert_equal_int(s, 1, int(c_err), &
            'tree no-replace publication reports the existing winner')
        inquire(file=trim(marker), exist=exists)
        call test_assert(s, exists, &
            'tree EEXIST path records success only after syncing its parent')
        call read_first(trim(marker), observed, local_err)
        call test_assert_equal_str(s, 'EEXIST', observed, &
            'tree race loser establishes destination parent durability')
    end subroutine test_tree_eexist_sync

    subroutine test_symlink_shard_rejected(s, base)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: base
        type(immutable_store_t) :: external, linked
        character(len=512) :: external_root, linked_root, source, new_source
        character(len=512) :: link_target, link_path, mkdir_path
        character(len=512) :: link_argv(4)
        character(len=64) :: sentinel_id, attempted_id
        integer(c_int) :: c_err
        integer :: local_err
        logical :: exists
        character(kind=c_char), allocatable :: c_mkdir_path(:)

        external_root = trim(base)//'/symlink-external'
        linked_root = trim(base)//'/symlink-linked'
        source = trim(base)//'/symlink-sentinel.bin'
        new_source = trim(base)//'/symlink-new.bin'
        call immutable_store_init(external, trim(external_root), local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'external sentinel store initializes')
        call write_text(trim(source), 'sentinel')
        call immutable_store_put_blob(external, trim(source), sentinel_id, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'external store publishes sentinel object')
        call immutable_store_init(linked, trim(linked_root), local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'symlink test store initializes')
        link_target = trim(external_root)//'/blobs'
        link_path = trim(linked_root)//'/blobs'
        link_argv(1) = 'ln'
        link_argv(2) = '-s'
        link_argv(3) = trim(link_target)
        link_argv(4) = trim(link_path)
        call proc_exec_silent(link_argv, 4, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'store shard path is replaced by external symlink')
        call immutable_store_verify_blob(linked, sentinel_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'verification rejects a symlinked shard parent')
        call immutable_store_put_blob(linked, trim(source), attempted_id, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'put rejects a symlinked shard before trusting or replacing it')
        call write_text(trim(new_source), 'must-stay-outside')
        call immutable_store_put_blob(linked, trim(new_source), attempted_id, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'put cannot publish a new object through an external shard symlink')
        mkdir_path = trim(linked_root)//'/blobs/escape'
        c_mkdir_path = c_string(trim(mkdir_path))
        c_err = c_mkdirs(c_mkdir_path)
        call test_assert(s, c_err /= 0_c_int, &
            'mkdir traversal refuses every symlinked parent component')
        call immutable_store_verify_blob(external, sentinel_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'external sentinel remains valid after rejected store operations')
        inquire(file=trim(external_root)//'/blobs/escape', exist=exists)
        call test_assert(s, .not. exists, &
            'rejected mkdir did not create an external path')
    end subroutine test_symlink_shard_rejected

    subroutine test_manifest_encoding(s, blob_id)
        type(test_suite_t), intent(inout) :: s
        character(len=*), intent(in) :: blob_id
        type(immutable_tree_entry_t) :: entry(1)
        character(len=:), allocatable :: encoded, variant, expected

        entry(1)%path = 'sample.bin'
        entry(1)%role = 'source'
        entry(1)%object_id = blob_id
        entry(1)%mode = 420
        entry(1)%kind = IMMUTABLE_BLOB
        encoded = immutable_manifest_serialize(entry)
        expected = 'FXTREE1'//achar(10)//'B'//achar(9)//'644'//achar(9)// &
            'source'//achar(9)//'sample.bin'//achar(9)//trim(blob_id)//achar(10)
        call test_assert_equal_str(s, expected, encoded, &
            'canonical bytes bind schema, type, mode, role, path, and ID')

        variant = 'FXTREE2'//encoded(8:)
        call test_assert(s, sha256_string(encoded) /= sha256_string(variant), &
            'schema version participates in tree identity')
        entry(1)%path = 'sample-alt.bin'
        call test_assert(s, immutable_manifest_serialize(entry) /= encoded, &
            'normalized path participates in tree identity')
        entry(1)%path = 'sample.bin'
        entry(1)%role = 'build'
        call test_assert(s, immutable_manifest_serialize(entry) /= encoded, &
            'role participates in tree identity')
        entry(1)%role = 'source'
        entry(1)%mode = 493
        call test_assert(s, immutable_manifest_serialize(entry) /= encoded, &
            'executable mode participates in tree identity')
        entry(1)%mode = 420
        entry(1)%kind = IMMUTABLE_TREE
        call test_assert(s, immutable_manifest_serialize(entry) /= encoded, &
            'entry type participates in tree identity')
    end subroutine test_manifest_encoding

    subroutine test_role_and_tree_manifests(s, cache, blob_id, root_id, sub_id)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: blob_id
        character(len=64), intent(out) :: root_id
        character(len=64), intent(out) :: sub_id
        type(immutable_tree_entry_t) :: sub(1), top(3), reversed(3), invalid(1)
        character(len=64) :: reversed_id, ignored_id
        integer :: local_err

        sub(1)%path = 'nested.bin'
        sub(1)%role = 'build'
        sub(1)%object_id = blob_id
        sub(1)%mode = 420
        sub(1)%kind = IMMUTABLE_BLOB
        call immutable_store_put_tree(cache, sub, sub_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'subtree manifest publishes')

        top(1)%path = 'source.bin'
        top(1)%role = 'source'
        top(1)%object_id = blob_id
        top(1)%mode = 420
        top(1)%kind = IMMUTABLE_BLOB
        top(2)%path = 'generated.bin'
        top(2)%role = 'build'
        top(2)%object_id = blob_id
        top(2)%mode = 493
        top(2)%kind = IMMUTABLE_BLOB
        top(3)%path = 'sub'
        top(3)%role = 'source'
        top(3)%object_id = sub_id
        top(3)%mode = 493
        top(3)%kind = IMMUTABLE_TREE
        call immutable_store_put_tree(cache, top, root_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'root manifest publishes after referenced objects')
        reversed(1) = top(3)
        reversed(2) = top(1)
        reversed(3) = top(2)
        call immutable_store_put_tree(cache, reversed, reversed_id, local_err)
        call test_assert_equal_str(s, trim(root_id), trim(reversed_id), &
            'canonical tree ID ignores input order')
        call immutable_store_verify_tree(cache, root_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'tree recursively verifies child blobs and subtrees')
        call test_assert(s, top(1)%object_id == top(2)%object_id .and. &
            top(1)%role /= top(2)%role, &
            'different roles share one raw-byte blob identity')
        invalid(1) = top(1)
        invalid(1)%path = '../escape'
        call immutable_store_put_tree(cache, invalid, ignored_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_INVALID, local_err, &
            'tree paths reject traversal instead of normalizing through it')
    end subroutine test_role_and_tree_manifests

    subroutine test_materialization(s, cache, base, blob_id, root_id)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base, blob_id, root_id
        character(len=512) :: blob_path, other_path, mat_path, tree_path
        integer :: local_err
        logical :: cloned, exists
        character(len=5) :: changed, expected
        integer(int64) :: bytes, mtime, inode

        copy_path = trim(base)//'/materialized-copy.bin'
        call immutable_store_materialize_blob(cache, blob_id, trim(copy_path), &
            420, IMMUTABLE_MATERIALIZE_COPY, cloned, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'forced byte-copy materialization succeeds')
        call test_assert(s, .not. cloned, 'forced byte-copy reports no clone')
        clone_path = trim(base)//'/materialized-clone.bin'
        call immutable_store_materialize_blob(cache, blob_id, trim(clone_path), &
            420, IMMUTABLE_MATERIALIZE_CLONE, cloned, local_err)
        call test_assert(s, local_err == IMMUTABLE_OK .or. &
            local_err == IMMUTABLE_UNSUPPORTED, &
            'clone path either succeeds or reports unsupported filesystem')
        if (local_err == IMMUTABLE_OK) then
            call test_assert(s, cloned, 'clone-required path reports clone use')
            call overwrite_file(trim(clone_path), 'edited', local_err)
            call test_assert_equal_int(s, 0, local_err, &
                'writable clone materialization can be edited')
            call immutable_store_verify_blob(cache, blob_id, local_err)
            call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
                'editing a clone leaves its immutable source valid')
        else
            inquire(file=trim(clone_path), exist=exists)
            call test_assert(s, .not. exists, &
                'unsupported clone leaves no published destination')
        end if

        fallback_path = trim(base)//'/materialized-auto.bin'
        call immutable_store_materialize_blob(cache, blob_id, &
            trim(fallback_path), 420, IMMUTABLE_MATERIALIZE_AUTO, cloned, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'automatic clone-or-copy materialization succeeds')
        expected = transfer([char(0), 'A', char(255), 'z', char(10)], &
            expected)
        call overwrite_file(trim(base)//'/source.bin', 'edited', local_err)
        call test_assert_equal_int(s, 0, local_err, 'source file is edited')
        call overwrite_file(trim(copy_path), 'other', local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'writable materialization can be edited')
        call immutable_store_verify_blob(cache, blob_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'editing materialization leaves immutable blob valid')
        call read_first(trim(fallback_path), changed, local_err)
        call test_assert(s, changed == expected, &
            'separate materialization remains unchanged')

        tree_path = trim(base)//'/materialized-tree'
        call immutable_store_materialize_tree(cache, root_id, trim(tree_path), &
            IMMUTABLE_MATERIALIZE_COPY, cloned, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'nested tree materializes transactionally')
        mat_path = trim(tree_path)//'/source.bin'
        call read_first(trim(mat_path), changed, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'materialized tree contains source blob')
        other_path = trim(tree_path)//'/sub/nested.bin'
        call read_first(trim(other_path), changed, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'materialized tree contains nested subtree blob')
        call immutable_store_file_info(cache, blob_id, bytes, mtime, inode, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'blob remains verifiable after materialization')
        call test_assert(s, bytes > 0_int64, &
            'blob metadata survives materialization')
    end subroutine test_materialization

    subroutine test_killed_partial_is_hidden(s, cache, base)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base
        character(len=512) :: path, temp_path, source_path, ready, release
        character(len=512) :: child_exe
        character(len=8192) :: child_argv(3), command
        character(len=1) :: bytes(262144)
        character(len=64) :: missing_id
        integer :: local_err, child_status, file_err, i
        integer(int64) :: partial_size
        logical :: exists
        character(len=8192) :: marker_path

        do i = 1, size(bytes)
            bytes(i) = char(mod(i + 17, 251))
        end do
        source_path = trim(base)//'/killed-source.bin'
        call write_bytes(trim(source_path), bytes, file_err)
        call test_assert_equal_int(s, 0, file_err, &
            'large crash fixture is written')
        missing_id = sha256_string(transfer(bytes, repeat(' ', size(bytes))))
        path = immutable_store_blob_path(cache, missing_id)
        ready = trim(base)//'/killed-copy-ready'
        release = trim(base)//'/killed-copy-release-never'
        call get_command_argument(0, executable)
        command = quote(trim(executable))//' --put-blob-barrier '// &
            quote(trim(cache%root_dir))//' '//quote(trim(source_path))//' '// &
            quote(trim(ready))//' '//quote(trim(release))//' '// &
            quote('')//' '//quote('')//' '//quote('')// &
            ' & writer=$!; '
        command = trim(command)//wait_for_marker(trim(ready))
        command = trim(command)//'kill -9 $writer; wait $writer 2>/dev/null; '// &
            'status=$?; [ $status -ge 128 ]'
        child_argv(1) = 'sh'
        child_argv(2) = '-c'
        child_argv(3) = trim(command)
        call proc_exec_silent(child_argv, 3, child_status)
        call test_assert_equal_int(s, 0, child_status, &
            'real immutable_store_put_blob worker is killed at partial temp write')
        call read_line(trim(ready), marker_path, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'copy barrier identifies the owned temporary file')
        temp_path = trim(marker_path)
        inquire(file=trim(path), exist=exists)
        call test_assert(s, .not. exists, &
            'killed partial writer exposes no content-addressed blob')
        partial_size = -1_int64
        inquire(file=trim(temp_path), exist=exists, size=partial_size)
        call test_assert(s, exists .and. partial_size > 0_int64 .and. &
            partial_size < size(bytes, kind=int64), &
            'killed writer leaves a genuine incomplete owned temp, never final')
        call immutable_store_verify_blob(cache, missing_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_MISSING, local_err, &
            'killed publication remains missing, never a valid partial')
    end subroutine test_killed_partial_is_hidden

    subroutine test_corrupt_tree_is_reported(s, cache, blob_id, root_id, sub_id)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: blob_id, root_id, sub_id
        type(immutable_tree_entry_t) :: entries(3)
        character(len=512) :: tree_path, chmod_argv(3)
        character(len=64) :: rejected_id
        character(len=7) :: observed
        integer :: local_err, chmod_status
        logical :: exists

        entries(1)%path = 'source.bin'
        entries(1)%role = 'source'
        entries(1)%object_id = blob_id
        entries(1)%mode = 420
        entries(1)%kind = IMMUTABLE_BLOB
        entries(2)%path = 'generated.bin'
        entries(2)%role = 'build'
        entries(2)%object_id = blob_id
        entries(2)%mode = 493
        entries(2)%kind = IMMUTABLE_BLOB
        entries(3)%path = 'sub'
        entries(3)%role = 'source'
        entries(3)%object_id = sub_id
        entries(3)%mode = 493
        entries(3)%kind = IMMUTABLE_TREE
        tree_path = immutable_store_tree_path(cache, root_id)
        chmod_argv(1) = 'chmod'
        chmod_argv(2) = 'u+w'
        chmod_argv(3) = trim(tree_path)
        call proc_exec_silent(chmod_argv, 3, chmod_status)
        call test_assert_equal_int(s, 0, chmod_status, &
            'test makes stored tree writable for corruption injection')
        call overwrite_file(trim(tree_path), 'CORRUPT', local_err)
        call test_assert_equal_int(s, 0, local_err, 'tree bytes are corrupted')
        call immutable_store_verify_tree(cache, root_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'corrupt tree manifest never verifies')
        call immutable_store_put_tree(cache, entries, rejected_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'put rejects an established corrupt tree without replacing it')
        inquire(file=trim(tree_path), exist=exists)
        call test_assert(s, exists, 'corrupt tree remains at its digest path')
        call read_first(trim(tree_path), observed, local_err)
        call test_assert_equal_str(s, 'CORRUPT', observed, &
            'rejected tree publication preserves corrupt evidence')
    end subroutine test_corrupt_tree_is_reported

    subroutine test_corruption_is_reported(s, cache, base)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base
        character(len=512) :: source_path, blob_path
        character(len=512) :: chmod_argv(3)
        character(len=64) :: id
        character(len=6) :: observed
        integer :: local_err, chmod_status, unit, ios

        source_path = trim(base)//'/corrupt-source.bin'
        call write_text(trim(source_path), 'sound')
        call immutable_store_put_blob(cache, trim(source_path), id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'corruption fixture publishes')
        blob_path = immutable_store_blob_path(cache, id)
        chmod_argv(1) = 'chmod'
        chmod_argv(2) = 'u+w'
        chmod_argv(3) = trim(blob_path)
        call proc_exec_silent(chmod_argv, 3, chmod_status)
        call test_assert_equal_int(s, 0, chmod_status, &
            'test makes stored file writable for mode verification')
        call immutable_store_verify_blob(cache, id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'writable object is rejected as non-immutable even with valid bytes')
        chmod_argv(2) = '444'
        call proc_exec_silent(chmod_argv, 3, chmod_status)
        call test_assert_equal_int(s, 0, chmod_status, &
            'test restores immutable file permissions')
        call immutable_store_verify_blob(cache, id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'read-only object verifies before corruption')
        chmod_argv(2) = 'u+w'
        call proc_exec_silent(chmod_argv, 3, chmod_status)
        call test_assert_equal_int(s, 0, chmod_status, &
            'test reopens payload for corruption injection')
        open(newunit=unit, file=trim(blob_path), status='old', access='stream', &
            form='unformatted', action='write', position='rewind', iostat=ios)
        if (ios == 0) then
            write(unit, iostat=ios) 'faulty'
            close(unit)
        end if
        call test_assert_equal_int(s, 0, ios, 'stored payload is corrupted')
        call immutable_store_verify_blob(cache, id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'corrupt immutable object never verifies')
        call immutable_store_put_blob(cache, trim(source_path), crash_id, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_CORRUPT, local_err, &
            'publication reports corruption without replacing established ID')
        call read_first(trim(blob_path), observed, local_err)
        call test_assert_equal_str(s, 'faulty', observed, &
            'rejected blob publication preserves corrupt evidence')
    end subroutine test_corruption_is_reported

    subroutine run_worker_put()
        character(len=4096) :: store_root, source_path
        type(immutable_store_t) :: worker_store
        character(len=64) :: id
        integer :: local_err
        call get_command_argument(2, store_root)
        call get_command_argument(3, source_path)
        call immutable_store_init(worker_store, trim(store_root), local_err)
        if (local_err /= IMMUTABLE_OK) stop 10
        call immutable_store_put_blob(worker_store, trim(source_path), id, &
            local_err)
        if (local_err /= IMMUTABLE_OK) stop 11
    end subroutine run_worker_put

    subroutine run_worker_put_barrier()
        character(len=4096) :: ready, copy_release, publish_ready
        character(len=4096) :: publish_release, eexist
        call get_command_argument(4, ready)
        call get_command_argument(5, copy_release)
        call get_command_argument(6, publish_ready)
        call get_command_argument(7, publish_release)
        call get_command_argument(8, eexist)
        call configure_test_hooks(trim(ready), trim(copy_release), &
            trim(publish_ready), trim(publish_release), trim(eexist))
        call run_worker_put()
    end subroutine run_worker_put_barrier

    subroutine run_mkdir_worker()
        character(len=4096) :: path, ready, release, existing
        character(kind=c_char), allocatable :: c_path(:), c_ready(:)
        character(kind=c_char), allocatable :: c_release(:), c_existing(:)
        integer(c_int) :: status
        call get_command_argument(2, path)
        call get_command_argument(3, ready)
        call get_command_argument(4, release)
        call get_command_argument(5, existing)
        c_path = c_string(trim(path))
        c_ready = c_string(trim(ready))
        c_release = c_string(trim(release))
        c_existing = c_string(trim(existing))
        call c_test_mkdir_configure(c_path, c_ready, c_release, c_existing)
        status = c_mkdirs(c_path)
        if (status /= 0_c_int) stop 13
    end subroutine run_mkdir_worker

    subroutine configure_test_hooks(ready, copy_release, publish_ready, &
            publish_release, eexist)
        character(len=*), intent(in) :: ready, copy_release, publish_ready
        character(len=*), intent(in) :: publish_release, eexist
        character(kind=c_char), allocatable :: c_ready(:), c_copy_release(:)
        character(kind=c_char), allocatable :: c_publish_ready(:)
        character(kind=c_char), allocatable :: c_publish_release(:), c_eexist(:)
        c_ready = c_string(ready)
        c_copy_release = c_string(copy_release)
        c_publish_ready = c_string(publish_ready)
        c_publish_release = c_string(publish_release)
        c_eexist = c_string(eexist)
        call c_test_configure(c_ready, c_copy_release, c_publish_ready, &
            c_publish_release, c_eexist)
    end subroutine configure_test_hooks

    subroutine write_bytes(path, bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=1), intent(in) :: bytes(:)
        integer, intent(out) :: ierr
        integer :: unit
        open(newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ierr)
        if (ierr /= 0) return
        write(unit, iostat=ierr) bytes
        close(unit)
    end subroutine write_bytes

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: ierr
        integer :: unit
        open(newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=ierr)
        if (ierr /= 0) return
        write(unit, iostat=ierr) text
        close(unit)
    end subroutine write_text

    subroutine overwrite_file(path, text, ierr)
        character(len=*), intent(in) :: path, text
        integer, intent(out) :: ierr
        integer :: unit
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='write', position='rewind', iostat=ierr)
        if (ierr /= 0) return
        write(unit, iostat=ierr) text
        close(unit)
    end subroutine overwrite_file

    subroutine read_first(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text
        integer, intent(out) :: ierr
        integer :: unit
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=ierr)
        if (ierr /= 0) return
        read(unit, iostat=ierr) text
        close(unit)
    end subroutine read_first

    subroutine read_line(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: text
        integer, intent(out) :: ierr
        integer :: unit
        text = ''
        open(newunit=unit, file=path, status='old', access='stream', &
            form='formatted', action='read', iostat=ierr)
        if (ierr /= 0) return
        read(unit, '(A)', iostat=ierr) text
        close(unit)
    end subroutine read_line

    function c_string(text) result(c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable :: c_text(:)
        integer :: i, n
        n = len_trim(text)
        allocate(c_text(n + 1))
        do i = 1, n
            c_text(i) = text(i:i)
        end do
        c_text(n + 1) = c_null_char
    end function c_string

    subroutine drain_events(fd)
        integer, intent(in) :: fd
        character(len=512) :: path
        integer :: event, poll_err
        logical :: got
        do
            call proc_watch_poll(fd, path, event, 0, got, poll_err)
            if (poll_err /= 0 .or. .not. got) exit
        end do
    end subroutine drain_events

    function quote(text) result(quoted)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: quoted
        quoted = "'"//trim(text)//"'"
    end function quote

    function wait_for_marker(path) result(command_text)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: command_text
        command_text = 'i=0; while [ ! -f '//quote(path)//' ]; do '// &
            'i=$((i+1)); [ $i -lt 500 ] || exit 91; sleep 0.01; done; '
    end function wait_for_marker

end program test_immutable_store
