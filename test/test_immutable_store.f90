program test_immutable_store
    use, intrinsic :: iso_fortran_env, only: int64
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
    use fx_proc, only: proc_pid, proc_kill, proc_exec_silent, &
        proc_watch_init, proc_watch_add, proc_watch_poll, proc_watch_close
    use fx_path, only: path_dirname
    implicit none

    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    character(len=64) :: object_id, tree_id, sub_id, expected_id
    character(len=64) :: crash_id
    character(len=512) :: root, source, copy_path, clone_path
    character(len=512) :: fallback_path, executable
    character(len=512) :: arg
    character(len=1) :: payload(5)
    integer :: ierr

    call get_command_argument(1, arg)
    if (trim(arg) == '--kill-before-publish') then
        call run_killed_writer()
        stop 99
    end if
    if (trim(arg) == '--put-blob') then
        call run_worker_put()
        stop 0
    end if
    call test_suite_init(suite, 'fx_immutable_store')
    write(root, '(A,I0)') '/tmp/fx_store42_', proc_pid()
    call immutable_store_init(store, trim(root), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')

    payload = [char(0), 'A', char(255), 'z', char(10)]
    source = trim(root)//'/source.bin'
    call write_bytes(trim(source), payload, ierr)
    call test_assert_equal_int(suite, 0, ierr, 'source fixture is written')
    call immutable_store_put_blob(store, trim(source), object_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'blob publishes')
    expected_id = sha256_string(transfer(payload, repeat(' ', size(payload))))
    call test_assert_equal_str(suite, trim(expected_id), trim(object_id), &
        'blob ID hashes payload bytes only')
    call immutable_store_verify_blob(store, object_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'published blob verifies')

    call test_reuse_does_not_write(suite, store, source, object_id)
    call test_concurrent_publish(suite, store, root)
    call test_manifest_encoding(suite, object_id)
    call test_role_and_tree_manifests(suite, store, object_id, tree_id, sub_id)
    call test_materialization(suite, store, root, object_id, tree_id)
    call test_killed_partial_is_hidden(suite, store)
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
        integer :: local_err, fd, wd, evt, poll_err
        logical :: event_seen

        blob_path = immutable_store_blob_path(cache, id)
        call immutable_store_file_info(cache, id, bytes0, mtime0, ino0, &
            local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'blob metadata is readable before reuse')
        fd = -1
        call proc_watch_init(fd, local_err)
        if (local_err == 0) then
            call proc_watch_add(fd, trim(path_dirname(blob_path)), 4095, wd, &
                local_err)
        end if
        call test_assert_equal_int(s, 0, local_err, 'watch monitors blob shard')
        if (fd >= 0 .and. local_err == 0) call drain_events(fd)
        call immutable_store_put_blob(cache, source_path, expected_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'identical payload reuses verified blob')
        call immutable_store_file_info(cache, id, bytes1, mtime1, ino1, &
            local_err)
        call test_assert(s, bytes0 == bytes1 .and. mtime0 == mtime1 .and. &
            ino0 == ino1, 'reuse preserves blob size, inode, and mtime')
        if (fd >= 0) then
            call proc_watch_poll(fd, watched_path, evt, 30, event_seen, poll_err)
            call test_assert(s, poll_err == 0 .and. .not. event_seen, &
                'reuse emits no file-write or directory-publication event')
            call proc_watch_close(fd, local_err)
        end if
    end subroutine test_reuse_does_not_write

    subroutine test_concurrent_publish(s, cache, base)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=*), intent(in) :: base
        integer, parameter :: NWRITERS = 6
        character(len=512) :: paths(NWRITERS)
        character(len=64) :: ref_id
        character(len=512) :: child_exe, number
        character(len=4096) :: command
        character(len=4096) :: worker_argv(3)
        character(len=1) :: bytes(2048)
        integer :: t, local_err, file_err, child_status

        bytes = char(mod([(t, t=1, size(bytes))], 251))
        ref_id = sha256_string(transfer(bytes, repeat(' ', size(bytes))))
        do t = 1, NWRITERS
            write(paths(t), '(A,A,I0,A)') trim(base), '/writer-', t, '.bin'
            call write_bytes(trim(paths(t)), bytes, file_err)
            call test_assert_equal_int(s, 0, file_err, &
                'concurrent writer input created')
        end do
        call get_command_argument(0, child_exe)
        command = 'status=0; '
        do t = 1, NWRITERS
            write(number, '(I0)') t
            command = trim(command)//quote(trim(child_exe))// &
                ' --put-blob '//quote(trim(base))//' '// &
                quote(trim(paths(t)))//' & p'//trim(number)//'=$!; '
        end do
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
            'concurrent identical publication succeeds')
        call immutable_store_put_blob(cache, trim(paths(1)), ref_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'concurrent publisher returns the raw-byte ID')
        call immutable_store_verify_blob(cache, ref_id, local_err)
        call test_assert_equal_int(s, IMMUTABLE_OK, local_err, &
            'concurrently published blob is complete and valid')
    end subroutine test_concurrent_publish

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

    subroutine test_killed_partial_is_hidden(s, cache)
        type(test_suite_t), intent(inout) :: s
        type(immutable_store_t), intent(in) :: cache
        character(len=512) :: path, temp_path
        character(len=512) :: child_argv(3), mkdir_argv(3)
        character(len=64) :: missing_id
        integer :: local_err, child_status
        integer(int64) :: partial_size
        logical :: exists
        character(len=7) :: partial

        missing_id = sha256_string('must-not-publish')
        path = immutable_store_blob_path(cache, missing_id)
        temp_path = trim(path_dirname(path))//'/.fx-tmp-interrupted'
        mkdir_argv(1) = 'mkdir'
        mkdir_argv(2) = '-p'
        mkdir_argv(3) = trim(path_dirname(path))
        call proc_exec_silent(mkdir_argv, 3, local_err)
        call test_assert_equal_int(s, 0, local_err, &
            'test creates the CAS shard before interrupted publication')
        call get_command_argument(0, executable)
        child_argv(1) = trim(executable)
        child_argv(2) = '--kill-before-publish'
        child_argv(3) = trim(temp_path)
        call proc_exec_silent(child_argv, 3, child_status)
        call test_assert(s, child_status >= 128, &
            'writer process was killed with a partial temporary')
        inquire(file=trim(path), exist=exists)
        call test_assert(s, .not. exists, &
            'killed partial writer exposes no content-addressed blob')
        partial_size = -1_int64
        inquire(file=trim(temp_path), exist=exists, size=partial_size)
        call test_assert(s, exists .and. partial_size == 7_int64, &
            'killed writer leaves only a hidden partial in the blob shard')
        call read_first(trim(temp_path), partial, local_err)
        call test_assert_equal_str(s, 'partial', partial, &
            'partial temporary bytes remain isolated from the canonical ID')
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

    subroutine run_killed_writer()
        character(len=4096) :: temp
        integer :: ierr
        call get_command_argument(2, temp)
        call write_text(trim(temp), 'partial')
        call proc_kill(proc_pid(), 9, ierr)
    end subroutine run_killed_writer

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

end program test_immutable_store
