program test_immutable_gc
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_remove_tree, fx_test_mkdir_p
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_tree_entry_t, IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_BLOB, immutable_store_init, immutable_store_put_blob, &
        immutable_store_blob_path, immutable_store_tree_path, &
        immutable_store_root_set, immutable_store_reason_release, &
        immutable_store_publication_lease_acquire, &
        immutable_store_read_lease_acquire, immutable_store_lease_release
    use fx_immutable_tree, only: immutable_store_put_tree
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_publish, &
        action_result_action_key, ACTION_RESULT_OK, ACTION_RESULT_CONFLICT
    use fx_immutable_gc, only: immutable_store_collect
    implicit none

    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(action_result_store_t) :: actions
    type(immutable_lease_t) :: publishing, reading
    type(immutable_tree_entry_t) :: entry(1)
    character(len=512) :: root, source, record
    character(len=64) :: root_blob, orphan_blob, extra_orphan, &
        pending_blob, read_blob
    character(len=64) :: tree_id, bound_id, action_key
    character(len=64) :: conflict_a, conflict_b, conflict_blob_a, conflict_blob_b
    character(len=64) :: malformed_blob
    character(len=64) :: ids(1), group_ids(8)
    character(len=4) :: kinds(1), group_kinds(8)
    integer :: ierr, scanned, deleted, unit, i
    character(len=32) :: group_payload
    integer(int64) :: allocated, reclaimed
    logical :: exists

    call test_suite_init(suite, 'fx_immutable_gc')
    write(root, '(A,I0)') '/var/tmp/fx_gc_', proc_pid()
    call immutable_store_init(store, trim(root), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')
    call action_result_store_init(actions, trim(root), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'action store initializes')
    source = trim(root)//'/source'

    call put_text('rooted', root_blob)
    call put_text('orphan', orphan_blob)
    call put_text('extra-orphan', extra_orphan)
    call put_text('pending', pending_blob)
    call put_text('reading', read_blob)

    entry(1)%path = 'output'
    entry(1)%role = 'object'
    entry(1)%kind = IMMUTABLE_BLOB
    entry(1)%mode = 420
    entry(1)%object_id = root_blob
    call immutable_store_put_tree(store, entry, tree_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'tree publishes')
    kinds(1) = 'tree'
    ids(1) = tree_id
    call immutable_store_root_set(store, 'owner', 'version1', 'result', &
        kinds, ids, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'tree root registers')
    call immutable_store_root_set(store, 'owner', 'version2', 'result', &
        kinds, ids, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'second worktree version root registers')

    kinds(1) = 'blob'
    ids(1) = pending_blob
    call immutable_store_publication_lease_acquire(store, 'writer', 'version1', &
        'pending', kinds, ids, publishing, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'publication lease registers')
    call immutable_store_read_lease_acquire(store, 'reader', 'version1', &
        'restore', 'blob', read_blob, reading, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'read lease registers')

    call collect(1, 1, 0_int64, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_IO_ERROR, ierr, &
        'incomplete inventory fails closed')
    call exists_blob(orphan_blob, .true., 'scan cap preserves orphan')
    call collect(100, 10, huge(0_int64)/2, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'age floor accepted')
    call test_assert_equal_int(suite, 0, deleted, 'age floor prevents deletion')
    call collect(100, 10, 0_int64, huge(0_int64)/2, 100)
    call test_assert_equal_int(suite, 0, deleted, &
        'both pressure thresholds prevent deletion')
    call test_assert(suite, allocated > 0_int64 .and. scanned >= 5, &
        'allocated bytes and object inodes are reported')
    call collect(100, 1, 0_int64, huge(0_int64)/2, 0)
    call test_assert_equal_int(suite, 1, deleted, &
        'inode pressure collects even below byte threshold')
    call collect(100, 1, 0_int64, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'bounded collection succeeds')
    call test_assert_equal_int(suite, 1, deleted, 'delete count is capped')
    call exists_blob(root_blob, .true., 'tree child survives')
    call exists_blob(pending_blob, .true., 'publication lease survives')
    call exists_blob(read_blob, .true., 'read lease survives')
    call exists_tree(tree_id, .true., 'durable root survives')

    call collect(100, 10, 0_int64, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'second bounded collection succeeds')
    call exists_blob(orphan_blob, .false., 'orphan blob is collected')
    call exists_blob(extra_orphan, .false., 'second orphan is collected')
    call exists_blob(root_blob, .true., 'rooted blob survives sweep')
    call immutable_store_lease_release(store, publishing, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'publication lease releases')
    call immutable_store_lease_release(store, reading, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'read lease releases')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_blob(pending_blob, .false., 'released publication is collected')
    call exists_blob(read_blob, .false., 'released read object is collected')

    call immutable_store_reason_release(store, 'owner', 'version1', &
        'result', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'first worktree version releases')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_tree(tree_id, .true., 'second version retains tree')
    call exists_blob(root_blob, .true., 'second version retains blob')

    call action_result_publish(actions, 'retained-action', entry, bound_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
        'action result binds')
    call immutable_store_reason_release(store, 'owner', 'version2', &
        'result', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'second worktree version releases')
    call immutable_store_reason_release(store, &
        action_result_action_key('retained-action'), 'fx-action-v1', &
        'bound', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'fixture removes durable action reason')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_tree(bound_id, .true., 'action record retains result tree')
    call exists_blob(root_blob, .true., 'action record retains result blob')

    action_key = action_result_action_key('retained-action')
    record = trim(root)//'/actions/sha256/'//action_key(1:2)//'/'//action_key
    open(newunit=unit, file=trim(record), status='replace', iostat=ierr)
    call test_assert_equal_int(suite, 0, ierr, 'binding fixture opens')
    if (ierr == 0) then
        write(unit, '(A)') 'FXACTION2'
        write(unit, '(A)') action_key
        write(unit, '(A)') 'RETIRED'
        close(unit, iostat=ierr)
    end if
    call test_assert_equal_int(suite, 0, ierr, &
        'retired marker records no result graph')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_tree(bound_id, .false., 'retired result tree is collected')
    call exists_blob(root_blob, .false., 'retired result blob is collected')

    call put_text('conflict-first', conflict_blob_a)
    entry(1)%object_id = conflict_blob_a
    call action_result_publish(actions, 'conflicting-action', entry, &
        conflict_a, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
        'first conflicting result binds')
    call put_text('conflict-second', conflict_blob_b)
    entry(1)%object_id = conflict_blob_b
    call action_result_publish(actions, 'conflicting-action', entry, &
        conflict_b, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_CONFLICT, ierr, &
        'second result records conflict')
    action_key = action_result_action_key('conflicting-action')
    call immutable_store_reason_release(store, action_key, 'fx-action-v1', &
        'conflict', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'fixture releases conflict reason')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_tree(conflict_a, .true., 'conflict record retains first tree')
    call exists_tree(conflict_b, .true., 'conflict record retains second tree')
    call exists_blob(conflict_blob_a, .true., &
        'conflict record retains first blob')
    call exists_blob(conflict_blob_b, .true., &
        'conflict record retains second blob')

    call put_text('malformed-orphan', malformed_blob)
    action_key = action_result_action_key('malformed-action')
    record = trim(root)//'/actions/sha256/'//action_key(1:2)//'/'//action_key
    ierr = fx_test_mkdir_p(trim(root)//'/actions/sha256/'//action_key(1:2))
    call test_assert_equal_int(suite, 0, ierr, &
        'malformed record directory is prepared')
    open(newunit=unit, file=trim(record), status='replace', iostat=ierr)
    if (ierr == 0) then
        write(unit, '(A)') 'bad action record'
        close(unit, iostat=ierr)
    end if
    call test_assert_equal_int(suite, 0, ierr, 'malformed record is written')
    call collect(100, 10, 0_int64, huge(0_int64)/2, 100)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'under-budget inventory need not inspect malformed action')
    call test_assert_equal_int(suite, 0, deleted, &
        'under-budget inventory deletes nothing')
    call exists_blob(malformed_blob, .true., &
        'under-budget orphan remains available')
    call collect(100, 10, 0_int64, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_IO_ERROR, ierr, &
        'malformed record stops collection')
    call exists_blob(malformed_blob, .true., &
        'malformed record preserves otherwise orphaned blob')
    open(newunit=unit, file=trim(record), status='old', iostat=ierr)
    if (ierr == 0) close(unit, status='delete', iostat=ierr)
    call test_assert_equal_int(suite, 0, ierr, &
        'malformed fixture is removed')
    call collect(100, 10, 0_int64, 0_int64)
    call exists_blob(malformed_blob, .false., &
        'orphan becomes collectible after repair')

    group_kinds = 'blob'
    do i = 1, 8
        write(group_payload, '(A,I0)') 'compact-member-', i
        call put_text(trim(group_payload), group_ids(i))
    end do
    call immutable_store_root_set(store, 'compact-owner', 'version1', &
        'group', group_kinds, group_ids, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'eight-object root group registers')
    call assert_compact_group()
    call collect(100, 100, 0_int64, 0_int64)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'compact lease snapshot is parsed')
    do i = 1, 8
        call exists_blob(group_ids(i), .true., 'compact group member survives')
    end do
    call immutable_store_reason_release(store, 'compact-owner', 'version1', &
        'group', ierr)
    call collect(100, 100, 0_int64, 0_int64)
    do i = 1, 8
        call exists_blob(group_ids(i), .false., &
            'released compact group member is collected')
    end do

    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'fixture tree removed')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine assert_compact_group()
        character(len=512) :: line
        integer :: fd, status
        logical :: found
        found = .false.
        open(newunit=fd, file=trim(root)//'/.fx-metadata/leases', &
            status='old', action='read', iostat=status)
        if (status == 0) then
            do
                read(fd, '(A)', iostat=status) line
                if (status /= 0) exit
                if (index(line, 'G|R|') == 1) found = .true.
            end do
            close(fd)
        end if
        call test_assert(suite, found, 'lease snapshot uses compact group')
    end subroutine assert_compact_group

    subroutine put_text(payload, id)
        character(len=*), intent(in) :: payload
        character(len=64), intent(out) :: id
        integer :: status, fd
        open(newunit=fd, file=trim(source), status='replace', &
            access='stream', form='unformatted', iostat=status)
        if (status == 0) then
            write(fd) payload
            close(fd)
            call immutable_store_put_blob(store, trim(source), id, status)
        end if
        call test_assert_equal_int(suite, IMMUTABLE_OK, status, &
            'fixture blob publishes')
    end subroutine put_text

    subroutine collect(scan, limit, age, pressure, objects)
        integer, intent(in) :: scan, limit
        integer(int64), intent(in) :: age, pressure
        integer, intent(in), optional :: objects
        integer :: object_limit
        object_limit = 0
        if (present(objects)) object_limit = objects
        call immutable_store_collect(store, scan, limit, age, pressure, &
            object_limit, scanned, allocated, deleted, reclaimed, ierr)
    end subroutine collect

    subroutine exists_blob(id, expected, label)
        character(len=*), intent(in) :: id, label
        logical, intent(in) :: expected
        inquire(file=immutable_store_blob_path(store, id), exist=exists)
        call test_assert(suite, exists .eqv. expected, label)
    end subroutine exists_blob

    subroutine exists_tree(id, expected, label)
        character(len=*), intent(in) :: id, label
        logical, intent(in) :: expected
        inquire(file=immutable_store_tree_path(store, id), exist=exists)
        call test_assert(suite, exists .eqv. expected, label)
    end subroutine exists_tree
end program test_immutable_gc
