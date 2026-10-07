program test_immutable_root_compact
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_proc, only: proc_pid
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_test_fs, only: fx_test_remove_tree
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        immutable_store_init, immutable_store_put_blob, &
        immutable_store_blob_path, immutable_store_tree_path, &
        immutable_store_root_set, immutable_store_reason_release, &
        immutable_store_graph_read_lease_acquire, &
        immutable_store_publication_lease_acquire, immutable_store_lease_release, &
        IMMUTABLE_OK
    use fx_immutable_tree, only: immutable_store_verify_tree
    use fx_immutable_gc, only: immutable_store_collect
    use fx_immutable_root_compact, only: immutable_store_compact_generation_roots
    implicit none

    interface
        integer(c_int) function c_replace(root, start, reason, ids, count, &
                tree_id, publication_start, publication_token) &
                bind(C, name='fx_immutable_lease_generation_replace')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: root(*), start(*), reason(*)
            character(kind=c_char), intent(in) :: ids(*), tree_id(*)
            integer(c_int), value :: count
            character(kind=c_char), intent(in) :: publication_start(*)
            character(kind=c_char), intent(in) :: publication_token(*)
        end function c_replace
    end interface

    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_lease_t) :: reading, publication
    character(len=512) :: root, source, snapshot, line
    character(len=64) :: ids(12), tree_a, tree_b, tree_ids(1)
    character(len=4) :: kinds(12), tree_kinds(1)
    character(len=64) :: owner_a, owner_b
    character(len=128) :: reason_a, reason_b
    character(len=780) :: expected_ids
    integer :: ierr, i, unit, scanned, compacted, deleted, scan_count
    integer :: compact_before, compact_after, compact_final
    integer(int64) :: bytes_before, bytes_after, allocated, reclaimed
    integer(c_int) :: replaced

    call test_suite_init(suite, 'fx_immutable_root_compact')
    write (root, '(a,i0)') '/var/tmp/fx_root_compact_', proc_pid()
    ierr = fx_test_remove_tree(trim(root))
    call immutable_store_init(store, trim(root), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')
    source = trim(root)//'/source'
    snapshot = trim(root)//'/.fx-metadata/leases'
    owner_a = repeat('a', 64)
    owner_b = repeat('b', 64)
    reason_a = 'generation-'//owner_a
    reason_b = 'generation-'//owner_b
    kinds = 'blob'
    do i = 1, 12
        call put_text(i, ids(i))
        expected_ids((i - 1) * 65 + 1:i * 65) = ids(i)//achar(10)
    end do
    call immutable_store_root_set(store, 'fo-generation', owner_a, &
        trim(reason_a), kinds, ids, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'first old blob group registers')
    call immutable_store_root_set(store, 'fo-generation', owner_b, &
        trim(reason_b), kinds, ids, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'second worktree owner remains independent')
    call snapshot_measure(bytes_before, compact_before)
    call test_assert_equal_int(suite, 24, compact_before, &
        'two old groups serialize 24 compact object rows')

    call immutable_store_graph_read_lease_acquire(store, 'fo-generation', &
        owner_b, 'active-reader', reading, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'active owner graph lease acquires')
    call immutable_store_compact_generation_roots(store, 1000, 2, 16, &
        scanned, compacted, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'bounded compaction completes')
    call test_assert_equal_int(suite, 1, compacted, &
        'inactive group compacts while active owner is skipped')
    call immutable_store_compact_generation_roots(store, 1000, 1, 16, &
        scanned, compacted, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'active-owner scan completes without a rewrite')
    call test_assert_equal_int(suite, 0, compacted, &
        'active owner is skipped by a fresh bounded scan')
    call tree_for_owner(owner_a, tree_a)
    call immutable_store_verify_tree(store, tree_a, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'replacement tree verifies all twelve children')
    call immutable_store_lease_release(store, reading, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'active owner read lease releases')
    call snapshot_measure(bytes_after, compact_after)
    call test_assert_equal_int(suite, 12, compact_after, &
        'one compact object group remains while second owner is active')
    call test_assert(suite, bytes_after < bytes_before, &
        'single tree root reduces durable snapshot bytes')
    call immutable_store_collect(store, 1000, 1000, 0_int64, 0_int64, 0, &
        scan_count, allocated, deleted, reclaimed, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'GC traverses replacement tree')
    do i = 1, 12
        call test_assert(suite, exists(immutable_store_blob_path(store, ids(i))), &
            'compacted root retains every input blob')
    end do

    tree_ids(1) = tree_a
    tree_kinds(1) = 'tree'
    call immutable_store_publication_lease_acquire(store, 'fx-root-compact', &
        store%writer_start, 'stale-check', tree_kinds, tree_ids, &
        publication, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'replacement tree is protected during stale check')
    call immutable_store_root_set(store, 'fo-generation', owner_b, &
        trim(reason_b), kinds(:11), ids(:11), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'concurrent owner version changes its exact root group')
    replaced = c_replace(store%root_dir//c_null_char, owner_b//c_null_char, &
        trim(reason_b)//c_null_char, expected_ids//c_null_char, 12_c_int, &
        tree_a//c_null_char, store%writer_start//c_null_char, &
        publication%token//c_null_char)
    call test_assert_equal_int(suite, 1, int(replaced), &
        'stale compare never overwrites changed owner roots')
    call immutable_store_lease_release(store, publication, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'stale check releases publication protection')
    call immutable_store_compact_generation_roots(store, 1000, 1, 16, &
        scanned, compacted, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'changed group remains eligible under its new exact contents')
    call test_assert_equal_int(suite, 1, compacted, &
        'second worktree version compacts independently')
    call tree_for_owner(owner_b, tree_b)
    call test_assert(suite, tree_a /= tree_b, &
        'changed worktree owns a distinct tree identity')
    call snapshot_measure(bytes_after, compact_final)
    call test_assert_equal_int(suite, 0, compact_final, &
        'all migrated roots use one tree row each')
    write (*, '(a,i0,a,i0,a,i0,a,i0)') 'ROOT COMPACTION: children=', 12, &
        ' before_bytes=', bytes_before, ' after_bytes=', bytes_after, &
        ' compact_rows=', compact_final

    call immutable_store_reason_release(store, 'fo-generation', owner_a, &
        trim(reason_a), ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'first exact owner root releases')
    call immutable_store_collect(store, 1000, 1000, 0_int64, 0_int64, 0, &
        scan_count, allocated, deleted, reclaimed, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'remaining worktree tree survives first release')
    do i = 1, 11
        call test_assert(suite, exists(immutable_store_blob_path(store, ids(i))), &
            'second worktree protects its child')
    end do
    call immutable_store_reason_release(store, 'fo-generation', owner_b, &
        trim(reason_b), ierr)
    call immutable_store_collect(store, 1000, 1000, 0_int64, 0_int64, 0, &
        scan_count, allocated, deleted, reclaimed, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'released trees and children collect')
    do i = 1, 12
        call test_assert(suite, .not. exists(immutable_store_blob_path(store, &
            ids(i))), 'released child is reclaimed')
    end do
    call test_assert(suite, .not. exists(immutable_store_tree_path(store, &
        tree_a)), 'first released tree is reclaimed')
    call test_assert(suite, .not. exists(immutable_store_tree_path(store, &
        tree_b)), 'second released tree is reclaimed')

    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'fixture tree removes')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine put_text(number, id)
        integer, intent(in) :: number
        character(len=64), intent(out) :: id
        integer :: fd, status
        open (newunit=fd, file=trim(source), status='replace', &
            access='stream', form='unformatted', iostat=status)
        call test_assert_equal_int(suite, 0, status, 'source opens')
        write (fd) 'root-child-'//int_text(number)
        close (fd)
        call immutable_store_put_blob(store, trim(source), id, status)
        call test_assert_equal_int(suite, IMMUTABLE_OK, status, 'child publishes')
    end subroutine put_text

    subroutine snapshot_measure(bytes, compact_rows)
        integer(int64), intent(out) :: bytes
        integer, intent(out) :: compact_rows
        integer :: fd, status
        inquire (file=trim(snapshot), size=bytes, iostat=status)
        call test_assert_equal_int(suite, 0, status, 'snapshot size reads')
        compact_rows = 0
        open (newunit=fd, file=trim(snapshot), status='old', &
            action='read', iostat=status)
        call test_assert_equal_int(suite, 0, status, 'snapshot opens')
        do
            read (fd, '(a)', iostat=status) line
            if (status /= 0) exit
            if (index(line, 'O|') == 1) compact_rows = compact_rows + 1
        end do
        close (fd)
    end subroutine snapshot_measure

    subroutine tree_for_owner(owner, tree_id)
        character(len=*), intent(in) :: owner
        character(len=64), intent(out) :: tree_id
        character(len=:), allocatable :: prefix
        integer :: fd, status
        tree_id = ''
        prefix = 'R||fo-generation|'//owner//'|generation-'//owner//'|tree|'
        open (newunit=fd, file=trim(snapshot), status='old', &
            action='read', iostat=status)
        call test_assert_equal_int(suite, 0, status, 'tree root snapshot opens')
        do
            read (fd, '(a)', iostat=status) line
            if (status /= 0) exit
            if (index(line, prefix) == 1) &
                tree_id = line(len(prefix) + 1:len(prefix) + 64)
        end do
        close (fd)
        call test_assert(suite, len_trim(tree_id) == 64, &
            'one durable tree root exists for exact owner')
    end subroutine tree_for_owner

    logical function exists(path)
        character(len=*), intent(in) :: path
        inquire (file=path, exist=exists)
    end function exists

    function int_text(value) result(text)
        integer, intent(in) :: value
        character(len=16) :: text
        write (text, '(i0)') value
    end function int_text
end program test_immutable_root_compact
