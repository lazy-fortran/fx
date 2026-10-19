program test_action_result_evict
    !! Size-bounded eviction: least recently used records go first, entries
    !! used or published within the grace period and objects pinned by other
    !! lease owners survive, shared objects stay while referenced, stale
    !! staging files are reaped, and dangling records never stop a run.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, fx_test_temp_root
    use fx_immutable_store, only: immutable_tree_entry_t, &
        immutable_store_blob_path, immutable_store_tree_path
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_publish_files, &
        action_result_lookup, action_result_action_key, action_result_evict, &
        action_result_evict_stats_t, action_result_store_policy, &
        ACTION_RESULT_OK, ACTION_RESULT_MISSING
    implicit none

    interface
        integer(c_int) function set_mtime(path, mtime) &
                bind(C, name='fx_c_set_mtime')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), value :: mtime
        end function set_mtime
        integer(c_long_long) function unix_time() bind(C, name='fx_c_unix_time')
            import c_long_long
        end function unix_time
    end interface

    integer, parameter :: NACT = 6, PAYLOAD = 65536
    type(test_suite_t) :: suite
    type(action_result_store_t) :: store
    type(action_result_evict_stats_t) :: stats
    type(immutable_tree_entry_t), allocatable :: found(:)
    character(len=512, kind=c_char) :: scratch
    character(len=512) :: root
    character(len=64) :: blob(NACT), tree(NACT), shared_blob, found_id
    integer :: ierr, end_path, i
    integer(c_long_long) :: now
    logical :: exists

    call test_suite_init(suite, 'fx_action_result_evict')
    scratch = c_null_char
    ierr = fx_test_temp_root(scratch, 512)
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-evict-', proc_pid()
    ierr = fx_test_mkdir_p(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'test root is created')
    call action_result_store_init(store, trim(root)//'/store/v2', ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, 'store initializes')
    call action_result_store_policy(max_bytes=0_int64, grace_seconds=3600_int64, &
        interval_seconds=huge(0_int64))
    now = unix_time()

    ! Six actions with distinct 64 KiB outputs, used in order 1..6 long ago.
    do i = 1, NACT
        call publish(i, 'payload-'//achar(48 + i), blob(i), tree(i))
        call age_action(i, now - 172800_c_long_long + 60_c_long_long * i)
    end do
    ! Action 3 also shares action 6's payload under a second action key.
    call publish(7, 'payload-6', shared_blob, found_id)
    call test_assert(suite, shared_blob == blob(6), &
        'identical payload deduplicates to one blob')
    call age_action(7, now - 172800_c_long_long)

    call action_result_evict(store, stats, ierr, max_bytes=0_int64)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, 'uncapped run succeeds')
    call test_assert(suite, stats%actions_retired == 0 .and. &
        stats%objects_deleted == 0, 'a zero cap evicts nothing')

    ! A read refreshes action 1's last-use stamp: it becomes most recent.
    call action_result_lookup(store, key_name(1), found, found_id, ierr)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, 'old action reads')

    ! Room for about two actions' objects (64 KiB blob plus a small tree).
    call action_result_evict(store, stats, ierr, max_bytes=200000_int64)
    call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, 'capped run succeeds')
    call test_assert(suite, stats%bytes_before > 200000_int64, &
        'fixture exceeds the cap')
    call test_assert(suite, stats%bytes_after <= 160000_int64, &
        'eviction reaches the low-water mark')
    call test_assert(suite, .not. stats%over_cap, 'store ends below the cap')
    call expect_hit(1, .true., 'recently read action survives')
    call expect_hit(2, .false., 'least recently used action is retired')
    call expect_hit(3, .false., 'second least recently used action is retired')
    call expect_hit(6, .true., 'most recently used action survives')
    inquire (file=immutable_store_blob_path(store%objects, blob(2)), exist=exists)
    call test_assert(suite, .not. exists, 'retired action blob is deleted')
    inquire (file=immutable_store_tree_path(store%objects, tree(2)), exist=exists)
    call test_assert(suite, .not. exists, 'retired action tree is deleted')
    inquire (file=immutable_store_blob_path(store%objects, blob(6)), exist=exists)
    call test_assert(suite, exists, 'blob shared with a live action survives')

    call test_grace_and_pins()
    call test_stale_and_dangling()

    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'test root is removed')
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_grace_and_pins()
        type(action_result_store_t) :: fresh
        character(len=64) :: young_blob, young_tree, pinned_blob, pinned_tree
        character(len=64) :: old_blob, old_tree
        integer :: unit, local_err

        call action_result_store_init(fresh, trim(root)//'/grace/v2', local_err)
        store = fresh
        ! Young: just published, never aged. Pinned: old but leased by an
        ! owner other than the action store. Old: unprotected.
        call publish(11, 'young', young_blob, young_tree)
        call publish(12, 'pinned', pinned_blob, pinned_tree)
        call publish(13, 'old', old_blob, old_tree)
        call age_action(12, now - 172800_c_long_long)
        call age_action(13, now - 172800_c_long_long)
        local_err = fx_test_mkdir_p(trim(root)//'/grace/v2/.fx-metadata')
        open (newunit=unit, file=trim(root)//'/grace/v2/.fx-metadata/leases', &
            status='replace', action='write')
        write (unit, '(a)') 'fxleases2|7'
        write (unit, '(a)') 'R||generation-owner|start-1|generation|blob|'// &
            pinned_blob
        write (unit, '(a)') 'R||'//action_result_action_key(key_name(13))// &
            '|fx-action-v1|bound|tree|'//old_tree
        close (unit)
        call action_result_evict(store, stats, local_err, max_bytes=1_int64)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, local_err, &
            'grace run succeeds')
        call expect_hit(11, .true., 'action published within grace survives')
        call expect_hit(13, .false., 'unprotected old action is retired')
        inquire (file=immutable_store_blob_path(store%objects, pinned_blob), &
            exist=exists)
        call test_assert(suite, exists, 'object leased by another owner survives')
        inquire (file=immutable_store_tree_path(store%objects, old_tree), &
            exist=exists)
        call test_assert(suite, .not. exists, &
            'legacy action-store lease rows do not pin objects')
        call test_assert(suite, stats%over_cap, &
            'cap stays soft while protected data exceeds it')
    end subroutine test_grace_and_pins

    subroutine test_stale_and_dangling()
        type(action_result_store_t) :: fresh
        character(len=64) :: b, t
        character(len=:), allocatable :: shard, old_temp, young_temp, action_tmp
        character(len=64) :: key
        integer :: unit, local_err

        call action_result_store_init(fresh, trim(root)//'/stale/v2', local_err)
        store = fresh
        call publish(21, 'dangling', b, t)
        call publish(22, 'kept', b, t)
        ! Remove action 21's tree: its record now dangles.
        call publish(23, 'other', b, t)
        local_err = fx_test_remove_tree(immutable_store_tree_path(store%objects, t))
        shard = trim(root)//'/stale/v2/blobs/sha256/'//b(1:2)
        old_temp = shard//'/.fx-owned-999999-1'
        young_temp = shard//'/.fx-owned-999999-2'
        local_err = fx_test_mkdir_p(old_temp)
        local_err = fx_test_mkdir_p(young_temp)
        local_err = set_mtime(old_temp//c_null_char, now - 172800_c_long_long)
        key = action_result_action_key(key_name(22))
        action_tmp = trim(root)//'/stale/v2/actions/sha256/'//key(1:2)// &
            '/.tmp.h.1.1'
        open (newunit=unit, file=action_tmp, status='replace', action='write')
        write (unit, '(a)') 'partial'
        close (unit)
        local_err = set_mtime(action_tmp//c_null_char, now - 172800_c_long_long)
        call action_result_evict(store, stats, local_err, max_bytes=0_int64)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, local_err, &
            'run with a dangling record succeeds')
        call test_assert(suite, stats%temps_removed == 2, &
            'stale staging directory and record temp are reaped')
        inquire (file=old_temp//'/.', exist=exists)
        call test_assert(suite, .not. exists, 'stale staging directory is removed')
        inquire (file=young_temp//'/.', exist=exists)
        call test_assert(suite, exists, 'young staging directory is kept')
        call expect_hit(23, .false., 'dangling record is a miss, not a hit')
        call expect_hit(22, .true., 'intact action still hits')
    end subroutine test_stale_and_dangling

    subroutine publish(i, text, blob_id, tree_id)
        integer, intent(in) :: i
        character(len=*), intent(in) :: text
        character(len=64), intent(out) :: blob_id, tree_id
        character(len=512) :: source(1)
        character(len=PAYLOAD) :: bytes
        type(immutable_tree_entry_t) :: entry(1)
        integer :: unit, local_err, j
        integer(int64) :: state

        ! Incompressible bytes, so allocated sizes match on compressing
        ! filesystems; the text prefix distinguishes payloads.
        state = 12345_int64
        do j = 1, PAYLOAD
            state = ieor(state, ishft(state, 13))
            state = ieor(state, ishft(state, -7))
            state = ieor(state, ishft(state, 17))
            bytes(j:j) = achar(int(modulo(ishft(state, -24), 256_int64)))
        end do
        bytes(1:len(text)) = text
        source(1) = trim(root)//'/source-'//key_name(i)
        open (newunit=unit, file=trim(source(1)), status='replace', &
            access='stream', form='unformatted')
        write (unit) bytes
        close (unit)
        entry(1)%path = 'object'
        entry(1)%role = 'object'
        entry(1)%mode = 420
        call action_result_publish_files(store, key_name(i), source, entry, &
            tree_id, local_err)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, local_err, &
            'fixture action publishes')
        blob_id = entry(1)%object_id
    end subroutine publish

    subroutine age_action(i, mtime)
        !! Age the record and its objects as if last used at mtime.
        integer, intent(in) :: i
        integer(c_long_long), intent(in) :: mtime
        character(len=64) :: key, id
        type(immutable_tree_entry_t), allocatable :: entries(:)
        integer :: local_err, j

        key = action_result_action_key(key_name(i))
        call action_result_lookup(store, key_name(i), entries, id, local_err)
        local_err = set_mtime(trim(store%root_dir)//'/actions/sha256/'// &
            key(1:2)//'/'//key//c_null_char, mtime)
        if (.not. allocated(entries)) return
        local_err = set_mtime(immutable_store_tree_path(store%objects, id)// &
            c_null_char, mtime)
        do j = 1, size(entries)
            local_err = set_mtime(immutable_store_blob_path(store%objects, &
                entries(j)%object_id)//c_null_char, mtime)
        end do
    end subroutine age_action

    subroutine expect_hit(i, hit, label)
        integer, intent(in) :: i
        logical, intent(in) :: hit
        character(len=*), intent(in) :: label
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=64) :: id
        integer :: local_err

        call action_result_lookup(store, key_name(i), entries, id, local_err)
        if (hit) then
            call test_assert_equal_int(suite, ACTION_RESULT_OK, local_err, label)
        else
            call test_assert(suite, local_err /= ACTION_RESULT_OK, label)
        end if
    end subroutine expect_hit

    function key_name(i) result(name)
        integer, intent(in) :: i
        character(len=:), allocatable :: name
        character(len=16) :: text

        write (text, '(a,i0)') 'evict-action-', i
        name = trim(text)
    end function key_name
end program test_action_result_evict
