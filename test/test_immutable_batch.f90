! Batched blob publication: one durability barrier set per batch, names only
! after the payload barrier, exact bytes, and rejection of changed inputs.
program test_immutable_batch
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr, c_null_char, &
        c_associated
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_hash, only: sha256_string
    use fx_test_fs, only: fx_test_temp_root, fx_test_remove_tree
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        IMMUTABLE_INVALID, IMMUTABLE_BLOB, immutable_tree_entry_t, &
        immutable_store_init, immutable_store_verify_blob, &
        immutable_store_blob_path
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree
    use fx_immutable_batch, only: immutable_batch_t, immutable_batch_begin, &
        immutable_batch_put_file, immutable_batch_commit, immutable_batch_end, &
        immutable_batch_covers
    implicit none
    interface
        function obs_begin(root) bind(C, name='fx_test_sync_batch_begin') &
                result(handle)
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: root(*)
            type(c_ptr) :: handle
        end function obs_begin
        integer(c_int) function obs_deferred(handle) &
                bind(C, name='fx_test_sync_batch_deferred')
            import :: c_int, c_ptr
            type(c_ptr), value :: handle
        end function obs_deferred
        integer(c_int) function obs_open(path) &
                bind(C, name='fx_test_sync_batch_open_file')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function obs_open
        function obs_stage(handle, blob_path, input) &
                bind(C, name='fx_test_sync_batch_stage') result(capture)
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: blob_path(*)
            integer(c_int), value :: input
            type(c_ptr) :: capture
        end function obs_stage
        integer(c_int) function obs_write(capture, bytes, count) &
                bind(C, name='fx_test_sync_batch_write')
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: capture
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), value :: count
        end function obs_write
        integer(c_int) function obs_stage_end(handle, capture, input, id, ok) &
                bind(C, name='fx_test_sync_batch_stage_end')
            import :: c_char, c_int, c_ptr
            type(c_ptr), value :: handle, capture
            integer(c_int), value :: input, ok
            character(kind=c_char), intent(in) :: id(*)
        end function obs_stage_end
        integer(c_int) function obs_commit(handle, retained) &
                bind(C, name='fx_test_sync_batch_commit')
            import :: c_int, c_ptr
            type(c_ptr), value :: handle
            integer(c_int), intent(out) :: retained(*)
        end function obs_commit
        subroutine obs_end(handle) bind(C, name='fx_test_sync_batch_end')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine obs_end
        integer(c_int) function obs_close(fd) bind(C, name='fx_test_sync_owned_close')
            import :: c_int
            integer(c_int), value :: fd
        end function obs_close
        subroutine obs_reset(fail) bind(C, name='fx_test_sync_reset')
            import :: c_int
            integer(c_int), value :: fail
        end subroutine obs_reset
        integer(c_int) function obs_syncs() bind(C, name='fx_test_sync_calls')
            import :: c_int
        end function obs_syncs
        integer(c_int) function obs_links() bind(C, name='fx_test_sync_links')
            import :: c_int
        end function obs_links
        integer(c_int) function obs_early_links() &
                bind(C, name='fx_test_sync_early_links')
            import :: c_int
        end function obs_early_links
    end interface

    type(test_suite_t) :: suite
    character(len=512) :: root
    character(len=512, kind=c_char) :: scratch
    integer :: end_path, ierr

    call test_suite_init(suite, 'immutable_batch')
    scratch = c_null_char
    ierr = fx_test_temp_root(scratch, len(scratch))
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-batch-', proc_pid()
    ierr = fx_test_remove_tree(trim(root))

    call check_barrier_count()
    call check_failed_barrier()
    call check_public_batch()
    ierr = fx_test_remove_tree(trim(root))
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit

        open (newunit=unit, file=path, access='stream', form='unformatted', &
            status='replace', action='write')
        write (unit) text
        close (unit)
    end subroutine write_text

    function payload(label, i) result(text)
        character(len=*), intent(in) :: label
        integer, intent(in) :: i
        character(len=:), allocatable :: text
        character(len=32) :: number

        write (number, '(i0)') i
        text = label//' payload '//trim(number)//repeat('x', 3 * i)
    end function payload

    !! Stage n payloads through the observed implementation and commit.
    subroutine observed_batch(store, label, n, fail, syncs, links, early, rc)
        type(immutable_store_t), intent(in) :: store
        character(len=*), intent(in) :: label
        integer, intent(in) :: n
        integer(c_int), intent(in) :: fail
        integer, intent(out) :: syncs, links, early, rc
        character(len=:), allocatable :: text
        character(len=64) :: id
        character(len=512) :: source
        integer(c_int) :: retained(n), fd, status
        type(c_ptr) :: batch, capture
        integer :: i

        call obs_reset(fail)
        batch = obs_begin(trim(store%root_dir)//c_null_char)
        rc = -10
        if (.not. c_associated(batch)) return
        do i = 1, n
            text = payload(label, i)
            id = sha256_string(text)
            write (source, '(a,a,i0)') trim(root), '/input-', i
            call write_text(trim(source), text)
            fd = obs_open(trim(source)//c_null_char)
            capture = obs_stage(batch, immutable_store_blob_path(store, id)// &
                c_null_char, fd)
            status = -20
            if (c_associated(capture)) then
                status = obs_write(capture, text, int(len(text), c_int))
                status = obs_stage_end(batch, capture, fd, id//c_null_char, 1_c_int)
            end if
            ierr = obs_close(fd)
            if (status /= 0_c_int) then
                rc = int(status)
                call obs_end(batch)
                return
            end if
        end do
        if (obs_deferred(batch) == 0_c_int) then
            ! Eager platforms publish per object; record and skip the bound.
            rc = 1
            call obs_end(batch)
            return
        end if
        rc = int(obs_commit(batch, retained))
        call obs_end(batch)
        syncs = int(obs_syncs())
        links = int(obs_links())
        early = int(obs_early_links())
    end subroutine observed_batch

    subroutine check_barrier_count()
        type(immutable_store_t) :: store
        integer :: syncs_small, syncs_large, links, early, rc, status, i

        call immutable_store_init(store, trim(root)//'/count-store', ierr)
        call test_assert(suite, ierr == IMMUTABLE_OK, 'count store initializes')
        call observed_batch(store, 'small', 4, 0_c_int, syncs_small, links, early, rc)
        if (rc == 1) then
            call test_assert(suite, .true., &
                'eager platform: per-object synchronized publication')
            return
        end if
        call test_assert_equal_int(suite, 0, rc, 'small batch commits')
        call test_assert_equal_int(suite, 0, early, 'no name before payload barrier')
        call observed_batch(store, 'large', 96, 0_c_int, syncs_large, links, early, rc)
        call test_assert_equal_int(suite, 0, rc, 'large batch commits')
        call test_assert_equal_int(suite, 96, links, 'every large payload is linked')
        call test_assert_equal_int(suite, 0, early, &
            'large batch links no name before its payload barrier')
        call test_assert_equal_int(suite, syncs_small, syncs_large, &
            'barrier count is independent of the blob count')
        call test_assert_equal_int(suite, 2, syncs_large, &
            'a batch costs one payload and one name barrier')
        do i = 1, 96
            call immutable_store_verify_blob(store, &
                sha256_string(payload('large', i)), status)
            call test_assert(suite, status == IMMUTABLE_OK, &
                'published batch blob verifies')
        end do
    end subroutine check_barrier_count

    subroutine check_failed_barrier()
        type(immutable_store_t) :: store
        integer :: syncs, links, early, rc, status

        call immutable_store_init(store, trim(root)//'/failed-store', ierr)
        call observed_batch(store, 'failed', 8, 1_c_int, syncs, links, early, rc)
        if (rc == 1) return
        call test_assert(suite, rc /= 0, 'failed payload barrier fails the commit')
        call test_assert_equal_int(suite, 0, links, &
            'failed payload barrier publishes no name')
        call immutable_store_verify_blob(store, &
            sha256_string(payload('failed', 1)), status)
        call test_assert(suite, status /= IMMUTABLE_OK, &
            'blob of a failed batch is absent')
    end subroutine check_failed_barrier

    subroutine check_public_batch()
        type(immutable_store_t) :: store
        type(immutable_batch_t) :: batch
        type(immutable_tree_entry_t) :: entries(3)
        character(len=64) :: ids(3), tree_id
        character(len=512) :: path
        integer :: i, status

        call immutable_store_init(store, trim(root)//'/public-store', ierr)
        call immutable_batch_begin(store, batch, ierr)
        call test_assert(suite, ierr == IMMUTABLE_OK, 'batch begins')
        do i = 1, 3
            write (path, '(a,a,i0)') trim(root), '/public-', i
            call write_text(trim(path), payload('public', i))
            ids(i) = sha256_string(payload('public', i))
            call immutable_batch_put_file(store, batch, trim(path), ids(i), status)
            call test_assert(suite, status == IMMUTABLE_OK, 'batch stages input')
        end do
        call immutable_batch_put_file(store, batch, trim(path), ids(3), status)
        call test_assert(suite, status == IMMUTABLE_OK, 'duplicate id is reused')
        call immutable_batch_put_file(store, batch, trim(path), &
            sha256_string('different bytes'), status)
        call test_assert(suite, status == IMMUTABLE_INVALID, &
            'bytes that differ from the expected id are rejected')
        call immutable_store_verify_blob(store, ids(1), status)
        call test_assert(suite, status /= IMMUTABLE_OK, &
            'staged blob is not visible before commit')
        call test_assert(suite, .not. immutable_batch_covers(batch, ids(1)), &
            'uncommitted batch vouches for nothing')
        call immutable_batch_commit(store, batch, status)
        call test_assert(suite, status == IMMUTABLE_OK, 'batch commits')
        do i = 1, 3
            call immutable_store_verify_blob(store, ids(i), status)
            call test_assert(suite, status == IMMUTABLE_OK, 'committed blob verifies')
            entries(i)%path = ids(i)
            entries(i)%role = 'input'
            entries(i)%object_id = ids(i)
            entries(i)%mode = 420
            entries(i)%kind = IMMUTABLE_BLOB
        end do
        call immutable_store_put_tree(store, entries, tree_id, status, batch)
        call test_assert(suite, status == IMMUTABLE_OK, 'tree over batch publishes')
        call immutable_store_verify_tree(store, tree_id, status)
        call test_assert(suite, status == IMMUTABLE_OK, 'tree graph verifies in full')
        call immutable_batch_end(batch)

        ! An abandoned batch leaves no object and no staging behind.
        call immutable_batch_begin(store, batch, ierr)
        write (path, '(a,a)') trim(root), '/abandoned'
        call write_text(trim(path), 'abandoned payload')
        call immutable_batch_put_file(store, batch, trim(path), &
            sha256_string('abandoned payload'), status)
        call test_assert(suite, status == IMMUTABLE_OK, 'abandoned input stages')
        call immutable_batch_end(batch)
        call immutable_store_verify_blob(store, sha256_string('abandoned payload'), &
            status)
        call test_assert(suite, status /= IMMUTABLE_OK, &
            'abandoned batch publishes nothing')
    end subroutine check_public_batch

end program test_immutable_batch
