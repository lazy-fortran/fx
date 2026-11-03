program test_immutable_reuse
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_suite_summary, test_suite_exit
    use fx_proc, only: proc_pid
    use fx_test_fs, only: fx_test_temp_root, fx_test_remove_tree
    use fx_immutable_store, only: immutable_store_t, immutable_lease_t, &
        IMMUTABLE_OK, IMMUTABLE_CORRUPT, immutable_store_init, &
        immutable_store_put_blob, immutable_store_blob_path, &
        immutable_store_file_info, immutable_store_publication_lease_acquire, &
        immutable_store_lease_release, immutable_store_verify_blob
    use fx_immutable_batch, only: immutable_batch_t, immutable_batch_begin, &
        immutable_batch_put_file, immutable_batch_commit, immutable_batch_end
    use fx_immutable_owned, only: owned_open_store, owned_open_verified, &
        owned_hash_fd, owned_close
    implicit none
    interface
        integer(c_int) function edit_preserving_mtime(path) &
                bind(C, name='fx_test_edit_preserving_mtime')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function edit_preserving_mtime
    end interface
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_batch_t) :: batch
    type(immutable_lease_t) :: lease
    character(len=512, kind=c_char) :: scratch
    character(len=512) :: root, source
    character(len=64) :: id, actual
    integer(int64) :: bytes, before, after, inode
    integer :: ierr, unit
    integer(c_int) :: root_fd, fd, cleanup
    logical :: hit

    call test_suite_init(suite, 'immutable_reuse')
    ierr = fx_test_temp_root(scratch, len(scratch))
    write (root, '(a,i0)') scratch(:index(scratch, c_null_char) - 1)//'/reuse-', proc_pid()
    call immutable_store_init(store, trim(root), ierr)
    source = trim(root)//'/source'
    open (newunit=unit, file=trim(source), status='replace')
    write (unit, '(a)') 'unchanged content'
    close (unit)
    call immutable_store_put_blob(store, trim(source), id, ierr)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'fixture publishes')
    call immutable_store_publication_lease_acquire(store, 'reuse-test', &
        store%writer_start, 'leased-reuse', ['blob'], [id], lease, ierr)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'publication lease acquired')
    call immutable_store_file_info(store, id, bytes, before, inode, ierr)
    call immutable_batch_begin(store, batch, ierr)
    call immutable_batch_put_file(store, batch, trim(source), id, ierr, lease)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'leased existing blob reused')
    call immutable_batch_commit(store, batch, ierr)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'leased reuse commits')
    call immutable_store_file_info(store, id, bytes, after, inode, ierr)
    call test_assert(suite, before == after, &
        'publication lease protects reuse without changing object timestamps')
    call immutable_batch_end(batch)

    root_fd = owned_open_store(store%root_dir//c_null_char)
    call owned_open_verified(root_fd, 1_c_int, id, fd, ierr)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'verified descriptor opens')
    if (fd >= 0) then
        call owned_hash_fd(fd, actual, ierr, hit)
        call test_assert(suite, ierr == IMMUTABLE_OK .and. hit .and. actual == id, &
            'unchanged descriptor reuses its verified content hash')
        cleanup = owned_close(fd)
    end if
    cleanup = owned_close(root_fd)
    ierr = edit_preserving_mtime(immutable_store_blob_path(store, id)//c_null_char)
    call test_assert(suite, ierr == 0, 'same-size corruption restores the old mtime')
    call immutable_store_verify_blob(store, id, ierr)
    call test_assert(suite, ierr == IMMUTABLE_CORRUPT, &
        'ctime change invalidates a previously verified blob')
    call immutable_store_lease_release(store, lease, ierr)
    call test_assert(suite, ierr == IMMUTABLE_OK, 'publication lease released')
    ierr = fx_test_remove_tree(trim(root))
    call test_suite_summary(suite)
    call test_suite_exit(suite)
end program test_immutable_reuse
