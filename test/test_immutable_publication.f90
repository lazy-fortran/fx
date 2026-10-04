program test_immutable_publication
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_proc, only: proc_pid
    use fx_hash, only: sha256_string
    use fx_test_fs, only: fx_test_temp_root
    use fx_immutable_store, only: immutable_store_t, immutable_tree_entry_t, &
        IMMUTABLE_OK, IMMUTABLE_BLOB, immutable_store_init, &
        immutable_store_put_blob, immutable_store_verify_blob, &
        immutable_store_file_info
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree
    use fx_immutable_manifest, only: immutable_manifest_serialize
    implicit none

    character(len=*), parameter :: PAYLOAD = 'captured publication payload'
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    type(immutable_tree_entry_t) :: entry(1)
    character(len=512) :: root, source
    character(len=512, kind=c_char) :: scratch
    character(len=64) :: blob_id, repeated_blob_id, tree_id, repeated_tree_id
    character(len=:), allocatable :: manifest, observed
    integer(kind=8) :: bytes_before, mtime_before, inode_before
    integer(kind=8) :: bytes_after, mtime_after, inode_after
    integer :: ierr, end_path

    call test_suite_init(suite, 'immutable_publication')
    scratch = c_null_char
    ierr = fx_test_temp_root(scratch, len(scratch))
    call test_assert_equal_int(suite, 0, ierr, &
        'physical system scratch resolves')
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-publish51-', proc_pid()
    source = trim(root)//'/source'
    call immutable_store_init(store, trim(root)//'/store', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')
    call write_text(source, PAYLOAD)

    call immutable_store_put_blob(store, trim(source), blob_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'blob publishes through the public store API')
    call test_assert_equal_str(suite, sha256_string(PAYLOAD), blob_id, &
        'published blob ID hashes its actual bytes')
    call immutable_store_verify_blob(store, blob_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'published blob reopens and verifies')
    call immutable_store_file_info(store, blob_id, bytes_before, mtime_before, &
        inode_before, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'published blob metadata is readable')
    call immutable_store_put_blob(store, trim(source), repeated_blob_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'identical blob publication remains successful')
    call test_assert_equal_str(suite, blob_id, repeated_blob_id, &
        'identical blob publication returns the same ID')
    call immutable_store_file_info(store, blob_id, bytes_after, mtime_after, &
        inode_after, ierr)
    call test_assert(suite, bytes_before == bytes_after .and. &
        mtime_before == mtime_after .and. inode_before == inode_after, &
        'identical publication preserves the existing immutable payload')

    entry(1)%path = 'payload.bin'
    entry(1)%role = 'source'
    entry(1)%object_id = blob_id
    entry(1)%kind = IMMUTABLE_BLOB
    entry(1)%mode = 420
    manifest = immutable_manifest_serialize(entry)
    call immutable_store_put_tree(store, entry, tree_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'tree manifest publishes through the public store API')
    call test_assert_equal_str(suite, sha256_string(manifest), tree_id, &
        'tree ID hashes the canonical manifest bytes')
    call immutable_store_verify_tree(store, tree_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'published tree manifest reopens and verifies')
    call immutable_store_put_tree(store, entry, repeated_tree_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'identical tree publication remains successful')
    call test_assert_equal_str(suite, tree_id, repeated_tree_id, &
        'identical tree publication returns the same ID')
    call immutable_store_verify_tree(store, tree_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'repeated publication keeps the complete tree available')
    call read_text(trim(source), observed, ierr)
    call test_assert(suite, ierr == 0 .and. observed == PAYLOAD, &
        'publication leaves its source file unchanged')

    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine write_text(path, text)
        character(len=*), intent(in) :: path, text
        integer :: unit, status, close_status
        open(newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted', action='write', iostat=status)
        if (status /= 0) stop 21
        write(unit, iostat=status) text
        close(unit, iostat=close_status)
        if (status /= 0 .or. close_status /= 0) stop 22
    end subroutine write_text

    subroutine read_text(path, text, status)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: status
        integer :: unit, count
        count = -1
        inquire(file=path, size=count, iostat=status)
        if (status /= 0 .or. count < 0) return
        text = repeat(' ', count)
        open(newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', action='read', iostat=status)
        if (status /= 0) return
        read(unit, iostat=status) text
        close(unit)
    end subroutine read_text
end program test_immutable_publication
