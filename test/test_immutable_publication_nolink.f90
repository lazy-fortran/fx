! Publication must succeed where hard links between directories fail (AFS).
! A test-only compilation adapter returns EXDEV for cross-directory links.
! The production object has no fixture policy or environment controls.
program test_immutable_publication_nolink
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_int, c_ptr, c_associated
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_proc, only: proc_pid
    use fx_hash, only: sha256_string
    use fx_test_fs, only: fx_test_temp_root
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        immutable_store_init, immutable_store_put_blob, &
        immutable_store_verify_blob, immutable_store_blob_path
    implicit none
    interface
        function begin_candidate(path, tree) result(handle) &
                bind(C, name='fx_test_nolink_owned_begin_path')
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: tree
            type(c_ptr) :: handle
        end function begin_candidate
        integer(c_int) function write_candidate(handle, bytes, count) &
                bind(C, name='fx_test_nolink_owned_write')
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), value :: count
        end function write_candidate
        integer(c_int) function publish_candidate(handle) &
                bind(C, name='fx_test_nolink_owned_publish')
            import :: c_ptr, c_int
            type(c_ptr), value :: handle
        end function publish_candidate
        subroutine dispose_candidate(handle) bind(C, name='fx_test_nolink_owned_dispose')
            import :: c_ptr
            type(c_ptr), value :: handle
        end subroutine dispose_candidate
        integer(c_int) function link_attempts() bind(C, name='fx_test_nolink_attempts')
            import :: c_int
        end function link_attempts
    end interface

    character(len=*), parameter :: PAYLOAD = 'payload published without links'
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    character(len=512) :: root, source
    character(len=512, kind=c_char) :: scratch
    character(len=64) :: blob_id, again_id
    integer :: ierr, end_path, unit, status
    type(c_ptr) :: candidate
    character(len=:), allocatable :: final

    call test_suite_init(suite, 'immutable_publication_nolink')
    scratch = c_null_char
    ierr = fx_test_temp_root(scratch, len(scratch))
    end_path = index(scratch, c_null_char)
    if (end_path <= 1) stop 20
    write (root, '(a,i0)') scratch(1:end_path - 1)//'/fx-nolink-', proc_pid()
    source = trim(root)//'/source'
    call immutable_store_init(store, trim(root)//'/store', ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, 'store initializes')
    open(newunit=unit, file=trim(source), status='replace', access='stream', &
        form='unformatted', action='write', iostat=status)
    if (status /= 0) stop 21
    write(unit) PAYLOAD
    close(unit)

    blob_id = sha256_string(PAYLOAD)
    final = immutable_store_blob_path(store, blob_id)
    candidate = begin_candidate(final//c_null_char, 0_c_int)
    call test_assert(suite, c_associated(candidate), 'test candidate opens')
    if (.not. c_associated(candidate)) stop 22
    ierr = write_candidate(candidate, PAYLOAD, int(len(PAYLOAD), c_int))
    call test_assert_equal_int(suite, 0, ierr, 'candidate bytes are staged')
    ierr = publish_candidate(candidate)
    call test_assert_equal_int(suite, 0, ierr, 'copy fallback publishes without cross links')
    call dispose_candidate(candidate)
    call test_assert(suite, link_attempts() > 0, 'oracle exercised unsupported link fallback')
    call test_assert_equal_str(suite, sha256_string(PAYLOAD), blob_id, &
        'copy-published blob ID hashes its bytes')
    call immutable_store_verify_blob(store, blob_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'copy-published blob verifies')
    call immutable_store_put_blob(store, trim(source), again_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'identical publication still succeeds without links')
    call test_assert_equal_str(suite, blob_id, again_id, &
        'identical publication returns the same ID')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
end program test_immutable_publication_nolink
