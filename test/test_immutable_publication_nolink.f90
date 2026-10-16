! Publication must succeed where hard links between directories fail (AFS).
! The test re-executes itself with FX_STORE_FORCE_NO_LINK set, which makes the
! store treat every link as EXDEV and take the copy fallback.
program test_immutable_publication_nolink
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    use fx_proc, only: proc_pid
    use fx_hash, only: sha256_string
    use fx_test_fs, only: fx_test_temp_root
    use fx_immutable_store, only: immutable_store_t, IMMUTABLE_OK, &
        immutable_store_init, immutable_store_put_blob, &
        immutable_store_verify_blob
    implicit none

    character(len=*), parameter :: PAYLOAD = 'payload published without links'
    type(test_suite_t) :: suite
    type(immutable_store_t) :: store
    character(len=512) :: root, source, flag, exe
    character(len=512, kind=c_char) :: scratch
    character(len=64) :: blob_id, again_id
    integer :: ierr, end_path, unit, status

    call get_environment_variable('FX_STORE_FORCE_NO_LINK', flag, status=status)
    if (status /= 0) then
        call get_command_argument(0, exe)
        call execute_command_line('FX_STORE_FORCE_NO_LINK=1 '//trim(exe), &
            exitstat=status)
        if (status /= 0) stop 1
        stop 0
    end if

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

    call immutable_store_put_blob(store, trim(source), blob_id, ierr)
    call test_assert_equal_int(suite, IMMUTABLE_OK, ierr, &
        'blob publishes when links are unavailable')
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
