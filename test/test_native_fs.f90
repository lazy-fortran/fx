program test_native_fs
    use, intrinsic :: iso_c_binding, only: c_int, c_int64_t
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, &
        fx_test_rename, fx_test_symlink, fx_test_sleep_ms
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, &
        test_suite_summary, test_suite_exit
    implicit none
    interface
        integer(c_int) function getpid() bind(C)
            import :: c_int
        end function getpid
    end interface
    type(test_suite_t) :: suite
    character(len=256) :: root, target
    character(len=64) :: content
    integer :: ierr, unit
    integer(c_int64_t) :: before, after, rate
    logical :: exists

    call test_suite_init(suite, 'native fixture filesystem')
    write (root, '(a,i0)') '/var/tmp/fx fixture;$(data)-', getpid()
    target = trim(root)//'-outside'
    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'absent fixture cleanup succeeds')
    ierr = fx_test_mkdir_p(trim(root)//'/nested/deeper')
    call test_assert_equal_int(suite, 0, ierr, 'nested mkdir succeeds')
    inquire (file=trim(root)//'/nested/deeper', exist=exists)
    call test_assert(suite, exists, 'nested directory exists independently')
    ierr = fx_test_mkdir_p(trim(root)//'/nested/deeper')
    call test_assert_equal_int(suite, 0, ierr, 'existing directory is accepted')
    call write_file(trim(root)//'/nested/deeper/payload', 'new payload')
    ierr = fx_test_mkdir_p(trim(root)//'/nested/deeper/payload/child')
    call test_assert(suite, ierr /= 0, 'mkdir rejects a non-directory parent')

    call write_file(trim(root)//'/old', 'old payload')
    ierr = fx_test_rename(trim(root)//'/nested/deeper/payload', trim(root)//'/old')
    call test_assert_equal_int(suite, 0, ierr, 'rename replaces existing file')
    open (newunit=unit, file=trim(root)//'/old', status='old', action='read')
    read (unit, '(a)') content
    close (unit)
    call test_assert_equal_str(suite, 'new payload', trim(content), &
        'replacement preserves source bytes')
    inquire (file=trim(root)//'/nested/deeper/payload', exist=exists)
    call test_assert(suite,.not. exists, 'rename removes source name')
    ierr = fx_test_rename(trim(root)//'/nested', trim(root)//'/renamed')
    call test_assert_equal_int(suite, 0, ierr, 'directory rename succeeds')
    inquire (file=trim(root)//'/renamed/deeper', exist=exists)
    call test_assert(suite, exists, 'renamed directory retains descendants')
    ierr = fx_test_rename(trim(root)//'/missing', trim(root)//'/destination')
    call test_assert(suite, ierr /= 0, 'missing rename source fails')

    ierr = fx_test_mkdir_p(trim(target))
    call test_assert_equal_int(suite, 0, ierr, 'external symlink target created')
    call write_file(trim(target)//'/keep', 'retained target')
    ierr = fx_test_symlink(trim(target), trim(root)//'/link')
    call test_assert_equal_int(suite, 0, ierr, 'symlink creation succeeds')
    open (newunit=unit, file=trim(root)//'/link/keep', status='old', action='read')
    read (unit, '(a)') content
    close (unit)
    call test_assert_equal_str(suite, 'retained target', trim(content), &
        'symlink resolves literal target path')
    ierr = fx_test_remove_tree(trim(root))
    call test_assert_equal_int(suite, 0, ierr, 'recursive removal succeeds')
    inquire (file=trim(root), exist=exists)
    call test_assert(suite,.not. exists, 'recursive removal removes root')
    inquire (file=trim(target)//'/keep', exist=exists)
    call test_assert(suite, exists, 'removal does not follow directory symlinks')
    ierr = fx_test_remove_tree(trim(target))
    call test_assert_equal_int(suite, 0, ierr, 'external target cleanup succeeds')
    ierr = fx_test_remove_tree('/var/tmp/../')
    call test_assert(suite, ierr /= 0, 'cleanup rejects parent traversal')

    call system_clock(count=before, count_rate=rate)
    ierr = fx_test_sleep_ms(40_c_int64_t)
    call system_clock(count=after)
    call test_assert_equal_int(suite, 0, ierr, 'native sleep succeeds')
    call test_assert(suite, after - before >= rate*40_c_int64_t/1000, &
        'independent monotonic clock observes requested delay')
    ierr = fx_test_sleep_ms(-1_c_int64_t)
    call test_assert(suite, ierr /= 0, 'negative sleep duration fails')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine write_file(path, text)
        character(len=*), intent(in) :: path, text
        integer :: output

        open (newunit=output, file=path, status='replace', action='write')
        write (output, '(a)') text
        close (output)
    end subroutine write_file
end program test_native_fs
