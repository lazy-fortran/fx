program test_immutable_manifest_index
    use fx_immutable_manifest, only: immutable_tree_entry_t, &
        immutable_entries_canonical, &
        immutable_manifest_serialize, IMMUTABLE_BLOB
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_INVALID
    use fx_hash, only: sha256_string
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, &
        test_suite_summary, test_suite_exit
    implicit none
    type(test_suite_t) :: suite
    type(immutable_tree_entry_t) :: pair(2)
    type(immutable_tree_entry_t), allocatable :: sorted(:), large(:)
    character(len=:), allocatable :: expected, encoded
    character(len=64) :: digest
    character(len=32) :: name
    integer, parameter :: count = 62922
    integer :: i, status

    call test_suite_init(suite, 'immutable_manifest_index')
    pair(1) = fixture('alpha')
    pair(2) = fixture('Zeta')
    call immutable_entries_canonical(pair, sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, &
        'distinct spellings accepted')
    expected = 'FXTREE1'//achar(10)//'B'//achar(9)//'644'//achar(9)// &
        'source'//achar(9)//'Zeta'//achar(9)//repeat('a', 64)//achar(10)// &
        'B'//achar(9)//'644'//achar(9)//'source'//achar(9)//'alpha'//achar(9)// &
        repeat('a', 64)//achar(10)
    call test_assert_equal_str(suite, expected, immutable_manifest_serialize(sorted), &
        'native equivalence indexing preserves exact byte ordering and encoding')
    call test_assert_equal_str(suite, sha256_string(expected), &
        sha256_string(immutable_manifest_serialize(sorted)), &
        'canonical digest unchanged')
    pair(1)%path = 'Alpha'
    pair(2)%path = 'Alpha'
    call rejected(pair, 'exact duplicate rejected')
    pair(2)%path = 'alpha'
    call rejected(pair, 'case alias separated in byte order rejected')
    pair(2)%path = 'Alphax'
    call immutable_entries_canonical(pair, sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, &
        'near-collision prefix accepted')
    pair(2)%path = 'alpha0'
    call immutable_entries_canonical(pair, sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, 'folded prefix accepted')
    pair(2)%path = 'nul'//achar(0)//'suffix'
    call rejected(pair, 'embedded NUL rejected before native name comparison')
    pair(2)%path = '../escape'
    call rejected(pair, 'non-component rejected')
    pair(2) = fixture('valid')
    pair(2)%role = 'bad role'
    call rejected(pair, 'invalid role rejected')
    pair(2) = fixture('valid')
    pair(2)%mode = 512
    call rejected(pair, 'invalid permission rejected')
    pair(2) = fixture('valid')
    pair(2)%kind = 3
    call rejected(pair, 'invalid kind rejected')
    pair(2) = fixture('valid')
    pair(2)%object_id = repeat('z', 64)
    call rejected(pair, 'invalid object ID rejected')
    call immutable_entries_canonical(pair(:0), sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, 'empty manifest accepted')

    allocate (large(count))
    do i = 1, count
        write (name, '("entry-",i8.8)') count + 1 - i
        large(i) = fixture(trim(name))
    end do
    call immutable_entries_canonical(large, sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, &
        'large consumer-sized inventory canonicalizes')
    call test_assert_equal_str(suite, 'entry-00000001', sorted(1)%path, &
        'large inventory retains first canonical entry')
    call test_assert_equal_str(suite, 'entry-00062922', sorted(count)%path, &
        'large inventory retains final canonical entry')
    encoded = immutable_manifest_serialize(sorted)
    digest = sha256_string(encoded)
    large(1)%mode = 384
    call immutable_entries_canonical(large, sorted, status)
    call test_assert_equal_int(suite, IMMUTABLE_OK, status, &
        'final-entry mutation accepted')
    call test_assert(suite, &
        sha256_string(immutable_manifest_serialize(sorted)) /= digest, &
        'final-entry mutation changes complete manifest digest')
    large(1)%path = 'ENTRY-00000001'
    call rejected(large, 'large inventory rejects distant case alias')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    function fixture(path) result(entry)
        character(len=*), intent(in) :: path
        type(immutable_tree_entry_t) :: entry
        entry%path = path
        entry%role = 'source'
        entry%object_id = repeat('a', 64)
        entry%mode = 420
        entry%kind = IMMUTABLE_BLOB
    end function fixture

    subroutine rejected(input, label)
        type(immutable_tree_entry_t), intent(in) :: input(:)
        character(len=*), intent(in) :: label
        type(immutable_tree_entry_t), allocatable :: output(:)
        integer :: ierr
        call immutable_entries_canonical(input, output, ierr)
        call test_assert_equal_int(suite, IMMUTABLE_INVALID, ierr, label)
    end subroutine rejected
end program test_immutable_manifest_index
