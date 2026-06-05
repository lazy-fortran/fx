program test_hash
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit, &
                       test_assert, test_assert_equal_str, &
                       test_assert_equal_int
    use fx_hash, only: fnv1a_string, fnv1a_file, xxhash64, xxhash64_file, &
                       hash_to_hex, hash_combine, &
                       hash_state_init, hash_state_update, hash_state_final, &
                       hash_state_t
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_hash')
    call test_fnv1a_known_values(suite)
    call test_fnv1a_file(suite)
    call test_xxhash64(suite)
    call test_hash_combine(suite)
    call test_hash_to_hex(suite)
    call test_incremental_hash(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_fnv1a_known_values(suite)
        type(test_suite_t), intent(inout) :: suite
        integer(int64) :: h

        ! Known FNV-1a 64-bit vectors (FNV offset basis = 14695981039346656037)
        h = fnv1a_string('')
        call test_assert_equal_str(suite, 'cbf29ce484222325', hash_to_hex(h), &
                                   'fnv1a: empty string')

        h = fnv1a_string('a')
        call test_assert_equal_str(suite, 'af63dc4c8601ec8c', hash_to_hex(h), &
                                   'fnv1a: "a"')

        h = fnv1a_string('foobar')
        call test_assert_equal_str(suite, '85944171f73967e8', hash_to_hex(h), &
                                   'fnv1a: "foobar"')

        ! Two different strings produce different hashes
        call test_assert(suite, fnv1a_string('foo') /= fnv1a_string('bar'), &
                         'fnv1a: collision avoidance')
    end subroutine test_fnv1a_known_values

    subroutine test_fnv1a_file(suite)
        type(test_suite_t), intent(inout) :: suite
        integer(int64) :: h_file, h_str
        integer :: ierr, unit

        ! Write known content to a temp file
        open(newunit=unit, file='/tmp/fx_test_hash_fnv1a.bin', &
             access='stream', form='unformatted', status='replace')
        write(unit) 'foobar'
        close(unit)

        call fnv1a_file('/tmp/fx_test_hash_fnv1a.bin', h_file, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'fnv1a_file: no error')
        h_str = fnv1a_string('foobar')
        call test_assert(suite, h_file == h_str, 'fnv1a_file: matches string hash')

        ! Non-existent file returns error
        call fnv1a_file('/tmp/fx_nonexistent_xyz.bin', h_file, ierr)
        call test_assert(suite, ierr /= 0, 'fnv1a_file: error on missing file')
    end subroutine test_fnv1a_file

    subroutine test_xxhash64(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=1), allocatable :: data(:)
        integer(int64) :: h1, h2

        ! Empty input, seed 0: known vector ef46db3751d8e999
        allocate(data(0))
        h1 = xxhash64(data, 0, 0_int64)
        call test_assert_equal_str(suite, 'ef46db3751d8e999', hash_to_hex(h1), &
                                   'xxhash64: empty seed 0')
        deallocate(data)

        ! Different inputs produce different hashes
        allocate(data(3))
        data(1) = 'f'; data(2) = 'o'; data(3) = 'o'
        h1 = xxhash64(data, 3, 0_int64)
        deallocate(data)

        allocate(data(3))
        data(1) = 'b'; data(2) = 'a'; data(3) = 'r'
        h2 = xxhash64(data, 3, 0_int64)
        deallocate(data)

        call test_assert(suite, h1 /= h2, 'xxhash64: distinct inputs differ')

        ! Seed changes output
        allocate(data(3))
        data(1) = 'f'; data(2) = 'o'; data(3) = 'o'
        h2 = xxhash64(data, 3, 1_int64)
        deallocate(data)
        call test_assert(suite, h1 /= h2, 'xxhash64: seed changes output')

        ! Large input (> 32 bytes) exercises the 4-lane path
        allocate(data(40))
        data = 'x'
        h1 = xxhash64(data, 40, 0_int64)
        call test_assert(suite, h1 /= 0_int64, 'xxhash64: 40-byte nonzero')
        deallocate(data)
    end subroutine test_xxhash64

    subroutine test_hash_combine(suite)
        type(test_suite_t), intent(inout) :: suite
        integer(int64) :: ha, hb, hc

        ha = fnv1a_string('hello')
        hb = fnv1a_string('world')

        ! Combine is deterministic
        hc = hash_combine(ha, hb)
        call test_assert(suite, hc == hash_combine(ha, hb), &
                         'hash_combine: deterministic')

        ! Combine differs from inputs
        call test_assert(suite, hc /= ha, 'hash_combine: differs from ha')
        call test_assert(suite, hc /= hb, 'hash_combine: differs from hb')

        ! Order matters (not commutative in general)
        call test_assert(suite, hash_combine(ha, hb) /= hash_combine(hb, ha), &
                         'hash_combine: order matters')
    end subroutine test_hash_combine

    subroutine test_hash_to_hex(suite)
        type(test_suite_t), intent(inout) :: suite

        ! All zeros
        call test_assert_equal_str(suite, '0000000000000000', &
                                   hash_to_hex(0_int64), 'hash_to_hex: zero')

        ! Known value: -1 = 0xFFFFFFFFFFFFFFFF
        call test_assert_equal_str(suite, 'ffffffffffffffff', &
                                   hash_to_hex(-1_int64), &
                                   'hash_to_hex: minus one')

        ! Length is always 16
        call test_assert_equal_int(suite, 16, &
                                   len(hash_to_hex(fnv1a_string('test'))), &
                                   'hash_to_hex: always 16 chars')

        ! Output is lowercase hex
        block
            character(len=16) :: h
            integer :: i
            h = hash_to_hex(fnv1a_string('foobar'))
            do i = 1, 16
                call test_assert(suite, &
                    (iachar(h(i:i)) >= iachar('0') .and. &
                     iachar(h(i:i)) <= iachar('9')) .or. &
                    (iachar(h(i:i)) >= iachar('a') .and. &
                     iachar(h(i:i)) <= iachar('f')), &
                    'hash_to_hex: lowercase hex chars')
            end do
        end block
    end subroutine test_hash_to_hex

    subroutine test_incremental_hash(suite)
        type(test_suite_t), intent(inout) :: suite
        type(hash_state_t) :: state
        character(len=1) :: bytes(3)
        integer(int64) :: h_incremental, h_full

        ! Hash "foo" incrementally byte by byte
        call hash_state_init(state)
        bytes(1) = 'f'
        call hash_state_update(state, bytes, 1)
        bytes(1) = 'o'
        call hash_state_update(state, bytes, 1)
        bytes(1) = 'o'
        call hash_state_update(state, bytes, 1)
        h_incremental = hash_state_final(state)

        ! Hash "foo" all at once
        h_full = fnv1a_string('foo')

        call test_assert(suite, h_incremental == h_full, &
                         'incremental: matches full hash')

        ! Empty state returns offset basis
        call hash_state_init(state)
        h_incremental = hash_state_final(state)
        h_full = fnv1a_string('')
        call test_assert(suite, h_incremental == h_full, &
                         'incremental: empty matches fnv1a empty')
    end subroutine test_incremental_hash

end program test_hash
