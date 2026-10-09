program test_hash
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, &
        test_suite_summary, test_suite_exit, &
        test_assert, test_assert_equal_str, &
        test_assert_equal_int
    use fx_hash, only: fnv1a_string, fnv1a_file, xxhash64, xxhash64_file, &
        sha256_bytes, sha256_string, sha256_file, &
        sha256_init, sha256_update, sha256_final, &
        sha256_hardware_available, sha256_hardware_digest, &
        sha256_state_t, &
        hash_to_hex, hash_combine, &
        hash_state_init, hash_state_update, hash_state_final, &
        hash_state_t
    use fx_proc, only: proc_file_write, proc_pid
    use fx_test_fs, only: fx_test_temp_root, fx_test_mkdir_p, fx_test_remove_tree
    implicit none

    type(test_suite_t) :: suite
    character(:), allocatable :: root
    integer :: cleanup_status

    call test_suite_init(suite, 'fx_hash')
    call setup_fixture()
    call test_fnv1a_known_values(suite)
    call test_fnv1a_file(suite)
    call test_xxhash64(suite)
    call test_sha256(suite)
    call test_native_file_bytes(suite)
    call test_hash_combine(suite)
    call test_hash_to_hex(suite)
    call test_incremental_hash(suite)
    cleanup_status = fx_test_remove_tree(root)
    call test_assert_equal_int(suite, 0, cleanup_status, 'remove owned hash fixture')
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine setup_fixture()
        character(kind=c_char) :: native_temporary(4096)
        character(len=4096) :: temporary
        character(len=32) :: pid
        integer :: ierr, i

        call get_environment_variable('TMPDIR', temporary, status=ierr)
        if (ierr /= 0 .or. len_trim(temporary) == 0) then
            ierr = fx_test_temp_root(native_temporary, size(native_temporary))
            call test_assert_equal_int(suite, 0, ierr, 'resolve native temporary root')
            if (ierr /= 0) call test_suite_exit(suite)
            temporary = ''
            do i = 1, size(native_temporary)
                if (native_temporary(i) == c_null_char) exit
                temporary(i:i) = native_temporary(i)
            end do
        end if
        write (pid, '(i0)') proc_pid()
        root = trim(temporary)//'/fx-hash-'//trim(pid)
        ierr = fx_test_mkdir_p(root)
        call test_assert_equal_int(suite, 0, ierr, 'create owned hash fixture')
        if (ierr /= 0) call test_suite_exit(suite)
    end subroutine setup_fixture

    subroutine test_native_file_bytes(suite)
        type(test_suite_t), intent(inout) :: suite
        character(:), allocatable :: directory, path, payload
        character(len=64) :: digest
        character :: bytes(6)
        integer(int64) :: hash
        integer :: ierr, i

        directory = root//'/café-試驗'
        do while (len(directory) < 530)
            directory = directory//'/'//repeat('d', 48)
        end do
        ierr = fx_test_mkdir_p(directory)
        call test_assert_equal_int(suite, 0, ierr, 'create long Unicode hash path')
        path = directory//'/binary'
        payload = 'f'//achar(0)//'o'//char(255)//achar(13)//achar(10)
        call proc_file_write(path, payload, len(payload), ierr)
        call test_assert_equal_int(suite, 0, ierr, 'write exact native binary bytes')
        call sha256_file(path, digest, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'hash complete long Unicode path')
        call test_assert_equal_str(suite, &
            'b80ec9b42c78d8643f12c5fd1ba347745'// &
            'ac34a46a09dae0f68c6834cf41bbd9f', digest, &
            'SHA256 matches independent NUL/high-byte/CRLF vector')
        call fnv1a_file(path, hash, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'FNV reads native long path')
        call test_assert(suite, hash == fnv1a_string(payload), 'FNV preserves bytes')
        do i = 1, size(bytes)
            bytes(i) = payload(i:i)
        end do
        call xxhash64_file(path, hash, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'XXH reads native long path')
        call test_assert(suite, hash == xxhash64(bytes, size(bytes), 0_int64), &
            'XXH preserves bytes')

        path = directory//'/empty'
        call proc_file_write(path, '', 0, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'write native empty file')
        call sha256_file(path, digest, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'hash native empty file')
        call test_assert_equal_str(suite, &
            'e3b0c44298fc1c149afbf4c8996fb924'// &
            '27ae41e4649b934ca495991b7852b855', digest, 'empty native hash vector')
        call sha256_file(directory, digest, ierr)
        call test_assert(suite, ierr /= 0 .and. len_trim(digest) == 0, &
            'directory is an error rather than an empty file')
        call sha256_file(directory//'/missing', digest, ierr)
        call test_assert(suite, ierr /= 0 .and. len_trim(digest) == 0, &
            'missing long path returns an error and empty digest')
    end subroutine test_native_file_bytes

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
        integer :: ierr

        ! Write known content to a temp file
        call proc_file_write(root//'/fnv1a.bin', 'foobar', 6, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'write FNV fixture')

        call fnv1a_file(root//'/fnv1a.bin', h_file, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'fnv1a_file: no error')
        h_str = fnv1a_string('foobar')
        call test_assert(suite, h_file == h_str, 'fnv1a_file: matches string hash')

        ! Non-existent file returns error
        call fnv1a_file(root//'/missing-fnv.bin', h_file, ierr)
        call test_assert(suite, ierr /= 0, 'fnv1a_file: error on missing file')
    end subroutine test_fnv1a_file

    subroutine test_xxhash64(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=1), allocatable :: data(:)
        integer(int64) :: h1, h2

        ! Empty input, seed 0: known vector ef46db3751d8e999
        allocate (data(0))
        h1 = xxhash64(data, 0, 0_int64)
        call test_assert_equal_str(suite, 'ef46db3751d8e999', hash_to_hex(h1), &
            'xxhash64: empty seed 0')
        deallocate (data)

        ! Different inputs produce different hashes
        allocate (data(3))
        data(1) = 'f'; data(2) = 'o'; data(3) = 'o'
        h1 = xxhash64(data, 3, 0_int64)
        deallocate (data)

        allocate (data(3))
        data(1) = 'b'; data(2) = 'a'; data(3) = 'r'
        h2 = xxhash64(data, 3, 0_int64)
        deallocate (data)

        call test_assert(suite, h1 /= h2, 'xxhash64: distinct inputs differ')

        ! Seed changes output
        allocate (data(3))
        data(1) = 'f'; data(2) = 'o'; data(3) = 'o'
        h2 = xxhash64(data, 3, 1_int64)
        deallocate (data)
        call test_assert(suite, h1 /= h2, 'xxhash64: seed changes output')

        ! Large input (> 32 bytes) exercises the 4-lane path
        allocate (data(40))
        data = 'x'
        h1 = xxhash64(data, 40, 0_int64)
        call test_assert(suite, h1 /= 0_int64, 'xxhash64: 40-byte nonzero')
        deallocate (data)
    end subroutine test_xxhash64

    subroutine test_sha256(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=1), allocatable :: data(:)
        character(len=64) :: h_empty, h_foo, h_file, h_stream
        character(len=64) :: h_hw
        type(sha256_state_t) :: state
        character(len=1) :: chunk(3)
        integer :: ierr, i

        allocate (data(0))
        h_empty = sha256_bytes(data, 0)
        deallocate (data)

        call test_assert_equal_int(suite, 64, len(h_empty), &
            'sha256: 64 hex chars')
        call test_assert_equal_str(suite, &
            'e3b0c44298fc1c149afbf4c8996fb924'// &
            '27ae41e4649b934ca495991b7852b855', &
            h_empty, 'sha256: empty vector')

        h_foo = sha256_string('foo')
        call test_assert(suite, h_foo /= h_empty, &
            'sha256: distinct input differs')
        chunk(1) = 'f'; chunk(2) = 'o'; chunk(3) = 'o'
        call test_assert_equal_str(suite, &
            '2c26b46b68ffc68ff99b453c1d304134'// &
            '13422d706483bfa0f98a5e886266e7ae', &
            h_foo, 'sha256: foo vector')

        call sha256_init(state)
        chunk(1) = 'f'
        call sha256_update(state, chunk, 1)
        chunk(1) = 'o'; chunk(2) = 'o'
        call sha256_update(state, chunk, 2)
        h_stream = sha256_final(state)
        call test_assert_equal_str(suite, h_foo, h_stream, &
            'sha256: streaming chunks match one-shot')

        call proc_file_write(root//'/wide.bin', 'foo', 3, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'write SHA fixture')
        call sha256_file(root//'/wide.bin', h_file, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'sha256_file: no error')
        call test_assert_equal_str(suite, h_foo, h_file, &
            'sha256_file: matches string hash')

        do i = 1, len(h_foo)
            call test_assert(suite, &
                (iachar(h_foo(i:i)) >= iachar('0') .and. &
                iachar(h_foo(i:i)) <= iachar('9')) .or. &
                (iachar(h_foo(i:i)) >= iachar('a') .and. &
                iachar(h_foo(i:i)) <= iachar('f')), &
                'sha256: lowercase hex chars')
        end do

        call sha256_file(root//'/missing-sha.bin', h_file, ierr)
        call test_assert(suite, ierr /= 0, 'sha256_file: missing file errors')
        chunk(1) = 'f'; chunk(2) = 'o'; chunk(3) = 'o'
        if (sha256_hardware_available()) then
            call test_assert(suite, sha256_hardware_digest(chunk, 3, h_hw), &
                'sha256 hardware digest succeeds when available')
            call test_assert_equal_str(suite, h_foo, h_hw, &
                'sha256 hardware digest matches scalar')
        else
            call test_assert(suite, .not. sha256_hardware_digest(chunk, 3, h_hw), &
                'sha256 hardware digest disabled when unavailable')
        end if
    end subroutine test_sha256

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
