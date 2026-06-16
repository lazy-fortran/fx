program test_cache
    use fx_cache, only: cache_t, cache_init, cache_key, cache_has, &
                        cache_store, cache_restore, cache_store_bytes, &
                        cache_restore_bytes, cache_evict, cache_gc, &
                        cache_stats
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
                       test_assert_equal_int, test_assert_equal_str, &
                       test_suite_summary, test_suite_exit
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
                                           c_null_char
    implicit none

    interface
        integer(c_int) function fx_c_set_mtime(path, mtime) bind(C)
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), value :: mtime
        end function fx_c_set_mtime

        integer(c_long_long) function fx_c_unix_time() bind(C)
            import :: c_long_long
        end function fx_c_unix_time
    end interface

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_cache')
    call test_cache_init(suite)
    call test_cache_key_separator(suite)
    call test_cache_key_empty_input(suite)
    call test_cache_store_restore(suite)
    call test_cache_store_bytes_restore_bytes(suite)
    call test_cache_evict(suite)
    call test_cache_gc_lru_and_stats(suite)
    call test_cache_gc_temp_cleanup(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_cache_init(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        logical :: exists

        root = temp_root('init')
        call cleanup_tree(root)

        call cache_init(cache, root)

        inquire(file=trim(root), exist=exists)
        call test_assert(suite, exists, 'cache_init creates root dir')
        call test_assert(suite, cache%initialized, &
                         'cache_init marks initialized')
        call test_assert_equal_str(suite, trim(root), trim(cache%root_dir), &
                                   'cache_init stores root path')

        call cleanup_tree(root)
    end subroutine test_cache_init

    subroutine test_cache_key_separator(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=2) :: parts_a(2) = [character(len=2) :: 'ab', 'c ']
        character(len=2) :: parts_b(2) = [character(len=2) :: 'a ', 'bc']
        character(len=1) :: parts_empty(2) = [character(len=1) :: ' ', 'b']
        character(len=1) :: parts_single(1) = [character(len=1) :: 'b']
        character(len=64) :: key_a
        character(len=64) :: key_b
        character(len=64) :: key_empty
        character(len=64) :: key_single

        key_a = cache_key(parts_a, 2)
        key_b = cache_key(parts_b, 2)
        key_empty = cache_key(parts_empty, 2)
        key_single = cache_key(parts_single, 1)

        call test_assert(suite, len_trim(key_a) == 16, &
                         'cache_key emits 16 hex digits')
        call test_assert(suite, is_hex_string(trim(key_a)), &
                         'cache_key returns hex')
        call test_assert(suite, trim(key_a) /= trim(key_b), &
                         'cache_key separates adjacent parts')
        call test_assert(suite, trim(key_empty) /= trim(key_single), &
                         'cache_key keeps empty part distinct')
        call test_assert_equal_str(suite, trim(key_a), &
                                   trim(cache_key(parts_a, 2)), &
                                   'cache_key deterministic')
    end subroutine test_cache_key_separator

    subroutine test_cache_key_empty_input(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=64) :: key
        character(len=1), allocatable :: parts(:)

        allocate(parts(0))
        key = cache_key(parts, 0)

        call test_assert(suite, len_trim(key) == 16, &
                         'cache_key zero parts length')
        call test_assert(suite, is_hex_string(trim(key)), &
                         'cache_key zero parts hex')
    end subroutine test_cache_key_empty_input

    subroutine test_cache_store_restore(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        character(len=:), allocatable :: source_path
        character(len=:), allocatable :: dest_path
        character(len=:), allocatable :: restored
        character(len=:), allocatable :: source_text
        character(len=64) :: key
        integer :: ierr
        logical :: exists

        root = temp_root('store')
        call cleanup_tree(root)
        call cache_init(cache, root)

        source_path = join_path(root, 'src/input.f90')
        dest_path = join_path(root, 'out/restored.f90')
        source_text = 'module cache_roundtrip' // new_line('a') // &
                      'contains' // new_line('a') // &
                      'end module cache_roundtrip' // new_line('a')
        key = 'aa0123456789abcd'

        call write_text_file(source_path, source_text, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_store source setup')

        call write_text_file(dest_path, 'old destination', ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_store dest setup')

        call cache_store(cache, key, source_path, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_store ierr')
        call test_assert(suite, cache_has(cache, key), 'cache_has after store')
        call test_assert(suite, path_exists(join_path(root, 'aa')), &
                         'cache_store creates prefix dir')
        call test_assert(suite, path_exists(cache_entry_path(root, key)), &
                         'cache_store writes cache entry')

        call cache_restore(cache, key, dest_path, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_restore ierr')
        call read_text_file(dest_path, restored, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_restore read back')
        call test_assert_equal_str(suite, source_text, restored, &
                                   'cache_restore copies exact content')

        call cleanup_tree(root)
    end subroutine test_cache_store_restore

    subroutine test_cache_store_bytes_restore_bytes(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        character(len=1), allocatable :: payload(:)
        character(len=1), allocatable :: roundtrip(:)
        character(len=1), allocatable :: empty_in(:)
        character(len=1), allocatable :: empty_out(:)
        character(len=64) :: key
        integer :: ierr
        integer :: n_bytes

        root = temp_root('bytes')
        call cleanup_tree(root)
        call cache_init(cache, root)

        allocate(payload(4))
        payload(1) = 'A'
        payload(2) = achar(0)
        payload(3) = 'B'
        payload(4) = 'Z'
        allocate(roundtrip(4))
        roundtrip = '?'
        key = 'bb0123456789abcd'

        call cache_store_bytes(cache, key, payload, 4, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_store_bytes ierr')
        call cache_restore_bytes(cache, key, roundtrip, n_bytes, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_restore_bytes ierr')
        call test_assert_equal_int(suite, 4, n_bytes, &
                                   'cache_restore_bytes size')
        call test_assert(suite, bytes_equal(payload, roundtrip, 4), &
                         'cache_store_bytes roundtrip')

        allocate(empty_in(0))
        allocate(empty_out(0))
        key = 'bbfedcba98765432'
        call cache_store_bytes(cache, key, empty_in, 0, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_store_bytes empty')
        call cache_restore_bytes(cache, key, empty_out, n_bytes, ierr)
        call test_assert_equal_int(suite, 0, ierr, &
                                   'cache_restore_bytes empty ierr')
        call test_assert_equal_int(suite, 0, n_bytes, &
                                   'cache_restore_bytes empty size')

        call cleanup_tree(root)
    end subroutine test_cache_store_bytes_restore_bytes

    subroutine test_cache_evict(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        character(len=:), allocatable :: source_path
        character(len=64) :: key
        integer :: ierr

        root = temp_root('evict')
        call cleanup_tree(root)
        call cache_init(cache, root)

        source_path = join_path(root, 'payload.txt')
        key = 'cc0123456789abcd'

        call write_text_file(source_path, 'payload', ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_evict source setup')
        call cache_store(cache, key, source_path, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_evict store ierr')
        call test_assert(suite, cache_has(cache, key), 'cache_evict has stored')

        call cache_evict(cache, key, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_evict ierr')
        call test_assert(suite, .not. cache_has(cache, key), &
                         'cache_evict removes entry')
        call test_assert(suite, .not. path_exists(cache_entry_path(root, key)), &
                         'cache_evict deletes file')

        call cache_evict(cache, key, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_evict missing ok')

        call cleanup_tree(root)
    end subroutine test_cache_evict

    subroutine test_cache_gc_lru_and_stats(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        character(len=1), allocatable :: payload(:)
        character(len=64) :: oldest_key
        character(len=64) :: middle_key
        character(len=64) :: newest_key
        integer :: ierr
        integer :: n_evicted
        integer :: n_entries
        integer :: total_size_mb

        root = temp_root('gc')
        call cleanup_tree(root)
        call cache_init(cache, root)

        allocate(payload(400000))
        payload = 'A'

        oldest_key = 'aa00000000000000'
        middle_key = 'bb00000000000000'
        newest_key = 'cc00000000000000'

        call cache_store_bytes(cache, oldest_key, payload, size(payload), ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc oldest store')
        call wait_seconds(1, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc wait oldest')
        call cache_store_bytes(cache, middle_key, payload, size(payload), ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc middle store')
        call wait_seconds(1, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc wait middle')
        call cache_store_bytes(cache, newest_key, payload, size(payload), ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc newest store')

        call cache_stats(cache, n_entries, total_size_mb)
        call test_assert_equal_int(suite, 3, n_entries, 'cache_stats count pre')
        call test_assert_equal_int(suite, 1, total_size_mb, &
                                   'cache_stats size pre')

        call cache_gc(cache, 1, n_evicted)
        call test_assert_equal_int(suite, 1, n_evicted, 'cache_gc evicted one')
        call test_assert(suite, .not. cache_has(cache, oldest_key), &
                         'cache_gc evicts oldest')
        call test_assert(suite, cache_has(cache, middle_key), &
                         'cache_gc keeps middle')
        call test_assert(suite, cache_has(cache, newest_key), &
                         'cache_gc keeps newest')
        call test_assert(suite, .not. path_exists(join_path(root, 'aa')), &
                         'cache_gc removes empty prefix dir')

        call cache_stats(cache, n_entries, total_size_mb)
        call test_assert_equal_int(suite, 2, n_entries, 'cache_stats count post')
        call test_assert_equal_int(suite, 0, total_size_mb, &
                                   'cache_stats size post')

        call cleanup_tree(root)
    end subroutine test_cache_gc_lru_and_stats

    subroutine test_cache_gc_temp_cleanup(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(len=:), allocatable :: root
        character(len=:), allocatable :: temp_dir
        character(len=:), allocatable :: temp_path
        character(len=1), allocatable :: payload(:)
        character(len=64) :: real_key
        integer :: ierr
        integer :: n_evicted
        integer :: n_entries
        integer :: total_size_mb

        root = temp_root('tmpgc')
        call cleanup_tree(root)
        call cache_init(cache, root)

        real_key = 'dd00000000000000'
        temp_dir = join_path(root, 'tt')
        temp_path = join_path(temp_dir, '.tmp.old.123')

        allocate(payload(1))
        payload(1) = 'R'
        call cache_store_bytes(cache, real_key, payload, 1, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc temp real store')

        call ensure_dir(temp_dir, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc temp dir create')
        call write_bytes_file(temp_path, payload, 1, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc temp write')
        call touch_older(temp_path, 2, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'cache_gc temp age')

        call cache_gc(cache, 100, n_evicted)
        call test_assert_equal_int(suite, 1, n_evicted, &
                                   'cache_gc temp evicts stale tmp file')
        call test_assert(suite, cache_has(cache, real_key), &
                         'cache_gc temp keeps real entry')
        call test_assert(suite, .not. path_exists(temp_path), &
                         'cache_gc temp removes stale tmp file')
        call test_assert(suite, .not. path_exists(temp_dir), &
                         'cache_gc temp removes empty temp dir')

        call cache_stats(cache, n_entries, total_size_mb)
        call test_assert_equal_int(suite, 1, n_entries, &
                                   'cache_gc temp stats count')
        call test_assert_equal_int(suite, 0, total_size_mb, &
                                   'cache_gc temp stats size')

        call cleanup_tree(root)
    end subroutine test_cache_gc_temp_cleanup

    function cache_entry_path(root, key) result(path)
        character(len=*), intent(in) :: root
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: path

        path = join_path(join_path(root, trim(key(1:2))), trim(key))
    end function cache_entry_path

    function join_path(left, right) result(path)
        character(len=*), intent(in) :: left
        character(len=*), intent(in) :: right
        character(len=:), allocatable :: path
        integer :: n_left

        n_left = len_trim(left)
        if (n_left == 0) then
            path = trim(right)
        else if (left(n_left:n_left) == '/') then
            path = trim(left) // trim(right)
        else
            path = trim(left) // '/' // trim(right)
        end if
    end function join_path

    logical function path_exists(path)
        character(len=*), intent(in) :: path
        logical :: exists

        inquire(file=trim(path), exist=exists)
        path_exists = exists
    end function path_exists

    subroutine cleanup_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf -- ' // trim(path))
    end subroutine cleanup_tree

    subroutine ensure_dir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        integer :: exitstat
        integer :: cmdstat
        character(len=256) :: cmdmsg

        call execute_command_line('mkdir -p -- ' // trim(path), &
                                  exitstat=exitstat, cmdstat=cmdstat, &
                                  cmdmsg=cmdmsg)
        if (cmdstat == 0 .and. exitstat == 0) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine ensure_dir

    subroutine write_text_file(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: text
        integer, intent(out) :: ierr
        character(len=1), allocatable :: bytes(:)

        call text_to_bytes(text, bytes)
        call write_bytes_file(path, bytes, size(bytes), ierr)
    end subroutine write_text_file

    subroutine write_bytes_file(path, bytes, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=1), intent(in) :: bytes(:)
        integer, intent(in) :: n_bytes
        integer, intent(out) :: ierr
        integer :: unit
        integer :: ios

        ierr = 0
        if (n_bytes < 0 .or. n_bytes > size(bytes)) then
            ierr = 1
            return
        end if

        if (n_bytes > 0) then
            call ensure_dir(parent_dir(path), ierr)
            if (ierr /= 0) return
        else
            call ensure_dir(parent_dir(path), ierr)
            if (ierr /= 0) return
        end if

        open(newunit=unit, file=trim(path), access='stream', &
             form='unformatted', status='replace', action='write', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        if (n_bytes > 0) then
            write(unit, iostat=ios) bytes(1:n_bytes)
        end if
        close(unit)
        if (ios /= 0) ierr = 1
    end subroutine write_bytes_file

    subroutine read_text_file(path, text, ierr)
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: text
        integer, intent(out) :: ierr
        character(len=1), allocatable :: bytes(:)
        integer :: n_bytes

        call read_bytes_from_file(path, bytes, n_bytes, ierr)
        if (ierr /= 0) then
            text = ''
            return
        end if

        call bytes_to_text(bytes, n_bytes, text)
    end subroutine read_text_file

    subroutine read_bytes_from_file(path, bytes, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=1), allocatable, intent(out) :: bytes(:)
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr
        integer :: unit
        integer :: ios
        logical :: exists

        inquire(file=trim(path), exist=exists)
        if (.not. exists) then
            ierr = 1
            n_bytes = 0
            allocate(bytes(0))
            return
        end if

        inquire(file=trim(path), size=n_bytes)
        if (n_bytes < 0) then
            ierr = 1
            allocate(bytes(0))
            return
        end if

        allocate(bytes(max(n_bytes, 0)))
        open(newunit=unit, file=trim(path), access='stream', &
             form='unformatted', status='old', action='read', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate(bytes)
            allocate(bytes(0))
            n_bytes = 0
            return
        end if

        if (n_bytes > 0) then
            read(unit, iostat=ios) bytes(1:n_bytes)
        end if
        close(unit)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate(bytes)
            allocate(bytes(0))
            n_bytes = 0
            return
        end if

        ierr = 0
    end subroutine read_bytes_from_file

    subroutine bytes_to_text(bytes, n_bytes, text)
        character(len=1), intent(in) :: bytes(:)
        integer, intent(in) :: n_bytes
        character(len=:), allocatable, intent(out) :: text
        integer :: i

        if (n_bytes <= 0) then
            allocate(character(len=0) :: text)
            return
        end if

        allocate(character(len=n_bytes) :: text)
        do i = 1, n_bytes
            text(i:i) = bytes(i)
        end do
    end subroutine bytes_to_text

    subroutine text_to_bytes(text, bytes)
        character(len=*), intent(in) :: text
        character(len=1), allocatable, intent(out) :: bytes(:)
        integer :: i

        allocate(bytes(len(text)))
        do i = 1, len(text)
            bytes(i) = text(i:i)
        end do
    end subroutine text_to_bytes

    logical function bytes_equal(lhs, rhs, n_bytes) result(equal)
        character(len=1), intent(in) :: lhs(:)
        character(len=1), intent(in) :: rhs(:)
        integer, intent(in) :: n_bytes
        integer :: i

        equal = .true.
        if (n_bytes < 0) then
            equal = .false.
            return
        end if
        if (size(lhs) < n_bytes .or. size(rhs) < n_bytes) then
            equal = .false.
            return
        end if
        do i = 1, n_bytes
            if (lhs(i) /= rhs(i)) then
                equal = .false.
                return
            end if
        end do
    end function bytes_equal

    logical function is_hex_string(text) result(ok)
        character(len=*), intent(in) :: text

        ok = verify(trim(text), '0123456789abcdef') == 0
    end function is_hex_string

    subroutine wait_seconds(seconds, ierr)
        integer, intent(in) :: seconds
        integer, intent(out) :: ierr
        integer :: exitstat
        integer :: cmdstat
        character(len=256) :: cmdmsg
        character(len=16) :: seconds_text

        write(seconds_text, '(I0)') seconds
        call execute_command_line('sleep ' // trim(seconds_text), &
                                  exitstat=exitstat, cmdstat=cmdstat, &
                                  cmdmsg=cmdmsg)
        if (cmdstat == 0 .and. exitstat == 0) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine wait_seconds

    subroutine touch_older(path, hours, ierr)
        character(len=*), intent(in) :: path
        integer, intent(in) :: hours
        integer, intent(out) :: ierr
        integer(c_long_long) :: target_mtime

        ! Set mtime directly via utimes() for cross-platform behavior; BSD
        ! touch on macOS does not accept GNU's `-d "N hours ago"`.
        target_mtime = fx_c_unix_time() - int(hours, c_long_long) * 3600_c_long_long
        ierr = int(fx_c_set_mtime(path // c_null_char, target_mtime))
    end subroutine touch_older

    function parent_dir(path) result(dir)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: dir
        integer :: i

        i = len_trim(path)
        do while (i > 1 .and. path(i:i) == '/')
            i = i - 1
        end do
        do while (i > 1 .and. path(i:i) /= '/')
            i = i - 1
        end do
        if (i <= 1) then
            dir = '/'
        else
            dir = path(:i - 1)
        end if
    end function parent_dir

    function temp_root(tag) result(path)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: path
        integer, save :: counter = 0
        character(len=32) :: counter_text

        counter = counter + 1
        write(counter_text, '(I0)') counter
        path = '/tmp/fx-cache-' // trim(tag) // '-' // trim(counter_text)
    end function temp_root

end program test_cache
