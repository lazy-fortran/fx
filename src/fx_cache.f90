module fx_cache
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_cache_fs, only: cache_t, cache_entry_t, cache_ready, CACHE_PATH_LEN, &
        cache_prefix_path, cache_entry_path, &
        cache_temp_path, cache_ensure_dir, cache_rename, &
        cache_unlink, cache_collect_entries, &
        cache_sort_entries, cache_clean_empty_dirs, &
        cache_copy_file, cache_write_bytes_file, &
        cache_read_bytes_file
    use fx_hash, only: xxhash64, hash_to_hex
    use fx_path, only: path_exists, path_normalize
    implicit none
    private

    integer(int64), parameter :: CACHE_MB_BYTES = 1048576_int64

    public :: cache_t, cache_entry_t
    public :: cache_init, cache_key, cache_has
    public :: cache_store, cache_restore
    public :: cache_store_bytes, cache_restore_bytes
    public :: cache_evict, cache_gc, cache_stats

contains

    subroutine cache_init(c, root_dir)
        type(cache_t), intent(out) :: c
        character(len=*), intent(in) :: root_dir
        character(len=:), allocatable :: normalized
        integer :: ierr

        c%root_dir = ' '
        c%initialized = .false.
        c%temp_seq = 0

        normalized = path_normalize(trim(root_dir))
        if (len_trim(normalized) == 0) return

        c%root_dir = normalized
        call cache_ensure_dir(trim(c%root_dir), ierr)
        c%initialized = (ierr == 0)
    end subroutine cache_init

    function cache_key(parts, n_parts) result(key)
        integer, intent(in) :: n_parts
        character(len=*), intent(in) :: parts(n_parts)
        character(len=64) :: key
        character(len=1), allocatable :: bytes(:)
        integer :: total_len
        integer :: i
        integer :: j
        integer :: k
        integer :: part_len
        integer(int64) :: hash

        total_len = 0
        do i = 1, n_parts
            total_len = total_len + len_trim(parts(i)) + 1
        end do

        allocate(bytes(max(total_len, 0)))
        k = 0
        do i = 1, n_parts
            part_len = len_trim(parts(i))
            do j = 1, part_len
                k = k + 1
                bytes(k) = parts(i)(j:j)
            end do
            k = k + 1
            bytes(k) = achar(0)
        end do

        hash = xxhash64(bytes, total_len, 0_int64)
        key = hash_to_hex(hash)
    end function cache_key

    logical function cache_has(c, key)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=CACHE_PATH_LEN) :: entry_path

        if (.not. cache_ready(c)) then
            cache_has = .false.
            return
        end if

        call cache_entry_path(c, key, entry_path)
        cache_has = len_trim(entry_path) > 0 .and. path_exists(trim(entry_path))
    end function cache_has

    subroutine cache_store(c, key, source_path, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: source_path
        integer, intent(out) :: ierr

        character(len=CACHE_PATH_LEN) :: entry_path
        character(len=:), allocatable :: temp_path
        integer :: cleanup_ierr

        ierr = 0
        if (.not. cache_ready(c)) then
            ierr = 1
            return
        end if

        call cache_entry_path(c, key, entry_path)
        if (len_trim(entry_path) == 0) then
            ierr = 1
            return
        end if

        call cache_ensure_dir(cache_prefix_path(c, key), ierr)
        if (ierr /= 0) return

        call cache_temp_path(c, key, temp_path)
        call cache_copy_file(source_path, temp_path, ierr)
        if (ierr /= 0) then
            call cache_unlink(temp_path, cleanup_ierr)
            return
        end if

        call cache_rename(temp_path, trim(entry_path), ierr)
        if (ierr /= 0) call cache_unlink(temp_path, cleanup_ierr)
    end subroutine cache_store

    subroutine cache_restore(c, key, dest_path, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: dest_path
        integer, intent(out) :: ierr

        character(len=CACHE_PATH_LEN) :: entry_path

        ierr = 0
        if (.not. cache_ready(c)) then
            ierr = 1
            return
        end if

        call cache_entry_path(c, key, entry_path)
        if (len_trim(entry_path) == 0) then
            ierr = 1
            return
        end if
        if (.not. path_exists(trim(entry_path))) then
            ierr = 1
            return
        end if

        call cache_copy_file(trim(entry_path), dest_path, ierr)
    end subroutine cache_restore

    subroutine cache_store_bytes(c, key, data, n_bytes, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        integer, intent(in) :: n_bytes
        character(len=1), intent(in) :: data(n_bytes)
        integer, intent(out) :: ierr

        character(len=CACHE_PATH_LEN) :: entry_path
        character(len=:), allocatable :: temp_path
        integer :: cleanup_ierr

        ierr = 0
        if (.not. cache_ready(c)) then
            ierr = 1
            return
        end if
        if (n_bytes < 0) then
            ierr = 1
            return
        end if

        call cache_entry_path(c, key, entry_path)
        if (len_trim(entry_path) == 0) then
            ierr = 1
            return
        end if

        call cache_ensure_dir(cache_prefix_path(c, key), ierr)
        if (ierr /= 0) return

        call cache_temp_path(c, key, temp_path)
        call cache_write_bytes_file(temp_path, data, n_bytes, ierr)
        if (ierr /= 0) then
            call cache_unlink(temp_path, cleanup_ierr)
            return
        end if

        call cache_rename(temp_path, trim(entry_path), ierr)
        if (ierr /= 0) call cache_unlink(temp_path, cleanup_ierr)
    end subroutine cache_store_bytes

    subroutine cache_restore_bytes(c, key, data, n_bytes, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=1), intent(out) :: data(:)
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr

        character(len=CACHE_PATH_LEN) :: entry_path

        ierr = 0
        n_bytes = 0
        if (.not. cache_ready(c)) then
            ierr = 1
            return
        end if

        call cache_entry_path(c, key, entry_path)
        if (len_trim(entry_path) == 0) then
            ierr = 1
            return
        end if
        if (.not. path_exists(trim(entry_path))) then
            ierr = 1
            return
        end if

        call cache_read_bytes_file(trim(entry_path), data, n_bytes, ierr)
    end subroutine cache_restore_bytes

    subroutine cache_evict(c, key, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        integer, intent(out) :: ierr

        character(len=CACHE_PATH_LEN) :: entry_path

        ierr = 0
        if (.not. cache_ready(c)) then
            ierr = 1
            return
        end if

        call cache_entry_path(c, key, entry_path)
        if (len_trim(entry_path) == 0) return

        call cache_unlink(trim(entry_path), ierr)
    end subroutine cache_evict

    subroutine cache_gc(c, max_size_mb, n_evicted)
        type(cache_t), intent(in) :: c
        integer, intent(in) :: max_size_mb
        integer, intent(out) :: n_evicted

        type(cache_entry_t), allocatable :: entries(:)
        integer :: n_entries
        integer :: n_temp_evicted
        integer :: i
        integer(int64) :: total_size_bytes
        integer(int64) :: max_bytes
        integer :: ierr

        n_evicted = 0
        if (.not. cache_ready(c)) return

        call cache_collect_entries(c, entries, n_entries, total_size_bytes, &
            n_temp_evicted, .true.)
        n_evicted = n_temp_evicted

        if (max_size_mb < 0) then
            max_bytes = 0_int64
        else
            max_bytes = int(max_size_mb, int64) * CACHE_MB_BYTES
        end if

        if (n_entries > 0 .and. total_size_bytes > max_bytes) then
            call cache_sort_entries(entries, n_entries)
            do i = 1, n_entries
                if (total_size_bytes <= max_bytes) exit
                call cache_unlink(entries(i)%path, ierr)
                if (ierr == 0) then
                    n_evicted = n_evicted + 1
                    total_size_bytes = total_size_bytes - entries(i)%size_bytes
                end if
            end do
        end if

        call cache_clean_empty_dirs(c)
    end subroutine cache_gc

    subroutine cache_stats(c, n_entries, total_size_mb)
        type(cache_t), intent(in) :: c
        integer, intent(out) :: n_entries
        integer, intent(out) :: total_size_mb

        type(cache_entry_t), allocatable :: entries(:)
        integer :: n_temp_evicted
        integer(int64) :: total_size_bytes

        n_entries = 0
        total_size_mb = 0
        if (.not. cache_ready(c)) return

        call cache_collect_entries(c, entries, n_entries, total_size_bytes, &
            n_temp_evicted, .false.)
        total_size_mb = int(total_size_bytes / CACHE_MB_BYTES)
    end subroutine cache_stats

end module fx_cache
