module fx_cache_fs
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
                                           c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_path, only: path_basename, path_dirname, path_join
    use fx_proc, only: proc_scan_dirs, proc_scan_files
    implicit none
    private

    integer, parameter, public :: CACHE_PATH_LEN = 512
    integer, parameter :: CACHE_COPY_BLOCK = 65536
    integer(int64), parameter :: CACHE_TMP_AGE_SEC = 3600_int64
    integer(int64), parameter :: CACHE_MB_BYTES = 1048576_int64

    type, public :: cache_t
        character(len=CACHE_PATH_LEN) :: root_dir = ' '
        logical :: initialized = .false.
        integer :: temp_seq = 0
    end type cache_t

    type, public :: cache_entry_t
        character(len=64) :: key = ' '
        character(len=512) :: path = ' '
        integer(int64) :: size_bytes = 0_int64
        integer(int64) :: timestamp = 0_int64
    end type cache_entry_t

    public :: cache_ready, cache_prefix_path, cache_entry_path, cache_temp_path
    public :: cache_ensure_dir, cache_rename, cache_unlink, cache_rmdir
    public :: cache_file_stat, cache_is_temp_name
    public :: cache_collect_entries, cache_sort_entries, cache_clean_empty_dirs
    public :: cache_copy_file, cache_write_bytes_file, cache_read_bytes_file
    public :: to_c_string

    interface
        integer(c_int) function fx_c_mkdir_p(path) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fx_c_mkdir_p

        integer(c_int) function fx_c_rename(src, dst) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: src(*)
            character(kind=c_char), intent(in) :: dst(*)
        end function fx_c_rename

        integer(c_int) function fx_c_unlink(path) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fx_c_unlink

        integer(c_int) function fx_c_rmdir(path) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function fx_c_rmdir

        integer(c_int) function fx_c_file_stat(path, size_bytes, mtime) &
                bind(C)
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: path(*)
            integer(c_long_long), intent(out) :: size_bytes
            integer(c_long_long), intent(out) :: mtime
        end function fx_c_file_stat

        integer(c_long_long) function fx_c_unix_time() bind(C)
            import :: c_long_long
        end function fx_c_unix_time
    end interface

contains

    logical function cache_ready(c) result(ready)
        type(cache_t), intent(in) :: c

        ready = c%initialized .and. len_trim(c%root_dir) > 0
    end function cache_ready

    pure function cache_prefix(key) result(prefix)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: prefix

        if (len_trim(key) < 2) then
            prefix = ''
        else
            prefix = key(1:2)
        end if
    end function cache_prefix

    function cache_prefix_path(c, key) result(path)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: path
        character(len=:), allocatable :: prefix

        prefix = cache_prefix(trim(key))
        if (len_trim(prefix) == 0) then
            path = ''
        else
            path = path_join(trim(c%root_dir), prefix)
        end if
    end function cache_prefix_path

    function cache_entry_path(c, key) result(path)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: path
        character(len=:), allocatable :: prefix
        character(len=:), allocatable :: entry_key

        entry_key = trim(key)
        prefix = cache_prefix(entry_key)
        if (len_trim(prefix) == 0) then
            path = ''
        else
            path = path_join(path_join(trim(c%root_dir), prefix), entry_key)
        end if
    end function cache_entry_path

    subroutine cache_temp_path(c, key, path)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: path

        character(len=:), allocatable :: prefix_path
        character(len=32) :: seq_text
        character(len=32) :: time_text
        integer :: clock
        integer(int64) :: now_sec

        now_sec = fx_c_unix_time()
        call system_clock(clock)
        write(seq_text, '(I0)') clock
        write(time_text, '(I0)') now_sec

        prefix_path = cache_prefix_path(c, key)
        path = path_join(prefix_path, '.tmp.' // trim(time_text) // '.' // &
                         trim(seq_text))
    end subroutine cache_temp_path

    subroutine cache_ensure_dir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(CACHE_PATH_LEN)
        integer(c_int) :: status

        call to_c_string(path, c_path)
        status = fx_c_mkdir_p(c_path)
        ierr = merge(0, 1, status == 0_c_int)
    end subroutine cache_ensure_dir

    subroutine cache_rename(src, dst, ierr)
        character(len=*), intent(in) :: src
        character(len=*), intent(in) :: dst
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_src(CACHE_PATH_LEN)
        character(kind=c_char) :: c_dst(CACHE_PATH_LEN)
        integer(c_int) :: status

        call to_c_string(src, c_src)
        call to_c_string(dst, c_dst)
        status = fx_c_rename(c_src, c_dst)
        ierr = merge(0, 1, status == 0_c_int)
    end subroutine cache_rename

    subroutine cache_unlink(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(CACHE_PATH_LEN)
        integer(c_int) :: status

        call to_c_string(path, c_path)
        status = fx_c_unlink(c_path)
        ierr = merge(0, 1, status == 0_c_int)
    end subroutine cache_unlink

    subroutine cache_rmdir(path, ierr)
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(CACHE_PATH_LEN)
        integer(c_int) :: status

        call to_c_string(path, c_path)
        status = fx_c_rmdir(c_path)
        ierr = merge(0, 1, status == 0_c_int)
    end subroutine cache_rmdir

    subroutine cache_file_stat(path, size_bytes, mtime, ierr)
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: size_bytes
        integer(int64), intent(out) :: mtime
        integer, intent(out) :: ierr
        character(kind=c_char) :: c_path(CACHE_PATH_LEN)
        integer(c_int) :: status
        integer(c_long_long) :: c_size
        integer(c_long_long) :: c_mtime

        call to_c_string(path, c_path)
        status = fx_c_file_stat(c_path, c_size, c_mtime)
        if (status == 0_c_int) then
            size_bytes = int(c_size, int64)
            mtime = int(c_mtime, int64)
            ierr = 0
        else
            size_bytes = 0_int64
            mtime = 0_int64
            ierr = 1
        end if
    end subroutine cache_file_stat

    logical function cache_is_temp_name(name)
        character(len=*), intent(in) :: name

        cache_is_temp_name = index(trim(name), '.tmp.') == 1
    end function cache_is_temp_name

    subroutine cache_collect_entries(c, entries, n_entries, total_size_bytes, &
                                     n_temp_evicted, prune_temp)
        type(cache_t), intent(in) :: c
        type(cache_entry_t), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: n_entries
        integer(int64), intent(out) :: total_size_bytes
        integer, intent(out) :: n_temp_evicted
        logical, intent(in) :: prune_temp

        character(len=:), allocatable :: files(:)
        integer :: n_files
        integer :: i
        integer :: ierr
        integer(int64) :: size_bytes
        integer(int64) :: mtime
        integer(int64) :: now_sec
        integer(int64) :: age
        character(len=:), allocatable :: base

        n_entries = 0
        total_size_bytes = 0_int64
        n_temp_evicted = 0

        call proc_scan_files(trim(c%root_dir), files, n_files, ierr)
        if (ierr /= 0) then
            allocate(entries(0))
            return
        end if

        allocate(entries(max(n_files, 0)))
        if (n_files == 0) return

        now_sec = fx_c_unix_time()
        do i = 1, n_files
            base = path_basename(trim(files(i)))
            if (cache_is_temp_name(base)) then
                call cache_file_stat(trim(files(i)), size_bytes, mtime, ierr)
                if (ierr == 0 .and. prune_temp) then
                    age = now_sec - mtime
                    if (age > CACHE_TMP_AGE_SEC) then
                        call cache_unlink(trim(files(i)), ierr)
                        if (ierr == 0) n_temp_evicted = n_temp_evicted + 1
                    end if
                end if
            else
                call cache_file_stat(trim(files(i)), size_bytes, mtime, ierr)
                if (ierr /= 0) cycle
                n_entries = n_entries + 1
                entries(n_entries)%key = path_basename(trim(files(i)))
                entries(n_entries)%path = trim(files(i))
                entries(n_entries)%size_bytes = size_bytes
                entries(n_entries)%timestamp = mtime
                total_size_bytes = total_size_bytes + size_bytes
            end if
        end do

        if (allocated(files)) deallocate(files)
    end subroutine cache_collect_entries

    subroutine cache_sort_entries(entries, n_entries)
        type(cache_entry_t), intent(inout) :: entries(:)
        integer, intent(in) :: n_entries
        integer :: i
        integer :: j
        type(cache_entry_t) :: current

        do i = 2, n_entries
            current = entries(i)
            j = i - 1
            do while (j >= 1 .and. entries(j)%timestamp > current%timestamp)
                entries(j + 1) = entries(j)
                j = j - 1
            end do
            entries(j + 1) = current
        end do
    end subroutine cache_sort_entries

    subroutine cache_clean_empty_dirs(c)
        type(cache_t), intent(in) :: c

        character(len=:), allocatable :: dirs(:)
        integer :: n_dirs
        integer :: i
        integer :: ierr

        call proc_scan_dirs(trim(c%root_dir), dirs, n_dirs, ierr)
        if (ierr /= 0) then
            if (allocated(dirs)) deallocate(dirs)
            return
        end if

        do i = 1, n_dirs
            if (trim(dirs(i)) == trim(c%root_dir)) cycle
            call cache_rmdir(trim(dirs(i)), ierr)
        end do

        if (allocated(dirs)) deallocate(dirs)
    end subroutine cache_clean_empty_dirs

    subroutine cache_copy_file(source_path, dest_path, ierr)
        character(len=*), intent(in) :: source_path
        character(len=*), intent(in) :: dest_path
        integer, intent(out) :: ierr

        character(len=1), allocatable :: buffer(:)
        integer :: src_unit
        integer :: dst_unit
        integer :: ios
        integer :: chunk
        integer :: cleanup_ierr
        integer(int64) :: remaining
        integer(int64) :: source_size
        logical :: exists

        ierr = 0
        inquire(file=trim(source_path), exist=exists, size=source_size)
        if (.not. exists) then
            ierr = 1
            return
        end if

        call cache_ensure_dir(path_dirname(dest_path), ierr)
        if (ierr /= 0) return

        open(newunit=src_unit, file=trim(source_path), access='stream', &
             form='unformatted', status='old', action='read', iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        open(newunit=dst_unit, file=trim(dest_path), access='stream', &
             form='unformatted', status='replace', action='write', &
             iostat=ios)
        if (ios /= 0) then
            close(src_unit)
            ierr = 1
            return
        end if

        allocate(buffer(CACHE_COPY_BLOCK))
        remaining = source_size
        do while (remaining > 0_int64)
            chunk = int(min(int(CACHE_COPY_BLOCK, int64), remaining))
            read(src_unit, iostat=ios) buffer(1:chunk)
            if (ios /= 0) then
                ierr = 1
                exit
            end if
            write(dst_unit, iostat=ios) buffer(1:chunk)
            if (ios /= 0) then
                ierr = 1
                exit
            end if
            remaining = remaining - int(chunk, int64)
        end do

        deallocate(buffer)
        close(src_unit)
        close(dst_unit)

        if (ierr /= 0) call cache_unlink(dest_path, cleanup_ierr)
    end subroutine cache_copy_file

    subroutine cache_write_bytes_file(path, data, n_bytes, ierr)
        character(len=*), intent(in) :: path
        integer, intent(in) :: n_bytes
        character(len=1), intent(in) :: data(n_bytes)
        integer, intent(out) :: ierr

        integer :: unit
        integer :: ios
        integer :: cleanup_ierr

        ierr = 0
        call cache_ensure_dir(path_dirname(path), ierr)
        if (ierr /= 0) return

        open(newunit=unit, file=trim(path), access='stream', &
             form='unformatted', status='replace', action='write', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        if (n_bytes > 0) then
            write(unit, iostat=ios) data(1:n_bytes)
        end if
        close(unit)
        if (ios /= 0) then
            ierr = 1
            call cache_unlink(path, cleanup_ierr)
        end if
    end subroutine cache_write_bytes_file

    subroutine cache_read_bytes_file(path, data, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=1), intent(out) :: data(:)
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr

        integer :: unit
        integer :: ios
        integer :: capacity
        logical :: exists
        integer(int64) :: file_size

        ierr = 0
        n_bytes = 0
        capacity = size(data)
        inquire(file=trim(path), exist=exists, size=file_size)
        if (.not. exists) then
            ierr = 1
            return
        end if
        if (file_size > int(huge(0), int64)) then
            ierr = 1
            return
        end if
        if (file_size > int(capacity, int64)) then
            ierr = 1
            return
        end if

        open(newunit=unit, file=trim(path), access='stream', &
             form='unformatted', status='old', action='read', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            return
        end if

        n_bytes = int(file_size)
        if (n_bytes > 0) then
            read(unit, iostat=ios) data(1:n_bytes)
        end if
        close(unit)
        if (ios /= 0) then
            ierr = 1
            n_bytes = 0
        end if
    end subroutine cache_read_bytes_file

    subroutine to_c_string(text, c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), intent(out) :: c_text(:)
        integer :: i
        integer :: n

        c_text = c_null_char
        n = min(len_trim(text), size(c_text) - 1)
        do i = 1, n
            c_text(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_text(n + 1) = c_null_char
    end subroutine to_c_string

end module fx_cache_fs
