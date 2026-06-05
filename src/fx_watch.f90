module fx_watch
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_proc, only: proc_path_is_dir, proc_scan_dirs, proc_watch_add, &
                       proc_watch_close, proc_watch_init, proc_watch_poll, &
                       proc_watch_rm
    implicit none
    private

    integer, parameter, public :: WATCH_MODIFY = 1
    integer, parameter, public :: WATCH_CREATE = 2
    integer, parameter, public :: WATCH_DELETE = 3

    integer, parameter :: WATCH_PATH_LEN = 4096
    integer, parameter :: WATCH_SELF_WRITE_WINDOW_MS = 500
    integer, parameter :: WATCH_INITIAL_WATCH_CAPACITY = 32
    integer, parameter :: WATCH_INITIAL_SELF_CAPACITY = 16

    integer, parameter :: IN_MODIFY_MASK = int(z'00000002')
    integer, parameter :: IN_CREATE_MASK = int(z'00000100')
    integer, parameter :: IN_DELETE_MASK = int(z'00000200')
    integer, parameter :: IN_MOVED_FROM_MASK = int(z'00000040')
    integer, parameter :: IN_MOVED_TO_MASK = int(z'00000080')
    integer, parameter :: IN_DELETE_SELF_MASK = int(z'00000400')
    integer, parameter :: IN_MOVE_SELF_MASK = int(z'00000800')

    integer, parameter :: WATCH_MASK = ior(ior(ior(ior(ior(ior( &
            IN_MODIFY_MASK, IN_CREATE_MASK), IN_DELETE_MASK), &
            IN_MOVED_FROM_MASK), IN_MOVED_TO_MASK), IN_DELETE_SELF_MASK), &
            IN_MOVE_SELF_MASK)

    type, public :: watcher_t
        integer :: fd = -1
        integer, allocatable :: watches(:)
        character(len=WATCH_PATH_LEN), allocatable :: watch_paths(:)
        integer :: n_watches = 0
        character(len=WATCH_PATH_LEN), allocatable :: self_written(:)
        integer(int64), allocatable :: self_written_at(:)
        integer :: n_self_written = 0
    end type watcher_t

    public :: watcher_init, watcher_add, watcher_remove
    public :: watcher_poll, watcher_mark_self_written, watcher_close

contains

    subroutine watcher_init(w, ierr)
        type(watcher_t), intent(out) :: w
        integer, intent(out) :: ierr

        call watcher_reset(w)
        call proc_watch_init(w%fd, ierr)
        if (ierr /= 0) w%fd = -1
    end subroutine watcher_init

    subroutine watcher_add(w, path, recursive, ierr)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        logical, intent(in) :: recursive
        integer, intent(out) :: ierr

        character(len=:), allocatable :: dirs(:)
        character(len=WATCH_PATH_LEN) :: root
        integer :: n_dirs
        integer :: i
        integer :: wd
        integer :: add_err

        ierr = 0
        root = watcher_canonical_path(path)
        if (len_trim(root) == 0) then
            ierr = 1
            return
        end if

        if (recursive) then
            call proc_scan_dirs(root, dirs, n_dirs, ierr)
            if (ierr /= 0) return
        else
            allocate(character(len=WATCH_PATH_LEN) :: dirs(1))
            dirs(1) = root
            n_dirs = 1
        end if

        do i = 1, n_dirs
            call proc_watch_add(w%fd, trim(dirs(i)), WATCH_MASK, wd, add_err)
            if (add_err == 0) then
                call watcher_store_watch(w, trim(dirs(i)), wd)
            else
                ierr = 1
            end if
        end do

        if (allocated(dirs)) deallocate(dirs)
    end subroutine watcher_add

    subroutine watcher_remove(w, path, ierr)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr

        character(len=WATCH_PATH_LEN) :: target
        integer :: i
        integer :: rm_err

        ierr = 0
        target = watcher_canonical_path(path)
        if (len_trim(target) == 0) return

        do i = w%n_watches, 1, -1
            if (watcher_path_matches(w%watch_paths(i), target)) then
                call proc_watch_rm(w%fd, w%watches(i), rm_err)
                call watcher_delete_watch_at(w, i)
            end if
        end do

        call watcher_remove_self_written_tree(w, target)
    end subroutine watcher_remove

    subroutine watcher_poll(w, changed_path, event_type, &
            timeout_ms, got_event)
        type(watcher_t), intent(inout) :: w
        character(len=WATCH_PATH_LEN), intent(out) :: changed_path
        integer, intent(out) :: event_type
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: got_event

        character(len=WATCH_PATH_LEN) :: path
        integer :: ierr

        changed_path = ''
        event_type = 0
        got_event = .false.

        call watcher_prune_self_written(w)
        call proc_watch_poll(w%fd, path, event_type, timeout_ms, got_event)
        if (.not. got_event) return

        path = watcher_canonical_path(path)
        if (len_trim(path) == 0) then
            changed_path = ''
            event_type = 0
            got_event = .false.
            return
        end if

        if (watcher_is_self_written(w, path)) then
            call watcher_remove_self_written_exact(w, path)
            changed_path = ''
            event_type = 0
            got_event = .false.
            return
        end if

        changed_path = path
        if (event_type == WATCH_DELETE) then
            if (watcher_find_watch(w, path) > 0) then
                call watcher_remove(w, path, ierr)
            end if
        else if (event_type == WATCH_CREATE) then
            if (proc_path_is_dir(path)) then
                call watcher_add(w, path, .true., ierr)
            end if
        end if
    end subroutine watcher_poll

    subroutine watcher_mark_self_written(w, path)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path

        character(len=WATCH_PATH_LEN) :: target
        integer(int64) :: now_ms
        integer :: idx

        target = watcher_canonical_path(path)
        if (len_trim(target) == 0) return

        call watcher_prune_self_written(w)
        now_ms = watcher_now_ms()
        idx = watcher_find_self_written(w, target)
        if (idx > 0) then
            w%self_written_at(idx) = now_ms
        else
            call watcher_store_self_written(w, target, now_ms)
        end if
    end subroutine watcher_mark_self_written

    subroutine watcher_close(w)
        type(watcher_t), intent(inout) :: w

        integer :: i
        integer :: ierr
        integer :: fd

        fd = w%fd
        if (fd >= 0) then
            do i = w%n_watches, 1, -1
                call proc_watch_rm(fd, w%watches(i), ierr)
            end do
            call proc_watch_close(fd, ierr)
        end if
        call watcher_reset(w)
    end subroutine watcher_close

    subroutine watcher_reset(w)
        type(watcher_t), intent(inout) :: w

        if (allocated(w%watches)) deallocate(w%watches)
        if (allocated(w%watch_paths)) deallocate(w%watch_paths)
        if (allocated(w%self_written)) deallocate(w%self_written)
        if (allocated(w%self_written_at)) deallocate(w%self_written_at)
        w%fd = -1
        w%n_watches = 0
        w%n_self_written = 0
    end subroutine watcher_reset

    subroutine watcher_store_watch(w, path, wd)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        integer, intent(in) :: wd

        integer :: idx

        idx = watcher_find_watch(w, path)
        if (idx > 0) then
            w%watches(idx) = wd
            w%watch_paths(idx) = watcher_canonical_path(path)
            return
        end if

        call watcher_ensure_watch_capacity(w)
        w%n_watches = w%n_watches + 1
        w%watches(w%n_watches) = wd
        w%watch_paths(w%n_watches) = watcher_canonical_path(path)
    end subroutine watcher_store_watch

    subroutine watcher_delete_watch_at(w, idx)
        type(watcher_t), intent(inout) :: w
        integer, intent(in) :: idx
        integer :: i

        if (idx < 1 .or. idx > w%n_watches) return
        do i = idx, w%n_watches - 1
            w%watches(i) = w%watches(i + 1)
            w%watch_paths(i) = w%watch_paths(i + 1)
        end do
        w%n_watches = w%n_watches - 1
    end subroutine watcher_delete_watch_at

    subroutine watcher_ensure_watch_capacity(w)
        type(watcher_t), intent(inout) :: w

        integer, allocatable :: new_watches(:)
        character(len=WATCH_PATH_LEN), allocatable :: new_paths(:)
        integer :: new_cap

        if (.not. allocated(w%watches)) then
            allocate(w%watches(WATCH_INITIAL_WATCH_CAPACITY))
            allocate(w%watch_paths(WATCH_INITIAL_WATCH_CAPACITY))
            w%watches = 0
            w%watch_paths = ''
            return
        end if

        if (w%n_watches < size(w%watches)) return

        new_cap = max(WATCH_INITIAL_WATCH_CAPACITY, size(w%watches) * 2)
        allocate(new_watches(new_cap))
        allocate(new_paths(new_cap))
        new_watches = 0
        new_paths = ''
        if (w%n_watches > 0) then
            new_watches(1:w%n_watches) = w%watches(1:w%n_watches)
            new_paths(1:w%n_watches) = w%watch_paths(1:w%n_watches)
        end if
        call move_alloc(new_watches, w%watches)
        call move_alloc(new_paths, w%watch_paths)
    end subroutine watcher_ensure_watch_capacity

    function watcher_find_watch(w, path) result(idx)
        type(watcher_t), intent(in) :: w
        character(len=*), intent(in) :: path
        integer :: idx
        integer :: i
        character(len=WATCH_PATH_LEN) :: target

        idx = 0
        target = watcher_canonical_path(path)
        do i = 1, w%n_watches
            if (trim(w%watch_paths(i)) == trim(target)) then
                idx = i
                return
            end if
        end do
    end function watcher_find_watch

    function watcher_path_matches(candidate, target) result(matches)
        character(len=*), intent(in) :: candidate
        character(len=*), intent(in) :: target
        logical :: matches
        integer :: n_candidate
        integer :: n_target

        matches = .false.
        n_candidate = len_trim(candidate)
        n_target = len_trim(target)
        if (n_target == 0 .or. n_candidate < n_target) return
        if (candidate(1:n_target) /= target(1:n_target)) return
        if (n_candidate == n_target) then
            matches = .true.
        else
            matches = candidate(n_target + 1:n_target + 1) == '/'
        end if
    end function watcher_path_matches

    subroutine watcher_remove_self_written_tree(w, target)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: target
        integer :: i

        do i = w%n_self_written, 1, -1
            if (watcher_path_matches(w%self_written(i), target)) then
                call watcher_delete_self_written_at(w, i)
            end if
        end do
    end subroutine watcher_remove_self_written_tree

    subroutine watcher_remove_self_written_exact(w, path)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        integer :: idx

        idx = watcher_find_self_written(w, path)
        if (idx > 0) call watcher_delete_self_written_at(w, idx)
    end subroutine watcher_remove_self_written_exact

    function watcher_is_self_written(w, path) result(found)
        type(watcher_t), intent(in) :: w
        character(len=*), intent(in) :: path
        logical :: found

        found = watcher_find_self_written(w, path) > 0
    end function watcher_is_self_written

    function watcher_find_self_written(w, path) result(idx)
        type(watcher_t), intent(in) :: w
        character(len=*), intent(in) :: path
        integer :: idx
        integer :: i
        character(len=WATCH_PATH_LEN) :: target

        idx = 0
        target = watcher_canonical_path(path)
        do i = 1, w%n_self_written
            if (trim(w%self_written(i)) == trim(target)) then
                idx = i
                return
            end if
        end do
    end function watcher_find_self_written

    subroutine watcher_store_self_written(w, path, now_ms)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        integer(int64), intent(in) :: now_ms

        integer :: idx

        idx = watcher_find_self_written(w, path)
        if (idx > 0) then
            w%self_written(idx) = watcher_canonical_path(path)
            w%self_written_at(idx) = now_ms
            return
        end if

        call watcher_ensure_self_written_capacity(w)
        w%n_self_written = w%n_self_written + 1
        w%self_written(w%n_self_written) = watcher_canonical_path(path)
        w%self_written_at(w%n_self_written) = now_ms
    end subroutine watcher_store_self_written

    subroutine watcher_delete_self_written_at(w, idx)
        type(watcher_t), intent(inout) :: w
        integer, intent(in) :: idx
        integer :: i

        if (idx < 1 .or. idx > w%n_self_written) return
        do i = idx, w%n_self_written - 1
            w%self_written(i) = w%self_written(i + 1)
            w%self_written_at(i) = w%self_written_at(i + 1)
        end do
        w%n_self_written = w%n_self_written - 1
    end subroutine watcher_delete_self_written_at

    subroutine watcher_ensure_self_written_capacity(w)
        type(watcher_t), intent(inout) :: w

        character(len=WATCH_PATH_LEN), allocatable :: new_paths(:)
        integer(int64), allocatable :: new_times(:)
        integer :: new_cap

        if (.not. allocated(w%self_written)) then
            allocate(w%self_written(WATCH_INITIAL_SELF_CAPACITY))
            allocate(w%self_written_at(WATCH_INITIAL_SELF_CAPACITY))
            w%self_written = ''
            w%self_written_at = 0_int64
            return
        end if

        if (w%n_self_written < size(w%self_written)) return

        new_cap = max(WATCH_INITIAL_SELF_CAPACITY, size(w%self_written) * 2)
        allocate(new_paths(new_cap))
        allocate(new_times(new_cap))
        new_paths = ''
        new_times = 0_int64
        if (w%n_self_written > 0) then
            new_paths(1:w%n_self_written) = w%self_written(1:w%n_self_written)
            new_times(1:w%n_self_written) = w%self_written_at(1:w%n_self_written)
        end if
        call move_alloc(new_paths, w%self_written)
        call move_alloc(new_times, w%self_written_at)
    end subroutine watcher_ensure_self_written_capacity

    subroutine watcher_prune_self_written(w)
        type(watcher_t), intent(inout) :: w

        integer(int64) :: now_ms
        integer :: i

        now_ms = watcher_now_ms()
        do i = w%n_self_written, 1, -1
            if (now_ms - w%self_written_at(i) > WATCH_SELF_WRITE_WINDOW_MS) then
                call watcher_delete_self_written_at(w, i)
            end if
        end do
    end subroutine watcher_prune_self_written

    function watcher_now_ms() result(now_ms)
        integer(int64) :: now_ms

        integer :: count
        integer :: rate

        call system_clock(count=count, count_rate=rate)
        if (rate <= 0) then
            now_ms = int(count, int64)
        else
            now_ms = int(count, int64) * 1000_int64 / int(rate, int64)
        end if
    end function watcher_now_ms

    function watcher_canonical_path(path) result(normalized)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: normalized

        integer :: n

        normalized = trim(path)
        n = len(normalized)
        do while (n > 1 .and. normalized(n:n) == '/')
            normalized = normalized(:n - 1)
            n = len(normalized)
        end do
    end function watcher_canonical_path

end module fx_watch
