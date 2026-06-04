module fx_watch
    implicit none
    private

    integer, parameter, public :: WATCH_MODIFY = 1
    integer, parameter, public :: WATCH_CREATE = 2
    integer, parameter, public :: WATCH_DELETE = 3

    integer, parameter :: MAX_WATCHES = 256
    integer, parameter :: MAX_SELF_WRITTEN = 64

    type, public :: watcher_t
        integer :: fd = -1
        integer :: watches(MAX_WATCHES) = 0
        character(len=512) :: watch_paths(MAX_WATCHES) = ' '
        integer :: n_watches = 0
        character(len=512) :: self_written(MAX_SELF_WRITTEN) = ' '
        integer :: n_self_written = 0
    end type watcher_t

    public :: watcher_init, watcher_add, watcher_remove
    public :: watcher_poll, watcher_mark_self_written, watcher_close

contains

    subroutine watcher_init(w, ierr)
        type(watcher_t), intent(out) :: w
        integer, intent(out) :: ierr
        error stop "fx_watch:watcher_init not implemented"
    end subroutine watcher_init

    subroutine watcher_add(w, path, recursive, ierr)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        logical, intent(in) :: recursive
        integer, intent(out) :: ierr
        error stop "fx_watch:watcher_add not implemented"
    end subroutine watcher_add

    subroutine watcher_remove(w, path, ierr)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        integer, intent(out) :: ierr
        error stop "fx_watch:watcher_remove not implemented"
    end subroutine watcher_remove

    subroutine watcher_poll(w, changed_path, event_type, &
            timeout_ms, got_event)
        type(watcher_t), intent(inout) :: w
        character(len=512), intent(out) :: changed_path
        integer, intent(out) :: event_type
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: got_event
        error stop "fx_watch:watcher_poll not implemented"
    end subroutine watcher_poll

    subroutine watcher_mark_self_written(w, path)
        type(watcher_t), intent(inout) :: w
        character(len=*), intent(in) :: path
        error stop "fx_watch:watcher_mark_self_written not implemented"
    end subroutine watcher_mark_self_written

    subroutine watcher_close(w)
        type(watcher_t), intent(inout) :: w
        error stop "fx_watch:watcher_close not implemented"
    end subroutine watcher_close

end module fx_watch
