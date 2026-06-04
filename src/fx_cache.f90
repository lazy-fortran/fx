module fx_cache
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    type, public :: cache_t
        character(len=512) :: root_dir = ' '
        logical :: initialized = .false.
    end type cache_t

    type, public :: cache_entry_t
        character(len=64) :: key = ' '
        character(len=512) :: path = ' '
        integer(int64) :: size_bytes = 0_int64
        integer(int64) :: timestamp = 0_int64
    end type cache_entry_t

    public :: cache_init, cache_key, cache_has
    public :: cache_store, cache_restore
    public :: cache_store_bytes, cache_restore_bytes
    public :: cache_evict, cache_gc, cache_stats

contains

    subroutine cache_init(c, root_dir)
        type(cache_t), intent(out) :: c
        character(len=*), intent(in) :: root_dir
        error stop "fx_cache:cache_init not implemented"
    end subroutine cache_init

    function cache_key(parts, n_parts) result(key)
        integer, intent(in) :: n_parts
        character(len=*), intent(in) :: parts(n_parts)
        character(len=64) :: key
        error stop "fx_cache:cache_key not implemented"
    end function cache_key

    logical function cache_has(c, key)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        error stop "fx_cache:cache_has not implemented"
    end function cache_has

    subroutine cache_store(c, key, source_path, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: source_path
        integer, intent(out) :: ierr
        error stop "fx_cache:cache_store not implemented"
    end subroutine cache_store

    subroutine cache_restore(c, key, dest_path, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: dest_path
        integer, intent(out) :: ierr
        error stop "fx_cache:cache_restore not implemented"
    end subroutine cache_restore

    subroutine cache_store_bytes(c, key, data, n_bytes, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        integer, intent(in) :: n_bytes
        character(len=1), intent(in) :: data(n_bytes)
        integer, intent(out) :: ierr
        error stop "fx_cache:cache_store_bytes not implemented"
    end subroutine cache_store_bytes

    subroutine cache_restore_bytes(c, key, data, n_bytes, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=1), intent(out) :: data(:)
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr
        error stop "fx_cache:cache_restore_bytes not implemented"
    end subroutine cache_restore_bytes

    subroutine cache_evict(c, key, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        integer, intent(out) :: ierr
        error stop "fx_cache:cache_evict not implemented"
    end subroutine cache_evict

    subroutine cache_gc(c, max_size_mb, n_evicted)
        type(cache_t), intent(in) :: c
        integer, intent(in) :: max_size_mb
        integer, intent(out) :: n_evicted
        error stop "fx_cache:cache_gc not implemented"
    end subroutine cache_gc

    subroutine cache_stats(c, n_entries, total_size_mb)
        type(cache_t), intent(in) :: c
        integer, intent(out) :: n_entries
        integer, intent(out) :: total_size_mb
        error stop "fx_cache:cache_stats not implemented"
    end subroutine cache_stats

end module fx_cache
