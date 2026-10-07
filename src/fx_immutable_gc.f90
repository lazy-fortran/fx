module fx_immutable_gc
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_immutable_store, only: immutable_store_t
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_IO_ERROR, &
        IMMUTABLE_INVALID
    implicit none
    private
    integer, parameter, public :: IMMUTABLE_GC_CHANGED = 6
    public :: immutable_store_collect

    interface
        integer(c_int) function c_collect(root, max_scan, max_delete, &
                min_age, pressure, pressure_objects, scanned, allocated, &
                deleted, reclaimed) &
                bind(C, name='fx_immutable_gc_collect')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), value :: max_scan, max_delete, pressure_objects
            integer(c_long_long), value :: min_age, pressure
            integer(c_int), intent(out) :: scanned, deleted
            integer(c_long_long), intent(out) :: allocated, reclaimed
        end function c_collect
    end interface
contains
    subroutine immutable_store_collect(store, max_scan, max_delete, &
            min_age_seconds, pressure_bytes, pressure_objects, &
            scanned_objects, allocated_bytes, deleted, reclaimed_bytes, ierr)
        !! Collect at most max_delete unreachable objects after a complete
        !! max_scan-bounded inventory. An incomplete inventory deletes nothing.
        type(immutable_store_t), intent(in) :: store
        integer, intent(in) :: max_scan, max_delete, pressure_objects
        integer(int64), intent(in) :: min_age_seconds, pressure_bytes
        integer, intent(out) :: scanned_objects, deleted, ierr
        integer(int64), intent(out) :: allocated_bytes, reclaimed_bytes
        integer(c_int) :: c_scanned, c_deleted, rc
        integer(c_long_long) :: c_allocated, c_reclaimed

        scanned_objects = 0
        allocated_bytes = 0_int64
        deleted = 0
        reclaimed_bytes = 0_int64
        ierr = IMMUTABLE_INVALID
        if (.not. store%initialized) return
        rc = c_collect(store%root_dir//c_null_char, int(max_scan, c_int), &
            int(max_delete, c_int), int(min_age_seconds, c_long_long), &
            int(pressure_bytes, c_long_long), int(pressure_objects, c_int), &
            c_scanned, c_allocated, c_deleted, c_reclaimed)
        scanned_objects = int(c_scanned)
        allocated_bytes = int(c_allocated, int64)
        deleted = int(c_deleted)
        reclaimed_bytes = int(c_reclaimed, int64)
        select case (rc)
        case (0)
            ierr = IMMUTABLE_OK
        case (1)
            ierr = IMMUTABLE_GC_CHANGED
        case (-2)
            ierr = IMMUTABLE_INVALID
        case default
            ierr = IMMUTABLE_IO_ERROR
        end select
    end subroutine immutable_store_collect
end module fx_immutable_gc
