module immutable_marker_oracle
    use, intrinsic :: iso_c_binding, only: c_int, c_char
    use fx_test, only: test_suite_t, test_assert, test_assert_equal_int
    implicit none
    private
    public :: marker_boundary_configure, probe_ready_marker
    interface
        subroutine marker_boundary_configure(entered, continued) &
                bind(C, name='fx_immutable_owned_test_marker_boundary')
            import :: c_char
            character(kind=c_char), intent(in) :: entered(*), continued(*)
        end subroutine marker_boundary_configure
        integer(c_int) function sleep_us(time) bind(C, name='usleep')
            import :: c_int
            integer(c_int), value :: time
        end function sleep_us
    end interface
contains
    subroutine probe_ready_marker(suite, ready)
        type(test_suite_t), intent(inout) :: suite
        character(len=*), intent(in) :: ready
        integer :: attempt, unit, ios, closed
        integer(c_int) :: ignored
        logical :: entered, visible
        entered = .false.
        do attempt = 1, 3000
            inquire (file=ready//'.open', exist=entered)
            if (entered) exit
            ignored = sleep_us(10000_c_int)
        end do
        call test_assert(suite, entered, 'marker writer pauses before staging path is written')
        inquire (file=ready, exist=visible)
        call test_assert(suite, .not. visible, 'incomplete boundary marker is not visible')
        open (newunit=unit, file=ready//'.write', status='replace', iostat=ios)
        call test_assert_equal_int(suite, 0, ios, 'marker writer release opens')
        if (ios /= 0) return
        write (unit, '(a)', iostat=ios) 'continue'
        close (unit, iostat=closed)
        call test_assert(suite, ios == 0 .and. closed == 0, 'marker writer release completes')
    end subroutine probe_ready_marker
end module immutable_marker_oracle
