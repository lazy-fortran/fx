module fx_log
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, parameter, public :: FX_LOG_DEBUG = 0
    integer, parameter, public :: FX_LOG_INFO = 1
    integer, parameter, public :: FX_LOG_WARN = 2
    integer, parameter, public :: FX_LOG_ERROR = 3

    integer, public :: log_level = FX_LOG_INFO

    public :: log_debug, log_info, log_warn, log_error, log_set_level

contains

    subroutine log_debug(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        error stop "fx_log:log_debug not implemented"
    end subroutine log_debug

    subroutine log_info(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        error stop "fx_log:log_info not implemented"
    end subroutine log_info

    subroutine log_warn(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        error stop "fx_log:log_warn not implemented"
    end subroutine log_warn

    subroutine log_error(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        error stop "fx_log:log_error not implemented"
    end subroutine log_error

    subroutine log_set_level(level)
        integer, intent(in) :: level
        error stop "fx_log:log_set_level not implemented"
    end subroutine log_set_level

end module fx_log
