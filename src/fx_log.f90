module fx_log
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    integer, parameter, public :: FX_LOG_DEBUG = 0
    integer, parameter, public :: FX_LOG_INFO = 1
    integer, parameter, public :: FX_LOG_WARN = 2
    integer, parameter, public :: FX_LOG_ERROR = 3
    integer, parameter, public :: FX_LOG_NONE = 4

    integer, public :: log_level = FX_LOG_INFO

    public :: log_debug, log_info, log_warn, log_error, log_set_level

contains

    subroutine log_debug(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        if (log_level <= FX_LOG_DEBUG) call log_write('DEBUG', component, message)
    end subroutine log_debug

    subroutine log_info(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        if (log_level <= FX_LOG_INFO) call log_write('INFO', component, message)
    end subroutine log_info

    subroutine log_warn(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        if (log_level <= FX_LOG_WARN) call log_write('WARN', component, message)
    end subroutine log_warn

    subroutine log_error(component, message)
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message
        if (log_level <= FX_LOG_ERROR) call log_write('ERROR', component, message)
    end subroutine log_error

    subroutine log_set_level(level)
        integer, intent(in) :: level
        log_level = level
    end subroutine log_set_level

    subroutine log_write(tag, component, message)
        character(len=*), intent(in) :: tag
        character(len=*), intent(in) :: component
        character(len=*), intent(in) :: message

        if (len_trim(component) == 0) then
            write(error_unit, '(A)') '[' // tag // '] ' // trim(message)
        else
            write(error_unit, '(A)') '[' // tag // '] ' // &
                trim(component) // ': ' // trim(message)
        end if
    end subroutine log_write

end module fx_log
