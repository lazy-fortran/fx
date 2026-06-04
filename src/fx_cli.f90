module fx_cli
    implicit none
    private

    integer, parameter :: MAX_ARGS = 64
    integer, parameter :: MAX_ARG_LEN = 512

    type, public :: cli_t
        character(len=MAX_ARG_LEN) :: args(MAX_ARGS) = ' '
        integer :: n_args = 0
        character(len=256) :: program_name = ' '
    end type cli_t

    public :: cli_init, cli_has_flag, cli_get_value
    public :: cli_get_positional, cli_n_positional, cli_command

contains

    subroutine cli_init(c)
        type(cli_t), intent(out) :: c
        error stop "fx_cli:cli_init not implemented"
    end subroutine cli_init

    logical function cli_has_flag(c, flag)
        type(cli_t), intent(in) :: c
        character(len=*), intent(in) :: flag
        error stop "fx_cli:cli_has_flag not implemented"
    end function cli_has_flag

    function cli_get_value(c, key, default_val) result(res)
        type(cli_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: default_val
        character(len=:), allocatable :: res
        error stop "fx_cli:cli_get_value not implemented"
    end function cli_get_value

    function cli_get_positional(c, index) result(res)
        type(cli_t), intent(in) :: c
        integer, intent(in) :: index
        character(len=:), allocatable :: res
        error stop "fx_cli:cli_get_positional not implemented"
    end function cli_get_positional

    integer function cli_n_positional(c)
        type(cli_t), intent(in) :: c
        error stop "fx_cli:cli_n_positional not implemented"
    end function cli_n_positional

    function cli_command(c) result(res)
        type(cli_t), intent(in) :: c
        character(len=:), allocatable :: res
        error stop "fx_cli:cli_command not implemented"
    end function cli_command

end module fx_cli
