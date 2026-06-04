module fx_diag
    use fx_json_build, only: json_builder_t
    implicit none
    private

    integer, parameter, public :: DIAG_ERROR = 0
    integer, parameter, public :: DIAG_WARNING = 1
    integer, parameter, public :: DIAG_INFO = 2
    integer, parameter, public :: DIAG_HINT = 3

    integer, parameter, public :: MAX_DIAGS = 512

    type, public :: diag_t
        character(len=512) :: file = ' '
        integer :: line = 0
        integer :: col = 0
        integer :: severity = DIAG_ERROR
        character(len=512) :: message = ' '
        character(len=256) :: hint = ' '
        character(len=256) :: source_line = ' '
    end type diag_t

    public :: diag_new, diag_to_string, diag_to_json
    public :: diags_to_json, diag_strip_prefix

contains

    function diag_new(file, line, col, severity, message) result(d)
        character(len=*), intent(in) :: file
        integer, intent(in) :: line
        integer, intent(in) :: col
        integer, intent(in) :: severity
        character(len=*), intent(in) :: message
        type(diag_t) :: d
        error stop "fx_diag:diag_new not implemented"
    end function diag_new

    function diag_to_string(d) result(res)
        type(diag_t), intent(in) :: d
        character(len=:), allocatable :: res
        error stop "fx_diag:diag_to_string not implemented"
    end function diag_to_string

    subroutine diag_to_json(d, jb)
        type(diag_t), intent(in) :: d
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_diag:diag_to_json not implemented"
    end subroutine diag_to_json

    subroutine diags_to_json(diags, n, jb)
        integer, intent(in) :: n
        type(diag_t), intent(in) :: diags(n)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_diag:diags_to_json not implemented"
    end subroutine diags_to_json

    subroutine diag_strip_prefix(d, prefix)
        type(diag_t), intent(inout) :: d
        character(len=*), intent(in) :: prefix
        error stop "fx_diag:diag_strip_prefix not implemented"
    end subroutine diag_strip_prefix

end module fx_diag
