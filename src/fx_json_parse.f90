module fx_json_parse
    implicit none
    private

    integer, parameter, public :: JSON_OBJECT_START = 1
    integer, parameter, public :: JSON_OBJECT_END = 2
    integer, parameter, public :: JSON_ARRAY_START = 3
    integer, parameter, public :: JSON_ARRAY_END = 4
    integer, parameter, public :: JSON_KEY = 5
    integer, parameter, public :: JSON_STRING = 6
    integer, parameter, public :: JSON_INTEGER = 7
    integer, parameter, public :: JSON_REAL = 8
    integer, parameter, public :: JSON_BOOL = 9
    integer, parameter, public :: JSON_NULL_VAL = 10
    integer, parameter, public :: JSON_ERROR = 11
    integer, parameter, public :: JSON_END_OF_INPUT = 12

    type, public :: json_event_t
        integer :: event_type = 0
        character(len=:), allocatable :: string_val
        integer :: int_val = 0
        double precision :: real_val = 0.0d0
        logical :: bool_val = .false.
    end type json_event_t

    type, public :: json_parser_t
        character(len=:), allocatable :: input
        integer :: pos = 1
        integer :: depth = 0
    end type json_parser_t

    public :: json_parser_init, json_parser_next, json_parser_reset
    public :: json_extract_string, json_extract_int, json_extract_bool

contains

    subroutine json_parser_init(p, input)
        type(json_parser_t), intent(out) :: p
        character(len=*), intent(in) :: input
        error stop "fx_json_parse:json_parser_init not implemented"
    end subroutine json_parser_init

    subroutine json_parser_next(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        error stop "fx_json_parse:json_parser_next not implemented"
    end subroutine json_parser_next

    subroutine json_parser_reset(p)
        type(json_parser_t), intent(inout) :: p
        error stop "fx_json_parse:json_parser_reset not implemented"
    end subroutine json_parser_reset

    subroutine json_extract_string(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: found
        error stop "fx_json_parse:json_extract_string not implemented"
    end subroutine json_extract_string

    subroutine json_extract_int(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        integer, intent(out) :: result
        logical, intent(out) :: found
        error stop "fx_json_parse:json_extract_int not implemented"
    end subroutine json_extract_int

    subroutine json_extract_bool(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        logical, intent(out) :: result
        logical, intent(out) :: found
        error stop "fx_json_parse:json_extract_bool not implemented"
    end subroutine json_extract_bool

end module fx_json_parse
