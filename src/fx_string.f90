module fx_string
    implicit none
    private

    type, public :: string_t
        character(len=:), allocatable :: s
    end type string_t

    type, public :: builder_t
        character(len=:), allocatable :: buf
        integer :: len = 0
        integer :: cap = 0
    end type builder_t

    public :: str, builder_new, builder_append, builder_to_string
    public :: builder_reset
    public :: to_lower, to_upper, split, join
    public :: starts_with, ends_with, contains_str, replace_str
    public :: find_str, strip, repeat_str, utf8_len

contains

    function str(chars) result(res)
        character(len=*), intent(in) :: chars
        type(string_t) :: res
        error stop "fx_string:str not implemented"
    end function str

    function builder_new(initial_cap) result(b)
        integer, intent(in) :: initial_cap
        type(builder_t) :: b
        error stop "fx_string:builder_new not implemented"
    end function builder_new

    subroutine builder_append(b, text)
        type(builder_t), intent(inout) :: b
        character(len=*), intent(in) :: text
        error stop "fx_string:builder_append not implemented"
    end subroutine builder_append

    function builder_to_string(b) result(res)
        type(builder_t), intent(in) :: b
        character(len=:), allocatable :: res
        error stop "fx_string:builder_to_string not implemented"
    end function builder_to_string

    subroutine builder_reset(b)
        type(builder_t), intent(inout) :: b
        error stop "fx_string:builder_reset not implemented"
    end subroutine builder_reset

    function to_lower(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        error stop "fx_string:to_lower not implemented"
    end function to_lower

    function to_upper(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        error stop "fx_string:to_upper not implemented"
    end function to_upper

    subroutine split(s, delimiter, parts, n_parts)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: delimiter
        character(len=256), intent(out) :: parts(:)
        integer, intent(out) :: n_parts
        error stop "fx_string:split not implemented"
    end subroutine split

    function join(parts, n_parts, delimiter) result(res)
        character(len=256), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=*), intent(in) :: delimiter
        character(len=:), allocatable :: res
        error stop "fx_string:join not implemented"
    end function join

    logical function starts_with(s, prefix)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: prefix
        error stop "fx_string:starts_with not implemented"
    end function starts_with

    logical function ends_with(s, suffix)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: suffix
        error stop "fx_string:ends_with not implemented"
    end function ends_with

    logical function contains_str(s, substr)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: substr
        error stop "fx_string:contains_str not implemented"
    end function contains_str

    function replace_str(s, old, new) result(res)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: old
        character(len=*), intent(in) :: new
        character(len=:), allocatable :: res
        error stop "fx_string:replace_str not implemented"
    end function replace_str

    integer function find_str(s, substr, start)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: substr
        integer, intent(in) :: start
        error stop "fx_string:find_str not implemented"
    end function find_str

    function strip(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        error stop "fx_string:strip not implemented"
    end function strip

    function repeat_str(s, n) result(res)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: res
        error stop "fx_string:repeat_str not implemented"
    end function repeat_str

    integer function utf8_len(s)
        character(len=*), intent(in) :: s
        error stop "fx_string:utf8_len not implemented"
    end function utf8_len

end module fx_string
