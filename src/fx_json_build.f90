module fx_json_build
    use fx_string, only: builder_t, builder_new, builder_append, &
                         builder_to_string, builder_reset
    implicit none
    private

    type, public :: json_builder_t
        type(builder_t) :: buf
        logical :: needs_comma = .false.
        integer :: depth = 0
    end type json_builder_t

    public :: json_new, json_object_start, json_object_end
    public :: json_array_start, json_array_end
    public :: json_key, json_value_string, json_value_int
    public :: json_value_real, json_value_bool, json_value_null
    public :: json_key_string, json_key_int, json_key_bool
    public :: json_to_string, json_reset, json_escape_string

contains

    function json_new() result(jb)
        type(json_builder_t) :: jb
        error stop "fx_json_build:json_new not implemented"
    end function json_new

    subroutine json_object_start(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_object_start not implemented"
    end subroutine json_object_start

    subroutine json_object_end(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_object_end not implemented"
    end subroutine json_object_end

    subroutine json_array_start(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_array_start not implemented"
    end subroutine json_array_start

    subroutine json_array_end(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_array_end not implemented"
    end subroutine json_array_end

    subroutine json_key(jb, name)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: name
        error stop "fx_json_build:json_key not implemented"
    end subroutine json_key

    subroutine json_value_string(jb, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: val
        error stop "fx_json_build:json_value_string not implemented"
    end subroutine json_value_string

    subroutine json_value_int(jb, val)
        type(json_builder_t), intent(inout) :: jb
        integer, intent(in) :: val
        error stop "fx_json_build:json_value_int not implemented"
    end subroutine json_value_int

    subroutine json_value_real(jb, val)
        type(json_builder_t), intent(inout) :: jb
        double precision, intent(in) :: val
        error stop "fx_json_build:json_value_real not implemented"
    end subroutine json_value_real

    subroutine json_value_bool(jb, val)
        type(json_builder_t), intent(inout) :: jb
        logical, intent(in) :: val
        error stop "fx_json_build:json_value_bool not implemented"
    end subroutine json_value_bool

    subroutine json_value_null(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_value_null not implemented"
    end subroutine json_value_null

    subroutine json_key_string(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
        error stop "fx_json_build:json_key_string not implemented"
    end subroutine json_key_string

    subroutine json_key_int(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        integer, intent(in) :: val
        error stop "fx_json_build:json_key_int not implemented"
    end subroutine json_key_int

    subroutine json_key_bool(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
        error stop "fx_json_build:json_key_bool not implemented"
    end subroutine json_key_bool

    function json_to_string(jb) result(res)
        type(json_builder_t), intent(in) :: jb
        character(len=:), allocatable :: res
        error stop "fx_json_build:json_to_string not implemented"
    end function json_to_string

    subroutine json_reset(jb)
        type(json_builder_t), intent(inout) :: jb
        error stop "fx_json_build:json_reset not implemented"
    end subroutine json_reset

    function json_escape_string(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        error stop "fx_json_build:json_escape_string not implemented"
    end function json_escape_string

end module fx_json_build
