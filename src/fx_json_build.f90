module fx_json_build
    use, intrinsic :: iso_fortran_env, only: real64
    use fx_string, only: builder_t, builder_new, builder_append, &
        builder_to_string, builder_reset
    implicit none
    private

    integer, parameter :: MAX_DEPTH = 64

    type, public :: json_builder_t
        type(builder_t) :: buf
        logical :: needs_comma = .false.
        integer :: depth = 0
        logical :: needs_comma_stack(MAX_DEPTH) = .false.
        logical :: in_object_stack(MAX_DEPTH) = .false.
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
        jb%buf = builder_new(256)
        jb%depth = 0
        jb%needs_comma = .false.
        jb%needs_comma_stack = .false.
        jb%in_object_stack = .false.
    end function json_new

    subroutine json_object_start(jb)
        type(json_builder_t), intent(inout) :: jb
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        call builder_append(jb%buf, '{')
        jb%depth = jb%depth + 1
        jb%needs_comma_stack(jb%depth) = .false.
        jb%in_object_stack(jb%depth) = .true.
    end subroutine json_object_start

    subroutine json_object_end(jb)
        type(json_builder_t), intent(inout) :: jb
        call builder_append(jb%buf, '}')
        jb%depth = jb%depth - 1
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_object_end

    subroutine json_array_start(jb)
        type(json_builder_t), intent(inout) :: jb
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        call builder_append(jb%buf, '[')
        jb%depth = jb%depth + 1
        jb%needs_comma_stack(jb%depth) = .false.
        jb%in_object_stack(jb%depth) = .false.
    end subroutine json_array_start

    subroutine json_array_end(jb)
        type(json_builder_t), intent(inout) :: jb
        call builder_append(jb%buf, ']')
        jb%depth = jb%depth - 1
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_array_end

    subroutine json_key(jb, name)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: name
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        call builder_append(jb%buf, '"')
        call builder_append(jb%buf, json_escape_string(name))
        call builder_append(jb%buf, '":')
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .false.
    end subroutine json_key

    subroutine json_value_string(jb, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: val
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth) .and. &
            .not. jb%in_object_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        call builder_append(jb%buf, '"')
        call builder_append(jb%buf, json_escape_string(val))
        call builder_append(jb%buf, '"')
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_value_string

    subroutine json_value_int(jb, val)
        type(json_builder_t), intent(inout) :: jb
        integer, intent(in) :: val
        character(len=32) :: buf
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth) .and. &
            .not. jb%in_object_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        write(buf, '(I0)') val
        call builder_append(jb%buf, trim(buf))
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_value_int

    subroutine json_value_real(jb, val)
        use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
        type(json_builder_t), intent(inout) :: jb
        real(real64), intent(in) :: val
        character(len=64) :: buf
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth) .and. &
            .not. jb%in_object_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        if (ieee_is_nan(val) .or. .not. ieee_is_finite(val)) then
            call builder_append(jb%buf, 'null')
        else
            write(buf, '(G0)') val
            call builder_append(jb%buf, trim(buf))
        end if
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_value_real

    subroutine json_value_bool(jb, val)
        type(json_builder_t), intent(inout) :: jb
        logical, intent(in) :: val
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth) .and. &
            .not. jb%in_object_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        if (val) then
            call builder_append(jb%buf, 'true')
        else
            call builder_append(jb%buf, 'false')
        end if
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_value_bool

    subroutine json_value_null(jb)
        type(json_builder_t), intent(inout) :: jb
        if (jb%depth > 0 .and. jb%needs_comma_stack(jb%depth) .and. &
            .not. jb%in_object_stack(jb%depth)) then
            call builder_append(jb%buf, ',')
        end if
        call builder_append(jb%buf, 'null')
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_value_null

    subroutine json_key_string(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
        call json_key(jb, key)
        call json_value_string(jb, val)
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_key_string

    subroutine json_key_int(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        integer, intent(in) :: val
        call json_key(jb, key)
        call json_value_int(jb, val)
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_key_int

    subroutine json_key_bool(jb, key, val)
        type(json_builder_t), intent(inout) :: jb
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
        call json_key(jb, key)
        call json_value_bool(jb, val)
        if (jb%depth > 0) jb%needs_comma_stack(jb%depth) = .true.
    end subroutine json_key_bool

    function json_to_string(jb) result(res)
        type(json_builder_t), intent(in) :: jb
        character(len=:), allocatable :: res
        res = builder_to_string(jb%buf)
    end function json_to_string

    subroutine json_reset(jb)
        type(json_builder_t), intent(inout) :: jb
        call builder_reset(jb%buf)
        jb%depth = 0
        jb%needs_comma = .false.
        jb%needs_comma_stack = .false.
        jb%in_object_stack = .false.
    end subroutine json_reset

    function json_escape_string(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i
        character(len=1) :: c
        integer :: code

        res = ''
        do i = 1, len(s)
            c = s(i:i)
            code = iachar(c)
            select case (code)
            case (34) ! "
                res = res // '\"'
            case (92) ! backslash
                res = res // '\\'
            case (8) ! backspace
                res = res // '\b'
            case (12) ! form feed
                res = res // '\f'
            case (10) ! newline
                res = res // '\n'
            case (13) ! carriage return
                res = res // '\r'
            case (9) ! tab
                res = res // '\t'
            case (0:7, 11, 14:31) ! other control chars
                block
                    character(len=6) :: hex
                    write(hex, '(A,Z4.4)') '\u', code
                    res = res // hex
                end block
            case default
                res = res // c
            end select
        end do
    end function json_escape_string

end module fx_json_build
