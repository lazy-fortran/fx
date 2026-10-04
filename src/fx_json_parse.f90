module fx_json_parse
    use, intrinsic :: iso_fortran_env, only: int64, real64
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

    integer, parameter, public :: JSON_ERR_NONE = 0
    integer, parameter, public :: JSON_ERR_SYNTAX = 1
    integer, parameter, public :: JSON_ERR_DEPTH = 2
    integer, parameter, public :: JSON_ERR_STRING = 3
    integer, parameter, public :: JSON_ERR_NUMBER = 4
    integer, parameter, public :: JSON_ERR_TRAILING = 5

    integer, parameter :: MAX_DEPTH = 128
    integer, parameter, public :: JSON_MAX_DEPTH = 64

    type, public :: json_event_t
        integer :: event_type = 0
        character(len=:), allocatable :: string_val
        integer :: int_val = 0
        integer(int64) :: int64_val = 0_int64
        ! False when raw JSON integer text is outside the int64 range.
        logical :: int64_valid = .false.
        real(real64) :: real_val = 0.0_real64
        logical :: real64_valid = .false.
        logical :: bool_val = .false.
        integer :: raw_start = 0
        integer :: raw_end = 0
        character(len=:), allocatable :: raw_val
        integer :: error_code = JSON_ERR_NONE
        ! 1-based input byte position; len(input)+1 means unexpected EOF.
        integer :: error_offset = 0
    end type json_event_t

    type, public :: json_parser_t
        character(len=:), allocatable :: input
        integer :: pos = 1
        integer :: depth = 0
        ! Retained for source compatibility with prior public parser state.
        logical :: in_object(MAX_DEPTH) = .false.
        logical :: expect_key(MAX_DEPTH) = .false.
        logical :: strict_int64 = .false.
        logical :: strict_is_object(MAX_DEPTH) = .false.
        integer :: strict_phase(MAX_DEPTH) = 0
        integer :: root_phase = 0
        integer :: error_code = JSON_ERR_NONE
        integer :: error_offset = 0
    end type json_parser_t

    public :: json_parser_init, json_parser_init_strict
    public :: json_parser_next, json_parser_reset
    public :: json_extract_string, json_extract_int, json_extract_bool

    contains

    ! Keeps the original API and default-integer event value while validating
    ! complete JSON. Use strict initialization for raw and int64 number access.
    subroutine json_parser_init(p, input)
        type(json_parser_t), intent(out) :: p
        character(len=*), intent(in) :: input

        p%input = input
        p%pos = 1
        p%depth = 0
        p%in_object = .false.
        p%expect_key = .false.
        p%strict_int64 = .false.
        p%strict_is_object = .false.
        p%strict_phase = 0
        p%root_phase = 0
        p%error_code = JSON_ERR_NONE
        p%error_offset = 0
    end subroutine json_parser_init

    ! Strict initialization enables int64 events and enforces one complete
    ! RFC 8259 value, limits nesting to JSON_MAX_DEPTH, and decodes Unicode.
    ! Events carry raw token text and 1-based inclusive byte bounds.
    ! JSON_ERROR carries a public code and
    ! 1-based byte offset (len+1 at EOF). Duplicate decoded names remain valid
    ! JSON; schema consumers must reject duplicates before mapping fields.
    subroutine json_parser_init_strict(p, input)
        type(json_parser_t), intent(out) :: p
        character(len=*), intent(in) :: input

        call json_parser_init(p, input)
        p%strict_int64 = .true.
    end subroutine json_parser_init_strict

    subroutine json_parser_reset(p)
        type(json_parser_t), intent(inout) :: p
        p%pos = 1
        p%depth = 0
        p%in_object = .false.
        p%expect_key = .false.
        p%strict_is_object = .false.
        p%strict_phase = 0
        p%root_phase = 0
        p%error_code = JSON_ERR_NONE
        p%error_offset = 0
    end subroutine json_parser_reset

    subroutine json_parser_next(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        call json_parser_next_strict(p, event)
    end subroutine json_parser_next

    recursive subroutine json_parser_next_strict(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        integer :: top
        character(len=1) :: ch
        logical :: is_key

        call json_event_init(event)
        if (p%error_code /= JSON_ERR_NONE) then
            event%event_type = JSON_ERROR
            event%error_code = p%error_code
            event%error_offset = p%error_offset
            return
        end if

        call skip_whitespace(p)
        if (p%pos > len(p%input)) then
            if (p%depth /= 0 .or. p%root_phase == 0) then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
            else
                event%event_type = JSON_END_OF_INPUT
            end if
            return
        end if

        if (p%depth == 0) then
            if (p%root_phase /= 0) then
                call strict_fail(p, event, JSON_ERR_TRAILING, p%pos)
                return
            end if
            call strict_value(p, event)
            return
        end if

        top = p%depth
        is_key = p%strict_is_object(top)
        select case (p%strict_phase(top))
        case (0)
            ch = p%input(p%pos:p%pos)
            if (is_key) then
                if (ch == '}') then
                    call strict_close(p, event, JSON_OBJECT_END)
                else
                    call strict_key(p, event)
                end if
            else
                if (ch == ']') then
                    call strict_close(p, event, JSON_ARRAY_END)
                else
                    call strict_value(p, event)
                end if
            end if
        case (1)
            if (is_key) then
                call strict_key(p, event)
            else
                call strict_value(p, event)
            end if
        case (2)
            if (p%input(p%pos:p%pos) /= ':') then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                return
            end if
            p%pos = p%pos + 1
            call skip_whitespace(p)
            if (p%pos > len(p%input)) then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                return
            end if
            call strict_value(p, event)
        case (3)
            ch = p%input(p%pos:p%pos)
            if (is_key) then
                if (ch == ',') then
                    p%pos = p%pos + 1
                    p%strict_phase(top) = 1
                    call json_parser_next_strict(p, event)
                else if (ch == '}') then
                    call strict_close(p, event, JSON_OBJECT_END)
                else
                    call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                end if
            else
                if (ch == ',') then
                    p%pos = p%pos + 1
                    p%strict_phase(top) = 1
                    call json_parser_next_strict(p, event)
                else if (ch == ']') then
                    call strict_close(p, event, JSON_ARRAY_END)
                else
                    call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                end if
            end if
        case default
            call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
        end select
    end subroutine json_parser_next_strict

    subroutine strict_key(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        integer :: start, error_offset
        logical :: valid

        start = p%pos
        if (p%input(p%pos:p%pos) /= '"') then
            call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
            return
        end if
        call strict_string(p, event%string_val, valid, error_offset)
        if (.not. valid) then
            call strict_fail(p, event, JSON_ERR_STRING, error_offset)
            return
        end if
        p%strict_phase(p%depth) = 2
        event%event_type = JSON_KEY
        call strict_set_raw(p, event, start)
    end subroutine strict_key

    subroutine strict_value(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        integer :: start, ios, error_offset
        character(len=1) :: ch
        logical :: valid

        start = p%pos
        ch = p%input(p%pos:p%pos)
        select case (ch)
        case ('{', '[')
            call strict_mark_value(p)
            if (p%depth >= JSON_MAX_DEPTH) then
                call strict_fail(p, event, JSON_ERR_DEPTH, p%pos)
                return
            end if
            p%pos = p%pos + 1
            p%depth = p%depth + 1
            p%strict_is_object(p%depth) = ch == '{'
            p%strict_phase(p%depth) = 0
            if (ch == '{') then
                event%event_type = JSON_OBJECT_START
            else
                event%event_type = JSON_ARRAY_START
            end if
        case ('"')
            call strict_string(p, event%string_val, valid, error_offset)
            if (.not. valid) then
                call strict_fail(p, event, JSON_ERR_STRING, error_offset)
                return
            end if
            call strict_mark_value(p)
            event%event_type = JSON_STRING
        case ('t')
            call strict_literal(p, 'true', valid)
            if (.not. valid) then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                return
            end if
            call strict_mark_value(p)
            event%event_type = JSON_BOOL
            event%bool_val = .true.
        case ('f')
            call strict_literal(p, 'false', valid)
            if (.not. valid) then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                return
            end if
            call strict_mark_value(p)
            event%event_type = JSON_BOOL
        case ('n')
            call strict_literal(p, 'null', valid)
            if (.not. valid) then
                call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
                return
            end if
            call strict_mark_value(p)
            event%event_type = JSON_NULL_VAL
        case ('-', '0':'9')
            call strict_number(p, event, valid)
            if (.not. valid) then
                call strict_fail(p, event, JSON_ERR_NUMBER, p%pos)
                return
            end if
            call strict_mark_value(p)
            if (event%event_type == JSON_INTEGER) then
                read(event%raw_val, *, iostat=ios) event%int64_val
                event%int64_valid = ios == 0
                if (event%int64_valid) then
                    if (event%int64_val >= &
                        -int(huge(event%int_val), int64) - 1_int64 .and. &
                        event%int64_val <= int(huge(event%int_val), int64)) then
                        event%int_val = int(event%int64_val)
                    else if (.not. p%strict_int64) then
                        call strict_fail(p, event, JSON_ERR_NUMBER, start)
                        return
                    end if
                else if (.not. p%strict_int64) then
                    call strict_fail(p, event, JSON_ERR_NUMBER, start)
                    return
                end if
            else
                read(event%raw_val, *, iostat=ios) event%real_val
                event%real64_valid = ios == 0
                if (.not. event%real64_valid) then
                    event%real_val = 0.0_real64
                    if (.not. p%strict_int64) then
                        call strict_fail(p, event, JSON_ERR_NUMBER, start)
                        return
                    end if
                end if
            end if
        case default
            call strict_fail(p, event, JSON_ERR_SYNTAX, p%pos)
            return
        end select
        call strict_set_raw(p, event, start)
    end subroutine strict_value

    subroutine strict_close(p, event, event_type)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        integer, intent(in) :: event_type
        integer :: start

        start = p%pos
        p%pos = p%pos + 1
        p%depth = p%depth - 1
        event%event_type = event_type
        call strict_set_raw(p, event, start)
    end subroutine strict_close

    subroutine strict_mark_value(p)
        type(json_parser_t), intent(inout) :: p

        if (p%depth == 0) then
            p%root_phase = 1
        else
            p%strict_phase(p%depth) = 3
        end if
    end subroutine strict_mark_value

    subroutine strict_set_raw(p, event, start)
        type(json_parser_t), intent(in) :: p
        type(json_event_t), intent(inout) :: event
        integer, intent(in) :: start

        event%raw_start = start
        event%raw_end = p%pos - 1
        event%raw_val = p%input(start:p%pos - 1)
    end subroutine strict_set_raw

    subroutine strict_fail(p, event, code, offset)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(inout) :: event
        integer, intent(in) :: code, offset

        p%error_code = code
        p%error_offset = offset
        event%event_type = JSON_ERROR
        event%error_code = code
        event%error_offset = offset
    end subroutine strict_fail

    subroutine strict_literal(p, literal, valid)
        type(json_parser_t), intent(inout) :: p
        character(len=*), intent(in) :: literal
        logical, intent(out) :: valid
        integer :: last

        valid = .false.
        last = p%pos + len(literal) - 1
        if (last > len(p%input)) return
        if (p%input(p%pos:last) /= literal) return
        p%pos = last + 1
        valid = .true.
    end subroutine strict_literal

    subroutine strict_number(p, event, valid)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(inout) :: event
        logical, intent(out) :: valid
        integer :: start
        logical :: is_real

        valid = .false.
        start = p%pos
        is_real = .false.
        if (p%input(p%pos:p%pos) == '-') then
            p%pos = p%pos + 1
            if (p%pos > len(p%input)) return
        end if
        if (p%input(p%pos:p%pos) == '0') then
            p%pos = p%pos + 1
            if (p%pos <= len(p%input)) then
                if (is_digit(p%input(p%pos:p%pos))) return
            end if
        else if (is_digit_1_9(p%input(p%pos:p%pos))) then
            do while (p%pos <= len(p%input))
                if (.not. is_digit(p%input(p%pos:p%pos))) exit
                p%pos = p%pos + 1
            end do
        else
            return
        end if
        if (p%pos <= len(p%input)) then
            if (p%input(p%pos:p%pos) == '.') then
                is_real = .true.
                p%pos = p%pos + 1
                if (p%pos > len(p%input)) return
                if (.not. is_digit(p%input(p%pos:p%pos))) return
                do while (p%pos <= len(p%input))
                    if (.not. is_digit(p%input(p%pos:p%pos))) exit
                    p%pos = p%pos + 1
                end do
            end if
        end if
        if (p%pos <= len(p%input)) then
            if (p%input(p%pos:p%pos) == 'e' .or. p%input(p%pos:p%pos) == 'E') then
                is_real = .true.
                p%pos = p%pos + 1
                if (p%pos <= len(p%input)) then
                    if (p%input(p%pos:p%pos) == '+' .or. &
                        p%input(p%pos:p%pos) == '-') p%pos = p%pos + 1
                end if
                if (p%pos > len(p%input)) return
                if (.not. is_digit(p%input(p%pos:p%pos))) return
                do while (p%pos <= len(p%input))
                    if (.not. is_digit(p%input(p%pos:p%pos))) exit
                    p%pos = p%pos + 1
                end do
            end if
        end if
        if (is_real) then
            event%event_type = JSON_REAL
        else
            event%event_type = JSON_INTEGER
        end if
        event%raw_val = p%input(start:p%pos - 1)
        valid = .true.
    end subroutine strict_number

    pure logical function is_digit(ch)
        character(len=1), intent(in) :: ch
        is_digit = ch >= '0' .and. ch <= '9'
    end function is_digit

    pure logical function is_digit_1_9(ch)
        character(len=1), intent(in) :: ch
        is_digit_1_9 = ch >= '1' .and. ch <= '9'
    end function is_digit_1_9

    subroutine strict_string(p, value, valid, error_offset)
        type(json_parser_t), intent(inout) :: p
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: valid
        integer, intent(out) :: error_offset
        character(len=1) :: ch
        integer :: code, low, ios, escape_start
        character(len=4) :: hex

        valid = .false.
        value = ''
        error_offset = p%pos
        if (p%input(p%pos:p%pos) /= '"') return
        p%pos = p%pos + 1
        do while (p%pos <= len(p%input))
            ch = p%input(p%pos:p%pos)
            if (ch == '"') then
                p%pos = p%pos + 1
                valid = .true.
                error_offset = 0
                return
            end if
            if (iachar(ch) < 32) then
                error_offset = p%pos
                return
            end if
            if (ch /= achar(92)) then
                value = value // ch
                p%pos = p%pos + 1
                cycle
            end if
            escape_start = p%pos
            p%pos = p%pos + 1
            if (p%pos > len(p%input)) then
                error_offset = len(p%input) + 1
                return
            end if
            ch = p%input(p%pos:p%pos)
            select case (ch)
            case ('"', achar(92), '/')
                value = value // ch
            case ('b')
                value = value // achar(8)
            case ('f')
                value = value // achar(12)
            case ('n')
                value = value // achar(10)
            case ('r')
                value = value // achar(13)
            case ('t')
                value = value // achar(9)
            case ('u')
                p%pos = p%pos + 1
                if (p%pos + 3 > len(p%input)) then
                    error_offset = len(p%input) + 1
                    return
                end if
                hex = p%input(p%pos:p%pos + 3)
                if (.not. is_hex_string(hex)) then
                    error_offset = p%pos
                    return
                end if
                read(hex, '(Z4)', iostat=ios) code
                if (ios /= 0) then
                    error_offset = p%pos
                    return
                end if
                p%pos = p%pos + 3
                if (code >= int(z'D800') .and. code <= int(z'DBFF')) then
                    if (p%pos + 1 > len(p%input)) then
                        error_offset = len(p%input) + 1
                        return
                    end if
                    if (p%input(p%pos + 1:p%pos + 1) /= achar(92)) then
                        error_offset = p%pos + 1
                        return
                    end if
                    if (p%pos + 2 > len(p%input)) then
                        error_offset = len(p%input) + 1
                        return
                    end if
                    if (p%input(p%pos + 2:p%pos + 2) /= 'u') then
                        error_offset = p%pos + 2
                        return
                    end if
                    if (p%pos + 6 > len(p%input)) then
                        error_offset = len(p%input) + 1
                        return
                    end if
                    hex = p%input(p%pos + 3:p%pos + 6)
                    if (.not. is_hex_string(hex)) then
                        error_offset = p%pos + 3
                        return
                    end if
                    read(hex, '(Z4)', iostat=ios) low
                    if (ios /= 0) then
                        error_offset = p%pos + 3
                        return
                    end if
                    if (low < int(z'DC00') .or. low > int(z'DFFF')) then
                        error_offset = escape_start
                        return
                    end if
                    code = 65536 + (code - 55296) * 1024 + (low - 56320)
                    p%pos = p%pos + 6
                else if (code >= int(z'DC00') .and. code <= int(z'DFFF')) then
                    error_offset = escape_start
                    return
                end if
                call append_utf8(value, code)
            case default
                error_offset = escape_start
                return
            end select
            p%pos = p%pos + 1
        end do
        error_offset = len(p%input) + 1
    end subroutine strict_string

    pure logical function is_hex_string(value)
        character(len=*), intent(in) :: value
        integer :: i, code

        is_hex_string = .false.
        do i = 1, len(value)
            code = iachar(value(i:i))
            if ((code >= iachar('0') .and. code <= iachar('9')) .or. &
                (code >= iachar('a') .and. code <= iachar('f')) .or. &
                (code >= iachar('A') .and. code <= iachar('F'))) cycle
            return
        end do
        is_hex_string = .true.
    end function is_hex_string

    subroutine append_utf8(value, code)
        character(len=:), allocatable, intent(inout) :: value
        integer, intent(in) :: code

        if (code < 128) then
            value = value // achar(code)
        else if (code < 2048) then
            value = value // achar(192 + code / 64) // achar(128 + mod(code, 64))
        else if (code < 65536) then
            value = value // achar(224 + code / 4096) // &
                achar(128 + mod(code / 64, 64)) // achar(128 + mod(code, 64))
        else
            value = value // achar(240 + code / 262144) // &
                achar(128 + mod(code / 4096, 64)) // &
                achar(128 + mod(code / 64, 64)) // achar(128 + mod(code, 64))
        end if
    end subroutine append_utf8

    subroutine json_event_init(event)
        type(json_event_t), intent(inout) :: event
        event%event_type = JSON_ERROR
        event%int_val = 0
        event%int64_val = 0_int64
        event%int64_valid = .false.
        event%real_val = 0.0_real64
        event%real64_valid = .false.
        event%bool_val = .false.
        event%raw_start = 0
        event%raw_end = 0
        event%error_code = JSON_ERR_NONE
        event%error_offset = 0
        if (allocated(event%string_val)) deallocate(event%string_val)
        if (allocated(event%raw_val)) deallocate(event%raw_val)
    end subroutine json_event_init

    pure logical function stack_index_is_valid(depth)
        integer, intent(in) :: depth
        stack_index_is_valid = depth >= 1 .and. depth <= MAX_DEPTH
    end function stack_index_is_valid

    ! Extract a string value at a dot/bracket path, e.g. "result.tools[1].name"
    subroutine json_extract_string(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        character(len=:), allocatable, intent(out) :: result
        logical, intent(out) :: found

        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=256) :: segments(64)
        integer :: n_segs
        character(len=256) :: key_stack(MAX_DEPTH)
        integer :: arr_idx(MAX_DEPTH)
        logical :: is_arr(MAX_DEPTH)
        integer :: depth

        found = .false.
        result = ''
        call parse_path(path, segments, n_segs)
        if (n_segs > size(segments)) return
        call json_parser_init(p, input)
        depth = 0
        key_stack = ''
        arr_idx = 0
        is_arr = .false.

        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                return
            case (JSON_OBJECT_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (.not. stack_index_is_valid(depth)) return
                depth = depth - 1
            case (JSON_KEY)
                if (.not. stack_index_is_valid(depth)) return
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_STRING)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                end if
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    if (allocated(ev%string_val)) result = ev%string_val
                    found = .true.
                    return
                end if
                if (stack_index_is_valid(depth)) key_stack(depth) = ''
            case (JSON_INTEGER, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                    key_stack(depth) = ''
                end if
            end select
        end do
    end subroutine json_extract_string

    subroutine json_extract_int(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        integer, intent(out) :: result
        logical, intent(out) :: found

        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=256) :: segments(64)
        integer :: n_segs
        character(len=256) :: key_stack(MAX_DEPTH)
        integer :: arr_idx(MAX_DEPTH)
        logical :: is_arr(MAX_DEPTH)
        integer :: depth

        found = .false.
        result = 0
        call parse_path(path, segments, n_segs)
        if (n_segs > size(segments)) return
        call json_parser_init(p, input)
        depth = 0
        key_stack = ''
        arr_idx = 0
        is_arr = .false.

        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                return
            case (JSON_OBJECT_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (.not. stack_index_is_valid(depth)) return
                depth = depth - 1
            case (JSON_KEY)
                if (.not. stack_index_is_valid(depth)) return
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_INTEGER)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                end if
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    result = ev%int_val
                    found = .true.
                    return
                end if
                if (stack_index_is_valid(depth)) key_stack(depth) = ''
            case (JSON_STRING, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                    key_stack(depth) = ''
                end if
            end select
        end do
    end subroutine json_extract_int

    subroutine json_extract_bool(input, path, result, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: path
        logical, intent(out) :: result
        logical, intent(out) :: found

        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=256) :: segments(64)
        integer :: n_segs
        character(len=256) :: key_stack(MAX_DEPTH)
        integer :: arr_idx(MAX_DEPTH)
        logical :: is_arr(MAX_DEPTH)
        integer :: depth

        found = .false.
        result = .false.
        call parse_path(path, segments, n_segs)
        if (n_segs > size(segments)) return
        call json_parser_init(p, input)
        depth = 0
        key_stack = ''
        arr_idx = 0
        is_arr = .false.

        do
            call json_parser_next(p, ev)
            select case (ev%event_type)
            case (JSON_END_OF_INPUT, JSON_ERROR)
                return
            case (JSON_OBJECT_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                if (.not. stack_index_is_valid(depth + 1)) return
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                if (.not. stack_index_is_valid(depth)) return
                depth = depth - 1
            case (JSON_KEY)
                if (.not. stack_index_is_valid(depth)) return
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_BOOL)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                end if
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    result = ev%bool_val
                    found = .true.
                    return
                end if
                if (stack_index_is_valid(depth)) key_stack(depth) = ''
            case (JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_NULL_VAL)
                if (stack_index_is_valid(depth)) then
                    if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                    key_stack(depth) = ''
                end if
            end select
        end do
    end subroutine json_extract_bool

    ! Check key_stack/arr_idx against path segments at given depth
    pure logical function path_matches(key_stack, arr_idx, is_arr, depth, &
            segments, n_segs)
        character(len=256), intent(in) :: key_stack(:)
        integer, intent(in) :: arr_idx(:)
        logical, intent(in) :: is_arr(:)
        integer, intent(in) :: depth
        character(len=256), intent(in) :: segments(:)
        integer, intent(in) :: n_segs
        integer :: i
        character(len=32) :: buf

        path_matches = .false.
        if (depth /= n_segs) return
        if (depth < 0) return
        if (depth > size(key_stack) .or. depth > size(arr_idx)) return
        if (depth > size(is_arr) .or. depth > size(segments)) return

        do i = 1, depth
            if (is_arr(i)) then
                write(buf, '(I0)') arr_idx(i)
                if (trim(segments(i)) /= trim(buf)) return
            else
                if (trim(key_stack(i)) /= trim(segments(i))) return
            end if
        end do
        path_matches = .true.
    end function path_matches

    ! Parse "a.b[0].c" → segments ["a","b","0","c"].
    ! Overflow returns size(segments) + 1; callers reject the incomplete path.
    pure subroutine parse_path(path, segments, n_segs)
        character(len=*), intent(in) :: path
        character(len=256), intent(out) :: segments(:)
        integer, intent(out) :: n_segs
        integer :: i, start, plen
        character(len=1) :: ch

        n_segs = 0
        plen = len_trim(path)
        if (plen == 0) return
        start = 1
        i = 1

        do while (i <= plen)
            ch = path(i:i)
            if (ch == '.' .or. ch == '[' .or. ch == ']') then
                if (i > start) then
                    n_segs = n_segs + 1
                    if (n_segs > size(segments)) return
                    segments(n_segs) = path(start:i - 1)
                end if
                start = i + 1
            end if
            i = i + 1
        end do
        if (start <= plen) then
            n_segs = n_segs + 1
            if (n_segs > size(segments)) return
            segments(n_segs) = path(start:plen)
        end if
    end subroutine parse_path

    subroutine skip_whitespace(p)
        type(json_parser_t), intent(inout) :: p
        integer :: code
        do while (p%pos <= len(p%input))
            code = iachar(p%input(p%pos:p%pos))
            if (code == 32 .or. code == 9 .or. code == 10 .or. code == 13) then
                p%pos = p%pos + 1
            else
                exit
            end if
        end do
    end subroutine skip_whitespace

end module fx_json_parse
