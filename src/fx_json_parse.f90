module fx_json_parse
    use, intrinsic :: iso_fortran_env, only: real64
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

    integer, parameter :: MAX_DEPTH = 128

    type, public :: json_event_t
        integer :: event_type = 0
        character(len=:), allocatable :: string_val
        integer :: int_val = 0
        real(real64) :: real_val = 0.0_real64
        logical :: bool_val = .false.
    end type json_event_t

    ! Parser state per nesting depth:
    !   in_object(d) = true when depth d is an object (vs array)
    !   expect_key(d) = true when next string at depth d is a JSON key
    type, public :: json_parser_t
        character(len=:), allocatable :: input
        integer :: pos = 1
        integer :: depth = 0
        logical :: in_object(MAX_DEPTH) = .false.
        logical :: expect_key(MAX_DEPTH) = .false.
    end type json_parser_t

    public :: json_parser_init, json_parser_next, json_parser_reset
    public :: json_extract_string, json_extract_int, json_extract_bool

contains

    subroutine json_parser_init(p, input)
        type(json_parser_t), intent(out) :: p
        character(len=*), intent(in) :: input
        p%input = input
        p%pos = 1
        p%depth = 0
        p%in_object = .false.
        p%expect_key = .false.
    end subroutine json_parser_init

    subroutine json_parser_reset(p)
        type(json_parser_t), intent(inout) :: p
        p%pos = 1
        p%depth = 0
        p%in_object = .false.
        p%expect_key = .false.
    end subroutine json_parser_reset

    subroutine json_parser_next(p, event)
        type(json_parser_t), intent(inout) :: p
        type(json_event_t), intent(out) :: event
        character(len=1) :: ch

        call json_event_init(event)
        call skip_separators(p, ch, event%event_type)
        if (event%event_type == JSON_END_OF_INPUT) return

        select case (ch)
        case ('{')
            p%pos = p%pos + 1
            p%depth = p%depth + 1
            p%in_object(p%depth) = .true.
            p%expect_key(p%depth) = .true.
            event%event_type = JSON_OBJECT_START
        case ('}')
            p%pos = p%pos + 1
            p%depth = p%depth - 1
            if (p%depth > 0 .and. p%in_object(p%depth)) &
                p%expect_key(p%depth) = .true.
            event%event_type = JSON_OBJECT_END
        case ('[')
            p%pos = p%pos + 1
            p%depth = p%depth + 1
            p%in_object(p%depth) = .false.
            p%expect_key(p%depth) = .false.
            event%event_type = JSON_ARRAY_START
        case (']')
            p%pos = p%pos + 1
            p%depth = p%depth - 1
            if (p%depth > 0 .and. p%in_object(p%depth)) &
                p%expect_key(p%depth) = .true.
            event%event_type = JSON_ARRAY_END
        case ('"')
            call parse_string(p, event%string_val, event%event_type)
            if (event%event_type /= JSON_ERROR) then
                if (p%depth > 0 .and. p%expect_key(p%depth)) then
                    event%event_type = JSON_KEY
                else
                    event%event_type = JSON_STRING
                    call mark_value_done(p)
                end if
            end if
        case ('t', 'f')
            call parse_bool(p, event%bool_val, event%event_type)
            call mark_value_done(p)
        case ('n')
            call parse_null(p, event%event_type)
            call mark_value_done(p)
        case ('-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
            call parse_number(p, event%int_val, event%real_val, event%event_type)
            call mark_value_done(p)
        case default
            event%event_type = JSON_ERROR
            p%pos = p%pos + 1
        end select
    end subroutine json_parser_next

    subroutine json_event_init(event)
        type(json_event_t), intent(inout) :: event
        event%event_type = JSON_ERROR
        event%int_val = 0
        event%real_val = 0.0_real64
        event%bool_val = .false.
        if (allocated(event%string_val)) deallocate(event%string_val)
    end subroutine json_event_init

    ! Skip commas and colons; set expect_key on colon. Returns first non-separator char.
    subroutine skip_separators(p, ch, event_type)
        type(json_parser_t), intent(inout) :: p
        character(len=1), intent(out) :: ch
        integer, intent(out) :: event_type

        ch = ' '
        event_type = JSON_ERROR
        do
            call skip_whitespace(p)
            if (p%pos > len(p%input)) then
                event_type = JSON_END_OF_INPUT
                return
            end if
            ch = p%input(p%pos:p%pos)
            if (ch == ',') then
                p%pos = p%pos + 1
                cycle
            end if
            if (ch == ':') then
                p%pos = p%pos + 1
                if (p%depth > 0) p%expect_key(p%depth) = .false.
                cycle
            end if
            return
        end do
    end subroutine skip_separators

    subroutine mark_value_done(p)
        type(json_parser_t), intent(inout) :: p
        if (p%depth > 0 .and. p%in_object(p%depth)) &
            p%expect_key(p%depth) = .true.
    end subroutine mark_value_done

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
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_STRING)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    if (allocated(ev%string_val)) result = ev%string_val
                    found = .true.
                    return
                end if
                key_stack(depth) = ''
            case (JSON_INTEGER, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                key_stack(depth) = ''
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
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_INTEGER)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    result = ev%int_val
                    found = .true.
                    return
                end if
                key_stack(depth) = ''
            case (JSON_STRING, JSON_REAL, JSON_BOOL, JSON_NULL_VAL)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                key_stack(depth) = ''
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
                depth = depth + 1
                is_arr(depth) = .false.
                key_stack(depth) = ''
            case (JSON_ARRAY_START)
                depth = depth + 1
                is_arr(depth) = .true.
                arr_idx(depth) = 0
            case (JSON_OBJECT_END, JSON_ARRAY_END)
                depth = depth - 1
            case (JSON_KEY)
                if (allocated(ev%string_val)) key_stack(depth) = ev%string_val
            case (JSON_BOOL)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                if (path_matches(key_stack, arr_idx, is_arr, depth, &
                    segments, n_segs)) then
                    result = ev%bool_val
                    found = .true.
                    return
                end if
                key_stack(depth) = ''
            case (JSON_STRING, JSON_INTEGER, JSON_REAL, JSON_NULL_VAL)
                if (is_arr(depth)) arr_idx(depth) = arr_idx(depth) + 1
                key_stack(depth) = ''
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

    ! Parse "a.b[0].c" → segments ["a","b","0","c"]
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
                    segments(n_segs) = path(start:i - 1)
                end if
                start = i + 1
            end if
            i = i + 1
        end do
        if (start <= plen) then
            n_segs = n_segs + 1
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

    subroutine parse_string(p, val, event_type)
        type(json_parser_t), intent(inout) :: p
        character(len=:), allocatable, intent(out) :: val
        integer, intent(out) :: event_type
        character(len=1) :: ch
        character(len=4) :: hex_buf
        integer :: code, ios

        p%pos = p%pos + 1 ! skip opening "
        val = ''
        event_type = JSON_STRING

        do while (p%pos <= len(p%input))
            ch = p%input(p%pos:p%pos)
            if (ch == '"') then
                p%pos = p%pos + 1
                return
            else if (iachar(ch) == 92) then ! backslash
                p%pos = p%pos + 1
                if (p%pos > len(p%input)) then
                    event_type = JSON_ERROR
                    return
                end if
                ch = p%input(p%pos:p%pos)
                select case (iachar(ch))
                case (34) ! "
                    val = val // '"'
                case (92) ! \
                    val = val // achar(92)
                case (47) ! /
                    val = val // '/'
                case (98) ! b
                    val = val // achar(8)
                case (102) ! f
                    val = val // achar(12)
                case (110) ! n
                    val = val // achar(10)
                case (114) ! r
                    val = val // achar(13)
                case (116) ! t
                    val = val // achar(9)
                case (117) ! u
                    p%pos = p%pos + 1
                    if (p%pos + 3 > len(p%input)) then
                        event_type = JSON_ERROR
                        return
                    end if
                    hex_buf = p%input(p%pos:p%pos + 3)
                    p%pos = p%pos + 3 ! will be incremented below
                    read(hex_buf, '(Z4)', iostat=ios) code
                    if (ios == 0 .and. code >= 0 .and. code < 128) then
                        val = val // achar(code)
                    end if
                case default
                    val = val // ch
                end select
            else
                val = val // ch
            end if
            p%pos = p%pos + 1
        end do
        event_type = JSON_ERROR ! unterminated string
    end subroutine parse_string

    subroutine parse_bool(p, bool_val, event_type)
        type(json_parser_t), intent(inout) :: p
        logical, intent(out) :: bool_val
        integer, intent(out) :: event_type

        if (p%pos + 3 <= len(p%input) .and. &
            p%input(p%pos:p%pos + 3) == 'true') then
            bool_val = .true.
            p%pos = p%pos + 4
            event_type = JSON_BOOL
        else if (p%pos + 4 <= len(p%input) .and. &
                p%input(p%pos:p%pos + 4) == 'false') then
            bool_val = .false.
            p%pos = p%pos + 5
            event_type = JSON_BOOL
        else
            event_type = JSON_ERROR
            p%pos = p%pos + 1
        end if
    end subroutine parse_bool

    subroutine parse_null(p, event_type)
        type(json_parser_t), intent(inout) :: p
        integer, intent(out) :: event_type

        if (p%pos + 3 <= len(p%input) .and. &
            p%input(p%pos:p%pos + 3) == 'null') then
            p%pos = p%pos + 4
            event_type = JSON_NULL_VAL
        else
            event_type = JSON_ERROR
            p%pos = p%pos + 1
        end if
    end subroutine parse_null

    subroutine parse_number(p, int_val, real_val, event_type)
        type(json_parser_t), intent(inout) :: p
        integer, intent(out) :: int_val
        real(real64), intent(out) :: real_val
        integer, intent(out) :: event_type
        integer :: start
        logical :: is_real
        integer :: ios

        start = p%pos
        is_real = .false.

        if (p%pos <= len(p%input) .and. p%input(p%pos:p%pos) == '-') &
            p%pos = p%pos + 1

        do while (p%pos <= len(p%input))
            select case (iachar(p%input(p%pos:p%pos)))
            case (48:57)
                p%pos = p%pos + 1
            case default
                exit
            end select
        end do

        if (p%pos <= len(p%input) .and. p%input(p%pos:p%pos) == '.') then
            is_real = .true.
            p%pos = p%pos + 1
            do while (p%pos <= len(p%input))
                select case (iachar(p%input(p%pos:p%pos)))
                case (48:57)
                    p%pos = p%pos + 1
                case default
                    exit
                end select
            end do
        end if

        if (p%pos <= len(p%input)) then
            select case (p%input(p%pos:p%pos))
            case ('e', 'E')
                is_real = .true.
                p%pos = p%pos + 1
                if (p%pos <= len(p%input)) then
                    select case (p%input(p%pos:p%pos))
                    case ('+', '-')
                        p%pos = p%pos + 1
                    end select
                end if
                do while (p%pos <= len(p%input))
                    select case (iachar(p%input(p%pos:p%pos)))
                    case (48:57)
                        p%pos = p%pos + 1
                    case default
                        exit
                    end select
                end do
            end select
        end if

        if (is_real) then
            event_type = JSON_REAL
            read(p%input(start:p%pos - 1), *, iostat=ios) real_val
            if (ios /= 0) event_type = JSON_ERROR
        else
            event_type = JSON_INTEGER
            read(p%input(start:p%pos - 1), *, iostat=ios) int_val
            if (ios /= 0) event_type = JSON_ERROR
        end if
    end subroutine parse_number

end module fx_json_parse
