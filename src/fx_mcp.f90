module fx_mcp
    use fx_json_build, only: json_builder_t
    use, intrinsic :: iso_c_binding, only: c_int, c_char
    implicit none
    private

    integer, parameter, public :: MCP_FRAME_UNKNOWN = -1
    integer, parameter, public :: MCP_FRAME_BARE_JSON = 0
    integer, parameter, public :: MCP_FRAME_CONTENT_LENGTH = 1

    integer, parameter :: MAX_LINE = 32768
    integer, parameter :: MAX_ACTIONS = 32
    integer, parameter :: MCP_READ_OK = 0
    integer, parameter :: MCP_READ_EOF = -1
    integer, parameter :: MCP_READ_TOO_LARGE = -2

    type, public :: mcp_tool_t
        character(len=64) :: name = ' '
        character(len=256) :: description = ' '
    end type mcp_tool_t

    type, public :: mcp_server_t
        type(mcp_tool_t) :: tool
        integer :: framing_mode = MCP_FRAME_BARE_JSON
        character(len=32) :: protocol_version = ' '
        character(len=64) :: action_names(MAX_ACTIONS) = ' '
        character(len=256) :: action_descs(MAX_ACTIONS) = ' '
        integer :: n_actions = 0
    end type mcp_server_t

    abstract interface
        subroutine mcp_action_handler(action, params, response, &
                is_error)
            character(len=*), intent(in) :: action
            character(len=*), intent(in) :: params
            character(len=:), allocatable, intent(out) :: response
            logical, intent(out) :: is_error
        end subroutine mcp_action_handler
    end interface

    public :: mcp_action_handler
    public :: mcp_server_init, mcp_server_add_action, mcp_server_run
    public :: mcp_read_message, mcp_send_response
    public :: mcp_make_initialize_response
    public :: mcp_make_tools_list_response
    public :: mcp_make_tool_text_response
    public :: mcp_make_tool_json_response
    public :: mcp_extract_action, mcp_extract_id
    public :: mcp_extract_param
    
    interface
        integer(c_int) function fx_c_get_mcp_framing() bind(C)
            import :: c_int
        end function fx_c_get_mcp_framing

        subroutine fx_c_read_jsonrpc_message(buf, bufsize, nread) bind(C)
            import :: c_int, c_char
            character(kind=c_char), intent(out) :: buf(*)
            integer(c_int), intent(in), value :: bufsize
            integer(c_int), intent(out) :: nread
        end subroutine fx_c_read_jsonrpc_message
    end interface

contains

    subroutine mcp_server_init(s, tool_name, tool_description)
        type(mcp_server_t), intent(out) :: s
        character(len=*), intent(in) :: tool_name
        character(len=*), intent(in) :: tool_description
        integer :: i

        s%tool%name = trim(tool_name)
        s%tool%description = trim(tool_description)
        s%framing_mode = MCP_FRAME_BARE_JSON
        s%protocol_version = '2025-03-26'
        s%n_actions = 0
        do i = 1, MAX_ACTIONS
            s%action_names(i) = ''
            s%action_descs(i) = ''
        end do
    end subroutine mcp_server_init

    subroutine mcp_server_add_action(s, action_name, description)
        type(mcp_server_t), intent(inout) :: s
        character(len=*), intent(in) :: action_name
        character(len=*), intent(in) :: description

        if (s%n_actions >= MAX_ACTIONS) return
        s%n_actions = s%n_actions + 1
        s%action_names(s%n_actions) = trim(action_name)
        s%action_descs(s%n_actions) = trim(description)
    end subroutine mcp_server_add_action

    subroutine mcp_server_run(s, handler)
        type(mcp_server_t), intent(inout) :: s
        procedure(mcp_action_handler) :: handler
        character(len=MAX_LINE) :: line
        character(len=:), allocatable :: response
        character(len=:), allocatable :: params
        character(len=:), allocatable :: action
        character(len=:), allocatable :: id_str
        character(len=:), allocatable :: protocol_ver
        character(len=:), allocatable :: method
        character(len=:), allocatable :: handler_result
        logical :: is_error
        logical :: eof
        integer :: read_status

        do
            line = ' '
            response = ''
            params = ''
            action = ''
            id_str = ''
            protocol_ver = ''
            method = ''
            handler_result = ''
            is_error = .false.

            call mcp_read_message(line, MAX_LINE, s%framing_mode, eof, read_status)
            if (read_status == MCP_READ_TOO_LARGE) then
                call mcp_make_error_response('', -32700, 'parse error', response)
                call mcp_send_response(response, s%framing_mode)
                cycle
            end if
            if (eof) exit
            if (len_trim(line) == 0) cycle

            if (.not. mcp_json_looks_like_object(trim(line))) then
                call mcp_make_error_response('', -32700, 'parse error', response)
                call mcp_send_response(response, s%framing_mode)
                cycle
            end if

            call mcp_extract_action(line, method)
            call mcp_extract_id(line, id_str)

            if (len_trim(method) == 0) then
                if (len_trim(id_str) > 0) then
                    call mcp_make_error_response(id_str, -32600, &
                        'invalid request', response)
                    call mcp_send_response(response, s%framing_mode)
                end if
                cycle
            end if

            select case (trim(method))
            case ('initialize')
                if (len_trim(id_str) == 0) cycle
                call mcp_extract_param(line, 'params.protocolVersion', protocol_ver)
                if (len_trim(protocol_ver) == 0) protocol_ver = s%protocol_version
                call mcp_make_initialize_response(id_str, trim(protocol_ver), &
                                                  s%tool%name, response)
                call mcp_send_response(response, s%framing_mode)
            case ('notifications/initialized')
                cycle
            case ('ping')
                if (len_trim(id_str) == 0) cycle
                response = '{"jsonrpc":"2.0","id":'// &
                           mcp_format_id(id_str)//',"result":{}}'
                call mcp_send_response(response, s%framing_mode)
            case ('tools/list')
                if (len_trim(id_str) == 0) cycle
                call mcp_make_tools_list_response(id_str, s, response)
                call mcp_send_response(response, s%framing_mode)
            case ('tools/call')
                call mcp_extract_param(line, 'params.arguments.action', action)
                call mcp_extract_param(line, 'params', params)
                if (len_trim(action) == 0) then
                    if (len_trim(id_str) > 0) then
                        call mcp_make_error_response(id_str, -32602, &
                                                     'missing action', response)
                        call mcp_send_response(response, s%framing_mode)
                    end if
                    cycle
                end if

                call handler(action, params, handler_result, is_error)
                if (len_trim(id_str) > 0) then
                    call mcp_make_tool_text_response(id_str, handler_result, &
                                                    is_error, response)
                    call mcp_send_response(response, s%framing_mode)
                end if
            case ('shutdown')
                response = '{"jsonrpc":"2.0","id":'// &
                           mcp_format_id(id_str)//',"result":null}'
                if (len_trim(id_str) > 0) then
                    call mcp_send_response(response, s%framing_mode)
                end if
                exit
            case default
                if (len_trim(id_str) > 0) then
                    call mcp_make_error_response(id_str, -32601, &
                                                'method not found', response)
                    call mcp_send_response(response, s%framing_mode)
                end if
            end select
        end do
    end subroutine mcp_server_run

    subroutine mcp_read_message(line, max_len, framing, eof, read_status)
        character(len=*), intent(out) :: line
        integer, intent(in) :: max_len
        integer, intent(inout) :: framing
        logical, intent(out) :: eof
        integer, intent(out) :: read_status
        character(kind=c_char), allocatable :: c_buf(:)
        integer(c_int) :: c_nread
        integer :: i, n

        if (max_len <= 0) then
            line = ' '
            eof = .true.
            read_status = MCP_READ_EOF
            return
        end if

        allocate(character(kind=c_char) :: c_buf(max_len))
        call fx_c_read_jsonrpc_message(c_buf, int(max_len, c_int), c_nread)
        read_status = int(c_nread, kind=4)
        if (read_status == MCP_READ_TOO_LARGE) then
            line = ' '
            eof = .false.
            framing = fx_c_get_mcp_framing()
            deallocate(c_buf)
            return
        end if

        if (c_nread <= 0) then
            line = ' '
            eof = c_nread < 0
            if (c_nread == 0) eof = .true.
            framing = fx_c_get_mcp_framing()
            deallocate(c_buf)
            return
        end if

        line = ' '
        n = min(max_len, int(c_nread))
        do i = 1, n
            line(i:i) = c_buf(i)
        end do
        eof = .false.
        framing = fx_c_get_mcp_framing()
        deallocate(c_buf)
    end subroutine mcp_read_message

    subroutine mcp_send_response(response, framing)
        character(len=*), intent(in) :: response
        integer, intent(in) :: framing
        integer :: out_framing
        character(len=32) :: len_str

        out_framing = framing
        if (out_framing == MCP_FRAME_UNKNOWN) then
            out_framing = fx_c_get_mcp_framing()
        end if
        if (out_framing == MCP_FRAME_UNKNOWN) out_framing = MCP_FRAME_BARE_JSON

        select case (out_framing)
        case (MCP_FRAME_CONTENT_LENGTH)
            write(len_str, '(I0)') len_trim(response)
            write(*, '(A)', advance='no') 'Content-Length: ' // trim(len_str) // &
                achar(13)//achar(10)//achar(13)//achar(10)
            write(*, '(A)', advance='no') trim(response)
        case default
            write(*, '(A)', advance='no') trim(response) // achar(10)
        end select
    end subroutine mcp_send_response

    subroutine mcp_make_initialize_response(id_str, proto_ver, &
            server_name, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: proto_ver
        character(len=*), intent(in) :: server_name
        character(len=:), allocatable, intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//mcp_format_id(id_str)//','// &
                   '"result":{"protocolVersion":"'//trim(proto_ver)//'",'// &
                   '"capabilities":{"tools":{"listChanged":false}},'// &
                   '"serverInfo":{"name":"'//trim(server_name)//'","version":"0.1.0"}}}'
    end subroutine mcp_make_initialize_response

    subroutine mcp_make_tools_list_response(id_str, s, response)
        character(len=*), intent(in) :: id_str
        type(mcp_server_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: response
        integer :: i
        character(len=MAX_LINE) :: enum_json

        enum_json = ' '
        if (s%n_actions > 0) then
            do i = 1, s%n_actions
                if (i > 1) enum_json = trim(enum_json)//','
                enum_json = trim(enum_json)//'"'//trim(s%action_names(i))//'"'
            end do
        end if

        response = '{"jsonrpc":"2.0","id":'//mcp_format_id(id_str)//','// &
                   '"result":{"tools":[{"name":"'//trim(s%tool%name)//'",'// &
                   '"description":"'//trim(s%tool%description)//'",'// &
                   '"inputSchema":{"type":"object","properties":{'// &
                   '"action":{"type":"string","enum":['//trim(enum_json)//'],'// &
                   '"description":"Action to run"}},'// &
                   '"required":["action"]}}]}}'
    end subroutine mcp_make_tools_list_response

    subroutine mcp_make_tool_text_response(id_str, text, is_error, &
            response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: text
        logical, intent(in) :: is_error
        character(len=:), allocatable, intent(out) :: response
        character(len=:), allocatable :: escaped

        escaped = json_escape(text)
        response = '{"jsonrpc":"2.0","id":'//mcp_format_id(id_str)// &
                   ',"result":{"content":[{"type":"text","text":"'// &
                   trim(escaped)//'"}],"isError":'// &
                   mcp_bool_to_json(is_error)//'}}'
    end subroutine mcp_make_tool_text_response

    subroutine mcp_make_tool_json_response(id_str, jb, is_error, &
            response)
        character(len=*), intent(in) :: id_str
        type(json_builder_t), intent(in) :: jb
        logical, intent(in) :: is_error
        character(len=:), allocatable, intent(out) :: response
        character(len=MAX_LINE) :: json_text

        json_text = jb%buf%buf
        if (len_trim(json_text) == 0) json_text = '{}'
        response = '{"jsonrpc":"2.0","id":'//mcp_format_id(id_str)// &
                   ',"result":{"content":[{"type":"text","text":'// &
                   trim(json_text)//'}],"isError":'// &
                   mcp_bool_to_json(is_error)//'}}'
    end subroutine mcp_make_tool_json_response

    subroutine mcp_extract_action(line, action)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: action
        call mcp_extract_param(line, 'method', action)
    end subroutine mcp_extract_action

    subroutine mcp_extract_id(line, id_str)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: id_str
        character(len=:), allocatable :: raw

        call mcp_extract_param(line, 'id', raw)
        if (len_trim(raw) == 0) then
            id_str = ''
            return
        end if

        if (len_trim(raw) >= 2) then
            if (raw(1:1) == '"' .and. raw(len_trim(raw):len_trim(raw)) == '"') then
                if (len_trim(raw) > 2) then
                    id_str = raw(2:len_trim(raw)-1)
                else
                    id_str = ''
                end if
            else
                id_str = trim(adjustl(raw))
            end if
        else
            id_str = trim(adjustl(raw))
        end if
    end subroutine mcp_extract_id

    subroutine mcp_extract_param(line, param_name, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable, intent(out) :: value

        character(len=MAX_LINE) :: raw_value
        logical :: found

        call mcp_json_find_value(trim(line), trim(param_name), raw_value, found)
        if (.not. found) then
            value = ''
            return
        end if

        if (len_trim(raw_value) >= 2) then
            if (raw_value(1:1) == '"' .and. &
                raw_value(len_trim(raw_value):len_trim(raw_value)) == '"') then
                if (len_trim(raw_value) > 2) then
                    value = raw_value(2:len_trim(raw_value)-1)
                else
                    value = ''
                end if
            else
                value = trim(adjustl(raw_value))
            end if
        else
            value = trim(adjustl(raw_value))
        end if
    end subroutine mcp_extract_param

    subroutine mcp_make_error_response(id_str, code, msg, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: code
        character(len=*), intent(in) :: msg
        character(len=:), allocatable, intent(out) :: response
        response = '{"jsonrpc":"2.0","id":'// &
                   mcp_format_id(id_str)//',"error":{'// &
                   '"code":'//mcp_int_to_string(code)//','// &
                   '"message":"'//trim(msg)//'"}}'
    end subroutine mcp_make_error_response

    function mcp_bool_to_json(v) result(json_bool)
        logical, intent(in) :: v
        character(len=5) :: json_bool
        if (v) then
            json_bool = 'true'
        else
            json_bool = 'false'
        end if
    end function mcp_bool_to_json

    function mcp_format_id(id_str) result(out)
        character(len=*), intent(in) :: id_str
        character(len=MAX_LINE) :: out

        if (len_trim(id_str) == 0) then
            out = 'null'
        else if (is_integer_literal(trim(adjustl(id_str)))) then
            out = trim(adjustl(id_str))
        else
            out = '"'//trim(adjustl(id_str))//'"'
        end if
    end function mcp_format_id

    function is_integer_literal(s) result(ok)
        character(len=*), intent(in) :: s
        logical :: ok
        integer :: i, len_s
        character :: c

        ok = .true.
        len_s = len_trim(s)
        if (len_s == 0) then
            ok = .false.
            return
        end if

        do i = 1, len_s
            c = s(i:i)
            if (i == 1 .and. (c == '-' .or. c == '+')) cycle
            if (c < '0' .or. c > '9') then
                ok = .false.
                return
            end if
        end do
    end function is_integer_literal

    function json_escape(s) result(out)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: out
        integer :: i
        integer :: len_s
        character(len=6) :: esc
        character :: c

        out = ''
        len_s = len_trim(s)
        do i = 1, len_s
            c = s(i:i)
            select case (c)
            case ('"')
                out = trim(out)//'\\"'
            case ('\\')
                out = trim(out)//'\\\\'
            case (achar(10))
                out = trim(out)//'\\n'
            case (achar(13))
                out = trim(out)//'\\r'
            case (achar(9))
                out = trim(out)//'\\t'
            case default
                if (iachar(c) < 32) then
                    write (esc, '("\\u",Z4.4)') iachar(c)
                    out = trim(out)//trim(esc)
                else
                    out = trim(out)//c
                end if
            end select
        end do
    end function json_escape

    function mcp_int_to_string(i) result(s)
        integer, intent(in) :: i
        character(len=32) :: s
        write(s, '(I0)') i
    end function mcp_int_to_string

    logical function mcp_json_looks_like_object(s)
        character(len=*), intent(in) :: s
        integer :: i, len_s
        integer :: depth
        logical :: in_str, escaped

        mcp_json_looks_like_object = .false.
        len_s = len_trim(s)
        if (len_s == 0) return

        if (first_non_ws_index(s) == 0) return
        if (s(first_non_ws_index(s):first_non_ws_index(s)) /= '{') return
        if (s(last_non_ws_index(s):last_non_ws_index(s)) /= '}') return

        depth = 0
        in_str = .false.
        escaped = .false.
        do i = 1, len_s
            if (in_str) then
                if (escaped) then
                    escaped = .false.
                else if (s(i:i) == '\\') then
                    escaped = .true.
                else if (s(i:i) == '"') then
                    in_str = .false.
                end if
            else
                select case (s(i:i))
                case ('"')
                    in_str = .true.
                case ('{')
                    depth = depth + 1
                case ('}')
                    depth = depth - 1
                    if (depth < 0) return
                end select
            end if
        end do

        if (depth /= 0) return
        mcp_json_looks_like_object = .true.
    end function mcp_json_looks_like_object

    integer function first_non_ws_index(s) result(pos)
        character(len=*), intent(in) :: s
        integer :: i
        pos = 0
        do i = 1, len(s)
            if (.not. is_json_ws(s(i:i))) then
                pos = i
                return
            end if
        end do
    end function first_non_ws_index

    integer function last_non_ws_index(s) result(pos)
        character(len=*), intent(in) :: s
        integer :: i
        pos = 0
        do i = len(s), 1, -1
            if (.not. is_json_ws(s(i:i))) then
                pos = i
                return
            end if
        end do
    end function last_non_ws_index

    logical function is_json_ws(c)
        character(len=1), intent(in) :: c
        is_json_ws = (c == ' ' .or. c == achar(9) .or. &
                      c == achar(10) .or. c == achar(13))
    end function is_json_ws

    subroutine skip_ws(text, pos_in, end_idx, pos_out)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos_in
        integer, intent(in) :: end_idx
        integer, intent(out) :: pos_out
        pos_out = pos_in
        do while (pos_out <= end_idx)
            if (.not. is_json_ws(text(pos_out:pos_out))) return
            pos_out = pos_out + 1
        end do
    end subroutine skip_ws

    subroutine parse_quoted_string(text, pos_in, val_start, val_end, pos_out)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos_in
        integer, intent(out) :: val_start
        integer, intent(out) :: val_end
        integer, intent(out) :: pos_out
        logical :: escaped

        val_start = 0
        val_end = 0
        pos_out = pos_in
        escaped = .false.

        if (pos_in > len(text) .or. text(pos_in:pos_in) /= '"') return
        pos_out = pos_in + 1
        val_start = pos_out

        do while (pos_out <= len(text))
            if (escaped) then
                escaped = .false.
            else if (text(pos_out:pos_out) == '\\') then
                escaped = .true.
            else if (text(pos_out:pos_out) == '"') then
                val_end = pos_out - 1
                pos_out = pos_out + 1
                return
            end if
            pos_out = pos_out + 1
        end do
    end subroutine parse_quoted_string

    subroutine parse_value_bounds(text, pos_in, end_idx, value_start, value_end)
        character(len=*), intent(in) :: text
        integer, intent(in) :: pos_in
        integer, intent(in) :: end_idx
        integer, intent(out) :: value_start
        integer, intent(out) :: value_end

        integer :: depth, p
        logical :: in_str, escaped

        call skip_ws(text, pos_in, end_idx, p)
        if (p > end_idx) then
            value_start = 0
            value_end = 0
            return
        end if

        value_start = p
        select case (text(p:p))
        case ('"')
            call parse_quoted_string(text, p, value_start, value_end, p)
        case ('{')
            depth = 1
            in_str = .false.
            escaped = .false.
            p = p + 1
            do while (p <= end_idx)
                if (in_str) then
                    if (escaped) then
                        escaped = .false.
                    else if (text(p:p) == '\\') then
                        escaped = .true.
                    else if (text(p:p) == '"') then
                        in_str = .false.
                    end if
                else
                    if (text(p:p) == '"') then
                        in_str = .true.
                    else if (text(p:p) == '{') then
                        depth = depth + 1
                    else if (text(p:p) == '}') then
                        depth = depth - 1
                        if (depth == 0) then
                            value_end = p
                            return
                        end if
                    end if
                end if
                p = p + 1
            end do
        case ('[')
            depth = 1
            in_str = .false.
            escaped = .false.
            p = p + 1
            do while (p <= end_idx)
                if (in_str) then
                    if (escaped) then
                        escaped = .false.
                    else if (text(p:p) == '\\') then
                        escaped = .true.
                    else if (text(p:p) == '"') then
                        in_str = .false.
                    end if
                else
                    if (text(p:p) == '"') then
                        in_str = .true.
                    else if (text(p:p) == '[') then
                        depth = depth + 1
                    else if (text(p:p) == ']') then
                        depth = depth - 1
                        if (depth == 0) then
                            value_end = p
                            return
                        end if
                    end if
                end if
                p = p + 1
            end do
        case default
            do while (p <= end_idx)
                if (p < len(text) .and. is_json_ws(text(p:p))) then
                    value_end = p - 1
                    return
                end if
                if (text(p:p) == ',' .or. text(p:p) == '}' .or. text(p:p) == ']') then
                    value_end = p - 1
                    return
                end if
                p = p + 1
            end do
            value_end = end_idx
        end select
    end subroutine parse_value_bounds

    subroutine json_find_member(json_text, start_idx, end_idx, key, value_start, &
                               value_end, found)
        character(len=*), intent(in) :: json_text
        integer, intent(in) :: start_idx, end_idx
        character(len=*), intent(in) :: key
        integer, intent(out) :: value_start, value_end
        logical, intent(out) :: found

        integer :: pos, key_start, key_end, ks
        integer :: skipped_start, skipped_end

        found = .false.
        value_start = 0
        value_end = 0
        pos = start_idx

        if (pos <= end_idx .and. json_text(pos:pos) == '{') pos = pos + 1
        do while (pos <= end_idx)
            call skip_ws(json_text, pos, end_idx, pos)
            if (pos > end_idx) return
            if (json_text(pos:pos) == '}') return
            if (json_text(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            if (json_text(pos:pos) /= '"') return

            call parse_quoted_string(json_text, pos, key_start, key_end, pos)
            if (key_start == 0 .or. key_end < key_start) return
            call skip_ws(json_text, pos, end_idx, pos)
            if (pos > end_idx .or. json_text(pos:pos) /= ':') return
            pos = pos + 1
            ks = len_trim(key)
            if (trim(json_text(key_start:key_end)) /= trim(key)) then
                call parse_value_bounds(json_text, pos, end_idx, skipped_start, skipped_end)
                if (skipped_start == 0) return
                pos = skipped_end + 1
                cycle
            end if

            call parse_value_bounds(json_text, pos, end_idx, value_start, value_end)
            if (value_start == 0) return
            found = .true.
            return
        end do
    end subroutine json_find_member

    subroutine mcp_json_find_value(json_text, path, value, found)
        character(len=*), intent(in) :: json_text
        character(len=*), intent(in) :: path
        character(len=MAX_LINE), intent(out) :: value
        logical, intent(out) :: found

        integer :: segment_start, segment_end
        integer :: search_start, search_end
        integer :: value_start, value_end, i
        character(len=128) :: segment

        value = ' '
        found = .false.
        segment_start = 1
        search_start = first_non_ws_index(json_text)
        if (search_start == 0) return
        search_end = last_non_ws_index(json_text)
        if (search_start >= search_end) return

        i = 1
        do while (i <= len_trim(path))
            segment_start = i
            do while (i <= len_trim(path) .and. path(i:i) /= '.')
                i = i + 1
            end do
            segment = path(segment_start:i-1)

            call json_find_member(json_text, search_start, search_end, &
                                 trim(segment), value_start, value_end, found)
            if (.not. found) return

            if (i > len_trim(path)) then
                value = json_text(value_start:value_end)
                return
            end if

            if (json_text(value_start:value_start) /= '{') return
            search_start = value_start + 1
            search_end = value_end - 1
            i = i + 1
        end do
    end subroutine mcp_json_find_value

end module fx_mcp
