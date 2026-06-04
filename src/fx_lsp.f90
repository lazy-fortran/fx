module fx_lsp
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fx_diag, only: diag_t
    implicit none
    private

    type, public :: lsp_server_t
        character(len=64) :: name = ' '
        integer :: capabilities = 0
        logical :: shutdown_received = .false.
    end type lsp_server_t

    abstract interface
        subroutine lsp_save_callback(uri, text)
            character(len=*), intent(in) :: uri
            character(len=*), intent(in) :: text
        end subroutine lsp_save_callback
    end interface

    public :: lsp_save_callback
    public :: lsp_server_init, lsp_server_run
    public :: lsp_read_message, lsp_send_message
    public :: lsp_make_initialize_response
    public :: lsp_publish_diagnostics, lsp_make_diagnostic
    public :: lsp_parse_did_save, lsp_parse_did_open
    public :: lsp_path_to_uri, lsp_uri_to_path

contains

    subroutine lsp_server_init(s, name)
        type(lsp_server_t), intent(out) :: s
        character(len=*), intent(in) :: name

        s%name = trim(name)
        s%capabilities = 0
        s%shutdown_received = .false.
    end subroutine lsp_server_init

    subroutine lsp_server_run(s, on_save_callback)
        type(lsp_server_t), intent(inout) :: s
        procedure(lsp_save_callback) :: on_save_callback

        character(len=4096) :: body
        integer :: content_len
        logical :: eof
        character(len=:), allocatable :: method
        character(len=:), allocatable :: id
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: response
        logical :: found

        do
            call lsp_read_message(body, content_len, eof)
            if (eof) exit
            if (content_len <= 0) cycle

            call lsp_extract_json_string(body, 'method', method, found)
            if (.not. found) cycle

            if (trim(method) == 'initialize') then
                call lsp_extract_json_value(body, 'id', id, found)
                if (.not. found) id = '0'
                call lsp_make_initialize_response(id, s%name, response)
                call lsp_send_message(response)
            else if (trim(method) == 'initialized') then
                cycle
            else if (trim(method) == 'shutdown') then
                call lsp_extract_json_value(body, 'id', id, found)
                response = '{"jsonrpc":"2.0","id":' // trim(id) // ',"result":null}'
                call lsp_send_message(response)
                s%shutdown_received = .true.
            else if (trim(method) == 'exit') then
                if (s%shutdown_received) return
                stop 1
            else if (trim(method) == 'textDocument/didSave') then
                call lsp_parse_did_save(body, uri, text)
                if (len(uri) > 0) call on_save_callback(uri, text)
            else if (trim(method) == 'textDocument/didOpen') then
                call lsp_parse_did_open(body, uri, text)
                if (len(uri) > 0) call on_save_callback(uri, text)
            end if
        end do
    end subroutine lsp_server_run

    subroutine lsp_read_message(content, content_len, eof)
        character(len=*), intent(out) :: content
        integer, intent(out) :: content_len
        logical, intent(out) :: eof
        character(len=256) :: header
        character(len=:), allocatable :: body
        integer :: ios
        integer :: iostat_value
        integer :: target_len
        integer :: bytes_read
        integer :: colon_pos

        content = ' '
        content_len = 0
        eof = .false.

        do
            read (*, '(A)', iostat=ios) header
            if (ios /= 0) then
                eof = .true.
                return
            end if
            if (lsp_trim_crlf(header) == '') exit

            if (index(lsp_to_lower(trim(header)), 'content-length:') == 1) then
                colon_pos = index(header, ':')
                if (colon_pos > 0) then
                    read (header(colon_pos + 1:), *) target_len
                else
                    target_len = 0
                end if
            end if
        end do

        if (target_len <= 0) return

        allocate (character(len=target_len) :: body)
        read (*, '(A)', advance='no', iostat=iostat_value, size=bytes_read) body
        if (iostat_value /= 0 .and. iostat_value /= -1) then
            bytes_read = 0
        end if

        if (bytes_read <= 0) then
            eof = .true.
            deallocate (body)
            return
        end if

        content_len = min(bytes_read, target_len, len(content))
        content = ' '
        if (content_len > 0) content(1:content_len) = body(1:content_len)
        deallocate (body)
    end subroutine lsp_read_message

    subroutine lsp_send_message(content)
        character(len=*), intent(in) :: content
        integer :: body_len

        body_len = len_trim(content)
        write (output_unit, '(A)', advance='no') 'Content-Length: '
        write (output_unit, '(I0)', advance='no') body_len
        write (output_unit, '(A)', advance='no') achar(13) // achar(10) // achar(13) // achar(10)
        if (body_len > 0) write (output_unit, '(A)', advance='no') content(1:body_len)
        call flush(output_unit)
    end subroutine lsp_send_message

    subroutine lsp_make_initialize_response(id_str, server_name, &
            response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: server_name
        character(len=:), allocatable, intent(out) :: response

        character(len=:), allocatable :: normalized_id

        normalized_id = lsp_normalize_id(id_str)
        response = '{"jsonrpc":"2.0","id":' // trim(normalized_id) // &
            ',"result":{"capabilities":{"textDocumentSync":{"openClose":true,"save":{"includeText":false}},' // &
            '"diagnosticProvider":{"interFileDependencies":true,"workspaceDiagnostics":false}},' // &
            '"serverInfo":{"name":"' // trim(server_name) // '","version":"0.1.0"}}}'
    end subroutine lsp_make_initialize_response

    subroutine lsp_publish_diagnostics(uri, diags, n_diags)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: n_diags
        type(diag_t), intent(in) :: diags(n_diags)
        character(len=:), allocatable :: response
        character(len=:), allocatable :: diag_payload
        integer :: i

        diag_payload = ''
        if (n_diags > 0) then
            do i = 1, n_diags
                if (i > 1) diag_payload = trim(diag_payload) // ','
                diag_payload = trim(diag_payload) // lsp_make_diagnostic(diags(i))
            end do
        end if

        response = '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics",' // &
            '"params":{"uri":"' // trim(uri) // '","diagnostics":[' // &
            trim(diag_payload) // ']}}'
        call lsp_send_message(response)
    end subroutine lsp_publish_diagnostics

    function lsp_make_diagnostic(d) result(res)
        type(diag_t), intent(in) :: d
        character(len=:), allocatable :: res
        character(len=:), allocatable :: msg
        integer :: line_idx
        integer :: char_idx

        line_idx = max(0, d%line - 1)
        char_idx = max(0, d%col - 1)
        if (d%line == 0) line_idx = 0
        if (d%col == 0) char_idx = 0
        msg = lsp_escape_json(d%message)
        res = '{"range":{"start":{"line":' // lsp_int_to_str(line_idx) // &
            ',"character":' // lsp_int_to_str(char_idx) // '},' // &
            '"end":{"line":' // lsp_int_to_str(line_idx) // &
            ',"character":' // lsp_int_to_str(char_idx) // '}},' // &
            '"severity":' // lsp_int_to_str(d%severity + 1) // ',"message":"' // msg // '"}'
    end function lsp_make_diagnostic

    subroutine lsp_parse_did_save(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        logical :: found

        call lsp_extract_json_string(content, 'uri', uri, found)
        if (.not. found) uri = ''

        call lsp_extract_json_string(content, 'text', text, found)
        if (.not. found) text = ''
    end subroutine lsp_parse_did_save

    subroutine lsp_parse_did_open(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        logical :: found

        call lsp_extract_json_string(content, 'uri', uri, found)
        if (.not. found) uri = ''

        call lsp_extract_json_string(content, 'text', text, found)
        if (.not. found) text = ''
    end subroutine lsp_parse_did_open

    function lsp_path_to_uri(path) result(uri)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: encoded
        integer :: i

        encoded = ''
        do i = 1, len(trim(path))
            encoded = trim(encoded) // lsp_encode_uri_char(path(i:i))
        end do

        if (index(lsp_to_lower(trim(path)), 'file://') == 1) then
            uri = trim(encoded)
        else
            uri = 'file://' // encoded
        end if
    end function lsp_path_to_uri

    function lsp_uri_to_path(uri) result(path)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable :: path
        character(len=:), allocatable :: inner
        integer :: start

        if (index(lsp_to_lower(trim(uri)), 'file://') == 1) then
            inner = uri(8:)
            if (len_trim(inner) >= 3 .and. inner(1:1) == '/' .and. inner(3:3) == ':') then
                inner = inner(2:)
            end if
            path = lsp_decode_uri(inner)
        else
            path = lsp_decode_uri(uri)
        end if
    end function lsp_uri_to_path

    function lsp_int_to_str(v) result(res)
        integer, intent(in) :: v
        character(len=:), allocatable :: res
        character(len=32) :: buffer
        write (buffer, '(I0)') v
        res = trim(adjustl(buffer))
    end function lsp_int_to_str

    function lsp_escape_json(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i, n
        character(len=1) :: c
        character(len=4) :: hex

        res = ''
        n = len_trim(s)
        do i = 1, n
            c = s(i:i)
            select case (c)
            case ('\')
                res = res // '\\'
            case ('"')
                res = res // '\"'
            case (achar(8))
                res = res // '\b'
            case (achar(9))
                res = res // '\t'
            case (achar(10))
                res = res // '\n'
            case (achar(13))
                res = res // '\r'
            case (achar(12))
                res = res // '\f'
            case default
                if (iachar(c) < 32) then
                    write (hex, '(Z2.2)') iachar(c)
                    res = res // '\u' // trim(hex)
                else
                    res = res // c
                end if
            end select
        end do
    end function lsp_escape_json

    subroutine lsp_extract_json_string(input, key, value, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found

        call lsp_extract_json_value(input, key, value, found)
    end subroutine lsp_extract_json_string

    subroutine lsp_extract_json_value(input, key, value, found)
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: value
        logical, intent(out) :: found

        integer :: key_pos
        integer :: cursor
        integer :: pos
        integer :: write_pos
        integer :: raw_len
        integer :: cbyte
        logical :: escape
        character(len=:), allocatable :: raw

        found = .false.
        value = ''
        key_pos = 0
        do
            cursor = index(input(key_pos + 1:), '"' // trim(key) // '"')
            if (cursor == 0) return

            key_pos = key_pos + cursor

            cursor = key_pos + len_trim(key) + 2
            do while (cursor <= len(input) .and. lsp_is_ws(input(cursor:cursor)))
                cursor = cursor + 1
            end do
            if (cursor <= len(input) .and. input(cursor:cursor) == ':') exit
        end do
        cursor = cursor + 1
        do while (cursor <= len(input) .and. lsp_is_ws(input(cursor:cursor)))
            cursor = cursor + 1
        end do
        if (cursor > len(input) .or. input(cursor:cursor) /= '"') return

        cursor = cursor + 1
        raw_len = len_trim(input)
        allocate (character(len=raw_len) :: raw)
        write_pos = 0
        escape = .false.
        do while (cursor <= len(input))
            if (cursor > len(input)) exit

            if (escape) then
                select case (input(cursor:cursor))
                case ('"')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = '"'
                case ('\')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = '\'
                case ('/')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = '/'
                case ('b')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = achar(8)
                case ('f')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = achar(12)
                case ('n')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = achar(10)
                case ('r')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = achar(13)
                case ('t')
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = achar(9)
                case ('u')
                    if (cursor + 4 <= len(input)) then
                        cbyte = lsp_hex_value(input(cursor + 1:cursor + 2)) * 16 + &
                            lsp_hex_value(input(cursor + 3:cursor + 4))
                        write_pos = write_pos + 1
                        raw(write_pos:write_pos) = achar(cbyte)
                        cursor = cursor + 4
                    end if
                case default
                    write_pos = write_pos + 1
                    raw(write_pos:write_pos) = input(cursor:cursor)
                end select
                escape = .false.
            else if (input(cursor:cursor) == '\') then
                escape = .true.
            else if (input(cursor:cursor) == '"') then
                exit
            else
                write_pos = write_pos + 1
                raw(write_pos:write_pos) = input(cursor:cursor)
            end if
            cursor = cursor + 1
        end do

        if (write_pos > 0) then
            value = raw(1:write_pos)
        else
            value = ''
        end if
        deallocate (raw)
        found = .true.
    end subroutine lsp_extract_json_value

    function lsp_normalize_id(id_str) result(res)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable :: res
        character(len=:), allocatable :: trimmed
        integer :: i
        logical :: all_digits
        trimmed = trim(id_str)
        if (len_trim(trimmed) >= 2) then
            if (trimmed(1:1) == '"' .and. trimmed(len_trim(trimmed):len_trim(trimmed)) == '"') then
                trimmed = trimmed(2:len_trim(trimmed)-1)
            end if
        end if

        if (len_trim(trimmed) == 0) then
            res = '0'
            return
        end if

        all_digits = .true.
        do i = 1, len_trim(trimmed)
            if (iachar(trimmed(i:i)) < iachar('0') .or. iachar(trimmed(i:i)) > iachar('9')) then
                all_digits = .false.
                exit
            end if
        end do
        if (all_digits) then
            res = trimmed
        else
            res = '"' // trimmed // '"'
        end if
    end function lsp_normalize_id

    function lsp_is_ws(ch) result(is_ws)
        character(len=1), intent(in) :: ch
        logical :: is_ws
        is_ws = (ch == ' ' .or. ch == char(9) .or. ch == char(10) .or. ch == char(13))
    end function lsp_is_ws

    function lsp_to_lower(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i

        res = trim(s)
        do i = 1, len_trim(res)
            if (res(i:i) >= 'A' .and. res(i:i) <= 'Z') then
                res(i:i) = achar(iachar(res(i:i)) + 32)
            end if
        end do
    end function lsp_to_lower

    function lsp_trim_crlf(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: end_pos

        end_pos = len_trim(s)
        do while (end_pos > 0 .and. (s(end_pos:end_pos) == char(13) .or. s(end_pos:end_pos) == char(10)))
            end_pos = end_pos - 1
        end do

        if (end_pos <= 0) then
            res = ''
        else
            res = s(1:end_pos)
        end if
    end function lsp_trim_crlf

    function lsp_is_unreserved(c) result(is_unreserved)
        character(len=1), intent(in) :: c
        logical :: is_unreserved

        is_unreserved = (c >= 'a' .and. c <= 'z') .or. &
            (c >= 'A' .and. c <= 'Z') .or. (c >= '0' .and. c <= '9') .or. &
            any(c == (/'-', '.', '_', ':', '/', '~'/))
    end function lsp_is_unreserved

    function lsp_encode_uri_char(c) result(res)
        character(len=1), intent(in) :: c
        character(len=:), allocatable :: res
        character(len=2) :: code

        if (lsp_is_unreserved(c) .and. c /= '%') then
            res = c
        else
            if (c == ' ') then
                res = '%20'
            else
                write (code, '(Z2.2)') iachar(c)
                res = '%' // code
            end if
        end if
    end function lsp_encode_uri_char

    function lsp_decode_uri(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i
        integer :: len_out
        integer :: decoded
        res = ''
        len_out = 0
        i = 1
        do while (i <= len(s))
            if (s(i:i) == '%' .and. i + 2 <= len(s)) then
                decoded = lsp_hex_value(s(i + 1:i + 2))
                if (decoded >= 0) then
                    len_out = len_out + 1
                    res = res // achar(decoded)
                    i = i + 3
                    cycle
                end if
            end if
            len_out = len_out + 1
            res = res // s(i:i)
            i = i + 1
        end do
        if (len_out == 0) res = ''
    end function lsp_decode_uri

    function lsp_hex_value(hex_pair) result(res)
        character(len=2), intent(in) :: hex_pair
        integer :: res

        if (hex_pair(1:1) >= '0' .and. hex_pair(1:1) <= '9') then
            res = iachar(hex_pair(1:1)) - iachar('0')
        else if (hex_pair(1:1) >= 'a' .and. hex_pair(1:1) <= 'f') then
            res = 10 + (iachar(hex_pair(1:1)) - iachar('a'))
        else if (hex_pair(1:1) >= 'A' .and. hex_pair(1:1) <= 'F') then
            res = 10 + (iachar(hex_pair(1:1)) - iachar('A'))
        else
            res = -1
            return
        end if

        if (hex_pair(2:2) >= '0' .and. hex_pair(2:2) <= '9') then
            res = res * 16 + (iachar(hex_pair(2:2)) - iachar('0'))
        else if (hex_pair(2:2) >= 'a' .and. hex_pair(2:2) <= 'f') then
            res = res * 16 + 10 + (iachar(hex_pair(2:2)) - iachar('a'))
        else if (hex_pair(2:2) >= 'A' .and. hex_pair(2:2) <= 'F') then
            res = res * 16 + 10 + (iachar(hex_pair(2:2)) - iachar('A'))
        else
            res = -1
            return
        end if
    end function lsp_hex_value

    function lsp_to_string(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        res = trim(s)
    end function lsp_to_string

end module fx_lsp
