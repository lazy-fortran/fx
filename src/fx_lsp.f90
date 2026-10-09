module fx_lsp
    use, intrinsic :: iso_fortran_env, only: output_unit, int64, real64
    use, intrinsic :: iso_c_binding, only: c_char, c_int
    use fx_diag, only: diag_t
    use fx_json_parse, only: json_extract_string, json_extract_int, &
        json_parser_t, json_event_t, json_parser_init, json_parser_next, &
        JSON_END_OF_INPUT, JSON_ERROR
    implicit none
    private

    type, public :: lsp_server_t
        character(len=64) :: name = ' '
        integer :: capabilities = 0
        logical :: shutdown_received = .false.
        integer :: debounce_ms = 150
    end type lsp_server_t

    type :: lsp_document_t
        character(:), allocatable :: uri, text
        integer :: version = -1
        logical :: pending = .false.
        real(real64) :: due = 0.0_real64
    end type lsp_document_t

    abstract interface
        subroutine lsp_diagnostic_callback(uri, text, diags)
            import :: diag_t
            character(len=*), intent(in) :: uri, text
            type(diag_t), allocatable, intent(out) :: diags(:)
        end subroutine lsp_diagnostic_callback
    end interface

    interface
        integer(c_int) function input_bytes(bytes, capacity, timeout_ms) &
                bind(C, name='fx_stdin_input')
            import :: c_char, c_int
            character(kind=c_char), intent(out) :: bytes(*)
            integer(c_int), intent(in), value :: capacity, timeout_ms
        end function input_bytes
    end interface

    public :: lsp_diagnostic_callback
    public :: lsp_server_init, lsp_server_run
    public :: lsp_read_message, lsp_send_message
    public :: lsp_make_initialize_response
    public :: lsp_publish_diagnostics, lsp_make_diagnostic
    public :: lsp_parse_did_save, lsp_parse_did_open, lsp_parse_did_change
    public :: lsp_make_parse_error_response, lsp_make_shutdown_response
    public :: lsp_path_to_uri, lsp_uri_to_path

contains

    subroutine lsp_server_init(s, name)
        type(lsp_server_t), intent(out) :: s
        character(len=*), intent(in) :: name

        s%name = trim(name)
        s%capabilities = 0
        s%shutdown_received = .false.
    end subroutine lsp_server_init

    subroutine lsp_server_run(s, on_document)
        type(lsp_server_t), intent(inout) :: s
        procedure(lsp_diagnostic_callback) :: on_document
        type(lsp_document_t), allocatable :: documents(:)
        type(diag_t), allocatable :: diagnostics(:)
        character(:), allocatable :: body, method, id, uri, text, response, value
        integer :: content_len, i, j, version, ios, wait_ms, prepared, prepared_version
        logical :: eof, ready, found
        real(real64) :: now, delay

        allocate (documents(0))
        prepared = 0
        do
            now = lsp_now()
            wait_ms = -1
            do i = 1, size(documents)
                if (.not. documents(i)%pending) cycle
                delay = max(0.0_real64, documents(i)%due - now)
                if (wait_ms < 0) wait_ms = ceiling(delay * 1000.0_real64)
                wait_ms = min(wait_ms, ceiling(delay * 1000.0_real64))
            end do
            if (prepared > 0) wait_ms = 0
            call lsp_read_message(body, content_len, eof, wait_ms, ready)
            if (eof) exit
            if (.not. ready) then
                if (prepared > 0) then
                    if (documents(prepared)%version == prepared_version) then
                        call lsp_publish_diagnostics(documents(prepared)%uri, &
                            diagnostics, size(diagnostics), prepared_version)
                    end if
                    prepared = 0
                    cycle
                end if
                now = lsp_now()
                do i = 1, size(documents)
                    if (.not. documents(i)%pending) cycle
                    if (documents(i)%due > now) cycle
                    documents(i)%pending = .false.
                    call on_document(documents(i)%uri, documents(i)%text, diagnostics)
                    prepared = i
                    prepared_version = documents(i)%version
                    exit
                end do
                cycle
            end if
            if (content_len <= 0) cycle
            if (.not. lsp_valid_json(body)) then
                call lsp_make_parse_error_response(response)
                call lsp_send_message(response)
                cycle
            end if
            call json_extract_string(body, 'method', method, found)
            if (.not. found) then
                call lsp_make_parse_error_response(response)
                call lsp_send_message(response)
                cycle
            end if
            select case (method)
            case ('initialize')
                call lsp_extract_json_value(body, 'id', id, found)
                if (.not. found) id = '0'
                call lsp_make_initialize_response(id, s%name, response)
                call lsp_send_message(response)
            case ('shutdown')
                call lsp_extract_json_value(body, 'id', id, found)
                call lsp_make_shutdown_response(id, response)
                call lsp_send_message(response)
                s%shutdown_received = .true.
                prepared = 0
                documents%pending = .false.
            case ('exit')
                if (s%shutdown_received) return
                stop 1
            case ('textDocument/didOpen', 'textDocument/didChange', &
                    'textDocument/didClose')
                if (s%shutdown_received) cycle
                call json_extract_string(body, 'params.textDocument.uri', uri, found)
                if (.not. found) cycle
                j = 0
                do i = 1, size(documents)
                    if (documents(i)%uri == uri) j = i
                end do
                if (method == 'textDocument/didClose') then
                    if (j == 0) cycle
                    documents(j)%pending = .false.
                    documents(j)%version = -1
                    documents(j)%text = ''
                    if (prepared == j) prepared = 0
                    block
                        type(diag_t) :: empty(0)
                        call lsp_publish_diagnostics(uri, empty, 0)
                    end block
                    cycle
                end if
                call lsp_extract_json_value(body, 'params.textDocument.version', &
                    value, found)
                if (.not. found) cycle
                read (value, *, iostat=ios) version
                if (ios /= 0) cycle
                if (j > 0) then
                    if (version <= documents(j)%version) cycle
                else
                    documents = [documents, lsp_document_t()]
                    j = size(documents)
                    documents(j)%uri = uri
                end if
                if (method == 'textDocument/didOpen') then
                    call json_extract_string(body, 'params.textDocument.text', &
                        text, found)
                else
                    call lsp_parse_did_change(body, uri, text, found)
                end if
                if (.not. found) cycle
                documents(j)%text = text
                documents(j)%version = version
                documents(j)%pending = .true.
                documents(j)%due = lsp_now() + real(max(0, s%debounce_ms), &
                    real64) / 1000.0_real64
            end select
        end do
    end subroutine lsp_server_run

    real(real64) function lsp_now() result(now)
        integer(int64) :: count, rate
        call system_clock(count, rate)
        now = real(count, real64) / real(rate, real64)
    end function lsp_now

    subroutine lsp_read_message(content, content_len, eof, timeout_ms, ready)
        character(:), allocatable, intent(out) :: content
        integer, intent(out) :: content_len
        logical, intent(out) :: eof
        integer, intent(in), optional :: timeout_ms
        logical, intent(out), optional :: ready
        character(:), allocatable, save :: buffered
        character(kind=c_char) :: bytes(4096)
        character(len=4096) :: chunk
        character(:), allocatable :: headers, line
        integer :: header_end, body_start, length, newline, colon, ios, n, i, wait_ms
        real(real64) :: deadline

        if (.not. allocated(buffered)) buffered = ''
        content = ''
        content_len = 0
        eof = .false.
        if (present(ready)) ready = .false.
        wait_ms = -1
        if (present(timeout_ms)) wait_ms = timeout_ms
        deadline = lsp_now() + real(max(0, wait_ms), real64) / 1000.0_real64
        do
            header_end = index(buffered, achar(13)//achar(10)//achar(13)//achar(10))
            body_start = header_end + 4
            if (header_end == 0) then
                header_end = index(buffered, achar(10)//achar(10))
                body_start = header_end + 2
            end if
            if (header_end > 0) then
                headers = buffered(:header_end - 1)//achar(10)
                length = -1
                do while (len(headers) > 0)
                    newline = index(headers, achar(10))
                    if (newline == 0) exit
                    line = headers(:newline - 1)
                    headers = headers(newline + 1:)
                    if (index(lsp_to_lower(line), 'content-length:') /= 1) cycle
                    colon = index(line, ':')
                    read (line(colon + 1:), *, iostat=ios) length
                    if (ios /= 0) length = -1
                end do
                if (length < 0 .or. length > 16777216) then
                    eof = .true.
                    return
                end if
                if (len(buffered) >= body_start - 1 + length) then
                    content = buffered(body_start:body_start + length - 1)
                    buffered = buffered(body_start + length:)
                    content_len = length
                    if (present(ready)) ready = .true.
                    return
                end if
            end if
            if (len(buffered) > 16785408) then
                eof = .true.
                return
            end if
            n = int(input_bytes(bytes, 4096_c_int, int(wait_ms, c_int)))
            if (n < 0) then
                eof = .true.
                return
            end if
            if (n == 0) return
            do i = 1, n
                chunk(i:i) = bytes(i)
            end do
            buffered = buffered//chunk(:n)
            if (wait_ms >= 0) then
                wait_ms = max(0, ceiling((deadline - lsp_now()) * 1000.0_real64))
            end if
        end do
    end subroutine lsp_read_message

    subroutine lsp_send_message(content)
        character(len=*), intent(in) :: content
        integer :: body_len

        body_len = len_trim(content)
        write (output_unit, '(A)', advance='no') 'Content-Length: '
        write (output_unit, '(I0)', advance='no') body_len
        write (output_unit, '(A)', advance='no') &
            achar(13) // achar(10) // achar(13) // achar(10)
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
            ',"result":{"capabilities":{"textDocumentSync":{' // &
            '"openClose":true,"change":1,"save":{"includeText":false}}' // &
            '},' // &
            '"serverInfo":{"name":"' // lsp_escape_json(server_name) // &
            '","version":"0.1.0"}}}'
    end subroutine lsp_make_initialize_response

    subroutine lsp_publish_diagnostics(uri, diags, n_diags, version)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: n_diags
        type(diag_t), intent(in) :: diags(n_diags)
        integer, intent(in), optional :: version
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
            '"params":{"uri":"' // lsp_escape_json(uri) // '"'
        if (present(version)) response = response // &
            ',"version":' // lsp_int_to_str(version)
        response = response // ',"diagnostics":[' // trim(diag_payload) // ']}}'
        call lsp_send_message(response)
    end subroutine lsp_publish_diagnostics

    function lsp_make_diagnostic(d) result(res)
        type(diag_t), intent(in) :: d
        character(len=:), allocatable :: res
        character(len=:), allocatable :: msg
        integer :: line_idx
        integer :: char_idx, end_line, end_col

        line_idx = max(0, d%line - 1)
        char_idx = max(0, d%col - 1)
        if (d%line == 0) line_idx = 0
        if (d%col == 0) char_idx = 0
        end_line = max(line_idx, d%end_line - 1)
        end_col = char_idx
        if (d%end_col > 0) end_col = d%end_col - 1
        msg = lsp_escape_json(d%message)
        res = '{"range":{"start":{"line":' // lsp_int_to_str(line_idx) // &
            ',"character":' // lsp_int_to_str(char_idx) // '},' // &
            '"end":{"line":' // lsp_int_to_str(end_line) // &
            ',"character":' // lsp_int_to_str(end_col) // '}},' // &
            '"severity":' // lsp_int_to_str(d%severity + 1) // &
            ',"message":"' // msg // '"'
        if (d%code /= 0) res = res // ',"code":' // lsp_int_to_str(d%code)
        res = res // '}'
    end function lsp_make_diagnostic

    subroutine lsp_parse_did_save(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        logical :: found

        call json_extract_string(content, 'params.textDocument.uri', uri, found)
        if (.not. found) uri = ''

        call json_extract_string(content, 'params.textDocument.text', text, found)
        if (.not. found) text = ''
    end subroutine lsp_parse_did_save

    subroutine lsp_parse_did_open(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        logical :: found

        call json_extract_string(content, 'params.textDocument.uri', uri, found)
        if (.not. found) uri = ''

        call json_extract_string(content, 'params.textDocument.text', text, found)
        if (.not. found) text = ''
    end subroutine lsp_parse_did_open

    subroutine lsp_parse_did_change(content, uri, text, valid)
        character(len=*), intent(in) :: content
        character(:), allocatable, intent(out) :: uri, text
        logical, intent(out), optional :: valid
        logical :: found
        integer :: i
        character(len=64) :: path
        character(:), allocatable :: next_text

        call json_extract_string(content, 'params.textDocument.uri', uri, found)
        if (.not. found) uri = ''
        text = ''
        if (present(valid)) valid = .false.
        i = 1
        do
            write (path, '(a,i0,a)') 'params.contentChanges[', i, '].text'
            call json_extract_string(content, trim(path), next_text, found)
            if (.not. found) exit
            text = next_text
            if (present(valid)) valid = .true.
            i = i + 1
        end do
    end subroutine lsp_parse_did_change

    subroutine lsp_make_shutdown_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable, intent(out) :: response

        response = '{"jsonrpc":"2.0","id":' // trim(lsp_normalize_id(id_str)) // ',"result":null}'
    end subroutine lsp_make_shutdown_response

    subroutine lsp_make_parse_error_response(response)
        character(len=:), allocatable, intent(out) :: response
        response = '{"jsonrpc":"2.0","id":null,' // &
            '"error":{"code":-32700,"message":"Parse error"}}'
    end subroutine lsp_make_parse_error_response

    function lsp_path_to_uri(path) result(uri)
        character(len=*), intent(in) :: path
        character(:), allocatable :: uri, encoded, normalized
        integer :: i

        if (index(lsp_to_lower(path), 'file://') == 1) then
            uri = path
            return
        end if
        normalized = path
        do i = 1, len(normalized)
            if (normalized(i:i) == achar(92)) normalized(i:i) = '/'
        end do
        encoded = ''
        do i = 1, len(normalized)
            encoded = encoded // lsp_encode_uri_char(normalized(i:i))
        end do
        uri = 'file://' // encoded
        if (len(normalized) >= 2) then
            if (normalized(2:2) == ':') uri = 'file:///' // encoded
            if (normalized(:2) == '//') uri = 'file:' // encoded
        end if
    end function lsp_path_to_uri

    function lsp_uri_to_path(uri) result(path)
        character(len=*), intent(in) :: uri
        character(len=:), allocatable :: path
        character(len=:), allocatable :: inner
        if (index(lsp_to_lower(trim(uri)), 'file://') == 1) then
            inner = uri(8:)
            if (index(lsp_to_lower(inner), 'localhost/') == 1) inner = inner(10:)
            if (len(inner) > 0) then
                if (inner(1:1) /= '/') then
                    if (len(inner) >= 2) then
                        if (inner(2:2) /= ':') inner = '//' // inner
                    end if
                end if
            end if
            if (len(inner) >= 3) then
                if (inner(1:1) == '/' .and. inner(3:3) == ':') inner = inner(2:)
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
        integer :: i, n, code
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
                code = iachar(c)
                if (code < 32) then
                    write(hex, '(Z4.4)') code
                    res = res // '\u' // hex
                else
                    res = res // c
                end if
            end select
        end do
    end function lsp_escape_json

    logical function lsp_valid_json(body) result(valid)
        character(len=*), intent(in) :: body
        type(json_parser_t) :: parser
        type(json_event_t) :: event

        valid = .false.
        call json_parser_init(parser, body)
        do
            call json_parser_next(parser, event)
            if (event%event_type == JSON_ERROR) return
            if (event%event_type /= JSON_END_OF_INPUT) cycle
            valid = .true.
            return
        end do
    end function lsp_valid_json

    subroutine lsp_extract_json_value(input, key, value, found)
        character(len=*), intent(in) :: input, key
        character(:), allocatable, intent(out) :: value
        logical, intent(out) :: found
        integer :: number

        call json_extract_string(input, key, value, found)
        if (found) then
            value = '"' // lsp_escape_json(value) // '"'
            return
        end if
        call json_extract_int(input, key, number, found)
        value = ''
        if (found) value = lsp_int_to_str(number)
    end subroutine lsp_extract_json_value

    function lsp_normalize_id(id_str) result(res)
        character(len=*), intent(in) :: id_str
        character(len=:), allocatable :: res
        character(len=:), allocatable :: trimmed
        integer :: i, first_digit
        logical :: all_digits
        trimmed = trim(id_str)
        if (len_trim(trimmed) >= 2) then
            if (trimmed(1:1) == '"' .and. trimmed(len_trim(trimmed):len_trim(trimmed)) == '"') then
                res = trimmed
                return
            end if
        end if

        if (len_trim(trimmed) == 0) then
            res = '0'
            return
        end if

        first_digit = 1
        if (trimmed(1:1) == '-') first_digit = 2
        all_digits = first_digit <= len_trim(trimmed)
        do i = first_digit, len_trim(trimmed)
            if (iachar(trimmed(i:i)) < iachar('0') .or. iachar(trimmed(i:i)) > iachar('9')) then
                all_digits = .false.
                exit
            end if
        end do
        if (all_digits) then
            res = trimmed
        else
            res = '"' // lsp_escape_json(trimmed) // '"'
        end if
    end function lsp_normalize_id

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
            if (i + 2 <= len(s)) then
                if (s(i:i) == '%') then
                    decoded = lsp_hex_value(s(i + 1:i + 2))
                    if (decoded >= 0) then
                        len_out = len_out + 1
                        res = res // achar(decoded)
                        i = i + 3
                        cycle
                    end if
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

end module fx_lsp
