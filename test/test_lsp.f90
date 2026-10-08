program test_lsp
    use fx_diag, only: diag_t, DIAG_ERROR, DIAG_WARNING, DIAG_HINT
    use fx_lsp, only: lsp_make_diagnostic, lsp_make_initialize_response, &
        lsp_make_parse_error_response, lsp_make_shutdown_response, &
        lsp_parse_did_open, lsp_parse_did_save, lsp_path_to_uri, &
        lsp_uri_to_path
    implicit none

    integer :: failures
    integer :: passes

    failures = 0
    passes = 0

    call test_initialize_response(failures, passes)
    call test_make_diagnostic(failures, passes)
    call test_parse_did_save(failures, passes)
    call test_parse_did_open(failures, passes)
    call test_parse_did_save_nested_fields(failures, passes)
    call test_parse_did_open_nested_fields(failures, passes)
    call test_parse_error_response(failures, passes)
    call test_shutdown_response_id_handling(failures, passes)
    call test_uri_encoding_roundtrip(failures, passes)

    if (failures == 0) then
        write (*, '(a,i0,a)') 'test_lsp: passed ', passes, ' checks'
    else
        write (*, '(a,i0,a,i0)') 'test_lsp: failed ', failures, ' of ', passes + failures
        stop 1
    end if

contains

    subroutine test_initialize_response(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes

        character(len=:), allocatable :: response

        call lsp_make_initialize_response('17', 'fx-lsp-test', response)

        call expect_true(index(response, '"jsonrpc":"2.0"') > 0, &
            'initialize response includes jsonrpc', failures, passes)
        call expect_true(index(response, '"id":17') > 0, &
            'initialize response uses numeric id', failures, passes)
        call expect_true(index(response, '"openClose":true') > 0, &
            'initialize response enables openClose', failures, passes)
        call expect_true(index(response, '"save":{"includeText":false}') > 0, &
            'initialize response uses includeText false', failures, passes)
        call expect_true(index(response, '"diagnosticProvider"') == 0, &
            'push-only server does not advertise unsupported pull diagnostics', &
            failures, passes)
        call expect_true(index(response, '"name":"fx-lsp-test"') > 0, &
            'initialize response sets server name', failures, passes)
    end subroutine test_initialize_response

    subroutine test_make_diagnostic(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes

        type(diag_t) :: d
        character(len=:), allocatable :: payload

        d = diag_t(file='file:///tmp/sample%20file.f90', line=3, col=12, &
            severity=DIAG_ERROR, message='bad "quote" and \\escape')

        payload = lsp_make_diagnostic(d)

        call expect_true(index(payload, '"line":2') > 0, &
            'diagnostic converts 1-based line to 0-based', failures, passes)
        call expect_true(index(payload, '"character":11') > 0, &
            'diagnostic converts 1-based col to 0-based', failures, passes)
        call expect_true(index(payload, '"severity":1') > 0, &
            'diagnostic uses warning-compatible severity mapping', failures, passes)
        call expect_true(index(payload, '"message":"bad') > 0, &
            'diagnostic escapes message', failures, passes)

        d = diag_t(file='file:///tmp/zero.f90', line=0, col=0, &
            severity=DIAG_WARNING, message='start of file')
        payload = lsp_make_diagnostic(d)
        call expect_true(index(payload, '"line":0') > 0, &
            'diagnostic keeps zero line as zero', failures, passes)
        call expect_true(index(payload, '"character":0') > 0, &
            'diagnostic keeps zero column as zero', failures, passes)
        call expect_true(index(payload, '"severity":2') > 0, &
            'diagnostic emits warning severity 2', failures, passes)

        d%end_line = 5
        d%end_col = 7
        d%code = 123
        payload = lsp_make_diagnostic(d)
        call expect_true(index(payload, '"end":{"line":4,"character":6}') > 0, &
            'diagnostic preserves end span', failures, passes)
        call expect_true(index(payload, '"code":123') > 0, &
            'diagnostic preserves stable code', failures, passes)

        d = diag_t(file='file:///tmp/info.f90', line=1, col=1, &
            severity=DIAG_HINT, message='info')
        payload = lsp_make_diagnostic(d)
        call expect_true(index(payload, '"severity":4') > 0, &
            'diagnostic emits hint severity 4', failures, passes)
    end subroutine test_make_diagnostic

    subroutine test_parse_did_save(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text

        block
            character(len=:), allocatable :: payload
            payload = '{' // &
                '"jsonrpc":"2.0",' // &
                '"method":"textDocument/didSave",' // &
                '"id":9,' // &
                '"params":{' // &
                '"textDocument":{"uri":"file:///tmp/%66%6f%6f%20bar.f90"}}}'
            call lsp_parse_did_save(payload, uri, text)

            call expect_true(uri == 'file:///tmp/%66%6f%6f%20bar.f90', &
                'didSave parser extracts uri', failures, passes)
            call expect_true(len(text) == 0, 'didSave parser leaves empty text if absent', failures, passes)
        end block

        block
            character(len=:), allocatable :: payload
            payload = '{' // &
                '"method":"textDocument/didSave",' // &
                '"params":{' // &
                '"textDocument":{' // &
                '"uri":"file:///tmp/test with space.f90",' // &
                '"text":"line 1\nline 2"}}}'
            call lsp_parse_did_save(payload, uri, text)
            call expect_true(uri == 'file:///tmp/test with space.f90', &
                'didSave parser handles non-encoded path', failures, passes)
            call expect_true(text == ('line 1' // achar(10) // 'line 2'), &
                'didSave parser handles escaped newline', failures, passes)
        end block
    end subroutine test_parse_did_save

    subroutine test_parse_did_open(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text

        character(len=:), allocatable :: payload

        payload = '{' // &
            '"jsonrpc":"2.0",' // &
            '"method":"textDocument/didOpen",' // &
            '"params":{' // &
            '"textDocument":{"uri":"file:///tmp/open.txt",' // &
            '"text":"print *, ''ok''"}}'

        call lsp_parse_did_open(payload, uri, text)

        call expect_true(uri == 'file:///tmp/open.txt', &
            'didOpen parser extracts uri', failures, passes)
        call expect_true(text == 'print *, ''ok''', &
            'didOpen parser extracts raw text', failures, passes)
    end subroutine test_parse_did_open

    subroutine test_parse_did_save_nested_fields(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: payload

        payload = '{' // &
            '"uri":"file:///tmp/outer.f90",' // &
            '"jsonrpc":"2.0",' // &
            '"method":"textDocument/didSave",' // &
            '"params":{"textDocument":{"uri":"file:///tmp/inner.f90"}}}'
        call lsp_parse_did_save(payload, uri, text)

        call expect_true(uri == 'file:///tmp/inner.f90', &
            'didSave parser extracts nested textDocument.uri, not top-level uri', failures, passes)
        call expect_true(len(text) == 0, 'didSave parser keeps text empty when absent', failures, passes)
    end subroutine test_parse_did_save_nested_fields

    subroutine test_parse_did_open_nested_fields(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: payload

        payload = '{' // &
            '"text":"top-level text should be ignored",' // &
            '"jsonrpc":"2.0",' // &
            '"method":"textDocument/didOpen",' // &
            '"params":{"textDocument":{"uri":"file:///tmp/inner-open.f90",' // &
            '"text":"nested text line"}}}'
        call lsp_parse_did_open(payload, uri, text)

        call expect_true(uri == 'file:///tmp/inner-open.f90', &
            'didOpen parser extracts nested textDocument.uri, not top-level text', failures, passes)
        call expect_true(text == 'nested text line', &
            'didOpen parser extracts nested textDocument.text', failures, passes)
    end subroutine test_parse_did_open_nested_fields

    subroutine test_parse_error_response(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: response

        call lsp_make_parse_error_response(response)

        call expect_true(index(response, '"jsonrpc":"2.0"') > 0, &
            'parse-error response carries jsonrpc', failures, passes)
        call expect_true(index(response, '"id":null') > 0, &
            'parse-error response id is null', failures, passes)
        call expect_true(index(response, '"error":{"code":-32700') > 0, &
            'parse-error response uses parse error code', failures, passes)
        call expect_true(index(response, '"message":"Parse error"') > 0, &
            'parse-error response includes parse error message', failures, passes)
    end subroutine test_parse_error_response

    subroutine test_shutdown_response_id_handling(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: response

        call lsp_make_shutdown_response('17', response)
        call expect_true(index(response, '"id":17') > 0, &
            'shutdown response preserves numeric id', failures, passes)

        call lsp_make_shutdown_response('-17', response)
        call expect_true(index(response, '"id":-17') > 0, &
            'shutdown response preserves negative integer ids', failures, passes)
        call lsp_make_shutdown_response('abc', response)
        call expect_true(index(response, '"id":"abc"') > 0, &
            'shutdown response preserves string id', failures, passes)
        call expect_true(index(response, '"result":null') > 0, &
            'shutdown response has null result', failures, passes)
    end subroutine test_shutdown_response_id_handling

    subroutine test_uri_encoding_roundtrip(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: path

        uri = lsp_path_to_uri('C:/projects/my space/hello?x=1')
        call expect_true(index(uri, 'file:///C:/') == 1, &
            'Windows drive URI has an empty authority', failures, passes)
        call expect_true(index(uri, '%20') > 0, 'path_to_uri escapes spaces', failures, passes)
        call expect_true(index(uri, 'hello') > 0, 'path_to_uri preserves safe text', failures, passes)

        path = lsp_uri_to_path(uri)
        call expect_true(path == 'C:/projects/my space/hello?x=1', &
            'uri_to_path reverses encoding', failures, passes)
        uri = lsp_path_to_uri('C:' // achar(92) // 'space name' // &
            achar(92) // 'example.f90')
        call expect_true(uri == 'file:///C:/space%20name/example.f90', &
            'Windows native separators produce a valid drive URI', failures, passes)
        uri = lsp_path_to_uri('//server/share/space name.f90')
        call expect_true(uri == 'file://server/share/space%20name.f90', &
            'UNC URI preserves network authority', failures, passes)
        path = lsp_uri_to_path(uri)
        call expect_true(path == '//server/share/space name.f90', &
            'UNC URI decodes to a network path', failures, passes)
    end subroutine test_uri_encoding_roundtrip

    subroutine expect_true(condition, message, failures, passes)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes

        if (condition) then
            passes = passes + 1
            write (*, '(a)') 'ok: ' // trim(message)
        else
            failures = failures + 1
            write (*, '(a)') 'not ok: ' // trim(message)
        end if
    end subroutine expect_true

end program test_lsp
