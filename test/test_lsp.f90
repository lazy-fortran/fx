program test_lsp
    use fx_diag, only: diag_t, DIAG_ERROR, DIAG_WARNING, DIAG_HINT
    use fx_lsp, only: lsp_make_diagnostic, lsp_make_initialize_response, &
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
        call expect_true(index(response, &
            '"diagnosticProvider":{"interFileDependencies":true,"workspaceDiagnostics":false}') > 0, &
            'initialize response has diagnosticProvider', failures, passes)
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

    subroutine test_uri_encoding_roundtrip(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: path

        uri = lsp_path_to_uri('C:/projects/my space/hello?x=1')
        call expect_true(index(uri, '%20') > 0, 'path_to_uri escapes spaces', failures, passes)
        call expect_true(index(uri, 'hello') > 0, 'path_to_uri preserves safe text', failures, passes)

        path = lsp_uri_to_path(uri)
        call expect_true(path == 'C:/projects/my space/hello?x=1', &
            'uri_to_path reverses encoding', failures, passes)
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
