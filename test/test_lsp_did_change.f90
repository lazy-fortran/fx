program test_lsp_did_change
    use fx_lsp, only: lsp_parse_did_change
    implicit none

    integer :: failures
    integer :: passes

    failures = 0
    passes = 0

    call test_unicode_and_multiple_changes(failures, passes)
    call test_parse_did_change_basic(failures, passes)
    call test_parse_did_change_with_newlines(failures, passes)
    call test_parse_did_change_with_escaped_quotes(failures, passes)

    if (failures == 0) then
        write (*, '(a,i0,a)') 'test_lsp_did_change: passed ', passes, ' checks'
        stop 0
    else
        write (*, '(a,i0,a,i0)') 'FAIL: test_lsp_did_change: failed ', failures, ' of ', passes + failures
        stop 1
    end if

contains

    subroutine test_unicode_and_multiple_changes(failures, passes)
        integer, intent(inout) :: failures, passes
        character(:), allocatable :: uri, text

        call lsp_parse_did_change('{"params":{"textDocument":{"uri":' // &
            '"file:///test%20space.f90"},"contentChanges":[' // &
            '{"text":"superseded"},{"text":"\u03b1\ud83d\ude00"}]}}', uri, text)
        call expect_true(text == achar(206)//achar(177)//achar(240)// &
            achar(159)//achar(152)//achar(128), &
            'full-sync applies latest edit and decodes Unicode/surrogate pair', &
            failures, passes)
    end subroutine test_unicode_and_multiple_changes

    subroutine test_parse_did_change_basic(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: payload

        payload = '{' // &
            '"jsonrpc":"2.0",' // &
            '"method":"textDocument/didChange",' // &
            '"params":{' // &
            '"textDocument":{"uri":"file:///tmp/change.f90"},' // &
            '"contentChanges":[{"text":"program test\nend program"}]}}'

        call lsp_parse_did_change(payload, uri, text)

        call expect_true(uri == 'file:///tmp/change.f90', &
            'didChange parser extracts uri', failures, passes)
        call expect_true(text == 'program test' // achar(10) // 'end program', &
            'didChange parser extracts full buffer text', failures, passes)
    end subroutine test_parse_did_change_basic

    subroutine test_parse_did_change_with_newlines(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: payload

        payload = '{' // &
            '"method":"textDocument/didChange",' // &
            '"params":{' // &
            '"textDocument":{"uri":"file:///tmp/test.f90"},' // &
            '"contentChanges":[{"text":"line 1\nline 2\nline 3"}]}}'

        call lsp_parse_did_change(payload, uri, text)

        call expect_true(uri == 'file:///tmp/test.f90', &
            'didChange parser handles multi-line buffer', failures, passes)
        call expect_true(text == 'line 1' // achar(10) // 'line 2' // achar(10) // 'line 3', &
            'didChange parser preserves newlines in text', failures, passes)
    end subroutine test_parse_did_change_with_newlines

    subroutine test_parse_did_change_with_escaped_quotes(failures, passes)
        integer, intent(inout) :: failures
        integer, intent(inout) :: passes
        character(len=:), allocatable :: uri
        character(len=:), allocatable :: text
        character(len=:), allocatable :: payload

        payload = '{' // &
            '"method":"textDocument/didChange",' // &
            '"params":{' // &
            '"textDocument":{"uri":"file:///tmp/quoted.f90"},' // &
            '"contentChanges":[{"text":"print *, ''hello''"}]}}'

        call lsp_parse_did_change(payload, uri, text)

        call expect_true(uri == 'file:///tmp/quoted.f90', &
            'didChange parser extracts uri with escaped quotes', failures, passes)
        call expect_true(text == 'print *, ''hello''', &
            'didChange parser extracts quoted content', failures, passes)
    end subroutine test_parse_did_change_with_escaped_quotes

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
            write (*, '(a)') 'FAIL: ' // trim(message)
        end if
    end subroutine expect_true

end program test_lsp_did_change
