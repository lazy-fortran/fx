module fx_lsp
    use fx_diag, only: diag_t
    implicit none
    private

    type, public :: lsp_server_t
        character(len=64) :: name = ' '
        integer :: capabilities = 0
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

contains

    subroutine lsp_server_init(s, name)
        type(lsp_server_t), intent(out) :: s
        character(len=*), intent(in) :: name
        error stop "fx_lsp:lsp_server_init not implemented"
    end subroutine lsp_server_init

    subroutine lsp_server_run(s, on_save_callback)
        type(lsp_server_t), intent(inout) :: s
        procedure(lsp_save_callback) :: on_save_callback
        error stop "fx_lsp:lsp_server_run not implemented"
    end subroutine lsp_server_run

    subroutine lsp_read_message(content, content_len, eof)
        character(len=*), intent(out) :: content
        integer, intent(out) :: content_len
        logical, intent(out) :: eof
        error stop "fx_lsp:lsp_read_message not implemented"
    end subroutine lsp_read_message

    subroutine lsp_send_message(content)
        character(len=*), intent(in) :: content
        error stop "fx_lsp:lsp_send_message not implemented"
    end subroutine lsp_send_message

    subroutine lsp_make_initialize_response(id_str, server_name, &
            response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: server_name
        character(len=:), allocatable, intent(out) :: response
        error stop "fx_lsp:lsp_make_initialize_response not implemented"
    end subroutine lsp_make_initialize_response

    subroutine lsp_publish_diagnostics(uri, diags, n_diags)
        character(len=*), intent(in) :: uri
        integer, intent(in) :: n_diags
        type(diag_t), intent(in) :: diags(n_diags)
        error stop "fx_lsp:lsp_publish_diagnostics not implemented"
    end subroutine lsp_publish_diagnostics

    function lsp_make_diagnostic(d) result(res)
        type(diag_t), intent(in) :: d
        character(len=:), allocatable :: res
        error stop "fx_lsp:lsp_make_diagnostic not implemented"
    end function lsp_make_diagnostic

    subroutine lsp_parse_did_save(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        error stop "fx_lsp:lsp_parse_did_save not implemented"
    end subroutine lsp_parse_did_save

    subroutine lsp_parse_did_open(content, uri, text)
        character(len=*), intent(in) :: content
        character(len=:), allocatable, intent(out) :: uri
        character(len=:), allocatable, intent(out) :: text
        error stop "fx_lsp:lsp_parse_did_open not implemented"
    end subroutine lsp_parse_did_open

end module fx_lsp
