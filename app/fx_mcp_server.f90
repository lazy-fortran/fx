program fx_mcp_server
    use fx_mcp, only: mcp_server_t, mcp_server_init, mcp_server_add_action, &
                      mcp_server_run
    implicit none

    type(mcp_server_t) :: server

    call mcp_server_init(server, 'fx', 'Fortran MCP server')
    call mcp_server_add_action(server, 'status', 'Return a status payload')
    call mcp_server_run(server, mcp_server_handler)

contains

    subroutine mcp_server_handler(action, params, response, is_error)
        character(len=*), intent(in) :: action
        character(len=*), intent(in) :: params
        character(len=:), allocatable, intent(out) :: response
        logical, intent(out) :: is_error

        is_error = .false.
        select case (trim(action))
        case ('status')
            response = '{"status":"ready"}'
        case default
            response = '{"error":"unknown action: '//trim(action)//'"}'
            is_error = .true.
        end select
    end subroutine mcp_server_handler

end program fx_mcp_server
