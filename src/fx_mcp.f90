module fx_mcp
    use fx_json_build, only: json_builder_t
    implicit none
    private

    integer, parameter, public :: MCP_FRAME_CONTENT_LENGTH = 1
    integer, parameter, public :: MCP_FRAME_BARE_JSON = 2

    integer, parameter :: MAX_ACTIONS = 32

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

contains

    subroutine mcp_server_init(s, tool_name, tool_description)
        type(mcp_server_t), intent(out) :: s
        character(len=*), intent(in) :: tool_name
        character(len=*), intent(in) :: tool_description
        error stop "fx_mcp:mcp_server_init not implemented"
    end subroutine mcp_server_init

    subroutine mcp_server_add_action(s, action_name, description)
        type(mcp_server_t), intent(inout) :: s
        character(len=*), intent(in) :: action_name
        character(len=*), intent(in) :: description
        error stop "fx_mcp:mcp_server_add_action not implemented"
    end subroutine mcp_server_add_action

    subroutine mcp_server_run(s, handler)
        type(mcp_server_t), intent(inout) :: s
        procedure(mcp_action_handler) :: handler
        error stop "fx_mcp:mcp_server_run not implemented"
    end subroutine mcp_server_run

    subroutine mcp_read_message(line, max_len, framing, eof)
        character(len=*), intent(out) :: line
        integer, intent(in) :: max_len
        integer, intent(inout) :: framing
        logical, intent(out) :: eof
        error stop "fx_mcp:mcp_read_message not implemented"
    end subroutine mcp_read_message

    subroutine mcp_send_response(response, framing)
        character(len=*), intent(in) :: response
        integer, intent(in) :: framing
        error stop "fx_mcp:mcp_send_response not implemented"
    end subroutine mcp_send_response

    subroutine mcp_make_initialize_response(id_str, proto_ver, &
            server_name, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: proto_ver
        character(len=*), intent(in) :: server_name
        character(len=:), allocatable, intent(out) :: response
        error stop "fx_mcp:mcp_make_initialize_response not implemented"
    end subroutine mcp_make_initialize_response

    subroutine mcp_make_tools_list_response(id_str, s, response)
        character(len=*), intent(in) :: id_str
        type(mcp_server_t), intent(in) :: s
        character(len=:), allocatable, intent(out) :: response
        error stop "fx_mcp:mcp_make_tools_list_response not implemented"
    end subroutine mcp_make_tools_list_response

    subroutine mcp_make_tool_text_response(id_str, text, is_error, &
            response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(in) :: text
        logical, intent(in) :: is_error
        character(len=:), allocatable, intent(out) :: response
        error stop "fx_mcp:mcp_make_tool_text_response not implemented"
    end subroutine mcp_make_tool_text_response

    subroutine mcp_make_tool_json_response(id_str, jb, is_error, &
            response)
        character(len=*), intent(in) :: id_str
        type(json_builder_t), intent(in) :: jb
        logical, intent(in) :: is_error
        character(len=:), allocatable, intent(out) :: response
        error stop "fx_mcp:mcp_make_tool_json_response not implemented"
    end subroutine mcp_make_tool_json_response

    subroutine mcp_extract_action(line, action)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: action
        error stop "fx_mcp:mcp_extract_action not implemented"
    end subroutine mcp_extract_action

    subroutine mcp_extract_id(line, id_str)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: id_str
        error stop "fx_mcp:mcp_extract_id not implemented"
    end subroutine mcp_extract_id

    subroutine mcp_extract_param(line, param_name, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(in) :: param_name
        character(len=:), allocatable, intent(out) :: value
        error stop "fx_mcp:mcp_extract_param not implemented"
    end subroutine mcp_extract_param

end module fx_mcp
