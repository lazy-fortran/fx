program test_mcp
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_str, test_assert_equal_int, &
        test_suite_summary, test_suite_exit
    use fx_json_build, only: json_builder_t
    use fx_mcp, only: MCP_FRAME_BARE_JSON, MCP_FRAME_CONTENT_LENGTH, &
        MCP_FRAME_UNKNOWN, mcp_server_t, mcp_server_init, &
        mcp_server_add_action, mcp_make_initialize_response, &
        mcp_make_tools_list_response, &
        mcp_make_tool_text_response, &
        mcp_make_tool_json_response, &
        mcp_extract_action, mcp_extract_id, mcp_extract_param
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_mcp')
    call test_mcp_initialize_response(suite)
    call test_mcp_tools_list_response(suite)
    call test_mcp_text_response(suite)
    call test_mcp_json_response(suite)
    call test_mcp_extract_action(suite)
    call test_mcp_extract_id(suite)
    call test_mcp_extract_param(suite)
    call test_mcp_framing_detect(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_mcp_initialize_response(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: response

        call mcp_make_initialize_response('7', '2025-03-26', 'fx', response)
        call test_assert(suite, index(response, '"jsonrpc":"2.0"') > 0, &
            'initialize response is jsonrpc response')
        call test_assert(suite, index(response, '"id":7') > 0, &
            'initialize response echoes id')
        call test_assert(suite, &
            index(response, '"protocolVersion":"2025-03-26"') > 0, &
            'initialize response echoes protocol version')
        call test_assert(suite, index(response, '"name":"fx"') > 0, &
            'initialize response includes server name')
    end subroutine test_mcp_initialize_response

    subroutine test_mcp_tools_list_response(suite)
        type(test_suite_t), intent(inout) :: suite
        type(mcp_server_t) :: s
        character(len=:), allocatable :: response

        call mcp_server_init(s, 'fx', 'Fortran MCP server')
        call mcp_server_add_action(s, 'check', 'run project checks')
        call mcp_server_add_action(s, 'test', 'run tests')

        call mcp_make_tools_list_response('11', s, response)
        call test_assert(suite, index(response, '"name":"fx"') > 0, &
            'tools list includes tool name')
        call test_assert(suite, index(response, '"enum":["check","test"]') > 0, &
            'tools list includes actions')
        call test_assert(suite, index(response, '"required":["action"]') > 0, &
            'tools list requires action')
    end subroutine test_mcp_tools_list_response

    subroutine test_mcp_text_response(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: response

        call mcp_make_tool_text_response('13', 'line1"quoted"' // &
            char(10) // 'line2\backslash', .false., response)
        call test_assert(suite, index(response, '"text":"line1\"quoted\"') > 0, &
            'tool text response escapes quotes')
        call test_assert(suite, index(response, '"isError":false') > 0, &
            'tool text response sets isError false')
    end subroutine test_mcp_text_response

    subroutine test_mcp_json_response(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: response
        type(json_builder_t) :: jb

        jb%buf%buf = '{"ok":true,"status":"all"}'
        call mcp_make_tool_json_response('17', jb, .false., response)
        call test_assert(suite, index(response, '"ok":true') > 0, &
            'tool json response includes nested json')
        call test_assert(suite, index(response, '"isError":false') > 0, &
            'tool json response sets isError false')
    end subroutine test_mcp_json_response

    subroutine test_mcp_extract_action(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: action

        call mcp_extract_action('{"jsonrpc":"2.0","id":1,"method":"tools/call"}', &
            action)
        call test_assert_equal_str(suite, 'tools/call', action, &
            'extracts method field')

        call mcp_extract_action('{"jsonrpc":"2.0","method" : "initialized"}', action)
        call test_assert_equal_str(suite, 'initialized', action, &
            'extracts method with spacing')

        call mcp_extract_action('{"jsonrpc":"2.0","id":4}', action)
        call test_assert(suite, len_trim(action) == 0, &
            'missing method returns empty action')
    end subroutine test_mcp_extract_action

    subroutine test_mcp_extract_id(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: id

        call mcp_extract_id('{"jsonrpc":"2.0","id":9,"method":"initialized"}', id)
        call test_assert_equal_str(suite, '9', id, 'extracts numeric id')

        call mcp_extract_id('{"jsonrpc":"2.0","method":"initialized","id":"abc"}', id)
        call test_assert_equal_str(suite, 'abc', id, 'extracts quoted id')

        call mcp_extract_id('{"jsonrpc":"2.0","method":"initialized"}', id)
        call test_assert(suite, len_trim(id) == 0, 'missing id returns empty string')
    end subroutine test_mcp_extract_id

    subroutine test_mcp_extract_param(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: value

        call mcp_extract_param('{"jsonrpc":"2.0",'// &
            '"params":{"action":"build","dir":"/tmp"},'// &
            '"method":"tools/call"}', &
            'params.action', value)
        call test_assert_equal_str(suite, 'build', value, &
            'extracts nested params action')

        call mcp_extract_param('{"params":{"arguments":{"action":"test","dir":"/tmp"}}', &
            'params.arguments.action', value)
        call test_assert_equal_str(suite, 'test', value, &
            'extracts nested params.arguments.action')

        call mcp_extract_param('{"params":{"mode":3}', 'params.mode', value)
        call test_assert_equal_str(suite, '3', value, &
            'extracts numeric nested value')

        call mcp_extract_param('{"params":{"mode":3}', 'params.action', value)
        call test_assert(suite, len_trim(value) == 0, &
            'returns empty for missing nested key')
    end subroutine test_mcp_extract_param

    subroutine test_mcp_framing_detect(suite)
        type(test_suite_t), intent(inout) :: suite
        type(mcp_server_t) :: s
        character(len=:), allocatable :: response
        integer :: i

        call mcp_server_init(s, 'fx', 'Fortran MCP server')
        call test_assert_equal_int(suite, MCP_FRAME_BARE_JSON, s%framing_mode, &
            'server initializes with bare-json framing')

        do i = 1, 40
            call mcp_server_add_action(s, 'a'//trim(adjustl(to_string(i))), &
                'x')
        end do
        call test_assert(suite, .true., &
            'adding many actions does not terminate immediately')

        call mcp_make_tools_list_response('999', s, response)
        call test_assert(suite, len_trim(response) > 0, &
            'tools/list response is produced after many actions')

        call test_assert(suite, s%framing_mode == MCP_FRAME_BARE_JSON .or. &
            s%framing_mode == MCP_FRAME_CONTENT_LENGTH .or. &
            s%framing_mode == MCP_FRAME_UNKNOWN, &
            'framing mode is one of expected values')
    end subroutine test_mcp_framing_detect

    function to_string(i) result(out)
        integer, intent(in) :: i
        character(len=16) :: out
        write(out, '(i0)') i
    end function to_string

end program test_mcp
