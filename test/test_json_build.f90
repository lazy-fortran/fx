program test_json_build
    use fx_test, only: test_suite_t, test_suite_init, &
        test_suite_summary, test_suite_exit, &
        test_assert, test_assert_equal_str, &
        test_assert_equal_int
    use fx_json_build, only: json_builder_t, json_new, json_object_start, &
        json_object_end, json_array_start, json_array_end, &
        json_key, json_value_string, json_value_int, &
        json_value_bool, json_value_null, &
        json_key_string, json_key_int, json_key_bool, &
        json_to_string, json_reset, json_escape_string
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_json_build')
    call test_json_empty_object(suite)
    call test_json_nested_object(suite)
    call test_json_array(suite)
    call test_json_escape_special_chars(suite)
    call test_json_key_value_shortcuts(suite)
    call test_json_large_output(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_json_empty_object(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_builder_t) :: jb

        jb = json_new()
        call json_object_start(jb)
        call json_object_end(jb)
        call test_assert_equal_str(suite, '{}', json_to_string(jb), &
            'empty object')

        call json_reset(jb)
        call json_array_start(jb)
        call json_array_end(jb)
        call test_assert_equal_str(suite, '[]', json_to_string(jb), &
            'empty array')
    end subroutine test_json_empty_object

    subroutine test_json_nested_object(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_builder_t) :: jb
        character(len=:), allocatable :: result

        jb = json_new()
        call json_object_start(jb)
        call json_key(jb, 'name')
        call json_value_string(jb, 'Alice')
        call json_key(jb, 'age')
        call json_value_int(jb, 30)
        call json_object_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '{"name":"Alice","age":30}', result, &
            'object with string and int')

        call json_reset(jb)
        call json_object_start(jb)
        call json_key(jb, 'inner')
        call json_object_start(jb)
        call json_key(jb, 'x')
        call json_value_int(jb, 1)
        call json_object_end(jb)
        call json_object_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '{"inner":{"x":1}}', result, &
            'nested object')

        ! Two keys — comma between them
        call json_reset(jb)
        call json_object_start(jb)
        call json_key(jb, 'a')
        call json_value_int(jb, 1)
        call json_key(jb, 'b')
        call json_value_int(jb, 2)
        call json_object_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '{"a":1,"b":2}', result, &
            'two keys comma separated')
    end subroutine test_json_nested_object

    subroutine test_json_array(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_builder_t) :: jb
        character(len=:), allocatable :: result

        jb = json_new()
        call json_array_start(jb)
        call json_value_int(jb, 1)
        call json_value_int(jb, 2)
        call json_value_int(jb, 3)
        call json_array_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '[1,2,3]', result, &
            'int array')

        call json_reset(jb)
        call json_array_start(jb)
        call json_value_string(jb, 'a')
        call json_value_string(jb, 'b')
        call json_array_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '["a","b"]', result, &
            'string array')

        call json_reset(jb)
        call json_object_start(jb)
        call json_key(jb, 'items')
        call json_array_start(jb)
        call json_value_int(jb, 10)
        call json_value_int(jb, 20)
        call json_array_end(jb)
        call json_object_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '{"items":[10,20]}', result, &
            'object with array')

        ! bool and null in array
        call json_reset(jb)
        call json_array_start(jb)
        call json_value_bool(jb, .true.)
        call json_value_bool(jb, .false.)
        call json_value_null(jb)
        call json_array_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, '[true,false,null]', result, &
            'bool and null array')
    end subroutine test_json_array

    subroutine test_json_escape_special_chars(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, '\"', json_escape_string('"'), &
            'escape: quote')
        call test_assert_equal_str(suite, '\\', json_escape_string('\'), &
            'escape: backslash')
        call test_assert_equal_str(suite, '\n', json_escape_string(achar(10)), &
            'escape: newline')
        call test_assert_equal_str(suite, '\r', json_escape_string(achar(13)), &
            'escape: carriage return')
        call test_assert_equal_str(suite, '\t', json_escape_string(achar(9)), &
            'escape: tab')
        call test_assert_equal_str(suite, 'hello', json_escape_string('hello'), &
            'escape: plain text unchanged')
        call test_assert_equal_str(suite, '', json_escape_string(''), &
            'escape: empty unchanged')

        ! Verify escaping in value context
        block
            type(json_builder_t) :: jb
            jb = json_new()
            call json_object_start(jb)
            call json_key(jb, 'msg')
            call json_value_string(jb, 'say "hi"')
            call json_object_end(jb)
            call test_assert_equal_str(suite, '{"msg":"say \"hi\""}', &
                json_to_string(jb), &
                'escape in value context')
        end block
    end subroutine test_json_escape_special_chars

    subroutine test_json_key_value_shortcuts(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_builder_t) :: jb
        character(len=:), allocatable :: result

        jb = json_new()
        call json_object_start(jb)
        call json_key_string(jb, 'name', 'Bob')
        call json_key_int(jb, 'count', 42)
        call json_key_bool(jb, 'active', .true.)
        call json_object_end(jb)
        result = json_to_string(jb)
        call test_assert_equal_str(suite, &
            '{"name":"Bob","count":42,"active":true}', result, &
            'key_* shortcuts')
    end subroutine test_json_key_value_shortcuts

    subroutine test_json_large_output(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_builder_t) :: jb
        character(len=:), allocatable :: result
        integer :: i

        jb = json_new()
        call json_object_start(jb)
        do i = 1, 10000
            call json_key_int(jb, 'k', i)
        end do
        call json_object_end(jb)
        result = json_to_string(jb)

        call test_assert(suite, len(result) > 60000, 'large: output length')
        call test_assert(suite, result(1:1) == '{', 'large: starts with {')
        call test_assert(suite, result(len(result):len(result)) == '}', &
            'large: ends with }')
    end subroutine test_json_large_output

end program test_json_build
