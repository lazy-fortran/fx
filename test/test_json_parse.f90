program test_json_parse
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_test, only: test_suite_t, test_suite_init, &
        test_suite_summary, test_suite_exit, &
        test_assert, test_assert_equal_str, &
        test_assert_equal_int
    use fx_json_parse, only: json_parser_t, json_event_t, json_parser_init, &
        json_parser_init_strict, &
        json_parser_next, json_parser_reset, &
        json_extract_string, json_extract_int, &
        json_extract_bool, &
        JSON_OBJECT_START, JSON_OBJECT_END, &
        JSON_ARRAY_START, JSON_ARRAY_END, &
        JSON_KEY, JSON_STRING, JSON_INTEGER, JSON_REAL, &
        JSON_BOOL, JSON_NULL_VAL, JSON_ERROR, &
        JSON_END_OF_INPUT, JSON_ERR_SYNTAX, JSON_ERR_DEPTH, JSON_ERR_STRING, &
        JSON_ERR_NUMBER, JSON_ERR_TRAILING, JSON_MAX_DEPTH
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_json_parse')
    call test_parse_empty_object(suite)
    call test_parse_nested(suite)
    call test_parse_array(suite)
    call test_parse_escaped_strings(suite)
    call test_parse_numbers(suite)
    call test_parse_booleans_null(suite)
    call test_extract_string_path(suite)
    call test_extract_int_path(suite)
    call test_parse_malformed(suite)
    call test_strict_typed_values(suite)
    call test_strict_rejects_malformed(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_parse_empty_object(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        call json_parser_init(p, '{}')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_OBJECT_START, ev%event_type, &
            'empty object: start')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_OBJECT_END, ev%event_type, &
            'empty object: end')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_END_OF_INPUT, ev%event_type, &
            'empty object: eof')

        call json_parser_init(p, '[]')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_ARRAY_START, ev%event_type, &
            'empty array: start')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_ARRAY_END, ev%event_type, &
            'empty array: end')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_END_OF_INPUT, ev%event_type, &
            'empty array: eof')

        ! reset then re-use
        call json_parser_reset(p)
        call test_assert_equal_int(suite, 0, p%depth, 'reset: depth zero')
        call test_assert_equal_int(suite, 1, p%pos, 'reset: pos one')
    end subroutine test_parse_empty_object

    subroutine test_parse_nested(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        ! {"name":"Alice","age":30}
        call json_parser_init(p, '{"name":"Alice","age":30}')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_OBJECT_START, ev%event_type, &
            'nested: start')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'nested: key name type')
        call test_assert_equal_str(suite, 'name', ev%string_val, &
            'nested: key name val')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_STRING, ev%event_type, &
            'nested: string val type')
        call test_assert_equal_str(suite, 'Alice', ev%string_val, &
            'nested: string val')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'nested: key age type')
        call test_assert_equal_str(suite, 'age', ev%string_val, &
            'nested: key age val')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_INTEGER, ev%event_type, &
            'nested: int type')
        call test_assert_equal_int(suite, 30, ev%int_val, 'nested: int val')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_OBJECT_END, ev%event_type, &
            'nested: end')

        ! nested object: {"outer":{"inner":1}}
        call json_parser_init(p, '{"outer":{"inner":1}}')
        call json_parser_next(p, ev) ! outer start
        call json_parser_next(p, ev) ! key "outer"
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'nested obj: outer key')
        call json_parser_next(p, ev) ! inner start
        call test_assert_equal_int(suite, JSON_OBJECT_START, ev%event_type, &
            'nested obj: inner start')
        call json_parser_next(p, ev) ! key "inner"
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'nested obj: inner key')
        call json_parser_next(p, ev) ! value 1
        call test_assert_equal_int(suite, JSON_INTEGER, ev%event_type, &
            'nested obj: inner val type')
        call test_assert_equal_int(suite, 1, ev%int_val, 'nested obj: inner val')
        call json_parser_next(p, ev) ! inner end
        call test_assert_equal_int(suite, JSON_OBJECT_END, ev%event_type, &
            'nested obj: inner end')
        call json_parser_next(p, ev) ! outer end
        call test_assert_equal_int(suite, JSON_OBJECT_END, ev%event_type, &
            'nested obj: outer end')
    end subroutine test_parse_nested

    subroutine test_parse_array(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        ! [1,2,3]
        call json_parser_init(p, '[1,2,3]')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_ARRAY_START, ev%event_type, &
            'array: start')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_INTEGER, ev%event_type, &
            'array: elem1 type')
        call test_assert_equal_int(suite, 1, ev%int_val, 'array: elem1')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, 2, ev%int_val, 'array: elem2')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, 3, ev%int_val, 'array: elem3')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_ARRAY_END, ev%event_type, &
            'array: end')

        ! ["a","b"]
        call json_parser_init(p, '["x","y"]')
        call json_parser_next(p, ev) ! array start
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, JSON_STRING, ev%event_type, &
            'str array: type')
        call test_assert_equal_str(suite, 'x', ev%string_val, 'str array: x')
        call json_parser_next(p, ev)
        call test_assert_equal_str(suite, 'y', ev%string_val, 'str array: y')

        ! array in object: {"items":[10,20]}
        call json_parser_init(p, '{"items":[10,20]}')
        call json_parser_next(p, ev) ! OBJECT_START
        call json_parser_next(p, ev) ! KEY "items"
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'arr in obj: key type')
        call json_parser_next(p, ev) ! ARRAY_START
        call test_assert_equal_int(suite, JSON_ARRAY_START, ev%event_type, &
            'arr in obj: array start')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, 10, ev%int_val, 'arr in obj: 10')
        call json_parser_next(p, ev)
        call test_assert_equal_int(suite, 20, ev%int_val, 'arr in obj: 20')
        call json_parser_next(p, ev) ! ARRAY_END
        call test_assert_equal_int(suite, JSON_ARRAY_END, ev%event_type, &
            'arr in obj: array end')
        call json_parser_next(p, ev) ! OBJECT_END
        call test_assert_equal_int(suite, JSON_OBJECT_END, ev%event_type, &
            'arr in obj: obj end')
    end subroutine test_parse_array

    subroutine test_parse_escaped_strings(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        ! {"q":"say \"hi\""} → q = say "hi"
        call json_parser_init(p, '{"q":"say \"hi\""}')
        call json_parser_next(p, ev) ! OBJECT_START
        call json_parser_next(p, ev) ! KEY
        call json_parser_next(p, ev) ! STRING
        call test_assert_equal_int(suite, JSON_STRING, ev%event_type, &
            'escape: type')
        call test_assert_equal_str(suite, 'say "hi"', ev%string_val, &
            'escape: quote')

        ! tab, newline, backslash
        call json_parser_init(p, '["' // achar(92) // 't' // achar(92) // &
            'n' // achar(92) // achar(92) // '"]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! STRING
        call test_assert(suite, ev%event_type == JSON_STRING, &
            'escape sequences: type')
        call test_assert(suite, len(ev%string_val) == 3, &
            'escape sequences: length 3')
        call test_assert(suite, iachar(ev%string_val(1:1)) == 9, &
            'escape: tab char')
        call test_assert(suite, iachar(ev%string_val(2:2)) == 10, &
            'escape: newline char')
        call test_assert(suite, iachar(ev%string_val(3:3)) == 92, &
            'escape: backslash char')

        ! A → 'A'
        call json_parser_init(p, '["A"]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! STRING
        call test_assert_equal_str(suite, 'A', ev%string_val, &
            'escape: \\u0041 is A')
    end subroutine test_parse_escaped_strings

    subroutine test_parse_numbers(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        call json_parser_init(p, '{"i":42,"neg":-5}')
        call json_parser_next(p, ev) ! OBJECT_START
        call json_parser_next(p, ev) ! KEY i
        call json_parser_next(p, ev) ! INTEGER 42
        call test_assert_equal_int(suite, JSON_INTEGER, ev%event_type, &
            'numbers: int type')
        call test_assert_equal_int(suite, 42, ev%int_val, 'numbers: 42')
        call json_parser_next(p, ev) ! KEY neg
        call json_parser_next(p, ev) ! INTEGER -5
        call test_assert_equal_int(suite, -5, ev%int_val, 'numbers: -5')

        ! floating point
        call json_parser_init(p, '[3.14]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! REAL
        call test_assert_equal_int(suite, JSON_REAL, ev%event_type, &
            'numbers: real type')
        call test_assert(suite, abs(ev%real_val - 3.14d0) < 1.0d-10, &
            'numbers: 3.14')

        ! scientific notation
        call json_parser_init(p, '[1.5e2]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! REAL
        call test_assert_equal_int(suite, JSON_REAL, ev%event_type, &
            'numbers: sci type')
        call test_assert(suite, abs(ev%real_val - 150.0d0) < 1.0d-10, &
            'numbers: 1.5e2=150')

        ! zero
        call json_parser_init(p, '[0]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! INTEGER 0
        call test_assert_equal_int(suite, JSON_INTEGER, ev%event_type, &
            'numbers: zero type')
        call test_assert_equal_int(suite, 0, ev%int_val, 'numbers: zero')
    end subroutine test_parse_numbers

    subroutine test_parse_booleans_null(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev

        call json_parser_init(p, '[true,false,null]')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! true
        call test_assert_equal_int(suite, JSON_BOOL, ev%event_type, &
            'bool: true type')
        call test_assert(suite, ev%bool_val, 'bool: true val')
        call json_parser_next(p, ev) ! false
        call test_assert_equal_int(suite, JSON_BOOL, ev%event_type, &
            'bool: false type')
        call test_assert(suite, .not. ev%bool_val, 'bool: false val')
        call json_parser_next(p, ev) ! null
        call test_assert_equal_int(suite, JSON_NULL_VAL, ev%event_type, &
            'bool: null type')

        ! booleans as object values
        call json_parser_init(p, '{"a":true,"b":false}')
        call json_parser_next(p, ev) ! OBJECT_START
        call json_parser_next(p, ev) ! KEY a
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'bool obj: key a type')
        call json_parser_next(p, ev) ! BOOL true
        call test_assert(suite, ev%bool_val, 'bool obj: a=true')
        call json_parser_next(p, ev) ! KEY b
        call test_assert_equal_int(suite, JSON_KEY, ev%event_type, &
            'bool obj: key b type')
        call json_parser_next(p, ev) ! BOOL false
        call test_assert(suite, .not. ev%bool_val, 'bool obj: b=false')
    end subroutine test_parse_booleans_null

    subroutine test_extract_string_path(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: result
        logical :: found

        ! simple top-level key
        call json_extract_string('{"name":"Bob"}', 'name', result, found)
        call test_assert(suite, found, 'extract_str: found simple')
        call test_assert_equal_str(suite, 'Bob', result, 'extract_str: val simple')

        ! nested key
        call json_extract_string('{"a":{"b":"deep"}}', 'a.b', result, found)
        call test_assert(suite, found, 'extract_str: found nested')
        call test_assert_equal_str(suite, 'deep', result, 'extract_str: val nested')

        ! missing key
        call json_extract_string('{"x":1}', 'y', result, found)
        call test_assert(suite, .not. found, 'extract_str: not found')

        ! second key in object (checks that expect_key resets for non-string vals)
        call json_extract_string('{"count":5,"label":"ok"}', 'label', result, found)
        call test_assert(suite, found, 'extract_str: after int key found')
        call test_assert_equal_str(suite, 'ok', result, 'extract_str: after int key val')

        ! string in array (1-based index)
        call json_extract_string('{"tags":["alpha","beta","gamma"]}', &
            'tags[2]', result, found)
        call test_assert(suite, found, 'extract_str: array elem found')
        call test_assert_equal_str(suite, 'beta', result, 'extract_str: array elem val')
    end subroutine test_extract_string_path

    subroutine test_extract_int_path(suite)
        type(test_suite_t), intent(inout) :: suite
        integer :: result
        logical :: found

        call json_extract_int('{"count":7}', 'count', result, found)
        call test_assert(suite, found, 'extract_int: found')
        call test_assert_equal_int(suite, 7, result, 'extract_int: val')

        ! missing → not found
        call json_extract_int('{"x":"hello"}', 'x', result, found)
        call test_assert(suite, .not. found, 'extract_int: type mismatch not found')

        ! nested
        call json_extract_int('{"a":{"b":{"c":99}}}', 'a.b.c', result, found)
        call test_assert(suite, found, 'extract_int: deep nested found')
        call test_assert_equal_int(suite, 99, result, 'extract_int: deep nested val')

        ! array element (1-based)
        call json_extract_int('{"vals":[10,20,30]}', 'vals[3]', result, found)
        call test_assert(suite, found, 'extract_int: array 3rd found')
        call test_assert_equal_int(suite, 30, result, 'extract_int: array 3rd val')

        call json_extract_int('{"vals":[10,20,30]}', 'vals[1]', result, found)
        call test_assert(suite, found, 'extract_int: array 1st found')
        call test_assert_equal_int(suite, 10, result, 'extract_int: array 1st val')
    end subroutine test_extract_int_path

    subroutine test_parse_malformed(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        logical :: starts_ok
        integer :: i
        character(len=:), allocatable :: deep_json

        ! Invalid token at top level
        call json_parser_init(p, 'invalid')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: invalid token')

        ! Unterminated string
        call json_parser_init(p, '["unterminated')
        call json_parser_next(p, ev) ! ARRAY_START
        call json_parser_next(p, ev) ! ERROR (string not closed)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: unterminated string')

        ! Strict entry points reject missing documents and stay failed.
        call json_parser_init(p, '')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR .and. &
            ev%error_code == JSON_ERR_SYNTAX, 'malformed: empty document')
        call test_assert_equal_int(suite, 1, ev%error_offset, &
            'malformed: empty input offset')
        call test_assert_equal_int(suite, 0, p%depth, &
            'malformed: empty depth remains zero')

        ! Unmatched closers fail without underflowing or resuming at later text.
        call json_parser_init(p, '}{')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: unmatched object closer')
        call test_assert_equal_int(suite, 0, p%depth, &
            'malformed: unmatched closer depth remains zero')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: parser error remains sticky')

        call json_parser_init(p, ']')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: unmatched array closer')
        call test_assert_equal_int(suite, 0, p%depth, &
            'malformed: unmatched array depth remains zero')

        ! The strict parser accepts JSON_MAX_DEPTH and rejects one more level.
        deep_json = repeat('[', JSON_MAX_DEPTH + 1) // '0' // &
            repeat(']', JSON_MAX_DEPTH + 1)
        call json_parser_init(p, deep_json)
        starts_ok = .true.
        do i = 1, JSON_MAX_DEPTH
            call json_parser_next(p, ev)
            if (ev%event_type /= JSON_ARRAY_START) then
                starts_ok = .false.
                exit
            end if
        end do
        call test_assert(suite, starts_ok, 'malformed: maximum nesting accepted')
        if (starts_ok) then
            call json_parser_next(p, ev)
            call test_assert(suite, ev%event_type == JSON_ERROR .and. &
                ev%error_code == JSON_ERR_DEPTH, &
                'malformed: excess nesting rejected')
            call test_assert_equal_int(suite, JSON_MAX_DEPTH, p%depth, &
                'malformed: excess nesting preserves maximum depth')
        end if

        ! Tokens ending at end-of-input stay valid without a trailing delimiter.
        call json_parser_init(p, 'true')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_BOOL .and. ev%bool_val, &
            'boundary: true at end of input')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_END_OF_INPUT, &
            'boundary: true reaches end of input')

        call json_parser_init(p, 'false')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_BOOL .and. &
            .not. ev%bool_val, 'boundary: false at end of input')

        call json_parser_init(p, 'null')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_NULL_VAL, &
            'boundary: null at end of input')

        call json_parser_init(p, '42')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_INTEGER .and. &
            ev%int_val == 42, 'boundary: integer at end of input')

        ! Whitespace only is an empty document in strict mode.
        call json_parser_init(p, '   ')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ERROR .and. &
            ev%error_code == JSON_ERR_SYNTAX, 'malformed: whitespace only')
        call test_assert_equal_int(suite, 4, ev%error_offset, &
            'malformed: whitespace-only error offset')

        ! Object with invalid interior
        call json_parser_init(p, '{xyz}')
        call json_parser_next(p, ev) ! OBJECT_START
        call json_parser_next(p, ev) ! ERROR (x is not valid key start)
        call test_assert(suite, ev%event_type == JSON_ERROR, &
            'malformed: invalid object key')
    end subroutine test_parse_malformed

    subroutine test_strict_typed_values(suite)
        type(test_suite_t), intent(inout) :: suite
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        character(len=:), allocatable :: decoded

        decoded = 'A' // achar(206) // achar(187) // achar(240) // &
            achar(159) // achar(152) // achar(128)
        call json_parser_init_strict(p, &
            '{"unicode":"A\u03BB\uD83D\uDE00","bounds":[' // &
            '-9223372036854775808,9223372036854775807,' // &
            '9223372036854775808,-9223372036854775809,1.25e+2]}')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_OBJECT_START, &
            'strict: object start')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_KEY, &
            'strict: first key event type')
        call assert_allocated_string(suite, ev%string_val, 'unicode', &
            'strict: decoded first key')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_STRING, &
            'strict: Unicode string type')
        call assert_allocated_string(suite, ev%string_val, decoded, &
            'strict: BMP and surrogate-pair UTF-8 bytes')
        call assert_allocated_string(suite, ev%raw_val, &
            '"A\u03BB\uD83D\uDE00"', 'strict: raw string token is preserved')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_KEY, &
            'strict: second key event type')
        call assert_allocated_string(suite, ev%string_val, 'bounds', &
            'strict: decoded second key')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ARRAY_START, &
            'strict: integer array start')

        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_INTEGER .and. &
            ev%int64_valid, 'strict: signed int64 minimum fits')
        call test_assert(suite, ev%int64_val == -huge(0_int64) - 1_int64, &
            'strict: signed int64 minimum value')
        call assert_allocated_string(suite, ev%raw_val, &
            '-9223372036854775808', 'strict: minimum raw integer')

        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_INTEGER .and. &
            ev%int64_valid, 'strict: signed int64 maximum fits')
        call test_assert(suite, ev%int64_val == huge(0_int64), &
            'strict: signed int64 maximum value')
        call assert_allocated_string(suite, ev%raw_val, &
            '9223372036854775807', 'strict: maximum raw integer')

        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_INTEGER .and. &
            .not. ev%int64_valid, 'strict: out-of-range integer stays JSON')
        call assert_allocated_string(suite, ev%raw_val, '9223372036854775808', &
            'strict: out-of-range integer raw value')

        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_INTEGER .and. &
            .not. ev%int64_valid, 'strict: negative underflow stays JSON')
        call assert_allocated_string(suite, ev%raw_val, &
            '-9223372036854775809', 'strict: negative underflow raw value')

        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_REAL .and. &
            ev%real64_valid .and. abs(ev%real_val - 125.0d0) < 1.0d-12, &
            'strict: exponent number decodes')
        call assert_allocated_string(suite, ev%raw_val, '1.25e+2', &
            'strict: real raw value')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_ARRAY_END, &
            'strict: array end')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_OBJECT_END, &
            'strict: object end')
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_END_OF_INPUT, &
            'strict: complete document ends')
        call test_assert(suite, p%error_code == 0, &
            'strict: complete document has no parser error')

        ! Distinct source spellings that decode to the same name stay visible
        ! to domain consumers, which own duplicate-field rejection.
        call json_parser_init_strict(p, '{"a":1,"\u0061":2}')
        call json_parser_next(p, ev)
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_KEY, &
            'strict: first duplicate key event type')
        call assert_allocated_string(suite, ev%string_val, 'a', &
            'strict: first duplicate key')
        call json_parser_next(p, ev)
        call json_parser_next(p, ev)
        call test_assert(suite, ev%event_type == JSON_KEY, &
            'strict: escaped duplicate key event type')
        call assert_allocated_string(suite, ev%string_val, 'a', &
            'strict: escaped duplicate key decodes')
    end subroutine test_strict_typed_values

    subroutine test_strict_rejects_malformed(suite)
        type(test_suite_t), intent(inout) :: suite

        call expect_strict_error(suite, '{"a" 1}', JSON_ERR_SYNTAX, 6, &
            'strict: missing colon')
        call expect_strict_error(suite, '{"a":1 "b":2}', JSON_ERR_SYNTAX, 8, &
            'strict: missing comma')
        call expect_strict_error(suite, '{"a":1,}', JSON_ERR_SYNTAX, 8, &
            'strict: trailing object comma')
        call expect_strict_error(suite, '[1,]', JSON_ERR_SYNTAX, 4, &
            'strict: trailing array comma')
        call expect_strict_error(suite, '[01]', JSON_ERR_NUMBER, 3, &
            'strict: leading zero')
        call expect_strict_error(suite, '[1.]', JSON_ERR_NUMBER, 4, &
            'strict: fraction needs digits')
        call expect_strict_error(suite, '[1e]', JSON_ERR_NUMBER, 4, &
            'strict: exponent needs digits')
        call expect_strict_error(suite, 'true false', JSON_ERR_TRAILING, 6, &
            'strict: trailing root value')
        call expect_strict_error(suite, '{"a":1}x', JSON_ERR_TRAILING, 8, &
            'strict: trailing root text offset')
        call expect_strict_error(suite, '"' // achar(92) // 'x"', &
            JSON_ERR_STRING, 2, 'strict: invalid escape offset')
        call expect_strict_error(suite, '"' // achar(92) // 'uD800"', &
            JSON_ERR_STRING, 8, 'strict: unpaired high surrogate offset')
        call expect_strict_error(suite, '"' // achar(92) // 'uDC00"', &
            JSON_ERR_STRING, 2, 'strict: unpaired low surrogate offset')
        call expect_strict_error(suite, '["a' // achar(1) // '"]', &
            JSON_ERR_STRING, 4, 'strict: raw control character offset')
        call expect_strict_error(suite, '', JSON_ERR_SYNTAX, 1, &
            'strict: empty input offset')
    end subroutine test_strict_rejects_malformed

    subroutine expect_strict_error(suite, input, expected_code, expected_offset, name)
        type(test_suite_t), intent(inout) :: suite
        character(len=*), intent(in) :: input, name
        integer, intent(in) :: expected_code, expected_offset
        type(json_parser_t) :: p
        type(json_event_t) :: ev
        integer :: i

        call json_parser_init_strict(p, input)
        do i = 1, 128
            call json_parser_next(p, ev)
            if (ev%event_type == JSON_ERROR) then
                call test_assert_equal_int(suite, expected_code, ev%error_code, &
                    name // ': error class')
                call test_assert_equal_int(suite, expected_offset, &
                    ev%error_offset, name // ': error offset')
                return
            end if
            if (ev%event_type == JSON_END_OF_INPUT) exit
        end do
        call test_assert(suite, .false., name // ': malformed input rejected')
    end subroutine expect_strict_error

    subroutine assert_allocated_string(suite, value, expected, name)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable, intent(in) :: value
        character(len=*), intent(in) :: expected, name

        if (.not. allocated(value)) then
            call test_assert(suite, .false., name // ': value is allocated')
            return
        end if
        call test_assert_equal_str(suite, expected, value, name)
    end subroutine assert_allocated_string

end program test_json_parse
