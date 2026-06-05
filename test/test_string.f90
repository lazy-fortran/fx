program test_string
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit, &
                       test_assert, test_assert_equal_int, &
                       test_assert_equal_str
    use fx_string, only: str, builder_new, builder_append, builder_to_string, &
                         builder_reset, to_lower, to_upper, split, join, &
                         starts_with, ends_with, contains_str, replace_str, &
                         find_str, strip, repeat_str, utf8_len
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_string')
    call test_str_constructor(suite)
    call test_builder_append(suite)
    call test_builder_geometric_growth(suite)
    call test_to_lower_upper(suite)
    call test_split_join(suite)
    call test_starts_ends_with(suite)
    call test_contains_find(suite)
    call test_replace(suite)
    call test_strip(suite)
    call test_utf8_len(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_str_constructor(suite)
        use fx_string, only: string_t
        type(test_suite_t), intent(inout) :: suite
        type(string_t) :: s

        s = str('hello')
        call test_assert(suite, allocated(s%s), 'str: allocated')
        call test_assert_equal_str(suite, 'hello', s%s, 'str: content')

        s = str('')
        call test_assert(suite, allocated(s%s), 'str: empty allocated')
        call test_assert_equal_int(suite, 0, len(s%s), 'str: empty len')
    end subroutine test_str_constructor

    subroutine test_builder_append(suite)
        use fx_string, only: builder_t
        type(test_suite_t), intent(inout) :: suite
        type(builder_t) :: b

        b = builder_new(16)
        call test_assert_equal_int(suite, 0, b%len, 'builder_new: len zero')

        call builder_append(b, 'foo')
        call test_assert_equal_str(suite, 'foo', builder_to_string(b), &
                                   'append: first chunk')

        call builder_append(b, 'bar')
        call test_assert_equal_str(suite, 'foobar', builder_to_string(b), &
                                   'append: second chunk')

        call builder_append(b, '')
        call test_assert_equal_str(suite, 'foobar', builder_to_string(b), &
                                   'append: empty no-op')

        call builder_reset(b)
        call test_assert_equal_int(suite, 0, b%len, 'builder_reset: len zero')
        call test_assert_equal_str(suite, '', builder_to_string(b), &
                                   'builder_reset: empty string')
    end subroutine test_builder_append

    subroutine test_builder_geometric_growth(suite)
        use fx_string, only: builder_t
        type(test_suite_t), intent(inout) :: suite
        type(builder_t) :: b
        character(len=:), allocatable :: result
        integer :: i

        b = builder_new(4)
        do i = 1, 100
            call builder_append(b, 'x')
        end do
        call test_assert_equal_int(suite, 100, b%len, 'growth: len 100')
        result = builder_to_string(b)
        call test_assert_equal_int(suite, 100, len(result), 'growth: result len')
        call test_assert(suite, result == repeat('x', 100), 'growth: content')
        call test_assert(suite, b%cap >= 100, 'growth: cap sufficient')
    end subroutine test_builder_geometric_growth

    subroutine test_to_lower_upper(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'hello', to_lower('HELLO'), &
                                   'to_lower: all caps')
        call test_assert_equal_str(suite, 'hello world', to_lower('Hello World'), &
                                   'to_lower: mixed')
        call test_assert_equal_str(suite, 'hello', to_lower('hello'), &
                                   'to_lower: already lower')
        call test_assert_equal_str(suite, '', to_lower(''), 'to_lower: empty')

        call test_assert_equal_str(suite, 'HELLO', to_upper('hello'), &
                                   'to_upper: all lower')
        call test_assert_equal_str(suite, 'HELLO WORLD', to_upper('Hello World'), &
                                   'to_upper: mixed')
        call test_assert_equal_str(suite, 'HELLO', to_upper('HELLO'), &
                                   'to_upper: already upper')
        call test_assert_equal_str(suite, '', to_upper(''), 'to_upper: empty')
    end subroutine test_to_lower_upper

    subroutine test_split_join(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=256) :: parts(16)
        integer :: n

        call split('a,b,c', ',', parts, n)
        call test_assert_equal_int(suite, 3, n, 'split: count')
        call test_assert_equal_str(suite, 'a', trim(parts(1)), 'split: part 1')
        call test_assert_equal_str(suite, 'b', trim(parts(2)), 'split: part 2')
        call test_assert_equal_str(suite, 'c', trim(parts(3)), 'split: part 3')

        call split('hello', ',', parts, n)
        call test_assert_equal_int(suite, 1, n, 'split: no delimiter')
        call test_assert_equal_str(suite, 'hello', trim(parts(1)), &
                                   'split: no delimiter content')

        call split('a,,b', ',', parts, n)
        call test_assert_equal_int(suite, 3, n, 'split: consecutive delimiters')
        call test_assert_equal_str(suite, '', trim(parts(2)), &
                                   'split: empty part between delimiters')

        call split('', ',', parts, n)
        call test_assert_equal_int(suite, 1, n, 'split: empty string')

        parts(1) = 'x'
        parts(2) = 'y'
        parts(3) = 'z'
        call test_assert_equal_str(suite, '', join(parts, 0, ':'), &
                                   'join: zero parts')
        call test_assert_equal_str(suite, 'x:y:z', join(parts, 3, ':'), &
                                   'join: three parts')
        call test_assert_equal_str(suite, 'x', join(parts, 1, ':'), &
                                   'join: one part')
    end subroutine test_split_join

    subroutine test_starts_ends_with(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert(suite, starts_with('hello world', 'hello'), &
                         'starts_with: match')
        call test_assert(suite, .not. starts_with('hello world', 'world'), &
                         'starts_with: no match')
        call test_assert(suite, starts_with('hello', ''), &
                         'starts_with: empty prefix')
        call test_assert(suite, starts_with('', ''), &
                         'starts_with: both empty')
        call test_assert(suite, .not. starts_with('hi', 'hello'), &
                         'starts_with: prefix longer than string')

        call test_assert(suite, ends_with('hello world', 'world'), &
                         'ends_with: match')
        call test_assert(suite, .not. ends_with('hello world', 'hello'), &
                         'ends_with: no match')
        call test_assert(suite, ends_with('hello', ''), &
                         'ends_with: empty suffix')
        call test_assert(suite, .not. ends_with('hi', 'hello'), &
                         'ends_with: suffix longer than string')
    end subroutine test_starts_ends_with

    subroutine test_contains_find(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert(suite, contains_str('hello world', 'world'), &
                         'contains_str: found')
        call test_assert(suite, .not. contains_str('hello world', 'xyz'), &
                         'contains_str: not found')
        call test_assert(suite, contains_str('hello', ''), &
                         'contains_str: empty substr')

        call test_assert_equal_int(suite, 7, find_str('hello world', 'world', 1), &
                                   'find_str: found from start')
        call test_assert_equal_int(suite, 0, find_str('hello world', 'xyz', 1), &
                                   'find_str: not found')
        call test_assert_equal_int(suite, 0, find_str('hello', 'world', 6), &
                                   'find_str: start beyond end')
        call test_assert_equal_int(suite, 3, find_str('abcabc', 'c', 3), &
                                   'find_str: from mid offset')
        call test_assert_equal_int(suite, 6, find_str('abcabc', 'c', 4), &
                                   'find_str: second occurrence')
    end subroutine test_contains_find

    subroutine test_replace(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'hXXlo', &
                                   replace_str('hello', 'el', 'XX'), &
                                   'replace_str: basic')
        call test_assert_equal_str(suite, 'XbXbXb', &
                                   replace_str('ababab', 'a', 'X'), &
                                   'replace_str: multiple')
        call test_assert_equal_str(suite, 'hello', &
                                   replace_str('hello', 'xyz', 'ABC'), &
                                   'replace_str: no match')
        call test_assert_equal_str(suite, 'hello', &
                                   replace_str('hello', '', 'X'), &
                                   'replace_str: empty old unchanged')
        call test_assert_equal_str(suite, '', &
                                   replace_str('aaa', 'a', ''), &
                                   'replace_str: delete all')
    end subroutine test_replace

    subroutine test_strip(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'hello', strip('  hello  '), &
                                   'strip: spaces both sides')
        call test_assert_equal_str(suite, 'hello', strip('hello'), &
                                   'strip: no whitespace')
        call test_assert_equal_str(suite, '', strip('   '), &
                                   'strip: all spaces')
        call test_assert_equal_str(suite, '', strip(''), &
                                   'strip: empty')
        call test_assert_equal_str(suite, 'a b', strip('  a b  '), &
                                   'strip: internal space preserved')
    end subroutine test_strip

    subroutine test_utf8_len(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_int(suite, 5, utf8_len('hello'), &
                                   'utf8_len: ascii')
        call test_assert_equal_int(suite, 0, utf8_len(''), &
                                   'utf8_len: empty')
        ! 2-byte codepoint: U+00E9 (é) = 0xC3 0xA9
        call test_assert_equal_int(suite, 1, &
                                   utf8_len(achar(195) // achar(169)), &
                                   'utf8_len: 2-byte codepoint')
        ! 3-byte codepoint: U+20AC (€) = 0xE2 0x82 0xAC
        call test_assert_equal_int(suite, 1, &
                                   utf8_len(achar(226) // achar(130) // achar(172)), &
                                   'utf8_len: 3-byte codepoint')
    end subroutine test_utf8_len

end program test_string
