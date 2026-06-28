program test_diag
    use fx_test, only: test_suite_t, test_suite_init, &
        test_suite_summary, test_suite_exit, &
        test_assert, test_assert_equal_str, &
        test_assert_equal_int
    use fx_diag, only: diag_t, diag_new, diag_to_string, diag_to_json, &
        diags_to_json, diag_strip_prefix, &
        DIAG_ERROR, DIAG_WARNING, DIAG_INFO, DIAG_HINT
    use fx_json_build, only: json_builder_t, json_new, json_to_string
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_diag')
    call test_diag_new(suite)
    call test_diag_to_string(suite)
    call test_diag_to_json(suite)
    call test_diag_strip_prefix(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_diag_new(suite)
        type(test_suite_t), intent(inout) :: suite
        type(diag_t) :: d

        d = diag_new('foo.f90', 10, 5, DIAG_ERROR, 'undeclared variable')
        call test_assert_equal_str(suite, 'foo.f90', trim(d%file), &
            'new: file')
        call test_assert_equal_int(suite, 10, d%line, 'new: line')
        call test_assert_equal_int(suite, 5, d%col, 'new: col')
        call test_assert_equal_int(suite, DIAG_ERROR, d%severity, &
            'new: severity error')
        call test_assert_equal_str(suite, 'undeclared variable', &
            trim(d%message), 'new: message')

        d = diag_new('bar.f90', 1, 1, DIAG_WARNING, 'unused variable')
        call test_assert_equal_int(suite, DIAG_WARNING, d%severity, &
            'new: severity warning')

        d = diag_new('', 0, 0, DIAG_INFO, 'note')
        call test_assert_equal_int(suite, DIAG_INFO, d%severity, &
            'new: severity info')

        d = diag_new('x.f90', 3, 2, DIAG_HINT, 'suggestion')
        call test_assert_equal_int(suite, DIAG_HINT, d%severity, &
            'new: severity hint')
    end subroutine test_diag_new

    subroutine test_diag_to_string(suite)
        type(test_suite_t), intent(inout) :: suite
        type(diag_t) :: d
        character(len=:), allocatable :: s

        d = diag_new('foo.f90', 10, 5, DIAG_ERROR, 'bad thing')
        s = diag_to_string(d)
        call test_assert_equal_str(suite, 'foo.f90:10:5: error: bad thing', s, &
            'to_string: error format')

        d = diag_new('src/bar.f90', 1, 1, DIAG_WARNING, 'unused')
        s = diag_to_string(d)
        call test_assert_equal_str(suite, 'src/bar.f90:1:1: warning: unused', s, &
            'to_string: warning format')

        d = diag_new('x.f90', 99, 0, DIAG_INFO, 'note text')
        s = diag_to_string(d)
        call test_assert_equal_str(suite, 'x.f90:99:0: info: note text', s, &
            'to_string: info format')

        d = diag_new('y.f90', 2, 7, DIAG_HINT, 'try this')
        s = diag_to_string(d)
        call test_assert_equal_str(suite, 'y.f90:2:7: hint: try this', s, &
            'to_string: hint format')
    end subroutine test_diag_to_string

    subroutine test_diag_to_json(suite)
        type(test_suite_t), intent(inout) :: suite
        type(diag_t) :: d
        type(diag_t) :: diags(3)
        type(json_builder_t) :: jb
        character(len=:), allocatable :: s

        d = diag_new('foo.f90', 10, 5, DIAG_ERROR, 'bad thing')
        jb = json_new()
        call diag_to_json(d, jb)
        s = json_to_string(jb)
        call test_assert_equal_str(suite, &
            '{"file":"foo.f90","line":10,"col":5,"severity":0,"message":"bad thing"}', &
            s, 'to_json: basic error')

        ! warning severity
        d = diag_new('bar.f90', 3, 1, DIAG_WARNING, 'unused')
        jb = json_new()
        call diag_to_json(d, jb)
        s = json_to_string(jb)
        call test_assert_equal_str(suite, &
            '{"file":"bar.f90","line":3,"col":1,"severity":1,"message":"unused"}', &
            s, 'to_json: warning')

        ! with hint
        d = diag_new('x.f90', 1, 1, DIAG_HINT, 'suggestion')
        d%hint = 'add intent(in)'
        jb = json_new()
        call diag_to_json(d, jb)
        s = json_to_string(jb)
        call test_assert(suite, index(s, '"hint":"add intent(in)"') > 0, &
            'to_json: hint included')

        ! diags_to_json: array of two
        diags(1) = diag_new('a.f90', 1, 1, DIAG_ERROR, 'err1')
        diags(2) = diag_new('b.f90', 2, 2, DIAG_WARNING, 'warn1')
        jb = json_new()
        call diags_to_json(diags, 2, jb)
        s = json_to_string(jb)
        call test_assert(suite, s(1:1) == '[', 'diags_to_json: starts with [')
        call test_assert(suite, s(len(s):len(s)) == ']', 'diags_to_json: ends with ]')
        call test_assert(suite, index(s, '"a.f90"') > 0, 'diags_to_json: first file')
        call test_assert(suite, index(s, '"b.f90"') > 0, 'diags_to_json: second file')

        ! empty array
        jb = json_new()
        call diags_to_json(diags, 0, jb)
        call test_assert_equal_str(suite, '[]', json_to_string(jb), &
            'diags_to_json: empty')
    end subroutine test_diag_to_json

    subroutine test_diag_strip_prefix(suite)
        type(test_suite_t), intent(inout) :: suite
        type(diag_t) :: d

        d = diag_new('/home/user/proj/src/foo.f90', 1, 1, DIAG_ERROR, 'e')
        call diag_strip_prefix(d, '/home/user/proj')
        call test_assert_equal_str(suite, 'src/foo.f90', trim(d%file), &
            'strip_prefix: strips leading path')

        ! no match: unchanged
        d = diag_new('/other/path/foo.f90', 1, 1, DIAG_ERROR, 'e')
        call diag_strip_prefix(d, '/home/user')
        call test_assert_equal_str(suite, '/other/path/foo.f90', trim(d%file), &
            'strip_prefix: no match unchanged')

        ! exact match: becomes empty
        d = diag_new('/a/b', 1, 1, DIAG_ERROR, 'e')
        call diag_strip_prefix(d, '/a/b')
        call test_assert_equal_str(suite, '', trim(d%file), &
            'strip_prefix: exact match empty')

        ! partial dir name must not match
        d = diag_new('/a/bc/foo.f90', 1, 1, DIAG_ERROR, 'e')
        call diag_strip_prefix(d, '/a/b')
        call test_assert_equal_str(suite, '/a/bc/foo.f90', trim(d%file), &
            'strip_prefix: no partial dir match')

        ! empty prefix: unchanged
        d = diag_new('/a/b/c.f90', 1, 1, DIAG_ERROR, 'e')
        call diag_strip_prefix(d, '')
        call test_assert_equal_str(suite, '/a/b/c.f90', trim(d%file), &
            'strip_prefix: empty prefix unchanged')
    end subroutine test_diag_strip_prefix

end program test_diag
