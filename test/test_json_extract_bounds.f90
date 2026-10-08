program test_json_extract_bounds
    use fx_test, only: test_suite_t, test_suite_init, test_suite_summary, &
        test_suite_exit, test_assert, test_assert_equal_str, test_assert_equal_int
    use fx_json_parse, only: json_extract_string, json_extract_int, json_extract_bool
    implicit none
    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_json_extract_bounds')
    call test_root_scalars(suite)
    call test_path_capacity(suite)
    call test_container_boundaries(suite)
    call test_nested_array_elements(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_nested_array_elements(suite)
        type(test_suite_t), intent(inout) :: suite
        character(:), allocatable :: text
        integer :: number
        logical :: found, value
        character(len=*), parameter :: input = &
            '{"items":[{"name":"first","n":11,"ok":false},' // &
            '{"name":"second","n":22,"ok":true}],' // &
            '"nested":[["zero"],["one","two"]]}'

        call json_extract_string(input, 'items[2].name', text, found)
        call test_assert(suite, found, 'object array string exists')
        call test_assert_equal_str(suite, 'second', text, 'second object string')
        call json_extract_int(input, 'items[2].n', number, found)
        call test_assert(suite, found, 'object array integer exists')
        call test_assert_equal_int(suite, 22, number, 'second object integer')
        call json_extract_bool(input, 'items[2].ok', value, found)
        call test_assert(suite, found, 'object array boolean exists')
        call test_assert(suite, value, 'second object boolean')
        call json_extract_string(input, 'nested[2][2]', text, found)
        call test_assert(suite, found, 'nested array string exists')
        call test_assert_equal_str(suite, 'two', text, 'nested array position')
    end subroutine test_nested_array_elements

    subroutine test_root_scalars(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=16), parameter :: inputs(*) = &
            [character(len=16) :: '42', '-7', 'true', 'false', '"root"', '""', &
            'null', '1.25']
        character(len=:), allocatable :: text
        integer :: number, i
        logical :: value, found

        do i = 1, size(inputs)
            ! A named path cannot match a scalar root, regardless of scalar type.
            call json_extract_string(trim(inputs(i)), 'missing', text, found)
            call test_assert(suite, .not. found, 'root named string path absent')
            call test_assert_equal_str(suite, '', text, 'root missing string default')
            call json_extract_int(trim(inputs(i)), 'missing', number, found)
            call test_assert(suite, .not. found, 'root named integer path absent')
            call test_assert_equal_int(suite, 0, number, 'root missing integer default')
            call json_extract_bool(trim(inputs(i)), 'missing', value, found)
            call test_assert(suite, .not. found, 'root named boolean path absent')
            call test_assert(suite, .not. value, 'root missing boolean default')

            ! An empty path names the root, with the usual typed extraction rules.
            call json_extract_string(trim(inputs(i)), '', text, found)
            call test_assert(suite, found .eqv. (i == 5 .or. i == 6), &
                'root empty string path type match')
            if (i == 5) call test_assert_equal_str(suite, 'root', text, 'root text')
            if (i == 6) call test_assert_equal_str(suite, '', text, 'root empty text')
            call json_extract_int(trim(inputs(i)), '', number, found)
            call test_assert(suite, found .eqv. (i == 1 .or. i == 2), &
                'root empty integer path type match')
            if (i == 1) call test_assert_equal_int(suite, 42, number, 'root integer')
            if (i == 2) call test_assert_equal_int(suite, -7, number, 'root negative')
            call json_extract_bool(trim(inputs(i)), '', value, found)
            call test_assert(suite, found .eqv. (i == 3 .or. i == 4), &
                'root empty boolean path type match')
            if (i == 3) call test_assert(suite, value, 'root true')
            if (i == 4) call test_assert(suite, .not. value, 'root false')
        end do
    end subroutine test_root_scalars

    subroutine test_path_capacity(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: input, path, text
        integer :: number
        logical :: value, found

        ! The 64-segment capacity is usable; a 65th segment must not overflow or
        ! accidentally match its truncated 64-segment prefix.
        input = repeat('{"a":', 64) // '42' // repeat('}', 64)
        path = repeat('a.', 63) // 'a'
        call json_extract_int(input, path, number, found)
        call test_assert(suite, found, 'maximum path capacity found')
        call test_assert_equal_int(suite, 42, number, 'maximum path capacity value')
        path = path // '[1]'
        call json_extract_int(input, path, number, found)
        call test_assert(suite, .not. found, 'excess bracket path absent')
        call json_extract_string('"root"', repeat('a.', 64) // 'a', text, found)
        call test_assert(suite, .not. found, 'excess dotted string path absent')
        call json_extract_bool('true', repeat('a.', 64) // 'a', value, found)
        call test_assert(suite, .not. found, 'excess dotted boolean path absent')
        call json_extract_int('{}', repeat('a.', 65), number, found)
        call test_assert(suite, .not. found, 'excess terminated dotted path absent')
        call json_extract_string('[]', repeat('[1]', 65), text, found)
        call test_assert(suite, .not. found, 'excess terminated bracket path absent')
    end subroutine test_path_capacity

    subroutine test_container_boundaries(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=2), parameter :: empty_inputs(*) = &
            [character(len=2) :: '', '{}', '[]', '}', ']']
        character(len=:), allocatable :: input, text
        integer :: number, i
        logical :: value, found

        do i = 1, size(empty_inputs)
            call json_extract_string(trim(empty_inputs(i)), '', text, found)
            call test_assert(suite, .not. found, 'empty or invalid string absent')
            call json_extract_int(trim(empty_inputs(i)), '', number, found)
            call test_assert(suite, .not. found, 'empty or invalid integer absent')
            call json_extract_bool(trim(empty_inputs(i)), '', value, found)
            call test_assert(suite, .not. found, 'empty or invalid boolean absent')
        end do
        do i = 128, 129
            input = repeat('[', i) // '42' // repeat(']', i)
            call json_extract_string(input, 'missing', text, found)
            call test_assert(suite, .not. found, 'maximum or excess string absent')
            call json_extract_int(input, 'missing', number, found)
            call test_assert(suite, .not. found, 'maximum or excess integer absent')
            call json_extract_bool(input, 'missing', value, found)
            call test_assert(suite, .not. found, 'maximum or excess boolean absent')
        end do
    end subroutine test_container_boundaries

end program test_json_extract_bounds
