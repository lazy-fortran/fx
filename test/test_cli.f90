program test_cli
    use fx_cli, only: cli_t, cli_init, cli_has_flag, cli_get_value, &
                      cli_get_positional, cli_n_positional, cli_command
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
                       test_assert_equal_int, test_assert_equal_str, &
                       test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_cli')
    call test_cli_flag_and_value_patterns(suite)
    call test_cli_terminator_and_positionals(suite)
    call test_cli_duplicates_and_empty_value(suite)
    call test_cli_empty_args(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_cli_flag_and_value_patterns(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cli_t) :: cli
        type(cli_t) :: dot_cli
        type(cli_t) :: spaced_cli
        type(cli_t) :: blocked_cli
        character(len=32), parameter :: args(2) = [character(len=32) :: &
            '--json=compact', 'build']
        character(len=32), parameter :: dot_args(1) = [character(len=32) :: &
            '--dot']
        character(len=32), parameter :: spaced_args(2) = [character(len=32) :: &
            '--json', 'full']
        character(len=32), parameter :: blocked_args(2) = [character(len=32) :: &
            '--json', '--other']

        call load_cli(cli, args)
        call load_cli(dot_cli, dot_args)
        call load_cli(spaced_cli, spaced_args)
        call load_cli(blocked_cli, blocked_args)

        call test_assert(suite, cli_has_flag(dot_cli, '--dot'), &
                         'cli_has_flag exact match')
        call test_assert(suite, .not. cli_has_flag(cli, '--json'), &
                         'cli_has_flag ignores key=value')
        call test_assert_equal_str(suite, 'compact', &
                                   cli_get_value(cli, 'json', 'fallback'), &
                                   'cli_get_value key=value')
        call test_assert_equal_str(suite, 'fallback', &
                                   cli_get_value(cli, 'missing', 'fallback'), &
                                   'cli_get_value default when missing')
        call test_assert_equal_str(suite, 'full', &
                                   cli_get_value(spaced_cli, 'json', &
                                                 'fallback'), &
                                   'cli_get_value spaced value')
        call test_assert_equal_int(suite, 0, cli_n_positional(spaced_cli), &
                                   'cli_n_positional skips consumed value')
        call test_assert_equal_str(suite, '', &
                                   cli_get_positional(spaced_cli, 1), &
                                   'cli_get_positional skips consumed value')
        call test_assert_equal_str(suite, 'fallback', &
                                   cli_get_value(blocked_cli, 'json', &
                                                 'fallback'), &
                                   'cli_get_value does not consume next flag')
        call test_assert(suite, cli_has_flag(blocked_cli, '--other'), &
                         'cli_has_flag sees later flag')
        call test_assert_equal_int(suite, 0, cli_n_positional(blocked_cli), &
                                   'cli_n_positional skips bare flags')
        call test_assert_equal_str(suite, '', &
                                   cli_get_positional(blocked_cli, 1), &
                                   'cli_get_positional skips bare flags')
        call test_assert_equal_int(suite, 1, cli_n_positional(cli), &
                                   'cli_n_positional counts one positional')
        call test_assert_equal_str(suite, 'build', cli_command(cli), &
                                   'cli_command first positional')
        call test_assert_equal_str(suite, 'build', &
                                   cli_get_positional(cli, 1), &
                                   'cli_get_positional 1-based index')
        call test_assert_equal_str(suite, '', cli_get_positional(cli, 0), &
                                   'cli_get_positional rejects zero index')
    end subroutine test_cli_flag_and_value_patterns

    subroutine test_cli_terminator_and_positionals(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cli_t) :: cli
        character(len=32), parameter :: args(5) = [character(len=32) :: &
            'build', '--', '--check', '-s', 'target']

        call load_cli(cli, args)

        call test_assert_equal_str(suite, 'fallback', &
                                   cli_get_value(cli, 'check', 'fallback'), &
                                   'cli_get_value ignores args after terminator')
        call test_assert(suite, .not. cli_has_flag(cli, '--check'), &
                         'cli_has_flag ignores args after terminator')
        call test_assert(suite, .not. cli_has_flag(cli, '-s'), &
                         'cli_has_flag rejects short flags')
        call test_assert_equal_int(suite, 4, cli_n_positional(cli), &
                                   'cli_n_positional counts args after terminator')
        call test_assert_equal_str(suite, 'build', cli_command(cli), &
                                   'cli_command before terminator')
        call test_assert_equal_str(suite, '--check', &
                                   cli_get_positional(cli, 2), &
                                   'cli_get_positional keeps post-terminator arg')
        call test_assert_equal_str(suite, '-s', cli_get_positional(cli, 3), &
                                   'cli_get_positional keeps short flag literal')
        call test_assert_equal_str(suite, 'target', &
                                   cli_get_positional(cli, 4), &
                                   'cli_get_positional keeps trailing positional')
        call test_assert_equal_str(suite, '', cli_get_positional(cli, -1), &
                                   'cli_get_positional rejects negative index')
    end subroutine test_cli_terminator_and_positionals

    subroutine test_cli_duplicates_and_empty_value(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cli_t) :: cli
        character(len=32), parameter :: args(4) = [character(len=32) :: &
            'go', '--json=', '--check', '--check']

        call load_cli(cli, args)

        call test_assert_equal_str(suite, '', &
                                   cli_get_value(cli, 'json', 'fallback'), &
                                   'cli_get_value preserves empty value')
        call test_assert(suite, cli_has_flag(cli, '--check'), &
                         'cli_has_flag tolerates duplicates')
        call test_assert_equal_int(suite, 1, cli_n_positional(cli), &
                                   'cli_n_positional ignores duplicate flags')
        call test_assert_equal_str(suite, 'go', cli_command(cli), &
                                   'cli_command with one positional')
        call test_assert_equal_str(suite, 'go', cli_get_positional(cli, 1), &
                                   'cli_get_positional with one positional')
        call test_assert_equal_str(suite, '', cli_get_positional(cli, 2), &
                                   'cli_get_positional rejects out-of-range index')
    end subroutine test_cli_duplicates_and_empty_value

    subroutine test_cli_empty_args(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cli_t) :: cli
        character(len=32), allocatable :: args(:)

        allocate(args(0))
        call load_cli(cli, args)

        call test_assert_equal_int(suite, 0, cli_n_positional(cli), &
                                   'cli_n_positional zero for empty args')
        call test_assert_equal_str(suite, '', cli_command(cli), &
                                   'cli_command empty for empty args')
        call test_assert_equal_str(suite, '', cli_get_positional(cli, 1), &
                                   'cli_get_positional empty for empty args')
        call test_assert_equal_str(suite, '', cli_get_value(cli, 'json', ''), &
                                   'cli_get_value default for empty args')
        call test_assert(suite, .not. cli_has_flag(cli, '--json'), &
                         'cli_has_flag false for empty args')
    end subroutine test_cli_empty_args

    subroutine load_cli(cli, args)
        type(cli_t), intent(out) :: cli
        character(len=*), intent(in) :: args(:)

        call cli_init(cli, args)
    end subroutine load_cli

end program test_cli
