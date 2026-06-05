module fx_cli
    implicit none
    private

    integer, parameter :: MAX_ARGS = 64
    integer, parameter :: MAX_ARG_LEN = 512

    type, public :: cli_t
        character(len=MAX_ARG_LEN) :: args(MAX_ARGS) = ' '
        integer :: n_args = 0
        character(len=256) :: program_name = ' '
    end type cli_t

    public :: cli_init, cli_has_flag, cli_get_value
    public :: cli_get_positional, cli_n_positional, cli_command

contains

    subroutine cli_init(c, args)
        type(cli_t), intent(out) :: c
        character(len=*), intent(in), optional :: args(:)
        integer :: i
        integer :: count
        character(len=MAX_ARG_LEN) :: arg

        c%args = ' '
        c%n_args = 0
        c%program_name = ' '

        if (present(args)) then
            count = min(size(args), MAX_ARGS)
            c%n_args = count
            do i = 1, count
                c%args(i) = trim(args(i))
            end do
        else
            count = min(command_argument_count(), MAX_ARGS)
            call get_command_argument(0, c%program_name)
            c%program_name = trim(c%program_name)
            c%n_args = count
            do i = 1, count
                call get_command_argument(i, arg)
                c%args(i) = trim(arg)
            end do
        end if
    end subroutine cli_init

    logical function cli_has_flag(c, flag)
        type(cli_t), intent(in) :: c
        character(len=*), intent(in) :: flag
        integer :: i
        logical :: past_terminator
        character(len=MAX_ARG_LEN) :: flag_text
        character(len=MAX_ARG_LEN) :: arg_text

        cli_has_flag = .false.
        flag_text = trim(flag)
        if (.not. is_long_flag(flag_text)) return

        past_terminator = .false.
        do i = 1, c%n_args
            arg_text = trim(c%args(i))
            if (past_terminator) exit
            if (is_terminator(arg_text)) then
                past_terminator = .true.
            else if (arg_text == flag_text) then
                cli_has_flag = .true.
                return
            end if
        end do
    end function cli_has_flag

    function cli_get_value(c, key, default_val) result(res)
        type(cli_t), intent(in) :: c
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: default_val
        character(len=:), allocatable :: res
        integer :: i
        integer :: key_len
        integer :: arg_len
        logical :: past_terminator
        character(len=MAX_ARG_LEN) :: key_text
        character(len=MAX_ARG_LEN) :: arg_text
        character(len=MAX_ARG_LEN) :: next_text
        character(len=MAX_ARG_LEN) :: prefix

        res = default_val
        key_text = trim(key)
        if (len_trim(key_text) >= 2 .and. key_text(1:2) == '--') then
            key_text = key_text(3:)
        end if
        if (len_trim(key_text) == 0) return

        key_text = '--' // trim(key_text)
        key_len = len_trim(key_text)
        prefix = key_text(1:key_len) // '='

        past_terminator = .false.
        do i = 1, c%n_args
            arg_text = trim(c%args(i))
            if (past_terminator) exit
            if (is_terminator(arg_text)) then
                past_terminator = .true.
            else if (arg_text == key_text) then
                if (i < c%n_args) then
                    next_text = trim(c%args(i + 1))
                    if (takes_value(next_text)) res = trim(next_text)
                end if
            else
                arg_len = len_trim(arg_text)
                if (arg_len >= key_len + 1) then
                    if (arg_text(1:key_len + 1) == prefix) then
                        if (arg_len == key_len + 1) then
                            res = ''
                        else
                            res = arg_text(key_len + 2:arg_len)
                        end if
                    end if
                end if
            end if
        end do
    end function cli_get_value

    function cli_get_positional(c, index) result(res)
        type(cli_t), intent(in) :: c
        integer, intent(in) :: index
        character(len=:), allocatable :: res
        integer :: i
        integer :: positional_index
        logical :: past_terminator
        logical :: skip_next
        character(len=MAX_ARG_LEN) :: arg_text

        res = ''
        if (index <= 0) return

        positional_index = 0
        past_terminator = .false.
        skip_next = .false.
        do i = 1, c%n_args
            arg_text = trim(c%args(i))
            if (skip_next) then
                skip_next = .false.
                cycle
            end if

            if (past_terminator) then
                positional_index = positional_index + 1
                if (positional_index == index) then
                    res = trim(arg_text)
                    return
                end if
            else if (is_terminator(arg_text)) then
                past_terminator = .true.
            else if (is_long_flag(arg_text)) then
                if (consumes_separate_value(arg_text, i, c)) then
                    skip_next = .true.
                end if
            else
                positional_index = positional_index + 1
                if (positional_index == index) then
                    res = trim(arg_text)
                    return
                end if
            end if
        end do
    end function cli_get_positional

    integer function cli_n_positional(c)
        type(cli_t), intent(in) :: c
        integer :: i
        logical :: past_terminator
        logical :: skip_next
        character(len=MAX_ARG_LEN) :: arg_text

        cli_n_positional = 0
        past_terminator = .false.
        skip_next = .false.
        do i = 1, c%n_args
            arg_text = trim(c%args(i))
            if (skip_next) then
                skip_next = .false.
                cycle
            end if

            if (past_terminator) then
                cli_n_positional = cli_n_positional + 1
            else if (is_terminator(arg_text)) then
                past_terminator = .true.
            else if (is_long_flag(arg_text)) then
                if (consumes_separate_value(arg_text, i, c)) then
                    skip_next = .true.
                end if
            else
                cli_n_positional = cli_n_positional + 1
            end if
        end do
    end function cli_n_positional

    function cli_command(c) result(res)
        type(cli_t), intent(in) :: c
        character(len=:), allocatable :: res

        res = cli_get_positional(c, 1)
    end function cli_command

    logical function is_long_flag(text)
        character(len=*), intent(in) :: text
        integer :: text_len

        text_len = len_trim(text)
        is_long_flag = text_len > 2 .and. text(1:2) == '--'
    end function is_long_flag

    logical function is_terminator(text)
        character(len=*), intent(in) :: text

        is_terminator = trim(text) == '--'
    end function is_terminator

    logical function takes_value(text)
        character(len=*), intent(in) :: text

        takes_value = .not. is_terminator(text) .and. .not. is_long_flag(text)
    end function takes_value

    logical function consumes_separate_value(arg_text, arg_index, c)
        character(len=*), intent(in) :: arg_text
        integer, intent(in) :: arg_index
        type(cli_t), intent(in) :: c
        integer :: eq_pos
        character(len=MAX_ARG_LEN) :: next_text

        consumes_separate_value = .false.
        eq_pos = index(trim(arg_text), '=')
        if (eq_pos /= 0) return
        if (arg_index >= c%n_args) return

        next_text = trim(c%args(arg_index + 1))
        consumes_separate_value = takes_value(next_text)
    end function consumes_separate_value

end module fx_cli
