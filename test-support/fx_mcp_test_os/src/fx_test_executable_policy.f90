module fx_test_executable_policy
    implicit none
    private
    public :: forbidden_executable
contains
    logical function forbidden_executable(path, shebang)
        character(len=*), intent(in) :: path, shebang
        character(len=:), allocatable :: basename, extension, interpreter
        integer :: slash, dot
        slash = scan(trim(path), '/', back=.true.)
        basename = lower(trim(path(slash + 1:)))
        dot = scan(basename, '.', back=.true.)
        extension = ''
        if (dot > 0) extension = basename(dot:)
        forbidden_executable = forbidden_extension(extension) .or. forbidden_name(basename)
        if (forbidden_executable) return
        interpreter = shebang_interpreter(shebang)
        forbidden_executable = forbidden_name(interpreter)
    end function forbidden_executable

    logical function forbidden_extension(extension)
        character(len=*), intent(in) :: extension
        select case (extension)
        case ('.sh', '.bash', '.zsh', '.ksh', '.fish', '.csh', '.py', &
                '.pyc', '.pyo', '.js', '.mjs', '.cjs', '.pl', '.pm', &
                '.rb', '.lua', '.php', '.awk', '.tcl', '.r')
            forbidden_extension = .true.
        case default
            forbidden_extension = .false.
        end select
    end function forbidden_extension

    function shebang_interpreter(line) result(name)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: name, token
        integer :: start
        name = ''
        if (len_trim(line) < 3) return
        if (line(:2) /= '#!') return
        start = 3
        token = next_word(line, start)
        name = basename_of(token)
        if (name == 'env') name = env_interpreter(remainder(line, start))
    end function shebang_interpreter

    recursive function env_interpreter(line) result(name)
        character(len=*), intent(in) :: line
        character(len=:), allocatable :: name, token, split_text
        integer :: start, option_kind
        logical :: options
        name = ''
        start = 1
        options = .true.
        do
            token = next_word(line, start)
            if (len(token) == 0) return
            if (token == '--') then
                options = .false.
                cycle
            end if
            if (options) then
                call env_option(token, option_kind, split_text)
                select case (option_kind)
                case (1)
                    cycle
                case (2)
                    token = next_word(line, start)
                    cycle
                case (3)
                    split_text = next_word(line, start)
                    name = env_interpreter(split_text//' '//remainder(line, start))
                    return
                case (4)
                    name = env_interpreter(split_text//' '//remainder(line, start))
                    return
                end select
            end if
            if (index(token, '=') > 0) cycle
            name = basename_of(token)
            if (name == 'env') name = env_interpreter(remainder(line, start))
            return
        end do
    end function env_interpreter

    subroutine env_option(token, kind, split_text)
        character(len=*), intent(in) :: token
        integer, intent(out) :: kind
        character(len=:), allocatable, intent(out) :: split_text
        integer :: i
        kind = 0
        split_text = ''
        if (len(token) < 1) return
        if (token(1:1) /= '-') return
        kind = 1
        select case (token)
        case ('-u', '--unset', '-C', '--chdir', '-a', '--argv0', '-P')
            kind = 2
            return
        case ('-S', '--split-string')
            kind = 3
            return
        end select
        if (index(token, '--split-string=') == 1) then
            kind = 4
            split_text = token(len('--split-string=') + 1:)
            return
        end if
        if (len(token) < 2) return
        if (token(2:2) == '-') return
        do i = 2, len(token)
            select case (token(i:i))
            case ('u', 'C', 'a', 'P')
                if (i == len(token)) kind = 2
                return
            case ('S')
                kind = 4
                split_text = token(i + 1:)
                if (i == len(token)) kind = 3
                return
            end select
        end do
    end subroutine env_option

    function next_word(line, start) result(word)
        character(len=*), intent(in) :: line
        integer, intent(inout) :: start
        character(len=:), allocatable :: word
        character :: quote, current
        word = ''
        quote = achar(0)
        do while (start <= len_trim(line))
            current = line(start:start)
            if (current /= ' ' .and. current /= achar(9)) exit
            start = start + 1
        end do
        do while (start <= len_trim(line))
            current = line(start:start)
            if (quote == achar(0)) then
                if (current == ' ' .or. current == achar(9)) exit
            end if
            start = start + 1
            if (current == achar(92) .and. quote /= "'") then
                if (start <= len_trim(line)) then
                    word = word//line(start:start)
                    start = start + 1
                end if
            else if (quote == current) then
                quote = achar(0)
            else if (quote == achar(0) .and. (current == '"' .or. current == "'")) then
                quote = current
            else
                word = word//current
            end if
        end do
    end function next_word

    function remainder(line, start) result(text)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start
        character(len=:), allocatable :: text
        text = ''
        if (start <= len(line)) text = line(start:)
    end function remainder

    function basename_of(path) result(name)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: name
        integer :: slash
        slash = scan(trim(path), '/', back=.true.)
        name = lower(trim(path(slash + 1:)))
    end function basename_of

    logical function forbidden_name(name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: lower_name
        lower_name = lower(name)
        forbidden_name = lower_name == 'sh' .or. lower_name == 'bash' .or. &
            lower_name == 'dash' .or. lower_name == 'ash' .or. &
            lower_name == 'zsh' .or. lower_name == 'ksh' .or. &
            lower_name == 'csh' .or. lower_name == 'tcsh' .or. &
            lower_name == 'fish' .or. lower_name == 'python' .or. &
            lower_name == 'python2' .or. lower_name == 'python3' .or. &
            lower_name == 'node' .or. lower_name == 'nodejs' .or. &
            lower_name == 'npm' .or. lower_name == 'npx' .or. &
            lower_name == 'perl' .or. lower_name == 'ruby' .or. &
            lower_name == 'lua' .or. lower_name == 'php' .or. &
            lower_name == 'awk' .or. lower_name == 'mawk' .or. &
            lower_name == 'gawk' .or. lower_name == 'tclsh' .or. &
            lower_name == 'wish' .or. lower_name == 'r' .or. &
            lower_name == 'rscript'
        forbidden_name = forbidden_name .or. &
            starts_with_version(lower_name, 'python') .or. &
            starts_with_version(lower_name, 'node')
    end function forbidden_name

    logical function starts_with_version(name, prefix)
        character(len=*), intent(in) :: name, prefix
        integer :: i
        starts_with_version = .false.
        if (len(name) <= len(prefix)) return
        if (name(:len(prefix)) /= prefix) return
        do i = len(prefix) + 1, len(name)
            if ((name(i:i) < '0' .or. name(i:i) > '9') .and. &
                name(i:i) /= '.') return
        end do
        starts_with_version = .true.
    end function starts_with_version

    function lower(text) result(result)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: result
        integer :: i, code
        result = text
        do i = 1, len(text)
            code = iachar(text(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) &
                result(i:i) = achar(code + 32)
        end do
    end function lower
end module fx_test_executable_policy
