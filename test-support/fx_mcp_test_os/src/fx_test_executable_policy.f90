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
        integer :: start, finish, count
        name = ''
        if (len_trim(line) < 3) return
        if (line(:2) /= '#!') return
        start = 3
        count = 0
        do while (start <= len_trim(line))
            do while (start <= len_trim(line))
                if (line(start:start) /= ' ' .and. line(start:start) /= achar(9)) exit
                start = start + 1
            end do
            if (start > len_trim(line)) exit
            finish = start
            do while (finish <= len_trim(line))
                if (line(finish:finish) == ' ' .or. line(finish:finish) == achar(9)) exit
                finish = finish + 1
            end do
            token = line(start:finish - 1)
            start = finish + 1
            if (count == 0 .and. basename_of(token) == 'env') cycle
            if (token == '-S' .or. token == '-i') cycle
            name = basename_of(token)
            return
        end do
    end function shebang_interpreter

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
