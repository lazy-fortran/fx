module fx_path
    use fx_proc, only: proc_path_is_dir
    implicit none
    private

    public :: path_join, path_dirname, path_basename
    public :: path_extension, path_stem, path_strip_prefix
    public :: path_normalize, path_is_absolute, path_relative
    public :: path_exists, path_is_dir, path_is_file

contains

    pure function path_join(a, b) result(res)
        character(len=*), intent(in) :: a
        character(len=*), intent(in) :: b
        character(len=:), allocatable :: res
        character(len=:), allocatable :: left
        character(len=:), allocatable :: right

        left = strip_trailing_slashes(a)
        right = trim(b)

        if (len_trim(left) == 0) then
            res = right
        else if (len_trim(right) == 0) then
            res = left
        else if (left == '/') then
            res = '/' // right
        else
            res = left // '/' // right
        end if
    end function path_join

    pure function path_dirname(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        character(len=:), allocatable :: clean
        integer :: idx

        clean = strip_trailing_slashes(p)
        if (len_trim(clean) == 0) then
            res = '.'
            return
        end if

        if (clean == '/') then
            res = '/'
            return
        end if

        idx = last_slash(clean)
        if (idx <= 0) then
            res = '.'
        else if (idx == 1) then
            res = '/'
        else
            res = clean(:idx - 1)
        end if
    end function path_dirname

    pure function path_basename(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        character(len=:), allocatable :: clean
        integer :: idx

        clean = strip_trailing_slashes(p)
        if (len_trim(clean) == 0) then
            res = ''
            return
        end if

        if (clean == '/') then
            res = '/'
            return
        end if

        idx = last_slash(clean)
        if (idx <= 0) then
            res = clean
        else
            res = clean(idx + 1:)
        end if
    end function path_basename

    pure function path_extension(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        character(len=:), allocatable :: base
        integer :: idx

        base = path_basename(p)
        idx = last_dot(base)
        if (idx <= 1 .or. idx >= len_trim(base)) then
            res = ''
        else
            res = base(idx:)
        end if
    end function path_extension

    pure function path_stem(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        character(len=:), allocatable :: base
        integer :: idx

        base = path_basename(p)
        idx = last_dot(base)
        if (idx <= 1 .or. idx >= len_trim(base)) then
            res = base
        else
            res = base(:idx - 1)
        end if
    end function path_stem

    pure function path_strip_prefix(p, prefix) result(res)
        character(len=*), intent(in) :: p
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: res
        character(len=:), allocatable :: clean_p
        character(len=:), allocatable :: clean_prefix
        integer :: n_prefix

        clean_p = path_normalize(p)
        clean_prefix = path_normalize(prefix)
        n_prefix = len_trim(clean_prefix)

        if (n_prefix == 0) then
            res = clean_p
            return
        end if

        if (trim(clean_p) == trim(clean_prefix)) then
            res = ''
            return
        end if

        if (len_trim(clean_p) > n_prefix .and. &
            clean_p(1:n_prefix) == clean_prefix .and. &
            clean_p(n_prefix + 1:n_prefix + 1) == '/') then
            res = clean_p(n_prefix + 2:)
        else
            res = clean_p
        end if
    end function path_strip_prefix

    pure function path_normalize(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        integer :: i
        logical :: prev_slash
        character(len=1) :: ch

        res = ''
        prev_slash = .false.
        do i = 1, len_trim(p)
            ch = p(i:i)
            if (ch == '/') then
                if (prev_slash) cycle
                prev_slash = .true.
            else
                prev_slash = .false.
            end if
            res = res // ch
        end do

        if (len_trim(res) > 1 .and. res(len_trim(res):len_trim(res)) == '/') then
            res = res(:len_trim(res) - 1)
        end if
    end function path_normalize

    pure logical function path_is_absolute(p)
        character(len=*), intent(in) :: p

        path_is_absolute = len_trim(p) > 0 .and. p(1:1) == '/'
    end function path_is_absolute

    pure function path_relative(p, base) result(res)
        character(len=*), intent(in) :: p
        character(len=*), intent(in) :: base
        character(len=:), allocatable :: res
        character(len=:), allocatable :: clean_p
        character(len=:), allocatable :: clean_base
        integer :: n_base

        clean_p = path_normalize(p)
        clean_base = path_normalize(base)
        n_base = len_trim(clean_base)

        if (n_base == 0) then
            res = clean_p
            return
        end if

        if (trim(clean_p) == trim(clean_base)) then
            res = ''
        else if (len_trim(clean_p) > n_base .and. &
                 clean_p(1:n_base) == clean_base .and. &
                 clean_p(n_base + 1:n_base + 1) == '/') then
            res = clean_p(n_base + 2:)
        else
            res = clean_p
        end if
    end function path_relative

    logical function path_exists(p)
        character(len=*), intent(in) :: p
        logical :: exists

        inquire(file=trim(p), exist=exists)
        path_exists = exists
    end function path_exists

    logical function path_is_dir(p)
        character(len=*), intent(in) :: p

        path_is_dir = proc_path_is_dir(trim(p))
    end function path_is_dir

    logical function path_is_file(p)
        character(len=*), intent(in) :: p

        path_is_file = path_exists(p) .and. .not. path_is_dir(p)
    end function path_is_file

    pure function strip_trailing_slashes(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        integer :: n

        n = len_trim(p)
        if (n == 0) then
            res = ''
            return
        end if

        do while (n > 1 .and. p(n:n) == '/')
            n = n - 1
        end do
        res = p(:n)
    end function strip_trailing_slashes

    pure integer function last_slash(p) result(idx)
        character(len=*), intent(in) :: p
        integer :: i

        idx = 0
        do i = len_trim(p), 1, -1
            if (p(i:i) == '/') then
                idx = i
                return
            end if
        end do
    end function last_slash

    pure integer function last_dot(p) result(idx)
        character(len=*), intent(in) :: p
        integer :: i

        idx = 0
        do i = len_trim(p), 1, -1
            if (p(i:i) == '.') then
                idx = i
                return
            end if
        end do
    end function last_dot

end module fx_path
