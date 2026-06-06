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

        if (path_is_absolute(b)) then
            res = trim(b)
            return
        end if

        left = strip_trailing_slashes(a)
        if (len_trim(left) == 0) then
            res = trim(b)
        else if (len_trim(b) == 0) then
            res = left
        else if (left == '/') then
            res = '/' // trim(b)
        else
            res = left // '/' // trim(b)
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

        if (n_prefix == 0 .or. clean_prefix == '.') then
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
        character(len=256) :: stack(256)
        integer :: n_stack
        logical :: is_abs
        integer :: i, start, seg_end
        character(len=256) :: seg

        if (len_trim(p) == 0) then
            res = '.'
            return
        end if

        is_abs = p(1:1) == '/'
        n_stack = 0
        i = 1

        do
            if (i > len_trim(p)) exit

            ! Skip consecutive slashes
            do while (i <= len_trim(p) .and. p(i:i) == '/')
                i = i + 1
            end do
            if (i > len_trim(p)) exit

            ! Find end of segment
            start = i
            do while (i <= len_trim(p) .and. p(i:i) /= '/')
                i = i + 1
            end do
            seg_end = i - 1
            seg = p(start:seg_end)

            if (trim(seg) == '.' .or. len_trim(seg) == 0) then
                cycle
            else if (trim(seg) == '..') then
                if (n_stack > 0 .and. trim(stack(n_stack)) /= '..') then
                    n_stack = n_stack - 1
                else if (.not. is_abs) then
                    n_stack = n_stack + 1
                    stack(n_stack) = '..'
                end if
            else
                n_stack = n_stack + 1
                stack(n_stack) = seg
            end if
        end do

        if (n_stack == 0) then
            if (is_abs) then
                res = '/'
            else
                res = '.'
            end if
            return
        end if

        if (is_abs) then
            res = '/' // trim(stack(1))
        else
            res = trim(stack(1))
        end if
        do i = 2, n_stack
            res = res // '/' // trim(stack(i))
        end do
    end function path_normalize

    pure logical function path_is_absolute(p)
        character(len=*), intent(in) :: p

        if (len_trim(p) == 0) then
            path_is_absolute = .false.
        else
            path_is_absolute = p(1:1) == '/'
        end if
    end function path_is_absolute

    pure function path_relative(base, target) result(res)
        character(len=*), intent(in) :: base
        character(len=*), intent(in) :: target
        character(len=:), allocatable :: res
        character(len=:), allocatable :: norm_base
        character(len=:), allocatable :: norm_target
        character(len=256) :: base_parts(256)
        character(len=256) :: tgt_parts(256)
        integer :: n_base, n_tgt, common, i
        character(len=:), allocatable :: rel

        norm_base = path_normalize(base)
        norm_target = path_normalize(target)

        if (norm_base == norm_target) then
            res = '.'
            return
        end if

        call split_path(norm_base, base_parts, n_base)
        call split_path(norm_target, tgt_parts, n_tgt)

        common = 0
        do while (common < n_base .and. common < n_tgt)
            if (trim(base_parts(common + 1)) == trim(tgt_parts(common + 1))) then
                common = common + 1
            else
                exit
            end if
        end do

        rel = ''
        do i = 1, n_base - common
            if (len_trim(rel) == 0) then
                rel = '..'
            else
                rel = rel // '/..'
            end if
        end do
        do i = common + 1, n_tgt
            if (len_trim(rel) == 0) then
                rel = trim(tgt_parts(i))
            else
                rel = rel // '/' // trim(tgt_parts(i))
            end if
        end do

        if (len_trim(rel) == 0) then
            res = '.'
        else
            res = rel
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

    ! Split normalized path on '/' into parts array
    pure subroutine split_path(p, parts, n_parts)
        character(len=*), intent(in) :: p
        character(len=256), intent(out) :: parts(:)
        integer, intent(out) :: n_parts
        integer :: i, start

        n_parts = 0
        start = 1
        if (len_trim(p) == 0 .or. p == '.') return

        ! Skip leading slash for absolute paths
        if (p(1:1) == '/') start = 2

        i = start
        do
            if (i > len_trim(p)) then
                if (i > start) then
                    n_parts = n_parts + 1
                    parts(n_parts) = p(start:)
                end if
                exit
            end if
            if (p(i:i) == '/') then
                if (i > start) then
                    n_parts = n_parts + 1
                    parts(n_parts) = p(start:i - 1)
                end if
                start = i + 1
            end if
            i = i + 1
        end do
    end subroutine split_path

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
