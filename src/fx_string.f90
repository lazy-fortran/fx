module fx_string
    implicit none
    private

    type, public :: string_t
        character(len=:), allocatable :: s
    end type string_t

    type, public :: builder_t
        character(len=:), allocatable :: buf
        integer :: len = 0
        integer :: cap = 0
    end type builder_t

    public :: str, builder_new, builder_append, builder_to_string
    public :: builder_reset
    public :: to_lower, to_upper, split, join
    public :: starts_with, ends_with, contains_str, replace_str
    public :: find_str, strip, repeat_str, utf8_len

contains

    function str(chars) result(res)
        character(len=*), intent(in) :: chars
        type(string_t) :: res
        res%s = chars
    end function str

    function builder_new(initial_cap) result(b)
        integer, intent(in) :: initial_cap
        type(builder_t) :: b
        integer :: cap
        cap = max(initial_cap, 1)
        allocate(character(len=cap) :: b%buf)
        b%len = 0
        b%cap = cap
    end function builder_new

    subroutine builder_append(b, text)
        type(builder_t), intent(inout) :: b
        character(len=*), intent(in) :: text
        integer :: m, new_len, new_cap
        character(len=:), allocatable :: new_buf

        m = len(text)
        if (m == 0) return

        new_len = b%len + m
        if (.not. allocated(b%buf)) then
            new_cap = max(new_len, 64)
            allocate(character(len=new_cap) :: b%buf)
            b%cap = new_cap
            b%len = 0
        else if (new_len > b%cap) then
            new_cap = max(new_len, b%cap * 2)
            allocate(character(len=new_cap) :: new_buf)
            new_buf(1:b%len) = b%buf(1:b%len)
            call move_alloc(new_buf, b%buf)
            b%cap = new_cap
        end if
        b%buf(b%len + 1:new_len) = text(1:m)
        b%len = new_len
    end subroutine builder_append

    function builder_to_string(b) result(res)
        type(builder_t), intent(in) :: b
        character(len=:), allocatable :: res
        if (b%len == 0) then
            res = ''
        else
            res = b%buf(1:b%len)
        end if
    end function builder_to_string

    subroutine builder_reset(b)
        type(builder_t), intent(inout) :: b
        b%len = 0
    end subroutine builder_reset

    function to_lower(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i, c
        res = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 65 .and. c <= 90) res(i:i) = achar(c + 32)
        end do
    end function to_lower

    function to_upper(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i, c
        res = s
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= 97 .and. c <= 122) res(i:i) = achar(c - 32)
        end do
    end function to_upper

    subroutine split(s, delimiter, parts, n_parts)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: delimiter
        character(len=256), intent(out) :: parts(:)
        integer, intent(out) :: n_parts
        integer :: dlen, slen, pos, start
        dlen = len(delimiter)
        slen = len(s)
        n_parts = 0
        if (dlen == 0) then
            if (n_parts < size(parts)) then
                n_parts = 1
                parts(1) = s
            end if
            return
        end if
        start = 1
        do
            if (start > slen) then
                if (n_parts < size(parts)) then
                    n_parts = n_parts + 1
                    parts(n_parts) = ''
                end if
                exit
            end if
            pos = index(s(start:), delimiter)
            if (pos == 0) then
                if (n_parts < size(parts)) then
                    n_parts = n_parts + 1
                    parts(n_parts) = s(start:)
                end if
                exit
            end if
            if (n_parts < size(parts)) then
                n_parts = n_parts + 1
                parts(n_parts) = s(start:start + pos - 2)
            end if
            start = start + pos - 1 + dlen
        end do
    end subroutine split

    function join(parts, n_parts, delimiter) result(res)
        character(len=256), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=*), intent(in) :: delimiter
        character(len=:), allocatable :: res
        integer :: i
        res = ''
        do i = 1, n_parts
            if (i > 1) res = res // delimiter
            res = res // trim(parts(i))
        end do
    end function join

    logical function starts_with(s, prefix)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: prefix
        integer :: n
        n = len(prefix)
        if (n == 0) then
            starts_with = .true.
        else if (len(s) < n) then
            starts_with = .false.
        else
            starts_with = s(1:n) == prefix
        end if
    end function starts_with

    logical function ends_with(s, suffix)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: suffix
        integer :: n, m
        n = len(suffix)
        m = len(s)
        if (n == 0) then
            ends_with = .true.
        else if (m < n) then
            ends_with = .false.
        else
            ends_with = s(m - n + 1:m) == suffix
        end if
    end function ends_with

    logical function contains_str(s, substr)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: substr
        contains_str = index(s, substr) > 0
    end function contains_str

    function replace_str(s, old, new) result(res)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: old
        character(len=*), intent(in) :: new
        character(len=:), allocatable :: res
        integer :: pos, start, olen
        olen = len(old)
        res = ''
        if (olen == 0) then
            res = s
            return
        end if
        start = 1
        do
            pos = index(s(start:), old)
            if (pos == 0) then
                res = res // s(start:)
                exit
            end if
            res = res // s(start:start + pos - 2) // new
            start = start + pos - 1 + olen
            if (start > len(s)) exit
        end do
    end function replace_str

    integer function find_str(s, substr, start)
        character(len=*), intent(in) :: s
        character(len=*), intent(in) :: substr
        integer, intent(in) :: start
        integer :: pos
        if (start < 1 .or. start > len(s)) then
            find_str = 0
            return
        end if
        pos = index(s(start:), substr)
        if (pos == 0) then
            find_str = 0
        else
            find_str = start + pos - 1
        end if
    end function find_str

    function strip(s) result(res)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: res
        integer :: i, j
        i = 1
        j = len(s)
        do while (i <= j)
            if (s(i:i) == ' ' .or. s(i:i) == achar(9) .or. &
                s(i:i) == achar(10) .or. s(i:i) == achar(13)) then
                i = i + 1
            else
                exit
            end if
        end do
        do while (j >= i)
            if (s(j:j) == ' ' .or. s(j:j) == achar(9) .or. &
                s(j:j) == achar(10) .or. s(j:j) == achar(13)) then
                j = j - 1
            else
                exit
            end if
        end do
        if (i > j) then
            res = ''
        else
            res = s(i:j)
        end if
    end function strip

    function repeat_str(s, n) result(res)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: res
        integer :: i
        res = ''
        do i = 1, n
            res = res // s
        end do
    end function repeat_str

    integer function utf8_len(s)
        character(len=*), intent(in) :: s
        integer :: i
        utf8_len = 0
        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) utf8_len = utf8_len + 1
        end do
    end function utf8_len

end module fx_string
