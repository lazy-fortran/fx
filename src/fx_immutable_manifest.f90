module fx_immutable_manifest
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_INVALID, &
        IMMUTABLE_CORRUPT
    implicit none
    private

    integer, parameter, public :: IMMUTABLE_BLOB = 1
    integer, parameter, public :: IMMUTABLE_TREE = 2
    integer, parameter :: HASH_LEN = 64
    ! Each entry names one normalized relative component. Nested directories
    ! are represented by entries of type TREE, so traversal is never encoded.
    ! FXTREE1 bytes bind the schema version, type, permission mode, role, path,
    ! and referenced raw-byte ID into the tree digest.
    character(len=*), parameter :: TREE_HEADER = 'FXTREE1'//achar(10)

    type, public :: immutable_tree_entry_t
        character(len=:), allocatable :: path
        character(len=:), allocatable :: role
        character(len=HASH_LEN) :: object_id = ''
        integer :: mode = 0
        integer :: kind = IMMUTABLE_BLOB
    end type immutable_tree_entry_t

    public :: immutable_entries_canonical, immutable_manifest_serialize
    public :: immutable_manifest_encode
    public :: immutable_manifest_parse, immutable_id_valid

    interface
        integer(c_int) function name_valid(name) bind(C, name='fx_immutable_name_valid')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name(*)
        end function name_valid
        integer(c_int) function names_equivalent(left, right) &
                bind(C, name='fx_immutable_names_equivalent')
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: left(*), right(*)
        end function names_equivalent
    end interface

contains

    subroutine immutable_entries_canonical(input, output, ierr)
        type(immutable_tree_entry_t), intent(in) :: input(:)
        type(immutable_tree_entry_t), allocatable, intent(out) :: output(:)
        integer, intent(out) :: ierr
        integer :: i, j

        allocate(output(size(input)))
        output = input
        call sort_entries(output, 1, size(output))
        ierr = IMMUTABLE_INVALID
        do i = 1, size(output)
            if (.not. valid_component(output(i)%path)) return
            if (name_valid(output(i)%path//c_null_char) /= 1_c_int) return
            if (.not. valid_role(output(i)%role)) return
            if (.not. immutable_id_valid(output(i)%object_id)) return
            if (output(i)%mode < 0 .or. output(i)%mode > 511) return
            if (output(i)%kind /= IMMUTABLE_BLOB .and. &
                output(i)%kind /= IMMUTABLE_TREE) return
            do j = 1, i - 1
                if (names_equivalent(output(j)%path//c_null_char, &
                    output(i)%path//c_null_char) /= 0_c_int) return
            end do
        end do
        ierr = IMMUTABLE_OK
    end subroutine immutable_entries_canonical

    recursive subroutine sort_entries(entries, first, last)
        type(immutable_tree_entry_t), intent(inout) :: entries(:)
        integer, intent(in) :: first, last
        type(immutable_tree_entry_t) :: pivot, swap
        integer :: left, right

        if (first >= last) return
        pivot = entries((first + last)/2)
        left = first
        right = last
        do while (left <= right)
            do while (byte_less(entries(left)%path, pivot%path))
                left = left + 1
            end do
            do while (byte_less(pivot%path, entries(right)%path))
                right = right - 1
            end do
            if (left <= right) then
                swap = entries(left)
                entries(left) = entries(right)
                entries(right) = swap
                left = left + 1
                right = right - 1
            end if
        end do
        if (first < right) call sort_entries(entries, first, right)
        if (left < last) call sort_entries(entries, left, last)
    end subroutine sort_entries

    function immutable_manifest_serialize(entries) result(text)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=:), allocatable :: text

        call immutable_manifest_encode(entries, text)
    end function immutable_manifest_serialize

    subroutine immutable_manifest_encode(entries, text)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=:), allocatable, intent(out) :: text
        character(len=3) :: mode_text
        character(len=1) :: kind_text
        integer :: i, n, pos

        ! Caller-owned length metadata stays private during concurrent encoding.
        n = len(TREE_HEADER)
        do i = 1, size(entries)
            n = n + 73 + len(entries(i)%role) + len(entries(i)%path)
        end do
        allocate(character(len=n) :: text)
        text(1:len(TREE_HEADER)) = TREE_HEADER
        pos = len(TREE_HEADER) + 1
        do i = 1, size(entries)
            kind_text = 'B'
            if (entries(i)%kind == IMMUTABLE_TREE) kind_text = 'T'
            mode_text = mode_octal(entries(i)%mode)
            call append_text(text, pos, kind_text)
            call append_text(text, pos, achar(9))
            call append_text(text, pos, mode_text)
            call append_text(text, pos, achar(9))
            call append_text(text, pos, entries(i)%role)
            call append_text(text, pos, achar(9))
            call append_text(text, pos, entries(i)%path)
            call append_text(text, pos, achar(9))
            call append_text(text, pos, entries(i)%object_id)
            call append_text(text, pos, achar(10))
        end do
    end subroutine immutable_manifest_encode

    subroutine immutable_manifest_parse(text, entries, ierr)
        character(len=*), intent(in) :: text
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: ierr
        type(immutable_tree_entry_t), allocatable :: parsed(:), sorted(:)
        character(len=:), allocatable :: canonical
        character(len=:), allocatable :: field(:)
        integer :: n, i, start, stop, line_end, mode

        ierr = IMMUTABLE_CORRUPT
        if (len(text) < len(TREE_HEADER)) return
        if (text(1:len(TREE_HEADER)) /= TREE_HEADER) return
        if (text(len(text):len(text)) /= achar(10)) return
        n = 0
        do i = len(TREE_HEADER) + 1, len(text)
            if (text(i:i) == achar(10)) n = n + 1
        end do
        allocate(parsed(n))
        start = len(TREE_HEADER) + 1
        do i = 1, n
            line_end = index(text(start:), achar(10)) + start - 1
            if (line_end <= start) return
            stop = line_end - 1
            call split_manifest_line(text(start:stop), field, ierr)
            if (ierr /= IMMUTABLE_OK) return
            if (len_trim(field(1)) /= 1) then
                ierr = IMMUTABLE_CORRUPT
                return
            end if
            if (len_trim(field(2)) /= 3) then
                ierr = IMMUTABLE_CORRUPT
                return
            end if
            if (len_trim(field(5)) /= HASH_LEN) then
                ierr = IMMUTABLE_CORRUPT
                return
            end if
            if (field(1) == 'B') then
                parsed(i)%kind = IMMUTABLE_BLOB
            else if (field(1) == 'T') then
                parsed(i)%kind = IMMUTABLE_TREE
            else
                ierr = IMMUTABLE_CORRUPT
                return
            end if
            call parse_octal(field(2), mode, ierr)
            if (ierr /= IMMUTABLE_OK) return
            parsed(i)%mode = mode
            parsed(i)%role = trim(field(3))
            parsed(i)%path = trim(field(4))
            parsed(i)%object_id = trim(field(5))
            start = line_end + 1
        end do
        if (start /= len(text) + 1) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        call immutable_entries_canonical(parsed, sorted, ierr)
        if (ierr /= IMMUTABLE_OK) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        call immutable_manifest_encode(sorted, canonical)
        if (canonical /= text) then
            ierr = IMMUTABLE_CORRUPT
            return
        end if
        allocate(entries(size(sorted)))
        entries = sorted
        ierr = IMMUTABLE_OK
    end subroutine immutable_manifest_parse

    subroutine split_manifest_line(line, fields, ierr)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: fields(:)
        integer, intent(out) :: ierr
        integer :: tabs(4), i, n, start, f

        ierr = IMMUTABLE_CORRUPT
        n = 0
        do i = 1, len(line)
            if (line(i:i) == achar(9)) then
                n = n + 1
                if (n > 4) return
                tabs(n) = i
            end if
        end do
        if (n /= 4) return
        allocate(character(len=len(line)) :: fields(5))
        fields = ''
        start = 1
        do f = 1, 4
            if (tabs(f) <= start) return
            fields(f) = line(start:tabs(f) - 1)
            start = tabs(f) + 1
        end do
        if (start > len(line)) return
        fields(5) = line(start:)
        ierr = IMMUTABLE_OK
    end subroutine split_manifest_line

    subroutine append_text(target, pos, piece)
        character(len=*), intent(inout) :: target
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: piece
        integer :: n
        n = len(piece)
        if (n > 0) target(pos:pos + n - 1) = piece
        pos = pos + n
    end subroutine append_text

    function mode_octal(mode) result(text)
        integer, intent(in) :: mode
        character(len=3) :: text
        integer :: value, i
        value = mode
        do i = 3, 1, -1
            text(i:i) = achar(iachar('0') + mod(value, 8))
            value = value/8
        end do
    end function mode_octal

    subroutine parse_octal(text, mode, ierr)
        character(len=*), intent(in) :: text
        integer, intent(out) :: mode, ierr
        integer :: i, digit
        mode = 0
        ierr = IMMUTABLE_CORRUPT
        if (len_trim(text) /= 3) return
        do i = 1, 3
            digit = iachar(text(i:i)) - iachar('0')
            if (digit < 0 .or. digit > 7) return
            mode = mode*8 + digit
        end do
        ierr = IMMUTABLE_OK
    end subroutine parse_octal

    logical function immutable_id_valid(text)
        character(len=*), intent(in) :: text
        integer :: i, c
        immutable_id_valid = .false.
        if (len(text) /= HASH_LEN) return
        do i = 1, HASH_LEN
            c = iachar(text(i:i))
            if (.not. ((c >= iachar('0') .and. c <= iachar('9')) .or. &
                (c >= iachar('a') .and. c <= iachar('f')))) return
        end do
        immutable_id_valid = .true.
    end function immutable_id_valid

    logical function valid_role(role)
        character(len=*), intent(in) :: role
        integer :: i, c
        valid_role = .false.
        if (len(role) == 0) return
        do i = 1, len(role)
            c = iachar(role(i:i))
            if (.not. ((c >= iachar('a') .and. c <= iachar('z')) .or. &
                (c >= iachar('0') .and. c <= iachar('9')) .or. &
                index('._-', role(i:i)) > 0)) return
        end do
        valid_role = .true.
    end function valid_role

    logical function valid_component(path)
        character(len=*), intent(in) :: path
        integer :: i, c
        valid_component = .false.
        if (len(path) == 0) return
        if (path == '.' .or. path == '..') return
        if (len_trim(path) /= len(path)) return
        if (path(1:1) == ' ') return
        if (index(path, '/') > 0 .or. index(path, achar(92)) > 0) return
        do i = 1, len(path)
            c = iachar(path(i:i))
            if (c < 32 .or. c == 127) return
        end do
        valid_component = .true.
    end function valid_component

    logical function byte_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: i, common
        common = min(len(a), len(b))
        byte_less = .false.
        do i = 1, common
            if (iachar(a(i:i)) == iachar(b(i:i))) cycle
            byte_less = iachar(a(i:i)) < iachar(b(i:i))
            return
        end do
        byte_less = len(a) < len(b)
    end function byte_less

    logical function same_bytes(a, b)
        character(len=*), intent(in) :: a, b
        same_bytes = len(a) == len(b)
        if (same_bytes) same_bytes = .not. byte_less(a, b) .and. &
            .not. byte_less(b, a)
    end function same_bytes


end module fx_immutable_manifest
