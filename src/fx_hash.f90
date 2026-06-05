module fx_hash
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    private

    integer(int64), parameter :: FNV_OFFSET = -3750763034362895579_int64
    integer(int64), parameter :: FNV_PRIME = 1099511628211_int64

    integer(int64), parameter :: XXH_PRIME64_1 = -7046029288634856825_int64
    integer(int64), parameter :: XXH_PRIME64_2 = -4417276706812531889_int64
    integer(int64), parameter :: XXH_PRIME64_3 = 1609587929392839161_int64
    integer(int64), parameter :: XXH_PRIME64_4 = -8796714831421723037_int64
    integer(int64), parameter :: XXH_PRIME64_5 = 2870177450012600261_int64

    type, public :: hash_state_t
        integer(int64) :: hash = 0_int64
        logical :: initialized = .false.
    end type hash_state_t

    public :: fnv1a, fnv1a_string, fnv1a_file
    public :: xxhash64, xxhash64_file
    public :: hash_to_hex, hash_combine
    public :: hash_state_init, hash_state_update, hash_state_final

contains

    function fnv1a(data, n) result(hash)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        integer(int64) :: hash

        hash = FNV_OFFSET
        call fnv1a_update(hash, data, n)
    end function fnv1a

    function fnv1a_string(s) result(hash)
        character(len=*), intent(in) :: s
        integer(int64) :: hash
        character(len=1), allocatable :: bytes(:)
        integer :: i

        allocate(bytes(len(s)))
        do i = 1, len(s)
            bytes(i) = s(i:i)
        end do
        hash = fnv1a(bytes, size(bytes))
    end function fnv1a_string

    subroutine fnv1a_file(path, hash, ierr)
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: hash
        integer, intent(out) :: ierr
        character(len=1), allocatable :: bytes(:)
        integer :: n_bytes

        call load_file_bytes(path, bytes, n_bytes, ierr)
        if (ierr /= 0) then
            hash = 0_int64
            return
        end if

        hash = fnv1a(bytes, n_bytes)
    end subroutine fnv1a_file

    function xxhash64(data, n, seed) result(hash)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        integer(int64), intent(in) :: seed
        integer(int64) :: hash
        integer(int64) :: v1, v2, v3, v4
        integer(int64) :: lane
        integer :: idx

        if (n >= 32) then
            v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2
            v2 = seed + XXH_PRIME64_2
            v3 = seed
            v4 = seed - XXH_PRIME64_1
            idx = 1

            do while (idx <= n - 31)
                v1 = xxh64_round(v1, xxh64_read_u64(data, idx))
                idx = idx + 8
                v2 = xxh64_round(v2, xxh64_read_u64(data, idx))
                idx = idx + 8
                v3 = xxh64_round(v3, xxh64_read_u64(data, idx))
                idx = idx + 8
                v4 = xxh64_round(v4, xxh64_read_u64(data, idx))
                idx = idx + 8
            end do

            hash = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + &
                   rotl64(v4, 18)
            hash = xxh64_merge_round(hash, v1)
            hash = xxh64_merge_round(hash, v2)
            hash = xxh64_merge_round(hash, v3)
            hash = xxh64_merge_round(hash, v4)
        else
            hash = seed + XXH_PRIME64_5
            idx = 1
        end if

        hash = hash + int(n, int64)

        do while (idx <= n - 7)
            lane = xxh64_round(0_int64, xxh64_read_u64(data, idx))
            hash = ieor(hash, lane)
            hash = rotl64(hash, 27) * XXH_PRIME64_1 + XXH_PRIME64_4
            idx = idx + 8
        end do

        if (idx <= n - 3) then
            hash = ieor(hash, int(xxh64_read_u32(data, idx), int64) * &
                        XXH_PRIME64_1)
            hash = rotl64(hash, 23) * XXH_PRIME64_2 + XXH_PRIME64_3
            idx = idx + 4
        end if

        do while (idx <= n)
            hash = ieor(hash, int(iachar(data(idx)), int64) * XXH_PRIME64_5)
            hash = rotl64(hash, 11) * XXH_PRIME64_1
            idx = idx + 1
        end do

        hash = xxh64_avalanche(hash)
    end function xxhash64

    subroutine xxhash64_file(path, hash, ierr)
        character(len=*), intent(in) :: path
        integer(int64), intent(out) :: hash
        integer, intent(out) :: ierr
        character(len=1), allocatable :: bytes(:)
        integer :: n_bytes

        call load_file_bytes(path, bytes, n_bytes, ierr)
        if (ierr /= 0) then
            hash = 0_int64
            return
        end if

        hash = xxhash64(bytes, n_bytes, 0_int64)
    end subroutine xxhash64_file

    function hash_to_hex(hash) result(hex)
        integer(int64), intent(in) :: hash
        character(len=16) :: hex
        character(len=*), parameter :: digits = '0123456789abcdef'
        integer :: i
        integer :: shift
        integer :: nibble

        do i = 1, 16
            shift = (16 - i) * 4
            nibble = int(iand(ishft(hash, -shift), 15_int64))
            hex(i:i) = digits(nibble + 1:nibble + 1)
        end do
    end function hash_to_hex

    function hash_combine(h1, h2) result(combined)
        integer(int64), intent(in) :: h1
        integer(int64), intent(in) :: h2
        integer(int64) :: combined

        combined = ieor(h1, h2 + 2654435769_int64 + ishft(h1, 6) + &
                        ishft(h1, -2))
    end function hash_combine

    subroutine hash_state_init(state)
        type(hash_state_t), intent(out) :: state

        state%hash = FNV_OFFSET
        state%initialized = .true.
    end subroutine hash_state_init

    subroutine hash_state_update(state, data, n)
        type(hash_state_t), intent(inout) :: state
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)

        if (.not. state%initialized) call hash_state_init(state)
        call fnv1a_update(state%hash, data, n)
    end subroutine hash_state_update

    function hash_state_final(state) result(hash)
        type(hash_state_t), intent(in) :: state
        integer(int64) :: hash

        if (state%initialized) then
            hash = state%hash
        else
            hash = FNV_OFFSET
        end if
    end function hash_state_final

    pure subroutine fnv1a_update(hash, data, n)
        integer(int64), intent(inout) :: hash
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        integer :: i

        do i = 1, n
            hash = ieor(hash, int(iachar(data(i)), int64))
            hash = hash * FNV_PRIME
        end do
    end subroutine fnv1a_update

    pure integer(int64) function xxh64_round(acc, input) result(out)
        integer(int64), intent(in) :: acc
        integer(int64), intent(in) :: input

        out = acc + input * XXH_PRIME64_2
        out = rotl64(out, 31)
        out = out * XXH_PRIME64_1
    end function xxh64_round

    pure integer(int64) function xxh64_merge_round(hash, acc) result(out)
        integer(int64), intent(in) :: hash
        integer(int64), intent(in) :: acc

        out = ieor(hash, xxh64_round(0_int64, acc))
        out = out * XXH_PRIME64_1 + XXH_PRIME64_4
    end function xxh64_merge_round

    pure integer(int64) function xxh64_avalanche(hash) result(out)
        integer(int64), intent(in) :: hash

        out = hash
        out = ieor(out, ishft(out, -33))
        out = out * XXH_PRIME64_2
        out = ieor(out, ishft(out, -29))
        out = out * XXH_PRIME64_3
        out = ieor(out, ishft(out, -32))
    end function xxh64_avalanche

    pure integer(int64) function rotl64(x, r) result(out)
        integer(int64), intent(in) :: x
        integer, intent(in) :: r
        integer :: shift

        shift = mod(r, 64)
        if (shift == 0) then
            out = x
        else
            out = ior(ishft(x, shift), ishft(x, shift - 64))
        end if
    end function rotl64

    pure integer(int64) function xxh64_read_u64(data, idx) result(value)
        character(len=1), intent(in) :: data(:)
        integer, intent(in) :: idx
        integer :: i
        integer :: shift

        value = 0_int64
        do i = 0, 7
            shift = i * 8
            value = ior(value, ishft(int(iachar(data(idx + i)), int64), shift))
        end do
    end function xxh64_read_u64

    pure integer(int64) function xxh64_read_u32(data, idx) result(value)
        character(len=1), intent(in) :: data(:)
        integer, intent(in) :: idx
        integer :: i
        integer :: shift

        value = 0_int64
        do i = 0, 3
            shift = i * 8
            value = ior(value, ishft(int(iachar(data(idx + i)), int64), shift))
        end do
    end function xxh64_read_u32

    subroutine load_file_bytes(path, bytes, n_bytes, ierr)
        character(len=*), intent(in) :: path
        character(len=1), allocatable, intent(out) :: bytes(:)
        integer, intent(out) :: n_bytes
        integer, intent(out) :: ierr
        integer :: unit
        integer :: ios
        logical :: exists

        inquire(file=trim(path), exist=exists)
        if (.not. exists) then
            ierr = 1
            n_bytes = 0
            allocate(bytes(0))
            return
        end if

        inquire(file=trim(path), size=n_bytes)
        if (n_bytes < 0) then
            ierr = 1
            allocate(bytes(0))
            n_bytes = 0
            return
        end if

        allocate(bytes(max(n_bytes, 0)))
        open(newunit=unit, file=trim(path), access='stream', &
             form='unformatted', status='old', action='read', &
             iostat=ios)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate(bytes)
            allocate(bytes(0))
            n_bytes = 0
            return
        end if

        if (n_bytes > 0) then
            read(unit, iostat=ios) bytes(1:n_bytes)
        end if
        close(unit)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate(bytes)
            allocate(bytes(0))
            n_bytes = 0
            return
        end if

        ierr = 0
    end subroutine load_file_bytes

end module fx_hash
