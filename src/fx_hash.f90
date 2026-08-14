module fx_hash
    use, intrinsic :: iso_c_binding, only: c_char, c_int
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

    type, public :: sha256_state_t
        !! Block-buffered SHA-256 streaming state. Holds only the 64-byte
        !! pending block plus the running digest state, never the whole
        !! message, so feeding a digest never grows or reallocates a buffer.
        !! The digest path runs from OpenMP parallel regions (fo's link /
        !! compile loops), where a growing shared allocatable is the classic
        !! realloc data race; a fixed-size state has no allocation to race on.
        integer :: h(8) = 0
        character(len=1) :: buf(64) = ''
        integer :: nbuf = 0
        integer(int64) :: total = 0_int64
        logical :: initialized = .false.
    end type sha256_state_t

    public :: fnv1a, fnv1a_string, fnv1a_file
    public :: xxhash64, xxhash64_file
    public :: sha256, sha256_bytes, sha256_string, sha256_file
    public :: sha256_init, sha256_update, sha256_final
    public :: sha256_hardware_available, sha256_hardware_digest
    public :: hash_to_hex, hash_combine
    public :: hash_state_init, hash_state_update, hash_state_final

    interface
        integer(c_int) function fx_c_sha256_available() bind(C)
            import :: c_int
        end function fx_c_sha256_available

        integer(c_int) function fx_c_sha256_digest(data, n, out_hex) bind(C)
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: data(*)
            integer(c_int), value :: n
            character(kind=c_char), intent(out) :: out_hex(*)
        end function fx_c_sha256_digest
    end interface

    integer, save :: sha256_backend = -1
    !$omp threadprivate (sha256_backend)

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

        allocate (bytes(len(s)))
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
            hash = rotl64(hash, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
            idx = idx + 8
        end do

        if (idx <= n - 3) then
            hash = ieor(hash, int(xxh64_read_u32(data, idx), int64)* &
                XXH_PRIME64_1)
            hash = rotl64(hash, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
            idx = idx + 4
        end if

        do while (idx <= n)
            hash = ieor(hash, int(iachar(data(idx)), int64)*XXH_PRIME64_5)
            hash = rotl64(hash, 11)*XXH_PRIME64_1
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

    function sha256(data, n) result(hex)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        character(len=64) :: hex

        hex = sha256_bytes(data, n)
    end function sha256

    function sha256_bytes(data, n) result(hex)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        character(len=64) :: hex

        if (sha256_hardware_digest(data, n, hex)) return
        hex = sha256_scalar(data, n)
    end function sha256_bytes

    function sha256_scalar(data, n) result(hex)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        character(len=64) :: hex

        integer :: h(8), padded_len
        character(len=1), allocatable :: msg(:)
        integer :: bit_len_hi, bit_len_lo
        integer :: offset

        h = [int(z'6a09e667'), int(z'bb67ae85'), int(z'3c6ef372'), &
            int(z'a54ff53a'), int(z'510e527f'), int(z'9b05688c'), &
            int(z'1f83d9ab'), int(z'5be0cd19')]

        padded_len = ((n + 9 + 63)/64)*64
        allocate (msg(padded_len))
        msg = achar(0)
        if (n > 0) msg(1:n) = data(1:n)
        msg(n + 1) = achar(128)
        bit_len_hi = int(ishft(int(n, int64)*8_int64, -32))
        bit_len_lo = int(iand(int(n, int64)*8_int64, int(z'ffffffff', int64)))
        call put_u32_be(msg, padded_len - 7, bit_len_hi)
        call put_u32_be(msg, padded_len - 3, bit_len_lo)

        do offset = 1, padded_len, 64
            call sha256_block(msg(offset:offset + 63), h)
        end do

        hex = ''
        do offset = 1, 8
            hex((offset - 1)*8 + 1:offset*8) = u32_to_hex(h(offset))
        end do
    end function sha256_scalar

    function sha256_string(s) result(hex)
        character(len=*), intent(in) :: s
        character(len=64) :: hex

        character(len=1), allocatable :: bytes(:)
        integer :: i

        allocate (bytes(len(s)))
        do i = 1, len(s)
            bytes(i) = s(i:i)
        end do
        hex = sha256_bytes(bytes, len(s))
    end function sha256_string

    subroutine sha256_file(path, hex, ierr)
        character(len=*), intent(in) :: path
        character(len=64), intent(out) :: hex
        integer, intent(out) :: ierr

        character(len=1), allocatable :: bytes(:)
        integer :: n_bytes

        call load_file_bytes(path, bytes, n_bytes, ierr)
        if (ierr /= 0) then
            hex = ''
            return
        end if
        hex = sha256_bytes(bytes, n_bytes)
    end subroutine sha256_file

    subroutine sha256_init(state)
        type(sha256_state_t), intent(out) :: state

        state%h = [int(z'6a09e667'), int(z'bb67ae85'), int(z'3c6ef372'), &
            int(z'a54ff53a'), int(z'510e527f'), int(z'9b05688c'), &
            int(z'1f83d9ab'), int(z'5be0cd19')]
        state%buf = ''
        state%nbuf = 0
        state%total = 0_int64
        state%initialized = .true.
    end subroutine sha256_init

    subroutine sha256_update(state, data, n)
        type(sha256_state_t), intent(inout) :: state
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)

        integer :: i, take

        if (.not. state%initialized) call sha256_init(state)
        if (n <= 0) return

        state%total = state%total + int(n, int64)
        i = 1
        do while (i <= n)
            take = min(64 - state%nbuf, n - i + 1)
            state%buf(state%nbuf + 1:state%nbuf + take) = data(i:i + take - 1)
            state%nbuf = state%nbuf + take
            i = i + take
            if (state%nbuf == 64) then
                call sha256_block(state%buf, state%h)
                state%nbuf = 0
            end if
        end do
    end subroutine sha256_update

    function sha256_final(state) result(hex)
        type(sha256_state_t), intent(in) :: state
        character(len=64) :: hex

        type(sha256_state_t) :: s
        integer :: i
        integer(int64) :: bit_len

        if (.not. state%initialized) then
            hex = sha256_string('')
            return
        end if

        s = state
        bit_len = s%total*8_int64
        s%nbuf = s%nbuf + 1
        s%buf(s%nbuf) = achar(128)
        if (s%nbuf > 56) then
            do i = s%nbuf + 1, 64
                s%buf(i) = achar(0)
            end do
            call sha256_block(s%buf, s%h)
            s%nbuf = 0
        end if
        do i = s%nbuf + 1, 56
            s%buf(i) = achar(0)
        end do
        do i = 1, 8
            s%buf(56 + i) = &
                achar(int(iand(ishft(bit_len, -8*(8 - i)), 255_int64)))
        end do
        call sha256_block(s%buf, s%h)

        hex = ''
        do i = 1, 8
            hex((i - 1)*8 + 1:i*8) = u32_to_hex(s%h(i))
        end do
    end function sha256_final

    logical function sha256_hardware_available() result(available)
        call sha256_select_backend()
        available = sha256_backend == 1
    end function sha256_hardware_available

    logical function sha256_hardware_digest(data, n, hex) result(ok)
        integer, intent(in) :: n
        character(len=1), intent(in) :: data(n)
        character(len=64), intent(out) :: hex

        character(kind=c_char), allocatable :: c_data(:)
        character(kind=c_char) :: c_hex(64)
        integer(c_int) :: status
        integer :: i

        call sha256_select_backend()
        ok = .false.
        hex = ''
        if (sha256_backend /= 1) return
        allocate (c_data(max(n, 1)))
        do i = 1, n
            c_data(i) = data(i)
        end do
        status = fx_c_sha256_digest(c_data, int(n, c_int), c_hex)
        if (status /= 0_c_int) return
        do i = 1, 64
            hex(i:i) = c_hex(i)
        end do
        ok = .true.
    end function sha256_hardware_digest

    subroutine sha256_select_backend()
        if (sha256_backend >= 0) return
        if (fx_c_sha256_available() == 1_c_int) then
            sha256_backend = 1
        else
            sha256_backend = 0
        end if
    end subroutine sha256_select_backend

    subroutine sha256_block(block, h)
        character(len=1), intent(in) :: block(64)
        integer, intent(inout) :: h(8)
        integer, parameter :: k(64) = [ &
            int(z'428a2f98'), int(z'71374491'), int(z'b5c0fbcf'), int(z'e9b5dba5'), &
            int(z'3956c25b'), int(z'59f111f1'), int(z'923f82a4'), int(z'ab1c5ed5'), &
            int(z'd807aa98'), int(z'12835b01'), int(z'243185be'), int(z'550c7dc3'), &
            int(z'72be5d74'), int(z'80deb1fe'), int(z'9bdc06a7'), int(z'c19bf174'), &
            int(z'e49b69c1'), int(z'efbe4786'), int(z'0fc19dc6'), int(z'240ca1cc'), &
            int(z'2de92c6f'), int(z'4a7484aa'), int(z'5cb0a9dc'), int(z'76f988da'), &
            int(z'983e5152'), int(z'a831c66d'), int(z'b00327c8'), int(z'bf597fc7'), &
            int(z'c6e00bf3'), int(z'd5a79147'), int(z'06ca6351'), int(z'14292967'), &
            int(z'27b70a85'), int(z'2e1b2138'), int(z'4d2c6dfc'), int(z'53380d13'), &
            int(z'650a7354'), int(z'766a0abb'), int(z'81c2c92e'), int(z'92722c85'), &
            int(z'a2bfe8a1'), int(z'a81a664b'), int(z'c24b8b70'), int(z'c76c51a3'), &
            int(z'd192e819'), int(z'd6990624'), int(z'f40e3585'), int(z'106aa070'), &
            int(z'19a4c116'), int(z'1e376c08'), int(z'2748774c'), int(z'34b0bcb5'), &
            int(z'391c0cb3'), int(z'4ed8aa4a'), int(z'5b9cca4f'), int(z'682e6ff3'), &
            int(z'748f82ee'), int(z'78a5636f'), int(z'84c87814'), int(z'8cc70208'), &
            int(z'90befffa'), int(z'a4506ceb'), int(z'bef9a3f7'), int(z'c67178f2')]
        integer :: w(64), a, b, c, d, e, f, g, hh, i, t1, t2

        do i = 1, 16
            w(i) = read_u32_be(block, (i - 1)*4 + 1)
        end do
        do i = 17, 64
            w(i) = sha_small1(w(i - 2)) + w(i - 7) + sha_small0(w(i - 15)) + w(i - 16)
        end do

        a = h(1); b = h(2); c = h(3); d = h(4)
        e = h(5); f = h(6); g = h(7); hh = h(8)
        do i = 1, 64
            t1 = hh + sha_big1(e) + sha_ch(e, f, g) + k(i) + w(i)
            t2 = sha_big0(a) + sha_maj(a, b, c)
            hh = g; g = f; f = e; e = d + t1
            d = c; c = b; b = a; a = t1 + t2
        end do
        h(1) = h(1) + a; h(2) = h(2) + b; h(3) = h(3) + c; h(4) = h(4) + d
        h(5) = h(5) + e; h(6) = h(6) + f; h(7) = h(7) + g; h(8) = h(8) + hh
    end subroutine sha256_block

    function hash_to_hex(hash) result(hex)
        integer(int64), intent(in) :: hash
        character(len=16) :: hex
        character(len=*), parameter :: digits = '0123456789abcdef'
        integer :: i
        integer :: shift
        integer :: nibble

        do i = 1, 16
            shift = (16 - i)*4
            nibble = int(iand(ishft(hash, -shift), 15_int64))
            hex(i:i) = digits(nibble + 1:nibble + 1)
        end do
    end function hash_to_hex

    pure integer function sha_rotr(x, n) result(out)
        integer, intent(in) :: x, n
        out = ior(ishft(x, -n), ishft(x, 32 - n))
    end function sha_rotr

    pure integer function sha_big0(x) result(out)
        integer, intent(in) :: x
        out = ieor(ieor(sha_rotr(x, 2), sha_rotr(x, 13)), sha_rotr(x, 22))
    end function sha_big0

    pure integer function sha_big1(x) result(out)
        integer, intent(in) :: x
        out = ieor(ieor(sha_rotr(x, 6), sha_rotr(x, 11)), sha_rotr(x, 25))
    end function sha_big1

    pure integer function sha_small0(x) result(out)
        integer, intent(in) :: x
        out = ieor(ieor(sha_rotr(x, 7), sha_rotr(x, 18)), ishft(x, -3))
    end function sha_small0

    pure integer function sha_small1(x) result(out)
        integer, intent(in) :: x
        out = ieor(ieor(sha_rotr(x, 17), sha_rotr(x, 19)), ishft(x, -10))
    end function sha_small1

    pure integer function sha_ch(x, y, z) result(out)
        integer, intent(in) :: x, y, z
        out = ieor(iand(x, y), iand(not(x), z))
    end function sha_ch

    pure integer function sha_maj(x, y, z) result(out)
        integer, intent(in) :: x, y, z
        out = ieor(ieor(iand(x, y), iand(x, z)), iand(y, z))
    end function sha_maj

    pure integer function read_u32_be(data, idx) result(value)
        character(len=1), intent(in) :: data(:)
        integer, intent(in) :: idx
        value = ior(ior(ishft(iachar(data(idx)), 24), &
            ishft(iachar(data(idx + 1)), 16)), &
            ior(ishft(iachar(data(idx + 2)), 8), &
            iachar(data(idx + 3))))
    end function read_u32_be

    pure subroutine put_u32_be(data, idx, value)
        character(len=1), intent(inout) :: data(:)
        integer, intent(in) :: idx, value
        data(idx) = achar(iand(ishft(value, -24), 255))
        data(idx + 1) = achar(iand(ishft(value, -16), 255))
        data(idx + 2) = achar(iand(ishft(value, -8), 255))
        data(idx + 3) = achar(iand(value, 255))
    end subroutine put_u32_be

    pure function u32_to_hex(value) result(hex)
        integer, intent(in) :: value
        character(len=8) :: hex
        character(len=*), parameter :: digits = '0123456789abcdef'
        integer :: i, nibble
        do i = 1, 8
            nibble = iand(ishft(value, -((8 - i)*4)), 15)
            hex(i:i) = digits(nibble + 1:nibble + 1)
        end do
    end function u32_to_hex

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
            hash = hash*FNV_PRIME
        end do
    end subroutine fnv1a_update

    pure integer(int64) function xxh64_round(acc, input) result(out)
        integer(int64), intent(in) :: acc
        integer(int64), intent(in) :: input

        out = acc + input*XXH_PRIME64_2
        out = rotl64(out, 31)
        out = out*XXH_PRIME64_1
    end function xxh64_round

    pure integer(int64) function xxh64_merge_round(hash, acc) result(out)
        integer(int64), intent(in) :: hash
        integer(int64), intent(in) :: acc

        out = ieor(hash, xxh64_round(0_int64, acc))
        out = out*XXH_PRIME64_1 + XXH_PRIME64_4
    end function xxh64_merge_round

    pure integer(int64) function xxh64_avalanche(hash) result(out)
        integer(int64), intent(in) :: hash

        out = hash
        out = ieor(out, ishft(out, -33))
        out = out*XXH_PRIME64_2
        out = ieor(out, ishft(out, -29))
        out = out*XXH_PRIME64_3
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
            shift = i*8
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
            shift = i*8
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

        inquire (file=trim(path), exist=exists)
        if (.not. exists) then
            ierr = 1
            n_bytes = 0
            allocate (bytes(0))
            return
        end if

        inquire (file=trim(path), size=n_bytes)
        if (n_bytes < 0) then
            ierr = 1
            allocate (bytes(0))
            n_bytes = 0
            return
        end if

        allocate (bytes(max(n_bytes, 0)))
        open (newunit=unit, file=trim(path), access='stream', &
            form='unformatted', status='old', action='read', &
            iostat=ios)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate (bytes)
            allocate (bytes(0))
            n_bytes = 0
            return
        end if

        if (n_bytes > 0) then
            read (unit, iostat=ios) bytes(1:n_bytes)
        end if
        close (unit)
        if (ios /= 0) then
            ierr = 1
            if (allocated(bytes)) deallocate (bytes)
            allocate (bytes(0))
            n_bytes = 0
            return
        end if

        ierr = 0
    end subroutine load_file_bytes

end module fx_hash
