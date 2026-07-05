module fx_cache_key
    use fx_hash, only: sha256_string, sha256_file
    implicit none
    private

    integer, parameter, public :: HASH_LEN = 64
    integer, parameter :: MAX_PARTS = 132
    integer, parameter :: MAX_INCLUDE_DEPTH = 16

    public :: cache_key_for, cache_source_tree_hash
    public :: cache_digest, cache_file_digest, hash_mod_file
    public :: cache_file_content_key

contains

    function cache_key_for(filename, compiler, flags, dep_keys, &
            n_dep_keys) result(key)
        character(len=*), intent(in) :: filename, compiler, flags
        character(len=HASH_LEN), intent(in) :: dep_keys(:)
        integer, intent(in) :: n_dep_keys
        character(len=HASH_LEN) :: key

        character(len=HASH_LEN) :: file_hash
        integer :: i, n_parts
        character(len=512) :: parts(MAX_PARTS)

        call cache_source_tree_hash(filename, file_hash)

        parts(1) = 'fx-cache-schema-1'
        parts(2) = file_hash
        parts(3) = trim(compiler)
        parts(4) = trim(flags)
        n_parts = 4
        do i = 1, min(n_dep_keys, MAX_PARTS - 4)
            n_parts = n_parts + 1
            parts(n_parts) = dep_keys(i)
        end do
        key = digest_parts(parts, n_parts)
    end function cache_key_for

    subroutine cache_source_tree_hash(filename, hash)
        !! Hash a source file together with every file it (recursively) pulls in
        !! via Fortran `include` statements. Without this, editing an .inc file
        !! leaves the including .f90's content hash unchanged, so the compile
        !! cache serves a stale object and the edit silently never takes effect.
        character(len=*), intent(in) :: filename
        character(len=HASH_LEN), intent(out) :: hash

        character(len=512) :: parts(MAX_PARTS)
        integer :: n_parts

        n_parts = 0
        call accumulate_source_hashes(filename, parts, n_parts, 0)
        if (n_parts == 0) then
            hash = ''
        else
            hash = digest_parts(parts, n_parts)
        end if
    end subroutine cache_source_tree_hash

    recursive subroutine accumulate_source_hashes(filename, parts, n_parts, depth)
        character(len=*), intent(in) :: filename
        character(len=512), intent(inout) :: parts(:)
        integer, intent(inout) :: n_parts
        integer, intent(in) :: depth

        character(len=HASH_LEN) :: fh
        character(len=512) :: line, incfile, dir
        integer :: u, ios, ierr

        if (depth > MAX_INCLUDE_DEPTH .or. n_parts >= size(parts)) return

        call sha256_file(filename, fh, ierr)
        if (ierr == 0) then
            n_parts = n_parts + 1
            parts(n_parts) = fh
        end if

        dir = dirname_of(filename)
        open (newunit=u, file=trim(filename), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call parse_include_path(line, incfile)
            if (len_trim(incfile) == 0) cycle
            if (incfile(1:1) /= '/') incfile = trim(dir)//trim(incfile)
            call accumulate_source_hashes(trim(incfile), parts, n_parts, depth + 1)
        end do
        close (u)
    end subroutine accumulate_source_hashes

    subroutine parse_include_path(line, incfile)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: incfile

        character(len=512) :: t
        character(len=7) :: head
        character(len=1) :: qc
        integer :: i, q1, q2

        incfile = ''
        t = adjustl(line)
        if (len_trim(t) < 9) return
        head = t(1:7)
        do i = 1, 7
            if (head(i:i) >= 'A' .and. head(i:i) <= 'Z') &
                head(i:i) = achar(iachar(head(i:i)) + 32)
        end do
        if (head /= 'include') return
        if (t(8:8) /= ' ' .and. t(8:8) /= '''' .and. t(8:8) /= '"') return
        q1 = scan(t, '''"')
        if (q1 == 0) return
        qc = t(q1:q1)
        q2 = index(t(q1 + 1:), qc)
        if (q2 == 0) return
        incfile = t(q1 + 1:q1 + q2 - 1)
    end subroutine parse_include_path

    function dirname_of(path) result(d)
        character(len=*), intent(in) :: path
        character(len=512) :: d
        integer :: s

        s = index(path, '/', back=.true.)
        if (s == 0) then
            d = './'
        else
            d = path(1:s)
        end if
    end function dirname_of

    function cache_digest(parts, n_parts) result(key)
        !! Public content digest over string parts (link-action keys etc.).
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=HASH_LEN) :: key

        key = digest_parts(parts, n_parts)
    end function cache_digest

    subroutine cache_file_digest(path, key)
        !! Content key of an arbitrary file (e.g. an object or archive), used to
        !! build link-action keys. Empty on failure.
        character(len=*), intent(in) :: path
        character(len=HASH_LEN), intent(out) :: key

        integer :: sz, ierr

        call cache_file_content_key(path, 'file', key, sz, ierr)
        if (ierr /= 0) key = ''
    end subroutine cache_file_digest

    subroutine hash_mod_file(modpath, key)
        character(len=*), intent(in) :: modpath
        character(len=HASH_LEN), intent(out) :: key

        integer :: size_bytes, ierr

        call cache_file_content_key(modpath, 'mod', key, size_bytes, ierr)
        if (ierr /= 0) key = ''
    end subroutine hash_mod_file

    subroutine cache_file_content_key(path, kind, key, size_bytes, ierr)
        character(len=*), intent(in) :: path, kind
        character(len=HASH_LEN), intent(out) :: key
        integer, intent(out) :: size_bytes, ierr

        character(len=HASH_LEN) :: hex
        character(len=512) :: parts(3)

        call sha256_file(path, hex, ierr)
        if (ierr /= 0) then
            key = ''
            size_bytes = 0
            return
        end if
        call file_size(path, size_bytes)
        parts(1) = 'fx-payload-schema-1'
        parts(2) = trim(kind)
        parts(3) = hex
        key = digest_parts(parts, 3)
    end subroutine cache_file_content_key

    function digest_parts(parts, n_parts) result(key)
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=HASH_LEN) :: key

        character(len=:), allocatable :: text
        character(len=32) :: len_text
        integer :: i

        text = ''
        do i = 1, n_parts
            write (len_text, '(i0)') len_trim(parts(i))
            text = text//trim(len_text)//':'//trim(parts(i))//achar(10)
        end do
        key = sha256_string(text)
    end function digest_parts

    subroutine file_size(path, size_bytes)
        character(len=*), intent(in) :: path
        integer, intent(out) :: size_bytes

        integer :: nbytes, ios
        logical :: exists

        size_bytes = 0
        inquire (file=trim(path), exist=exists, size=nbytes, iostat=ios)
        if (ios == 0 .and. exists .and. nbytes > 0) size_bytes = nbytes
    end subroutine file_size

end module fx_cache_key
