module fx_cache_key
    use fx_hash, only: sha256_string, sha256_file, sha256_state_t, &
        sha256_init, sha256_update, sha256_final
    implicit none
    private

    integer, parameter, public :: HASH_LEN = 64
    integer, parameter :: MAX_PARTS = 132
    integer, parameter :: MAX_INCLUDE_DEPTH = 16

    public :: cache_key_for, cache_source_tree_hash
    public :: cache_digest, cache_file_digest, hash_mod_file
    public :: cache_file_content_key
    public :: cache_set_file_hash_hook, cache_clear_file_hash_hook

    abstract interface
        subroutine file_hash_i(path, hex, ierr)
            character(len=*), intent(in) :: path
            character(len=64), intent(out) :: hex
            integer, intent(out) :: ierr
        end subroutine file_hash_i
    end interface

    procedure(file_hash_i), pointer :: file_hash_hook => null()

contains

    subroutine cache_set_file_hash_hook(hook)
        !! Install a memoizing hash (e.g. fo's stat-memo) for all source and
        !! payload content keys, replacing the default sha256_file. The hook must
        !! return the same sha256 hex as sha256_file for unchanged content; it
        !! only avoids re-reading files whose (mtime, size) are unchanged.
        procedure(file_hash_i) :: hook

        file_hash_hook => hook
    end subroutine cache_set_file_hash_hook

    subroutine cache_clear_file_hash_hook()
        file_hash_hook => null()
    end subroutine cache_clear_file_hash_hook

    subroutine hash_file(path, hex, ierr)
        character(len=*), intent(in) :: path
        character(len=64), intent(out) :: hex
        integer, intent(out) :: ierr

        if (associated(file_hash_hook)) then
            call file_hash_hook(path, hex, ierr)
        else
            call sha256_file(path, hex, ierr)
        end if
    end subroutine hash_file

    function cache_key_for(filename, compiler, flags, dep_keys, &
            n_dep_keys, include_dirs) result(key)
        character(len=*), intent(in) :: filename, compiler, flags
        character(len=HASH_LEN), intent(in) :: dep_keys(:)
        integer, intent(in) :: n_dep_keys
        character(len=*), intent(in), optional :: include_dirs(:)
        character(len=HASH_LEN) :: key

        character(len=HASH_LEN) :: file_hash
        integer :: i, n_parts
        character(len=512) :: parts(MAX_PARTS)

        call cache_source_tree_hash(filename, file_hash, include_dirs)

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

    subroutine cache_source_tree_hash(filename, hash, include_dirs)
        !! Hash a source file together with every file it (recursively) pulls in
        !! via Fortran `include` statements. Without this, editing an .inc file
        !! leaves the including .f90's content hash unchanged, so the compile
        !! cache serves a stale object and the edit silently never takes effect.
        character(len=*), intent(in) :: filename
        character(len=HASH_LEN), intent(out) :: hash
        character(len=*), intent(in), optional :: include_dirs(:)

        character(len=512) :: parts(MAX_PARTS)
        integer :: n_parts

        n_parts = 0
        call accumulate_source_hashes(filename, parts, n_parts, 0, include_dirs)
        if (n_parts == 0) then
            hash = ''
        else
            hash = digest_parts(parts, n_parts)
        end if
    end subroutine cache_source_tree_hash

    recursive subroutine accumulate_source_hashes(filename, parts, n_parts, depth, &
            include_dirs)
        character(len=*), intent(in) :: filename
        character(len=512), intent(inout) :: parts(:)
        integer, intent(inout) :: n_parts
        integer, intent(in) :: depth
        character(len=*), intent(in), optional :: include_dirs(:)

        character(len=HASH_LEN) :: fh
        character(len=512) :: line, incfile, resolved
        integer :: u, ios, ierr

        if (depth > MAX_INCLUDE_DEPTH .or. n_parts >= size(parts)) return

        call hash_file(filename, fh, ierr)
        if (ierr == 0) then
            n_parts = n_parts + 1
            parts(n_parts) = fh
        end if

        open (newunit=u, file=trim(filename), status='old', iostat=ios)
        if (ios /= 0) return
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            call parse_include_path(line, incfile)
            if (len_trim(incfile) == 0) cycle
            call resolve_include(filename, incfile, include_dirs, resolved)
            call accumulate_source_hashes(trim(resolved), parts, n_parts, depth + 1, &
                include_dirs)
        end do
        close (u)
    end subroutine accumulate_source_hashes

    subroutine resolve_include(source, name, include_dirs, path)
        character(len=*), intent(in) :: source, name
        character(len=*), intent(in), optional :: include_dirs(:)
        character(len=*), intent(out) :: path
        integer :: i
        logical :: exists

        path = name
        if (name(1:1) == '/') return
        path = trim(dirname_of(source))//trim(name)
        inquire (file=trim(path), exist=exists)
        if (exists) return
        if (.not. present(include_dirs)) return
        do i = 1, size(include_dirs)
            path = trim(include_dirs(i))//'/'//trim(name)
            inquire (file=trim(path), exist=exists)
            if (exists) return
        end do
    end subroutine resolve_include

    subroutine parse_include_path(line, incfile)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: incfile

        character(len=512) :: t
        character(len=7) :: head
        character(len=1) :: qc
        integer :: i, q1, q2

        incfile = ''
        t = adjustl(line)
        if (t(1:1) == '#') t = adjustl(t(2:))
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

        call hash_file(path, hex, ierr)
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
        !! Digest over length-prefixed parts. Feeds each part directly into a
        !! block-buffered incremental SHA-256, so the digest is computed without
        !! growing or reallocating any buffer. digest_parts is reached from
        !! OpenMP parallel regions (fo's link_binary), where a growing shared
        !! allocatable `text = text // ...` is the classic realloc data race;
        !! streaming keeps every byte the caller passes on the caller's own
        !! thread and leaves the parallel worker with no shared allocation.
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=HASH_LEN) :: key

        type(sha256_state_t) :: state
        character(len=32) :: len_text
        integer :: i, ln

        call sha256_init(state)
        do i = 1, n_parts
            write (len_text, '(i0)') len_trim(parts(i))
            ln = len_trim(len_text)
            call sha256_update(state, len_text(:ln), ln)
            call sha256_update(state, ':', 1)
            call sha256_update(state, trim(parts(i)), len_trim(parts(i)))
            call sha256_update(state, achar(10), 1)
        end do
        key = sha256_final(state)
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
