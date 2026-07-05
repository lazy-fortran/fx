module fx_action_cache
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_cache, only: cache_t, cache_init, cache_has, cache_store, &
        cache_restore, cache_store_bytes, cache_restore_bytes
    use fx_cache_fs, only: cache_file_fingerprint
    use fx_cache_key, only: HASH_LEN, cache_key_for, cache_source_tree_hash, &
        cache_digest, cache_file_digest, hash_mod_file, &
        cache_file_content_key
    use fx_string, only: to_lower
    implicit none
    private

    public :: cache_t, HASH_LEN
    public :: cache_key_for, cache_source_tree_hash, cache_digest, &
        cache_file_digest, hash_mod_file
    public :: action_cache_root, action_cache_store_root, action_cache_schema
    public :: action_cache_init, cache_lookup
    public :: cache_store_action, cache_restore_action, cache_action_mod_key
    public :: cache_store_binary, cache_restore_binary, &
        cache_binary_matches
    public :: cache_debug_write_action_record, cache_debug_corrupt_object_payload

    integer, parameter :: MAX_MOD_NAME = 132
    integer, parameter :: CACHE_SCHEMA_VERSION = 1
    integer, parameter :: MAX_RECORD_BYTES = 8192

contains

    subroutine action_cache_root(env_var, subdir, root)
        !! Resolve the cache root for a tool. env_var (e.g. 'FO_CACHE_DIR')
        !! overrides everything; otherwise $HOME/.cache/<subdir>. Each tool owns
        !! its own env var and namespace so caches never collide.
        character(len=*), intent(in) :: env_var, subdir
        character(len=*), intent(out) :: root

        character(len=512) :: home, override

        call get_environment_variable(trim(env_var), override)
        if (len_trim(override) > 0) then
            root = trim(override)
            return
        end if
        call get_environment_variable('HOME', home)
        if (len_trim(home) == 0) call get_environment_variable('USERPROFILE', home)
        root = trim(home)//'/.cache/'//trim(subdir)
    end subroutine action_cache_root

    subroutine action_cache_store_root(env_var, subdir, root)
        character(len=*), intent(in) :: env_var, subdir
        character(len=*), intent(out) :: root

        character(len=512) :: base

        call action_cache_root(env_var, subdir, base)
        root = trim(base)//'/store/v1'
    end subroutine action_cache_store_root

    subroutine action_cache_schema(schema)
        character(len=*), intent(out) :: schema

        schema = 'action-output-v1'
    end subroutine action_cache_schema

    subroutine action_cache_init(c, env_var, subdir, ierr)
        type(cache_t), intent(out) :: c
        character(len=*), intent(in) :: env_var, subdir
        integer, intent(out) :: ierr

        character(len=512) :: root

        ierr = 0
        call action_cache_store_root(env_var, subdir, root)
        call cache_init(c, trim(root))
        if (.not. c%initialized) ierr = 1
    end subroutine action_cache_init

    function cache_lookup(c, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        logical :: hit

        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_MOD_NAME) :: mod_label
        integer :: ierr, obj_size, mod_size
        logical :: has_mod

        hit = .false.
        call restore_action_record(c, key, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        if (ierr /= 0) return
        hit = cache_has(c, trim(out_id)//'-d') .and. &
            cache_has(c, trim(object_key)//'-d')
        if (hit .and. has_mod) hit = cache_has(c, trim(mod_key)//'-d')
    end function cache_lookup

    subroutine cache_restore_action(c, action_id, obj_path, mod_dir, restored, &
            output_id)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir
        logical, intent(out) :: restored
        character(len=HASH_LEN), intent(out), optional :: output_id

        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_MOD_NAME) :: mod_label
        integer :: ierr, obj_size, mod_size
        logical :: has_mod, local_ok

        restored = .false.
        if (present(output_id)) output_id = ''
        if (.not. c%initialized) return

        call restore_action_record(c, action_id, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        if (ierr /= 0) return
        if (present(output_id)) output_id = out_id
        if (.not. cache_has(c, trim(out_id)//'-d')) return

        local_ok = local_outputs_match(obj_path, mod_dir, mod_label, object_key, &
            mod_key, has_mod)
        if (local_ok) then
            restored = .true.
            return
        end if

        call cache_restore(c, trim(object_key)//'-d', obj_path, ierr)
        if (ierr /= 0) return
        if (has_mod) then
            call cache_restore(c, trim(mod_key)//'-d', &
                trim(mod_dir)//'/'//trim(mod_label)//'.mod', ierr)
            if (ierr /= 0) return
        end if

        restored = local_outputs_match(obj_path, mod_dir, mod_label, object_key, &
            mod_key, has_mod)
    end subroutine cache_restore_action

    subroutine cache_store_action(c, action_id, obj_path, mod_dir, mod_name, &
            output_id, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir, mod_name
        character(len=HASH_LEN), intent(out) :: output_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: object_key, mod_key
        character(len=512) :: parts(4)
        character(len=:), allocatable :: lower_name, mod_path
        character(len=1) :: marker(1)
        integer :: obj_size, mod_size, store_ierr
        logical :: has_mod

        output_id = ''
        ierr = 0
        if (.not. c%initialized) then
            ierr = 1
            return
        end if

        call cache_file_content_key(obj_path, 'object', object_key, obj_size, ierr)
        if (ierr /= 0) return

        lower_name = to_lower(trim(mod_name))
        mod_path = trim(mod_dir)//'/'//lower_name//'.mod'
        inquire (file=mod_path, exist=has_mod)
        mod_key = ''
        mod_size = 0
        if (has_mod) then
            call cache_file_content_key(mod_path, 'mod', mod_key, mod_size, ierr)
            if (ierr /= 0) return
        end if

        parts(1) = 'fx-output-schema-1'
        parts(2) = object_key
        parts(3) = mod_key
        parts(4) = lower_name
        output_id = cache_digest(parts, 4)

        call cache_store(c, trim(object_key)//'-d', obj_path, store_ierr)
        if (store_ierr /= 0) then
            ierr = store_ierr
            return
        end if
        if (has_mod) then
            call cache_store(c, trim(mod_key)//'-d', mod_path, store_ierr)
            if (store_ierr /= 0) then
                ierr = store_ierr
                return
            end if
        end if

        marker(1) = '1'
        call cache_store_bytes(c, trim(output_id)//'-d', marker, 1, ierr)
        if (ierr /= 0) return

        call store_action_record(c, action_id, output_id, object_key, obj_size, &
            lower_name, mod_key, mod_size, has_mod, ierr)
    end subroutine cache_store_action

    subroutine cache_action_mod_key(c, action_id, mod_key, found)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: mod_key
        logical, intent(out) :: found

        character(len=HASH_LEN) :: out_id, object_key
        character(len=MAX_MOD_NAME) :: mod_label
        integer :: ierr, obj_size, mod_size
        logical :: has_mod

        mod_key = ''
        found = .false.
        if (.not. c%initialized) return

        call restore_action_record(c, action_id, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        if (ierr /= 0 .or. .not. has_mod) then
            mod_key = ''
            return
        end if
        found = cache_has(c, trim(mod_key)//'-d')
        if (.not. found) mod_key = ''
    end subroutine cache_action_mod_key

    subroutine cache_store_binary(c, action_id, bin_path, ierr)
        !! Record action_id -> (mtime, size) for a just-linked binary so a later
        !! build can skip relinking an unchanged output (cache_binary_matches).
        !!
        !! The binary payload is deliberately NOT stored in the CAS. Linked
        !! binaries can be huge and caching every distinct one fills the disk.
        !! The warm-build win comes from the mtime/size skip, not from restoring
        !! bytes; on a genuine miss the caller simply relinks. The record stores
        !! a placeholder content key in the slot cache_restore_binary
        !! would read, so restore always misses and the link runs.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        integer, intent(out) :: ierr

        character(len=:), allocatable :: rec_text
        character(len=1), allocatable :: rec(:)
        character(len=40) :: mt_text, sz_text
        integer(int64) :: mtime, fsize
        integer :: i, n, stat_ierr

        ierr = 1
        if (.not. c%initialized) return
        call cache_file_fingerprint(bin_path, fsize, mtime, stat_ierr)
        if (stat_ierr /= 0) return
        write (mt_text, '(i0)') mtime
        write (sz_text, '(i0)') fsize
        rec_text = 'nopayload '//trim(mt_text)//' '//trim(sz_text)
        n = len(rec_text)
        allocate (rec(n))
        do i = 1, n
            rec(i) = rec_text(i:i)
        end do
        call cache_store_bytes(c, trim(action_id)//'-l', rec, n, ierr)
        deallocate (rec)
    end subroutine cache_store_binary

    subroutine cache_binary_matches(c, action_id, bin_path, matches)
        !! True iff bin_path exists and its (mtime, size) equal what the link
        !! record stored: the warm-build fast path leaving unchanged outputs
        !! untouched so a build that relinks nothing rewrites nothing.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        logical, intent(out) :: matches

        character(len=HASH_LEN) :: content_key
        integer(int64) :: rec_mtime, rec_size, cur_mtime, cur_size
        integer :: stat_ierr
        logical :: ok

        matches = .false.
        call read_link_record(c, action_id, content_key, rec_mtime, rec_size, ok)
        if (.not. ok) return
        call cache_file_fingerprint(bin_path, cur_size, cur_mtime, stat_ierr)
        if (stat_ierr /= 0) return
        matches = (cur_mtime == rec_mtime .and. cur_size == rec_size)
    end subroutine cache_binary_matches

    subroutine cache_restore_binary(c, action_id, dest_path, restored)
        !! Restore a cached binary for action_id to dest_path (raw bytes; the
        !! caller sets the execute bit). restored is .false. on any miss.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, dest_path
        logical, intent(out) :: restored

        character(len=HASH_LEN) :: content_key
        integer(int64) :: mtime, fsize
        integer :: ierr
        logical :: ok

        restored = .false.
        call read_link_record(c, action_id, content_key, mtime, fsize, ok)
        if (.not. ok) return
        if (.not. cache_has(c, trim(content_key)//'-d')) return
        call cache_restore(c, trim(content_key)//'-d', dest_path, ierr)
        if (ierr /= 0) return
        restored = .true.
    end subroutine cache_restore_binary

    subroutine cache_debug_write_action_record(c, action_id, record_text, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, record_text
        integer, intent(out) :: ierr

        character(len=1), allocatable :: bytes(:)
        integer :: i, n

        ierr = 0
        n = len_trim(record_text)
        allocate (bytes(max(n, 0)))
        do i = 1, n
            bytes(i) = record_text(i:i)
        end do
        call cache_store_bytes(c, trim(action_id)//'-a', bytes, n, ierr)
    end subroutine cache_debug_write_action_record

    subroutine cache_debug_corrupt_object_payload(c, action_id, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: out_id, object_key, mod_key
        character(len=MAX_MOD_NAME) :: mod_label
        character(len=1) :: bad(7)
        integer :: obj_size, mod_size, i
        logical :: has_mod

        call restore_action_record(c, action_id, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, ierr)
        if (ierr /= 0) return
        do i = 1, size(bad)
            bad(i) = achar(iachar('0') + modulo(i, 10))
        end do
        call cache_store_bytes(c, trim(object_key)//'-d', bad, size(bad), ierr)
    end subroutine cache_debug_corrupt_object_payload

    subroutine store_action_record(c, action_id, output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, output_id, object_key, mod_name
        character(len=*), intent(in) :: mod_key
        integer, intent(in) :: obj_size, mod_size
        logical, intent(in) :: has_mod
        integer, intent(out) :: ierr

        character(len=:), allocatable :: text
        character(len=1), allocatable :: bytes(:)
        character(len=32) :: num
        integer :: i, n

        write (num, '(i0)') CACHE_SCHEMA_VERSION
        text = 'schema '//trim(num)//achar(10)//'kind compile'//achar(10)
        text = text//'output '//trim(output_id)//achar(10)
        write (num, '(i0)') obj_size
        text = text//'object '//trim(object_key)//' '//trim(num)//achar(10)
        if (has_mod) then
            write (num, '(i0)') mod_size
            text = text//'mod '//trim(mod_name)//' '//trim(mod_key)//' '// &
                trim(num)//achar(10)
        end if

        n = len(text)
        allocate (bytes(n))
        do i = 1, n
            bytes(i) = text(i:i)
        end do
        call cache_store_bytes(c, trim(action_id)//'-a', bytes, n, ierr)
        deallocate (bytes)
    end subroutine store_action_record

    subroutine restore_action_record(c, action_id, output_id, object_key, &
            obj_size, mod_name, mod_key, mod_size, has_mod, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key
        character(len=*), intent(out) :: mod_name
        integer, intent(out) :: obj_size, mod_size, ierr
        logical, intent(out) :: has_mod

        character(len=1) :: rec_bytes(MAX_RECORD_BYTES)
        character(len=MAX_RECORD_BYTES) :: rec_text
        integer :: n_rec, i

        output_id = ''
        object_key = ''
        mod_key = ''
        mod_name = ''
        obj_size = 0
        mod_size = 0
        has_mod = .false.
        ierr = 1
        if (.not. c%initialized) return
        if (.not. cache_has(c, trim(action_id)//'-a')) return

        call cache_restore_bytes(c, trim(action_id)//'-a', rec_bytes, n_rec, ierr)
        if (ierr /= 0 .or. n_rec <= 0 .or. n_rec > MAX_RECORD_BYTES) then
            ierr = 1
            return
        end if
        rec_text = ''
        do i = 1, n_rec
            rec_text(i:i) = rec_bytes(i)
        end do
        call parse_action_record(rec_text(1:n_rec), output_id, object_key, &
            obj_size, mod_name, mod_key, mod_size, has_mod, ierr)
    end subroutine restore_action_record

    subroutine parse_action_record(text, output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, ierr)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key
        character(len=*), intent(out) :: mod_name
        integer, intent(out) :: obj_size, mod_size, ierr
        logical, intent(out) :: has_mod

        character(len=512) :: line, tag
        integer :: ios, schema, p, q, n

        output_id = ''
        object_key = ''
        mod_key = ''
        mod_name = ''
        obj_size = 0
        mod_size = 0
        has_mod = .false.
        ierr = 1
        schema = -1

        n = len(text)
        p = 1
        do while (p <= n)
            q = index(text(p:n), achar(10))
            if (q == 0) then
                line = text(p:n)
                p = n + 1
            else
                line = text(p:p + q - 2)
                p = p + q
            end if
            if (len_trim(line) == 0) cycle
            read (line, *, iostat=ios) tag
            if (ios /= 0) cycle
            select case (trim(tag))
            case ('schema')
                read (line, *, iostat=ios) tag, schema
            case ('output')
                read (line, *, iostat=ios) tag, output_id
            case ('object')
                read (line, *, iostat=ios) tag, object_key, obj_size
            case ('mod')
                read (line, *, iostat=ios) tag, mod_name, mod_key, mod_size
                if (ios == 0) has_mod = .true.
            end select
        end do

        if (schema /= CACHE_SCHEMA_VERSION) return
        if (len_trim(output_id) == 0 .or. len_trim(object_key) == 0) return
        ierr = 0
    end subroutine parse_action_record

    subroutine read_link_record(c, action_id, content_key, mtime, fsize, ok)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: content_key
        integer(int64), intent(out) :: mtime, fsize
        logical, intent(out) :: ok

        character(len=1) :: rec(256)
        character(len=256) :: text
        integer :: n_rec, ierr, i

        ok = .false.
        content_key = ''
        mtime = 0_int64
        fsize = 0_int64
        if (.not. c%initialized) return
        if (.not. cache_has(c, trim(action_id)//'-l')) return
        call cache_restore_bytes(c, trim(action_id)//'-l', rec, n_rec, ierr)
        if (ierr /= 0 .or. n_rec <= 0 .or. n_rec > 256) return
        text = ''
        do i = 1, n_rec
            text(i:i) = rec(i)
        end do
        read (text(1:n_rec), *, iostat=ierr) content_key, mtime, fsize
        if (ierr /= 0) return
        ok = .true.
    end subroutine read_link_record

    logical function local_outputs_match(obj_path, mod_dir, mod_name, object_key, &
            mod_key, has_mod) result(ok)
        character(len=*), intent(in) :: obj_path, mod_dir, mod_name
        character(len=*), intent(in) :: object_key, mod_key
        logical, intent(in) :: has_mod

        character(len=HASH_LEN) :: actual_key
        integer :: size_bytes, ierr

        ok = .false.
        call cache_file_content_key(obj_path, 'object', actual_key, size_bytes, ierr)
        if (ierr /= 0 .or. trim(actual_key) /= trim(object_key)) return
        if (has_mod) then
            call cache_file_content_key(trim(mod_dir)//'/'//trim(mod_name)//'.mod', &
                'mod', actual_key, size_bytes, ierr)
            if (ierr /= 0 .or. trim(actual_key) /= trim(mod_key)) return
        end if
        ok = .true.
    end function local_outputs_match

end module fx_action_cache
