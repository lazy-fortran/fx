module fx_action_cache_record
    use fx_cache, only: cache_t, cache_has, cache_store_bytes, cache_restore_bytes
    use fx_cache_key, only: HASH_LEN, cache_digest
    implicit none
    private

    public :: MAX_MOD_NAME, store_action_record, restore_action_record, valid_smod_name

    integer, parameter :: MAX_MOD_NAME = 257
    integer, parameter :: CACHE_SCHEMA_VERSION = 2
    integer, parameter :: MAX_RECORD_BYTES = 8192

contains

    subroutine store_action_record(c, action_id, output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, smod_name, smod_key, &
            smod_size, has_smod, ierr)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, output_id, object_key, mod_name
        character(len=*), intent(in) :: mod_key, smod_name, smod_key
        integer, intent(in) :: obj_size, mod_size, smod_size
        logical, intent(in) :: has_mod, has_smod
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
        if (has_smod) then
            write (num, '(i0)') smod_size
            text = text//'smod '//trim(smod_name)//' '//trim(smod_key)//' '// &
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
            obj_size, mod_name, mod_key, mod_size, has_mod, smod_name, smod_key, &
            smod_size, has_smod, ierr)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key, smod_key
        character(len=*), intent(out) :: mod_name, smod_name
        integer, intent(out) :: obj_size, mod_size, smod_size, ierr
        logical, intent(out) :: has_mod, has_smod

        character(len=1) :: rec_bytes(MAX_RECORD_BYTES)
        character(len=MAX_RECORD_BYTES) :: rec_text
        integer :: n_rec, i

        output_id = ''
        object_key = ''
        mod_key = ''
        mod_name = ''
        smod_key = ''
        smod_name = ''
        obj_size = 0
        mod_size = 0
        has_mod = .false.
        smod_size = 0
        has_smod = .false.
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
            obj_size, mod_name, mod_key, mod_size, has_mod, smod_name, smod_key, &
            smod_size, has_smod, ierr)
    end subroutine restore_action_record

    subroutine parse_action_record(text, output_id, object_key, obj_size, &
            mod_name, mod_key, mod_size, has_mod, smod_name, smod_key, &
            smod_size, has_smod, ierr)
        character(len=*), intent(in) :: text
        character(len=HASH_LEN), intent(out) :: output_id, object_key, mod_key, smod_key
        character(len=*), intent(out) :: mod_name, smod_name
        integer, intent(out) :: obj_size, mod_size, smod_size, ierr
        logical, intent(out) :: has_mod, has_smod

        character(len=512) :: line, tag, artifact_name, parts(6)
        integer :: ios, schema, p, q, n

        output_id = ''
        object_key = ''
        mod_key = ''
        mod_name = ''
        smod_key = ''
        smod_name = ''
        obj_size = 0
        mod_size = 0
        has_mod = .false.
        smod_size = 0
        has_smod = .false.
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
                read (line, *, iostat=ios) tag, artifact_name, mod_key, mod_size
                if (ios /= 0) return
                if (len_trim(artifact_name) > len(mod_name)) return
                mod_name = artifact_name
                has_mod = .true.
            case ('smod')
                read (line, *, iostat=ios) tag, artifact_name, smod_key, smod_size
                if (ios /= 0) return
                if (.not. valid_smod_name(trim(artifact_name))) return
                if (len_trim(artifact_name) > len(smod_name)) return
                smod_name = artifact_name
                if (len_trim(smod_name) == 0 .or. len_trim(smod_key) == 0) return
                if (smod_size < 0) return
                has_smod = .true.
            end select
        end do

        if (schema /= CACHE_SCHEMA_VERSION) return
        if (len_trim(output_id) == 0 .or. len_trim(object_key) == 0) return
        parts(1) = 'fx-output-schema-2'
        parts(2) = object_key
        parts(3) = mod_key
        parts(4) = mod_name
        parts(5) = smod_key
        parts(6) = smod_name
        if (output_id /= cache_digest(parts, 6)) return
        ierr = 0
    end subroutine parse_action_record

    pure logical function valid_smod_name(name) result(valid)
        character(len=*), intent(in) :: name
        integer :: i, code, separators
        logical :: initial, letter

        valid = .false.
        if (len_trim(name) == 0 .or. len_trim(name) > MAX_MOD_NAME) return
        initial = .true.
        separators = 0
        do i = 1, len_trim(name)
            code = iachar(name(i:i))
            letter = (code >= iachar('a') .and. code <= iachar('z')) .or. &
                (code >= iachar('A') .and. code <= iachar('Z'))
            if (initial) then
                if (.not. letter) return
                initial = .false.
            else if (name(i:i) == '@') then
                separators = separators + 1
                if (separators > 1) return
                initial = .true.
            else if (.not. letter) then
                if (name(i:i) /= '_') then
                    if (code < iachar('0') .or. code > iachar('9')) return
                end if
            end if
        end do
        valid = .not. initial
    end function valid_smod_name

end module fx_action_cache_record
