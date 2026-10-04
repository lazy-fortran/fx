module fx_action_result_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_string
    use fx_cache_fs, only: cache_read_bytes_file
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_CORRUPT, &
        IMMUTABLE_MISSING
    use fx_immutable_manifest, only: immutable_tree_entry_t, &
        immutable_id_valid, immutable_manifest_parse
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        immutable_store_put_blob, immutable_store_tree_path, &
        immutable_store_materialize_blob, IMMUTABLE_MATERIALIZE_AUTO
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree
    use fx_action_result_record, only: ACTION_RECORD_BOUND, &
        ACTION_RECORD_CONFLICT, action_result_bound_record, &
        action_result_conflict_record, action_result_record_parse
    implicit none
    private

    integer, parameter, public :: ACTION_RESULT_OK = 0
    integer, parameter, public :: ACTION_RESULT_CONFLICT = 1
    integer, parameter, public :: ACTION_RESULT_QUARANTINED = 2
    integer, parameter, public :: ACTION_RESULT_MISSING = 3
    integer, parameter, public :: ACTION_RESULT_INVALID = 4
    integer, parameter, public :: ACTION_RESULT_IO_ERROR = 5
    integer, parameter, public :: ACTION_RESULT_CORRUPT = 6

    integer, parameter :: HASH_LEN = 64
    integer, parameter :: RECORD_LIMIT = 512

    type, public :: action_result_store_t
        type(immutable_store_t) :: objects
        character(len=:), allocatable :: root_dir
        logical :: initialized = .false.
    end type action_result_store_t

    public :: action_result_store_init, action_result_put_blob
    public :: action_result_publish, action_result_publish_files
    public :: action_result_lookup, action_result_conflicts
    public :: action_result_materialize_blob, action_result_action_key
    public :: action_result_file_mode

    interface
        integer(c_int) function c_action_lock(root, action_id) &
                bind(C, name='fx_action_result_lock')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
        end function c_action_lock
        integer(c_int) function c_action_unlock(handle) &
                bind(C, name='fx_action_result_unlock')
            import c_int
            integer(c_int), value :: handle
        end function c_action_unlock
        integer(c_int) function c_action_exists(root, action_id) &
                bind(C, name='fx_action_result_exists')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
        end function c_action_exists
        integer(c_int) function c_action_read(root, action_id, bytes, capacity, count) &
                bind(C, name='fx_action_result_read')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
            character(kind=c_char), intent(out) :: bytes(*)
            integer(c_int), value :: capacity
            integer(c_int), intent(out) :: count
        end function c_action_read
        integer(c_int) function c_action_write(root, action_id, bytes, count) &
                bind(C, name='fx_action_result_write')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*), bytes(*)
            integer(c_int), value :: count
        end function c_action_write
        integer(c_int) function c_file_mode(path, mode) &
                bind(C, name='fx_action_result_file_mode')
            import c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), intent(out) :: mode
        end function c_file_mode
        integer(c_int) function c_temp_path(destination, output, capacity) &
                bind(C, name='fx_action_result_temp_path')
            import c_char, c_int
            character(kind=c_char), intent(in) :: destination(*)
            character(kind=c_char), intent(out) :: output(*)
            integer(c_int), value :: capacity
        end function c_temp_path
        integer(c_int) function c_replace(source, destination) &
                bind(C, name='fx_action_result_replace')
            import c_char, c_int
            character(kind=c_char), intent(in) :: source(*), destination(*)
        end function c_replace
        integer(c_int) function c_unlink(path) &
                bind(C, name='fx_action_result_unlink')
            import c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_unlink
    end interface

contains

    subroutine action_result_store_init(store, root_dir, ierr)
        type(action_result_store_t), intent(out) :: store
        character(len=*), intent(in) :: root_dir
        integer, intent(out) :: ierr

        call immutable_store_init(store%objects, trim(root_dir), ierr)
        if (ierr /= IMMUTABLE_OK) return
        store%root_dir = trim(root_dir)
        store%initialized = .true.
    end subroutine action_result_store_init

    subroutine action_result_put_blob(store, source_path, blob_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: source_path
        character(len=HASH_LEN), intent(out) :: blob_id
        integer, intent(out) :: ierr

        blob_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        call immutable_store_put_blob(store%objects, trim(source_path), blob_id, ierr)
        if (ierr /= IMMUTABLE_OK) then
            if (ierr == IMMUTABLE_CORRUPT) then
                ierr = ACTION_RESULT_CORRUPT
            else
                ierr = ACTION_RESULT_IO_ERROR
            end if
        end if
    end subroutine action_result_put_blob

    subroutine action_result_file_mode(source_path, mode, ierr)
        character(len=*), intent(in) :: source_path
        integer, intent(out) :: mode, ierr
        character(kind=c_char), allocatable :: c_path(:)
        integer(c_int) :: c_mode, rc

        mode = 0
        ierr = ACTION_RESULT_INVALID
        call to_c_text(source_path, c_path)
        rc = c_file_mode(c_path, c_mode)
        if (rc == 0_c_int) then
            mode = int(c_mode)
            ierr = ACTION_RESULT_OK
        end if
    end subroutine action_result_file_mode

    subroutine action_result_publish(store, action_id, entries, result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: key, current, ids(2), temporary_id
        character(len=:), allocatable :: record
        character(kind=c_char), allocatable :: c_root(:), c_key(:), c_record(:)
        character(kind=c_char) :: existing(RECORD_LIMIT)
        integer(c_int) :: lock, count, rc, unlock_rc
        integer :: verify_status, parse_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return

        call immutable_store_put_tree(store%objects, entries, result_id, verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if

        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        lock = c_action_lock(c_root, c_key)
        if (lock < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        rc = c_action_read(c_root, c_key, existing, RECORD_LIMIT, count)
        if (rc == 1_c_int) then
            record = action_result_bound_record(key, result_id)
            call write_record(c_root, c_key, record, ierr)
            unlock_rc = c_action_unlock(lock)
            if (ierr == ACTION_RESULT_OK .and. unlock_rc /= 0_c_int) &
                ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        if (rc /= 0_c_int) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if

        call action_result_record_parse(existing, int(count), key, current, ids, &
            parse_status)
        if (parse_status == ACTION_RECORD_CONFLICT) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_QUARANTINED
            return
        end if
        if (parse_status /= ACTION_RECORD_BOUND) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        if (current == result_id) then
            unlock_rc = c_action_unlock(lock)
            ierr = merge(ACTION_RESULT_OK, ACTION_RESULT_IO_ERROR, &
                unlock_rc == 0_c_int)
            return
        end if

        ids(1) = current
        ids(2) = result_id
        if (ids(1) > ids(2)) then
            temporary_id = ids(1)
            ids(1) = ids(2)
            ids(2) = temporary_id
        end if
        record = action_result_conflict_record(key, ids)
        call write_record(c_root, c_key, record, ierr)
        unlock_rc = c_action_unlock(lock)
        if (ierr == ACTION_RESULT_OK .and. unlock_rc == 0_c_int) &
            ierr = ACTION_RESULT_CONFLICT
        if (unlock_rc /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
    end subroutine action_result_publish

    subroutine action_result_publish_files(store, action_id, source_paths, &
            entries, result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id, source_paths(:)
        type(immutable_tree_entry_t), intent(inout) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        integer :: i

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (size(source_paths) /= size(entries)) return
        do i = 1, size(entries)
            call action_result_put_blob(store, trim(source_paths(i)), &
                entries(i)%object_id, ierr)
            if (ierr /= ACTION_RESULT_OK) return
        end do
        call action_result_publish(store, action_id, entries, result_id, ierr)
    end subroutine action_result_publish_files

    subroutine action_result_lookup(store, action_id, entries, result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: key, current, ids(2)
        character(kind=c_char), allocatable :: c_root(:), c_key(:)
        character(kind=c_char) :: record_bytes(RECORD_LIMIT)
        integer(c_int) :: lock, count, rc, unlock_rc, exists_rc
        integer :: parse_status, verify_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return
        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        exists_rc = c_action_exists(c_root, c_key)
        if (exists_rc == 1_c_int) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        if (exists_rc /= 0_c_int) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        lock = c_action_lock(c_root, c_key)
        if (lock < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        rc = c_action_read(c_root, c_key, record_bytes, RECORD_LIMIT, count)
        if (rc == 1_c_int) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_MISSING
            return
        end if
        if (rc /= 0_c_int) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call action_result_record_parse(record_bytes, int(count), key, current, &
            ids, parse_status)
        if (parse_status == ACTION_RECORD_CONFLICT) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_QUARANTINED
            return
        end if
        if (parse_status /= ACTION_RECORD_BOUND) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call immutable_store_verify_tree(store%objects, current, verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call read_result_tree(store, current, entries, ierr)
        result_id = current
        unlock_rc = c_action_unlock(lock)
        if (ierr == ACTION_RESULT_OK .and. unlock_rc /= 0_c_int) &
            ierr = ACTION_RESULT_IO_ERROR
    end subroutine action_result_lookup

    subroutine action_result_conflicts(store, action_id, ids, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: ids(2)
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: key, bound, parsed_ids(2)
        character(kind=c_char), allocatable :: c_root(:), c_key(:)
        character(kind=c_char) :: bytes(RECORD_LIMIT)
        integer(c_int) :: lock, count, rc, unlock_rc, exists_rc
        integer :: status

        ids = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        exists_rc = c_action_exists(c_root, c_key)
        if (exists_rc == 1_c_int) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        if (exists_rc /= 0_c_int) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        lock = c_action_lock(c_root, c_key)
        if (lock < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        rc = c_action_read(c_root, c_key, bytes, RECORD_LIMIT, count)
        if (rc /= 0_c_int) then
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_MISSING
            return
        end if
        call action_result_record_parse(bytes, int(count), key, bound, &
            parsed_ids, status)
        unlock_rc = c_action_unlock(lock)
        if (status /= ACTION_RECORD_CONFLICT) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        ids = parsed_ids
        ierr = ACTION_RESULT_QUARANTINED
    end subroutine action_result_conflicts

    subroutine action_result_materialize_blob(store, blob_id, destination, &
            mode, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: blob_id, destination
        integer, intent(in) :: mode
        integer, intent(out) :: ierr
        logical :: cloned
        character(len=4096) :: temporary
        character(len=:), allocatable :: staging_path
        character(kind=c_char), allocatable :: c_temp(:), c_destination(:)
        integer(c_int) :: rc, cleanup
        integer :: end_path

        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        call to_c_text(destination, c_destination)
        rc = c_temp_path(c_destination, temporary, int(len(temporary), c_int))
        if (rc /= 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        end_path = index(temporary, c_null_char)
        if (end_path <= 1) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        staging_path = temporary(:end_path - 1)
        call to_c_text(staging_path, c_temp)
        call immutable_store_materialize_blob(store%objects, blob_id, &
            staging_path, mode, IMMUTABLE_MATERIALIZE_AUTO, cloned, ierr)
        if (ierr /= IMMUTABLE_OK) then
            cleanup = c_unlink(c_temp)
            if (ierr == IMMUTABLE_CORRUPT) then
                ierr = ACTION_RESULT_CORRUPT
            else if (ierr == IMMUTABLE_MISSING) then
                ierr = ACTION_RESULT_MISSING
            else
                ierr = ACTION_RESULT_IO_ERROR
            end if
            return
        end if
        rc = c_replace(c_temp, c_destination)
        if (rc /= 0_c_int) write (*,*) 'REPLACE', staging_path, &
            trim(destination)
        ierr = ACTION_RESULT_IO_ERROR
        if (rc == 0_c_int) ierr = ACTION_RESULT_OK
        if (rc /= 0_c_int) cleanup = c_unlink(c_temp)
    end subroutine action_result_materialize_blob

    function action_result_action_key(action_id) result(key)
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN) :: key

        if (immutable_id_valid(trim(action_id))) then
            key = trim(action_id)
        else
            key = sha256_string('FXACTIONKEY2'//achar(10)//trim(action_id))
        end if
    end function action_result_action_key

    subroutine read_result_tree(store, result_id, entries, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: result_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: ierr
        character(len=1), allocatable :: bytes(:)
        character(len=:), allocatable :: text
        integer(int64) :: size_bytes
        integer :: i, count

        ierr = ACTION_RESULT_CORRUPT
        size_bytes = -1_int64
        inquire(file=immutable_store_tree_path(store%objects, result_id), &
            size=size_bytes)
        if (size_bytes < 0_int64 .or. size_bytes > int(huge(0), int64)) return
        allocate(bytes(int(size_bytes)))
        call cache_read_bytes_file(immutable_store_tree_path(store%objects, &
            result_id), bytes, count, ierr)
        if (ierr /= 0 .or. count /= size(bytes)) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        allocate(character(len=count) :: text)
        do i = 1, count
            text(i:i) = bytes(i)
        end do
        call immutable_manifest_parse(text, entries, ierr)
        if (ierr /= IMMUTABLE_OK) ierr = ACTION_RESULT_CORRUPT
        if (ierr == IMMUTABLE_OK) ierr = ACTION_RESULT_OK
    end subroutine read_result_tree

    subroutine write_record(c_root, c_key, record, ierr)
        character(kind=c_char), intent(in) :: c_root(:), c_key(:)
        character(len=*), intent(in) :: record
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: bytes(:)
        integer(c_int) :: rc
        integer :: i

        allocate(bytes(len(record)))
        do i = 1, len(record)
            bytes(i) = char(iachar(record(i:i)), kind=c_char)
        end do
        rc = c_action_write(c_root(1), c_key(1), bytes(1), &
            int(len(record), c_int))
        ierr = ACTION_RESULT_IO_ERROR
        if (rc == 0_c_int) ierr = ACTION_RESULT_OK
    end subroutine write_record

    subroutine to_c_text(text, c_text)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable, intent(out) :: c_text(:)
        integer :: i, n

        n = len_trim(text)
        allocate(c_text(n + 1))
        do i = 1, n
            c_text(i) = char(iachar(text(i:i)), kind=c_char)
        end do
        c_text(n + 1) = c_null_char
    end subroutine to_c_text

end module fx_action_result_store
