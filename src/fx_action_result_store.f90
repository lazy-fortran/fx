module fx_action_result_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fx_hash, only: sha256_string
    use fx_cache_key, only: cache_digest
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_CORRUPT, &
        IMMUTABLE_MISSING
    use fx_immutable_manifest, only: immutable_tree_entry_t, &
        immutable_id_valid, immutable_manifest_parse, &
        immutable_entries_canonical, immutable_manifest_serialize
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        immutable_store_put_blob, &
        immutable_lease_t, immutable_store_publication_lease_acquire, &
        immutable_store_publication_commit, immutable_store_lease_release, &
        immutable_store_reason_release, &
        immutable_store_graph_read_lease_acquire, &
        immutable_store_materialize_blob, IMMUTABLE_MATERIALIZE_AUTO
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree
    use fx_immutable_owned, only: owned_open_store, owned_read_manifest, &
        owned_close
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

    !! Read handles keep the action's current and pending result graph leased.
    !! Release only after every restored output has been atomically replaced.
    type, public :: action_result_read_t
        private
        type(immutable_lease_t) :: graph_lease
        character(len=:), allocatable :: action_key
        character(len=:), allocatable :: store_root
        logical :: active = .false.
    end type action_result_read_t

    public :: action_result_store_init, action_result_put_blob
    public :: action_result_publish, action_result_publish_files
    public :: action_result_lookup, action_result_conflicts
    public :: action_result_read_acquire, action_result_read_lookup, &
        action_result_read_release
    public :: action_result_preview, action_result_preview_confirm
    public :: action_result_materialize_blob, action_result_action_key, &
        action_result_action_key_parts, action_result_compile_action_key
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
        store%root_dir = store%objects%root_dir
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
        character(len=HASH_LEN) :: root_ids(2)
        character(len=4) :: root_kinds(2)
        character(len=:), allocatable :: record, manifest
        type(immutable_tree_entry_t), allocatable :: sorted_entries(:)
        type(immutable_lease_t) :: publication_lease
        character(kind=c_char), allocatable :: c_root(:), c_key(:), c_record(:)
        character(kind=c_char) :: existing(RECORD_LIMIT)
        integer(c_int) :: lock, count, rc, unlock_rc
        integer :: verify_status, parse_status, root_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return

        call immutable_entries_canonical(entries, sorted_entries, verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        manifest = immutable_manifest_serialize(sorted_entries)
        result_id = sha256_string(manifest)
        root_kinds = 'tree'
        root_ids(1) = result_id
        call immutable_store_publication_lease_acquire(store%objects, key, &
            'fx-action-v1', 'publication', root_kinds(1:1), root_ids(1:1), &
            publication_lease, root_status)
        if (root_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        call immutable_store_put_tree(store%objects, sorted_entries, result_id, &
            verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            call immutable_store_lease_release(store%objects, publication_lease, &
                root_status)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if

        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        lock = c_action_lock(c_root, c_key)
        if (lock < 0_c_int) then
            call immutable_store_lease_release(store%objects, publication_lease, &
                root_status)
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
            if (ierr == ACTION_RESULT_OK) then
                call immutable_store_publication_commit(store%objects, &
                    publication_lease, 'bound', root_kinds(1:1), &
                    root_ids(1:1), root_status)
                if (root_status /= IMMUTABLE_OK) ierr = ACTION_RESULT_IO_ERROR
            else
                call immutable_store_lease_release(store%objects, &
                    publication_lease, root_status)
            end if
            return
        end if
        if (rc /= 0_c_int) then
            unlock_rc = c_action_unlock(lock)
            call immutable_store_lease_release(store%objects, publication_lease, &
                root_status)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if

        call action_result_record_parse(existing, int(count), key, current, ids, &
            parse_status)
        if (parse_status == ACTION_RECORD_CONFLICT) then
            unlock_rc = c_action_unlock(lock)
            root_ids = ids
            call immutable_store_publication_commit(store%objects, &
                publication_lease, 'conflict', root_kinds, root_ids, root_status)
            ierr = ACTION_RESULT_QUARANTINED
            if (root_status /= IMMUTABLE_OK) ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        if (parse_status /= ACTION_RECORD_BOUND) then
            unlock_rc = c_action_unlock(lock)
            call immutable_store_lease_release(store%objects, publication_lease, &
                root_status)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        if (current == result_id) then
            unlock_rc = c_action_unlock(lock)
            call immutable_store_publication_commit(store%objects, &
                publication_lease, 'bound', root_kinds(1:1), root_ids(1:1), &
                root_status)
            ierr = ACTION_RESULT_OK
            if (unlock_rc /= 0_c_int .or. root_status /= IMMUTABLE_OK) &
                ierr = ACTION_RESULT_IO_ERROR
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
        root_ids = ids
        if (ierr == ACTION_RESULT_OK) then
            call immutable_store_publication_commit(store%objects, &
                publication_lease, 'conflict', root_kinds, root_ids, root_status)
            if (root_status == IMMUTABLE_OK) then
                call immutable_store_reason_release(store%objects, key, &
                    'fx-action-v1', 'bound', root_status)
            else
                ierr = ACTION_RESULT_IO_ERROR
            end if
        else
            call immutable_store_lease_release(store%objects, publication_lease, &
                root_status)
        end if
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

    subroutine action_result_read_acquire(store, action_id, read, ierr)
        !! Acquire before checking or reading the action binding record.
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(action_result_read_t), intent(out) :: read
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: key
        integer :: lease_status

        read%active = .false.
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return
        call immutable_store_graph_read_lease_acquire(store%objects, key, &
            'fx-action-v1', 'action-read', read%graph_lease, lease_status)
        if (lease_status == IMMUTABLE_MISSING) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        if (lease_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        read%action_key = key
        read%store_root = store%root_dir
        read%active = .true.
        ierr = ACTION_RESULT_OK
    end subroutine action_result_read_acquire

    subroutine action_result_read_release(store, read, ierr)
        !! Release only this handle's token after all graph consumers are done.
        type(action_result_store_t), intent(in) :: store
        type(action_result_read_t), intent(inout) :: read
        integer, intent(out) :: ierr
        integer :: lease_status

        ierr = ACTION_RESULT_INVALID
        if (.not. read%active) return
        if (.not. store%initialized) return
        if (.not. allocated(read%store_root)) return
        if (read%store_root /= store%root_dir) return
        call immutable_store_lease_release(store%objects, read%graph_lease, &
            lease_status)
        if (lease_status == IMMUTABLE_OK .or. &
            lease_status == IMMUTABLE_MISSING) then
            read%active = .false.
            ierr = ACTION_RESULT_OK
        else
            ierr = ACTION_RESULT_IO_ERROR
        end if
    end subroutine action_result_read_release

    subroutine action_result_lookup(store, action_id, entries, result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        type(action_result_read_t) :: read
        integer :: release_status

        result_id = ''
        call action_result_read_acquire(store, action_id, read, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        call action_result_read_lookup(store, read, entries, result_id, ierr)
        call action_result_read_release(store, read, release_status)
        if (release_status /= ACTION_RESULT_OK .and. &
            ierr == ACTION_RESULT_OK) ierr = ACTION_RESULT_IO_ERROR
    end subroutine action_result_lookup

    subroutine action_result_preview(store, action_id, entries, result_id, ierr)
        !! Read a validated bound snapshot without acquiring a graph lease.
        !! Call action_result_preview_confirm after checking local outputs.
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: key, snapshot
        integer :: verify_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return
        call action_result_record_snapshot(store, key, snapshot, &
            .true., ACTION_RESULT_MISSING, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        call immutable_store_verify_tree(store%objects, snapshot, verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call read_result_tree(store, snapshot, entries, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        result_id = snapshot
    end subroutine action_result_preview

    subroutine action_result_preview_confirm(store, action_id, result_id, ierr)
        !! Confirm a preview still names the action's bound, non-conflicting result.
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id, result_id
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: key

        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        key = action_result_action_key(action_id)
        if (.not. immutable_id_valid(key)) return
        if (.not. immutable_id_valid(result_id)) return
        call action_result_record_confirm(store, key, result_id, &
            .true., ACTION_RESULT_MISSING, ierr)
    end subroutine action_result_preview_confirm

    subroutine action_result_record_snapshot(store, key, result_id, &
            check_exists, missing_status, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: key
        character(len=HASH_LEN), intent(out) :: result_id
        logical, intent(in) :: check_exists
        integer, intent(in) :: missing_status
        integer, intent(out) :: ierr
        character(kind=c_char), allocatable :: c_root(:), c_key(:)
        character(kind=c_char) :: record_bytes(RECORD_LIMIT)
        character(len=HASH_LEN) :: current, ids(2)
        integer(c_int) :: exists_status, lock, count, rc, unlock_status
        integer :: parse_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        if (.not. immutable_id_valid(key)) return
        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        if (check_exists) then
            exists_status = c_action_exists(c_root, c_key)
            if (exists_status == 1_c_int) then
                ierr = missing_status
                return
            end if
            if (exists_status /= 0_c_int) then
                ierr = ACTION_RESULT_CORRUPT
                return
            end if
        end if
        lock = c_action_lock(c_root, c_key)
        if (lock < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        rc = c_action_read(c_root, c_key, record_bytes, RECORD_LIMIT, count)
        if (rc /= 0_c_int) then
            unlock_status = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call action_result_record_parse(record_bytes, int(count), key, current, &
            ids, parse_status)
        unlock_status = c_action_unlock(lock)
        if (unlock_status /= 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        if (parse_status == ACTION_RECORD_CONFLICT) then
            ierr = ACTION_RESULT_QUARANTINED
            return
        end if
        if (parse_status /= ACTION_RECORD_BOUND) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        result_id = current
        ierr = ACTION_RESULT_OK
    end subroutine action_result_record_snapshot

    subroutine action_result_record_confirm(store, key, expected_id, &
            check_exists, missing_status, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: key, expected_id
        logical, intent(in) :: check_exists
        integer, intent(in) :: missing_status
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: current

        call action_result_record_snapshot(store, key, current, check_exists, &
            missing_status, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        if (current /= expected_id) ierr = ACTION_RESULT_MISSING
    end subroutine action_result_record_confirm

    subroutine action_result_read_lookup(store, read, entries, result_id, ierr)
        !! Verify and load the manifest while the graph read handle remains active.
        type(action_result_store_t), intent(in) :: store
        type(action_result_read_t), intent(in) :: read
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr

        character(len=HASH_LEN) :: key, snapshot
        integer :: verify_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        if (.not. read%active) return
        if (.not. allocated(read%action_key)) return
        if (.not. allocated(read%store_root)) return
        if (read%store_root /= store%root_dir) return
        key = read%action_key
        if (.not. immutable_id_valid(key)) return
        call action_result_record_snapshot(store, key, snapshot, &
            .true., ACTION_RESULT_MISSING, ierr)
        if (ierr /= ACTION_RESULT_OK) return

        ! Result verification and manifest loading can hash an arbitrarily
        ! large tree. Do that outside the per-action publication lock.
        call immutable_store_verify_tree(store%objects, snapshot, verify_status)
        if (verify_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_CORRUPT
            return
        end if
        call read_result_tree(store, snapshot, entries, ierr)
        if (ierr /= ACTION_RESULT_OK) return

        ! A second lock check provides the linearization point; conflicts win.
        call action_result_record_confirm(store, key, snapshot, &
            .false., ACTION_RESULT_CORRUPT, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        result_id = snapshot
        ierr = ACTION_RESULT_OK
    end subroutine action_result_read_lookup

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

    function action_result_action_key_parts(parts, n_parts) result(key)
        !! Build an action key from length-prefixed compiler/action inputs.
        !! Callers include flags, toolchain, runtime and oracle identities as
        !! separate parts so delimiters or embedded whitespace cannot alias.
        character(len=*), intent(in) :: parts(:)
        integer, intent(in) :: n_parts
        character(len=HASH_LEN) :: key
        character(len=max(len(parts), 32)) :: framed(size(parts) + 1)

        key = ''
        if (n_parts < 1 .or. n_parts > size(parts)) return
        framed(1) = 'fx-action-key-v2'
        framed(2:n_parts + 1) = parts(1:n_parts)
        key = cache_digest(framed, n_parts + 1)
    end function action_result_action_key_parts

    function action_result_compile_action_key(source_key, flags, toolchain, &
            runtime, oracle) result(key)
        !! Compile-action identity requires every execution and oracle input.
        character(len=*), intent(in) :: source_key, flags, toolchain, runtime, oracle
        character(len=HASH_LEN) :: key
        character(len=max(len(source_key), len(flags), len(toolchain), &
            len(runtime), len(oracle))) :: parts(5)

        key = ''
        if (len_trim(source_key) == 0 .or. len_trim(toolchain) == 0 .or. &
            len_trim(runtime) == 0 .or. len_trim(oracle) == 0) return
        parts(1) = trim(source_key)
        parts(2) = trim(flags)
        parts(3) = trim(toolchain)
        parts(4) = trim(runtime)
        parts(5) = trim(oracle)
        key = action_result_action_key_parts(parts, size(parts))
    end function action_result_compile_action_key

    subroutine read_result_tree(store, result_id, entries, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: result_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable :: text
        integer(c_int) :: root, cleanup
        integer :: verify_status

        ierr = ACTION_RESULT_CORRUPT
        root = owned_open_store(store%objects%root_dir//c_null_char)
        if (root < 0_c_int) return
        call owned_read_manifest(root, result_id, text, verify_status)
        cleanup = owned_close(root)
        if (verify_status /= IMMUTABLE_OK) return
        call immutable_manifest_parse(text, entries, verify_status)
        if (verify_status /= IMMUTABLE_OK) return
        ierr = ACTION_RESULT_OK
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
