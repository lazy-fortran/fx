module fx_action_result_store
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, &
        c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_string
    use fx_cache_key, only: cache_digest
    use fx_immutable_constants, only: IMMUTABLE_OK, IMMUTABLE_CORRUPT, &
        IMMUTABLE_MISSING
    use fx_immutable_manifest, only: immutable_tree_entry_t, &
        immutable_id_valid, immutable_manifest_parse, &
        immutable_entries_canonical, immutable_manifest_serialize
    use fx_immutable_store, only: immutable_store_t, immutable_store_init, &
        immutable_store_put_blob, immutable_store_hash_file, &
        immutable_lease_t, immutable_store_publication_lease_acquire, &
        immutable_store_publication_commit, immutable_store_lease_release, &
        immutable_store_reason_release, &
        immutable_store_graph_read_lease_acquire, &
        immutable_store_materialize_blob, IMMUTABLE_MATERIALIZE_AUTO
    use fx_immutable_tree, only: immutable_store_put_tree, &
        immutable_store_verify_tree
    use fx_immutable_owned, only: owned_open_store, owned_read_manifest, &
        owned_close
    use fx_immutable_gc, only: immutable_store_collect, IMMUTABLE_GC_CHANGED
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

    !! A sample exists only when this tick actually attempts CAS collection.
    !! Allocated bytes precede deletion; reclaimed bytes belong to that pass.
    !! CAS block counts exclude materializations and can include shared extents.
    type, public :: action_result_pressure_t
        logical :: sampled = .false.
        logical :: complete = .false.
        integer(int64) :: sampled_at = 0_int64
        integer :: status = IMMUTABLE_OK
        integer :: objects = 0
        integer(int64) :: allocated_bytes = 0_int64
        integer(int64) :: reclaimed_bytes = 0_int64
        integer(int64) :: pressure_bytes = 8589934592_int64
        integer :: pressure_objects = 100000
    end type action_result_pressure_t

    public :: action_result_store_init, action_result_put_blob
    public :: action_result_publish, action_result_publish_files
    public :: action_result_lookup, action_result_conflicts
    public :: action_result_read_acquire, action_result_read_lookup, &
        action_result_read_release
    public :: action_result_preview, action_result_preview_confirm
    public :: action_result_materialize_blob, action_result_action_key, &
        action_result_action_key_parts, action_result_compile_action_key
    public :: action_result_file_mode
    public :: action_result_retire, action_result_retire_key
    public :: action_result_maintenance_tick

    interface
        integer(c_int) function c_action_lock(root, action_id) &
                bind(C, name='fx_action_result_lock')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
        end function c_action_lock
        integer(c_int) function c_action_try_lock(root, action_id) &
                bind(C, name='fx_action_result_try_lock')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
        end function c_action_try_lock
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
        integer(c_int) function c_action_read_touch(root, action_id, bytes, &
                capacity, count) bind(C, name='fx_action_result_read_touch')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
            character(kind=c_char), intent(out) :: bytes(*)
            integer(c_int), value :: capacity
            integer(c_int), intent(out) :: count
        end function c_action_read_touch
        integer(c_int) function c_action_write(root, action_id, bytes, count) &
                bind(C, name='fx_action_result_write')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*), bytes(*)
            integer(c_int), value :: count
        end function c_action_write
        integer(c_int) function c_action_age(root, action_id, min_age) &
                bind(C, name='fx_action_result_age')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*), action_id(*)
            integer(c_long_long), value :: min_age
        end function c_action_age
        integer(c_int) function c_action_remove(root, action_id) &
                bind(C, name='fx_action_result_remove')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*), action_id(*)
        end function c_action_remove
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
        integer(c_int) function c_maintenance_begin(root) &
                bind(C, name='fx_action_maintenance_begin')
            import c_char, c_int
            character(kind=c_char), intent(in) :: root(*)
        end function c_maintenance_begin
        integer(c_int) function c_maintenance_end(handle) &
                bind(C, name='fx_action_maintenance_end')
            import c_int
            integer(c_int), value :: handle
        end function c_maintenance_end
        integer(c_int) function c_maintenance_due(root, interval) &
                bind(C, name='fx_action_maintenance_due')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_long_long), value :: interval
        end function c_maintenance_due
        integer(c_int) function c_maintenance_load(root, shard, offset, &
                last_scan, last_gc) bind(C, name='fx_action_maintenance_load')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(out) :: shard
            integer(c_long_long), intent(out) :: offset, last_scan, last_gc
        end function c_maintenance_load
        integer(c_int) function c_maintenance_save(root, shard, offset, &
                last_scan, last_gc) bind(C, name='fx_action_maintenance_save')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), value :: shard
            integer(c_long_long), value :: offset, last_scan, last_gc
        end function c_maintenance_save
        integer(c_int) function c_maintenance_next(root, shard, offset, &
                budget, key) bind(C, name='fx_action_maintenance_next')
            import c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_int), intent(inout) :: shard, budget
            integer(c_long_long), intent(inout) :: offset
            character(kind=c_char), intent(out) :: key(*)
        end function c_maintenance_next
        integer(c_long_long) function c_maintenance_now() &
                bind(C, name='fx_action_maintenance_now')
            import c_long_long
        end function c_maintenance_now
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

    subroutine action_result_put_blob(store, source_path, blob_id, ierr, protected_by)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: source_path
        character(len=HASH_LEN), intent(out) :: blob_id
        integer, intent(out) :: ierr
        type(immutable_lease_t), intent(in), optional :: protected_by

        blob_id = ''
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized) return
        call immutable_store_put_blob(store%objects, trim(source_path), blob_id, &
            ierr, protected_by)
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
            if (ierr == ACTION_RESULT_OK) then
                call immutable_store_publication_commit(store%objects, &
                    publication_lease, 'bound', root_kinds(1:1), &
                    root_ids(1:1), root_status)
                if (root_status /= IMMUTABLE_OK) ierr = ACTION_RESULT_IO_ERROR
            else
                call immutable_store_lease_release(store%objects, &
                    publication_lease, root_status)
            end if
            unlock_rc = c_action_unlock(lock)
            if (unlock_rc /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
            if (ierr == ACTION_RESULT_OK) call maybe_maintain_after_publish(store)
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
            root_ids = ids
            call immutable_store_publication_commit(store%objects, &
                publication_lease, 'conflict', root_kinds, root_ids, root_status)
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_QUARANTINED
            if (root_status /= IMMUTABLE_OK .or. unlock_rc /= 0_c_int) &
                ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        if (record_retired(existing, int(count), key)) then
            call immutable_store_reason_release(store%objects, key, &
                'fx-action-v1', 'bound', root_status)
            if (root_status == IMMUTABLE_OK) then
                record = action_result_bound_record(key, result_id)
                call write_record(c_root, c_key, record, ierr)
                if (ierr == ACTION_RESULT_OK) then
                    call immutable_store_publication_commit(store%objects, &
                        publication_lease, 'bound', root_kinds(1:1), &
                        root_ids(1:1), root_status)
                    if (root_status /= IMMUTABLE_OK) ierr = ACTION_RESULT_IO_ERROR
                end if
            else
                ierr = ACTION_RESULT_IO_ERROR
            end if
            if (publication_lease%active) &
                call immutable_store_lease_release(store%objects, &
                    publication_lease, root_status)
            unlock_rc = c_action_unlock(lock)
            if (unlock_rc /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
            if (ierr == ACTION_RESULT_OK) call maybe_maintain_after_publish(store)
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
            call immutable_store_publication_commit(store%objects, &
                publication_lease, 'bound', root_kinds(1:1), root_ids(1:1), &
                root_status)
            unlock_rc = c_action_unlock(lock)
            ierr = ACTION_RESULT_OK
            if (unlock_rc /= 0_c_int .or. root_status /= IMMUTABLE_OK) &
                ierr = ACTION_RESULT_IO_ERROR
            if (ierr == ACTION_RESULT_OK) call maybe_maintain_after_publish(store)
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
        unlock_rc = c_action_unlock(lock)
        if (ierr == ACTION_RESULT_OK .and. unlock_rc == 0_c_int) &
            ierr = ACTION_RESULT_CONFLICT
        if (unlock_rc /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
    end subroutine action_result_publish

    subroutine maybe_maintain_after_publish(store)
        type(action_result_store_t), intent(in) :: store
        integer(c_int) :: due
        integer :: scanned, retired, deleted, maintenance_status

        due = c_maintenance_due(store%root_dir//c_null_char, 60_c_long_long)
        if (due /= 1_c_int) return
        call action_result_maintenance_tick(store, scanned, retired, deleted, &
            maintenance_status)
        ! Housekeeping status is independent of the durable publish result.
    end subroutine maybe_maintain_after_publish

    subroutine action_result_retire(store, action_id, min_age_seconds, retired, ierr)
        !! Retire one action identified by its external action ID.
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        integer, intent(in) :: min_age_seconds
        logical, intent(out) :: retired
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: key

        key = action_result_action_key(action_id)
        call action_result_retire_key(store, key, min_age_seconds, &
            retired, ierr)
    end subroutine action_result_retire

    subroutine action_result_retire_key(store, key, min_age_seconds, retired, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: key
        integer, intent(in) :: min_age_seconds
        logical, intent(out) :: retired
        integer, intent(out) :: ierr

        call retire_key_impl(store, key, min_age_seconds, retired, ierr, .false.)
    end subroutine action_result_retire_key

    subroutine retire_key_impl(store, key, min_age_seconds, retired, &
            ierr, nonblocking)
        !! Retire one old bound record. A durable marker closes the crash gap
        !! between hiding the binding and releasing its root. Retry this call
        !! on a marker to finish interrupted cleanup, regardless of its age.
        !! The key is the exact 64-hex action filename for bounded scanners.
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: key
        integer, intent(in) :: min_age_seconds
        logical, intent(out) :: retired
        integer, intent(out) :: ierr
        logical, intent(in) :: nonblocking
        character(len=HASH_LEN) :: bound, ids(2)
        character(len=:), allocatable :: marker
        character(kind=c_char), allocatable :: c_root(:), c_key(:)
        character(kind=c_char) :: bytes(RECORD_LIMIT)
        integer(c_int) :: lock, count, rc, age, unlock_rc
        integer :: parse_status, root_status

        retired = .false.
        ierr = ACTION_RESULT_INVALID
        if (.not. store%initialized .or. min_age_seconds < 0) return
        if (.not. immutable_id_valid(key)) return
        call to_c_text(store%root_dir, c_root)
        call to_c_text(key, c_key)
        if (nonblocking) then
            lock = c_action_try_lock(c_root, c_key)
        else
            lock = c_action_lock(c_root, c_key)
        end if
        if (lock == -2_c_int) then
            ierr = ACTION_RESULT_OK
            return
        end if
        if (lock < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        rc = c_action_read(c_root, c_key, bytes, RECORD_LIMIT, count)
        if (rc == 1_c_int) then
            ierr = ACTION_RESULT_MISSING
        else if (rc /= 0_c_int) then
            ierr = ACTION_RESULT_CORRUPT
        else if (record_retired(bytes, int(count), key)) then
            call immutable_store_reason_release(store%objects, key, &
                'fx-action-v1', 'bound', root_status)
            ierr = ACTION_RESULT_IO_ERROR
            if (root_status == IMMUTABLE_OK) then
                rc = c_action_remove(c_root, c_key)
                if (rc == 0_c_int) then
                    retired = .true.
                    ierr = ACTION_RESULT_OK
                end if
            end if
        else
            call action_result_record_parse(bytes, int(count), key, bound, &
                ids, parse_status)
            if (parse_status == ACTION_RECORD_CONFLICT) then
                ierr = ACTION_RESULT_QUARANTINED
            else if (parse_status /= ACTION_RECORD_BOUND) then
                ierr = ACTION_RESULT_CORRUPT
            else
                age = c_action_age(c_root, c_key, int(min_age_seconds, c_long_long))
                if (age == 2_c_int) then
                    ierr = ACTION_RESULT_OK
                else if (age == 1_c_int) then
                    ierr = ACTION_RESULT_MISSING
                else if (age /= 0_c_int) then
                    ierr = ACTION_RESULT_IO_ERROR
                else
                    marker = retired_record(key)
                    call write_record(c_root, c_key, marker, ierr)
                    if (ierr == ACTION_RESULT_OK) then
                        call immutable_store_reason_release(store%objects, &
                            key, 'fx-action-v1', 'bound', root_status)
                        if (root_status /= IMMUTABLE_OK) then
                            ierr = ACTION_RESULT_IO_ERROR
                        else
                            rc = c_action_remove(c_root, c_key)
                            if (rc /= 0_c_int) then
                                ierr = ACTION_RESULT_IO_ERROR
                            else
                                retired = .true.
                            end if
                        end if
                    end if
                end if
            end if
        end if
        unlock_rc = c_action_unlock(lock)
        if (unlock_rc /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
    end subroutine retire_key_impl

    subroutine action_result_maintenance_tick(store, scanned, retired, deleted, &
            ierr, max_scan, min_age_seconds, pressure_bytes, pressure_objects, &
            max_delete, gc_interval_seconds, pressure)
        !! Run a persistent sweep. max_scan caps action directory entries and
        !! shard steps; GC separately inventories up to one million CAS objects.
        !! A busy owner skips without waiting. Fo calls this at owner start/stop.
        type(action_result_store_t), intent(in) :: store
        integer, intent(out) :: scanned, retired, deleted, ierr
        integer, intent(in), optional :: max_scan, pressure_objects, max_delete
        integer(int64), intent(in), optional :: min_age_seconds, &
            pressure_bytes, gc_interval_seconds
        type(action_result_pressure_t), intent(out), optional :: pressure
        integer(c_int) :: owner, shard, budget, status, end_status
        integer(c_long_long) :: offset, last_scan, last_gc, now
        character(kind=c_char) :: key_bytes(65)
        character(len=HASH_LEN) :: key
        integer :: scan_cap, delete_cap, object_cap, i, retire_status
        integer :: objects_scanned, collected_status
        integer(int64) :: age_floor, byte_cap, gc_interval
        integer(int64) :: allocated_bytes, reclaimed_bytes
        logical :: did_retire

        scanned = 0
        retired = 0
        deleted = 0
        ierr = ACTION_RESULT_INVALID
        if (present(pressure)) pressure = action_result_pressure_t()
        if (.not. store%initialized) return
        scan_cap = 64
        if (present(max_scan)) scan_cap = max_scan
        delete_cap = 32
        if (present(max_delete)) delete_cap = max_delete
        object_cap = 100000
        if (present(pressure_objects)) object_cap = pressure_objects
        age_floor = 30_int64 * 24_int64 * 3600_int64
        if (present(min_age_seconds)) age_floor = min_age_seconds
        byte_cap = 8_int64 * 1024_int64 * 1024_int64 * 1024_int64
        if (present(pressure_bytes)) byte_cap = pressure_bytes
        gc_interval = 3600_int64
        if (present(gc_interval_seconds)) gc_interval = gc_interval_seconds
        if (present(pressure)) then
            pressure%pressure_bytes = byte_cap
            pressure%pressure_objects = object_cap
        end if
        if (scan_cap < 1 .or. scan_cap > 4096 .or. delete_cap < 0 .or. &
            delete_cap > 4096 .or. object_cap < 0 .or. age_floor < 0 .or. &
            byte_cap < 0 .or. gc_interval < 0 .or. &
            age_floor > int(huge(0), int64)) return
        owner = c_maintenance_begin(store%root_dir//c_null_char)
        if (owner == -2_c_int) then
            ierr = ACTION_RESULT_OK
            return
        end if
        if (owner < 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        ierr = ACTION_RESULT_IO_ERROR
        status = c_maintenance_load(store%root_dir//c_null_char, shard, &
            offset, last_scan, last_gc)
        if (status /= 0_c_int) goto 900
        now = c_maintenance_now()
        if (now < 0_c_long_long) goto 900
        budget = int(scan_cap, c_int)
        do while (budget > 0_c_int)
            status = c_maintenance_next(store%root_dir//c_null_char, &
                shard, offset, budget, key_bytes)
            if (status < 0_c_int) goto 900
            if (status == 0_c_int) exit
            do i = 1, HASH_LEN
                key(i:i) = key_bytes(i)
            end do
            scanned = scanned + 1
            call retire_key_impl(store, key, int(age_floor), &
                did_retire, retire_status, .true.)
            if (retire_status /= ACTION_RESULT_OK .and. &
                retire_status /= ACTION_RESULT_MISSING .and. &
                retire_status /= ACTION_RESULT_QUARANTINED) goto 900
            if (did_retire) retired = retired + 1
        end do
        last_scan = now
        status = c_maintenance_save(store%root_dir//c_null_char, shard, &
            offset, last_scan, last_gc)
        if (status /= 0_c_int) goto 900
        if (now - last_gc >= int(gc_interval, c_long_long)) then
            call immutable_store_collect(store%objects, 1000000, delete_cap, &
                age_floor, byte_cap, object_cap, objects_scanned, &
                allocated_bytes, deleted, reclaimed_bytes, collected_status)
            if (present(pressure)) then
                pressure%sampled = .true.
                pressure%complete = collected_status == IMMUTABLE_OK
                pressure%sampled_at = int(now, int64)
                pressure%status = collected_status
                pressure%objects = objects_scanned
                pressure%allocated_bytes = allocated_bytes
                pressure%reclaimed_bytes = reclaimed_bytes
            end if
            if (collected_status == IMMUTABLE_GC_CHANGED) then
                ierr = ACTION_RESULT_OK
                goto 900
            end if
            if (collected_status /= IMMUTABLE_OK) goto 900
            last_gc = now
            status = c_maintenance_save(store%root_dir//c_null_char, shard, &
                offset, last_scan, last_gc)
            if (status /= 0_c_int) goto 900
        end if
        ierr = ACTION_RESULT_OK
900     end_status = c_maintenance_end(owner)
        if (end_status /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
    end subroutine action_result_maintenance_tick

    function retired_record(key) result(record)
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: record

        record = 'FXACTION2'//achar(10)//trim(key)//achar(10)// &
            'RETIRED'//achar(10)
    end function retired_record

    logical function record_retired(bytes, count, key)
        character(kind=c_char), intent(in) :: bytes(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: key
        character(len=:), allocatable :: marker
        integer :: i

        record_retired = .false.
        marker = retired_record(key)
        if (count /= len(marker)) return
        do i = 1, count
            if (bytes(i) /= marker(i:i)) return
        end do
        record_retired = .true.
    end function record_retired

    subroutine action_result_publish_files(store, action_id, source_paths, &
            entries, result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id, source_paths(:)
        type(immutable_tree_entry_t), intent(inout) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        type(immutable_lease_t) :: outputs_lease
        character(len=4), allocatable :: kinds(:)
        character(len=HASH_LEN), allocatable :: ids(:)
        integer :: i, lease_status

        result_id = ''
        ierr = ACTION_RESULT_INVALID
        if (size(source_paths) /= size(entries)) return
        if (size(entries) == 0) then
            call action_result_publish(store, action_id, entries, result_id, ierr)
            return
        end if
        allocate(kinds(size(entries)), ids(size(entries)))
        kinds = 'blob'
        ! Keep every output protected until the action graph is committed.
        do i = 1, size(entries)
            call immutable_store_hash_file(trim(source_paths(i)), ids(i), ierr)
            if (ierr /= IMMUTABLE_OK) then
                ierr = ACTION_RESULT_IO_ERROR
                return
            end if
        end do
        call immutable_store_publication_lease_acquire(store%objects, &
            'fx-action-files', store%objects%writer_start, 'outputs', kinds, &
            ids, outputs_lease, lease_status)
        if (lease_status /= IMMUTABLE_OK) then
            ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        do i = 1, size(entries)
            call action_result_put_blob(store, trim(source_paths(i)), &
                entries(i)%object_id, ierr, outputs_lease)
            if (ierr /= ACTION_RESULT_OK) exit
        end do
        if (ierr == ACTION_RESULT_OK) &
            call action_result_publish(store, action_id, entries, result_id, ierr)
        call immutable_store_lease_release(store%objects, outputs_lease, &
            lease_status)
        if (ierr == ACTION_RESULT_OK .and. lease_status /= IMMUTABLE_OK) &
            ierr = ACTION_RESULT_IO_ERROR
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
        rc = c_action_read_touch(c_root, c_key, record_bytes, RECORD_LIMIT, count)
        if (rc /= 0_c_int) then
            unlock_status = c_action_unlock(lock)
            ierr = missing_status
            if (rc /= 1_c_int) ierr = ACTION_RESULT_CORRUPT
            return
        end if
        if (record_retired(record_bytes, int(count), key)) then
            unlock_status = c_action_unlock(lock)
            ierr = ACTION_RESULT_MISSING
            if (unlock_status /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        call action_result_record_parse(record_bytes, int(count), key, current, &
            ids, parse_status)
        if (parse_status == ACTION_RECORD_CONFLICT) then
            unlock_status = c_action_unlock(lock)
            ierr = ACTION_RESULT_QUARANTINED
            if (unlock_status /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        if (parse_status /= ACTION_RECORD_BOUND) then
            unlock_status = c_action_unlock(lock)
            ierr = ACTION_RESULT_CORRUPT
            if (unlock_status /= 0_c_int) ierr = ACTION_RESULT_IO_ERROR
            return
        end if
        unlock_status = c_action_unlock(lock)
        if (unlock_status /= 0_c_int) then
            ierr = ACTION_RESULT_IO_ERROR
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
