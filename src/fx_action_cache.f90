module fx_action_cache
    use fx_cache, only: cache_t, cache_init, cache_store, &
        cache_store_bytes
    use fx_cache_fs, only: cache_entry_path
    use fx_cache_key, only: HASH_LEN, cache_key_for, cache_source_tree_hash, &
        cache_digest, cache_file_digest, hash_mod_file, &
        cache_file_content_key, cache_set_file_hash_hook, &
        cache_clear_file_hash_hook
    use fx_string, only: to_lower
    use fx_action_cache_record, only: MAX_MOD_NAME, store_action_record, &
        restore_action_record, valid_smod_name
    use fx_immutable_manifest, only: immutable_tree_entry_t
    use fx_immutable_store, only: immutable_store_hash_file, &
        immutable_store_blob_path
    use fx_immutable_constants, only: IMMUTABLE_OK
    use fx_immutable_tree, only: immutable_store_verify_tree
    use fx_action_result_store, only: action_result_store_t, &
        action_result_read_t, action_result_read_acquire, &
        action_result_read_lookup, action_result_read_release, &
        action_result_store_init, &
        action_result_manifest_preview, action_result_preview_confirm, &
        action_result_publish_files, action_result_lookup, &
        action_result_materialize_blob, action_result_file_mode, &
        action_result_compile_action_key, &
        ACTION_RESULT_OK, ACTION_RESULT_MISSING, ACTION_RESULT_IO_ERROR
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
    public :: action_result_compile_action_key
    public :: cache_debug_write_action_record, cache_debug_corrupt_object_payload
    public :: cache_set_file_hash_hook, cache_clear_file_hash_hook

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
        root = trim(base)//'/store/v2'
    end subroutine action_cache_store_root

    subroutine action_cache_schema(schema)
        character(len=*), intent(out) :: schema

        schema = 'action-output-v2'
    end subroutine action_cache_schema

    subroutine action_cache_init(c, env_var, subdir, ierr)
        type(cache_t), intent(out) :: c
        character(len=*), intent(in) :: env_var, subdir
        integer, intent(out) :: ierr

        character(len=512) :: base

        ierr = 0
        call action_cache_root(env_var, subdir, base)
        call cache_init(c, trim(base)//'/store/v1')
        if (.not. c%initialized) ierr = 1
    end subroutine action_cache_init

    function cache_lookup(c, key) result(hit)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: key
        logical :: hit

        type(action_result_store_t) :: store
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=HASH_LEN) :: result_id
        integer :: ierr, init_status

        hit = .false.
        call init_result_store(c, store, init_status)
        if (init_status /= 0) return
        call lookup_or_import(c, store, key, entries, result_id, ierr)
        hit = ierr == ACTION_RESULT_OK
    end function cache_lookup

    subroutine init_result_store(c, store, ierr)
        type(cache_t), intent(in) :: c
        type(action_result_store_t), intent(out) :: store
        integer, intent(out) :: ierr
        character(len=512) :: root
        integer :: n

        ierr = 1
        if (.not. c%initialized) return
        n = len_trim(c%root_dir)
        if (n >= 9) then
            if (c%root_dir(n - 8:n) == '/store/v1') then
                root = c%root_dir(:n - 9)//'/store/v2'
            else
                root = trim(c%root_dir)//'/store/v2'
            end if
        else
            root = trim(c%root_dir)//'/store/v2'
        end if
        call action_result_store_init(store, trim(root), ierr)
    end subroutine init_result_store

    subroutine lookup_or_import(c, store, action_id, entries, result_id, ierr)
        type(cache_t), intent(in) :: c
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr

        call action_result_lookup(store, action_id, entries, result_id, ierr)
        if (ierr /= ACTION_RESULT_MISSING) return
        call import_legacy_compile_result(c, store, action_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        call action_result_lookup(store, action_id, entries, result_id, ierr)
    end subroutine lookup_or_import

    subroutine lookup_or_import_read(c, store, action_id, read, entries, &
            result_id, ierr)
        type(cache_t), intent(in) :: c
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(action_result_read_t), intent(out) :: read
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        integer :: release_status

        call action_result_read_acquire(store, action_id, read, ierr)
        if (ierr == ACTION_RESULT_MISSING) then
            call import_legacy_compile_result(c, store, action_id, ierr)
            if (ierr /= ACTION_RESULT_OK) return
            call action_result_read_acquire(store, action_id, read, ierr)
        end if
        if (ierr /= ACTION_RESULT_OK) return
        call action_result_read_lookup(store, read, entries, result_id, ierr)
        if (ierr == ACTION_RESULT_MISSING) then
            call action_result_read_release(store, read, release_status)
            if (release_status /= ACTION_RESULT_OK) then
                ierr = ACTION_RESULT_IO_ERROR
                return
            end if
            call import_legacy_compile_result(c, store, action_id, ierr)
            if (ierr /= ACTION_RESULT_OK) return
            call action_result_read_acquire(store, action_id, read, ierr)
            if (ierr /= ACTION_RESULT_OK) return
            call action_result_read_lookup(store, read, entries, result_id, ierr)
        end if
        if (ierr /= ACTION_RESULT_OK) then
            call action_result_read_release(store, read, release_status)
        end if
    end subroutine lookup_or_import_read

    subroutine import_legacy_compile_result(c, store, action_id, ierr)
        type(cache_t), intent(in) :: c
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        integer, intent(out) :: ierr
        character(len=HASH_LEN) :: old_output, object_key, mod_key, smod_key
        character(len=MAX_MOD_NAME) :: mod_name, smod_name
        character(len=512) :: paths(3)
        type(immutable_tree_entry_t) :: entries(3)
        character(len=HASH_LEN) :: actual
        character(len=HASH_LEN) :: result_id
        integer :: obj_size, mod_size, smod_size, n, size_bytes, status
        logical :: has_mod, has_smod

        call restore_action_record(c, action_id, old_output, object_key, &
            obj_size, mod_name, mod_key, mod_size, has_mod, smod_name, &
            smod_key, smod_size, has_smod, ierr)
        if (ierr /= 0) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        n = 1
        call cache_entry_path(c, trim(object_key)//'-d', paths(1))
        call cache_file_content_key(trim(paths(1)), 'object', actual, &
            size_bytes, status)
        if (status /= 0 .or. actual /= object_key .or. size_bytes /= obj_size) then
            ierr = ACTION_RESULT_MISSING
            return
        end if
        entries(1) = result_entry('object', 'object', 420)
        if (has_mod) then
            n = n + 1
            call cache_entry_path(c, trim(mod_key)//'-d', paths(n))
            call cache_file_content_key(trim(paths(n)), 'mod', actual, &
                size_bytes, status)
            if (status /= 0 .or. actual /= mod_key .or. size_bytes /= mod_size) then
                ierr = ACTION_RESULT_MISSING
                return
            end if
            entries(n) = result_entry('module-'//trim(mod_name), 'module', 420)
        end if
        if (has_smod) then
            n = n + 1
            call cache_entry_path(c, trim(smod_key)//'-d', paths(n))
            call cache_file_content_key(trim(paths(n)), 'smod', actual, &
                size_bytes, status)
            if (status /= 0 .or. actual /= smod_key .or. size_bytes /= smod_size) then
                ierr = ACTION_RESULT_MISSING
                return
            end if
            entries(n) = result_entry('smod-'//trim(smod_name), 'smod', 420)
        end if
        call action_result_publish_files(store, action_id, paths(1:n), &
            entries(1:n), result_id, ierr)
    end subroutine import_legacy_compile_result

    function result_entry(path, role, mode) result(entry)
        character(len=*), intent(in) :: path, role
        integer, intent(in) :: mode
        type(immutable_tree_entry_t) :: entry

        entry%path = path
        entry%role = role
        entry%mode = mode
    end function result_entry

    logical function result_has_smod(entries, required_name)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: required_name
        integer :: i

        result_has_smod = .false.
        do i = 1, size(entries)
            if (entries(i)%role /= 'smod') cycle
            if (entries(i)%path == 'smod-'//to_lower(trim(required_name))) &
                result_has_smod = .true.
        end do
    end function result_has_smod

    function compile_destination(entry, obj_path, mod_dir) result(path)
        type(immutable_tree_entry_t), intent(in) :: entry
        character(len=*), intent(in) :: obj_path, mod_dir
        character(len=:), allocatable :: path
        character(len=MAX_MOD_NAME) :: label

        path = ''
        if (entry%role == 'object') then
            path = trim(obj_path)
        else if (entry%role == 'module') then
            label = entry%path(8:)
            path = trim(mod_dir)//'/'//trim(label)//'.mod'
        else if (entry%role == 'smod') then
            label = entry%path(6:)
            path = trim(mod_dir)//'/'//trim(label)//'.smod'
        end if
    end function compile_destination

    logical function local_result_matches(entries, obj_path, mod_dir)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: obj_path, mod_dir
        integer :: i

        local_result_matches = .false.
        do i = 1, size(entries)
            if (.not. local_result_entry_matches(entries(i), obj_path, mod_dir)) return
        end do
        local_result_matches = .true.
    end function local_result_matches

    logical function local_result_entry_matches(entry, obj_path, mod_dir)
        type(immutable_tree_entry_t), intent(in) :: entry
        character(len=*), intent(in) :: obj_path, mod_dir
        character(len=HASH_LEN) :: actual
        character(len=:), allocatable :: path
        integer :: ierr, mode

        local_result_entry_matches = .false.
        path = compile_destination(entry, obj_path, mod_dir)
        if (len(path) == 0) return
        call immutable_store_hash_file(path, actual, ierr)
        if (ierr /= 0) return
        if (actual /= entry%object_id) return
        call action_result_file_mode(path, mode, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        local_result_entry_matches = mode == entry%mode
    end function local_result_entry_matches

    subroutine cache_restore_action(c, action_id, obj_path, mod_dir, restored, &
            output_id, required_smod_name)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir
        logical, intent(out) :: restored
        character(len=HASH_LEN), intent(out), optional :: output_id
        character(len=*), intent(in), optional :: required_smod_name

        type(action_result_store_t) :: store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=HASH_LEN) :: result_id
        character(len=HASH_LEN) :: old_output, old_object, old_mod, old_smod
        character(len=MAX_MOD_NAME) :: old_mod_name, old_smod_name
        integer :: ierr, i, init_status, release_status
        integer :: old_obj_size, old_mod_size, old_smod_size
        integer :: preview_status, verify_status
        logical :: local_ok, preview_ok
        logical :: old_has_mod, old_has_smod

        restored = .false.
        if (present(output_id)) output_id = ''
        if (.not. c%initialized) return

        call init_result_store(c, store, init_status)
        if (init_status /= 0) return

        call action_result_manifest_preview(store, action_id, entries, &
            result_id, ierr)
        preview_ok = ierr == ACTION_RESULT_OK
        if (preview_ok) then
            if (present(required_smod_name)) then
                if (len_trim(required_smod_name) > 0) then
                    preview_ok = result_has_smod(entries, required_smod_name)
                end if
            end if
        end if
        if (preview_ok) then
            preview_ok = local_result_matches(entries, obj_path, mod_dir)
        end if
        if (preview_ok) then
            call immutable_store_verify_tree(store%objects, result_id, &
                verify_status)
            if (verify_status == IMMUTABLE_OK) then
                call action_result_preview_confirm(store, action_id, result_id, &
                    preview_status)
                if (preview_status == ACTION_RESULT_OK) then
                    if (present(output_id)) then
                        output_id = result_id
                        call restore_action_record(c, action_id, old_output, &
                            old_object, old_obj_size, old_mod_name, old_mod, &
                            old_mod_size, old_has_mod, old_smod_name, old_smod, &
                            old_smod_size, old_has_smod, ierr)
                        if (ierr == 0) output_id = old_output
                    end if
                    restored = .true.
                    return
                end if
            end if
        end if

        call lookup_or_import_read(c, store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        if (present(required_smod_name)) then
            if (len_trim(required_smod_name) > 0) then
                if (.not. result_has_smod(entries, required_smod_name)) then
                    call action_result_read_release(store, read, release_status)
                    return
                end if
            end if
        end if
        if (present(output_id)) then
            output_id = result_id
            call restore_action_record(c, action_id, old_output, old_object, &
                old_obj_size, old_mod_name, old_mod, old_mod_size, old_has_mod, &
                old_smod_name, old_smod, old_smod_size, old_has_smod, ierr)
            if (ierr == 0) output_id = old_output
        end if
        local_ok = local_result_matches(entries, obj_path, mod_dir)
        if (local_ok) then
            call action_result_read_release(store, read, release_status)
            restored = release_status == ACTION_RESULT_OK
            return
        end if
        do i = 1, size(entries)
            if (local_result_entry_matches(entries(i), obj_path, mod_dir)) cycle
            call action_result_materialize_blob(store, entries(i)%object_id, &
                compile_destination(entries(i), obj_path, mod_dir), &
                entries(i)%mode, ierr)
            if (ierr /= ACTION_RESULT_OK) exit
        end do
        if (ierr /= ACTION_RESULT_OK) then
            call action_result_read_release(store, read, release_status)
            return
        end if
        restored = local_result_matches(entries, obj_path, mod_dir)
        call action_result_read_release(store, read, release_status)
        if (release_status /= ACTION_RESULT_OK) restored = .false.
    end subroutine cache_restore_action

    subroutine cache_store_action(c, action_id, obj_path, mod_dir, mod_name, &
            output_id, ierr, smod_name)
        type(cache_t), intent(inout) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir, mod_name
        character(len=HASH_LEN), intent(out) :: output_id
        integer, intent(out) :: ierr
        character(len=*), intent(in), optional :: smod_name

        character(len=HASH_LEN) :: object_key, mod_key, smod_key
        character(len=512) :: parts(6), sources(3)
        type(action_result_store_t) :: result_store
        type(immutable_tree_entry_t) :: entries(3)
        character(len=HASH_LEN) :: result_id
        character(len=:), allocatable :: lower_name, mod_path, smod_label, smod_path
        character(len=1) :: marker(1)
        integer :: obj_size, mod_size, smod_size, store_ierr, n_entries
        logical :: has_mod, has_smod

        output_id = ''
        ierr = 0
        if (.not. c%initialized) then
            ierr = 1
            return
        end if

        call cache_file_content_key(obj_path, 'object', object_key, obj_size, ierr)
        if (ierr /= 0) return

        lower_name = to_lower(trim(mod_name))
        if (len(lower_name) > MAX_MOD_NAME) then
            ierr = 1
            return
        end if
        mod_path = trim(mod_dir)//'/'//lower_name//'.mod'
        inquire (file=mod_path, exist=has_mod)
        mod_key = ''
        mod_size = 0
        if (has_mod) then
            call cache_file_content_key(mod_path, 'mod', mod_key, mod_size, ierr)
            if (ierr /= 0) return
        end if
        smod_label = ''
        if (present(smod_name)) smod_label = to_lower(trim(smod_name))
        has_smod = len(smod_label) > 0
        smod_key = ''
        smod_size = 0
        if (has_smod) then
            if (.not. valid_smod_name(smod_label)) then
                ierr = 1
                return
            end if
            smod_path = trim(mod_dir)//'/'//smod_label//'.smod'
            call cache_file_content_key(smod_path, 'smod', smod_key, smod_size, ierr)
            if (ierr /= 0) return
        end if

        parts(1) = 'fx-output-schema-2'
        parts(2) = object_key
        parts(3) = mod_key
        parts(4) = ''
        if (has_mod) parts(4) = lower_name
        parts(5) = smod_key
        parts(6) = smod_label
        output_id = cache_digest(parts, 6)

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
        if (has_smod) then
            call cache_store(c, trim(smod_key)//'-d', smod_path, store_ierr)
            if (store_ierr /= 0) then
                ierr = store_ierr
                return
            end if
        end if

        marker(1) = '1'
        call cache_store_bytes(c, trim(output_id)//'-d', marker, 1, ierr)
        if (ierr /= 0) return

        call store_action_record(c, action_id, output_id, object_key, obj_size, &
            lower_name, mod_key, mod_size, has_mod, smod_label, smod_key, &
            smod_size, has_smod, ierr)
        if (ierr /= 0) return

        n_entries = 1
        sources(1) = trim(obj_path)
        entries(1) = result_entry('object', 'object', 420)
        if (has_mod) then
            n_entries = n_entries + 1
            sources(n_entries) = mod_path
            entries(n_entries) = result_entry('module-'//trim(lower_name), &
                'module', 420)
        end if
        if (has_smod) then
            n_entries = n_entries + 1
            sources(n_entries) = smod_path
            entries(n_entries) = result_entry('smod-'//trim(smod_label), &
                'smod', 420)
        end if
        call init_result_store(c, result_store, store_ierr)
        if (store_ierr /= 0) then
            ierr = 1
            return
        end if
        call action_result_publish_files(result_store, action_id, &
            sources(1:n_entries), entries(1:n_entries), result_id, store_ierr)
        ierr = 1
        if (store_ierr == ACTION_RESULT_OK) ierr = 0
    end subroutine cache_store_action

    subroutine cache_action_mod_key(c, action_id, mod_key, found)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: mod_key
        logical, intent(out) :: found

        type(action_result_store_t) :: store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=HASH_LEN) :: result_id
        character(len=:), allocatable :: blob_path
        integer :: ierr, init_status, size_bytes, i, release_status

        mod_key = ''
        found = .false.
        if (.not. c%initialized) return

        call init_result_store(c, store, init_status)
        if (init_status /= 0) return
        call lookup_or_import_read(c, store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        do i = 1, size(entries)
            if (entries(i)%role /= 'module') cycle
            blob_path = immutable_store_blob_path(store%objects, &
                entries(i)%object_id)
            call cache_file_content_key(blob_path, 'mod', mod_key, size_bytes, ierr)
            if (ierr == 0) found = .true.
            exit
        end do
        call action_result_read_release(store, read, release_status)
        if (release_status /= ACTION_RESULT_OK) then
            mod_key = ''
            found = .false.
        end if
    end subroutine cache_action_mod_key

    subroutine cache_store_binary(c, action_id, bin_path, ierr)
        !! Publish the complete executable bytes as an immutable action result.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        integer, intent(out) :: ierr
        type(action_result_store_t) :: store
        type(immutable_tree_entry_t) :: entry(1)
        character(len=HASH_LEN) :: result_id
        character(len=512) :: sources(1)
        integer :: mode, init_status

        call init_result_store(c, store, init_status)
        ierr = 1
        if (init_status /= 0) return
        call action_result_file_mode(bin_path, mode, ierr)
        if (ierr /= ACTION_RESULT_OK) then
            ierr = 1
            return
        end if
        entry(1) = result_entry('program', 'executable', mode)
        sources(1) = bin_path
        call action_result_publish_files(store, action_id, sources, entry, &
            result_id, ierr)
        if (ierr == ACTION_RESULT_OK) then
            ierr = 0
        else
            ierr = 1
        end if
    end subroutine cache_store_binary

    subroutine cache_binary_matches(c, action_id, bin_path, matches)
        !! True iff bin_path exists and its (mtime, size) equal what the link
        !! record stored: the warm-build fast path leaving unchanged outputs
        !! untouched so a build that relinks nothing rewrites nothing.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, bin_path
        logical, intent(out) :: matches

        type(action_result_store_t) :: store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=HASH_LEN) :: result_id, actual_id
        integer :: ierr, mode, init_status, release_status

        matches = .false.
        call init_result_store(c, store, init_status)
        if (init_status /= 0) return
        call lookup_or_import_read(c, store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        if (size(entries) == 1) then
            if (entries(1)%role == 'executable') then
                call immutable_store_hash_file(bin_path, actual_id, ierr)
                if (ierr == 0) then
                    call action_result_file_mode(bin_path, mode, ierr)
                    if (ierr == ACTION_RESULT_OK) then
                        matches = actual_id == entries(1)%object_id .and. &
                            mode == entries(1)%mode
                    end if
                end if
            end if
        end if
        call action_result_read_release(store, read, release_status)
        if (release_status /= ACTION_RESULT_OK) matches = .false.
    end subroutine cache_binary_matches

    subroutine cache_restore_binary(c, action_id, dest_path, restored)
        !! Restore a cached binary for action_id to dest_path (raw bytes; the
        !! caller sets the execute bit). restored is .false. on any miss.
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, dest_path
        logical, intent(out) :: restored

        type(action_result_store_t) :: store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=HASH_LEN) :: result_id
        integer :: ierr, init_status, release_status

        restored = .false.
        call init_result_store(c, store, init_status)
        if (init_status /= 0) return
        call lookup_or_import_read(c, store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        if (size(entries) == 1) then
            if (entries(1)%role == 'executable') then
                call action_result_materialize_blob(store, entries(1)%object_id, &
                    dest_path, entries(1)%mode, ierr)
                if (ierr == ACTION_RESULT_OK) restored = .true.
            end if
        end if
        call action_result_read_release(store, read, release_status)
        if (release_status /= ACTION_RESULT_OK) restored = .false.
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

        character(len=HASH_LEN) :: out_id, object_key, mod_key, smod_key
        character(len=MAX_MOD_NAME) :: mod_label, smod_label
        character(len=1) :: bad(7)
        integer :: obj_size, mod_size, smod_size, i
        logical :: has_mod, has_smod

        call restore_action_record(c, action_id, out_id, object_key, obj_size, &
            mod_label, mod_key, mod_size, has_mod, smod_label, smod_key, &
            smod_size, has_smod, ierr)
        if (ierr /= 0) return
        do i = 1, size(bad)
            bad(i) = achar(iachar('0') + modulo(i, 10))
        end do
        call cache_store_bytes(c, trim(object_key)//'-d', bad, size(bad), ierr)
    end subroutine cache_debug_corrupt_object_payload

end module fx_action_cache
