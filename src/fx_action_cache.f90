module fx_action_cache
    use fx_cache, only: cache_t, cache_init
    use fx_cache_key, only: HASH_LEN, cache_key_for, cache_source_tree_hash, &
        cache_digest, cache_file_digest, hash_mod_file, &
        cache_file_content_key, cache_set_file_hash_hook, &
        cache_clear_file_hash_hook
    use fx_string, only: to_lower
    use fx_immutable_manifest, only: immutable_tree_entry_t
    use fx_immutable_store, only: immutable_store_hash_file, &
        immutable_store_blob_path
    use fx_action_result_store, only: action_result_store_t, &
        action_result_read_t, action_result_read_acquire, &
        action_result_read_lookup, action_result_read_release, &
        action_result_store_init, &
        action_result_preview, action_result_preview_confirm, &
        action_result_publish_files, action_result_lookup, &
        action_result_materialize_blob, action_result_file_mode, &
        action_result_compile_action_key, &
        ACTION_RESULT_OK
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
    public :: cache_set_file_hash_hook, cache_clear_file_hash_hook

    integer, parameter :: MAX_MOD_NAME = 257

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
        call cache_init(c, trim(base))
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
        call action_result_lookup(store, key, entries, result_id, ierr)
        hit = ierr == ACTION_RESULT_OK
    end function cache_lookup

    subroutine init_result_store(c, store, ierr)
        type(cache_t), intent(in) :: c
        type(action_result_store_t), intent(out) :: store
        integer, intent(out) :: ierr
        character(len=512) :: root
        ierr = 1
        if (.not. c%initialized) return
        root = trim(c%root_dir)//'/store/v2'
        call action_result_store_init(store, trim(root), ierr)
    end subroutine init_result_store

    subroutine lookup_result_read(store, action_id, read, entries, &
            result_id, ierr)
        type(action_result_store_t), intent(in) :: store
        character(len=*), intent(in) :: action_id
        type(action_result_read_t), intent(out) :: read
        type(immutable_tree_entry_t), allocatable, intent(out) :: entries(:)
        character(len=HASH_LEN), intent(out) :: result_id
        integer, intent(out) :: ierr
        integer :: release_status

        call action_result_read_acquire(store, action_id, read, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        call action_result_read_lookup(store, read, entries, result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) then
            call action_result_read_release(store, read, release_status)
        end if
    end subroutine lookup_result_read

    function result_entry(path, role, mode) result(entry)
        character(len=*), intent(in) :: path, role
        integer, intent(in) :: mode
        type(immutable_tree_entry_t) :: entry

        entry%path = path
        entry%role = role
        entry%mode = mode
    end function result_entry

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

    logical function valid_compile_entries(entries) result(valid)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=:), allocatable :: name
        integer :: i, objects

        valid = .false.
        objects = 0
        do i = 1, size(entries)
            select case (entries(i)%role)
            case ('object')
                if (entries(i)%path /= 'object') return
                objects = objects + 1
            case ('module')
                if (index(entries(i)%path, 'module-') /= 1) return
                name = entries(i)%path(8:)
                if (.not. valid_smod_name(name)) return
                if (index(name, '@') > 0) return
            case ('smod')
                if (index(entries(i)%path, 'smod-') /= 1) return
                name = entries(i)%path(6:)
                if (.not. valid_smod_name(name)) return
            case default
                return
            end select
        end do
        valid = objects == 1
    end function valid_compile_entries

    logical function result_has_artifacts(entries, names, role, prefix) result(found)
        type(immutable_tree_entry_t), intent(in) :: entries(:)
        character(len=*), intent(in) :: names(:), role, prefix
        integer :: i, j
        logical :: matched

        found = .false.
        do i = 1, size(names)
            if (.not. valid_smod_name(names(i))) return
            if (role == 'module' .and. index(names(i), '@') > 0) return
            matched = .false.
            do j = 1, size(entries)
                if (entries(j)%role /= role) cycle
                if (entries(j)%path == prefix//to_lower(trim(names(i)))) &
                    matched = .true.
            end do
            if (.not. matched) return
        end do
        found = .true.
    end function result_has_artifacts

    recursive subroutine compile_destination(entry, obj_path, mod_dir, path)
        ! An output argument keeps the allocation and its length with the caller.
        ! Deferred-length function results use shared compiler-generated lengths.
        type(immutable_tree_entry_t), intent(in) :: entry
        character(len=*), intent(in) :: obj_path, mod_dir
        character(len=:), allocatable, intent(out) :: path
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
    end subroutine compile_destination

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
        call compile_destination(entry, obj_path, mod_dir, path)
        if (len(path) == 0) return
        call immutable_store_hash_file(path, actual, ierr)
        if (ierr /= 0) return
        if (actual /= entry%object_id) return
        call action_result_file_mode(path, mode, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        local_result_entry_matches = mode == entry%mode
    end function local_result_entry_matches

    subroutine cache_restore_action(c, action_id, obj_path, mod_dir, &
            restored, output_id, required_mod_names, required_smod_names)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir
        logical, intent(out) :: restored
        character(len=HASH_LEN), intent(out), optional :: output_id
        character(len=*), intent(in), optional :: required_mod_names(:), &
            required_smod_names(:)

        type(action_result_store_t) :: store
        type(action_result_read_t) :: read
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: destination
        character(len=HASH_LEN) :: result_id
        integer :: ierr, i, init_status, release_status
        integer :: preview_status
        logical :: local_ok, preview_ok

        restored = .false.
        if (present(output_id)) output_id = ''
        if (.not. c%initialized) return

        call init_result_store(c, store, init_status)
        if (init_status /= 0) return

        call action_result_preview(store, action_id, entries, result_id, ierr)
        preview_ok = ierr == ACTION_RESULT_OK
        if (preview_ok) preview_ok = valid_compile_entries(entries)
        if (preview_ok .and. present(required_mod_names)) then
            preview_ok = result_has_artifacts(entries, required_mod_names, &
                'module', 'module-')
        end if
        if (preview_ok .and. present(required_smod_names)) then
            preview_ok = result_has_artifacts(entries, required_smod_names, &
                'smod', 'smod-')
        end if
        if (preview_ok) then
            preview_ok = local_result_matches(entries, obj_path, mod_dir)
        end if
        if (preview_ok) then
            call action_result_preview_confirm(store, action_id, result_id, &
                preview_status)
            if (preview_status == ACTION_RESULT_OK) then
                if (present(output_id)) output_id = result_id
                restored = .true.
                return
            end if
        end if

        call lookup_result_read(store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        if (.not. valid_compile_entries(entries)) then
            call action_result_read_release(store, read, release_status)
            return
        end if
        if (present(required_mod_names)) then
            if (.not. result_has_artifacts(entries, required_mod_names, &
                    'module', 'module-')) then
                call action_result_read_release(store, read, release_status)
                return
            end if
        end if
        if (present(required_smod_names)) then
            if (.not. result_has_artifacts(entries, required_smod_names, &
                    'smod', 'smod-')) then
                call action_result_read_release(store, read, release_status)
                return
            end if
        end if
        if (present(output_id)) output_id = result_id
        local_ok = local_result_matches(entries, obj_path, mod_dir)
        if (local_ok) then
            call action_result_read_release(store, read, release_status)
            restored = release_status == ACTION_RESULT_OK
            return
        end if
        do i = 1, size(entries)
            if (local_result_entry_matches(entries(i), obj_path, mod_dir)) cycle
            call compile_destination(entries(i), obj_path, mod_dir, destination)
            call action_result_materialize_blob(store, entries(i)%object_id, &
                destination, &
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

    subroutine cache_store_action(c, action_id, obj_path, mod_dir, module_names, &
            output_id, ierr, smod_names)
        type(cache_t), intent(in) :: c
        character(len=*), intent(in) :: action_id, obj_path, mod_dir, module_names(:)
        character(len=HASH_LEN), intent(out) :: output_id
        integer, intent(out) :: ierr
        character(len=*), intent(in), optional :: smod_names(:)

        character(len=512), allocatable :: sources(:)
        type(action_result_store_t) :: result_store
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: name
        integer :: store_ierr, n_entries, n_smod, i
        logical :: exists

        output_id = ''
        ierr = 1
        if (.not. c%initialized) return
        n_smod = 0
        if (present(smod_names)) n_smod = size(smod_names)
        allocate (sources(1 + size(module_names) + n_smod))
        allocate (entries(size(sources)))
        sources(1) = trim(obj_path)
        entries(1) = result_entry('object', 'object', 420)
        n_entries = 1
        do i = 1, size(module_names)
            name = to_lower(trim(module_names(i)))
            if (.not. valid_smod_name(name)) return
            if (index(name, '@') > 0) return
            n_entries = n_entries + 1
            sources(n_entries) = trim(mod_dir)//'/'//name//'.mod'
            inquire (file=trim(sources(n_entries)), exist=exists)
            if (.not. exists) return
            entries(n_entries) = result_entry('module-'//name, 'module', 420)
        end do
        do i = 1, n_smod
            name = to_lower(trim(smod_names(i)))
            if (len(name) == 0) cycle
            if (.not. valid_smod_name(name)) return
            n_entries = n_entries + 1
            sources(n_entries) = trim(mod_dir)//'/'//name//'.smod'
            entries(n_entries) = result_entry('smod-'//name, 'smod', 420)
        end do
        call init_result_store(c, result_store, store_ierr)
        if (store_ierr /= 0) return
        call action_result_publish_files(result_store, action_id, &
            sources(:n_entries), entries(:n_entries), output_id, store_ierr)
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
        integer :: ierr, init_status, size_bytes, i, n, release_status
        character(len=HASH_LEN), allocatable :: interface_keys(:)

        mod_key = ''
        found = .false.
        if (.not. c%initialized) return

        call init_result_store(c, store, init_status)
        if (init_status /= 0) return
        call lookup_result_read(store, action_id, read, entries, &
            result_id, ierr)
        if (ierr /= ACTION_RESULT_OK) return
        n = 0
        allocate (interface_keys(size(entries)))
        if (valid_compile_entries(entries)) then
            do i = 1, size(entries)
                if (entries(i)%role /= 'module' .and. entries(i)%role /= 'smod') cycle
                n = n + 1
                blob_path = immutable_store_blob_path(store%objects, entries(i)%object_id)
                call cache_file_content_key(blob_path, 'mod', interface_keys(n), &
                    size_bytes, ierr)
                if (ierr /= 0) exit
            end do
            if (n > 0 .and. ierr == 0) then
                mod_key = interface_keys(1)
                if (n > 1) mod_key = cache_digest(interface_keys, n)
                found = .true.
            end if
        end if
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
        call lookup_result_read(store, action_id, read, entries, &
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
        call lookup_result_read(store, action_id, read, entries, &
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

end module fx_action_cache
