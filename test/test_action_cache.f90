program test_action_cache
    use fx_action_cache, only: cache_t, HASH_LEN, action_cache_root, &
        cache_store_action, cache_restore_action, cache_lookup, &
        cache_action_mod_key, cache_store_binary, cache_binary_matches, &
        cache_restore_binary, &
        cache_source_tree_hash, cache_set_file_hash_hook, &
        cache_clear_file_hash_hook, cache_debug_write_action_record, &
        cache_debug_corrupt_object_payload
    use fx_cache, only: cache_init, cache_store_bytes
    use fx_cache_key, only: cache_file_content_key
    use fx_cache_fs, only: cache_entry_path, CACHE_PATH_LEN
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_str, test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite
    integer :: hook_calls = 0

    call test_suite_init(suite, 'fx_action_cache')
    call test_root_resolution(suite)
    call test_action_round_trip(suite)
    call test_smod_round_trip(suite)
    call test_smod_payload_rejection(suite)
    call test_smod_metadata_rejection(suite)
    call test_binary_fingerprint(suite)
    call test_legacy_nopayload_rejection(suite)
    call test_validated_legacy_import(suite)
    call test_file_hash_hook(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_root_resolution(suite)
        type(test_suite_t), intent(inout) :: suite
        character(len=512) :: root

        call action_cache_root('FX_ABSENT_ENV_9db3', 'mytool', root)
        call test_assert(suite, &
            index(trim(root), '/.cache/mytool') > 0, &
            'action_cache_root falls back to HOME/.cache/<subdir>')
    end subroutine test_root_resolution

    subroutine test_action_round_trip(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: store_c, restore_c
        character(len=:), allocatable :: root, src_dir, dst_dir
        character(len=:), allocatable :: obj_path, mod_path, obj2, mod2
        character(len=HASH_LEN) :: out_id, out_id2, mkey, mkey_after
        character(len=CACHE_PATH_LEN) :: mod_payload_path
        integer :: ierr
        logical :: restored, hit, found

        root = temp_root('action')
        src_dir = root//'/src'
        dst_dir = root//'/dst'
        call cleanup_tree(root)
        call make_dir(src_dir)
        call make_dir(dst_dir)

        obj_path = src_dir//'/widget.o'
        mod_path = src_dir//'/widget.mod'
        call write_file(obj_path, 'OBJECT-PAYLOAD-12345')
        call write_file(mod_path, 'MODULE-PAYLOAD-67890')

        call cache_init(store_c, root//'/cache')
        call cache_store_action(store_c, 'act-widget', obj_path, src_dir, &
            'Widget', out_id, ierr)
        call test_assert(suite, ierr == 0, 'action store succeeds')
        call test_assert(suite, len_trim(out_id) == HASH_LEN, &
            'store returns full-length output id')

        call cache_init(restore_c, root//'/cache')
        hit = cache_lookup(restore_c, 'act-widget')
        call test_assert(suite, hit, 'fresh cache reports action hit')

        call cache_action_mod_key(restore_c, 'act-widget', mkey, found)
        call test_assert(suite, found .and. len_trim(mkey) == HASH_LEN, &
            'mod key recovered from action record')
        call cache_entry_path(restore_c, trim(mkey)//'-d', mod_payload_path)
        call delete_file(mod_payload_path)
        call cache_action_mod_key(restore_c, 'act-widget', mkey_after, found)
        call test_assert(suite, found .and. mkey_after == mkey, &
            'module dependency key comes from the immutable result')

        obj2 = dst_dir//'/widget.o'
        mod2 = dst_dir//'/widget.mod'
        call cache_restore_action(restore_c, 'act-widget', obj2, dst_dir, &
            restored, out_id2)
        call test_assert(suite, restored, 'restore into fresh dir succeeds')
        call test_assert_equal_str(suite, trim(out_id), trim(out_id2), &
            'restored output id matches stored id')
        call test_assert(suite, files_equal(obj_path, obj2), &
            'restored object payload matches original')
        call test_assert(suite, files_equal(mod_path, mod2), &
            'restored mod payload matches original')

        call cleanup_tree(root)
    end subroutine test_action_round_trip

    subroutine test_smod_round_trip(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, stem, obj_path, mod_path, smod_path
        character(len=HASH_LEN) :: first_id, second_id
        integer :: ierr
        logical :: restored

        root = temp_root('smod')
        call cleanup_tree(root)
        call make_dir(root)
        obj_path = root//'/root.o'
        mod_path = root//'/root.mod'
        ! Exercise a composite ancestor@child name beyond the old 132-byte limit.
        stem = repeat('a', 120)//'@'//repeat('b', 120)
        smod_path = root//'/'//stem//'.smod'
        call write_file(obj_path, 'OBJECT-BYTES')
        call write_file(mod_path, 'MODULE-BYTES')
        call write_file(smod_path, 'SUBMODULE-BYTES-ONE')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'smod-first', obj_path, root, 'Root', &
            first_id, ierr, stem)
        call test_assert(suite, ierr == 0, 'store object, mod and long-name smod')
        call delete_file(obj_path)
        call delete_file(mod_path)
        call delete_file(smod_path)
        call cache_restore_action(c, 'smod-first', obj_path, root, restored, &
            required_smod_name=stem)
        call test_assert(suite, restored, 'restore all deleted compiler outputs')
        call test_assert(suite, file_has_bytes(obj_path, 'OBJECT-BYTES'), &
            'restored object bytes match the independent payload')
        call test_assert(suite, file_has_bytes(mod_path, 'MODULE-BYTES'), &
            'restored module bytes match the independent payload')
        call test_assert(suite, file_has_bytes(smod_path, 'SUBMODULE-BYTES-ONE'), &
            'restored submodule bytes match the independent payload')
        call write_file(smod_path, 'SUBMODULE-BYTES-TWO')
        call cache_store_action(c, 'smod-second', obj_path, root, 'Root', &
            second_id, ierr, stem)
        call test_assert(suite, ierr == 0 .and. first_id /= second_id, &
            'submodule content participates in the output identity')
        call cache_restore_action(c, 'smod-first', obj_path, root, restored, &
            required_smod_name=stem)
        call test_assert(suite, restored, 'repair changed local submodule output')
        call test_assert(suite, file_has_bytes(smod_path, 'SUBMODULE-BYTES-ONE'), &
            'repair restores the recorded submodule content')
        call cleanup_tree(root)
    end subroutine test_smod_round_trip

    subroutine test_smod_payload_rejection(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, obj_path, smod_path
        character(len=CACHE_PATH_LEN) :: payload_path
        character(len=HASH_LEN) :: out_id, smod_key
        integer :: ierr, smod_size
        logical :: restored

        root = temp_root('smod-payload')
        call cleanup_tree(root)
        call make_dir(root)
        obj_path = root//'/child.o'
        smod_path = root//'/root@child.smod'
        call write_file(obj_path, 'CHILD-OBJECT')
        call write_file(smod_path, 'CHILD-INTERFACE')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'smod-child', obj_path, root, '', &
            out_id, ierr, 'root@child')
        call test_assert(suite, ierr == 0, 'store child submodule without a mod')
        call cache_restore_action(c, 'smod-child', obj_path, root, restored, &
            required_smod_name='ROOT@CHILD')
        call test_assert(suite, restored, 'required submodule name is case insensitive')
        call cache_restore_action(c, 'smod-child', obj_path, root, restored, &
            required_smod_name='other@child')
        call test_assert(suite, .not. restored, 'required submodule must match the record')
        call cache_file_content_key(smod_path, 'smod', smod_key, smod_size, ierr)
        call cache_entry_path(c, trim(smod_key)//'-d', payload_path)
        call delete_file(payload_path)
        call test_assert(suite, cache_lookup(c, 'smod-child'), &
            'versioned result survives removal of the legacy copy')
        call cache_restore_action(c, 'smod-child', obj_path, root, restored, &
            required_smod_name='root@child')
        call test_assert(suite, restored, &
            'complete result does not depend on the legacy payload')
        call cache_store_action(c, 'smod-child', obj_path, root, '', &
            out_id, ierr, 'root@child')
        call write_file(payload_path, 'CORRUPT-INTERFACE')
        call delete_file(smod_path)
        call cache_restore_action(c, 'smod-child', obj_path, root, restored, &
            required_smod_name='root@child')
        call test_assert(suite, restored, 'result restores despite corrupt legacy copy')
        call test_assert(suite, file_has_bytes(smod_path, 'CHILD-INTERFACE'), &
            'restored submodule bytes come from the immutable result')
        call cleanup_tree(root)
    end subroutine test_smod_payload_rejection

    subroutine test_smod_metadata_rejection(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, obj_path, record
        character(len=HASH_LEN) :: out_id, object_key
        character(len=32) :: size_text
        integer :: ierr, obj_size
        logical :: restored

        root = temp_root('smod-metadata')
        call cleanup_tree(root)
        call make_dir(root)
        obj_path = root//'/root.o'
        call write_file(obj_path, 'PARENT-OBJECT')
        call write_file(root//'/root.smod', 'STALE-LOCAL-INTERFACE')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'object-only', obj_path, root, '', out_id, ierr)
        call test_assert(suite, ierr == 0, 'existing object-only API still stores')
        call cache_restore_action(c, 'object-only', obj_path, root, restored, &
            required_smod_name='root')
        call test_assert(suite, .not. restored, &
            'stale local submodule cannot supply missing record metadata')
        call cache_file_content_key(obj_path, 'object', object_key, obj_size, ierr)
        write (size_text, '(i0)') obj_size
        record = 'schema 1'//achar(10)//'output '//out_id//achar(10)// &
            'object '//object_key//' '//trim(size_text)//achar(10)
        call cache_debug_write_action_record(c, 'legacy', record, ierr)
        call cache_restore_action(c, 'legacy', obj_path, root, restored)
        call test_assert(suite, .not. restored, &
            'legacy records with valid payloads and matching local files are invalidated')
        call cache_store_action(c, 'with-smod', obj_path, root, '', &
            out_id, ierr, 'root')
        record = 'schema 2'//achar(10)//'output '//out_id//achar(10)// &
            'object '//object_key//' '//trim(size_text)//achar(10)
        call cache_debug_write_action_record(c, 'stripped-smod', record, ierr)
        call cache_restore_action(c, 'stripped-smod', obj_path, root, restored)
        call test_assert(suite, .not. restored, &
            'removed submodule metadata cannot reuse the original output marker')
        call make_dir(root//'/input')
        call write_file(root//'/escaped.smod', 'ESCAPED-INTERFACE')
        call cache_store_action(c, 'escaped-smod', obj_path, root//'/input', '', &
            out_id, ierr, '../escaped')
        call test_assert(suite, ierr /= 0, &
            'submodule stem cannot escape its compiler output directory')
        call cache_store_action(c, 'missing-smod', obj_path, root, '', &
            out_id, ierr, 'missing')
        call test_assert(suite, ierr /= 0, 'missing required store artifact fails')
        call test_assert(suite, .not. cache_lookup(c, 'missing-smod'), &
            'failed submodule store never publishes an action record')
        call cleanup_tree(root)
    end subroutine test_smod_metadata_rejection

    subroutine test_binary_fingerprint(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, bin_path, restored_path
        integer :: ierr
        logical :: matches, restored

        root = temp_root('binary')
        call cleanup_tree(root)
        call make_dir(root)
        bin_path = root//'/prog'
        restored_path = root//'/restored-prog'
        call write_file(bin_path, 'BINARY-ONE')

        call cache_init(c, root//'/cache')
        call cache_store_binary(c, 'link-prog', bin_path, ierr)
        call test_assert(suite, ierr == 0, 'store binary fingerprint succeeds')

        call cache_restore_binary(c, 'link-prog', restored_path, restored)
        call test_assert(suite, restored, 'binary result restores its executable bytes')
        call test_assert(suite, files_equal(bin_path, restored_path), &
            'restored link output matches the independent producer bytes')

        call cache_binary_matches(c, 'link-prog', bin_path, matches)
        call test_assert(suite, matches, 'unchanged binary matches fingerprint')

        call write_file(bin_path, 'BINARY-TWO-LONGER-PAYLOAD')
        call cache_binary_matches(c, 'link-prog', bin_path, matches)
        call test_assert(suite, .not. matches, 'resized binary fails fingerprint')

        call cleanup_tree(root)
    end subroutine test_binary_fingerprint

    subroutine test_legacy_nopayload_rejection(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, binary, staged
        character(len=17) :: record
        character(len=1) :: old_record(17)
        integer :: i, ierr
        logical :: matches, restored

        root = temp_root('legacy-nopayload')
        call cleanup_tree(root)
        call make_dir(root)
        binary = root//'/program'
        staged = root//'/restored-program'
        call write_file(binary, 'OLD-LINK-OUTPUT')
        record = 'nopayload 1 10'
        do i = 1, size(old_record)
            old_record(i) = record(i:i)
        end do
        call cache_init(c, root//'/cache')
        call cache_store_bytes(c, 'legacy-link-l', old_record, &
            size(old_record), ierr)
        call test_assert(suite, ierr == 0, 'fixture stores the legacy nopayload record')
        call cache_binary_matches(c, 'legacy-link', binary, matches)
        call test_assert(suite, .not. matches, &
            'legacy nopayload record cannot produce a binary hit')
        call cache_restore_binary(c, 'legacy-link', staged, restored)
        call test_assert(suite, .not. restored, &
            'legacy nopayload record cannot restore absent executable bytes')
        call cleanup_tree(root)
    end subroutine test_legacy_nopayload_rejection

    subroutine test_validated_legacy_import(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, object_file
        character(len=HASH_LEN) :: result_id
        integer :: ierr

        root = temp_root('legacy-import')
        call cleanup_tree(root)
        call make_dir(root)
        object_file = root//'/legacy.o'
        call write_file(object_file, 'VALID LEGACY OBJECT')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'legacy-valid-action', object_file, root, '', &
            result_id, ierr)
        call test_assert(suite, ierr == 0, 'seed complete legacy compile record')
        call execute_command_line('rm -rf -- '//trim(c%root_dir)//'/store/v2')
        call test_assert(suite, cache_lookup(c, 'legacy-valid-action'), &
            'validated legacy payloads lazily import into v2 result store')

        call write_file(object_file, 'CORRUPT LEGACY OBJECT')
        call cache_store_action(c, 'legacy-corrupt-action', object_file, root, '', &
            result_id, ierr)
        call test_assert(suite, ierr == 0, 'seed second legacy compile record')
        call cache_debug_corrupt_object_payload(c, 'legacy-corrupt-action', ierr)
        call test_assert(suite, ierr == 0, 'corrupt only its old payload copy')
        call execute_command_line('rm -rf -- '//trim(c%root_dir)//'/store/v2')
        call test_assert(suite, .not. cache_lookup(c, 'legacy-corrupt-action'), &
            'invalid legacy payload is rejected during lazy import')
        call cleanup_tree(root)
    end subroutine test_validated_legacy_import

    function temp_root(tag) result(path)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: path
        integer, save :: counter = 0
        character(len=32) :: counter_text

        counter = counter + 1
        write (counter_text, '(I0)') counter
        path = '/var/tmp/fx-action-'//trim(tag)//'-'//trim(counter_text)
    end function temp_root

    subroutine make_dir(path)
        character(len=*), intent(in) :: path

        call execute_command_line('mkdir -p -- '//trim(path))
    end subroutine make_dir

    subroutine cleanup_tree(path)
        character(len=*), intent(in) :: path

        call execute_command_line('rm -rf -- '//trim(path))
    end subroutine cleanup_tree

    subroutine write_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u

        open (newunit=u, file=trim(path), status='replace', access='stream', &
            form='unformatted')
        write (u) content
        close (u)
    end subroutine write_file

    subroutine delete_file(path)
        character(len=*), intent(in) :: path
        integer :: u, ios

        open (newunit=u, file=trim(path), status='old', iostat=ios)
        if (ios == 0) close (u, status='delete')
    end subroutine delete_file

    logical function file_has_bytes(path, expected) result(equal)
        character(len=*), intent(in) :: path, expected
        character(len=:), allocatable :: actual
        integer :: u, ios, file_size
        logical :: exists

        equal = .false.
        inquire (file=trim(path), exist=exists)
        if (.not. exists) return
        inquire (file=trim(path), size=file_size)
        if (file_size /= len(expected)) return
        allocate (character(len=file_size) :: actual)
        open (newunit=u, file=trim(path), status='old', access='stream', &
            form='unformatted', iostat=ios)
        if (ios /= 0) return
        read (u, iostat=ios) actual
        close (u)
        if (ios /= 0) return
        equal = actual == expected
    end function file_has_bytes

    logical function files_equal(a, b) result(equal)
        character(len=*), intent(in) :: a, b
        integer :: sa, sb, ua, ub, ios
        character(len=1) :: ca, cb

        equal = .false.
        inquire (file=trim(a), size=sa)
        inquire (file=trim(b), size=sb)
        if (sa /= sb .or. sa <= 0) return
        open (newunit=ua, file=trim(a), access='stream', form='unformatted', &
            status='old')
        open (newunit=ub, file=trim(b), access='stream', form='unformatted', &
            status='old')
        equal = .true.
        do
            read (ua, iostat=ios) ca
            if (ios /= 0) exit
            read (ub, iostat=ios) cb
            if (ios /= 0 .or. ca /= cb) then
                equal = .false.
                exit
            end if
        end do
        close (ua)
        close (ub)
    end function files_equal

    subroutine test_file_hash_hook(suite)
        !! Installing a file-hash hook must route source keying through it, and
        !! clearing it must restore the default sha256 keying.
        type(test_suite_t), intent(inout) :: suite
        character(len=:), allocatable :: root, path
        character(len=HASH_LEN) :: h_default, h_hook, h_restored

        root = temp_root('hook')
        call make_dir(root)
        path = root//'/unit.f90'
        call write_file(path, 'program p; end program p')

        call cache_source_tree_hash(path, h_default)

        hook_calls = 0
        call cache_set_file_hash_hook(counting_hash)
        call cache_source_tree_hash(path, h_hook)
        call test_assert(suite, hook_calls > 0, 'installed hook is invoked')
        call test_assert(suite, h_hook /= h_default, &
            'hook changes the source-tree key')

        call cache_clear_file_hash_hook()
        call cache_source_tree_hash(path, h_restored)
        call test_assert(suite, h_restored == h_default, &
            'clearing the hook restores default keying')
    end subroutine test_file_hash_hook

    subroutine counting_hash(path, hex, ierr)
        character(len=*), intent(in) :: path
        character(len=64), intent(out) :: hex
        integer, intent(out) :: ierr

        hook_calls = hook_calls + 1
        hex = repeat('a', 64)
        if (len_trim(path) == 0) hex = repeat('b', 64)
        ierr = 0
    end subroutine counting_hash

end program test_action_cache
