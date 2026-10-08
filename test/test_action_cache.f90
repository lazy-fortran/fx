program test_action_cache
    use fx_action_cache, only: cache_t, HASH_LEN, action_cache_root, &
        cache_store_action, cache_restore_action, cache_lookup, &
        cache_action_mod_key, cache_store_binary, cache_binary_matches, &
        cache_restore_binary, &
        cache_source_tree_hash, cache_set_file_hash_hook, &
        cache_clear_file_hash_hook
    use fx_cache, only: cache_init
    use fx_cache_key, only: cache_file_content_key, cache_digest, cache_key_for
    use fx_proc, only: proc_exec, proc_result_t
        use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_preview, ACTION_RESULT_OK
    use fx_immutable_manifest, only: immutable_tree_entry_t
    use fx_immutable_store, only: immutable_store_blob_path
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, fx_test_chmod
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_assert_equal_str, test_suite_summary, &
        test_suite_exit
    implicit none

    type(test_suite_t) :: suite
    integer :: hook_calls = 0

    call test_suite_init(suite, 'fx_action_cache')
    call test_root_resolution(suite)
    call test_action_round_trip(suite)
    call test_smod_round_trip(suite)
    call test_action_result_warm_validation(suite)
    call test_smod_requirement(suite)
    call test_smod_metadata_rejection(suite)
    call test_binary_fingerprint(suite)
    call test_v2_only(suite)
    call test_file_hash_hook(suite)
    call test_include_search_dirs(suite)
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
        character(len=HASH_LEN) :: object_key, expected_id
        character(len=512) :: identity_parts(6)
        integer :: ierr, content_size
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
        call cache_file_content_key(obj_path, 'object', object_key, &
            content_size, ierr)
        call cache_file_content_key(mod_path, 'mod', mkey, content_size, ierr)
        identity_parts = ''
        identity_parts(1) = 'fx-output-schema-2'
        identity_parts(2) = object_key
        identity_parts(3) = mkey
        identity_parts(4) = 'widget'
        expected_id = cache_digest(identity_parts, 6)
        call test_assert_equal_str(suite, trim(expected_id), trim(out_id), &
            'stored output id keeps the schema-2 dependency identity')

        call cache_init(restore_c, root//'/cache')
        hit = cache_lookup(restore_c, 'act-widget')
        call test_assert(suite, hit, 'fresh cache reports action hit')

        call cache_action_mod_key(restore_c, 'act-widget', mkey, found)
        call test_assert(suite, found .and. len_trim(mkey) == HASH_LEN, &
            'mod key recovered from v2 result')
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
        character(len=HASH_LEN) :: first_id, second_id, restored_id
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
            restored_id, required_smod_name=stem)
        call test_assert(suite, restored, 'restore all deleted compiler outputs')
        call test_assert_equal_str(suite, trim(first_id), trim(restored_id), &
            'submodule output identity survives v2 restoration')
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

    subroutine test_action_result_warm_validation(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        type(action_result_store_t) :: result_store
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=:), allocatable :: root, object_file, blob_path
        character(len=HASH_LEN) :: result_id
        integer :: ierr, cleanup, epoch_before, epoch_after
        logical :: restored

        root = temp_root('action-result-warm')
        call cleanup_tree(root)
        call make_dir(root)
        object_file = root//'/object.o'
        call write_file(object_file, 'WARM OBJECT BYTES')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'warm-result', object_file, root, '', &
            result_id, ierr)
        call test_assert(suite, ierr == 0, 'publish the warm action result')
        call action_result_store_init(result_store, &
            root//'/cache/store/v2', ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'warm action result store opens')
        call action_result_preview(result_store, 'warm-result', entries, &
            result_id, ierr)
        call test_assert_equal_int(suite, ACTION_RESULT_OK, ierr, &
            'warm result manifest validates')
        if (.not. allocated(entries)) then
            call test_assert(suite, .false., &
                'warm result manifest contains its output entry')
            call cleanup_tree(root)
            return
        end if
        if (size(entries) == 0) then
            call test_assert(suite, .false., &
                'warm result manifest contains its output entry')
            call cleanup_tree(root)
            return
        end if
        blob_path = immutable_store_blob_path(result_store%objects, &
            entries(1)%object_id)

        epoch_before = read_lease_epoch(root//'/cache/store/v2')
        call test_assert(suite, epoch_before >= 0, &
            'warm fixture has readable lease metadata')
        call cache_restore_action(c, 'warm-result', object_file, root, restored)
        call test_assert(suite, restored, &
            'matching local object validates against the action result')
        call test_assert(suite, file_has_bytes(object_file, 'WARM OBJECT BYTES'), &
            'warm validation preserves the actual local object')
        epoch_after = read_lease_epoch(root//'/cache/store/v2')
        call test_assert(suite, epoch_after == epoch_before, &
            'warm local validation does not mutate graph lease metadata')

        cleanup = fx_test_chmod(blob_path, 420)
        call test_assert_equal_int(suite, 0, cleanup, &
            'test makes the immutable payload writable for corruption')
        call write_file(blob_path, 'CORRUPTED RESULT OBJECT')
        cleanup = fx_test_chmod(blob_path, 292)
        call test_assert_equal_int(suite, 0, cleanup, &
            'test restores immutable payload permissions')
        call cache_restore_action(c, 'warm-result', object_file, root, restored)
        call test_assert(suite, .not. restored, &
            'corrupt immutable object cannot produce a warm result hit')
        call test_assert(suite, file_has_bytes(object_file, 'WARM OBJECT BYTES'), &
            'corrupt immutable object does not replace the local output')

        cleanup = fx_test_remove_tree(blob_path)
        call test_assert(suite, cleanup == 0, 'test removes the immutable object')
        call cache_restore_action(c, 'warm-result', object_file, root, restored)
        call test_assert(suite, .not. restored, &
            'missing immutable object falls through without a warm hit')
        call test_assert(suite, file_has_bytes(object_file, 'WARM OBJECT BYTES'), &
            'missing immutable object leaves the local output intact')
        call cleanup_tree(root)
    end subroutine test_action_result_warm_validation

    subroutine test_smod_requirement(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, obj_path, smod_path
        character(len=HASH_LEN) :: out_id
        integer :: ierr
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
        call test_assert(suite, .not. restored, &
            'required submodule must match the published result')
        call cleanup_tree(root)
    end subroutine test_smod_requirement

    subroutine test_smod_metadata_rejection(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, obj_path
        character(len=HASH_LEN) :: out_id
        integer :: ierr
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
            'failed submodule store never publishes an action result')
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

    subroutine test_v2_only(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, object_file, destination
        character(len=HASH_LEN) :: output_id, restored_id
        integer :: ierr
        logical :: restored, v1_exists

        root = temp_root('v2-only')
        call cleanup_tree(root)
        call make_dir(root)
        object_file = root//'/source.o'
        destination = root//'/restored.o'
        call write_file(object_file, 'PERSISTENT V2 OBJECT')
        call cache_init(c, root//'/cache')
        call cache_store_action(c, 'v2-action', object_file, root, '', &
            output_id, ierr)
        call test_assert(suite, ierr == 0, 'v2 action publication succeeds')
        inquire (file=root//'/cache/store/v1', exist=v1_exists)
        call test_assert(suite, .not. v1_exists, &
            'compile publication creates no v1 store')
        call cache_init(c, root//'/cache')
        call test_assert(suite, cache_lookup(c, 'v2-action'), &
            'reopened v2 action remains available without v1')
        call cache_restore_action(c, 'v2-action', destination, root, restored, &
            restored_id)
        call test_assert(suite, restored, 'v2 object restores')
        call test_assert_equal_str(suite, trim(output_id), trim(restored_id), &
            'v2 manifest preserves schema-2 output identity')
        call test_assert(suite, file_has_bytes(destination, 'PERSISTENT V2 OBJECT'), &
            'v2 restore yields original object bytes')
        call cleanup_tree(root)
    end subroutine test_v2_only

    function temp_root(tag) result(path)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: path
        integer, save :: counter = 0
        character(len=32) :: counter_text
        character(len=512) :: tmpdir
        integer :: status

        counter = counter + 1
        write (counter_text, '(I0)') counter
        call get_environment_variable('TMPDIR', tmpdir, status=status)
        if (status /= 0 .or. len_trim(tmpdir) == 0) tmpdir = '/var/tmp'
        path = trim(tmpdir)//'/fx action;$(fixture)-'//trim(tag)//'-'// &
            trim(counter_text)
    end function temp_root

    subroutine make_dir(path)
        character(len=*), intent(in) :: path
        integer :: ierr

        ierr = fx_test_mkdir_p(path)
        if (ierr /= 0) error stop 'fixture directory creation failed'
    end subroutine make_dir

    subroutine cleanup_tree(path)
        character(len=*), intent(in) :: path

        call remove_tree(path)
    end subroutine cleanup_tree

    subroutine remove_tree(path)
        character(len=*), intent(in) :: path
        integer :: ierr

        ierr = fx_test_remove_tree(path)
        if (ierr /= 0) error stop 'fixture tree removal failed'
    end subroutine remove_tree

    subroutine write_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: u

        open (newunit=u, file=trim(path), status='replace', access='stream', &
            form='unformatted')
        write (u) content
        close (u)
    end subroutine write_file

    integer function read_lease_epoch(store_root) result(epoch)
        character(len=*), intent(in) :: store_root
        character(len=128) :: header
        integer :: unit, ios, separator

        epoch = -1
        open(newunit=unit, file=trim(store_root)//'/.fx-metadata/leases', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, '(A)', iostat=ios) header
        close(unit)
        if (ios /= 0) return
        separator = index(header, '|')
        if (separator <= 0) return
        read(header(separator + 1:), *, iostat=ios) epoch
        if (ios /= 0) epoch = -1
    end function read_lease_epoch

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

    subroutine test_include_search_dirs(suite)
        !! The compiler independently selects headers; cache receipts must follow it.
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: cache
        character(:), allocatable :: root, source, executable, restored
        character(len=512) :: directories(2), swapped(2)
        character(len=HASH_LEN) :: initial, warm, changed, cpp_key, dep_keys(0)
        integer :: ierr
        logical :: hit
        character(len=1), parameter :: nl = new_line('a')

        root = temp_root('include-search')
        call cleanup_tree(root)
        call make_dir(root//'/src')
        call make_dir(root//'/first')
        call make_dir(root//'/second')
        directories = [character(len=512) :: root//'/first', root//'/second']
        source = root//'/src/main.f90'
        executable = root//'/reference'
        restored = root//'/restored'
        call write_file(source, "program p"//nl//"include 'value.inc'"//nl// &
            'end program p'//nl)
        call write_file(root//'/first/value.inc', "include 'nested.inc'"//nl)
        call write_file(root//'/first/nested.inc', "print '(i0)', 11"//nl)
        call write_file(root//'/second/value.inc', "print '(i0)', 99"//nl)
        call reference_include_program(suite, source, directories, executable, '11')
        call cache_init(cache, root//'/cache')
        initial = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=directories)
        call cache_store_binary(cache, initial, executable, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'stores compiled include behavior')

        call write_file(root//'/second/value.inc', "print '(i0)', 77"//nl)
        warm = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=directories)
        call cache_restore_binary(cache, warm, restored, hit)
        call test_assert(suite, hit, 'editing unselected include retains warm receipt')
        call assert_program_output(suite, restored, '11')

        call write_file(root//'/first/nested.inc', "print '(i0)', 22"//nl)
        changed = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=directories)
        call cache_restore_binary(cache, changed, restored, hit)
        call test_assert(suite, .not. hit, &
            'recursive selected include invalidates receipt')
        call reference_include_program(suite, source, directories, executable, '22')
        call cache_store_binary(cache, changed, executable, ierr)
        call test_assert_equal_int(suite, 0, ierr, 'stores updated include behavior')
        call cache_restore_binary(cache, changed, restored, hit)
        call test_assert(suite, hit, 'updated include result restores')
        call assert_program_output(suite, restored, '22')

        swapped = directories(2:1:-1)
        changed = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=swapped)
        call cache_restore_binary(cache, changed, restored, hit)
        call test_assert(suite, .not. hit, 'include search order changes receipt')
        call reference_include_program(suite, source, swapped, executable, '77')
        call write_file(source, 'program p'//nl//'#include "value.inc"'//nl// &
            'end program p'//nl)
        cpp_key = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=directories)
        call reference_include_program(suite, source, directories, executable, '22')
        call cache_store_binary(cache, cpp_key, executable, ierr)
        call write_file(root//'/first/nested.inc', "print '(i0)', 33"//nl)
        changed = cache_key_for(source, 'reference-compiler', '-cpp', dep_keys, 0, &
            include_dirs=directories)
        call cache_restore_binary(cache, changed, restored, hit)
        call test_assert(suite, .not. hit, 'quoted CPP include invalidates receipt')
        call reference_include_program(suite, source, directories, executable, '33')
        call cleanup_tree(root)
    end subroutine test_include_search_dirs

    subroutine reference_include_program(suite, source, directories, executable, wanted)
        type(test_suite_t), intent(inout) :: suite
        character(len=*), intent(in) :: source, directories(:), executable, wanted
        character(len=512) :: arguments(7)
        type(proc_result_t) :: child

        arguments = [character(len=512) :: 'gfortran', '-cpp', &
            '-I'//trim(directories(1)), '-I'//trim(directories(2)), &
            source, '-o', executable]
        call proc_exec(arguments, size(arguments), child)
        call test_assert_equal_int(suite, 0, child%exit_code, &
            'independent compiler builds include fixture')
        if (child%exit_code /= 0) then
            write (*, '(a)') child%stderr_text
            return
        end if
        call assert_program_output(suite, executable, wanted)
    end subroutine reference_include_program

    subroutine assert_program_output(suite, executable, wanted)
        type(test_suite_t), intent(inout) :: suite
        character(len=*), intent(in) :: executable, wanted
        type(proc_result_t) :: child

        call proc_exec([executable], 1, child)
        call test_assert_equal_int(suite, 0, child%exit_code, 'include program runs')
        call test_assert_equal_str(suite, wanted//new_line('a'), child%stdout_text, &
            'include-selected observable executable output')
    end subroutine assert_program_output

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
