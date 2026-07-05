program test_action_cache
    use fx_action_cache, only: cache_t, HASH_LEN, action_cache_root, &
        cache_store_action, cache_restore_action, cache_lookup, &
        cache_action_mod_key, cache_store_binary, cache_binary_matches
    use fx_cache, only: cache_init
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_str, test_suite_summary, test_suite_exit
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_action_cache')
    call test_root_resolution(suite)
    call test_action_round_trip(suite)
    call test_binary_fingerprint(suite)
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
        character(len=HASH_LEN) :: out_id, out_id2, mkey
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

    subroutine test_binary_fingerprint(suite)
        type(test_suite_t), intent(inout) :: suite
        type(cache_t) :: c
        character(len=:), allocatable :: root, bin_path
        integer :: ierr
        logical :: matches

        root = temp_root('binary')
        call cleanup_tree(root)
        call make_dir(root)
        bin_path = root//'/prog'
        call write_file(bin_path, 'BINARY-ONE')

        call cache_init(c, root//'/cache')
        call cache_store_binary(c, 'link-prog', bin_path, ierr)
        call test_assert(suite, ierr == 0, 'store binary fingerprint succeeds')

        call cache_binary_matches(c, 'link-prog', bin_path, matches)
        call test_assert(suite, matches, 'unchanged binary matches fingerprint')

        call write_file(bin_path, 'BINARY-TWO-LONGER-PAYLOAD')
        call cache_binary_matches(c, 'link-prog', bin_path, matches)
        call test_assert(suite, .not. matches, 'resized binary fails fingerprint')

        call cleanup_tree(root)
    end subroutine test_binary_fingerprint

    function temp_root(tag) result(path)
        character(len=*), intent(in) :: tag
        character(len=:), allocatable :: path
        integer, save :: counter = 0
        character(len=32) :: counter_text

        counter = counter + 1
        write (counter_text, '(I0)') counter
        path = '/tmp/fx-action-'//trim(tag)//'-'//trim(counter_text)
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

end program test_action_cache
