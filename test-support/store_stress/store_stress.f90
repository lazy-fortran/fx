program store_stress
    !! Concurrency stress for the shared action store (not an fpm test).
    !!
    !!   store_stress worker ROOT SEED SECONDS NKEYS GRACE CAP EVICT_EVERY
    !!   store_stress verify ROOT NKEYS
    !!   store_stress evict ROOT GRACE CAP
    !!
    !! Workers look actions up, restore and compare every output byte with
    !! the content defined by the key, publish on a miss and run capped
    !! eviction every EVICT_EVERY operations. Any restored byte that differs
    !! is a wrong hit and is reported as WRONG. verify checks every key once.
    use, intrinsic :: iso_c_binding, only: c_long_long
    use, intrinsic :: iso_fortran_env, only: int64, output_unit
    use fx_proc, only: proc_pid
    use fx_immutable_manifest, only: immutable_tree_entry_t
    use fx_action_result_store, only: action_result_store_t, &
        action_result_store_init, action_result_lookup, &
        action_result_publish_files, action_result_materialize_blob, &
        action_result_evict, action_result_evict_stats_t, &
        action_result_store_policy, ACTION_RESULT_OK
    implicit none

    interface
        integer(c_long_long) function unix_time() bind(C, name='fx_c_unix_time')
            import c_long_long
        end function unix_time
    end interface

    character(len=4096) :: mode, root, arg
    type(action_result_store_t) :: store
    integer :: ierr

    call get_command_argument(1, mode)
    call get_command_argument(2, root)
    call action_result_store_init(store, trim(root), ierr)
    if (ierr /= ACTION_RESULT_OK) then
        write (output_unit, '(a,i0)') 'INIT_FAILED ', ierr
        stop 2
    end if
    select case (trim(mode))
    case ('worker')
        call worker()
    case ('verify')
        call verify()
    case ('evict')
        call evict_once()
    case default
        stop 2
    end select

contains

    integer(int64) function iarg(i)
        integer, intent(in) :: i
        call get_command_argument(i, arg)
        read (arg, *) iarg
    end function iarg

    subroutine worker()
        integer(int64) :: seed, seconds, nkeys, grace, cap, every, state, ops
        integer(int64) :: hits, misses, wrong, pub_ok, pub_err, evicts, retired
        integer(int64) :: deleted, restore_miss, k, t_end
        type(action_result_evict_stats_t) :: stats
        character(len=:), allocatable :: scratch

        seed = iarg(3)
        seconds = iarg(4)
        nkeys = iarg(5)
        grace = iarg(6)
        cap = iarg(7)
        every = iarg(8)
        call action_result_store_policy(max_bytes=cap, grace_seconds=grace, &
            temp_age_seconds=grace)
        write (arg, '(a,i0)') '/stress-', proc_pid()
        call get_environment_variable('STRESS_SCRATCH', root)
        scratch = trim(root)//trim(arg)
        call execute_command_line('mkdir -p '//scratch)
        state = seed * 2654435761_int64 + 1_int64
        hits = 0; misses = 0; wrong = 0; pub_ok = 0; pub_err = 0
        evicts = 0; retired = 0; deleted = 0; restore_miss = 0; ops = 0
        t_end = unix_time() + seconds
        do while (unix_time() < t_end)
            state = next(state)
            if (modulo(state, 2_int64) == 0) then
                k = 1 + modulo(ishft(state, -8), max(1_int64, nkeys / 10))
            else
                k = 1 + modulo(ishft(state, -8), nkeys)
            end if
            call one_key(k, scratch, hits, misses, wrong, pub_ok, pub_err, &
                restore_miss)
            ops = ops + 1
            if (every > 0 .and. modulo(ops, every) == 0) then
                call action_result_evict(store, stats, ierr)
                evicts = evicts + 1
                retired = retired + stats%actions_retired
                deleted = deleted + stats%objects_deleted
            end if
        end do
        call execute_command_line('rm -rf '//scratch)
        write (output_unit, '(a,10(1x,a,"=",i0))') 'WORKER', 'ops', ops, &
            'hits', hits, 'misses', misses, 'restore_miss', restore_miss, &
            'wrong', wrong, 'pub_ok', pub_ok, 'pub_err', pub_err, &
            'evicts', evicts, 'retired', retired, 'deleted', deleted
        if (wrong > 0) stop 3
    end subroutine worker

    subroutine one_key(k, scratch, hits, misses, wrong, pub_ok, pub_err, &
            restore_miss)
        integer(int64), intent(in) :: k
        character(len=*), intent(in) :: scratch
        integer(int64), intent(inout) :: hits, misses, wrong, pub_ok, pub_err, &
            restore_miss
        type(immutable_tree_entry_t), allocatable :: entries(:)
        type(immutable_tree_entry_t) :: fresh(2)
        character(len=64) :: result_id
        character(len=512) :: sources(2)
        character(len=:), allocatable :: id, out
        integer :: i, status
        logical :: ok

        id = key_id(k)
        call action_result_lookup(store, id, entries, result_id, status)
        if (status == ACTION_RESULT_OK) then
            ok = size(entries) == 2
            do i = 1, size(entries)
                out = scratch//'/out'
                call action_result_materialize_blob(store, entries(i)%object_id, &
                    out, 420, status)
                if (status /= ACTION_RESULT_OK) then
                    ok = .false.
                    restore_miss = restore_miss + 1
                    exit
                end if
                if (.not. matches(out, k, trim(entries(i)%role))) then
                    wrong = wrong + 1
                    write (output_unit, '(a,i0,1x,a)') 'WRONG key=', k, &
                        trim(entries(i)%role)
                    ok = .false.
                end if
            end do
            if (ok) then
                hits = hits + 1
                return
            end if
        end if
        misses = misses + 1
        do i = 1, 2
            sources(i) = scratch//'/src-'//role_name(i)
            call write_content(trim(sources(i)), k, role_name(i))
            fresh(i)%path = role_name(i)
            fresh(i)%role = role_name(i)
            fresh(i)%mode = 420
        end do
        call action_result_publish_files(store, id, sources, fresh, result_id, &
            status)
        if (status == ACTION_RESULT_OK) then
            pub_ok = pub_ok + 1
        else
            pub_err = pub_err + 1
        end if
    end subroutine one_key

    subroutine verify()
        integer(int64) :: nkeys, k, bound, wrong, broken
        type(immutable_tree_entry_t), allocatable :: entries(:)
        character(len=64) :: result_id
        character(len=:), allocatable :: out
        integer :: i, status

        nkeys = iarg(3)
        bound = 0; wrong = 0; broken = 0
        call get_environment_variable('STRESS_SCRATCH', arg)
        out = trim(arg)//'/verify-out'
        do k = 1, nkeys
            call action_result_lookup(store, key_id(k), entries, result_id, status)
            if (status /= ACTION_RESULT_OK) cycle
            bound = bound + 1
            do i = 1, size(entries)
                call action_result_materialize_blob(store, entries(i)%object_id, &
                    out, 420, status)
                if (status /= ACTION_RESULT_OK) then
                    broken = broken + 1
                else if (.not. matches(out, k, trim(entries(i)%role))) then
                    wrong = wrong + 1
                    write (output_unit, '(a,i0)') 'WRONG key=', k
                end if
            end do
        end do
        write (output_unit, '(a,3(1x,a,"=",i0))') 'VERIFY', 'bound', bound, &
            'wrong', wrong, 'dangling_outputs', broken
        if (wrong > 0) stop 3
    end subroutine verify

    subroutine evict_once()
        type(action_result_evict_stats_t) :: stats
        integer(int64) :: grace, cap

        grace = iarg(3)
        cap = iarg(4)
        call action_result_store_policy(max_bytes=cap, grace_seconds=grace, &
            temp_age_seconds=grace)
        call action_result_evict(store, stats, ierr)
        write (output_unit, '(a,8(1x,a,"=",i0))') 'EVICT', 'status', ierr, &
            'before', stats%bytes_before, 'after', stats%bytes_after, &
            'actions', stats%actions, 'retired', stats%actions_retired, &
            'deleted', stats%objects_deleted, 'temps', stats%temps_removed, &
            'over', merge(1, 0, stats%over_cap)
    end subroutine evict_once

    function key_id(k) result(id)
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: id
        character(len=32) :: text
        write (text, '(a,i0)') 'stress-key-', k
        id = trim(text)
    end function key_id

    function role_name(i) result(name)
        integer, intent(in) :: i
        character(len=:), allocatable :: name
        name = merge('object', 'module', i == 1)
    end function role_name

    integer(int64) function next(s)
        integer(int64), intent(in) :: s
        next = ieor(s, ishft(s, 13))
        next = ieor(next, ishft(next, -7))
        next = ieor(next, ishft(next, 17))
    end function next

    subroutine content(k, role, bytes)
        !! Key- and role-specific bytes: a text header, then xorshift noise.
        integer(int64), intent(in) :: k
        character(len=*), intent(in) :: role
        character(len=:), allocatable, intent(out) :: bytes
        integer(int64) :: n, s, j
        character(len=64) :: header

        n = 2000_int64 + modulo(k * 7919_int64, 120000_int64)
        if (role == 'module') n = 300_int64 + modulo(k * 31_int64, 5000_int64)
        allocate (character(len=n) :: bytes)
        s = k * 1000003_int64 + len(role)
        do j = 1, n
            s = next(s)
            bytes(j:j) = achar(int(modulo(ishft(s, -24), 256_int64)))
        end do
        write (header, '(a,i0,1x,a,a)') 'key=', k, role, achar(10)
        bytes(1:len_trim(header) + 1) = header(1:len_trim(header) + 1)
    end subroutine content

    subroutine write_content(path, k, role)
        character(len=*), intent(in) :: path, role
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: bytes
        integer :: unit

        call content(k, role, bytes)
        open (newunit=unit, file=path, status='replace', access='stream', &
            form='unformatted')
        write (unit) bytes
        close (unit)
    end subroutine write_content

    logical function matches(path, k, role)
        character(len=*), intent(in) :: path, role
        integer(int64), intent(in) :: k
        character(len=:), allocatable :: bytes, actual
        integer :: unit, size_bytes, status

        matches = .false.
        call content(k, role, bytes)
        inquire (file=path, size=size_bytes)
        if (size_bytes /= len(bytes)) return
        allocate (character(len=size_bytes) :: actual)
        open (newunit=unit, file=path, status='old', access='stream', &
            form='unformatted', iostat=status)
        if (status /= 0) return
        read (unit, iostat=status) actual
        close (unit)
        matches = status == 0 .and. actual == bytes
    end function matches
end program store_stress
