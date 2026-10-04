module action_publication_oracle
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use fx_proc, only: proc_pid
    use fx_action_result_store, only: action_result_action_key
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_lock, fx_test_unlock
    use fx_test_process, only: test_process_identity, test_process_sleep_ms
    implicit none
    private
    public :: publication_probe_t, publication_probe_lock, publication_probe_observe
    public :: publication_probe_resume, publication_probe_unlock
    public :: publication_probe_conflict

    type :: publication_probe_t
        character(len=512) :: root = ''
        character(len=64) :: key = ''
        integer :: action_lock = -1
        integer :: metadata_lock = -1
        integer :: pid = -1
        integer(c_int64_t) :: start = -1
    end type publication_probe_t

contains

    subroutine publication_probe_lock(root, action, probe, ierr)
        character(len=*), intent(in) :: root, action
        type(publication_probe_t), intent(out) :: probe
        integer, intent(out) :: ierr
        character(len=:), allocatable :: directory

        probe%root = root
        probe%key = action_result_action_key(action)
        directory = trim(root)//'/actions/sha256/'//probe%key(1:2)
        ierr = fx_test_mkdir_p(directory)
        if (ierr /= 0) return
        probe%action_lock = fx_test_lock(directory//'/'//probe%key//'.lock')
        ierr = 0
        if (probe%action_lock < 0) ierr = -1
    end subroutine publication_probe_lock

    subroutine publication_probe_observe(probe, pid, pending_id, ierr)
        type(publication_probe_t), intent(inout) :: probe
        integer, intent(in) :: pid
        character(len=*), intent(out) :: pending_id
        integer, intent(out) :: ierr
        integer :: attempt

        probe%pid = pid
        pending_id = ''
        ierr = -1
        do attempt = 1, 1500
            call read_pending_id(probe, pending_id)
            if (len_trim(pending_id) == 64) exit
            call test_process_sleep_ms(10)
        end do
        if (len_trim(pending_id) /= 64) return
        call verify_identity(probe, ierr)
    end subroutine publication_probe_observe

    subroutine verify_identity(probe, ierr)
        type(publication_probe_t), intent(inout) :: probe
        integer, intent(out) :: ierr
        integer(c_int64_t) :: actual_start
        integer :: parent
        character(len=1024) :: executable

        call test_process_identity(probe%pid, actual_start, parent, executable, ierr)
        if (ierr /= 0) return
        if (parent /= proc_pid()) then
            ierr = -1
            return
        end if
        if (probe%start < 0) probe%start = actual_start
        if (actual_start /= probe%start) ierr = -1
    end subroutine verify_identity

    subroutine publication_probe_resume(probe, ierr)
        type(publication_probe_t), intent(inout) :: probe
        integer, intent(out) :: ierr
        integer :: unlock_error

        call verify_identity(probe, ierr)
        if (ierr /= 0) return
        if (probe%action_lock >= 0) then
            unlock_error = fx_test_unlock(probe%action_lock)
            if (unlock_error /= 0) then
                ierr = unlock_error
                return
            end if
        end if
    end subroutine publication_probe_resume

    subroutine publication_probe_unlock(probe)
        type(publication_probe_t), intent(inout) :: probe
        integer :: ignored

        if (probe%action_lock >= 0) ignored = fx_test_unlock(probe%action_lock)
        if (probe%metadata_lock >= 0) ignored = fx_test_unlock(probe%metadata_lock)
    end subroutine publication_probe_unlock

    subroutine publication_probe_conflict(probe, ierr)
        type(publication_probe_t), intent(inout) :: probe
        integer, intent(out) :: ierr
        integer :: attempt
        logical :: conflicted

        probe%metadata_lock = fx_test_lock(trim(probe%root)//'/.fx-metadata/lock')
        ierr = -1
        if (probe%metadata_lock < 0) return
        call publication_probe_resume(probe, ierr)
        if (ierr /= 0) return
        do attempt = 1, 1500
            conflicted = binding_is_conflict(probe)
            if (conflicted) exit
            call test_process_sleep_ms(10)
        end do
        if (.not. conflicted) then
            ierr = -1
            return
        end if
        call verify_identity(probe, ierr)
    end subroutine publication_probe_conflict

    subroutine read_pending_id(probe, id)
        type(publication_probe_t), intent(in) :: probe
        character(len=*), intent(out) :: id
        character(len=2048) :: line
        integer :: unit, ierr, delimiter

        id = ''
        open(newunit=unit, file=trim(probe%root)//'/.fx-metadata/leases', &
            status='old', action='read', iostat=ierr)
        if (ierr /= 0) return
        do
            read(unit, '(A)', iostat=ierr) line
            if (ierr /= 0) exit
            if (line(1:2) /= 'P|') cycle
            if (index(line, '|'//probe%key// &
                '|fx-action-v1|publication|tree|') == 0) cycle
            delimiter = index(trim(line), '|', back=.true.)
            if (delimiter > 0) id = trim(line(delimiter + 1:))
        end do
        close(unit)
    end subroutine read_pending_id

    logical function binding_is_conflict(probe) result(found)
        type(publication_probe_t), intent(in) :: probe
        character(len=128) :: line
        character(len=:), allocatable :: path
        integer :: unit, ierr, i

        found = .false.
        path = trim(probe%root)//'/actions/sha256/'//probe%key(1:2)//'/'//probe%key
        open(newunit=unit, file=path, status='old', action='read', iostat=ierr)
        if (ierr /= 0) return
        do i = 1, 3
            read(unit, '(A)', iostat=ierr) line
            if (ierr /= 0) exit
        end do
        close(unit)
        if (ierr /= 0) return
        found = trim(line) == 'NONDETERMINISTIC_ACTION incomplete-key'
    end function binding_is_conflict

end module action_publication_oracle
