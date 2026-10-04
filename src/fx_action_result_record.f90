module fx_action_result_record
    use, intrinsic :: iso_c_binding, only: c_char
    use fx_immutable_manifest, only: immutable_id_valid
    implicit none
    private

    integer, parameter, public :: ACTION_RECORD_BOUND = 1
    integer, parameter, public :: ACTION_RECORD_CONFLICT = 2
    integer, parameter, public :: ACTION_RECORD_INVALID = 3
    integer, parameter :: HASH_LEN = 64

    public :: action_result_bound_record, action_result_conflict_record
    public :: action_result_record_parse

contains

    function action_result_bound_record(action_id, result_id) result(record)
        character(len=*), intent(in) :: action_id, result_id
        character(len=:), allocatable :: record

        record = 'FXACTION2'//achar(10)//trim(action_id)//achar(10)// &
            'BOUND'//achar(10)//trim(result_id)//achar(10)
    end function action_result_bound_record

    function action_result_conflict_record(action_id, ids) result(record)
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(in) :: ids(2)
        character(len=:), allocatable :: record

        record = 'FXACTION2'//achar(10)//trim(action_id)//achar(10)// &
            'NONDETERMINISTIC_ACTION incomplete-key'//achar(10)// &
            trim(ids(1))//achar(10)//trim(ids(2))//achar(10)
    end function action_result_conflict_record

    subroutine action_result_record_parse(bytes, count, action_id, result_id, &
            ids, status)
        character(kind=c_char), intent(in) :: bytes(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: action_id
        character(len=HASH_LEN), intent(out) :: result_id, ids(2)
        integer, intent(out) :: status
        character(len=:), allocatable :: text, prefix
        integer :: i, offset, expected

        result_id = ''
        ids = ''
        status = ACTION_RECORD_INVALID
        if (count < 1 .or. count > size(bytes)) return
        allocate(character(len=count) :: text)
        do i = 1, count
            text(i:i) = bytes(i)
        end do
        prefix = 'FXACTION2'//achar(10)//trim(action_id)//achar(10)
        if (len(text) <= len(prefix)) return
        if (text(1:len(prefix)) /= prefix) return
        offset = len(prefix) + 1
        if (index(text(offset:), 'BOUND'//achar(10)) == 1) then
            offset = offset + len('BOUND'//achar(10))
            expected = offset + HASH_LEN
            if (len(text) /= expected) return
            result_id = text(offset:offset + HASH_LEN - 1)
            if (text(len(text):len(text)) /= achar(10)) return
            if (.not. immutable_id_valid(result_id)) return
            status = ACTION_RECORD_BOUND
            return
        end if
        if (index(text(offset:), &
            'NONDETERMINISTIC_ACTION incomplete-key'//achar(10)) /= 1) return
        offset = offset + len('NONDETERMINISTIC_ACTION incomplete-key'//achar(10))
        expected = offset + 2 * HASH_LEN + 1
        if (len(text) /= expected) return
        ids(1) = text(offset:offset + HASH_LEN - 1)
        offset = offset + HASH_LEN + 1
        ids(2) = text(offset:offset + HASH_LEN - 1)
        if (text(len(text):len(text)) /= achar(10)) return
        if (.not. immutable_id_valid(ids(1)) .or. &
            .not. immutable_id_valid(ids(2))) return
        if (ids(1) >= ids(2)) return
        status = ACTION_RECORD_CONFLICT
    end subroutine action_result_record_parse

end module fx_action_result_record
