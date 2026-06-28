module fx_diag
    use fx_json_build, only: json_builder_t, json_object_start, json_object_end, &
        json_array_start, json_array_end, &
        json_key_string, json_key_int
    implicit none
    private

    integer, parameter, public :: DIAG_ERROR = 0
    integer, parameter, public :: DIAG_WARNING = 1
    integer, parameter, public :: DIAG_INFO = 2
    integer, parameter, public :: DIAG_HINT = 3

    integer, parameter, public :: MAX_DIAGS = 512

    type, public :: diag_t
        character(len=512) :: file = ' '
        integer :: line = 0
        integer :: col = 0
        integer :: severity = DIAG_ERROR
        character(len=512) :: message = ' '
        character(len=256) :: hint = ' '
        character(len=256) :: source_line = ' '
    end type diag_t

    public :: diag_new, diag_to_string, diag_to_json
    public :: diags_to_json, diag_strip_prefix

contains

    function diag_new(file, line, col, severity, message) result(d)
        character(len=*), intent(in) :: file
        integer, intent(in) :: line
        integer, intent(in) :: col
        integer, intent(in) :: severity
        character(len=*), intent(in) :: message
        type(diag_t) :: d

        d%file = file
        d%line = line
        d%col = col
        d%severity = severity
        d%message = message
        d%hint = ''
        d%source_line = ''
    end function diag_new

    function diag_to_string(d) result(res)
        type(diag_t), intent(in) :: d
        character(len=:), allocatable :: res
        character(len=32) :: lbuf, cbuf
        character(len=8) :: sev_str

        write(lbuf, '(I0)') d%line
        write(cbuf, '(I0)') d%col

        select case (d%severity)
        case (DIAG_ERROR)
            sev_str = 'error'
        case (DIAG_WARNING)
            sev_str = 'warning'
        case (DIAG_INFO)
            sev_str = 'info'
        case (DIAG_HINT)
            sev_str = 'hint'
        case default
            sev_str = 'error'
        end select

        res = trim(d%file) // ':' // trim(lbuf) // ':' // trim(cbuf) // &
            ': ' // trim(sev_str) // ': ' // trim(d%message)
    end function diag_to_string

    subroutine diag_to_json(d, jb)
        type(diag_t), intent(in) :: d
        type(json_builder_t), intent(inout) :: jb

        call json_object_start(jb)
        call json_key_string(jb, 'file', trim(d%file))
        call json_key_int(jb, 'line', d%line)
        call json_key_int(jb, 'col', d%col)
        call json_key_int(jb, 'severity', d%severity)
        call json_key_string(jb, 'message', trim(d%message))
        if (len_trim(d%hint) > 0) &
            call json_key_string(jb, 'hint', trim(d%hint))
        if (len_trim(d%source_line) > 0) &
            call json_key_string(jb, 'source_line', trim(d%source_line))
        call json_object_end(jb)
    end subroutine diag_to_json

    subroutine diags_to_json(diags, n, jb)
        integer, intent(in) :: n
        type(diag_t), intent(in) :: diags(n)
        type(json_builder_t), intent(inout) :: jb
        integer :: i

        call json_array_start(jb)
        do i = 1, n
            call diag_to_json(diags(i), jb)
        end do
        call json_array_end(jb)
    end subroutine diags_to_json

    subroutine diag_strip_prefix(d, prefix)
        type(diag_t), intent(inout) :: d
        character(len=*), intent(in) :: prefix
        integer :: plen, flen

        plen = len_trim(prefix)
        flen = len_trim(d%file)

        if (plen == 0 .or. flen < plen) return

        ! Require prefix match followed by '/' separator
        if (d%file(1:plen) /= prefix(1:plen)) return
        if (flen == plen) then
            d%file = ''
            return
        end if
        if (d%file(plen + 1:plen + 1) /= '/') return
        d%file = d%file(plen + 2:flen)
    end subroutine diag_strip_prefix

end module fx_diag
