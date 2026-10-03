module fx_immutable_constants
    implicit none
    private

    integer, parameter, public :: IMMUTABLE_OK = 0
    integer, parameter, public :: IMMUTABLE_IO_ERROR = 1
    integer, parameter, public :: IMMUTABLE_INVALID = 2
    integer, parameter, public :: IMMUTABLE_MISSING = 3
    ! Corrupt objects are preserved at their digest path for diagnosis. Verify
    ! and put operations return this status; they never trust, overwrite, or
    ! quarantine the established path.
    integer, parameter, public :: IMMUTABLE_CORRUPT = 4
    integer, parameter, public :: IMMUTABLE_UNSUPPORTED = 5
end module fx_immutable_constants
