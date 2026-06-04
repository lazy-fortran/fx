module fx_path
    implicit none
    private

    public :: path_join, path_dirname, path_basename
    public :: path_extension, path_stem, path_strip_prefix
    public :: path_normalize, path_is_absolute, path_relative
    public :: path_exists, path_is_dir, path_is_file

contains

    pure function path_join(a, b) result(res)
        character(len=*), intent(in) :: a
        character(len=*), intent(in) :: b
        character(len=:), allocatable :: res
        error stop "fx_path:path_join not implemented"
    end function path_join

    pure function path_dirname(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        error stop "fx_path:path_dirname not implemented"
    end function path_dirname

    pure function path_basename(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        error stop "fx_path:path_basename not implemented"
    end function path_basename

    pure function path_extension(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        error stop "fx_path:path_extension not implemented"
    end function path_extension

    pure function path_stem(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        error stop "fx_path:path_stem not implemented"
    end function path_stem

    pure function path_strip_prefix(p, prefix) result(res)
        character(len=*), intent(in) :: p
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: res
        error stop "fx_path:path_strip_prefix not implemented"
    end function path_strip_prefix

    pure function path_normalize(p) result(res)
        character(len=*), intent(in) :: p
        character(len=:), allocatable :: res
        error stop "fx_path:path_normalize not implemented"
    end function path_normalize

    pure logical function path_is_absolute(p)
        character(len=*), intent(in) :: p
        error stop "fx_path:path_is_absolute not implemented"
    end function path_is_absolute

    pure function path_relative(p, base) result(res)
        character(len=*), intent(in) :: p
        character(len=*), intent(in) :: base
        character(len=:), allocatable :: res
        error stop "fx_path:path_relative not implemented"
    end function path_relative

    logical function path_exists(p)
        character(len=*), intent(in) :: p
        error stop "fx_path:path_exists not implemented"
    end function path_exists

    logical function path_is_dir(p)
        character(len=*), intent(in) :: p
        error stop "fx_path:path_is_dir not implemented"
    end function path_is_dir

    logical function path_is_file(p)
        character(len=*), intent(in) :: p
        error stop "fx_path:path_is_file not implemented"
    end function path_is_file

end module fx_path
