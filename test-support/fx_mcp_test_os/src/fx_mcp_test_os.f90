module fx_mcp_test_os
    use iso_c_binding, only: c_int, c_char, c_ptr
    implicit none
    private
    public :: fx_test_spawn, fx_test_find_server, fx_test_write
    public :: fx_test_read, fx_test_close
    interface
        type(c_ptr) function fx_test_spawn(path) bind(C)
            import c_ptr, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function
        integer(c_int) function fx_test_find_server(test_binary, path, n) bind(C)
            import c_int, c_char
            character(kind=c_char), intent(in) :: test_binary(*)
            character(kind=c_char), intent(out) :: path(*)
            integer(c_int), value :: n
        end function
        integer(c_int) function fx_test_write(handle, bytes, n) bind(C)
            import c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: bytes(*)
            integer(c_int), value :: n
        end function
        integer(c_int) function fx_test_read(handle, bytes, n, timeout) bind(C)
            import c_ptr, c_char, c_int
            type(c_ptr), value :: handle
            character(kind=c_char), intent(out) :: bytes(*)
            integer(c_int), value :: n, timeout
        end function
        integer(c_int) function fx_test_close(handle, timeout) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: handle
            integer(c_int), value :: timeout
        end function
    end interface
end module fx_mcp_test_os
