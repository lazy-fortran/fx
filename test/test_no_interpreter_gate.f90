program test_no_interpreter_gate
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_result_t, proc_exec, proc_exec_silent, &
        proc_pid, proc_scan_files
    use fx_test_executable_policy, only: forbidden_executable
    use fx_test_process, only: test_process_trace_execs, &
        test_process_is_executable, test_process_trace_supported, &
        test_process_spawn, test_process_sleep_ms
    use fx_test_fs, only: fx_test_mkdir_p, fx_test_remove_tree, &
        fx_test_symlink, fx_test_chmod
    implicit none

    integer, parameter :: TRACE_CAPACITY = 1024 * 1024
    character(len=64), parameter :: mutant_classes(5) = &
        [character(len=64) :: 'shell', 'python', 'node', 'perl', 'ruby']
    character(len=256), parameter :: mutant_extensions(5) = &
        [character(len=256) :: '.sh', '.py', '.js', '.pl', '.rb']
    character(len=64), parameter :: mutant_names(5) = &
        [character(len=64) :: 'sh', 'python3', 'node', 'perl', 'ruby']
    type(test_suite_t) :: suite
    character(len=:), allocatable :: root, bin_path, driver_path, clean_path
    logical :: gate_run, gate_skip, trace_supported
    character(len=16) :: env_value
    character(len=4096) :: self_path, mode, gate_arguments(4)
    integer :: runtime_status

    call get_command_argument(0, self_path)
    call get_command_argument(1, mode)
    if (trim(mode) == '--runtime-launch') then
        call launch_runtime_mutant(runtime_status)
        if (runtime_status /= 0) stop 1
        stop
    end if
    if (trim(mode) == '--runtime-orphan') then
        call launch_orphan_mutant(runtime_status)
        if (runtime_status /= 0) stop 1
        stop
    end if
    if (trim(mode) == '--runtime-wait') then
        call test_process_sleep_ms(5000)
        stop
    end if

    call test_suite_init(suite, 'test_no_interpreter_gate')
    call get_environment_variable('FX_SKIP_NO_INTERPRETER_GATE', env_value)
    gate_skip = trim(env_value) == '1'
    if (gate_skip) stop

    call get_environment_variable('FX_RUN_NO_INTERPRETER_GATE', env_value)
    gate_run = trim(env_value) == '1'
    root = '/var/tmp/fx-no-interpreter-'//integer_text(proc_pid())
    bin_path = root//'/bin'
    call check_equal(fx_test_remove_tree(root), 0, 'old gate fixture is removed')
    call check_equal(fx_test_mkdir_p(bin_path), 0, 'private gate PATH is created')

    call check_shebang_forms()
    call check_checked_in_inventory()
    call check_generated_inventory()
    call check_generated_outputs()
    trace_supported = test_process_trace_supported()
    if (trace_supported) then
        call check_runtime_mutants()
        call check_trace_timeout()
    else
        call check(.not. gate_run, &
            'process tracing is enabled on the Linux acceptance runner')
    end if

    if (gate_run) then
        if (trace_supported) then
            call find_driver(driver_path)
            call create_compiler_path(bin_path, clean_path)
            gate_arguments(1) = driver_path
            gate_arguments(2) = 'build'
            call trace_test_run(gate_arguments(:2), clean_path, 'native build')
            gate_arguments(2) = 'test'
            gate_arguments(3) = 'test_fx_test'
            gate_arguments(4) = 'test_mcp_system'
            call trace_test_run(gate_arguments, clean_path, 'focused tests')
            call trace_test_run(gate_arguments(:2), clean_path, 'full tests')
        end if
    end if

    call check_equal(fx_test_remove_tree(root), 0, 'gate fixture is removed')
    call test_suite_summary(suite)
    call test_suite_exit(suite)
contains
    subroutine check(ok, name)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: name
        call test_assert(suite, ok, name)
    end subroutine check

    subroutine check_equal(actual, expected, name)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: name
        call test_assert_equal_int(suite, expected, actual, name)
    end subroutine check_equal

    function integer_text(value) result(text)
        integer, intent(in) :: value
        character(len=32) :: text
        write (text, '(I0)') value
    end function integer_text

    subroutine check_shebang_forms()
        character(len=256), parameter :: forbidden(22) = [character(len=256) :: &
            '#!/usr/bin/env FOO=bar python3', &
            '#!/usr/bin/env FOO="bar baz" python3', &
            '#!/usr/bin/env -P /usr/bin python3', &
            '#!/usr/bin/env -u FOO python3', &
            '#!/usr/bin/env -uFOO python3', &
            '#!/usr/bin/env --unset FOO python3', &
            '#!/usr/bin/env --unset=FOO FOO=bar python3', &
            '#!/usr/bin/env -C /tmp python3', &
            '#!/usr/bin/env --chdir=/tmp python3', &
            '#!/usr/bin/env -a chosen-name python3', &
            '#!/usr/bin/env --argv0=chosen-name python3', &
            '#!/usr/bin/env -i FOO=bar python3', &
            '#!/usr/bin/env -iu FOO python3', &
            '#!/usr/bin/env -S "FOO=bar python3"', &
            '#!/usr/bin/env -S "-u FOO python3"', &
            '#!/usr/bin/env --split-string="FOO=bar python3"', &
            '#!/usr/bin/env -Spython3', &
            '#!/usr/bin/env -- FOO=bar python3', &
            '#!/usr/bin/env env FOO=bar python3', &
            '#!/usr/bin/env -a "" python3', &
            '#!/usr/bin/env --unset FOO node', &
            '#!/usr/bin/env -u FOO ruby']
        character(len=256), parameter :: allowed(8) = [character(len=256) :: &
            '#!/usr/bin/env FOO=python3 gfortran', &
            '#!/usr/bin/env -u python3 gfortran', &
            '#!/usr/bin/env --unset=python3 gfortran', &
            '#!/usr/bin/env -C /python3 gfortran', &
            '#!/usr/bin/env -P /python3 gfortran', &
            '#!/usr/bin/env -a python3 gfortran', &
            '#!/usr/bin/env --argv0=python3 gfortran', &
            '#!/usr/bin/env -S "FOO=python3 gfortran"']
        integer :: i
        do i = 1, size(forbidden)
            call check(forbidden_executable('extensionless', trim(forbidden(i))), &
                'env interpreter command is rejected: '//trim(forbidden(i)))
        end do
        do i = 1, size(allowed)
            call check(.not. forbidden_executable('extensionless', trim(allowed(i))), &
                'env option and assignment data is allowed: '//trim(allowed(i)))
        end do
    end subroutine check_shebang_forms

    subroutine check_checked_in_inventory()
        type(proc_result_t) :: listing
        character(len=:), allocatable :: line, path, mode
        integer :: start, finish, tab, next_line, mutant_count, i
        logical :: mutant_path, forbidden
        character(len=16) :: arguments(3)

        arguments(1) = 'git'
        arguments(2) = 'ls-files'
        arguments(3) = '-s'
        call proc_exec(arguments, 3, listing)
        call check(listing%exit_code == 0, 'tracked source inventory command succeeds')
        mutant_count = 0
        start = 1
        do while (start <= len(listing%stdout_text))
            next_line = index(listing%stdout_text(start:), achar(10))
            if (next_line == 0) then
                finish = len(listing%stdout_text)
            else
                finish = start + next_line - 2
            end if
            if (finish >= start) then
                line = listing%stdout_text(start:finish)
                tab = index(line, achar(9))
                if (tab > 0) then
                    mode = line(:6)
                    path = line(tab + 1:)
                    mutant_path = index(path, &
                        'test/fixtures/no_interpreter_mutants/checked_in_') == 1
                    forbidden = forbidden_executable(path, read_shebang(path))
                    if (mutant_path) then
                        mutant_count = mutant_count + 1
                        call check(forbidden, 'checked-in interpreter mutant is rejected: '//path)
                    else if (forbidden) then
                        call check(.false., 'tracked interpreter executable: '//path)
                    end if
                    if (mode == '100755' .and. mutant_path) then
                        call check(forbidden, 'executable bit mutant is rejected: '//path)
                    end if
                end if
            end if
            if (next_line == 0) exit
            start = finish + 2
        end do
        call check(mutant_count == 7, 'all checked-in language mutants are inventoried')
    end subroutine check_checked_in_inventory

    subroutine check_generated_inventory()
        character(len=:), allocatable :: files(:), path, fixture_path
        character(len=512) :: source
        integer :: count, ierr, i, unit
        logical :: fixture, forbidden

        fixture_path = root//'/generated'
        call check_equal(fx_test_mkdir_p(fixture_path), 0, &
            'generated mutant scratch is created')
        call check_equal(fx_test_mkdir_p(fixture_path), 0, &
            'generated mutant directory is created')
        do i = 1, size(mutant_classes)
            path = fixture_path//'/generated_'//trim(mutant_classes(i))// &
                trim(mutant_extensions(i))
            source = mutant_source(i)
            open (newunit=unit, file=path, status='replace', action='write')
            write (unit, '(A)') trim(source)
            close (unit)
            call check_equal(fx_test_chmod(path, 493), 0, &
                'generated mutant is executable: '//trim(mutant_classes(i)))
        end do
        call write_generated_control(fixture_path//'/env_assignment.fixture', &
            '#!/usr/bin/env FOO=bar python3')
        call write_generated_control(fixture_path//'/env_unset.fixture', &
            '#!/usr/bin/env -u FOO python3')
        call proc_scan_files(fixture_path, files, count, ierr)
        call check_equal(ierr, 0, 'generated executable inventory succeeds')
        do i = 1, count
            path = trim(files(i))
            fixture = index(path, fixture_path//'/') == 1
            forbidden = forbidden_executable(path, read_shebang(path))
            if (fixture) then
                call check(forbidden, 'generated interpreter mutant is rejected: '//path)
            else if (forbidden) then
                call check(.false., 'generated interpreter executable: '//path)
            end if
        end do
        call check(test_process_is_executable(fixture_path//'/'// &
            'generated_shell.sh') == 1, 'generated mutant has executable mode')
    end subroutine check_generated_inventory

    subroutine write_generated_control(path, shebang)
        character(len=*), intent(in) :: path, shebang
        integer :: unit
        open (newunit=unit, file=path, status='replace', action='write')
        write (unit, '(A)') shebang
        close (unit)
        call check_equal(fx_test_chmod(path, 493), 0, &
            'generated env mutant is executable: '//path)
    end subroutine write_generated_control

    subroutine check_generated_outputs()
        character(len=:), allocatable :: files(:), path
        integer :: count, ierr, i
        call proc_scan_files('build', files, count, ierr)
        call check_equal(ierr, 0, 'build output inventory succeeds')
        do i = 1, count
            path = trim(files(i))
            call check(.not. forbidden_executable(path, read_shebang(path)), &
                'generated build output is native or data: '//path)
        end do
    end subroutine check_generated_outputs

    function mutant_source(index) result(source)
        integer, intent(in) :: index
        character(len=512) :: source
        select case (index)
        case (1)
            source = '#!/bin/sh'
        case (2)
            source = '#!/usr/bin/env python3'
        case (3)
            source = '#!/usr/bin/env node'
        case (4)
            source = '#!/usr/bin/env perl'
        case default
            source = '#!/usr/bin/env ruby'
        end select
    end function mutant_source

    subroutine launch_runtime_mutant(status)
        integer, intent(out) :: status
        character(len=4096) :: arguments(3)
        integer :: i
        do i = 1, 3
            call get_command_argument(i + 1, arguments(i))
        end do
        call proc_exec_silent(arguments, 3, status)
    end subroutine launch_runtime_mutant

    subroutine launch_orphan_mutant(status)
        integer, intent(out) :: status
        character(len=4096) :: arguments(5)
        integer :: i, pid
        arguments(1) = self_path
        arguments(2) = '--runtime-launch'
        do i = 3, 5
            call get_command_argument(i - 1, arguments(i))
        end do
        call test_process_spawn(arguments, pid, status)
    end subroutine launch_orphan_mutant

    subroutine check_trace_timeout()
        character(kind=c_char) :: paths(TRACE_CAPACITY)
        character(len=4096) :: arguments(2)
        character(len=1) :: empty_environment(0)
        integer :: status, ierr
        arguments(1) = self_path
        arguments(2) = '--runtime-wait'
        call test_process_trace_execs(arguments, empty_environment, &
            paths, status, ierr, 50)
        call check(ierr /= 0, 'trace timeout is reported as failure')
        call check_equal(status, 137, 'timed-out tracee is killed and reaped')
    end subroutine check_trace_timeout

    subroutine check_runtime_mutants()
        character(kind=c_char) :: paths(TRACE_CAPACITY)
        character(len=:), allocatable :: forbidden
        character(len=4096) :: arguments(5), interpreter
        integer :: ierr, status, i
        character(len=1) :: empty_environment(0)

        arguments(1) = self_path
        arguments(2) = '--runtime-orphan'
        arguments(4) = '-c'
        do i = 1, size(mutant_names)
            call find_tool(trim(mutant_names(i)), interpreter, ierr)
            call check_equal(ierr, 0, 'runtime interpreter control is available')
            if (ierr /= 0) cycle
            arguments(3) = interpreter
            arguments(4) = '-c'
            if (i >= 3) arguments(4) = '-e'
            arguments(5) = '0'
            if (i == 1) arguments(5) = ':'
            call test_process_trace_execs(arguments, &
                empty_environment, paths, status, ierr, 5000)
            call check_equal(ierr, 0, 'runtime descendant trace succeeds')
            call check_equal(status, 0, 'runtime descendant exits normally')
            forbidden = forbidden_trace_path(paths)
            call check(len(forbidden) > 0, &
                'runtime interpreter descendant is rejected: '//trim(mutant_names(i)))
        end do
    end subroutine check_runtime_mutants

    subroutine find_driver(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=4096) :: configured
        integer :: length, status

        call get_environment_variable('FX_FO_EXECUTABLE', configured, &
            length=length, status=status)
        path = ''
        call check(status == 0 .and. length > 0, 'absolute fo driver is configured')
        if (status /= 0 .or. length < 1) return
        path = configured(:length)
        call check(path(1:1) == '/', 'fo driver path is absolute')
        call check(test_process_is_executable(path) == 1, 'fo driver is executable')
    end subroutine find_driver

    subroutine create_compiler_path(directory, path)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: path
        character(len=2048), parameter :: tools(13) = [character(len=2048) :: &
            'gfortran', 'gcc', 'cc', 'ld', 'as', 'ar', 'ranlib', &
            'collect2', 'mkdir', 'ln', 'chmod', 'rm', 'git']
        character(len=4096) :: source
        integer :: i, ierr

        path = directory
        do i = 1, size(tools)
            call find_tool(trim(tools(i)), source, ierr)
            if (ierr == 0) then
                ierr = fx_test_symlink(trim(source), &
                    trim(directory)//'/'//trim(tools(i)))
                if (ierr /= 0 .and. ierr /= 17) then
                    call check(.false., 'allowed tool link created: '//trim(tools(i)))
                end if
            else if (tools(i) == 'gfortran' .or. tools(i) == 'gcc' .or. &
                    tools(i) == 'cc' .or. tools(i) == 'ld' .or. &
                    tools(i) == 'ar' .or. tools(i) == 'as') then
                call check(.false., 'required compiler tool found: '//trim(tools(i)))
            end if
        end do
    end subroutine create_compiler_path

    subroutine find_tool(name, path, ierr)
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: path
        integer, intent(out) :: ierr
        character(len=4096) :: search_path, candidate
        integer :: length, start, finish, colon

        path = ''
        ierr = 1
        call get_environment_variable('PATH', search_path, length=length)
        start = 1
        do while (start <= length)
            colon = index(search_path(start:length), ':')
            if (colon == 0) then
                finish = length
            else
                finish = start + colon - 2
            end if
            if (finish >= start) then
                candidate = trim(search_path(start:finish))//'/'//trim(name)
                if (test_process_is_executable(trim(candidate)) == 1) then
                    path = trim(candidate)
                    ierr = 0
                    return
                end if
            end if
            if (colon == 0) exit
            start = finish + 2
        end do
    end subroutine find_tool

    subroutine trace_test_run(arguments, path, label)
        character(len=*), intent(in) :: arguments(:)
        character(len=*), intent(in) :: path, label
        character(kind=c_char) :: exec_paths(TRACE_CAPACITY)
        character(len=:), allocatable :: forbidden
        integer :: status, ierr
        character(len=4096) :: environment(4)

        environment(1) = 'PATH='//trim(path)
        environment(2) = 'FX_SKIP_NO_INTERPRETER_GATE=1'
        environment(3) = 'FO_DISABLE_SELF_REFRESH=1'
        environment(4) = 'FO_SELF_REFRESH=0'
        call test_process_trace_execs(arguments, environment, exec_paths, status, &
            ierr, 180000)
        call check_equal(ierr, 0, label//' process tree is traced')
        call check_equal(status, 0, label//' process tree passes')
        forbidden = forbidden_trace_path(exec_paths)
        call check(len(forbidden) == 0, &
            label//' process tree contains no interpreter: '//forbidden)
    end subroutine trace_test_run

    function forbidden_trace_path(paths) result(path)
        character(kind=c_char), intent(in) :: paths(:)
        character(len=:), allocatable :: path, one_path
        integer :: start, finish
        path = ''
        start = 1
        do while (start <= size(paths))
            finish = start
            do while (finish <= size(paths))
                if (paths(finish) == c_null_char .or. paths(finish) == achar(10)) exit
                finish = finish + 1
            end do
            if (finish > start) then
                one_path = chars_to_text(paths(start:finish - 1))
                if (forbidden_executable(one_path, '')) then
                    path = one_path
                    return
                end if
            end if
            if (finish > size(paths)) exit
            if (paths(finish) == c_null_char) exit
            start = finish + 1
        end do
    end function forbidden_trace_path

    function chars_to_text(chars) result(text)
        character(kind=c_char), intent(in) :: chars(:)
        character(len=:), allocatable :: text
        integer :: i
        allocate(character(len=size(chars)) :: text)
        do i = 1, size(chars)
            text(i:i) = chars(i)
        end do
    end function chars_to_text

    function read_shebang(path) result(line)
        character(len=*), intent(in) :: path
        character(len=512) :: line
        integer :: unit, status
        line = ''
        open (newunit=unit, file=path, status='old', action='read', &
            iostat=status)
        if (status /= 0) return
        read (unit, '(A)', iostat=status) line
        close (unit)
        if (status /= 0) line = ''
    end function read_shebang

end program test_no_interpreter_gate
