program test_no_interpreter_gate
    use, intrinsic :: iso_c_binding, only: c_char, c_null_char
    use fx_test, only: test_suite_t, test_suite_init, test_assert, &
        test_assert_equal_int, test_suite_summary, test_suite_exit
    use fx_proc, only: proc_result_t, proc_exec, proc_exec_silent, &
        proc_pid, proc_scan_files, proc_file_read, proc_file_write
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
    character(len=64), parameter :: mutant_names(25) = [character(len=64) :: &
        'sh', 'bash', 'dash', 'ash', 'zsh', 'ksh', 'csh', 'tcsh', 'fish', &
        'python3', 'node', 'nodejs', 'npm', 'npx', 'perl', 'ruby', 'lua', &
        'php', 'awk', 'mawk', 'gawk', 'tclsh', 'wish', 'R', 'Rscript']
    type(test_suite_t) :: suite
    character(len=:), allocatable :: root, bin_path, driver_path, clean_path
    logical :: gate_run, gate_skip, trace_supported
    character(len=16) :: env_value
    character(len=4096) :: self_path, mode, gate_arguments(4)
    integer :: runtime_status

    call get_command_argument(0, self_path)
    call get_command_argument(1, mode)
    if (trim(mode) == '--runtime-native') stop
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
    root = '/var/tmp/fx-no-interpreter-'//trim(integer_text(proc_pid()))
    bin_path = root//'/bin'
    call check_equal(fx_test_remove_tree(root), 0, 'old gate fixture is removed')
    call check_equal(fx_test_mkdir_p(bin_path), 0, 'private gate PATH is created')

    call check_command_names()
    call check_shebang_forms()
    call check_tracked_inventory('.', 0, 'repository')
    call check_temporary_tracked_mutants()
    call check_generated_inventory()
    call check_generated_outputs()
    trace_supported = test_process_trace_supported()
    if (trace_supported) then
        call check_runtime_mutants()
        call check_versioned_runtime()
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

    subroutine check_command_names()
        character(len=32), parameter :: forbidden(28) = [character(len=32) :: &
            'sh1', 'bash5.2', 'dash0.5', 'ash1.0', 'zsh5.9', 'ksh93', &
            'csh6', 'tcsh6.24', 'fish4.1', 'python3.14', 'node22', &
            'nodejs22', 'npm10', 'npx10', 'perl5.40.0', 'ruby3.3', &
            'lua5.4', 'php8.3', 'awk5.3', 'mawk1.3.4', 'gawk5.4.1', &
            'gawk-5.4.1', 'tclsh8.6', 'wish8.6', 'R4.5', 'R-4.5.1', &
            'Rscript4.5', 'ruby-3.3']
        character(len=32), parameter :: allowed(18) = [character(len=32) :: &
            'rubyhelper', 'ruby3.3helper', 'ruby.', 'ruby3.', 'ruby3..3', &
            'ruby-3.3-helper', 'perltex', 'perl5-config', 'luatex', &
            'lua5.4-helper', 'php-config', 'gawkbug', 'gawk-5.4.1-tools', &
            'tclshlib', 'Rscript-helper', 'r2', 'R2-D2', 'node_modules']
        integer :: i
        do i = 1, size(forbidden)
            call check(forbidden_executable('/native/'//trim(forbidden(i)), ''), &
                'versioned interpreter name is rejected: '//trim(forbidden(i)))
            call check(forbidden_executable('extensionless', &
                '#!/usr/bin/env '//trim(forbidden(i))), &
                'versioned shebang command is rejected: '//trim(forbidden(i)))
        end do
        do i = 1, size(allowed)
            call check(.not. forbidden_executable('/native/'//trim(allowed(i)), ''), &
                'unrelated native command is allowed: '//trim(allowed(i)))
        end do
    end subroutine check_command_names

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

    subroutine check_tracked_inventory(directory, expected, label)
        character(len=*), intent(in) :: directory, label
        integer, intent(in) :: expected
        type(proc_result_t) :: listing
        character(len=:), allocatable :: path
        character(len=4096) :: arguments(4)
        integer :: start, finish, next_line, forbidden_count, file_count
        arguments(1) = 'git'
        arguments(2) = '-C'
        arguments(3) = directory
        arguments(4) = 'ls-files'
        call proc_exec(arguments, 4, listing)
        call check_equal(listing%exit_code, 0, label//' tracked inventory succeeds')
        forbidden_count = 0
        file_count = 0
        start = 1
        do while (start <= len(listing%stdout_text))
            next_line = index(listing%stdout_text(start:), achar(10))
            finish = len(listing%stdout_text)
            if (next_line > 0) finish = start + next_line - 2
            if (finish >= start) then
                path = directory//'/'//listing%stdout_text(start:finish)
                file_count = file_count + 1
                if (forbidden_executable(path, read_shebang(path))) then
                    forbidden_count = forbidden_count + 1
                    if (expected == 0) call check(.false., &
                        'tracked interpreter executable: '//path)
                end if
            end if
            if (next_line == 0) exit
            start = finish + 2
        end do
        call check(file_count > 0, label//' tracked inventory is nonempty')
        call check_equal(forbidden_count, expected, &
            label//' tracked interpreter controls are classified')
    end subroutine check_tracked_inventory

    subroutine check_temporary_tracked_mutants()
        character(len=:), allocatable :: directory, path
        character(len=4096) :: arguments(7)
        integer :: ierr, i, unit
        directory = root//'/tracked-controls'
        call check_equal(fx_test_mkdir_p(directory), 0, &
            'temporary tracked-control repository is created')
        arguments(1) = 'git'
        arguments(2) = '-C'
        arguments(3) = directory
        arguments(4) = 'init'
        arguments(5) = '--quiet'
        arguments(6) = '--template='
        call proc_exec_silent(arguments, 6, ierr)
        call check_equal(ierr, 0, 'temporary native Git repository initializes')
        do i = 1, size(mutant_names)
            path = directory//'/control_'//trim(mutant_names(i))//'.fixture'
            call write_generated_control(path, &
                '#!/usr/bin/env '//trim(mutant_names(i)))
        end do
        do i = 1, size(mutant_classes)
            path = directory//'/extension_'//trim(mutant_classes(i))// &
                trim(mutant_extensions(i))
            call write_generated_control(path, trim(mutant_source(i)))
        end do
        call write_generated_control(directory//'/env_assignment.fixture', &
            '#!/usr/bin/env FOO=bar python3')
        call write_generated_control(directory//'/env_unset.fixture', &
            '#!/usr/bin/env -u FOO python3')
        open (newunit=unit, file=directory//'/native.f90', status='replace')
        write (unit, '(A)') 'program native_control'
        write (unit, '(A)') 'end program'
        close (unit)
        arguments(4) = 'add'
        arguments(5) = '-f'
        arguments(6) = '--'
        arguments(7) = '.'
        call proc_exec_silent(arguments, 7, ierr)
        call check_equal(ierr, 0, 'temporary Fortran-generated controls are indexed')
        call check_tracked_inventory(directory, size(mutant_names) + &
            size(mutant_classes) + 2, 'temporary controls')
    end subroutine check_temporary_tracked_mutants

    subroutine check_generated_inventory()
        character(len=:), allocatable :: files(:), path, fixture_path
        character(len=512) :: source
        integer :: count, ierr, i, unit

        fixture_path = root//'/generated'
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
        do i = 1, size(mutant_names)
            path = fixture_path//'/family_'//trim(mutant_names(i))//'.fixture'
            call write_generated_control(path, &
                '#!/usr/bin/env '//trim(mutant_names(i)))
        end do
        call write_generated_control(fixture_path//'/env_assignment.fixture', &
            '#!/usr/bin/env FOO=bar python3')
        call write_generated_control(fixture_path//'/env_unset.fixture', &
            '#!/usr/bin/env -u FOO python3')
        call proc_scan_files(fixture_path, files, count, ierr)
        call check_equal(ierr, 0, 'generated executable inventory succeeds')
        call check_equal(count, size(mutant_names) + size(mutant_classes) + 2, &
            'generated inventory contains every independent control')
        do i = 1, count
            path = trim(files(i))
            call check(forbidden_executable(path, read_shebang(path)), &
                'generated interpreter mutant is rejected: '//path)
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
        character(len=:), allocatable :: content, target, alias, forbidden
        character(len=4096) :: arguments(5)
        integer :: ierr, status, i, byte_count
        character(len=1) :: empty_environment(0)
        call proc_file_read(trim(self_path), content, byte_count, ierr)
        call check_equal(ierr, 0, 'native Fortran control image is read')
        if (ierr /= 0) return
        arguments(1) = self_path
        arguments(2) = '--runtime-orphan'
        arguments(4) = '--runtime-native'
        arguments(5) = ''
        do i = 1, size(mutant_names)
            target = root//'/'//trim(mutant_names(i))
            alias = root//'/neutral-'//trim(integer_text(i))
            call write_native_image(target, alias, content, byte_count)
            arguments(3) = alias
            call test_process_trace_execs(arguments, &
                empty_environment, paths, status, ierr, 5000)
            call check_equal(ierr, 0, 'native descendant trace succeeds')
            call check_equal(status, 0, 'native descendant exits normally')
            forbidden = forbidden_trace_path(paths)
            call check(forbidden == target, &
                'resolved forbidden family is rejected: '//trim(mutant_names(i)))
            call check_equal(fx_test_remove_tree(alias), 0, 'native alias is removed')
            call check_equal(fx_test_remove_tree(target), 0, 'native image is removed')
        end do
    end subroutine check_runtime_mutants

    subroutine write_native_image(target, alias, content, byte_count)
        character(len=*), intent(in) :: target, alias, content
        integer, intent(in) :: byte_count
        integer :: ierr
        call proc_file_write(target, content, byte_count, ierr)
        call check_equal(ierr, 0, 'native Fortran fixture image is written')
        call check_equal(fx_test_chmod(target, 493), 0, &
            'native Fortran fixture is executable')
        call check_equal(fx_test_symlink(target, alias), 0, &
            'neutral runtime alias is created')
        call check(.not. forbidden_executable(alias, ''), &
            'neutral alias has no interpreter name')
    end subroutine write_native_image

    subroutine check_versioned_runtime()
        character(kind=c_char) :: paths(TRACE_CAPACITY)
        character(len=:), allocatable :: content, target, alias, forbidden
        character(len=4096) :: arguments(2)
        character(len=1) :: empty_environment(0)
        integer :: ierr, status, byte_count
        target = root//'/ruby3.3'
        alias = root//'/native-runtime-alias'
        call proc_file_read(trim(self_path), content, byte_count, ierr)
        call check_equal(ierr, 0, 'native Fortran version control image is read')
        if (ierr /= 0) return
        call write_native_image(target, alias, content, byte_count)
        arguments(1) = alias
        arguments(2) = '--runtime-native'
        call test_process_trace_execs(arguments, empty_environment, &
            paths, status, ierr, 5000)
        call check_equal(ierr, 0, 'versioned native runtime alias is traced')
        call check_equal(status, 0, 'versioned native fixture executes normally')
        forbidden = forbidden_trace_path(paths)
        call check(forbidden == target, &
            'trace rejects resolved ruby3.3 name behind neutral alias')
    end subroutine check_versioned_runtime

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
