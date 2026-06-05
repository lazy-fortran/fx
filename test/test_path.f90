program test_path
    use fx_test, only: test_suite_t, test_suite_init, &
                       test_suite_summary, test_suite_exit, &
                       test_assert, test_assert_equal_str
    use fx_path, only: path_join, path_dirname, path_basename, &
                       path_extension, path_stem, path_strip_prefix, &
                       path_normalize, path_is_absolute, path_relative, &
                       path_exists, path_is_dir, path_is_file
    implicit none

    type(test_suite_t) :: suite

    call test_suite_init(suite, 'fx_path')
    call test_path_join(suite)
    call test_path_dirname_basename(suite)
    call test_path_extension_stem(suite)
    call test_path_strip_prefix(suite)
    call test_path_normalize(suite)
    call test_path_is_absolute(suite)
    call test_path_relative(suite)
    call test_suite_summary(suite)
    call test_suite_exit(suite)

contains

    subroutine test_path_join(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'a/b', path_join('a', 'b'), &
                                   'join: simple')
        call test_assert_equal_str(suite, '/a/b', path_join('/a', 'b'), &
                                   'join: absolute base')
        call test_assert_equal_str(suite, 'a/b', path_join('a/', 'b'), &
                                   'join: trailing slash stripped')
        call test_assert_equal_str(suite, '/b', path_join('/a', '/b'), &
                                   'join: abs b wins')
        call test_assert_equal_str(suite, 'b', path_join('', 'b'), &
                                   'join: empty base')
        call test_assert_equal_str(suite, 'a', path_join('a', ''), &
                                   'join: empty child')
        call test_assert_equal_str(suite, '/a/b', path_join('/a/', 'b'), &
                                   'join: root trailing slash')
    end subroutine test_path_join

    subroutine test_path_dirname_basename(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, '/a', path_dirname('/a/b'), &
                                   'dirname: simple')
        call test_assert_equal_str(suite, '/', path_dirname('/a'), &
                                   'dirname: root child')
        call test_assert_equal_str(suite, '.', path_dirname('a'), &
                                   'dirname: no slash')
        call test_assert_equal_str(suite, '/', path_dirname('/'), &
                                   'dirname: root')
        call test_assert_equal_str(suite, 'a/b', path_dirname('a/b/c'), &
                                   'dirname: two levels')

        call test_assert_equal_str(suite, 'b', path_basename('/a/b'), &
                                   'basename: simple')
        call test_assert_equal_str(suite, 'a', path_basename('a'), &
                                   'basename: no slash')
        call test_assert_equal_str(suite, '/', path_basename('/'), &
                                   'basename: root')
        call test_assert_equal_str(suite, 'c', path_basename('a/b/c'), &
                                   'basename: deep')
        call test_assert_equal_str(suite, 'b', path_basename('/a/b/'), &
                                   'basename: trailing slash stripped')
    end subroutine test_path_dirname_basename

    subroutine test_path_extension_stem(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, '.f90', path_extension('foo.f90'), &
                                   'extension: .f90')
        call test_assert_equal_str(suite, '.gz', &
                                   path_extension('archive.tar.gz'), &
                                   'extension: last dot only')
        call test_assert_equal_str(suite, '', path_extension('README'), &
                                   'extension: no dot')
        call test_assert_equal_str(suite, '', path_extension('.hidden'), &
                                   'extension: dotfile no ext')
        call test_assert_equal_str(suite, '.f90', &
                                   path_extension('/a/b/foo.f90'), &
                                   'extension: full path')

        call test_assert_equal_str(suite, 'foo', path_stem('foo.f90'), &
                                   'stem: basic')
        call test_assert_equal_str(suite, 'archive.tar', &
                                   path_stem('archive.tar.gz'), &
                                   'stem: keeps first dots')
        call test_assert_equal_str(suite, 'README', path_stem('README'), &
                                   'stem: no dot')
        call test_assert_equal_str(suite, '.hidden', path_stem('.hidden'), &
                                   'stem: dotfile')
    end subroutine test_path_extension_stem

    subroutine test_path_strip_prefix(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'b/c', &
                                   path_strip_prefix('/a/b/c', '/a'), &
                                   'strip_prefix: strips prefix')
        call test_assert_equal_str(suite, '/x/y', &
                                   path_strip_prefix('/x/y', '/z'), &
                                   'strip_prefix: no match unchanged')
        call test_assert_equal_str(suite, '', &
                                   path_strip_prefix('/a', '/a'), &
                                   'strip_prefix: exact match empty')
        call test_assert_equal_str(suite, '/a/bc', &
                                   path_strip_prefix('/a/bc', '/a/b'), &
                                   'strip_prefix: no partial dir match')
    end subroutine test_path_strip_prefix

    subroutine test_path_normalize(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'a/b', path_normalize('a//b'), &
                                   'normalize: double slash')
        call test_assert_equal_str(suite, 'a/b', path_normalize('a/./b'), &
                                   'normalize: dot segment')
        call test_assert_equal_str(suite, 'a', path_normalize('a/b/..'), &
                                   'normalize: parent')
        call test_assert_equal_str(suite, '/a/c', path_normalize('/a/b/../c'), &
                                   'normalize: abs parent')
        call test_assert_equal_str(suite, '/', path_normalize('/../../..'), &
                                   'normalize: parent past root')
        call test_assert_equal_str(suite, '.', path_normalize(''), &
                                   'normalize: empty is dot')
        call test_assert_equal_str(suite, '.', path_normalize('.'), &
                                   'normalize: dot')
        call test_assert_equal_str(suite, '/a/b', path_normalize('/a/b/'), &
                                   'normalize: trailing slash stripped')
        call test_assert_equal_str(suite, '..', path_normalize('a/../..'), &
                                   'normalize: parent of relative')
    end subroutine test_path_normalize

    subroutine test_path_is_absolute(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert(suite, path_is_absolute('/a/b'), 'is_absolute: /a/b')
        call test_assert(suite, path_is_absolute('/'), 'is_absolute: /')
        call test_assert(suite, .not. path_is_absolute('a/b'), &
                         'is_absolute: relative')
        call test_assert(suite, .not. path_is_absolute(''), 'is_absolute: empty')
        call test_assert(suite, .not. path_is_absolute('./a'), &
                         'is_absolute: dot relative')
    end subroutine test_path_is_absolute

    subroutine test_path_relative(suite)
        type(test_suite_t), intent(inout) :: suite

        call test_assert_equal_str(suite, 'b/c', &
                                   path_relative('/a', '/a/b/c'), &
                                   'relative: descend')
        call test_assert_equal_str(suite, '../b/c', &
                                   path_relative('/a/d', '/a/b/c'), &
                                   'relative: up then down')
        call test_assert_equal_str(suite, '.', &
                                   path_relative('/a/b/c', '/a/b/c'), &
                                   'relative: same path')
        call test_assert_equal_str(suite, '../../d', &
                                   path_relative('/a/b/c', '/a/d'), &
                                   'relative: multiple ups')
    end subroutine test_path_relative

end program test_path
