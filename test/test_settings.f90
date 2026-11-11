module test_settings
!! Conversion of options from text, and their checks.
use omp_lib, only: omp_get_num_procs
use testdrive, only: new_unittest, unittest_type, error_type, check
use fdoc_input, only: needs_fypp
use fdoc_settings, only: settings_t, new_settings, set_option, check_settings, is_option
use fdoc_util, only: name_index_t, index_find, index_set, unique_tag, string_t, resolve_path, expand_vars, &
                     split_lines, split_words, strip
implicit none
private

public :: collect_settings

contains

!> The tests of this module.
subroutine collect_settings(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the tests
    testsuite = [ &
                new_unittest("logical options", test_logical), &
                new_unittest("text options join values", test_text), &
                new_unittest("map options", test_map), &
                new_unittest("extra_filetypes", test_filetypes), &
                new_unittest("defaults are completed by the checks", test_checks), &
                new_unittest("equal doc markers are rejected", test_docmarks), &
                new_unittest("option names", test_names), &
                new_unittest("paths", test_paths), &
                new_unittest("project graph limit", test_project_nodes), &
                new_unittest("threads default to every core", test_threads), &
                new_unittest("Fypp files are recognized", test_fypp), &
                new_unittest("name index", test_name_index), &
                new_unittest("unique tags", test_unique_tag), &
                new_unittest("line splitting", test_lines) &
                ]
end subroutine collect_settings

!> Checks: files that would go through Fypp are found, and other preprocessors are left alone.
subroutine test_fypp(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'fpp_extensions', [string_t('fypp')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, .not. needs_fypp(s, 'a.fypp'), 'the default preprocessor is not Fypp')
    if (allocated(error)) return
    call set_option(s, 'preprocessor', [string_t('fypp')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, needs_fypp(s, 'a.fypp'))
    if (allocated(error)) return
    call check(error, .not. needs_fypp(s, 'a.f90'), 'other files are read as usual')
end subroutine test_fypp

!> Checks: tags for temporary names differ.
subroutine test_unique_tag(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    character(len=32) :: first, second

    call unique_tag(first)
    call unique_tag(second)
    call check(error, len_trim(first) > 0 .and. first /= second)
end subroutine test_unique_tag

!> Checks: the hash table, as it grows.
subroutine test_name_index(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(name_index_t) :: table
    character(len=12) :: name
    integer :: k

    call check(error, index_find(table, 'none'), 0)
    if (allocated(error)) return
    do k = 1, 5000
        write (name, '(a,i0)') 'n', k
        call index_set(table, trim(name), k)
    end do
    do k = 1, 5000
        write (name, '(a,i0)') 'n', k
        call check(error, index_find(table, trim(name)), k)
        if (allocated(error)) return
    end do
    call index_set(table, 'n7', 99)
    call check(error, index_find(table, 'n7'), 99)
    if (allocated(error)) return
    call check(error, table%n, 5000, 'updating does not add a name')
    if (allocated(error)) return
    call check(error, index_find(table, 'n5001'), 0)
end subroutine test_name_index

!> Checks: `parallel` uses every core unless it is set.
subroutine test_threads(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call check(error, s%parallel, omp_get_num_procs())
    if (allocated(error)) return
    call set_option(s, 'parallel', [string_t('2')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, s%parallel, 2)
end subroutine test_threads

!> Checks: the limit on graphs of a whole project.
subroutine test_project_nodes(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call check(error, s%graph_project_maxnodes, 1000)
    if (allocated(error)) return
    call set_option(s, 'graph_project_maxnodes', [string_t('5000')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, s%graph_project_maxnodes, 5000)
end subroutine test_project_nodes

!> Checks: logical options.
subroutine test_logical(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'graph', [string_t('TRUE')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, s%graph)
    if (allocated(error)) return
    call set_option(s, 'graph', [string_t('yes')], stat, msg)
    call check(error, stat /= 0)
end subroutine test_logical

!> Checks: text options join values.
subroutine test_text(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'summary', [string_t('one'), string_t('two')], stat, msg)
    call check(error, s%summary, 'one'//achar(10)//'two')
end subroutine test_text

!> Checks: map options.
subroutine test_map(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'extra_mods', [string_t('m : https://x.org/a:b'), string_t('')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, size(s%extra_mods), 1)
    if (allocated(error)) return
    call check(error, s%extra_mods(1)%key, 'm')
    if (allocated(error)) return
    call check(error, s%extra_mods(1)%value, 'https://x.org/a:b')
    if (allocated(error)) return
    call set_option(s, 'alias', [string_t('no separator')], stat, msg)
    call check(error, stat /= 0)
end subroutine test_map

!> Checks: extra_filetypes.
subroutine test_filetypes(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'extra_filetypes', [string_t('.inc ! fortran')], stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, s%extra_filetypes(1)%extension, '.inc')
    if (allocated(error)) return
    call check(error, s%extra_filetypes(1)%lexer, 'fortran')
    if (allocated(error)) return
    call set_option(s, 'extra_filetypes', [string_t('.inc')], stat, msg)
    call check(error, stat /= 0)
end subroutine test_filetypes

!> Checks: defaults are completed by the checks.
subroutine test_checks(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call check_settings(s, stat, msg)
    call check(error, stat, 0)
    if (allocated(error)) return
    call check(error, s%relative)
    if (allocated(error)) return
    call check(error, s%exclude_dir(1)%s, './doc')
    if (allocated(error)) return
    call check(error, size(s%extensions), 12)
    if (allocated(error)) return
    call check(error, size(s%extra_mods), 9)
end subroutine test_checks

!> Checks: equal doc markers are rejected.
subroutine test_docmarks(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(settings_t) :: s
    character(len=:), allocatable :: msg
    integer :: stat

    s = new_settings()
    call set_option(s, 'predocmark', [string_t('!')], stat, msg)
    call check_settings(s, stat, msg)
    call check(error, stat /= 0)
    if (allocated(error)) return
    s = new_settings()
    call set_option(s, 'docmark_alt', [string_t('')], stat, msg)
    call set_option(s, 'predocmark_alt', [string_t('')], stat, msg)
    call check_settings(s, stat, msg)
    call check(error, stat, 0, 'empty markers may be equal')
end subroutine test_docmarks

!> Checks: option names.
subroutine test_names(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    call check(error, is_option('src_dir'))
    if (allocated(error)) return
    call check(error, .not. is_option('project_dir'))
    if (allocated(error)) return
    call check(error, .not. is_option(''))
    if (allocated(error)) return
    call check(error, .not. is_option('src_dir graph'))
end subroutine test_names

!> Checks: paths.
subroutine test_paths(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    call check(error, resolve_path('/', './no-such-dir-fdoc/a/../b'), '/no-such-dir-fdoc/b')
    if (allocated(error)) return
    call check(error, resolve_path('C:\a\b', '..\c'), 'C:/a/c')
    if (allocated(error)) return
    call check(error, resolve_path('C:/a', 'D:\z'), 'D:/z')
    if (allocated(error)) return
    call check(error, resolve_path('C:\a', '.'), 'C:/a')
    if (allocated(error)) return
    call check(error, expand_vars('a$FDOC_SURELY_UNSET/b'), 'a$FDOC_SURELY_UNSET/b')
end subroutine test_paths

!> Checks: line splitting.
subroutine test_lines(error)
    type(error_type), allocatable, intent(out) :: error  !! the failure, if any
    type(string_t), allocatable :: lines(:)

    lines = split_lines('a'//achar(13)//achar(10)//'b'//achar(10)//achar(10))
    call check(error, size(lines), 3)
    if (allocated(error)) return
    call check(error, lines(3)%s, '')
    if (allocated(error)) return
    call check(error, size(split_words('  x  y'//achar(9)//'z ')), 3)
    if (allocated(error)) return
    call check(error, strip('  x '), 'x')
end subroutine test_lines

end module test_settings
