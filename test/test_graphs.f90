module test_graphs
!! Graphs, from a small project on real files, through the whole program.
use testdrive, only: new_unittest, unittest_type, error_type, check
use fdoc, only: run
use fdoc_util, only: string_t, read_whole_file, write_whole_file
use fdoc_shell, only: make_directory, remove_directory
implicit none
private

public :: collect_graphs

! this test's own directory, as tests run in parallel
character(len=*), parameter :: ROOT = 'build/test graphs'
character(len=*), parameter :: NL = new_line('a')

contains

!> The tests of this module.
subroutine collect_graphs(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the tests

    testsuite = [new_unittest("drawn in pages", test_drawn), new_unittest("cut at the depth limit", test_cut)]
end subroutine collect_graphs

!> Checks: pages show drawings with links and labels, a cycle is drawn, and `graph_dir` keeps the graphs.
subroutine test_drawn(error)
    type(error_type), allocatable, intent(out) :: error     !! the failure, if any
    type(string_t) :: args(4)
    character(len=:), allocatable :: text, errmsg
    integer :: stat

    call remove_directory(ROOT)
    call make_directory(ROOT//'/src', stat)
    call write_whole_file(ROOT//'/ford.md', 'project: graphs'//NL//'src_dir: ./src'//NL//'output_dir: ./doc'//NL// &
                          'graph: true'//NL//NL//'The project.'//NL, stat, errmsg)
    call check(error, stat, 0, 'the project file')
    if (allocated(error)) return
    call write_whole_file(ROOT//'/src/a.f90', &
                          'module low'//NL//'type :: inner'//NL//'end type inner'//NL//'contains'//NL// &
                          'recursive subroutine ping()'//NL//'call pong()'//NL//'end subroutine ping'//NL// &
                          'recursive subroutine pong()'//NL//'call ping()'//NL//'end subroutine pong'//NL// &
                          'end module low'//NL//'module high'//NL//'use low'//NL//'type :: outer'//NL// &
                          'type(inner) :: part'//NL//'end type outer'//NL//'end module high'//NL, stat, errmsg)
    call check(error, stat, 0, 'the source')
    if (allocated(error)) return

    args(1)%s = '-q'
    args(2)%s = '--config'
    args(3)%s = "graph_dir='"//ROOT//"/kept'"
    args(4)%s = ROOT//'/ford.md'
    call check(error, run(args), 0, 'the run')
    if (allocated(error)) return

    call read_whole_file(ROOT//'/doc/module/high.html', text, stat, errmsg)
    call check(error, stat, 0, 'the page of a module')
    if (allocated(error)) return
    call check(error, index(text, '<svg') > 0, 'it has a drawing')
    if (allocated(error)) return
    call check(error, index(text, 'href="../module/low.html"') > 0, 'nodes link to their pages')
    if (allocated(error)) return
    call read_whole_file(ROOT//'/doc/type/outer.html', text, stat, errmsg)
    call check(error, stat, 0, 'the page of a type')
    if (allocated(error)) return
    call check(error, index(text, '>part</text>') > 0, 'a component labels its edge')
    if (allocated(error)) return
    call check(error, index(text, 'stroke-dasharray') > 0, 'and it is dashed')
    if (allocated(error)) return
    call read_whole_file(ROOT//'/doc/proc/ping.html', text, stat, errmsg)
    call check(error, stat, 0, 'the page of a procedure')
    if (allocated(error)) return
    call check(error, index(text, '>pong</text>') > 0, 'a cycle is drawn')
    call remove_directory(ROOT)
end subroutine test_drawn

!> Checks: a graph stops at `graph_maxdepth`, and a node that leads further has an edge to a node `...`.
subroutine test_cut(error)
    type(error_type), allocatable, intent(out) :: error     !! the failure, if any
    character(len=*), parameter :: HERE = 'build/test graphs cut'
    type(string_t) :: args(2)
    character(len=:), allocatable :: text, errmsg
    integer :: stat

    call remove_directory(HERE)
    call make_directory(HERE//'/src', stat)
    call write_whole_file(HERE//'/ford.md', 'project: cut'//NL//'src_dir: ./src'//NL//'output_dir: ./doc'//NL// &
                          'graph: true'//NL//'graph_maxdepth: 2'//NL//NL//'The project.'//NL, stat, errmsg)
    call check(error, stat, 0, 'the project file')
    if (allocated(error)) return
    call write_whole_file(HERE//'/src/a.f90', &
                          'module chain'//NL//'contains'//NL// &
                          'subroutine a()'//NL//'call b()'//NL//'end subroutine a'//NL// &
                          'subroutine b()'//NL//'call c()'//NL//'end subroutine b'//NL// &
                          'subroutine c()'//NL//'call d()'//NL//'end subroutine c'//NL// &
                          'subroutine d()'//NL//'call e()'//NL//'end subroutine d'//NL// &
                          'subroutine e()'//NL//'end subroutine e'//NL//'end module chain'//NL, stat, errmsg)
    call check(error, stat, 0, 'the source')
    if (allocated(error)) return
    args(1)%s = '-q'
    args(2)%s = HERE//'/ford.md'
    call check(error, run(args), 0, 'the run')
    if (allocated(error)) return
    call read_whole_file(HERE//'/doc/proc/a.html', text, stat, errmsg)
    call check(error, stat, 0, 'the page of the first procedure')
    if (allocated(error)) return
    text = text(index(text, '<svg'):index(text, '</svg>'))
    call check(error, index(text, '>c</text>') > 0, 'two levels are drawn')
    if (allocated(error)) return
    call check(error, index(text, '>d</text>') == 0, 'not three')
    if (allocated(error)) return
    call check(error, index(text, '>...</text>') > 0, 'what is left out is marked')
    call remove_directory(HERE)
end subroutine test_cut

end module test_graphs
