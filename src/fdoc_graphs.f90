module fdoc_graphs
!! Dependency graphs: which modules use which, what procedures call, how types extend each other and
!! how files depend on each other. Graphs are laid out and drawn by fdot, in parallel, each by one
!! thread.
!!
!! ```mermaid
!! graph LR
!!   A[Relations] --> B[List of graphs]
!!   B --> C
!!   subgraph each thread
!!     C[Find what a graph holds] --> D[Layout with fdot]
!!     D --> E[Drawing]
!!   end
!!   E --> F[Graphs of the pages]
!! ```
!!
!! The relations and the list are made once, by one thread, and only read after that. Each thread has
!! working space of its own, so a graph is built as it is drawn, and needs no files.
use fdoc_util
use fdoc_buffer
use fdoc_model
use fdoc_settings, only: settings_t
use fdoc_project, only: project_t
use fdoc_urls, only: entity_url, is_shown
use fdoc_progress, only: progress_start, progress_tick, progress_finish, warn
use fdoc_shell, only: make_directory
use fdot, only: layout_t => graph_t, RANK_LR, RANK_RL, FDOT_OK
implicit none
private

public :: graph_set_t, build_graphs

integer, parameter, public :: REL_USES = 1       !! modules used
integer, parameter, public :: REL_CALLS = 2      !! procedures called
integer, parameter, public :: REL_INHERITS = 3   !! types extended
integer, parameter, public :: REL_FILES = 4      !! files depended on
integer, parameter :: NUM_RELATIONS = 4

character(len=*), parameter :: EDGE_COLOURS(*) = [character(len=7) :: '#3657c9', '#1d7a55', '#a15c00', '#b3261e', &
                                                  '#6a4fb3', '#0b7285', '#a61e69', '#5c6f2d']

type :: int_list_t
    !! A list of entities.
    integer, allocatable :: ids(:)              !! the entities
end type int_list_t

type :: label_list_t
    !! Labels of edges.
    type(string_t), allocatable :: text(:)      !! the label of each edge, `''` for a solid edge
end type label_list_t

type :: adjacency_t
    !! Each direction of one relation, for each entity.
    type(int_list_t), allocatable :: out(:)     !! what each entity points to
    type(int_list_t), allocatable :: in(:)      !! what points to each entity
    type(label_list_t), allocatable :: labels(:)    !! the labels of `out`
end type adjacency_t

type :: graph_t
    !! A graph of a page.
    character(len=:), allocatable :: title      !! its heading
    character(len=:), allocatable :: svg        !! its drawing
    integer :: owner = 0                        !! the entity whose page shows it, or 0
    integer :: list_page = 0                    !! the list page that shows it, or 0
    integer, allocatable :: neighbours(:)       !! what is next to the entity of a page in its graph
end type graph_t

type :: graph_set_t
    !! The graphs of a project.
    type(graph_t), allocatable :: graphs(:)     !! the graphs
end type graph_set_t

type :: graph_job_t
    !! A graph waiting to be drawn.
    integer, allocatable :: roots(:)            !! where to start
    character(len=:), allocatable :: title      !! its heading
    integer :: relation = 0                     !! the relation
    logical :: forward = .true.                 !! true to follow edges forward
    integer :: max_depth = 0                    !! how many hops to follow
    integer :: max_nodes = 0                    !! how many nodes to allow
    integer :: direction = RANK_LR              !! which way ranks go
    integer :: owner = 0                        !! the entity whose page shows it, or 0
    integer :: list_page = 0                    !! the list page that shows it, or 0
end type graph_job_t

type :: walk_t
    !! The working space of one thread, sized once, for finding what a graph holds.
    integer, allocatable :: marked(:)           !! when each entity was last reached
    integer, allocatable :: places(:)           !! the number of each entity in the graph
    integer, allocatable :: queue(:)            !! the entities reached
    integer, allocatable :: levels(:)           !! the hops to each of `queue`
    logical, allocatable :: more(:)             !! whether each of `queue` leads to more than is drawn
    integer, allocatable :: from(:)             !! where each edge starts
    integer, allocatable :: to(:)               !! where each edge ends
    integer :: stamp = 0                        !! a number new for each graph
end type walk_t

contains

!> Build the relations between entities.
subroutine build_relations(p, relations)
    type(project_t), intent(in) :: p                        !! the project
    type(adjacency_t), intent(out) :: relations(NUM_RELATIONS)  !! the relations
    integer, allocatable :: seen(:)
    logical, allocatable :: done(:)
    logical :: changed
    integer :: r, k, j, target, seen_stamp

    allocate (seen(p%list%n))
    seen = 0
    seen_stamp = 0
    do r = 1, NUM_RELATIONS
        allocate (relations(r)%out(p%list%n), relations(r)%in(p%list%n), relations(r)%labels(p%list%n))
    end do
    do k = 1, p%list%n
        associate (e => p%list%box(k)%e)
            if (e%kind == E_NONE) cycle
            if (allocated(e%more%uses)) then
                do j = 1, size(e%more%uses)
                    if (e%more%uses(j)%target > 0) call connect(relations(REL_USES), k, e%more%uses(j)%target)
                end do
            end if
            if (e%kind == E_SUBMODULE) then
                if (e%parent_submodule_id > 0) then
                    call connect(relations(REL_USES), k, e%parent_submodule_id)
                else if (e%ancestor_id > 0) then
                    call connect(relations(REL_USES), k, e%ancestor_id)
                end if
            end if
            if (e%visible .and. e%kind /= E_BOUNDPROC) then
                seen_stamp = seen_stamp + 1
                call visible_calls(p, k, k, seen, seen_stamp, relations(REL_CALLS))
            end if
            if (allocated(e%more%modproc_ids)) then
                do j = 1, size(e%more%modproc_ids)
                    if (e%more%modproc_ids(j) == 0) cycle
                    if (p%list%box(e%more%modproc_ids(j))%e%visible) call connect(relations(REL_CALLS), k, e%more%modproc_ids(j))
                end do
            end if
            if (e%kind == E_INTERFACE) then
                do j = 1, size(e%children)
                    if (p%list%box(e%children(j))%e%visible) call connect(relations(REL_CALLS), k, e%children(j))
                end do
            end if
            if (e%extends_id > 0) call connect(relations(REL_INHERITS), k, e%extends_id)
            if (e%kind == E_TYPE .and. e%visible) then
                do j = 1, size(e%children)
                    associate (c => p%list%box(e%children(j))%e)
                        if (c%kind /= E_VARIABLE .or. c%proto_id == 0) cycle
                        if (p%list%box(c%proto_id)%e%kind == E_TYPE) then
                            call connect(relations(REL_INHERITS), k, c%proto_id, c%name)
                        end if
                    end associate
                end do
            end if
            if (e%file > 0 .and. allocated(e%more%uses)) then
                do j = 1, size(e%more%uses)
                    target = e%more%uses(j)%target
                    if (target == 0) cycle
                    target = p%list%box(target)%e%file
                    if (target > 0 .and. target /= e%file) call connect(relations(REL_FILES), e%file, target)
                end do
            end if
        end associate
    end do

    ! a generic binding is a node only once something refers to it
    allocate (done(p%list%n))
    done = .false.
    changed = .true.
    do while (changed)
        changed = .false.
        do k = 1, p%list%n
            if (done(k) .or. p%list%box(k)%e%kind /= E_BOUNDPROC) cycle
            if (.not. allocated(relations(REL_CALLS)%in(k)%ids)) cycle
            done(k) = .true.
            changed = .true.
            seen_stamp = seen_stamp + 1
            call visible_calls(p, k, k, seen, seen_stamp, relations(REL_CALLS))
        end do
    end do
end subroutine build_relations

!> True if a graph shows binding `id` as the procedure it binds: it binds one, which can be shown.
logical function is_simple_binding(p, id)
    type(project_t), intent(in) :: p    !! the project
    integer, intent(in) :: id           !! the entity

    is_simple_binding = .false.
    associate (e => p%list%box(id)%e)
        if (e%kind /= E_BOUNDPROC .or. .not. allocated(e%more%binding_ids)) return
        if (size(e%more%binding_ids) /= 1) return
        if (e%deferred) then
            is_simple_binding = .true.
        else if (e%more%binding_ids(1) > 0) then
            is_simple_binding = p%list%box(e%more%binding_ids(1))%e%kind /= E_BOUNDPROC .and. &
                                p%list%box(e%more%binding_ids(1))%e%visible
        end if
    end associate
end function is_simple_binding

!> Add edges from `from` to what `id` calls. What cannot be shown is replaced by what it calls.
recursive subroutine visible_calls(p, id, from, seen, stamp, relation)
    type(project_t), intent(in) :: p                !! the project
    integer, intent(in) :: id                       !! the entity whose calls to follow
    integer, intent(in) :: from                     !! where the edges start
    integer, intent(inout) :: seen(:)               !! when each entity was last visited
    integer, intent(in) :: stamp                    !! a number new for each `from`
    type(adjacency_t), intent(inout) :: relation    !! the relation
    integer :: j

    associate (e => p%list%box(id)%e)
        if (allocated(e%call_ids)) then
            do j = 1, size(e%call_ids)
                call visit(e%call_ids(j))
            end do
        end if
        if (allocated(e%more%binding_ids)) then
            do j = 1, size(e%more%binding_ids)
                if (e%more%binding_ids(j) > 0) call visit(e%more%binding_ids(j))
            end do
        end if
    end associate

contains

    !> Handle a call to `target`.
    recursive subroutine visit(target)
        integer, intent(in) :: target   !! what is called

        if (seen(target) == stamp) return
        seen(target) = stamp
        if (p%list%box(target)%e%visible .and. .not. is_simple_binding(p, target)) then
            call connect(relation, from, target)
        else
            call visible_calls(p, target, from, seen, stamp, relation)
        end if
    end subroutine visit

end subroutine visible_calls

!> Add an edge, once. With `label`, the edge is dashed and labelled: components of a type have one.
subroutine connect(relation, from, to, label)
    type(adjacency_t), intent(inout) :: relation    !! the relation
    integer, intent(in) :: from                     !! where it starts
    integer, intent(in) :: to                       !! where it ends
    character(len=*), intent(in), optional :: label !! its label
    integer :: k

    if (from == to .and. .not. present(label)) return
    if (allocated(relation%out(from)%ids)) then
        do k = 1, size(relation%out(from)%ids)
            if (relation%out(from)%ids(k) /= to) cycle
            if (present(label)) then
                if (len(relation%labels(from)%text(k)%s) > 0) then
                    relation%labels(from)%text(k)%s = relation%labels(from)%text(k)%s//', '//label
                end if
            end if
            return
        end do
    end if
    call push_int(relation%out(from)%ids, to)
    call push_int(relation%in(to)%ids, from)
    if (present(label)) then
        call push(relation%labels(from)%text, label)
    else
        call push(relation%labels(from)%text, '')
    end if
end subroutine connect

!> True if entity `id` is drawn with graphs.
logical function graphs_wanted(p, s, id, depth, nodes)
    type(project_t), intent(in) :: p    !! the project
    type(settings_t), intent(in) :: s   !! the settings
    integer, intent(in) :: id           !! the entity
    integer, intent(out) :: depth       !! its `graph_maxdepth`
    integer, intent(out) :: nodes       !! its `graph_maxnodes`
    logical :: flag, ok
    integer :: k, value

    graphs_wanted = s%graph
    depth = s%graph_maxdepth
    nodes = s%graph_maxnodes
    associate (e => p%list%box(id)%e)
        if (.not. allocated(e%meta)) return
        do k = 1, size(e%meta)
            select case (e%meta(k)%key)
            case ('graph')
                call parse_bool(e%meta(k)%values(1)%s, flag, ok)
                if (ok) graphs_wanted = flag
            case ('graph_maxdepth')
                call parse_int(e%meta(k)%values(1)%s, value, ok)
                if (ok) depth = value
            case ('graph_maxnodes')
                call parse_int(e%meta(k)%values(1)%s, value, ok)
                if (ok) nodes = value
            end select
        end do
    end associate
end function graphs_wanted

!> Put the graph of `job` in `g`. `found` is false if it would have only one node or no edges. Nodes go
!> in as they are reached, and edges after them. A node that leads to more than `graph_maxdepth` and
!> `graph_maxnodes` let in has an edge to a node `...`.
subroutine describe_graph(p, s, relation, job, w, g, found)
    type(project_t), intent(in) :: p        !! the project
    type(settings_t), intent(in) :: s       !! the settings
    type(adjacency_t), intent(in) :: relation   !! the relation
    type(graph_job_t), intent(in) :: job    !! the graph
    type(walk_t), intent(inout) :: w        !! the working space
    type(layout_t), intent(out) :: g        !! the description
    logical, intent(out) :: found           !! whether there is a graph
    integer, allocatable :: next(:)
    integer :: head, k, id, other, j, reached, edges, dots

    w%stamp = w%stamp + 1
    reached = size(job%roots)
    do k = 1, reached
        w%marked(job%roots(k)) = w%stamp
        w%queue(k) = job%roots(k)
        w%levels(k) = 0
    end do
    w%more(:reached) = .false.
    head = 1
    edges = 0
    do while (head <= reached)
        id = w%queue(head)
        if (w%levels(head) >= job%max_depth) then
            w%more(head) = leads_on(id)
        else
            if (job%forward) then
                if (allocated(relation%out(id)%ids)) then
                    next = relation%out(id)%ids
                else
                    allocate (next(0))
                end if
            else
                if (allocated(relation%in(id)%ids)) then
                    next = relation%in(id)%ids
                else
                    allocate (next(0))
                end if
            end if
            do j = 1, size(next)
                other = next(j)
                if (p%list%box(other)%e%kind == E_NONE) cycle
                if (w%marked(other) /= w%stamp) then
                    if (reached >= job%max_nodes) then
                        w%more(head) = .true.
                        cycle
                    end if
                    w%marked(other) = w%stamp
                    reached = reached + 1
                    w%queue(reached) = other
                    w%levels(reached) = w%levels(head) + 1
                    w%more(reached) = .false.
                end if
                edges = edges + 1
                if (edges > size(w%from)) call grow_edges(w)
                w%from(edges) = merge(id, other, job%forward)
                w%to(edges) = merge(other, id, job%forward)
            end do
            deallocate (next)
        end if
        head = head + 1
    end do
    found = reached > 1 .and. edges > 0
    if (.not. found) return
    call g%initialize(direction=job%direction, concentrate=.true.)
    do k = 1, reached
        call add_node(p, w%queue(k), k == 1 .and. size(job%roots) == 1, g, w%places(w%queue(k)))
    end do
    do k = 1, edges
        call add_edge(s, relation, w%from(k), w%to(k), w%places, g)
    end do
    do k = 1, reached
        if (.not. w%more(k)) cycle
        call g%add_node('...', dots, fill='#8a93a5')
        if (job%forward) then
            call g%add_edge(w%places(w%queue(k)), dots, dashed=.true.)
        else
            call g%add_edge(dots, w%places(w%queue(k)), dashed=.true.)
        end if
    end do
contains
    !> True if entity `id` has neighbours in the direction of the graph that are not in it.
    logical function leads_on(id)
        integer, intent(in) :: id   !! the entity
        integer :: m

        leads_on = .false.
        if (job%forward) then
            if (.not. allocated(relation%out(id)%ids)) return
            do m = 1, size(relation%out(id)%ids)
                if (p%list%box(relation%out(id)%ids(m))%e%kind == E_NONE) cycle
                if (w%marked(relation%out(id)%ids(m)) /= w%stamp) leads_on = .true.
            end do
        else
            if (.not. allocated(relation%in(id)%ids)) return
            do m = 1, size(relation%in(id)%ids)
                if (p%list%box(relation%in(id)%ids(m))%e%kind == E_NONE) cycle
                if (w%marked(relation%in(id)%ids(m)) /= w%stamp) leads_on = .true.
            end do
        end if
    end function leads_on
end subroutine describe_graph

!> Make room for more edges.
pure subroutine grow_edges(w)
    type(walk_t), intent(inout) :: w    !! the working space
    integer, allocatable :: grown(:)
    integer :: n

    n = size(w%from)
    allocate (grown(2*n))
    grown(1:n) = w%from
    call move_alloc(grown, w%from)
    allocate (grown(2*n))
    grown(1:n) = w%to
    call move_alloc(grown, w%to)
end subroutine grow_edges

!> Add node `id` to a description. The key to the graphs in `fdoc.js` lists these colours.
subroutine add_node(p, id, root, g, place)
    type(project_t), intent(in) :: p    !! the project
    integer, intent(in) :: id           !! the entity
    logical, intent(in) :: root         !! true for the entity the graph is about
    type(layout_t), intent(inout) :: g  !! the description
    integer, intent(out) :: place       !! its number in `g`
    character(len=:), allocatable :: url, colour, name, outline, owner_url, owner_colour
    integer :: at, owner

    associate (e => p%list%box(id)%e)
        select case (e%kind)
        case (E_MODULE); colour = '#3657c9'
        case (E_SUBMODULE); colour = '#5a78de'
        case (E_PROGRAM); colour = '#a61e69'
        case (E_SUBROUTINE); colour = '#1d7a55'
        case (E_FUNCTION); colour = '#0b7285'
        case (E_INTERFACE, E_MODPROC); colour = '#6a4fb3'
        case (E_TYPE); colour = '#a15c00'
        case (E_BOUNDPROC); colour = '#7a6a2d'
        case (E_FILE); colour = '#5c6f2d'
        case default; colour = '#646d7e'
        end select
        name = e%name
        if (e%kind == E_FILE) then
            at = last_separator(name)
            name = name(at + 1:)
        end if
        ! what belongs to a type is drawn joined to it, with a link to each
        owner = 0
        if (e%kind == E_BOUNDPROC) owner = e%parent
        if (e%bound_to > 0) owner = e%bound_to
        url = ''
        ! something shown inside something hidden, such as a binding of a private type, has no page either
        if (is_shown(p%list, id) .or. allocated(e%external_url)) call entity_url(p%list, id, url)
        if (.not. is_shown(p%list, id) .and. .not. allocated(e%external_url)) colour = '#8a93a5'
        if (allocated(e%external_url)) colour = '#646d7e'
        if (len(url) > 0 .and. .not. allocated(e%external_url)) url = '../'//url
        outline = ''
        if (root) outline = '#f0a868'
        if (owner > 0) then
            call node_link(p, owner, owner_url)
            owner_colour = merge('#a15c00', '#8a93a5', len(owner_url) > 0)
            call g%add_node(name, place, fill=colour, link=url, outline=outline, owner=p%list%box(owner)%e%name, &
                            owner_fill=owner_colour, owner_link=owner_url)
        else
            call g%add_node(name, place, fill=colour, link=url, outline=outline)
        end if
    end associate
end subroutine add_node

!> The link of entity `id` from a page, or `''` if it has no page.
subroutine node_link(p, id, url)
    type(project_t), intent(in) :: p                    !! the project
    integer, intent(in) :: id                           !! the entity
    character(len=:), allocatable, intent(out) :: url   !! the link

    url = ''
    associate (e => p%list%box(id)%e)
        if (is_shown(p%list, id) .or. allocated(e%external_url)) call entity_url(p%list, id, url)
        if (len(url) > 0 .and. .not. allocated(e%external_url)) url = '../'//url
    end associate
end subroutine node_link

!> Add an edge to a description.
subroutine add_edge(s, relation, from, to, places, g)
    type(settings_t), intent(in) :: s           !! the settings
    type(adjacency_t), intent(in) :: relation   !! the relation
    integer, intent(in) :: from                 !! where it starts
    integer, intent(in) :: to                   !! where it ends
    integer, intent(in) :: places(:)            !! the number in `g` of each entity
    type(layout_t), intent(inout) :: g          !! the description
    character(len=:), allocatable :: label
    integer :: k

    label = ''
    do k = 1, size(relation%out(from)%ids)
        if (relation%out(from)%ids(k) /= to) cycle
        label = relation%labels(from)%text(k)%s
        exit
    end do
    if (s%coloured_edges) then
        call g%add_edge(places(from), places(to), colour=EDGE_COLOURS(mod(from, size(EDGE_COLOURS)) + 1), label=label)
    else
        call g%add_edge(places(from), places(to), label=label)
    end if
end subroutine add_edge

!> Build the graphs of the project: those of `s%graph`, and of entities that ask for them.
subroutine build_graphs(p, s, members, list_codes, set, stat, errmsg)
    type(project_t), intent(in) :: p                    !! the project
    type(settings_t), intent(in) :: s                   !! the settings
    integer, intent(in) :: members(:)                   !! the entities that have pages
    integer, intent(in) :: list_codes(4)                !! the list pages of files, modules, procedures, types
    type(graph_set_t), intent(out) :: set               !! the graphs
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg !! what is wrong
    type(adjacency_t) :: relations(NUM_RELATIONS)
    type(graph_job_t), allocatable :: jobs(:)
    type(string_t), allocatable :: drawings(:), descriptions(:)
    logical, allocatable :: failed(:)
    integer, allocatable :: roots(:)
    character(len=:), allocatable :: directory
    character(len=32) :: tag
    logical :: any_wanted, keep
    integer :: k, m, n, depth, nodes, status, count_jobs

    stat = 0
    allocate (set%graphs(0))
    any_wanted = s%graph
    do k = 1, p%list%n
        if (.not. allocated(p%list%box(k)%e%meta)) cycle
        if (any([(p%list%box(k)%e%meta(n)%key == 'graph', n=1, size(p%list%box(k)%e%meta))])) any_wanted = .true.
    end do
    if (.not. any_wanted) return

    call progress_start('Finding dependencies', size(members))
    call build_relations(p, relations)
    allocate (jobs(0))
    count_jobs = 0
    do k = 1, size(members)
        call progress_tick()
        associate (id => members(k))
            if (.not. graphs_wanted(p, s, id, depth, nodes)) cycle
            select case (p%list%box(id)%e%kind)
            case (E_MODULE, E_SUBMODULE)
                call one(id, REL_USES, .true., 'Uses', RANK_RL)
                call one(id, REL_USES, .false., 'Used by', RANK_RL)
            case (E_TYPE)
                call one(id, REL_INHERITS, .true., 'Inherits', RANK_RL)
                call one(id, REL_INHERITS, .false., 'Inherited by', RANK_RL)
            case (E_SUBROUTINE, E_FUNCTION, E_MODPROC, E_INTERFACE)
                call one(id, REL_CALLS, .true., 'Calls', RANK_LR)
                call one(id, REL_CALLS, .false., 'Called by', RANK_RL)
            case (E_PROGRAM)
                call one(id, REL_CALLS, .true., 'Calls', RANK_LR)
                call one(id, REL_USES, .true., 'Uses', RANK_RL)
            case (E_BLOCKDATA)
                call one(id, REL_USES, .true., 'Uses', RANK_RL)
            case (E_FILE)
                call one(id, REL_FILES, .true., 'Files this one depends on', RANK_RL)
                call one(id, REL_FILES, .false., 'Files that depend on this one', RANK_RL)
            end select
        end associate
    end do
    if (s%graph) then
        call project_graph([E_MODULE, E_SUBMODULE], REL_USES, list_codes(2), 'Module graph')
        call project_graph([E_TYPE], REL_INHERITS, list_codes(4), 'Type graph')
        call project_graph([E_SUBROUTINE, E_FUNCTION, E_PROGRAM, E_INTERFACE], REL_CALLS, list_codes(3), 'Call graph')
        call project_graph([E_FILE], REL_FILES, list_codes(1), 'File graph')
    end if
    call progress_finish()

    n = count_jobs
    if (n == 0) return
    keep = allocated(s%graph_dir)
    allocate (drawings(n), descriptions(n), failed(n))
    call draw_graphs(p, s, relations, jobs(1:n), keep, drawings, descriptions, failed, max(1, s%parallel))

    if (keep) then
        call unique_tag(tag)
        ! a directory of this run's own, as the names of the files inside are the same in every run
        directory = s%graph_dir//'/'//trim(tag)
        call make_directory(directory, status)
    end if
    deallocate (set%graphs)
    allocate (set%graphs(count([(allocated(drawings(k)%s), k=1, n)])))
    m = 0
    do k = 1, n
        if (failed(k)) call warn('no layout for a graph of '//jobs(k)%title)
        if (.not. allocated(drawings(k)%s)) cycle
        m = m + 1
        if (keep) then
            call write_whole_file(directory//'/g'//trim(int_text(m))//'.gv', descriptions(k)%s, stat, errmsg)
            if (stat /= 0) return
            call write_whole_file(directory//'/g'//trim(int_text(m))//'.gv.svg', drawings(k)%s, stat, errmsg)
            if (stat /= 0) return
        end if
        call move_alloc(drawings(k)%s, set%graphs(m)%svg)
        call move_alloc(jobs(k)%title, set%graphs(m)%title)
        set%graphs(m)%owner = jobs(k)%owner
        set%graphs(m)%list_page = jobs(k)%list_page
        if (jobs(k)%owner > 0) call neighbours_of(p, relations(jobs(k)%relation), jobs(k), set%graphs(m)%neighbours)
    end do

contains

    !> Add a graph of the page of `id`.
    subroutine one(id, relation, forward, title, direction)
        integer, intent(in) :: id                   !! the entity
        integer, intent(in) :: relation             !! the relation
        logical, intent(in) :: forward              !! true to follow edges forward
        character(len=*), intent(in) :: title       !! the heading
        integer, intent(in) :: direction            !! which way ranks go

        call add_job([id], relation, forward, depth, nodes, direction, title, id, 0)
    end subroutine one

    !> Add the graph of a whole relation to a list page.
    subroutine project_graph(kinds, relation, page, title)
        integer, intent(in) :: kinds(:)         !! the kinds of entity
        integer, intent(in) :: relation         !! the relation
        integer, intent(in) :: page             !! the list page
        character(len=*), intent(in) :: title   !! the heading
        integer :: j

        allocate (roots(0))
        do j = 1, size(members)
            if (p%list%box(members(j))%e%iface == IFACE_ABSTRACT) cycle
            if (any(p%list%box(members(j))%e%kind == kinds)) call push_int(roots, members(j))
        end do
        if (size(roots) > s%graph_project_maxnodes) then
            call warn(title//' left out: it would have more than '//trim(int_text(s%graph_project_maxnodes))// &
                      ' nodes (see graph_project_maxnodes)')
        else if (size(roots) > 1) then
            call add_job(roots, relation, .true., 1, huge(1), RANK_RL, title, 0, page)
        end if
        deallocate (roots)
    end subroutine project_graph

    !> Add a graph to the list.
    subroutine add_job(start, relation, forward, max_depth, max_nodes, direction, title, owner, page)
        integer, intent(in) :: start(:)         !! where to start
        integer, intent(in) :: relation         !! the relation
        logical, intent(in) :: forward          !! true to follow edges forward
        integer, intent(in) :: max_depth        !! how many hops to follow
        integer, intent(in) :: max_nodes        !! how many nodes to allow
        integer, intent(in) :: direction        !! which way ranks go
        character(len=*), intent(in) :: title   !! its heading
        integer, intent(in) :: owner            !! the entity whose page shows it, or 0
        integer, intent(in) :: page             !! the list page that shows it, or 0
        type(graph_job_t), allocatable :: grown(:)
        integer :: j

        if (count_jobs == size(jobs)) then
            allocate (grown(max(64, 2*count_jobs)))
            do j = 1, count_jobs
                call move_alloc(jobs(j)%roots, grown(j)%roots)
                call move_alloc(jobs(j)%title, grown(j)%title)
                grown(j)%relation = jobs(j)%relation
                grown(j)%forward = jobs(j)%forward
                grown(j)%max_depth = jobs(j)%max_depth
                grown(j)%max_nodes = jobs(j)%max_nodes
                grown(j)%direction = jobs(j)%direction
                grown(j)%owner = jobs(j)%owner
                grown(j)%list_page = jobs(j)%list_page
            end do
            call move_alloc(grown, jobs)
        end if
        count_jobs = count_jobs + 1
        associate (job => jobs(count_jobs))
            job%roots = start
            job%title = title
            job%relation = relation
            job%forward = forward
            job%max_depth = max_depth
            job%max_nodes = max_nodes
            job%direction = direction
            job%owner = owner
            job%list_page = page
        end associate
    end subroutine add_job

end subroutine build_graphs

!> What is next to the one root of `job` in its graph: the first entities that [[describe_graph]] reaches,
!> in the same order and within the same limit, so that a list of them matches the drawing.
subroutine neighbours_of(p, relation, job, ids)
    type(project_t), intent(in) :: p                !! the project
    type(adjacency_t), intent(in) :: relation       !! the relation
    type(graph_job_t), intent(in) :: job            !! the graph
    integer, allocatable, intent(out) :: ids(:)     !! the entities next to the root
    integer :: j, other, root

    allocate (ids(0))
    root = job%roots(1)
    if (job%max_depth < 1) return
    if (job%forward) then
        if (.not. allocated(relation%out(root)%ids)) return
        do j = 1, size(relation%out(root)%ids)
            call consider(relation%out(root)%ids(j))
        end do
    else
        if (.not. allocated(relation%in(root)%ids)) return
        do j = 1, size(relation%in(root)%ids)
            call consider(relation%in(root)%ids(j))
        end do
    end if

contains

    !> Add `other` if the graph has room for it.
    subroutine consider(item)
        integer, intent(in) :: item     !! the entity

        other = item
        if (other == root .or. p%list%box(other)%e%kind == E_NONE) return
        if (any(ids == other)) return
        if (size(ids) + 1 >= job%max_nodes) return
        call push_int(ids, other)
    end subroutine consider

end subroutine neighbours_of

!> Lay out and draw `jobs`. A graph that turns out to have nothing to show gets no drawing.
subroutine draw_graphs(p, s, relations, jobs, keep, drawings, descriptions, failed, threads)
    type(project_t), intent(in) :: p                    !! the project
    type(settings_t), intent(in) :: s                   !! the settings
    type(adjacency_t), intent(in) :: relations(NUM_RELATIONS)   !! the relations
    type(graph_job_t), intent(in) :: jobs(:)            !! the graphs
    logical, intent(in) :: keep                         !! whether to give the descriptions too
    type(string_t), intent(inout) :: drawings(:)        !! the drawing of each graph
    type(string_t), intent(inout) :: descriptions(:)    !! the description of each graph, with `keep`
    logical, intent(out) :: failed(:)                   !! whether the layout of each graph failed
    integer, intent(in) :: threads                      !! how many threads to use

    failed = .false.
    call progress_start('Laying out graphs', size(jobs))
    !$omp parallel num_threads(threads) default(shared)
    call draw_some(p, s, relations, jobs, keep, drawings, descriptions, failed)
    !$omp end parallel
    call progress_finish()
end subroutine draw_graphs

!> The share of [[draw_graphs]] of one thread, with working space of its own.
subroutine draw_some(p, s, relations, jobs, keep, drawings, descriptions, failed)
    type(project_t), intent(in) :: p                    !! the project
    type(settings_t), intent(in) :: s                   !! the settings
    type(adjacency_t), intent(in) :: relations(NUM_RELATIONS)   !! the relations
    type(graph_job_t), intent(in) :: jobs(:)            !! the graphs
    logical, intent(in) :: keep                         !! whether to give the descriptions too
    type(string_t), intent(inout) :: drawings(:)        !! the drawing of each graph
    type(string_t), intent(inout) :: descriptions(:)    !! the description of each graph, with `keep`
    logical, intent(inout) :: failed(:)                 !! whether the layout of each graph failed
    type(walk_t) :: w
    integer :: k

    allocate (w%marked(p%list%n), source=0)
    allocate (w%places(p%list%n), w%queue(p%list%n), w%levels(p%list%n), w%from(1024), w%to(1024))
    allocate (w%more(p%list%n))
    !$omp do schedule(dynamic)
    do k = 1, size(jobs)
        call draw_graph(p, s, relations(jobs(k)%relation), jobs(k), keep, w, drawings(k), descriptions(k), failed(k))
        call progress_tick()
    end do
    !$omp end do
end subroutine draw_some

!> Lay out and draw one graph.
subroutine draw_graph(p, s, relation, job, keep, w, drawing, description, failed)
    type(project_t), intent(in) :: p            !! the project
    type(settings_t), intent(in) :: s           !! the settings
    type(adjacency_t), intent(in) :: relation   !! the relation
    type(graph_job_t), intent(in) :: job        !! the graph
    logical, intent(in) :: keep                 !! whether to give the description too
    type(walk_t), intent(inout) :: w            !! the working space
    type(string_t), intent(inout) :: drawing    !! the drawing
    type(string_t), intent(inout) :: description    !! the description, with `keep`
    logical, intent(out) :: failed              !! whether the layout failed
    type(layout_t) :: g
    logical :: found
    integer :: status

    failed = .false.
    call describe_graph(p, s, relation, job, w, g, found)
    if (.not. found) return
    if (keep) call g%write_dot(description%s)
    call g%layout(status)
    failed = status /= FDOT_OK
    if (.not. failed) call g%write_svg(drawing%s)
end subroutine draw_graph

end module fdoc_graphs
