module fdoc_project
!! The whole project: loads and parses the files, merges them into one list of entities, and
!! connects the entities with each other.
!!
!! ```mermaid
!! graph TD
!!   A["load_project_sources: one list of entities"] --> B
!!   B["correlate: modules from elsewhere, use statements, public names, inheritance"] --> C
!!   C["link_project: implementations, calls, bindings, submodules"] --> D
!!   D["prune: hide what is not displayed, but keep what a shown binding binds to"]
!! ```
!!
!! Hiding comes last, so that names can still be found in what will not be shown.
use json_module, only: json_core, json_value
use fdoc_util
use fdoc_model
use fdoc_settings, only: settings_t
use ffmt_fs, only: parent_directory
use fdoc_input, only: find_sources, load_source, runs_command, expand_includes
use fdoc_progress, only: progress_start, progress_tick, progress_finish, warn
use fdoc_parser, only: parse_file
use fdoc_shell, only: make_directory, quoted, quoted_path, run_command
use fdoc_urls, only: is_shown
implicit none
private

public :: load_project_sources, correlate, prune, public_symbols, find_symbol

type, public :: symbol_t
    !! An entity under the name it is known by in some scope.
    character(len=:), allocatable :: name   !! the name, in lowercase
    integer :: id = 0                       !! the entity
end type symbol_t

type :: symbol_list_t
    !! Names, in the order they were added, with an index.
    type(symbol_t), allocatable :: s(:)         !! storage; only `s(1:n)` is in use
    integer :: n = 0                            !! how many there are
    type(name_index_t) :: index                 !! the position of each name
end type symbol_list_t

type :: symbols_t
    !! The names a module makes public, by category.
    type(symbol_list_t) :: procs                !! procedures and interfaces
    type(symbol_list_t) :: absints              !! abstract interfaces
    type(symbol_list_t) :: types                !! derived types
    type(symbol_list_t) :: vars                 !! variables
    type(name_index_t) :: public_list           !! the names of the module's `public` statements
    integer :: state = 0                        !! 0 not computed, 1 being computed, 2 computed
end type symbols_t

type, public :: project_t
    !! The parsed project.
    type(entity_list_t) :: list             !! all entities
    integer, allocatable :: files(:)        !! the file entities
    integer, allocatable :: modules(:)      !! the module entities
    type(symbols_t), allocatable :: pub(:)  !! the public names of each entity in `modules`
    integer, allocatable :: submodules(:)   !! the submodule entities
end type project_t

contains

!> The public names of module `which` in a category: 1 procedures, 2 abstract interfaces, 3 types,
!> 4 variables.
function public_symbols(p, which, category) result(symbols)
    type(project_t), intent(in) :: p    !! the project
    integer, intent(in) :: which        !! position in `p%modules`
    integer, intent(in) :: category     !! the category
    type(symbol_t), allocatable :: symbols(:)

    select case (category)
    case (1); symbols = p%pub(which)%procs%s(:p%pub(which)%procs%n)
    case (2); symbols = p%pub(which)%absints%s(:p%pub(which)%absints%n)
    case (3); symbols = p%pub(which)%types%s(:p%pub(which)%types%n)
    case default; symbols = p%pub(which)%vars%s(:p%pub(which)%vars%n)
    end select
end function public_symbols

!> Find, load and parse the source files, and merge the results into `p`.
subroutine load_project_sources(s, p, stat, errmsg)
    type(settings_t), intent(in) :: s                   !! the settings
    type(project_t), intent(out) :: p                   !! the project
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg  !! what is wrong
    type(string_t), allocatable :: paths(:), texts(:), raws(:)
    type(entity_list_t), allocatable :: lists(:)
    integer :: k

    stat = 0
    call find_sources(s, paths)
    if (size(paths) == 0) then
        stat = 1
        errmsg = 'No source files with appropriate extension found in specified directory.'
        return
    end if
    allocate (texts(size(paths)), lists(size(paths)), raws(size(paths)))
    ! files that go through an external command share its temporary files, so they take turns
    call progress_start('Reading files', size(paths))
    !$omp parallel do schedule(dynamic) num_threads(max(1, s%parallel)) default(shared) private(k)
    do k = 1, size(paths)
        if (.not. runs_command(s, paths(k)%s)) call read_source(s, paths(k)%s, raws(k)%s, texts(k)%s)
        call progress_tick()
    end do
    !$omp end parallel do
    do k = 1, size(paths)
        if (runs_command(s, paths(k)%s)) call read_source(s, paths(k)%s, raws(k)%s, texts(k)%s)
        call expand_includes(s%include, parent_directory(paths(k)%s), texts(k)%s, 1)
    end do
    call progress_finish()

    call progress_start('Parsing files', size(paths))
    !$omp parallel do schedule(dynamic) num_threads(max(1, s%parallel)) default(shared)
    do k = 1, size(paths)
        call parse_file(paths(k)%s, texts(k)%s, s, lists(k))
        call progress_tick()
    end do
    !$omp end parallel do
    call progress_finish()

    do k = 1, size(lists)
        call move_alloc(raws(k)%s, lists(k)%box(1)%e%source_text)
        call merge_list(p, lists(k))
    end do
end subroutine load_project_sources

!> Read source file `path`, warning if that fails.
subroutine read_source(s, path, raw, text)
    type(settings_t), intent(in) :: s                   !! the settings
    character(len=*), intent(in) :: path                !! the file
    character(len=:), allocatable, intent(out) :: raw   !! the file as it is
    character(len=:), allocatable, intent(out) :: text  !! the text to parse
    character(len=:), allocatable :: msg
    integer :: ios

    call load_source(s, path, raw, text, ios, msg)
    if (ios /= 0) then
        call warn('could not read '//path//': '//msg)
        text = ''
        raw = ''
    end if
end subroutine read_source

!> Move the entities of `from` to the end of `p%list`, adjusting their indices.
subroutine merge_list(p, from)
    type(project_t), intent(inout) :: p         !! the project
    type(entity_list_t), intent(inout) :: from  !! the entities of a file
    type(entity_box_t), allocatable :: grown(:)
    integer :: offset, k, needed

    offset = p%list%n
    needed = offset + from%n
    if (.not. allocated(p%list%box)) allocate (p%list%box(max(needed, 64)))
    if (needed > size(p%list%box)) then
        allocate (grown(2*needed))
        do k = 1, offset
            call move_alloc(p%list%box(k)%e, grown(k)%e)
        end do
        call move_alloc(grown, p%list%box)
    end if
    do k = 1, from%n
        call move_alloc(from%box(k)%e, p%list%box(offset + k)%e)
        associate (e => p%list%box(offset + k)%e)
            if (e%parent > 0) e%parent = e%parent + offset
            e%file = e%file + offset
            e%children = e%children + offset
            if (e%result > 0) e%result = e%result + offset
            if (allocated(e%more%arg_ids)) e%more%arg_ids = e%more%arg_ids + offset
            if (e%kind == E_MODULE) call push_int(p%modules, offset + k)
            if (e%kind == E_SUBMODULE) call push_int(p%submodules, offset + k)
            if (e%kind == E_FILE) call push_int(p%files, offset + k)
        end associate
    end do
    p%list%n = needed
end subroutine merge_list

!> Connect entities across files: the public names of each module, and what types inherit.
subroutine correlate(p, s)
    type(project_t), intent(inout) :: p     !! the project
    type(settings_t), intent(in) :: s       !! the settings
    integer :: k, n

    if (.not. allocated(p%modules)) allocate (p%modules(0))
    if (.not. allocated(p%submodules)) allocate (p%submodules(0))
    call add_external_modules(p, s)
    allocate (p%pub(size(p%modules)))
    call link_uses(p)
    do k = 1, size(p%modules)
        call public_names(p, k)
    end do
    n = p%list%n
    do k = 1, n
        if (p%list%box(k)%e%kind /= E_TYPE) cycle
        if (p%list%box(k)%e%parent == 0) cycle
        call set_constructor(p, k)
        call inherit(p, k)
    end do
end subroutine correlate

!> Add modules documented elsewhere: `extra_mods`, and the `modules.json` of external projects.
subroutine add_external_modules(p, s)
    type(project_t), intent(inout) :: p     !! the project
    type(settings_t), intent(in) :: s       !! the settings
    integer :: k, id

    do k = 1, size(s%extra_mods)
        id = p%list%add(E_MODULE, 0)
        associate (e => p%list%box(id)%e)
            e%name = s%extra_mods(k)%key
            e%external_url = s%extra_mods(k)%value
            e%permission = 'public'
            allocate (e%more%uses(0), e%more%public_list(0), e%attribs(0), e%doc(0), e%meta(0))
        end associate
        call push_int(p%modules, id)
    end do
    do k = 1, size(s%external)
        call load_external_project(p, s, s%external(k)%value)
    end do
end subroutine add_external_modules

!> Add the modules in the `modules.json` of an external project at `url`, a path or a web address.
subroutine load_external_project(p, s, url)
    type(project_t), intent(inout) :: p     !! the project
    type(settings_t), intent(in) :: s       !! the settings
    character(len=*), intent(in) :: url     !! where the project's documentation is
    type(json_core) :: json
    type(json_value), pointer :: root, modules, item
    character(len=:), allocatable :: path, base, fetched
    character(len=32) :: tag
    logical :: found, remote
    integer :: k, n, id, status

    remote = starts_with(url, 'http://') .or. starts_with(url, 'https://')
    if (remote) then
        base = url
        if (.not. ends_with(base, '/')) base = base//'/'
        call unique_tag(tag)
        fetched = s%output_dir//'/.external-modules-'//trim(tag)//'.json'
        call make_directory(s%output_dir, status)
        call run_command('curl -fsSL -o '//quoted_path(fetched)//' '//quoted(base//'modules.json'), status)
        path = fetched
        if (status /= 0) then
            call warn("Could not open external URL '"//url//"'")
            return
        end if
    else
        base = resolve_path(s%directory, url)//'/'
        path = base//'modules.json'
    end if

    call json%initialize()
    call json%load(path, root)
    if (remote) then
        call remove_file(fetched)
    end if
    if (json%failed()) then
        call json%clear_exceptions()
        call warn("Could not open external URL '"//url//"'")
        return
    end if
    call json%get_child(root, 'modules', modules, found)
    if (found) then
        call json%info(modules, n_children=n)
        do k = 1, n
            call json%get_child(modules, k, item, found)
            call read_external(p, json, item, base, 0, id)
            if (id > 0) call push_int(p%modules, id)
        end do
    end if
    call json%destroy(root)
    if (remote) then
        call remove_file(path)
    end if
end subroutine load_external_project

!> Add the entity described by `node` of an external `modules.json`, with those inside it.
recursive subroutine read_external(p, json, node, base, parent, id)
    type(project_t), intent(inout) :: p             !! the project
    type(json_core), intent(inout) :: json          !! the JSON handler
    type(json_value), pointer, intent(in) :: node   !! the entity
    character(len=*), intent(in) :: base            !! the external project's address, ending in `/`
    integer, intent(in) :: parent                   !! the entity it goes in, or 0
    integer, intent(out) :: id                      !! the new entity, or 0
    character(len=*), parameter :: LISTS(*) = [character(len=13) :: 'pub_procs', 'pub_absints', 'pub_types', 'pub_vars', &
                                               'boundprocs', 'variables']
    character(len=:), allocatable :: name, url, obj, proctype, text
    type(json_value), pointer :: list, item
    logical :: found
    integer :: k, n, j, child, kind

    id = 0
    call json%get(node, 'name', name, found)
    if (.not. found) return
    call json%get(node, 'obj', obj, found)
    call json%get(node, 'proctype', proctype, found)
    if (.not. found) proctype = ''
    select case (obj)
    case ('module'); kind = E_MODULE
    case ('type'); kind = E_TYPE
    case ('variable'); kind = E_VARIABLE
    case ('boundprocedure'); kind = E_BOUNDPROC
    case ('interface'); kind = E_INTERFACE
    case default; kind = merge(E_FUNCTION, E_SUBROUTINE, proctype == 'Function')
    end select
    id = p%list%add(kind, parent)
    call json%get(node, 'external_url', url, found)
    associate (e => p%list%box(id)%e)
        e%name = name
        if (found) then
            if (starts_with(url, './')) url = url(3:)
            e%external_url = base//url
        end if
        call json%get(node, 'permission', text, found)
        e%permission = 'public'
        if (found) e%permission = text
        call json%get(node, 'vartype', text, found)
        e%vartype = ''
        if (found) e%vartype = text
        allocate (e%attribs(0), e%doc(0), e%meta(0))
        ! variables share one read-only `no_more`
        if (kind /= E_VARIABLE) allocate (e%more%uses(0), e%more%public_list(0))
        e%var_role = VAR_PLAIN
    end associate
    do k = 1, size(LISTS)
        call json%get_child(node, trim(LISTS(k)), list, found)
        if (.not. found) cycle
        call json%info(list, n_children=n)
        do j = 1, n
            call json%get_child(list, j, item, found)
            call read_external(p, json, item, base, id, child)
        end do
    end do
end subroutine read_external

!> Find the modules that `use` statements and submodules refer to.
subroutine link_uses(p)
    type(project_t), intent(inout) :: p     !! the project
    integer :: k, u, j, found

    do k = 1, p%list%n
        associate (e => p%list%box(k)%e)
            if (allocated(e%more%uses)) then
                do u = 1, size(e%more%uses)
                    found = find_module(p, e%more%uses(u)%module)
                    if (found > 0) e%more%uses(u)%target = p%modules(found)
                    call read_use_spec(e%more%uses(u)%spec, e%more%uses(u)%only, e%more%uses(u)%renames)
                end do
            end if
            if (e%kind /= E_SUBMODULE) cycle
            found = find_module(p, e%more%ancestor)
            if (found > 0) e%ancestor_id = p%modules(found)
            if (allocated(e%more%parent_submodule)) then
                do j = 1, size(p%submodules)
                    if (lowercase(p%list%box(p%submodules(j))%e%name) == e%more%parent_submodule) then
                        e%parent_submodule_id = p%submodules(j)
                    end if
                end do
            end if
        end associate
    end do
end subroutine link_uses

!> The position of `name` in `symbols`, or 0.
pure integer function find_in(symbols, name)
    type(symbol_list_t), intent(in) :: symbols  !! the names
    character(len=*), intent(in) :: name        !! the name, in lowercase
    integer :: at

    find_in = 0
    at = index_find(symbols%index, name)
    if (at > 0) find_in = symbols%s(at)%id
end function find_in

!> True if entity `e` declares a name of category `category`: 1 procedures, 2 abstract interfaces,
!> 3 types, 4 variables.
pure logical function in_category(e, category)
    type(entity_t), intent(in) :: e     !! the entity
    integer, intent(in) :: category     !! the category

    select case (category)
    case (1)
        in_category = e%kind == E_INTERFACE .or. ((is_procedure(e%kind) .or. &
                                                   e%kind == E_MODPROC) .and. e%iface /= IFACE_ABSTRACT)
    case (2)
        in_category = is_procedure(e%kind) .and. e%iface == IFACE_ABSTRACT
    case (3)
        in_category = e%kind == E_TYPE
    case default
        in_category = e%kind == E_VARIABLE
    end select
end function in_category

!> What `name` means in scope `scope` for category `category` (see [[in_category]]), or 0.
recursive integer function find_symbol(p, scope, name, category) result(id)
    type(project_t), intent(in) :: p        !! the project
    integer, intent(in) :: scope            !! the entity to look in
    character(len=*), intent(in) :: name    !! the name, in lowercase
    integer, intent(in) :: category         !! the category
    integer :: c, b, child, u, j, used, target

    id = 0
    associate (e => p%list%box(scope)%e)
        do c = 1, size(e%children)
            child = e%children(c)
            associate (item => p%list%box(child)%e)
                if (.not. allocated(item%name)) cycle
                if (in_category(item, category)) then
                    if (lowercase(item%name) == name) then
                        id = child
                        return
                    end if
                end if
                if (category == 1 .and. item%kind == E_INTERFACE) then
                    do b = 1, size(item%children)
                        associate (body => p%list%box(item%children(b))%e)
                            if (.not. allocated(body%name)) cycle
                            if (body%kind /= E_VARIABLE .and. lowercase(body%name) == name) then
                                id = item%children(b)
                                return
                            end if
                        end associate
                    end do
                end if
                if (category == 1 .and. item%kind == E_VARIABLE) then
                    if (item%vartype == 'procedure' .and. lowercase(item%name) == name) then
                        id = child
                        return
                    end if
                end if
            end associate
        end do
        if (allocated(e%more%uses)) then
            do u = 1, size(e%more%uses)
                target = e%more%uses(u)%target
                if (target == 0) cycle
                used = module_position(p, target)
                if (used == 0) cycle
                if (e%more%uses(u)%only) then
                    do j = 1, size(e%more%uses(u)%renames)
                        if (e%more%uses(u)%renames(j)%local /= name) cycle
                        id = public_symbol(p, used, e%more%uses(u)%renames(j)%used, category)
                        if (id > 0) return
                    end do
                else
                    id = public_symbol(p, used, name, category)
                    if (id > 0) return
                end if
            end do
        end if
        if (e%kind == E_SUBMODULE) then
            if (e%parent_submodule_id > 0) then
                id = find_symbol(p, e%parent_submodule_id, name, category)
            else if (e%ancestor_id > 0) then
                id = find_symbol(p, e%ancestor_id, name, category)
            end if
            if (id > 0) return
        end if
        if (e%parent > 0) id = find_symbol(p, e%parent, name, category)
    end associate
end function find_symbol

!> The public entity of module `which` named `name` in category `category`, or 0.
pure integer function public_symbol(p, which, name, category) result(id)
    type(project_t), intent(in) :: p        !! the project
    integer, intent(in) :: which            !! position in `p%modules`
    character(len=*), intent(in) :: name    !! the name, in lowercase
    integer, intent(in) :: category         !! the category

    id = 0
    if (p%pub(which)%state == 0) return
    select case (category)
    case (1); id = find_in(p%pub(which)%procs, name)
    case (2); id = find_in(p%pub(which)%absints, name)
    case (3); id = find_in(p%pub(which)%types, name)
    case default; id = find_in(p%pub(which)%vars, name)
    end select
end function public_symbol

!> The position in `p%modules` of module entity `id`, or 0.
pure integer function module_position(p, id)
    type(project_t), intent(in) :: p    !! the project
    integer, intent(in) :: id           !! the module
    integer :: k

    module_position = 0
    do k = 1, size(p%modules)
        if (p%modules(k) == id) then
            module_position = k
            return
        end if
    end do
end function module_position

!> Work out what type `id` inherits, after its parent type.
recursive subroutine inherit(p, id)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: id               !! the type
    integer, allocatable :: parent_items(:)
    integer :: base, k, item, copy

    if (p%list%box(id)%e%inherit_state /= 0) return
    p%list%box(id)%e%inherit_state = 1
    allocate (p%list%box(id)%e%more%inherited(0))
    if (allocated(p%list%box(id)%e%more%extends)) then
        base = find_symbol(p, p%list%box(id)%e%parent, lowercase(p%list%box(id)%e%more%extends), 3)
        if (base > 0 .and. base /= id) then
            p%list%box(id)%e%extends_id = base
            call inherit(p, base)
            parent_items = [p%list%box(base)%e%more%inherited, p%list%box(base)%e%children]
            do k = 1, size(parent_items)
                item = parent_items(k)
                associate (e => p%list%box(item)%e)
                    select case (e%kind)
                    case (E_VARIABLE)
                        if (e%permission == 'public') call push_int(p%list%box(id)%e%more%inherited, item)
                    case (E_BOUNDPROC)
                        if (e%permission == 'private') cycle
                        if (overrides(p, id, e%name)) cycle
                        if (e%generic) then
                            copy = copy_binding(p, item, id)
                        else
                            call push_int(p%list%box(id)%e%more%inherited, item)
                        end if
                    end select
                end associate
            end do
        end if
    end if
    p%list%box(id)%e%inherit_state = 2
end subroutine inherit

!> True if type `id` declares a binding named `name` itself.
logical function overrides(p, id, name)
    type(project_t), intent(in) :: p        !! the project
    integer, intent(in) :: id               !! the type
    character(len=*), intent(in) :: name    !! the binding
    integer :: c, child

    overrides = .false.
    do c = 1, size(p%list%box(id)%e%children)
        child = p%list%box(id)%e%children(c)
        if (p%list%box(child)%e%kind /= E_BOUNDPROC) cycle
        if (lowercase(p%list%box(child)%e%name) == lowercase(name)) then
            overrides = .true.
            return
        end if
    end do
end function overrides

!> Add to type `id` a copy of generic binding `item`, returning the copy.
integer function copy_binding(p, item, id) result(copy)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: item             !! the binding
    integer, intent(in) :: id               !! the type
    integer :: file

    copy = p%list%add(E_BOUNDPROC, id)
    file = p%list%box(copy)%e%file
    p%list%box(copy)%e = p%list%box(item)%e
    if (p%list%box(item)%e%kind /= E_VARIABLE) allocate (p%list%box(copy)%e%more, source=p%list%box(item)%e%more)
    p%list%box(copy)%e%parent = id
    p%list%box(copy)%e%file = file
end function copy_binding

!> Give a procedure or interface named like type `id`, its constructor, the type's permission.
subroutine set_constructor(p, id)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: id               !! the type
    integer :: c, child, parent

    parent = p%list%box(id)%e%parent
    do c = 1, size(p%list%box(parent)%e%children)
        child = p%list%box(parent)%e%children(c)
        associate (e => p%list%box(child)%e)
            select case (e%kind)
            case (E_INTERFACE, E_SUBROUTINE, E_FUNCTION)
                if (lowercase(e%name) == lowercase(p%list%box(id)%e%name)) e%permission = p%list%box(id)%e%permission
            end select
        end associate
    end do
end subroutine set_constructor

!> Add `name` for entity `id` to `list`, replacing any entity it has.
subroutine add_symbol(list, name, id)
    type(symbol_list_t), intent(inout) :: list  !! the names
    character(len=*), intent(in) :: name        !! the name
    integer, intent(in) :: id                   !! the entity
    type(symbol_t), allocatable :: grown(:)
    integer :: k, at

    at = index_find(list%index, name)
    if (at > 0) then
        list%s(at)%id = id
        return
    end if
    if (list%n == size(list%s)) then
        allocate (grown(2*list%n))
        do k = 1, list%n
            call move_alloc(list%s(k)%name, grown(k)%name)
            grown(k)%id = list%s(k)%id
        end do
        call move_alloc(grown, list%s)
    end if
    list%n = list%n + 1
    list%s(list%n)%name = name
    list%s(list%n)%id = id
    call index_set(list%index, name, list%n)
end subroutine add_symbol

!> The position in `p%modules` of the module named `name`, or 0.
integer function find_module(p, name)
    type(project_t), intent(in) :: p        !! the project
    character(len=*), intent(in) :: name    !! the name, in lowercase
    integer :: k

    find_module = 0
    do k = 1, size(p%modules)
        if (lowercase(p%list%box(p%modules(k))%e%name) == name) then
            find_module = k
            return
        end if
    end do
end function find_module

!> Compute the public names of module `which`, after those of the modules it uses.
recursive subroutine public_names(p, which)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: which            !! position in `p%modules`
    integer :: id, c, child, u, used

    if (p%pub(which)%state /= 0) return
    p%pub(which)%state = 1
    id = p%modules(which)
    allocate (p%pub(which)%procs%s(16), p%pub(which)%absints%s(16), p%pub(which)%types%s(16), p%pub(which)%vars%s(16))
    do c = 1, size(p%list%box(id)%e%more%public_list)
        call index_set(p%pub(which)%public_list, p%list%box(id)%e%more%public_list(c)%s, c)
    end do

    associate (m => p%list%box(id)%e)
        do c = 1, size(m%children)
            child = m%children(c)
            associate (e => p%list%box(child)%e)
                if (.not. allocated(e%permission)) cycle
                if (e%permission /= 'public' .and. e%permission /= 'protected') cycle
                select case (e%kind)
                case (E_TYPE)
                    call add_symbol(p%pub(which)%types, lowercase(e%name), child)
                case (E_VARIABLE)
                    if (e%var_role == VAR_HIDDEN) cycle
                    call add_symbol(p%pub(which)%vars, lowercase(e%name), child)
                    if (e%vartype == 'procedure') call add_symbol(p%pub(which)%procs, lowercase(e%name), child)
                case (E_SUBROUTINE, E_FUNCTION)
                    if (e%iface == IFACE_ABSTRACT) then
                        call add_symbol(p%pub(which)%absints, lowercase(e%name), child)
                    else
                        call add_symbol(p%pub(which)%procs, lowercase(e%name), child)
                    end if
                case (E_INTERFACE)
                    call add_symbol(p%pub(which)%procs, lowercase(e%name), child)
                end select
            end associate
        end do
    end associate
    call generic_bodies(p, which, id)

    do u = 1, size(p%list%box(id)%e%more%uses)
        used = module_position(p, p%list%box(id)%e%more%uses(u)%target)
        if (used == 0) cycle
        if (p%pub(used)%state == 1) cycle
        call public_names(p, used)
        call import_names(p, which, used, p%list%box(id)%e%more%uses(u)%spec)
    end do
    p%pub(which)%state = 2
end subroutine public_names

!> Add the public bodies of the generic interfaces of module `id`.
subroutine generic_bodies(p, which, id)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: which            !! position in `p%modules`
    integer, intent(in) :: id               !! the module
    integer :: c, b, child, body

    do c = 1, size(p%list%box(id)%e%children)
        child = p%list%box(id)%e%children(c)
        if (p%list%box(child)%e%kind /= E_INTERFACE) cycle
        do b = 1, size(p%list%box(child)%e%children)
            body = p%list%box(child)%e%children(b)
            associate (e => p%list%box(body)%e)
                if (.not. is_procedure(e%kind)) cycle
                if (e%permission == 'public' .or. e%permission == 'protected') then
                    call add_symbol(p%pub(which)%procs, lowercase(e%name), body)
                end if
            end associate
        end do
    end do
end subroutine generic_bodies

!> Add to module `which` what it makes public of what `use` with `spec` imports from module `used`.
subroutine import_names(p, which, used, spec)
    type(project_t), intent(inout) :: p     !! the project
    integer, intent(in) :: which            !! position in `p%modules` of the importing module
    integer, intent(in) :: used             !! position in `p%modules` of the used module
    character(len=*), intent(in) :: spec    !! what follows the module name
    type(rename_t), allocatable :: renames(:)
    type(name_index_t) :: wanted
    logical :: only
    integer :: k

    call read_use_spec(spec, only, renames)
    do k = size(renames), 1, -1
        call index_set(wanted, renames(k)%used, k)
    end do
    call import_category(p%pub(used)%procs, p%pub(which)%procs)
    call import_category(p%pub(used)%absints, p%pub(which)%absints)
    call import_category(p%pub(used)%types, p%pub(which)%types)
    call import_category(p%pub(used)%vars, p%pub(which)%vars)

contains

    !> Import the names of one category.
    subroutine import_category(from, to)
        type(symbol_list_t), intent(in) :: from     !! the used module's names
        type(symbol_list_t), intent(inout) :: to    !! the importing module's names
        integer :: k, j

        do k = 1, from%n
            if (.not. only) then
                if (is_public_here(from%s(k)%name)) call add_symbol(to, from%s(k)%name, from%s(k)%id)
                cycle
            end if
            j = index_find(wanted, from%s(k)%name)
            if (j == 0) cycle
            if (is_public_here(renames(j)%local)) call add_symbol(to, renames(j)%local, from%s(k)%id)
        end do
    end subroutine import_category

    !> True if the importing module makes `name` public.
    logical function is_public_here(name)
        character(len=*), intent(in) :: name    !! the name
        is_public_here = p%list%box(p%modules(which))%e%permission == 'public' .or. &
                         index_find(p%pub(which)%public_list, name) > 0
    end function is_public_here

end subroutine import_names

!> Read what follows the module name in a `use` statement.
subroutine read_use_spec(spec, only, renames)
    character(len=*), intent(in) :: spec                    !! the text
    logical, intent(out) :: only                            !! true for an `only` list
    type(rename_t), allocatable, intent(out) :: renames(:)  !! the names used
    character(len=:), allocatable :: text
    type(string_t), allocatable :: items(:)
    integer :: k, at

    allocate (renames(0))
    text = strip(lowercase(spec))
    only = starts_with(text, 'only')
    if (.not. only) return
    at = index(text, ':')
    text = text(at + 1:)
    items = top_split(text, ',')
    do k = 1, size(items)
        at = index(items(k)%s, '=>')
        if (at > 0) then
            renames = [renames, rename_t(strip(items(k)%s(at + 2:)), strip(items(k)%s(:at - 1)))]
        else
            renames = [renames, rename_t(items(k)%s, items(k)%s)]
        end if
    end do
end subroutine read_use_spec

!> Hide what should not be displayed.
subroutine prune(p, s)
    type(project_t), intent(inout) :: p     !! the project
    type(settings_t), intent(in) :: s       !! the settings
    integer :: k

    do k = 1, p%list%n
        associate (e => p%list%box(k)%e)
            if (e%kind == E_NONE .or. e%parent == 0) cycle
            if (.not. allocated(e%permission)) cycle
            associate (parent => p%list%box(e%parent)%e)
                if (e%kind == E_VARIABLE .and. e%var_role /= VAR_PLAIN) cycle
                if (allocated(parent%display) .and. .not. is_program_unit(e%kind)) then
                    if (.not. contains_string(parent%display, e%permission)) e%visible = .false.
                end if
                if (s%hide_undoc .and. size(e%doc) == 0) e%visible = .false.
                if (is_procedure(parent%kind) .and. .not. parent%internals) then
                    e%visible = .false.
                end if
            end associate
        end associate
    end do
    call reveal_bound_procedures(p, s)
end subroutine prune

!> Show the procedures that shown bindings bind to, even if they are private: whoever can see the type
!> can call them through it.
subroutine reveal_bound_procedures(p, s)
    type(project_t), intent(inout) :: p     !! the project
    type(settings_t), intent(in) :: s       !! the settings
    integer :: k, j, target

    do k = 1, p%list%n
        if (p%list%box(k)%e%kind /= E_BOUNDPROC) cycle
        if (.not. is_shown(p%list, k)) cycle
        if (.not. allocated(p%list%box(k)%e%more%binding_ids)) cycle
        do j = 1, size(p%list%box(k)%e%more%binding_ids)
            target = p%list%box(k)%e%more%binding_ids(j)
            if (target == 0) cycle
            if (p%list%box(k)%e%generic) then
                ! a specific binding of a generic one, which may itself be private
                if (p%list%box(target)%e%kind /= E_BOUNDPROC) cycle
                if (.not. allocated(p%list%box(target)%e%more%binding_ids)) cycle
                if (p%list%box(target)%e%generic .or. size(p%list%box(target)%e%more%binding_ids) /= 1) cycle
                target = p%list%box(target)%e%more%binding_ids(1)
                if (target == 0) cycle
            end if
            call reveal(target)
        end do
    end do
contains
    !> Show procedure `id`, unless it is hidden for having no documentation, or its parent is not shown.
    subroutine reveal(id)
        integer, intent(in) :: id   !! the procedure
        associate (e => p%list%box(id)%e)
            if (e%visible) return
            if (.not. (is_procedure(e%kind) .or. e%kind == E_MODPROC)) return
            if (s%hide_undoc .and. size(e%doc) == 0) return
            if (e%parent > 0) then
                if (.not. is_shown(p%list, e%parent)) return
            end if
            e%visible = .true.
        end associate
    end subroutine reveal
end subroutine reveal_bound_procedures

!> True if entities of `kind` are program units. The permission of a module is what its members get
!> by default, so `display` says nothing about whether the module itself is shown.
pure logical function is_program_unit(kind)
    integer, intent(in) :: kind     !! the kind of entity
    is_program_unit = kind == E_MODULE .or. kind == E_SUBMODULE .or. kind == E_PROGRAM .or. kind == E_BLOCKDATA
end function is_program_unit

end module fdoc_project
