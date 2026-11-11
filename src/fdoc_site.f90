module fdoc_site
!! Writing the HTML site. Pages are rendered and written in parallel, so nothing that renders a page
!! calls a function returning a deferred-length string (see `example/slen_race.f90`).
use iso_fortran_env, only: int64
use fdoc_util
use fdoc_buffer
use fdoc_model
use fdoc_settings, only: settings_t
use fdoc_project, only: project_t
use fdoc_pages, only: page_t
use fdoc_graphs, only: graph_set_t, build_graphs
use json_module, only: json_core, json_value
use fdoc_urls, only: entity_url, entity_anchor, object_word, page_name, url_quote, is_shown
use fdoc_highlight, only: highlight_fortran
use fdoc_generics, only: write_call_forms
use fdoc_stats, only: file_stats_t, count_source, add_stats
use fdoc_assets, only: asset_fdoc_css, asset_fdoc_js
use fdoc_progress, only: progress_start, progress_tick, progress_finish, warn
use fdoc_shell, only: make_directory, remove_directory, copy_file, copy_path
use ffmt_fs, only: is_directory
implicit none
private

public :: assign_pages, write_site

integer, parameter :: LIST_FILES = 1, LIST_MODULES = 2, LIST_PROCEDURES = 3, LIST_ABSINT = 4, LIST_TYPES = 5
integer, parameter :: LIST_PROGRAMS = 6, LIST_BLOCKDATA = 7, LIST_NAMELISTS = 8
integer, parameter :: NUM_LISTS = 8

integer, parameter :: HERE_PAGES = -1   !! for `page_head`: a static page, not of a list
integer, parameter :: HERE_INDEX = -2   !! for `page_head`: the index of all names
integer, parameter :: MAX_NESTING = 16  !! how many levels of containers a page shows

character(len=*), parameter :: LIST_FILE(NUM_LISTS) = [character(len=10) :: 'files', 'modules', 'procedures', 'absint', &
                                                       'types', 'programs', 'blockdata', 'namelists']
character(len=*), parameter :: LIST_TITLE(NUM_LISTS) = [character(len=19) :: 'Source Files', 'Modules', 'Procedures', &
                                                        'Abstract Interfaces', 'Derived Types', 'Programs', 'Block Data', &
                                                        'Namelists']

character(len=*), parameter :: LIST_NOUN(NUM_LISTS) = [character(len=18) :: 'File', 'Module', 'Procedure', &
                                                       'Abstract Interface', 'Type', 'Program', 'Block Data Unit', &
                                                       'Namelist']

type :: index_list_t
    !! Entities, by index.
    integer, allocatable :: ids(:)              !! the entities
end type index_list_t

type :: site_t
    !! What every page needs.
    type(project_t), pointer :: p => null()     !! the project
    type(settings_t), pointer :: s => null()    !! the settings
    type(index_list_t), allocatable :: members(:)   !! the entities of each list page
    character(len=:), allocatable :: footer     !! the end of every page
    type(page_t), pointer :: pages(:) => null() !! the static pages
    type(graph_set_t) :: graphs                 !! the graphs
    type(index_list_t), allocatable :: graphs_of(:) !! the graphs of each entity
    type(file_stats_t), allocatable :: file_stats(:)    !! the size of each entity of the list of files
    integer, allocatable :: use_slot(:)         !! for each entity, its place in `type_uses`, or 0
    type(type_uses_t), allocatable :: type_uses(:)  !! where each type is used
end type site_t

type :: type_uses_t
    !! Where a type is used.
    integer, allocatable :: arguments(:)    !! the procedures that take it
    integer, allocatable :: results(:)      !! the functions that return it
    integer, allocatable :: components(:)   !! the types that hold it
    integer, allocatable :: variables(:)    !! the module variables of this type
end type type_uses_t

!> The directories of a site.
character(len=*), parameter :: SITE_DIRECTORIES(*) = [character(len=10) :: 'css', 'js', 'lists', 'module', 'proc', &
                                                      'type', 'interface', 'program', 'blockdata', 'namelist', &
                                                      'sourcefile', 'search', 'page']

contains

!> Give each entity with a page of its own a page name that is unique in its directory, as FORD does:
!> `name`, then `name~2`.
subroutine assign_pages(p)
    type(project_t), intent(inout) :: p     !! the project
    character(len=:), allocatable :: url, name
    type(name_index_t) :: seen
    integer :: k, count, at

    do k = 1, p%list%n
        associate (e => p%list%box(k)%e)
            if (e%kind == E_NONE .or. .not. allocated(e%name)) cycle
            if (allocated(e%external_url)) cycle
            ! these are parts of the page of what holds them
            select case (e%kind)
            case (E_VARIABLE, E_BOUNDPROC, E_COMMON, E_ENUM, E_FINAL)
                cycle
            end select
            if (e%kind == E_FILE) then
                at = last_separator(e%name)
                call page_name(e%name(at + 1:), name)
                e%page = name
            end if
            call entity_url(p%list, k, url)
            if (len(url) == 0 .or. index(url, '#') > 0) cycle
            count = index_find(seen, url) + 1
            call index_set(seen, url, count)
            if (count > 1) then
                call page_name(e%name, name)
                if (e%kind == E_FILE) name = e%page
                e%page = name//'~'//trim(int_text(count))
            end if
        end associate
    end do
end subroutine assign_pages

!> Length of the result of [[display_name]].
pure integer function display_length(e)
    type(entity_t), intent(in) :: e     !! the entity

    display_length = len(e%name)
    if (e%kind == E_FILE) display_length = len(e%name) - last_separator(e%name)
end function display_length

!> The members of each list page, in the order of the `sort` setting.
subroutine collect_lists(site)
    type(site_t), intent(inout) :: site     !! the site
    integer :: k, which

    allocate (site%members(NUM_LISTS))
    do which = 1, NUM_LISTS
        allocate (site%members(which)%ids(0))
    end do
    do k = 1, site%p%list%n
        which = list_of(site, k)
        if (which > 0) call push_int(site%members(which)%ids, k)
    end do
    do which = 1, NUM_LISTS
        call sort_ids(site, site%members(which)%ids)
    end do
end subroutine collect_lists

!> The list page that entity `id` belongs on, or 0.
integer function list_of(site, id) result(which)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: id           !! the entity
    logical :: page

    which = 0
    associate (e => site%p%list%box(id)%e)
        if (allocated(e%external_url) .or. .not. e%visible .or. .not. allocated(e%name)) return
        page = has_page(site, id)
        select case (e%kind)
        case (E_FILE); which = LIST_FILES
        case (E_MODULE, E_SUBMODULE); which = LIST_MODULES
        case (E_PROGRAM); which = LIST_PROGRAMS
        case (E_BLOCKDATA); which = LIST_BLOCKDATA
        case (E_NAMELIST); which = LIST_NAMELISTS
        case (E_TYPE); if (page) which = LIST_TYPES
        case (E_INTERFACE); if (page) which = LIST_PROCEDURES
        case (E_SUBROUTINE, E_FUNCTION, E_MODPROC)
            if (.not. page) return
            which = merge(LIST_ABSINT, LIST_PROCEDURES, e%iface == IFACE_ABSTRACT)
        end select
    end associate
end function list_of

!> True if entity `id` has a page of its own.
logical function has_page(site, id)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: id           !! the entity
    character(len=:), allocatable :: url

    associate (e => site%p%list%box(id)%e)
        has_page = .false.
        if (e%kind == E_NONE .or. e%kind == E_VARIABLE .or. e%kind == E_BOUNDPROC .or. e%kind == E_FINAL .or. &
            e%kind == E_COMMON .or. e%kind == E_ENUM) return
        if (.not. allocated(e%name) .or. allocated(e%external_url) .or. .not. e%visible) return
    end associate
    call entity_url(site%p%list, id, url)
    has_page = len(url) > 0 .and. index(url, '#') == 0
end function has_page

!> True if `a` sorts before `b`, for the `sort` setting.
logical function sorts_before(site, a, b)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: a            !! one entity
    integer, intent(in) :: b            !! the other
    character(len=:), allocatable :: mode
    integer :: pa, pb

    mode = site%s%sort
    sorts_before = .false.
    if (mode == 'src') return
    associate (x => site%p%list%box(a)%e, y => site%p%list%box(b)%e)
        if (starts_with(mode, 'permission')) then
            pa = permission_rank(x)
            pb = permission_rank(y)
            if (pa /= pb) then
                sorts_before = pa < pb
                return
            end if
            if (mode == 'permission') return
        end if
        if (starts_with(mode, 'type')) then
            if (allocated(x%vartype) .and. allocated(y%vartype)) then
                if (x%vartype /= y%vartype) then
                    sorts_before = llt(x%vartype, y%vartype)
                    return
                end if
            end if
            if (mode == 'type') return
        end if
        sorts_before = llt(lowercase(x%name), lowercase(y%name))
    end associate
end function sorts_before

!> The order of the permission of `e`.
pure integer function permission_rank(e)
    type(entity_t), intent(in) :: e     !! the entity

    permission_rank = 0
    if (.not. allocated(e%permission)) return
    select case (e%permission)
    case ('public'); permission_rank = 1
    case ('protected'); permission_rank = 2
    case ('private'); permission_rank = 3
    end select
end function permission_rank

!> Sort `ids`, keeping the source order of equal entries.
subroutine sort_ids(site, ids)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(inout) :: ids(:)        !! the entities
    integer :: i, j, id

    if (site%s%sort == 'src') return
    do i = 2, size(ids)
        id = ids(i)
        j = i - 1
        do while (j >= 1)
            if (.not. sorts_before(site, id, ids(j))) exit
            ids(j + 1) = ids(j)
            j = j - 1
        end do
        ids(j + 1) = id
    end do
end subroutine sort_ids

!> The visible children of `id` that `want` accepts, sorted: 1 variables, 2 functions, 3 subroutines,
!> 4 explicit and generic interfaces, 5 abstract interfaces, 6 types, 7 bindings, 8 final procedures,
!> 9 namelists, 10 common blocks, 11 enumerations, 12 separate module procedures.
subroutine children_of(site, id, want, ids)
    type(site_t), intent(in) :: site            !! the site
    integer, intent(in) :: id                   !! the entity
    integer, intent(in) :: want                 !! what to find
    integer, allocatable, intent(out) :: ids(:) !! the children
    integer :: k, child

    allocate (ids(0))
    associate (e => site%p%list%box(id)%e)
        do k = 1, size(e%children)
            child = e%children(k)
            if (accepts(site%p%list%box(child)%e, want)) call push_int(ids, child)
        end do
        if (allocated(e%more%inherited)) then
            do k = 1, size(e%more%inherited)
                child = e%more%inherited(k)
                if (accepts(site%p%list%box(child)%e, want) .and. contains_string(e%display, &
                                                                                  site%p%list%box(child)%e%permission)) then
                    call push_int(ids, child)
                end if
            end do
        end if
    end associate
    call sort_ids(site, ids)
end subroutine children_of

!> True if `c` is of the category `want` of [[children_of]].
pure logical function accepts(c, want)
    type(entity_t), intent(in) :: c     !! the entity
    integer, intent(in) :: want         !! the category

    accepts = .false.
    if (.not. c%visible .or. c%kind == E_NONE) return
    select case (want)
    case (1); accepts = c%kind == E_VARIABLE .and. c%var_role == VAR_PLAIN
    case (2); accepts = c%kind == E_FUNCTION .and. c%iface == IFACE_NONE
    case (3); accepts = c%kind == E_SUBROUTINE .and. c%iface == IFACE_NONE
    case (4); accepts = c%kind == E_INTERFACE .or. c%iface == IFACE_EXPLICIT
    case (5); accepts = c%iface == IFACE_ABSTRACT
    case (6); accepts = c%kind == E_TYPE
    case (7); accepts = c%kind == E_BOUNDPROC
    case (8); accepts = c%kind == E_FINAL
    case (9); accepts = c%kind == E_NAMELIST
    case (10); accepts = c%kind == E_COMMON
    case (11); accepts = c%kind == E_ENUM
    case (12); accepts = c%kind == E_MODPROC
    end select
end function accepts

!> Write `name`, linked to entity `id` if it can be.
subroutine link_to(site, id, name, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity, or 0
    character(len=*), intent(in) :: name    !! the text
    type(buffer_t), intent(inout) :: b      !! the HTML
    character(len=:), allocatable :: url

    url = ''
    if (id > 0) then
        associate (e => site%p%list%box(id)%e)
            if (is_shown(site%p%list, id) .or. allocated(e%external_url)) call entity_url(site%p%list, id, url)
        end associate
    end if
    if (len(url) == 0) then
        call html_escape(name, b, .false.)
        return
    end if
    call add(b, '<a href="')
    if (.not. allocated(site%p%list%box(id)%e%external_url)) call add(b, '../')
    call html_escape(url, b, .true.)
    call add(b, '">')
    call html_escape(name, b, .false.)
    call add(b, '</a>')
end subroutine link_to

!> Write the type of variable `e`.
subroutine write_type(site, e, b)
    type(site_t), intent(in) :: site        !! the site
    type(entity_t), intent(in) :: e         !! the variable
    type(buffer_t), intent(inout) :: b      !! the HTML

    call html_escape(e%vartype, b, .false.)
    if (allocated(e%type_kind) .and. allocated(e%strlen)) then
        call add(b, '(len=')
        call html_escape(e%strlen, b, .false.)
        call add(b, ', kind=')
        call html_escape(e%type_kind, b, .false.)
        call add(b, ')')
    else if (allocated(e%type_kind)) then
        call add(b, '(kind=')
        call html_escape(e%type_kind, b, .false.)
        call add(b, ')')
    else if (allocated(e%strlen)) then
        call add(b, '(len=')
        call html_escape(e%strlen, b, .false.)
        call add(b, ')')
    else if (allocated(e%proto)) then
        call add(b, '(')
        call link_to(site, e%proto_id, e%proto(1)%s, b)
        if (size(e%proto) > 1) then
            if (len(e%proto(2)%s) > 0) then
                call add(b, '(')
                call html_escape(e%proto(2)%s, b, .false.)
                call add(b, ')')
            end if
        end if
        call add(b, ')')
    end if
end subroutine write_type

!> Write the start of a page.
subroutine page_head(site, title, here, prefix, math, b)
    type(site_t), intent(in) :: site        !! the site
    character(len=*), intent(in) :: title   !! the title
    integer, intent(in) :: here             !! the list page this belongs to, `HERE_PAGES`, or 0
    character(len=*), intent(in) :: prefix  !! from the page to the root of the site: `../` or `''`
    logical, intent(in) :: math             !! true to load MathJax
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (s => site%s)
        call add(b, '<!DOCTYPE html>'//achar(10)//'<html lang="en">'//achar(10)//'<head>'//achar(10))
        call add(b, '<meta charset="utf-8">'//achar(10)//'<meta name="viewport" content="width=device-width, initial-scale=1">')
        call add(b, achar(10)//'<title>')
        call html_escape(title, b, .false.)
        if (title /= s%project) then
            call add(b, ' &ndash; ')
            call html_escape(s%project, b, .false.)
        end if
        call add(b, '</title>'//achar(10))
        if (allocated(s%author)) then
            call add(b, '<meta name="author" content="')
            call html_escape(s%author, b, .true.)
            call add(b, '">'//achar(10))
        end if
        if (allocated(s%favicon)) call add(b, '<link rel="icon" href="'//prefix//'favicon.png">'//achar(10))
        call add(b, '<link rel="stylesheet" href="'//prefix//'css/fdoc.css">'//achar(10))
        if (allocated(s%css)) call add(b, '<link rel="stylesheet" href="'//prefix//'css/user.css">'//achar(10))
        call add(b, '<script src="'//prefix//'js/fdoc.js"></script>'//achar(10))
        call add(b, '<script defer src="'//prefix//'js/sidebar.js"></script>'//achar(10))
        if (math) then
            call add(b, '<script async src="https://cdn.jsdelivr.net/npm/mathjax@2.7.9/MathJax.js?config=TeX-AMS-MML_HTMLorMML">')
            call add(b, '</script>'//achar(10))
        end if
        call add(b, '</head>'//achar(10)//'<body>'//achar(10)//'<header class="top">'//achar(10))
        call add(b, '<a class="brand" href="'//prefix//'index.html">')
        call html_escape(s%project, b, .false.)
        if (allocated(s%version)) then
            call add(b, '<small>')
            call html_escape(s%version, b, .false.)
            call add(b, '</small>')
        end if
        call add(b, '</a>'//achar(10)//'<nav>')
        do k = 1, NUM_LISTS
            if (size(site%members(k)%ids) == 0) cycle
            call add(b, '<a'//trim(merge(' class="here"', '             ', k == here))//' href="'//prefix//'lists/'// &
                     trim(LIST_FILE(k))//'.html">'//trim(LIST_TITLE(k))//'</a>')
        end do
        call add(b, '<a'//trim(merge(' class="here"', '             ', here == HERE_INDEX))//' href="'//prefix// &
                 'lists/names.html">Index</a>')
        if (size(site%pages) > 0) then
            call add(b, '<a'//trim(merge(' class="here"', '             ', here == HERE_PAGES))// &
                     ' href="'//prefix//'page/index.html">')
            call html_escape(site%pages(1)%title, b, .false.)
            call add(b, '</a>')
        end if
        call add(b, '</nav>'//achar(10))
        if (s%search) then
            call add(b, '<div class="search"><input type="search" placeholder="Search (/)" aria-label="Search">')
            call add(b, '<div class="results"></div></div>'//achar(10))
        end if
        call add(b, '<button class="theme" title="Change theme" aria-label="Change theme">&#9680;</button>')
        call add(b, achar(10)//'</header>'//achar(10)//'<div class="layout">'//achar(10))
    end associate
end subroutine page_head

!> Render static page `k`.
subroutine static_page(site, k, html)
    type(site_t), intent(in) :: site                    !! the site
    integer, intent(in) :: k                            !! the page
    character(len=:), allocatable, intent(out) :: html  !! the HTML
    type(buffer_t) :: b
    character(len=:), allocatable :: prefix
    integer :: j

    associate (page => site%pages(k))
        prefix = repeat('../', page%depth + 1)
        call page_head(site, page%title, HERE_PAGES, prefix, index(page%html, 'math/tex') > 0, b)
        call add(b, '<aside class="side tree">'//achar(10)//'<ul>')
        call page_links(site, 0, prefix, b)
        call add(b, '</ul>'//achar(10)//'</aside>'//achar(10)//'<main><div class="inner">'//achar(10))
        if (page%parent > 0) then
            call add(b, '<div class="crumbs">')
            j = page%parent
            call add(b, '<a href="'//prefix//'page/'//site%pages(j)%path//'">')
            call html_escape(site%pages(j)%title, b, .false.)
            call add(b, '</a></div>'//achar(10))
        end if
        call add(b, '<h1>')
        call html_escape(page%title, b, .false.)
        call add(b, '</h1>'//achar(10))
        if (allocated(page%author) .or. allocated(page%date)) then
            call add(b, '<div class="meta">')
            if (allocated(page%author)) then
                call add(b, '<span>')
                call html_escape(page%author, b, .false.)
                call add(b, '</span>')
            end if
            if (allocated(page%date)) then
                call add(b, '<span>')
                call html_escape(page%date, b, .false.)
                call add(b, '</span>')
            end if
            call add(b, '</div>'//achar(10))
        end if
        call add(b, '<div class="doc">'//achar(10))
        call add(b, page%html)
        call add(b, achar(10)//'</div>'//achar(10))
        call page_foot(site, b)
    end associate
    call take(b, html)
end subroutine static_page

!> Write the pages below page `parent`, or the top one if that is 0, as a tree of links.
recursive subroutine page_links(site, parent, prefix, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: parent           !! the page
    character(len=*), intent(in) :: prefix  !! from the current page to the root of the site
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    do k = 1, size(site%pages)
        if (site%pages(k)%parent /= parent) cycle
        call add(b, '<li><a href="'//prefix//'page/'//site%pages(k)%path//'">')
        call html_escape(site%pages(k)%title, b, .false.)
        call add(b, '</a>')
        if (any(site%pages%parent == k)) then
            call add(b, '<ul>')
            call page_links(site, k, prefix, b)
            call add(b, '</ul>')
        end if
        call add(b, '</li>')
    end do
end subroutine page_links

!> Render the search results page.
subroutine search_page(site, html)
    type(site_t), intent(in) :: site                    !! the site
    character(len=:), allocatable, intent(out) :: html  !! the HTML
    type(buffer_t) :: b

    call page_head(site, 'Search', 0, '', .false., b)
    call add(b, '<aside class="side">'//achar(10))
    call site_side(site, 0, '', b)
    call add(b, '</aside>'//achar(10)//'<main><div class="inner">'//achar(10)//'<h1>Search</h1>'//achar(10))
    call add(b, '<p class="search-count"></p><div class="search-page"></div>'//achar(10))
    call page_foot(site, b)
    call take(b, html)
end subroutine search_page

!> `html` as plain text of at most `limit` characters, with blanks collapsed.
subroutine plain_text(html, limit, out)
    character(len=*), intent(in) :: html                !! the HTML
    integer, intent(in) :: limit                        !! the length wanted
    character(len=:), allocatable, intent(out) :: out   !! the text
    type(buffer_t) :: b
    logical :: in_tag, blank
    integer :: k

    in_tag = .false.
    blank = .false.
    do k = 1, len(html)
        associate (c => html(k:k))
            if (c == '<') then
                in_tag = .true.
            else if (c == '>' .and. in_tag) then
                in_tag = .false.
            else if (.not. in_tag) then
                if (c == ' ' .or. c == achar(10)) then
                    blank = b%n > 0
                else
                    if (blank) call add(b, ' ')
                    blank = .false.
                    call add(b, c)
                end if
            end if
        end associate
        if (b%n >= limit) exit
    end do
    call take(b, out)
end subroutine plain_text

!> The search index, as a script: browsers do not let a page opened from a file read JSON.
subroutine search_index(site, text)
    type(site_t), intent(in) :: site                    !! the site
    character(len=:), allocatable, intent(out) :: text  !! the script
    type(json_core) :: json
    type(json_value), pointer :: root
    character(len=:), allocatable :: url, summary, json_text, word
    integer :: k, which, j

    call json%initialize(no_whitespace=.true.)
    call json%create_array(root, '')
    do which = 1, NUM_LISTS
        do j = 1, size(site%members(which)%ids)
            k = site%members(which)%ids(j)
            associate (e => site%p%list%box(k)%e)
                call entity_url(site%p%list, k, url)
                summary = ''
                if (allocated(e%summary)) call plain_text(e%summary, 140, summary)
                word = trim(LIST_TITLE(which))
                if (e%kind == E_SUBROUTINE) word = 'subroutine'
                if (e%kind == E_FUNCTION) word = 'function'
                if (e%kind == E_INTERFACE) word = 'interface'
                if (e%kind == E_TYPE) word = 'type'
                if (e%kind == E_MODULE) word = 'module'
                if (e%kind == E_SUBMODULE) word = 'submodule'
                if (e%kind == E_PROGRAM) word = 'program'
                if (e%kind == E_FILE) word = 'file'
                call add_entry(display_name(e), url, word, summary)
            end associate
        end do
    end do
    do k = 1, size(site%pages)
        call plain_text(site%pages(k)%html, 140, summary)
        call add_entry(site%pages(k)%title, 'page/'//site%pages(k)%path, 'page', summary)
    end do
    call json%serialize(root, json_text)
    call json%destroy(root)
    text = 'window.FDOC_INDEX='//json_text//';'//achar(10)

contains

    !> Add an entry.
    subroutine add_entry(title, link, kind, description)
        character(len=*), intent(in) :: title       !! what it is called
        character(len=*), intent(in) :: link        !! where it is
        character(len=*), intent(in) :: kind        !! what it is
        character(len=*), intent(in) :: description !! a line about it
        type(json_value), pointer :: item

        call json%create_object(item, '')
        call json%add(item, 't', title)
        call json%add(item, 'u', link)
        call json%add(item, 'k', kind)
        call json%add(item, 's', description)
        call json%add(root, item)
    end subroutine add_entry

end subroutine search_index

!> Write the end of a page.
subroutine page_foot(site, b)
    type(site_t), intent(in) :: site    !! the site
    type(buffer_t), intent(inout) :: b  !! the HTML

    call add(b, site%footer)
    call add(b, '</div>'//achar(10)//'</main>'//achar(10)//'</div>'//achar(10)//'</body>'//achar(10)//'</html>'//achar(10))
end subroutine page_foot

!> Write a sidebar group of links to the entities `ids`, on the same page if `anchors`.
subroutine side_group(site, title, ids, b)
    type(site_t), intent(in) :: site        !! the site
    character(len=*), intent(in) :: title   !! the heading
    integer, intent(in) :: ids(:)           !! the entities
    type(buffer_t), intent(inout) :: b      !! the HTML
    character(len=:), allocatable :: url
    integer :: k, at

    if (size(ids) == 0) return
    call add(b, '<h4>'//title//'</h4><ul>')
    do k = 1, size(ids)
        call entity_url(site%p%list, ids(k), url)
        at = index(url, '#')
        call add(b, '<li><a href="')
        if (at > 0) then
            call html_escape(url(at:), b, .true.)
        else if (len(url) > 0) then
            call add(b, '../')
            call html_escape(url, b, .true.)
        end if
        call add(b, '">')
        call side_icon(site%p%list%box(ids(k))%e, b)
        call html_escape(site%p%list%box(ids(k))%e%name, b, .false.)
        call add(b, '</a></li>')
    end do
    call add(b, '</ul>'//achar(10))
end subroutine side_group

!> Write the icon of entity `e` in a sidebar: a letter, with what it stands for as its tooltip.
subroutine side_icon(e, b)
    type(entity_t), intent(in) :: e     !! the entity
    type(buffer_t), intent(inout) :: b  !! the HTML
    character(len=:), allocatable :: letter, word

    call icon_of(e, letter, word)
    if (len(letter) == 0) return
    call add(b, '<span class="ico ico-'//letter_class(letter)//'" title="'//word//'" aria-hidden="true">'//letter//'</span>')
end subroutine side_icon

!> The letter of the icon of entity `e`, or `''` if it has none, and what it stands for.
subroutine icon_of(e, letter, word)
    type(entity_t), intent(in) :: e                         !! the entity
    character(len=:), allocatable, intent(out) :: letter    !! the letter
    character(len=:), allocatable, intent(out) :: word      !! what it stands for

    select case (e%kind)
    case (E_FILE); letter = '&equiv;'; word = 'source file'
    case (E_MODULE); letter = 'M'; word = 'module'
    case (E_SUBMODULE); letter = 'M'; word = 'submodule'
    case (E_PROGRAM); letter = 'P'; word = 'program'
    case (E_BLOCKDATA); letter = 'D'; word = 'block data'
    case (E_SUBROUTINE); letter = 'S'; word = 'subroutine'
    case (E_FUNCTION); letter = 'F'; word = 'function'
    case (E_MODPROC); letter = 'S'; word = 'module procedure'
    case (E_INTERFACE); letter = 'I'; word = 'interface'
    case (E_TYPE); letter = 'T'; word = 'derived type'
    case (E_BOUNDPROC); letter = 'B'; word = 'type-bound procedure'
    case (E_VARIABLE); letter = 'V'; word = 'variable'
    case (E_NAMELIST); letter = 'N'; word = 'namelist'
    case default
        letter = ''
        word = ''
        return
    end select
    if (e%iface == IFACE_EXPLICIT) then
        letter = 'I'
        word = 'interface'
    else if (e%iface == IFACE_ABSTRACT) then
        letter = 'A'
        word = 'abstract interface'
    end if
end subroutine icon_of

!> The lowercase class for icon `letter`.
pure function letter_class(letter) result(class)
    character(len=*), intent(in) :: letter  !! the letter, or an entity for a symbol
    character :: class

    class = '_'
    if (len(letter) == 1) class = lower_char(letter)
end function letter_class

!> Write the title of an entity's page, with what is known about it.
subroutine entity_title(site, id, word, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    character(len=*), intent(in) :: word    !! what it is
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k, body
    logical :: open

    associate (e => site%p%list%box(id)%e)
        call breadcrumbs(site, id, b)
        call add(b, '<h1><span class="kind">'//word//'</span>')
        call html_escape(display_name(e), b, .false.)
        call procedure_badges(e, b)
        if (allocated(e%permission)) then
            if (e%permission /= 'public' .and. e%kind /= E_MODULE .and. e%kind /= E_SUBMODULE .and. e%kind /= E_FILE .and. &
                e%kind /= E_PROGRAM) call badge(e%permission, b)
        end if
        call add(b, '</h1>'//achar(10))
        open = .false.
        do k = 1, size(e%meta)
            select case (e%meta(k)%key)
            case ('author', 'date', 'version', 'since', 'license', 'category')
                if (.not. open) call add(b, '<div class="meta">')
                open = .true.
                call add(b, '<span>'//e%meta(k)%key//': ')
                call html_escape(join(e%meta(k)%values, ' '), b, .false.)
                call add(b, '</span>')
            case ('deprecated')
                if (.not. open) call add(b, '<div class="meta">')
                open = .true.
                call badge('deprecated', b)
            end select
        end do
        if (open) call add(b, '</div>'//achar(10))
        select case (e%kind)
        case (E_SUBROUTINE, E_FUNCTION, E_MODPROC)
            call signature(site, id, b)
        end select
        if (allocated(e%html)) then
            if (len(e%html) > 0) then
                call add(b, '<div class="doc">'//achar(10))
                call add(b, e%html)
                call add(b, achar(10)//'</div>'//achar(10))
            end if
        end if
        body = hidden_body(site, id)
        if (body > 0) then
            if (allocated(site%p%list%box(body)%e%html)) then
                if (len(site%p%list%box(body)%e%html) > 0) then
                    call add(b, '<div class="doc">'//achar(10))
                    call add(b, site%p%list%box(body)%e%html)
                    call add(b, achar(10)//'</div>'//achar(10))
                end if
            end if
        end if
    end associate
end subroutine entity_title

!> The body of the separate module procedure whose interface is `id`, if it has no page of its own, so
!> that its documentation goes on the page of the interface; 0 otherwise.
integer function hidden_body(site, id) result(body)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: id           !! the entity

    body = 0
    associate (e => site%p%list%box(id)%e)
        if (e%iface /= IFACE_EXPLICIT .or. e%implements <= 0) return
        if (is_shown(site%p%list, e%implements)) return
        body = e%implements
    end associate
end function hidden_body

!> The name in the site of the file of the `logo` setting: `logo`, with its extension, or `''` if there
!> is no such file.
subroutine logo_name(s, name)
    type(settings_t), intent(in) :: s                   !! the settings
    character(len=:), allocatable, intent(out) :: name  !! the name
    logical :: found
    integer :: at

    name = ''
    if (.not. allocated(s%logo)) return
    !$omp critical (file_io)
    inquire (file=s%logo, exist=found)
    !$omp end critical (file_io)
    if (.not. found) return
    at = max(index(s%logo, '.', back=.true.), last_separator(s%logo) + 1)
    name = 'logo'//s%logo(at:)
    if (index(name, '.') == 0) name = 'logo'
end subroutine logo_name

!> Measure the source files of the list of files.
subroutine measure_files(site)
    type(site_t), intent(inout) :: site     !! the site
    integer :: k, n, file

    n = size(site%members(LIST_FILES)%ids)
    allocate (site%file_stats(n))
    !$omp parallel do schedule(dynamic) num_threads(max(1, site%s%parallel)) default(shared) private(k)
    do k = 1, n
        associate (e => site%p%list%box(site%members(LIST_FILES)%ids(k))%e)
            if (allocated(e%source_text)) call count_source(e%source_text, site%file_stats(k))
        end associate
    end do
    !$omp end parallel do
    if (n == 0) return
    do k = 1, site%p%list%n
        associate (e => site%p%list%box(k)%e)
            if (e%kind /= E_SUBROUTINE .and. e%kind /= E_FUNCTION .and. e%kind /= E_MODPROC) cycle
            if (e%iface /= IFACE_NONE .or. e%parent == 0) cycle
            if (site%p%list%box(e%parent)%e%kind == E_INTERFACE) cycle
            file = findloc(site%members(LIST_FILES)%ids, e%file, 1)
            if (file > 0) site%file_stats(file)%procedures = site%file_stats(file)%procedures + 1
        end associate
    end do
end subroutine measure_files

!> Write `stats` as cells of a table. Each has the plain number, to sort by.
subroutine stats_cells(stats, b)
    type(file_stats_t), intent(in) :: stats !! the counts
    type(buffer_t), intent(inout) :: b      !! the HTML

    call number_cell(int(stats%lines, int64), b)
    call number_cell(int(stats%code, int64), b)
    call number_cell(int(stats%comments, int64), b)
    call number_cell(int(stats%blank, int64), b)
    call add(b, '<td class="num" data-sort="')
    call add_int(b, stats%bytes)
    call add(b, '">')
    call size_text(stats%bytes, b)
    call add(b, '</td>')
    call number_cell(int(stats%procedures, int64), b)
end subroutine stats_cells

!> Write the row of totals of the list of files.
subroutine stats_total(site, described, b)
    type(site_t), intent(in) :: site        !! the site
    logical, intent(in) :: described        !! true if the table has descriptions
    type(buffer_t), intent(inout) :: b      !! the HTML
    type(file_stats_t) :: total
    integer :: k

    do k = 1, size(site%file_stats)
        call add_stats(total, site%file_stats(k))
    end do
    call add(b, '<tfoot><tr><td>Total</td>')
    call stats_cells(total, b)
    if (described) call add(b, '<td></td>')
    call add(b, '</tr></tfoot>'//achar(10))
end subroutine stats_total

!> Write `value` as a cell of a table.
subroutine number_cell(value, b)
    integer(int64), intent(in) :: value     !! the number
    type(buffer_t), intent(inout) :: b      !! the HTML

    call add(b, '<td class="num" data-sort="')
    call add_int(b, value)
    call add(b, '">')
    call grouped(value, b)
    call add(b, '</td>')
end subroutine number_cell

!> Write `value`, not negative, which may be too large for [[int_text]].
recursive subroutine add_int(b, value)
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer(int64), intent(in) :: value     !! the number

    if (value >= 10) call add_int(b, value/10)
    call add(b, achar(iachar('0') + int(mod(value, 10_int64))))
end subroutine add_int

!> Write `value`, not negative, with a comma between each group of three digits.
recursive subroutine grouped(value, b)
    integer(int64), intent(in) :: value     !! the number
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer(int64), parameter :: GROUP = 1000
    integer :: rest

    if (value < GROUP) then
        call add_int(b, value)
        return
    end if
    call grouped(value/GROUP, b)
    rest = int(mod(value, GROUP))
    call add(b, ',')
    if (rest < 100) call add(b, '0')
    if (rest < 10) call add(b, '0')
    call add_int(b, int(rest, int64))
end subroutine grouped

!> Write a size in bytes, with one decimal once it is in kilobytes or megabytes.
subroutine size_text(bytes, b)
    integer(int64), intent(in) :: bytes     !! the size
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer(int64), parameter :: KILO = 1000
    integer(int64) :: unit, tenths

    if (bytes < KILO) then
        call add_int(b, bytes)
        call add(b, ' B')
        return
    end if
    unit = merge(KILO, KILO*KILO, bytes < KILO*KILO)
    tenths = (10*bytes + unit/2)/unit
    call grouped(tenths/10, b)
    call add(b, '.')
    call add_int(b, mod(tenths, 10_int64))
    call add(b, merge(' kB', ' MB', bytes < KILO*KILO))
end subroutine size_text

!> Write what contains entity `id`, from its source file down, each with a link to its page.
subroutine breadcrumbs(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: chain(MAX_NESTING)
    integer :: k, n, up

    n = 0
    up = site%p%list%box(id)%e%parent
    do while (up > 0 .and. n < MAX_NESTING)
        n = n + 1
        chain(n) = up
        up = site%p%list%box(up)%e%parent
    end do
    if (n == 0) return
    call add(b, '<div class="crumbs">')
    do k = n, 1, -1
        call link_to(site, chain(k), display_name(site%p%list%box(chain(k))%e), b)
        if (k > 1) call add(b, ' <span class="sep">&rsaquo;</span> ')
    end do
    call add(b, '</div>'//achar(10))
end subroutine breadcrumbs

!> The base name of a file entity, or the name of another.
pure function display_name(e) result(name)
    type(entity_t), intent(in) :: e     !! the entity
    character(len=display_length(e)) :: name
    integer :: at

    at = 0
    if (e%kind == E_FILE) at = last_separator(e%name)
    name = e%name(at + 1:)
end function display_name

!> Write the declaration of a procedure.
subroutine signature(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the procedure
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (e => site%p%list%box(id)%e)
        call add(b, '<div class="sig">')
        if (allocated(e%attribs)) then
            do k = 1, size(e%attribs)
                call add(b, '<span class="k">')
                call html_escape(e%attribs(k)%s, b, .false.)
                call add(b, '</span> ')
            end do
        end if
        if (e%kind == E_FUNCTION .and. e%result > 0) then
            call write_type(site, site%p%list%box(e%result)%e, b)
            call add(b, ' ')
        end if
        select case (e%kind)
        case (E_FUNCTION); call add(b, '<span class="k">function</span> ')
        case (E_SUBROUTINE); call add(b, '<span class="k">subroutine</span> ')
        case (E_MODPROC); call add(b, '<span class="k">module procedure</span> ')
        end select
        call html_escape(e%name, b, .false.)
        if (allocated(e%more%args)) then
            call add(b, '(')
            call signature_arguments(site, e, b)
            call add(b, ')')
        end if
        if (e%kind == E_FUNCTION) then
            if (allocated(e%more%result_name)) then
                if (lowercase(e%more%result_name) /= lowercase(e%name)) then
                    call add(b, ' <span class="k">result</span>(')
                    call html_escape(e%more%result_name, b, .false.)
                    call add(b, ')')
                end if
            end if
        end if
        if (allocated(e%more%bind_c)) then
            call add(b, ' <span class="k">bind</span>(')
            call html_escape(e%more%bind_c, b, .false.)
            call add(b, ')')
        end if
        call add(b, '</div>'//achar(10))
    end associate
end subroutine signature

!> Write the arguments of procedure `e`, each in the colour of its intent, as in the table of arguments.
subroutine signature_arguments(site, e, b)
    type(site_t), intent(in) :: site        !! the site
    type(entity_t), intent(in) :: e         !! the procedure
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k, id

    do k = 1, size(e%more%args)
        if (k > 1) call add(b, ', ')
        id = 0
        if (allocated(e%more%arg_ids)) then
            if (size(e%more%arg_ids) == size(e%more%args)) id = e%more%arg_ids(k)
        end if
        if (id > 0) then
            if (allocated(site%p%list%box(id)%e%intent)) then
                if (len(site%p%list%box(id)%e%intent) > 0) then
                    associate (intent => site%p%list%box(id)%e%intent)
                        call add(b, '<span class="intent attr-'//intent//'" title="intent('//intent//')">')
                        call html_escape(e%more%args(k)%s, b, .false.)
                        call add(b, '</span>')
                    end associate
                    cycle
                end if
            end if
        end if
        call html_escape(e%more%args(k)%s, b, .false.)
    end do
end subroutine signature_arguments

!> Write a table of dummy arguments, or with `components` of other variables, such as the components
!> of a type. Those have no intent, and may have a default value and a permission.
subroutine argument_table(site, ids, components, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: ids(:)           !! the variables
    logical, intent(in) :: components       !! true for variables that are not arguments
    type(buffer_t), intent(inout) :: b      !! the HTML
    logical :: defaults
    integer :: k

    defaults = .false.
    if (components) then
        do k = 1, size(ids)
            if (allocated(site%p%list%box(ids(k))%e%initial)) defaults = .true.
        end do
    end if
    call add(b, '<table class="arguments">'//achar(10)//'<thead><tr><th>Name</th><th>Type</th>')
    if (.not. components) call add(b, '<th>Intent</th>')
    call add(b, '<th>Attributes</th>')
    if (defaults) call add(b, '<th>Default</th>')
    call add(b, '<th>Description</th></tr></thead>'//achar(10)//'<tbody>'//achar(10))
    do k = 1, size(ids)
        associate (e => site%p%list%box(ids(k))%e)
            call add(b, '<tr id="')
            call anchor(site, ids(k), b)
            call add(b, '"><td class="name">')
            call html_escape(e%name, b, .false.)
            call add(b, '</td><td class="type">')
            if (e%kind == E_VARIABLE) then
                call write_type(site, e, b)
                call add(b, '</td>')
                if (.not. components) then
                    call add(b, '<td>')
                    if (allocated(e%intent)) then
                        if (len(e%intent) > 0) call badge(e%intent, b)
                    end if
                    call add(b, '</td>')
                end if
                call add(b, '<td class="attributes">')
                if (components .and. allocated(e%permission)) then
                    if (e%permission /= 'public') call badge(e%permission, b)
                end if
                call argument_attributes(e, b)
                call add(b, '</td>')
                if (defaults) then
                    call add(b, '<td class="default">')
                    if (allocated(e%initial)) then
                        if (e%points) call add(b, '=&gt; ')
                        call html_escape(e%initial, b, .false.)
                    end if
                    call add(b, '</td>')
                end if
                call add(b, '<td>')
                if (allocated(e%html)) call add(b, e%html)
            else
                ! a dummy procedure with an interface
                call add(b, 'procedure</td><td></td><td class="attributes"></td><td>')
                if (allocated(e%summary)) call add(b, e%summary)
            end if
            call add(b, '</td></tr>'//achar(10))
        end associate
    end do
    call add(b, '</tbody>'//achar(10)//'</table>'//achar(10))
end subroutine argument_table

!> Write a table of type-bound procedures.
subroutine binding_table(site, ids, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: ids(:)           !! the bindings
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k, j

    call add(b, '<table class="arguments">'//achar(10)//'<thead><tr><th>Name</th><th>Binds to</th><th>Attributes</th>')
    call add(b, '<th>Description</th></tr></thead>'//achar(10)//'<tbody>'//achar(10))
    do k = 1, size(ids)
        associate (t => site%p%list%box(ids(k))%e)
            call add(b, '<tr id="')
            call anchor(site, ids(k), b)
            call add(b, '"><td class="name">')
            call html_escape(t%name, b, .false.)
            call add(b, '</td><td class="binds">')
            if (allocated(t%more%proto_text)) then
                call link_to(site, t%proto_id, t%more%proto_text, b)
            else if (allocated(t%more%binding_ids) .and. .not. t%deferred) then
                do j = 1, size(t%more%binding_ids)
                    if (j > 1) call add(b, ', ')
                    call link_to(site, t%more%binding_ids(j), t%more%bindings(j)%s, b)
                end do
            end if
            call add(b, '</td><td class="attributes">')
            if (t%generic) call badge('generic', b)
            if (t%deferred) call badge('deferred', b)
            if (t%permission /= 'public') call badge(t%permission, b)
            do j = 1, size(t%attribs)
                call badge(t%attribs(j)%s, b)
            end do
            call add(b, '</td><td>')
            call binding_description(site, ids(k), b)
            call add(b, '</td></tr>'//achar(10))
        end associate
    end do
    call add(b, '</tbody>'//achar(10)//'</table>'//achar(10))
end subroutine binding_table

!> Write the documentation of binding `id`, or else the summary of the one procedure it binds to.
subroutine binding_description(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the binding
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: target

    associate (t => site%p%list%box(id)%e)
        if (allocated(t%html)) then
            if (len(t%html) > 0) then
                call add(b, t%html)
                return
            end if
        end if
        if (t%generic .or. t%deferred .or. .not. allocated(t%more%binding_ids)) return
        if (size(t%more%binding_ids) /= 1) return
        target = t%more%binding_ids(1)
        if (target == 0) return
        if (.not. is_shown(site%p%list, target)) return
        if (allocated(site%p%list%box(target)%e%summary)) call add(b, site%p%list%box(target)%e%summary)
    end associate
end subroutine binding_description


!> Write the attributes of argument `e`.
subroutine argument_attributes(e, b)
    type(entity_t), intent(in) :: e     !! the argument
    type(buffer_t), intent(inout) :: b  !! the HTML
    integer :: k

    if (e%parameter) call badge('parameter', b)
    if (e%optional) call badge('optional', b)
    do k = 1, size(e%attribs)
        call badge(e%attribs(k)%s, b)
    end do
    if (allocated(e%dimension)) then
        if (index(e%dimension, '(') == 1) call badge('dimension'//e%dimension, b)
    end if
end subroutine argument_attributes

!> Write `text`, an intent or attribute, as a badge. Its class comes from its first word, such as
!> `attr-pointer`, which is what gives it its colour.
subroutine badge(text, b)
    character(len=*), intent(in) :: text    !! the intent or attribute
    type(buffer_t), intent(inout) :: b      !! the HTML
    character(len=*), parameter :: LETTERS = 'abcdefghijklmnopqrstuvwxyz'
    integer :: n

    n = verify(lowercase(text)//'(', LETTERS) - 1
    call add(b, '<span class="attr attr-'//lowercase(text(:n))//'">')
    call html_escape(text, b, .false.)
    call add(b, '</span>')
end subroutine badge

!> Write the attributes of procedure `e` that say how it behaves, as badges. Anything else is left out.
subroutine procedure_badges(e, b)
    type(entity_t), intent(in) :: e     !! the entity
    type(buffer_t), intent(inout) :: b  !! the HTML
    integer :: k

    if (.not. is_procedure(e%kind)) return
    if (allocated(e%attribs)) then
        do k = 1, size(e%attribs)
            select case (lowercase(e%attribs(k)%s))
            case ('pure')
                call explained_badge('pure', 'Has no side effects', b)
            case ('impure')
                call explained_badge('impure', 'May have side effects', b)
            case ('elemental')
                call explained_badge('elemental', 'Works on each element of arrays of any rank', b)
            case ('recursive')
                call explained_badge('recursive', 'May call itself', b)
            case ('non_recursive')
                call explained_badge('non_recursive', 'Must not call itself', b)
            end select
        end do
    end if
    if (allocated(e%more%bind_c)) call explained_badge('bind(C)', 'Can be called from C', b)
    if (e%more%saved_state) then
        call explained_badge('saved state', 'Keeps values between calls, so calls from several threads at once may conflict', b)
    end if
    if (e%more%common_block) then
        call explained_badge('common', 'Uses a common block, so calls from several threads at once may conflict', b)
    end if
end subroutine procedure_badges

!> Write `text` as a badge, with an explanation for when the pointer is over it.
subroutine explained_badge(text, explanation, b)
    character(len=*), intent(in) :: text        !! the attribute
    character(len=*), intent(in) :: explanation !! what it means
    type(buffer_t), intent(inout) :: b          !! the HTML
    type(buffer_t) :: item
    character(len=:), allocatable :: html

    call badge(text, item)
    call take(item, html)
    call add(b, html(:index(html, '>') - 1)//' title="'//explanation//'"'//html(index(html, '>'):))
end subroutine explained_badge

!> Write the anchor of entity `id`.
subroutine anchor(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML

    call entity_anchor(site%p%list, id, b)
end subroutine anchor

!> Write the graphs of entity `id`.
subroutine graph_sections(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    if (.not. allocated(site%graphs_of(id)%ids)) return
    do k = 1, size(site%graphs_of(id)%ids)
        associate (g => site%graphs%graphs(site%graphs_of(id)%ids(k)))
            if (len(g%svg) == 0) cycle
            call graph_heading(g%title, b)
            ! a list goes with the graph of the same thing, above it
            if (g%title == 'Uses') call uses_section(site, id, .false., b)
            if (g%title == 'Calls') call calls_section(site, id, .false., b)
            if (g%title == 'Used by' .or. g%title == 'Called by') call neighbour_chips(site, g%neighbours, b)
            call add(b, '<div class="graph">'//g%svg//'</div>'//achar(10))
        end associate
    end do
end subroutine graph_sections

!> Write `ids`, what is next to an entity in a graph, as chips in the order of their names.
subroutine neighbour_chips(site, ids, b)
    type(site_t), intent(in) :: site            !! the site
    integer, allocatable, intent(in) :: ids(:)  !! the entities, if any
    type(buffer_t), intent(inout) :: b          !! the HTML
    integer, allocatable :: sorted(:)
    integer :: k

    if (.not. allocated(ids)) return
    if (size(ids) == 0) return
    sorted = ids
    call sort_by_name(site%p%list, sorted)
    call add(b, '<ul class="chips">')
    do k = 1, size(sorted)
        call add(b, '<li>')
        call link_to(site, sorted(k), site%p%list%box(sorted(k))%e%name, b)
        call add(b, '</li>')
    end do
    call add(b, '</ul>'//achar(10))
end subroutine neighbour_chips

!> True if entity `id` has a graph titled `title`.
logical function has_graph(site, id, title)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    character(len=*), intent(in) :: title   !! the title
    integer :: k

    has_graph = .false.
    if (.not. allocated(site%graphs_of(id)%ids)) return
    do k = 1, size(site%graphs_of(id)%ids)
        associate (g => site%graphs%graphs(site%graphs_of(id)%ids(k)))
            if (g%title == title .and. len(g%svg) > 0) has_graph = .true.
        end associate
    end do
end function has_graph

!> Write the heading of a graph, at the level of the other sections of a page.
subroutine graph_heading(title, b)
    character(len=*), intent(in) :: title   !! the title
    type(buffer_t), intent(inout) :: b      !! the HTML

    call add(b, '<h2 id="'//section_id(title)//'">')
    call html_escape(title, b, .false.)
    call add(b, '</h2>'//achar(10))
end subroutine graph_heading

!> Write graph `k`.
subroutine write_graph(site, k, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: k                !! the graph
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (g => site%graphs%graphs(k))
        if (len(g%svg) == 0) return
        call graph_heading(g%title, b)
        call add(b, '<div class="graph">')
        call add(b, g%svg)
        call add(b, '</div>'//achar(10))
    end associate
end subroutine write_graph

!> Write a section of short entries that link to the pages of `ids`.
subroutine summary_section(site, title, ids, b)
    type(site_t), intent(in) :: site        !! the site
    character(len=*), intent(in) :: title   !! the heading
    integer, intent(in) :: ids(:)           !! the entities
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    if (size(ids) == 0) return
    call add(b, '<h2 id="'//section_id(title)//'">'//title//'</h2>'//achar(10))
    do k = 1, size(ids)
        call summary_item(site, ids(k), b)
    end do
end subroutine summary_section

!> An anchor for the section titled `title`.
pure function section_id(title) result(id)
    character(len=*), intent(in) :: title   !! the title
    character(len=len(title)) :: id
    integer :: k

    id = lowercase(title)
    do k = 1, len(id)
        if (id(k:k) == ' ') id(k:k) = '-'
    end do
end function section_id

!> Write a short entry for entity `id`.
subroutine summary_item(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (e => site%p%list%box(id)%e)
        call add(b, '<div class="item" id="')
        call anchor(site, id, b)
        call add(b, '"><h3>')
        call link_to(site, id, e%name, b)
        call procedure_badges(e, b)
        if (allocated(e%permission)) then
            if (e%permission /= 'public') call badge(e%permission, b)
        end if
        call add(b, '</h3>'//achar(10))
        select case (e%kind)
        case (E_SUBROUTINE, E_FUNCTION, E_MODPROC)
            call signature(site, id, b)
        end select
        if (allocated(e%summary)) call add(b, e%summary)
        call add(b, achar(10)//'</div>'//achar(10))
    end associate
end subroutine summary_item

!> Write the `use` statements of a unit.
subroutine uses_section(site, id, heading, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the unit
    logical, intent(in) :: heading          !! true to write the heading too
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (e => site%p%list%box(id)%e)
        if (.not. allocated(e%more%uses)) return
        if (size(e%more%uses) == 0) return
        if (heading) call add(b, '<h2 id="uses">Uses</h2>'//achar(10))
        call add(b, '<ul class="chips">')
        do k = 1, size(e%more%uses)
            call add(b, '<li>')
            call link_to(site, e%more%uses(k)%target, e%more%uses(k)%module, b)
            call add(b, '</li>')
        end do
        call add(b, '</ul>'//achar(10))
    end associate
end subroutine uses_section

!> Write what a procedure calls.
subroutine calls_section(site, id, heading, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the procedure
    logical, intent(in) :: heading          !! true to write the heading too
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (e => site%p%list%box(id)%e)
        if (.not. allocated(e%call_ids)) return
        if (size(e%call_ids) == 0) return
        if (heading) call add(b, '<h2 id="calls">Calls</h2>'//achar(10))
        call add(b, '<ul class="chips">')
        do k = 1, size(e%call_ids)
            call add(b, '<li>')
            call link_to(site, e%call_ids(k), site%p%list%box(e%call_ids(k))%e%name, b)
            call add(b, '</li>')
        end do
        call add(b, '</ul>'//achar(10))
    end associate
end subroutine calls_section

!> Write the source of entity `id`, if `source` is set, from the text of its file.
subroutine source_section(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the procedure or type
    type(buffer_t), intent(inout) :: b      !! the HTML

    if (.not. wants_source(site, id)) return
    associate (e => site%p%list%box(id)%e)
        if (e%source_first == 0) return
        call add(b, '<h2 id="source">Source</h2>'//achar(10)//'<details class="source"><summary>Show the code</summary>')
        call add(b, '<pre><code>')
        call highlight_fortran(site%p%list%box(e%file)%e%source_text(e%source_first:e%source_last), b, .false.)
        call add(b, '</code></pre></details>'//achar(10))
    end associate
end subroutine source_section

!> True if the source of entity `id` should be shown.
logical function wants_source(site, id)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: id           !! the entity
    logical :: flag, ok
    integer :: k

    wants_source = site%s%source
    associate (e => site%p%list%box(id)%e)
        do k = 1, size(e%meta)
            if (e%meta(k)%key /= 'source') cycle
            call parse_bool(e%meta(k)%values(1)%s, flag, ok)
            if (ok) wants_source = flag
        end do
    end associate
end function wants_source

!> Find the code of each procedure and type in the text of its file. Files are handled in parallel.
subroutine locate_sources(site)
    type(site_t), intent(inout) :: site     !! the site
    integer, allocatable :: ids(:), starts(:)
    integer :: k, n, runs, r, file

    allocate (ids(site%p%list%n), starts(site%p%list%n + 1))
    n = 0
    runs = 0
    file = 0
    do k = 1, site%p%list%n
        associate (e => site%p%list%box(k)%e)
            if (.not. is_procedure(e%kind) .and. e%kind /= E_TYPE) cycle
            if (allocated(e%external_url) .or. .not. e%visible) cycle
            if (.not. wants_source(site, k)) cycle
            n = n + 1
            ids(n) = k
            if (e%file /= file) then
                file = e%file
                runs = runs + 1
                starts(runs) = n
            end if
        end associate
    end do
    starts(runs + 1) = n + 1
    !$omp parallel do schedule(dynamic) num_threads(max(1, site%s%parallel)) default(shared) private(r)
    do r = 1, runs
        call locate_in_file(site, ids(starts(r):starts(r + 1) - 1))
    end do
    !$omp end parallel do
end subroutine locate_sources

!> Find the code of entities `ids`, all from one file. They are in source order, so each search starts
!> where the last one found something. Only the lines that name a construct are searched, as a name
!> that is not in the file, like those Fypp makes, would otherwise cost two readings of the file.
subroutine locate_in_file(site, ids)
    type(site_t), intent(inout) :: site     !! the site
    integer, intent(in) :: ids(:)           !! the entities
    character(len=:), allocatable :: low
    integer, allocatable :: starts(:), stops(:)
    integer :: k, cursor, found, first, last

    ! on the heap and lowered in place, as a whole file would not fit on the stack of a thread
    low = site%p%list%box(site%p%list%box(ids(1))%e%file)%e%source_text
    call lower_in_place(low)
    call construct_lines(low, starts, stops)
    cursor = 1
    do k = 1, size(ids)
        associate (e => site%p%list%box(ids(k))%e)
            select case (e%kind)
            case (E_SUBROUTINE)
                call find_source(low, starts, stops, cursor, 'subroutine', lowercase(e%name), found, first, last)
                if (first == 0 .and. cursor > 1) &
                    call find_source(low, starts, stops, 1, 'subroutine', lowercase(e%name), found, first, last)
            case (E_FUNCTION)
                call find_source(low, starts, stops, cursor, 'function', lowercase(e%name), found, first, last)
                if (first == 0 .and. cursor > 1) &
                    call find_source(low, starts, stops, 1, 'function', lowercase(e%name), found, first, last)
            case default
                call find_source(low, starts, stops, cursor, 'type', lowercase(e%name), found, first, last)
                if (first == 0 .and. cursor > 1) &
                    call find_source(low, starts, stops, 1, 'type', lowercase(e%name), found, first, last)
            end select
            if (first == 0) cycle
            e%source_first = first
            e%source_last = last
            cursor = found
        end associate
    end do
end subroutine locate_in_file

!> The lines of `low` that could start or end a procedure or type.
pure subroutine construct_lines(low, starts, stops)
    character(len=*), intent(in) :: low             !! the file, in lowercase
    integer, allocatable, intent(out) :: starts(:)  !! where each line starts
    integer, allocatable, intent(out) :: stops(:)   !! where each line ends
    integer :: pass, i, j, stop_at, n

    do pass = 1, 2
        n = 0
        i = 1
        do while (i <= len(low))
            j = index(low(i:), achar(10))
            stop_at = merge(len(low), i + j - 2, j == 0)
            if (index(low(i:stop_at), 'subroutine') > 0 .or. index(low(i:stop_at), 'function') > 0 .or. &
                index(low(i:stop_at), 'type') > 0) then
                n = n + 1
                if (pass == 2) then
                    starts(n) = i
                    stops(n) = stop_at
                end if
            end if
            if (j == 0) exit
            i = stop_at + 2
        end do
        if (pass == 1) allocate (starts(n), stops(n))
    end do
end subroutine construct_lines

!> Find the text of the construct `keyword name` in `low`, from its first line to its `end` line.
subroutine find_source(low, starts, stops, from, keyword, name, found, first, last)
    character(len=*), intent(in) :: low     !! the file, in lowercase
    integer, intent(in) :: starts(:)        !! where the lines to search start
    integer, intent(in) :: stops(:)         !! where they end
    integer, intent(in) :: from             !! the line to start from
    character(len=*), intent(in) :: keyword !! `subroutine`, `function` or `type`
    character(len=*), intent(in) :: name    !! the name, in lowercase
    integer, intent(out) :: found           !! the line where it starts
    integer, intent(out) :: first           !! where it starts, or 0
    integer, intent(out) :: last            !! where it ends
    integer :: k, at, comment

    first = 0
    last = 0
    found = 0
    do k = from, size(starts)
        associate (line => low(starts(k):stops(k)))
            if (index(line, name) == 0) cycle
            comment = index(line, '!')
            if (comment == 0) comment = len(line) + 1
            at = line_has(line(:comment - 1), keyword, name)
            if (at > 0 .and. first == 0 .and. .not. starts_word(adjustl(line), 'end')) then
                first = starts(k)
                found = k
            else if (first > 0 .and. at > 0 .and. starts_word(adjustl(line), 'end')) then
                last = stops(k)
                return
            end if
        end associate
    end do
    first = 0
end subroutine find_source

!> The position of `keyword` followed by `name` in `line`, or 0.
pure integer function line_has(line, keyword, name)
    character(len=*), intent(in) :: line    !! the line
    character(len=*), intent(in) :: keyword !! the keyword
    character(len=*), intent(in) :: name    !! the name
    integer :: k, n

    line_has = 0
    do k = 1, len(line) - len(keyword) + 1
        if (.not. word_at(line, k, keyword)) cycle
        n = k + len(keyword)
        do while (n <= len(line))
            if (index(' :,', line(n:n)) == 0) exit
            if (line(n:n) == ',') exit
            n = n + 1
        end do
        if (n <= len(line)) then
            if (word_at(line, n, name)) then
                line_has = k
                return
            end if
        end if
    end do
    if (keyword == 'type' .and. index(line, '::') > 0) then
        n = index(line, '::') + 2
        n = n + verify(line(n:)//'x', ' ') - 1
        if (word_at(line, n, name) .and. starts_word(adjustl(line), 'type')) line_has = 1
    end if
end function line_has

!> Render the page of entity `id`.
subroutine entity_page(site, id, html)
    type(site_t), intent(in) :: site                    !! the site
    integer, intent(in) :: id                           !! the entity
    character(len=:), allocatable, intent(out) :: html  !! the page
    type(buffer_t) :: b, side
    integer, allocatable :: variables(:), functions(:), subroutines(:), interfaces(:), abstracts(:), types(:)
    integer, allocatable :: bindings(:), finals(:), lists(:), blocks(:), implementations(:), enums(:)
    character(len=:), allocatable :: word, text
    integer :: here

    call children_of(site, id, 1, variables)
    call children_of(site, id, 2, functions)
    call children_of(site, id, 3, subroutines)
    call children_of(site, id, 4, interfaces)
    call children_of(site, id, 5, abstracts)
    call children_of(site, id, 6, types)
    call children_of(site, id, 7, bindings)
    call children_of(site, id, 8, finals)
    call children_of(site, id, 9, lists)
    call children_of(site, id, 10, blocks)
    call children_of(site, id, 11, enums)
    call children_of(site, id, 12, implementations)

    associate (e => site%p%list%box(id)%e)
        here = list_of(site, id)
        select case (e%kind)
        case (E_MODULE); word = 'module'
        case (E_SUBMODULE); word = 'submodule'
        case (E_PROGRAM); word = 'program'
        case (E_BLOCKDATA); word = 'block data'
        case (E_SUBROUTINE); word = 'subroutine'
        case (E_FUNCTION); word = 'function'
        case (E_MODPROC); word = 'module procedure'
        case (E_INTERFACE); word = 'interface'
        case (E_TYPE); word = 'type'
        case (E_NAMELIST); word = 'namelist'
        case default; word = 'file'
        end select
        if (e%iface == IFACE_EXPLICIT) word = 'interface'
        if (e%iface == IFACE_ABSTRACT) word = 'abstract interface'

        if (e%kind == E_TYPE) then
            call side_group(site, 'Components', variables, side)
        else
            call side_group(site, 'Variables', variables, side)
        end if
        call side_group(site, 'Interfaces', interfaces, side)
        call side_group(site, 'Abstract interfaces', abstracts, side)
        call side_group(site, 'Types', types, side)
        call side_group(site, 'Functions', functions, side)
        call side_group(site, 'Subroutines', subroutines, side)
        call side_group(site, 'Module procedures', implementations, side)
        call side_group(site, 'Bindings', bindings, side)
        call page_outline(site, id, side)
        call take(side, text)

        call page_head(site, display_name(e), here, '../', uses_math(site, id), b)
        call add(b, '<aside class="side">'//achar(10))
        if (len(text) > 0) call add(b, text)
        call site_side(site, here, '../', b)
        call add(b, '</aside>'//achar(10)//'<main><div class="inner">'//achar(10))
        call entity_title(site, id, word, b)

        select case (e%kind)
        case (E_SUBROUTINE, E_FUNCTION, E_MODPROC)
            call procedure_body(site, id, b)
        case (E_INTERFACE)
            call interface_body(site, id, b)
        case (E_TYPE)
            call type_body(site, id, variables, bindings, finals, b)
        case (E_NAMELIST)
            if (allocated(e%more%args)) then
                call add(b, '<h2>Variables</h2>'//achar(10)//'<ul class="chips">')
                call add_words(e%more%args, b)
                call add(b, '</ul>'//achar(10))
            end if
        case (E_FILE)
            call file_body(site, id, b)
        case default
            call unit_body(site, id, b)
        end select

        call graph_sections(site, id, b)
        if (e%kind /= E_FILE .and. e%kind /= E_INTERFACE .and. e%kind /= E_TYPE .and. e%kind /= E_NAMELIST) then
            if (size(variables) > 0) then
                call add(b, '<h2 id="variables">Variables</h2>'//achar(10))
                call argument_table(site, variables, .true., b)
            end if
            call enum_sections(site, enums, b)
            call common_sections(site, blocks, b)
            call summary_section(site, 'Interfaces', interfaces, b)
            call summary_section(site, 'Abstract interfaces', abstracts, b)
            call summary_section(site, 'Types', types, b)
            call summary_section(site, 'Namelists', lists, b)
            call summary_section(site, 'Functions', functions, b)
            call summary_section(site, 'Subroutines', subroutines, b)
            call summary_section(site, 'Module procedures', implementations, b)
        end if
        call page_source(site, id, b)
        call page_foot(site, b)
    end associate
    call take(b, html)
end subroutine entity_page

!> Write enumerations.
subroutine enum_sections(site, ids, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: ids(:)           !! the enumerations
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    if (size(ids) == 0) return
    call add(b, '<h2 id="enumerations">Enumerations</h2>'//achar(10))
    do k = 1, size(ids)
        call add(b, '<div class="item" id="')
        call anchor(site, ids(k), b)
        call add(b, '">'//achar(10))
        call argument_table(site, site%p%list%box(ids(k))%e%children, .true., b)
        call add(b, '</div>'//achar(10))
    end do
end subroutine enum_sections

!> Write common blocks.
subroutine common_sections(site, ids, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: ids(:)           !! the common blocks
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    if (size(ids) == 0) return
    call add(b, '<h2 id="common-blocks">Common blocks</h2>'//achar(10))
    do k = 1, size(ids)
        associate (e => site%p%list%box(ids(k))%e)
            call add(b, '<div class="item" id="')
            call anchor(site, ids(k), b)
            call add(b, '"><h3>')
            call html_escape(e%name, b, .false.)
            call add(b, '</h3>'//achar(10))
            if (allocated(e%html)) call add(b, e%html)
            call add(b, '<ul class="chips">')
            call add_words(e%more%args, b)
            call add(b, '</ul></div>'//achar(10))
        end associate
    end do
end subroutine common_sections

!> Write `words` as list items.
subroutine add_words(words, b)
    type(string_t), intent(in) :: words(:)  !! the words
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    do k = 1, size(words)
        call add(b, '<li>')
        call html_escape(words(k)%s, b, .false.)
        call add(b, '</li>')
    end do
end subroutine add_words

!> True if the documentation shown on the page of `id` has math.
logical function uses_math(site, id)
    type(site_t), intent(in) :: site    !! the site
    integer, intent(in) :: id           !! the entity
    integer :: k

    uses_math = has_math(site%p%list%box(id)%e)
    if (uses_math) return
    if (hidden_body(site, id) > 0) uses_math = has_math(site%p%list%box(hidden_body(site, id))%e)
    if (uses_math) return
    do k = 1, size(site%p%list%box(id)%e%children)
        uses_math = has_math(site%p%list%box(site%p%list%box(id)%e%children(k))%e)
        if (uses_math) return
    end do
end function uses_math

!> True if `e` has math in its documentation.
pure logical function has_math(e)
    type(entity_t), intent(in) :: e     !! the entity

    has_math = .false.
    if (allocated(e%html)) has_math = index(e%html, 'math/tex') > 0
end function has_math

!> Write what is specific to a module, submodule, program or block data unit.
subroutine unit_body(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the unit
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (e => site%p%list%box(id)%e)
        if (e%kind == E_SUBMODULE) then
            call add(b, '<div class="meta"><span>submodule of ')
            call link_to(site, e%ancestor_id, e%more%ancestor, b)
            call add(b, '</span></div>'//achar(10))
        end if
        if (.not. has_graph(site, id, 'Uses')) call uses_section(site, id, .true., b)
        if (allocated(e%more%descendants)) then
            call add(b, '<h2 id="submodules">Submodules</h2>'//achar(10)//'<ul class="chips">')
            do k = 1, size(e%more%descendants)
                call add(b, '<li>')
                call link_to(site, e%more%descendants(k), site%p%list%box(e%more%descendants(k))%e%name, b)
                call add(b, '</li>')
            end do
            call add(b, '</ul>'//achar(10))
        end if
        if (e%kind == E_PROGRAM .and. .not. has_graph(site, id, 'Calls')) call calls_section(site, id, .true., b)
    end associate
end subroutine unit_body

!> Write what is specific to a procedure.
subroutine procedure_body(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the procedure
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (e => site%p%list%box(id)%e)
        if (allocated(e%more%arg_ids)) then
            if (size(e%more%arg_ids) > 0) then
                call add(b, '<h2 id="arguments">Arguments</h2>'//achar(10))
                call argument_table(site, e%more%arg_ids, .false., b)
            end if
        end if
        if (e%result > 0) then
            call add(b, '<h2 id="result">Result</h2>'//achar(10))
            call argument_table(site, [e%result], .true., b)
        end if
        if (hidden_body(site, id) > 0) then
            ! the documentation of the body is on this page; say where the body is
            call add(b, '<div class="meta"><span>implemented in ')
            associate (unit => site%p%list%box(e%implements)%e%parent)
                call link_to(site, unit, site%p%list%box(unit)%e%name, b)
            end associate
            call add(b, '</span></div>'//achar(10))
        else if (e%implements > 0) then
            call add(b, '<div class="meta"><span>')
            call add(b, merge('implements  ', 'implemented ', e%kind == E_MODPROC))
            call link_to(site, e%implements, site%p%list%box(e%implements)%e%name, b)
            call add(b, '</span></div>'//achar(10))
        end if
        if (.not. has_graph(site, id, 'Uses')) call uses_section(site, id, .true., b)
        if (.not. has_graph(site, id, 'Calls')) call calls_section(site, id, .true., b)
    end associate
end subroutine procedure_body

!> Write what is specific to a generic interface: the procedures in it.
subroutine interface_body(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the interface
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer, allocatable :: specifics(:)
    logical :: forms
    integer :: k

    associate (e => site%p%list%box(id)%e)
        allocate (specifics(0))
        do k = 1, size(e%more%modproc_ids)
            if (e%more%modproc_ids(k) > 0) call push_int(specifics, e%more%modproc_ids(k))
        end do
        do k = 1, size(e%children)
            if (site%p%list%box(e%children(k))%e%kind == E_NONE) cycle
            if (allocated(site%p%list%box(e%children(k))%e%more%args)) call push_int(specifics, e%children(k))
        end do
        call write_call_forms(site%p%list, e%name, specifics, b, forms)

        call add(b, '<h2 id="procedures">Procedures</h2>'//achar(10))
        if (forms) then
            call add(b, '<details class="specifics"><summary>All '//trim(int_text(size(specifics)))// &
                     ' procedures</summary>'//achar(10))
        end if
        do k = 1, size(e%more%modproc_ids)
            if (e%more%modproc_ids(k) == 0) then
                call add(b, '<div class="item"><h3>')
                call html_escape(e%more%modprocs(k)%s, b, .false.)
                call add(b, '</h3></div>'//achar(10))
            else
                call summary_item(site, e%more%modproc_ids(k), b)
            end if
        end do
        do k = 1, size(e%children)
            if (site%p%list%box(e%children(k))%e%kind == E_NONE) cycle
            if (allocated(site%p%list%box(e%children(k))%e%more%args)) call summary_item(site, e%children(k), b)
        end do
        if (forms) call add(b, '</details>'//achar(10))
    end associate
end subroutine interface_body

!> Write what is specific to a derived type.
subroutine type_body(site, id, variables, bindings, finals, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the type
    integer, intent(in) :: variables(:)     !! its components
    integer, intent(in) :: bindings(:)      !! its bindings
    integer, intent(in) :: finals(:)        !! its final procedures
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer :: k

    associate (e => site%p%list%box(id)%e)
        if (allocated(e%more%extends)) then
            call add(b, '<div class="meta"><span>extends ')
            call link_to(site, e%extends_id, e%more%extends, b)
            call add(b, '</span></div>'//achar(10))
        end if
        if (size(e%attribs) > 0) then
            call add(b, '<ul class="chips">')
            call add_words(e%attribs, b)
            call add(b, '</ul>'//achar(10))
        end if
        if (size(variables) > 0) then
            call add(b, '<h2 id="components">Components</h2>'//achar(10))
            call argument_table(site, variables, .true., b)
        end if
        if (e%constructor > 0) then
            call add(b, '<h2 id="constructor">Constructor</h2>'//achar(10))
            call summary_item(site, e%constructor, b)
        end if
        if (size(bindings) > 0) then
            call add(b, '<h2 id="bindings">Type-bound procedures</h2>'//achar(10))
            call binding_table(site, bindings, b)
        end if
        if (size(finals) > 0) then
            call add(b, '<h2 id="final">Final procedures</h2>'//achar(10)//'<ul class="chips">')
            do k = 1, size(finals)
                call add(b, '<li>')
                call link_to(site, site%p%list%box(finals(k))%e%proto_id, site%p%list%box(finals(k))%e%name, b)
                call add(b, '</li>')
            end do
            call add(b, '</ul>'//achar(10))
        end if
        call type_uses_section(site, id, b)
    end associate
end subroutine type_body

!> Find where each type is used, once, so that the page of a type does not search the whole project. A
!> specific procedure counts as its generic, which often is the only one with a page. The methods of a
!> type are left out, as each of them takes it, and so are local variables.
subroutine find_type_uses(site)
    type(site_t), intent(inout) :: site     !! the site
    integer, allocatable :: generic_of(:)
    integer :: k, j, slots, parent, owner

    associate (list => site%p%list)
        allocate (site%use_slot(list%n), source=0)
        allocate (generic_of(list%n), source=0)
        slots = 0
        do k = 1, list%n
            associate (e => list%box(k)%e)
                if (e%kind == E_TYPE) then
                    slots = slots + 1
                    site%use_slot(k) = slots
                else if (e%kind == E_INTERFACE .and. allocated(e%more%modproc_ids)) then
                    do j = 1, size(e%more%modproc_ids)
                        if (e%more%modproc_ids(j) > 0) generic_of(e%more%modproc_ids(j)) = k
                    end do
                end if
            end associate
        end do
        allocate (site%type_uses(slots))
        do k = 1, list%n
            associate (v => list%box(k)%e)
                if (v%kind /= E_VARIABLE .or. v%proto_id == 0 .or. v%parent == 0) cycle
                if (site%use_slot(v%proto_id) == 0) cycle
                parent = v%parent
                associate (uses => site%type_uses(site%use_slot(v%proto_id)), up => list%box(parent)%e)
                    select case (up%kind)
                    case (E_TYPE)
                        if (parent /= v%proto_id .and. is_shown(list, parent)) call add_use(uses%components, parent)
                    case (E_MODULE, E_SUBMODULE)
                        if (is_shown(list, k)) call add_use(uses%variables, k)
                    case (E_SUBROUTINE, E_FUNCTION, E_MODPROC)
                        if (up%bound_to == v%proto_id) cycle
                        owner = parent
                        if (list%box(up%parent)%e%kind == E_INTERFACE) owner = up%parent
                        if (generic_of(parent) > 0) owner = generic_of(parent)
                        if (.not. is_shown(list, owner)) cycle
                        if (up%result == k) then
                            call add_use(uses%results, owner)
                        else if (allocated(up%more%arg_ids)) then
                            if (findloc(up%more%arg_ids, k, 1) > 0) call add_use(uses%arguments, owner)
                        end if
                    end select
                end associate
            end associate
        end do
        do k = 1, slots
            associate (uses => site%type_uses(k))
                call sort_by_name(list, uses%arguments)
                call sort_by_name(list, uses%results)
                call sort_by_name(list, uses%components)
                call sort_by_name(list, uses%variables)
            end associate
        end do
    end associate
end subroutine find_type_uses

!> Add entity `id` to `ids`, once.
subroutine add_use(ids, id)
    integer, allocatable, intent(inout) :: ids(:)   !! the entities
    integer, intent(in) :: id                       !! the entity

    if (allocated(ids)) then
        if (any(ids == id)) return
    end if
    call push_int(ids, id)
end subroutine add_use

!> Put `ids`, if there are any, in the order of their names.
subroutine sort_by_name(list, ids)
    type(entity_list_t), intent(in) :: list         !! the entities
    integer, allocatable, intent(inout) :: ids(:)   !! the entities to sort
    integer :: k, j, value

    if (.not. allocated(ids)) return
    do k = 2, size(ids)
        value = ids(k)
        j = k - 1
        do while (j >= 1)
            if (lowercase(list%box(ids(j))%e%name) <= lowercase(list%box(value)%e%name)) exit
            ids(j + 1) = ids(j)
            j = j - 1
        end do
        ids(j + 1) = value
    end do
end subroutine sort_by_name

!> Write where type `id` is used, if it is.
subroutine type_uses_section(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the type
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (uses => site%type_uses(site%use_slot(id)))
        if (.not. (allocated(uses%arguments) .or. allocated(uses%results) .or. &
                   allocated(uses%components) .or. allocated(uses%variables))) return
        call add(b, '<h2 id="uses-of-type">Where it is used</h2>'//achar(10))
        call use_group(site, 'Argument of', uses%arguments, b)
        call use_group(site, 'Returned by', uses%results, b)
        call use_group(site, 'Component of', uses%components, b)
        call use_group(site, 'Module variables', uses%variables, b)
    end associate
end subroutine type_uses_section

!> Write a group of `ids`, collapsed if it is long.
subroutine use_group(site, title, ids, b)
    type(site_t), intent(in) :: site        !! the site
    character(len=*), intent(in) :: title   !! the heading
    integer, allocatable, intent(in) :: ids(:)  !! the entities, if any
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer, parameter :: LONG = 24
    character(len=:), allocatable :: letter, word
    integer :: k

    if (.not. allocated(ids)) return
    call add(b, '<h3>'//title//' ('//trim(int_text(size(ids)))//')</h3>'//achar(10))
    if (size(ids) > LONG) call add(b, '<details class="specifics"><summary>Show all</summary>'//achar(10))
    call add(b, '<ul class="chips kinds">')
    do k = 1, size(ids)
        associate (e => site%p%list%box(ids(k))%e)
            call icon_of(e, letter, word)
            call add(b, '<li class="ico-'//letter_class(letter)//'">')
            call side_icon(e, b)
            call link_to(site, ids(k), e%name, b)
            call add(b, '</li>')
        end associate
    end do
    call add(b, '</ul>'//achar(10))
    if (size(ids) > LONG) call add(b, '</details>'//achar(10))
end subroutine use_group

!> Write what is specific to a source file.
subroutine file_body(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the file
    type(buffer_t), intent(inout) :: b      !! the HTML
    integer, allocatable :: units(:)
    integer :: k

    associate (e => site%p%list%box(id)%e)
        allocate (units(0))
        do k = 1, size(e%children)
            associate (c => site%p%list%box(e%children(k))%e)
                select case (c%kind)
                case (E_MODULE, E_SUBMODULE, E_PROGRAM, E_BLOCKDATA, E_SUBROUTINE, E_FUNCTION)
                    call push_int(units, e%children(k))
                end select
            end associate
        end do
        call summary_section(site, 'Contents', units, b)
    end associate
end subroutine file_body

!> Write the source of entity `id`, the last section of its page: all of it for a file, which is
!> numbered, and folded away for anything else.
subroutine page_source(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (e => site%p%list%box(id)%e)
        select case (e%kind)
        case (E_FILE)
            if (site%s%incl_src .and. allocated(e%source_text)) then
                call add(b, '<h2 id="source">Source</h2>'//achar(10)//'<pre class="source"><code>')
                call highlight_fortran(e%source_text, b, .true.)
                call add(b, '</code></pre>'//achar(10))
            end if
        case (E_SUBROUTINE, E_FUNCTION, E_MODPROC, E_TYPE)
            call source_section(site, id, b)
        end select
    end associate
end subroutine page_source

!> Write the sections of a procedure page, for the sidebar.
subroutine page_outline(site, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (e => site%p%list%box(id)%e)
        if (.not. is_procedure(e%kind) .and. e%kind /= E_MODPROC) return
        call add(b, '<h4>On this page</h4><ul>')
        if (allocated(e%more%arg_ids)) then
            if (size(e%more%arg_ids) > 0) call add(b, '<li><a href="#arguments">Arguments</a></li>')
        end if
        if (e%result > 0) call add(b, '<li><a href="#result">Result</a></li>')
        if (allocated(e%more%uses)) then
            if (size(e%more%uses) > 0) call add(b, '<li><a href="#uses">Uses</a></li>')
        end if
        if (allocated(e%call_ids)) then
            if (size(e%call_ids) > 0) call add(b, '<li><a href="#calls">Calls</a></li>')
        end if
        if (e%source_first > 0) call add(b, '<li><a href="#source">Source</a></li>')
        call add(b, '</ul>'//achar(10))
    end associate
end subroutine page_outline

!> Write the links to the list pages, for the sidebar. The list of the page is empty here, as it could
!> be far too long to go in each page: `fdoc.js` fills it from `sidebar.js`. Pages outside the lists, which
!> have no place in one, show the start of each list.
subroutine site_side(site, here, prefix, b)
    type(site_t), intent(in) :: site            !! the site
    integer, intent(in) :: here                 !! the current list page, or 0
    character(len=*), intent(in) :: prefix      !! from the page to the root of the site
    type(buffer_t), intent(inout) :: b          !! the HTML
    character(len=:), allocatable :: url
    integer :: k, shown, limit

    do k = 1, NUM_LISTS
        if (size(site%members(k)%ids) == 0) cycle
        call add(b, '<h4><a href="'//prefix//'lists/'//trim(LIST_FILE(k))//'.html">'//trim(LIST_TITLE(k))//'</a></h4>')
        if (k == here) call add(b, '<ul data-list="'//trim(LIST_FILE(k))//'"></ul>'//achar(10))
        if (here /= 0) cycle
        call add(b, '<ul>')
        limit = min(site%s%max_frontpage_items, size(site%members(k)%ids))
        do shown = 1, limit
            associate (id => site%members(k)%ids(shown))
                call entity_url(site%p%list, id, url)
                call add(b, '<li><a href="'//prefix)
                call html_escape(url, b, .true.)
                call add(b, '">')
                call side_icon(site%p%list%box(id)%e, b)
                call html_escape(display_name(site%p%list%box(id)%e), b, .false.)
                call add(b, '</a></li>')
            end associate
        end do
        if (limit < size(site%members(k)%ids)) then
            call add(b, '<li><a class="more" href="'//prefix//'lists/'//trim(LIST_FILE(k))//'.html">All '// &
                     trim(int_text(size(site%members(k)%ids)))//'&hellip;</a></li>')
        end if
        call add(b, '</ul>'//achar(10))
    end do
end subroutine site_side

!> The script holding the entries of the lists of the sidebar, for `fdoc.js`.
subroutine sidebar_script(site, text)
    type(site_t), intent(in) :: site                    !! the site
    character(len=:), allocatable, intent(out) :: text  !! the script
    type(buffer_t) :: b, entry
    character(len=:), allocatable :: url, html
    logical :: first
    integer :: k, j

    call add(b, 'window.FDOC_SIDEBAR={')
    first = .true.
    do k = 1, NUM_LISTS
        if (size(site%members(k)%ids) == 0) cycle
        if (.not. first) call add(b, ',')
        first = .false.
        call add(b, "'"//trim(LIST_FILE(k))//"':'")
        do j = 1, size(site%members(k)%ids)
            associate (id => site%members(k)%ids(j))
                call entity_url(site%p%list, id, url)
                call add(entry, '<li><a href="@')
                call html_escape(url, entry, .true.)
                call add(entry, '">')
                call side_icon(site%p%list%box(id)%e, entry)
                call html_escape(display_name(site%p%list%box(id)%e), entry, .false.)
                call add(entry, '</a></li>')
                call take(entry, html)
                call add_js_text(html, b)
            end associate
        end do
        call add(b, "'")
    end do
    call add(b, '};'//achar(10))
    call take(b, text)
end subroutine sidebar_script

!> Append `text` for use in a JavaScript string in single quotes.
subroutine add_js_text(text, b)
    character(len=*), intent(in) :: text    !! the text
    type(buffer_t), intent(inout) :: b      !! the script
    integer :: k

    do k = 1, len(text)
        if (text(k:k) == "'" .or. text(k:k) == '\') call add(b, '\')
        call add(b, text(k:k))
    end do
end subroutine add_js_text

!> Write the columns of a list page between the name and the description.
subroutine list_columns(site, which, id, b)
    type(site_t), intent(in) :: site        !! the site
    integer, intent(in) :: which            !! the list
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML

    associate (e => site%p%list%box(id)%e)
        select case (which)
        case (LIST_MODULES, LIST_PROGRAMS, LIST_BLOCKDATA)
            call location(e%file)
        case (LIST_PROCEDURES)
            call location(e%parent)
            call add(b, '<td>')
            select case (e%kind)
            case (E_SUBROUTINE, E_FUNCTION)
                if (e%iface /= IFACE_NONE) then
                    call badge('interface', b)
                else
                    call badge(merge('function  ', 'subroutine', e%kind == E_FUNCTION), b)
                end if
            case (E_INTERFACE)
                call badge('interface', b)
            case default
                call badge('procedure', b)
            end select
            call procedure_badges(e, b)
            call add(b, '</td>')
        case (LIST_ABSINT, LIST_NAMELISTS)
            call location(e%parent)
        case (LIST_TYPES)
            call location(e%parent)
            call add(b, '<td class="link">')
            if (allocated(e%more%extends)) call link_to(site, e%extends_id, e%more%extends, b)
            call add(b, '</td>')
        end select
    end associate

contains

    !> Write a column linking to entity `where`.
    subroutine location(where)
        integer, intent(in) :: where    !! the entity

        call add(b, '<td class="link">')
        if (where > 0) call link_to(site, where, display_name(site%p%list%box(where)%e), b)
        call add(b, '</td>')
    end subroutine location

end subroutine list_columns

!> Write the types of the project as a tree, by what they extend, if any type extends another. A type that
!> extends one from elsewhere is at the top, with what it extends.
subroutine type_tree(site, b)
    type(site_t), intent(in) :: site        !! the site
    type(buffer_t), intent(inout) :: b      !! the HTML
    type(index_list_t), allocatable :: children(:)
    integer, allocatable :: slot(:), roots(:)
    integer :: k, parent

    associate (types => site%members(LIST_TYPES)%ids, list => site%p%list)
        allocate (slot(list%n), source=0)
        do k = 1, size(types)
            slot(types(k)) = k
        end do
        allocate (children(size(types)), roots(0))
        do k = 1, size(types)
            parent = list%box(types(k))%e%extends_id
            if (parent > 0) then
                if (slot(parent) > 0 .and. parent /= types(k)) then
                    call push_int(children(slot(parent))%ids, types(k))
                    cycle
                end if
            end if
            call push_int(roots, types(k))
        end do
        if (size(roots) == size(types)) return
        call add(b, '<h2 id="hierarchy">Hierarchy</h2>'//achar(10)//'<ul class="tree types">')
        call sort_by_name(list, roots)
        do k = 1, size(roots)
            call tree_node(roots(k), 1)
        end do
        call add(b, '</ul>'//achar(10)//'<h2 id="all-types">All types</h2>'//achar(10))
    end associate

contains

    !> Write type `id` and what extends it.
    recursive subroutine tree_node(id, depth)
        integer, intent(in) :: id       !! the type
        integer, intent(in) :: depth    !! how deep it is, against a loop in what extends what

        associate (e => site%p%list%box(id)%e, here => slot(id))
            call add(b, '<li>')
            if (allocated(children(here)%ids) .and. depth < MAX_NESTING) call add(b, '<details open><summary>')
            call side_icon(e, b)
            call link_to(site, id, e%name, b)
            if (e%extends_id > 0 .and. depth == 1 .and. allocated(e%more%extends)) then
                call add(b, ' <span class="where">extends ')
                call link_to(site, e%extends_id, e%more%extends, b)
                call add(b, '</span>')
            end if
            if (allocated(children(here)%ids) .and. depth < MAX_NESTING) then
                call add(b, '</summary><ul>')
                call sort_by_name(site%p%list, children(here)%ids)
                block
                    integer :: j

                    do j = 1, size(children(here)%ids)
                        call tree_node(children(here)%ids(j), depth + 1)
                    end do
                end block
                call add(b, '</ul></details>')
            end if
            call add(b, '</li>')
        end associate
    end subroutine tree_node

end subroutine type_tree

!> Render the index of all names: everything in the lists, from A to Z, with what each is and where.
subroutine names_page(site, html)
    type(site_t), intent(in) :: site                    !! the site
    character(len=:), allocatable, intent(out) :: html  !! the page
    type(buffer_t) :: b
    type(string_t), allocatable :: keys(:)
    integer, allocatable :: ids(:), order(:)
    character(len=:), allocatable :: url, letter, word
    character :: initial, previous
    integer :: k, which, n

    n = sum([(size(site%members(which)%ids), which=1, NUM_LISTS)])
    allocate (ids(n), keys(n))
    n = 0
    do which = 1, NUM_LISTS
        do k = 1, size(site%members(which)%ids)
            n = n + 1
            ids(n) = site%members(which)%ids(k)
            keys(n)%s = lowercase(display_name(site%p%list%box(ids(n))%e))
        end do
    end do
    call sorted_order(keys, order)

    call page_head(site, 'Index', HERE_INDEX, '../', .false., b)
    call add(b, '<aside class="side">'//achar(10))
    call site_side(site, 0, '../', b)
    call add(b, '</aside>'//achar(10)//'<main><div class="inner">'//achar(10)//'<h1>Index</h1>'//achar(10))
    call add(b, '<nav class="letters">')
    previous = ' '
    do k = 1, n
        initial = index_letter(keys(order(k))%s)
        if (initial == previous) cycle
        call add(b, '<a href="#letter-'//initial//'">'//upper_letter(initial)//'</a>')
        previous = initial
    end do
    call add(b, '</nav>'//achar(10))
    previous = ' '
    do k = 1, n
        initial = index_letter(keys(order(k))%s)
        if (initial /= previous) then
            if (previous /= ' ') call add(b, '</ul>'//achar(10))
            call add(b, '<h2 id="letter-'//initial//'">'//upper_letter(initial)//'</h2>'//achar(10)//'<ul class="names">')
            previous = initial
        end if
        associate (id => ids(order(k)), e => site%p%list%box(ids(order(k)))%e)
            call entity_url(site%p%list, id, url)
            call add(b, '<li><a href="../')
            call html_escape(url, b, .true.)
            call add(b, '">')
            call side_icon(e, b)
            call html_escape(display_name(e), b, .false.)
            call add(b, '</a>')
            call icon_of(e, letter, word)
            call add(b, ' <span class="where">'//word)
            if (e%parent > 0) then
                if (site%p%list%box(e%parent)%e%kind /= E_NONE) then
                    call add(b, ' in ')
                    call link_to(site, e%parent, display_name(site%p%list%box(e%parent)%e), b)
                end if
            end if
            call add(b, '</span></li>')
        end associate
    end do
    if (n > 0) call add(b, '</ul>'//achar(10))
    call page_foot(site, b)
    call take(b, html)

contains

    !> The group of a name in the index: its first letter, or `_` for anything else.
    pure character function index_letter(key)
        character(len=*), intent(in) :: key     !! the name, in lowercase

        index_letter = '_'
        if (len(key) == 0) return
        if (key(1:1) >= 'a' .and. key(1:1) <= 'z') index_letter = key(1:1)
    end function index_letter

    !> The heading of a group: its letter in capitals, or `#`.
    pure character function upper_letter(initial)
        character, intent(in) :: initial    !! the group

        upper_letter = '#'
        if (initial /= '_') upper_letter = achar(iachar(initial) - 32)
    end function upper_letter

end subroutine names_page

!> The order of `keys`, sorted, with equal keys in the order given: a merge sort, as an index can be long.
subroutine sorted_order(keys, order)
    type(string_t), intent(in) :: keys(:)           !! what to sort by
    integer, allocatable, intent(out) :: order(:)   !! the positions of `keys`, in order
    integer, allocatable :: other(:)
    integer :: width, left, middle, right, i, j, k, n

    n = size(keys)
    allocate (order(n), other(n))
    order = [(k, k=1, n)]
    width = 1
    do while (width < n)
        left = 1
        do while (left <= n)
            middle = min(left + width - 1, n)
            right = min(left + 2*width - 1, n)
            i = left
            j = middle + 1
            do k = left, right
                if (j > right) then
                    other(k) = order(i)
                    i = i + 1
                else if (i > middle) then
                    other(k) = order(j)
                    j = j + 1
                else if (keys(order(j))%s < keys(order(i))%s) then
                    other(k) = order(j)
                    j = j + 1
                else
                    other(k) = order(i)
                    i = i + 1
                end if
            end do
            left = left + 2*width
        end do
        order = other
        width = 2*width
    end do
end subroutine sorted_order

!> Render list page `which`.
subroutine list_page(site, which, html)
    type(site_t), intent(in) :: site                    !! the site
    integer, intent(in) :: which                        !! the list
    character(len=:), allocatable, intent(out) :: html  !! the page
    type(buffer_t) :: b
    character(len=:), allocatable :: url
    logical :: described
    integer :: k

    call page_head(site, trim(LIST_TITLE(which)), which, '../', .false., b)
    call add(b, '<aside class="side">'//achar(10))
    call site_side(site, which, '../', b)
    call add(b, '</aside>'//achar(10)//'<main><div class="inner">'//achar(10))
    call add(b, '<h1>'//trim(LIST_TITLE(which))//'</h1>'//achar(10))
    if (which == LIST_TYPES) call type_tree(site, b)
    if (which == LIST_FILES) then
        call add(b, '<table class="listing sortable">'//achar(10)//'<thead><tr><th>'//trim(LIST_NOUN(which))//'</th>')
    else
        call add(b, '<table class="listing">'//achar(10)//'<thead><tr><th>'//trim(LIST_NOUN(which))//'</th>')
    end if
    select case (which)
    case (LIST_FILES)
        call add(b, '<th class="num">Lines</th><th class="num">Code</th><th class="num">Comments</th>')
        call add(b, '<th class="num">Blank</th><th class="num">Size</th><th class="num">Procedures</th>')
    case (LIST_MODULES, LIST_PROGRAMS, LIST_BLOCKDATA)
        call add(b, '<th>Source File</th>')
    case (LIST_PROCEDURES)
        call add(b, '<th>Location</th><th>Type</th>')
    case (LIST_ABSINT, LIST_NAMELISTS)
        call add(b, '<th>Location</th>')
    case (LIST_TYPES)
        call add(b, '<th>Location</th><th>Extends</th>')
    end select
    ! what a file is about is rarely given, and the list of files is wide
    described = which /= LIST_FILES
    do k = 1, size(site%members(which)%ids)
        if (described) exit
        if (allocated(site%p%list%box(site%members(which)%ids(k))%e%summary)) then
            described = len_trim(site%p%list%box(site%members(which)%ids(k))%e%summary) > 0
        end if
    end do
    if (described) call add(b, '<th>Description</th>')
    call add(b, '</tr></thead>'//achar(10)//'<tbody>'//achar(10))
    do k = 1, size(site%members(which)%ids)
        associate (id => site%members(which)%ids(k))
            call entity_url(site%p%list, id, url)
            call add(b, '<tr><td><a href="../')
            call html_escape(url, b, .true.)
            call add(b, '">')
            call html_escape(display_name(site%p%list%box(id)%e), b, .false.)
            call add(b, '</a></td>')
            call list_columns(site, which, id, b)
            if (which == LIST_FILES) call stats_cells(site%file_stats(k), b)
            if (described) then
                call add(b, '<td>')
                if (allocated(site%p%list%box(id)%e%summary)) call add(b, site%p%list%box(id)%e%summary)
                call add(b, '</td>')
            end if
            call add(b, '</tr>'//achar(10))
        end associate
    end do
    call add(b, '</tbody>'//achar(10))
    if (which == LIST_FILES) call stats_total(site, described, b)
    call add(b, '</table>'//achar(10))
    do k = 1, size(site%graphs%graphs)
        if (site%graphs%graphs(k)%list_page == which) call write_graph(site, k, b)
    end do
    call page_foot(site, b)
    call take(b, html)
end subroutine list_page

!> Render the front page.
subroutine index_page(site, front, html)
    type(site_t), intent(in) :: site                    !! the site
    character(len=*), intent(in) :: front               !! the project file's text as HTML
    character(len=:), allocatable, intent(out) :: html  !! the page
    type(buffer_t) :: b
    character(len=:), allocatable :: name
    integer :: k

    associate (s => site%s)
        call page_head(site, s%project, 0, '', index(front, 'math/tex') > 0, b)
        call add(b, '<aside class="side">'//achar(10))
        call site_side(site, 0, '', b)
        call add(b, '</aside>'//achar(10)//'<main><div class="inner">'//achar(10))
        call logo_name(s, name)
        if (len(name) > 0) then
            call add(b, '<div class="hero with-logo">'//achar(10)//'<img class="hero-logo" src="')
            call html_escape(name, b, .true.)
            call add(b, '" alt="">'//achar(10))
        else
            call add(b, '<div class="hero">'//achar(10))
        end if
        call add(b, '<h1>')
        call html_escape(s%project, b, .false.)
        call add(b, '</h1>'//achar(10))
        if (allocated(s%summary)) then
            call add(b, '<div class="lead">')
            call html_escape(s%summary, b, .false.)
            call add(b, '</div>'//achar(10))
        end if
        call add(b, '<div class="project-links">')
        call icon_link(s%project_github, 'github', 'Source on GitHub', b)
        call icon_link(s%project_gitlab, 'gitlab', 'Source on GitLab', b)
        call icon_link(s%project_bitbucket, 'bitbucket', 'Source on Bitbucket', b)
        call icon_link(s%project_sourceforge, 'sourceforge', 'Source on SourceForge', b)
        call icon_link(s%project_website, 'website', 'Project website', b)
        call icon_link(s%project_download, 'download', 'Download', b)
        call add(b, '</div>'//achar(10)//'<div class="meta">')
        if (allocated(s%author)) then
            call add(b, '<span>')
            call html_escape(s%author, b, .false.)
            call add(b, '</span>')
        end if
        call add(b, '<span class="author-links">')
        call icon_link(s%github, 'github', 'Author on GitHub', b)
        call icon_link(s%gitlab, 'gitlab', 'Author on GitLab', b)
        call icon_link(s%bitbucket, 'bitbucket', 'Author on Bitbucket', b)
        call icon_link(s%linkedin, 'linkedin', 'Author on LinkedIn', b)
        call icon_link(s%twitter, 'twitter', 'Author on X', b)
        call icon_link(s%facebook, 'facebook', 'Author on Facebook', b)
        call icon_link(s%website, 'website', 'Author website', b)
        if (allocated(s%email)) then
            call add(b, '<a class="icon-link icon-email" title="Email the author" aria-label="Email the author" href="mailto:')
            call html_escape(s%email, b, .true.)
            call add(b, '"></a>')
        end if
        call add(b, '</span>')
        call add(b, '</div>'//achar(10)//'<div class="grid">')
        do k = 1, NUM_LISTS
            if (size(site%members(k)%ids) == 0) cycle
            call add(b, '<a class="card" href="lists/'//trim(LIST_FILE(k))//'.html"><b>'// &
                     trim(int_text(size(site%members(k)%ids)))//'</b><span>'//trim(LIST_TITLE(k))//'</span></a>')
        end do
        call add(b, '</div>'//achar(10)//'</div>'//achar(10)//'<div class="doc">'//achar(10))
        call add(b, front)
        call add(b, achar(10)//'</div>'//achar(10))
        call page_foot(site, b)
    end associate
    call take(b, html)
end subroutine index_page

!> Write an icon linking to `url`, if it is set.
subroutine icon_link(url, icon, label, b)
    character(len=:), allocatable, intent(in) :: url    !! the address
    character(len=*), intent(in) :: icon                !! the icon
    character(len=*), intent(in) :: label               !! what it links to
    type(buffer_t), intent(inout) :: b                  !! the HTML

    if (.not. allocated(url)) return
    call add(b, '<a class="icon-link icon-'//icon//'" title="'//label//'" aria-label="'//label//'" href="')
    call html_escape(url, b, .true.)
    call add(b, '"></a>')
end subroutine icon_link

!> The end of every page.
subroutine make_footer(s, footer)
    type(settings_t), intent(in) :: s                   !! the settings
    character(len=:), allocatable, intent(out) :: footer !! the HTML
    type(buffer_t) :: b

    call add(b, '<footer>')
    call html_escape(s%project, b, .false.)
    if (allocated(s%author)) then
        call add(b, ' was developed by ')
        call html_escape(s%author, b, .false.)
    end if
    if (len(s%license) > 0) then
        call add(b, '. ')
        call license_text(s%license, b)
    end if
    if (s%print_creation_date) then
        call add(b, '. Generated on ')
        call html_escape(s%creation_date, b, .false.)
    end if
    call add(b, '. Documentation generated by <a href="https://github.com/jacobwilliams/fdoc">fdoc</a>.</footer>'//achar(10))
    call take(b, footer)
end subroutine make_footer

!> Write the name of license `license`, linked if it is a known one.
subroutine license_text(license, b)
    character(len=*), intent(in) :: license !! the setting
    type(buffer_t), intent(inout) :: b      !! the HTML
    character(len=:), allocatable :: url, name

    select case (lowercase(license))
    case ('by'); name = 'CC BY'; url = 'http://creativecommons.org/licenses/by/4.0/'
    case ('by-nd'); name = 'CC BY-ND'; url = 'http://creativecommons.org/licenses/by-nd/4.0/'
    case ('by-sa'); name = 'CC BY-SA'; url = 'http://creativecommons.org/licenses/by-sa/4.0/'
    case ('by-nc'); name = 'CC BY-NC'; url = 'http://creativecommons.org/licenses/by-nc/4.0/'
    case ('by-nc-nd'); name = 'CC BY-NC-ND'; url = 'http://creativecommons.org/licenses/by-nc-nd/4.0/'
    case ('by-nc-sa'); name = 'CC BY-NC-SA'; url = 'http://creativecommons.org/licenses/by-nc-sa/4.0/'
    case ('gpl'); name = 'GNU GPL'; url = 'http://www.gnu.org/licenses/gpl'
    case ('lgpl'); name = 'GNU LGPL'; url = 'http://www.gnu.org/licenses/lgpl'
    case ('agpl'); name = 'GNU AGPL'; url = 'http://www.gnu.org/licenses/agpl'
    case ('gfdl'); name = 'GNU FDL'; url = 'http://www.gnu.org/licenses/old-licenses/fdl-1.2.en.html'
    case ('opl'); name = 'Open Publication License'; url = 'http://opencontent.org/openpub/'
    case ('pdl'); name = 'Public Documentation License'; url = 'http://www.openoffice.org/licenses/PDL.html'
    case ('bsd'); name = 'FreeBSD Documentation License'; url = 'http://www.freebsd.org/copyright/freebsd-doc-license.html'
    case ('isc'); name = 'ISC'; url = 'https://opensource.org/licenses/ISC'
    case ('mit'); name = 'MIT'; url = 'https://opensource.org/licenses/MIT'
    case default
        call html_escape(license, b, .false.)
        return
    end select
    call add(b, '<a rel="license" href="'//url//'">'//name//'</a>')
end subroutine license_text

!> Create the directories of the static pages.
subroutine page_directories(directory, pages)
    character(len=*), intent(in) :: directory   !! the output directory
    type(page_t), intent(in) :: pages(:)        !! the pages
    integer :: k, at, status

    do k = 1, size(pages)
        at = last_separator(pages(k)%path)
        if (at > 0) call make_directory(directory//'/page/'//pages(k)%path(:at - 1), status)
    end do
end subroutine page_directories

!> Copy the files and directories that static pages list.
subroutine copy_page_files(s, directory, pages)
    type(settings_t), intent(in) :: s           !! the settings
    character(len=*), intent(in) :: directory   !! the output directory
    type(page_t), intent(in) :: pages(:)        !! the pages
    integer :: k, j, status

    do k = 1, size(pages)
        do j = 1, size(pages(k)%copies)
            call copy_path(s%page_dir//'/'//pages(k)%copies(j)%s, directory//'/page/'//pages(k)%copies(j)%s, status)
        end do
    end do
end subroutine copy_page_files

!> Render the pages of entities `ids` and write them, in parallel. A page is not kept once it is
!> written, as together they can be larger than everything else in memory.
subroutine write_entity_pages(site, ids, directory, count, stat, errmsg)
    type(site_t), intent(in) :: site                    !! the site
    integer, intent(in) :: ids(:)                       !! the entities
    character(len=*), intent(in) :: directory           !! the directory of the site
    integer, intent(out) :: count                       !! how many pages were written
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg !! what is wrong
    integer, allocatable :: results(:)
    integer :: k

    allocate (results(size(ids)))
    call progress_start('Rendering pages', size(ids))
    !$omp parallel do schedule(dynamic) num_threads(max(1, site%s%parallel)) default(shared) private(k)
    do k = 1, size(ids)
        call write_entity_page(site, ids(k), directory, results(k))
        call progress_tick()
    end do
    !$omp end parallel do
    call progress_finish()
    count = size(ids)
    stat = 0
    errmsg = ''
    do k = 1, size(ids)
        if (results(k) == 0) cycle
        stat = results(k)
        errmsg = 'could not write the page of '//site%p%list%box(ids(k))%e%name
        return
    end do
end subroutine write_entity_pages

!> Render the page of entity `id` and write it.
subroutine write_entity_page(site, id, directory, stat)
    type(site_t), intent(in) :: site            !! the site
    integer, intent(in) :: id                   !! the entity
    character(len=*), intent(in) :: directory   !! the directory of the site
    integer, intent(out) :: stat                !! nonzero on failure
    character(len=:), allocatable :: html, url, errmsg

    call entity_page(site, id, html)
    call entity_url(site%p%list, id, url)
    call write_whole_file(directory//'/'//url, html, stat, errmsg)
end subroutine write_entity_page

!> Write the whole site.
subroutine write_site(p, s, front, pages_in, count, stat, errmsg)
    type(project_t), intent(inout), target :: p         !! the project
    type(settings_t), intent(in), target :: s           !! the settings
    character(len=*), intent(in) :: front               !! the project file's text as HTML
    type(page_t), intent(in), target :: pages_in(:)     !! the static pages
    integer, intent(out) :: count                       !! how many pages were written
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg !! what is wrong
    type(site_t) :: site
    type(string_t), allocatable :: pages(:), paths(:)
    integer, allocatable :: ids(:)
    character(len=:), allocatable :: url, directory
    type(buffer_t) :: b
    integer :: k, n, extra

    site%p => p
    site%s => s
    site%pages => pages_in
    call collect_lists(site)
    call make_footer(s, site%footer)
    call locate_sources(site)
    call measure_files(site)
    call find_type_uses(site)

    allocate (ids(0))
    do k = 1, p%list%n
        if (has_page(site, k)) call push_int(ids, k)
    end do
    n = size(ids)
    call build_graphs(p, s, ids, [LIST_FILES, LIST_MODULES, LIST_PROCEDURES, LIST_TYPES], site%graphs, stat, errmsg)
    if (stat /= 0) return
    allocate (site%graphs_of(p%list%n))
    do k = 1, size(site%graphs%graphs)
        if (site%graphs%graphs(k)%owner > 0) then
            call push_int(site%graphs_of(site%graphs%graphs(k)%owner)%ids, k)
        end if
    end do
    extra = size(pages_in) + 2
    allocate (pages(n + NUM_LISTS + 1 + extra), paths(n + NUM_LISTS + 1 + extra))
    count = 0

    directory = s%output_dir
    call make_directory(directory, stat)
    do k = 1, size(SITE_DIRECTORIES)
        if (stat /= 0) exit
        call make_directory(directory//'/'//trim(SITE_DIRECTORIES(k)), stat)
    end do
    if (stat /= 0) then
        errmsg = 'could not create '//directory
        return
    end if
    call page_directories(directory, pages_in)

    call write_entity_pages(site, ids, directory, count, stat, errmsg)
    if (stat /= 0) return

    do k = 1, NUM_LISTS
        paths(n + k)%s = 'lists/'//trim(LIST_FILE(k))//'.html'
        if (size(site%members(k)%ids) > 0) call list_page(site, k, pages(n + k)%s)
    end do
    paths(n + NUM_LISTS + 1)%s = 'index.html'
    call index_page(site, front, pages(n + NUM_LISTS + 1)%s)
    do k = 1, size(pages_in)
        paths(n + NUM_LISTS + 1 + k)%s = 'page/'//pages_in(k)%path
        call static_page(site, k, pages(n + NUM_LISTS + 1 + k)%s)
    end do
    paths(size(paths) - 1)%s = 'lists/names.html'
    call names_page(site, pages(size(paths) - 1)%s)
    paths(size(paths))%s = 'search.html'
    if (s%search) call search_page(site, pages(size(paths))%s)

    call progress_start('Writing files', size(pages))
    do k = 1, size(pages)
        call progress_tick()
        if (.not. allocated(pages(k)%s)) cycle
        count = count + 1
        call write_whole_file(directory//'/'//paths(k)%s, pages(k)%s, stat, errmsg)
        if (stat /= 0) return
    end do
    call progress_finish()
    call asset_fdoc_css(b)
    call take(b, url)
    call write_whole_file(directory//'/css/fdoc.css', url, stat, errmsg)
    if (stat /= 0) return
    call asset_fdoc_js(b)
    call take(b, url)
    call write_whole_file(directory//'/js/fdoc.js', url, stat, errmsg)
    if (stat /= 0) return
    call sidebar_script(site, url)
    call write_whole_file(directory//'/js/sidebar.js', url, stat, errmsg)
    if (stat /= 0) return
    if (s%search) then
        call search_index(site, url)
        call write_whole_file(directory//'/search/index.js', url, stat, errmsg)
        if (stat /= 0) return
    end if
    call copy_page_files(s, directory, pages_in)
    if (allocated(s%css)) call copy_file(s%css, directory//'/css/user.css', k)
    if (allocated(s%favicon)) call copy_file(s%favicon, directory//'/favicon.png', k)
    call logo_name(s, url)
    if (len(url) > 0) then
        call copy_file(s%logo, directory//'/'//url, k)
    else if (allocated(s%logo)) then
        call warn('the logo '//s%logo//' was not found, so the main page has none')
    end if
    if (allocated(s%media_dir)) then
        if (is_directory(s%media_dir)) then
            call remove_directory(directory//'/media')
            call copy_path(s%media_dir, directory//'/media', k)
        end if
    end if
end subroutine write_site

end module fdoc_site
