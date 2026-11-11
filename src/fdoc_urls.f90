module fdoc_urls
!! Where entities are documented, and what `[[name]]` links refer to.
!!
!! This runs while documentation is rendered in parallel, so nothing here calls a function returning a
!! deferred-length string (see `example/slen_race.f90`).
use fdoc_util
use fdoc_model
use fdoc_buffer, only: buffer_t, add, html_escape
implicit none
private

public :: page_name, object_word, entity_url, entity_anchor, link_context_t, resolve_link, url_quote, index_entity_names
public :: is_shown, page_entity

type :: link_context_t
    !! Where a `[[name]]` link is being read.
    type(entity_list_t), pointer :: list => null()  !! the entities
    integer :: entity = 0                           !! the entity being documented, or 0
    character(len=:), allocatable :: prefix         !! from the page to the root of the site
end type link_context_t

contains

!> `name` as used in file names and anchors.
subroutine page_name(name, out)
    character(len=*), intent(in) :: name                !! the name
    character(len=:), allocatable, intent(out) :: out   !! the result
    integer :: k

    out = ''
    do k = 1, len(name)
        select case (name(k:k))
        case ('<'); out = out//'lt'
        case ('>'); out = out//'gt'
        case ('/'); out = out//'SLASH'
        case ('*'); out = out//'ASTERISK'
        case ('A':'Z'); out = out//lower_char(name(k:k))
        case default; out = out//name(k:k)
        end select
    end do
    if (len(out) == 0) out = '__unnamed__'
end subroutine page_name

!> Percent-encode the characters of `text` that are not safe in a URL.
subroutine url_quote(text, out)
    character(len=*), intent(in) :: text                !! the text
    character(len=:), allocatable, intent(out) :: out   !! the result
    character(len=*), parameter :: HEX = '0123456789ABCDEF'
    character(len=*), parameter :: SAFE = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.-~/'
    integer :: k, c

    out = ''
    do k = 1, len(text)
        if (index(SAFE, text(k:k)) > 0) then
            out = out//text(k:k)
        else
            c = iand(ichar(text(k:k)), 255)
            out = out//'%'//HEX(c/16 + 1:c/16 + 1)//HEX(mod(c, 16) + 1:mod(c, 16) + 1)
        end if
    end do
end subroutine url_quote

!> The word FORD uses for the kind of entity `e`.
subroutine object_word(e, word)
    type(entity_t), intent(in) :: e                     !! the entity
    character(len=:), allocatable, intent(out) :: word  !! the word

    select case (e%kind)
    case (E_FILE); word = 'sourcefile'
    case (E_MODULE); word = 'module'
    case (E_SUBMODULE); word = 'submodule'
    case (E_PROGRAM); word = 'program'
    case (E_BLOCKDATA); word = 'blockdata'
    case (E_SUBROUTINE, E_FUNCTION, E_MODPROC); word = 'proc'
    case (E_INTERFACE); word = 'interface'
    case (E_TYPE); word = 'type'
    case (E_ENUM); word = 'enum'
    case (E_BOUNDPROC); word = 'boundprocedure'
    case (E_FINAL); word = 'finalproc'
    case (E_VARIABLE); word = 'variable'
    case (E_NAMELIST); word = 'namelist'
    case (E_COMMON); word = 'common'
    case default; word = ''
    end select
    ! a separate module procedure, whose body has been found, is a procedure like any other
    if (is_procedure(e%kind) .and. e%iface /= IFACE_NONE .and. e%implements == 0) word = 'interface'
end subroutine object_word

!> Write the anchor of an entity in a page.
subroutine entity_anchor(list, id, b)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: id               !! the entity
    type(buffer_t), intent(inout) :: b      !! the HTML
    character(len=:), allocatable :: word, name, quoted

    call object_word(list%box(id)%e, word)
    call page_name(list%box(id)%e%name, name)
    call url_quote(name, quoted)
    call add(b, word//'-')
    call html_escape(quoted, b, .true.)
end subroutine entity_anchor

!> The URL of an entity relative to the root of the site, or `''`.
recursive subroutine entity_url(list, id, url)
    type(entity_list_t), intent(in) :: list             !! the entities
    integer, intent(in) :: id                           !! the entity
    character(len=:), allocatable, intent(out) :: url   !! the URL
    character(len=:), allocatable :: word, name, quoted
    integer :: at
    logical :: has_page

    associate (e => list%box(id)%e)
        if (allocated(e%external_url)) then
            url = e%external_url
            return
        end if
        has_page = .false.
        select case (e%kind)
        case (E_FILE, E_MODULE, E_SUBMODULE, E_PROGRAM, E_BLOCKDATA, E_NAMELIST)
            has_page = .true.
        case (E_TYPE, E_INTERFACE, E_SUBROUTINE, E_FUNCTION, E_MODPROC)
            has_page = e%iface /= IFACE_NONE
            if (e%parent > 0) then
                select case (list%box(e%parent)%e%kind)
                case (E_FILE, E_MODULE, E_SUBMODULE, E_PROGRAM, E_BLOCKDATA)
                    has_page = .true.
                end select
            end if
        end select
        call object_word(e, word)
        call page_name(e%name, name)
        if (has_page) then
            if (e%kind == E_SUBMODULE) word = 'module'
            if (allocated(e%page)) then
                url = word//'/'//e%page//'.html'
            else
                url = word//'/'//name//'.html'
            end if
            return
        end if
        url = ''
        if (e%parent == 0) return
        select case (e%kind)
        case (E_BOUNDPROC, E_COMMON, E_VARIABLE, E_ENUM, E_FINAL, E_SUBROUTINE, E_FUNCTION)
            call entity_url(list, e%parent, url)
            if (len(url) == 0) return
            at = index(url, '#')
            if (at > 0) url = url(:at - 1)
            call url_quote(name, quoted)
            url = url//'#'//word//'-'//quoted
        end select
    end associate
end subroutine entity_url

!> True if entity `e` is of class `entity`, as written in a link. Any entity matches `''`.
pure logical function of_class(e, entity)
    type(entity_t), intent(in) :: e         !! the entity
    character(len=*), intent(in) :: entity  !! the class, in lowercase

    select case (entity)
    case ('')
        of_class = .true.
    case ('module')
        of_class = e%kind == E_MODULE
    case ('submodule')
        of_class = e%kind == E_SUBMODULE
    case ('type')
        of_class = e%kind == E_TYPE
    case ('procedure', 'proc')
        of_class = is_procedure(e%kind)
    case ('subroutine')
        of_class = e%kind == E_SUBROUTINE
    case ('function')
        of_class = e%kind == E_FUNCTION
    case ('file', 'sourcefile')
        of_class = e%kind == E_FILE
    case ('interface')
        of_class = e%kind == E_INTERFACE .or. e%iface == IFACE_EXPLICIT
    case ('absinterface')
        of_class = e%iface == IFACE_ABSTRACT
    case ('program')
        of_class = e%kind == E_PROGRAM
    case ('block')
        of_class = e%kind == E_BLOCKDATA
    case ('namelist')
        of_class = e%kind == E_NAMELIST
    case ('variable')
        of_class = e%kind == E_VARIABLE
    case ('bound')
        of_class = e%kind == E_BOUNDPROC
    case ('final')
        of_class = e%kind == E_FINAL
    case ('common')
        of_class = e%kind == E_COMMON
    case default
        of_class = .false.
    end select
end function of_class

!> True if entity `id` is named `name`, in lowercase, and is of class `entity`.
logical function matches(list, id, name, entity)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: id               !! the entity
    character(len=*), intent(in) :: name    !! the name
    character(len=*), intent(in) :: entity  !! the class
    character(len=:), allocatable :: file_name
    integer :: at

    matches = .false.
    associate (e => list%box(id)%e)
        if (.not. allocated(e%name) .or. e%kind == E_NONE) return
        if (.not. of_class(e, entity)) return
        if (e%kind == E_FILE) then
            at = last_separator(e%name)
            file_name = e%name(at + 1:)
            matches = lowercase(file_name) == name
            return
        end if
        matches = lowercase(e%name) == name
    end associate
end function matches

!> The child of `parent` named `name`, or 0.
integer function find_child(list, parent, name, entity) result(found)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: parent           !! the entity
    character(len=*), intent(in) :: name    !! the name, in lowercase
    character(len=*), intent(in) :: entity  !! the class, in lowercase
    integer :: k

    found = 0
    associate (e => list%box(parent)%e)
        do k = 1, size(e%children)
            if (matches(list, e%children(k), name, entity)) then
                found = e%children(k)
                return
            end if
        end do
        if (.not. allocated(e%more%inherited)) return
        do k = 1, size(e%more%inherited)
            if (matches(list, e%more%inherited(k), name, entity)) then
                found = e%more%inherited(k)
                return
            end if
        end do
    end associate
end function find_child

!> True if a link to `kind` can name it from anywhere in the project.
elemental logical function top_level(kind)
    integer, intent(in) :: kind     !! the kind of entity

    select case (kind)
    case (E_MODULE, E_SUBMODULE, E_TYPE, E_SUBROUTINE, E_FUNCTION, E_INTERFACE, E_PROGRAM, E_BLOCKDATA, E_FILE)
        top_level = .true.
    case default
        top_level = .false.
    end select
end function top_level

!> Index the entities that links can name from anywhere, for [[find_anywhere]].
subroutine index_entity_names(list)
    type(entity_list_t), intent(inout) :: list  !! the entities
    integer, allocatable :: last(:)
    integer :: k, first, at

    allocate (list%next_named(list%n), last(list%n))
    list%next_named = 0
    do k = 1, list%n
        associate (e => list%box(k)%e)
            if (.not. top_level(e%kind) .or. .not. allocated(e%name)) cycle
            at = merge(last_separator(e%name), 0, e%kind == E_FILE)
            first = index_find(list%names, lowercase(e%name(at + 1:)))
            if (first == 0) then
                call index_set(list%names, lowercase(e%name(at + 1:)), k)
                last(k) = k
            else
                list%next_named(last(first)) = k
                last(first) = k
            end if
        end associate
    end do
end subroutine index_entity_names

!> The entity named `name` at the top level of the project, or 0.
integer function find_anywhere(list, name, entity) result(found)
    type(entity_list_t), intent(in) :: list !! the entities
    character(len=*), intent(in) :: name    !! the name, in lowercase
    character(len=*), intent(in) :: entity  !! the class, in lowercase
    integer :: k

    found = 0
    k = index_find(list%names, name)
    do while (k > 0)
        if (matches(list, k, name, entity)) then
            found = k
            return
        end if
        k = list%next_named(k)
    end do
end function find_anywhere

!> True if `item` and everything that holds it are shown.
pure logical function is_shown(list, item)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: item             !! the entity
    integer :: k

    is_shown = .true.
    k = item
    do while (k > 0)
        if (.not. list%box(k)%e%visible) is_shown = .false.
        k = list%box(k)%e%parent
    end do
end function is_shown

!> The entity shown in place of `id`: the interface of the body of a separate module procedure, whose
!> page is that of both, or else `id`.
pure integer function page_entity(list, id) result(item)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: id               !! the entity

    item = id
    if (list%box(id)%e%visible .or. .not. is_separate_body(list, id)) return
    if (is_shown(list, list%box(id)%e%implements)) item = list%box(id)%e%implements
end function page_entity

!> Replace `item` by what holds it, if it is inside something not shown, which has no page.
pure subroutine shown_entity(list, item)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(inout) :: item          !! the entity
    integer :: k, parent

    if (allocated(list%box(item)%e%external_url)) return
    item = page_entity(list, item)
    k = item
    do while (k > 0)
        parent = list%box(k)%e%parent
        if (.not. list%box(k)%e%visible .and. parent > 0) item = parent
        k = parent
    end do
end subroutine shown_entity

!> Find what a `[[name]]` link means: see [[link_resolver]] in [[fdoc_markdown]].
subroutine resolve_link(data, name, entity, child, child_entity, url, text, found)
    class(*), intent(in) :: data                        !! a [[link_context_t]]
    character(len=*), intent(in) :: name                !! the name
    character(len=*), intent(in) :: entity              !! its class, or `''`
    character(len=*), intent(in) :: child               !! the child's name, or `''`
    character(len=*), intent(in) :: child_entity        !! the child's class, or `''`
    character(len=:), allocatable, intent(out) :: url   !! the link
    character(len=:), allocatable, intent(out) :: text  !! the text to show
    logical, intent(out) :: found                       !! false if there is no such entity
    character(len=len(name)) :: low
    character(len=len(entity)) :: class
    character(len=:), allocatable :: label
    integer :: item, inner, parent

    found = .false.
    url = ''
    text = name
    low = lowercase(name)
    class = lowercase(entity)
    select type (data)
    type is (link_context_t)
        item = 0
        if (data%entity > 0) then
            item = find_child(data%list, data%entity, low, class)
            parent = data%list%box(data%entity)%e%parent
            if (item == 0 .and. parent > 0) item = find_child(data%list, parent, low, class)
        end if
        if (item == 0) item = find_anywhere(data%list, low, class)
        if (item == 0) return
        label = data%list%box(item)%e%name
        if (len(child) > 0) then
            inner = find_child(data%list, item, lowercase(child), lowercase(child_entity))
            if (inner > 0) then
                item = inner
                label = data%list%box(item)%e%name
            end if
        end if
        call shown_entity(data%list, item)
        call entity_url(data%list, item, url)
        ! something not shown has no anchor to go to: link to the page around it
        do while (len(url) == 0 .and. data%list%box(item)%e%parent > 0)
            item = data%list%box(item)%e%parent
            call entity_url(data%list, item, url)
        end do
        if (len(url) == 0) return
        if (.not. data%list%box(item)%e%visible .and. index(url, '#') > 0) url = url(:index(url, '#') - 1)
        if (.not. allocated(data%list%box(item)%e%external_url)) url = data%prefix//url
        text = label
        found = .true.
    end select
end subroutine resolve_link

end module fdoc_urls
