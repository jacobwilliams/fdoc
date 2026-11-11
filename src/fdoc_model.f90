module fdoc_model
!! The documented program: source files and the entities in them.
!!
!! Entities live in one list and refer to each other by index. Each is held through an allocatable, so
!! that growing the list or merging one list into another moves entities and does not copy them.
use fdoc_util, only: string_t, push, push_int, name_index_t
implicit none
private

public :: is_procedure, is_separate_body, is_separate_interface

! entity kinds
integer, parameter, public :: E_NONE = 0
integer, parameter, public :: E_FILE = 1          !! a source file
integer, parameter, public :: E_MODULE = 2
integer, parameter, public :: E_SUBMODULE = 3
integer, parameter, public :: E_PROGRAM = 4
integer, parameter, public :: E_BLOCKDATA = 5
integer, parameter, public :: E_SUBROUTINE = 6
integer, parameter, public :: E_FUNCTION = 7
integer, parameter, public :: E_MODPROC = 8       !! implementation of a separate module procedure
integer, parameter, public :: E_INTERFACE = 9     !! a generic interface; other blocks dissolve into their bodies
integer, parameter, public :: E_TYPE = 10
integer, parameter, public :: E_ENUM = 11
integer, parameter, public :: E_BOUNDPROC = 12
integer, parameter, public :: E_FINAL = 13
integer, parameter, public :: E_VARIABLE = 14
integer, parameter, public :: E_NAMELIST = 15
integer, parameter, public :: E_COMMON = 16

! how a procedure relates to an interface block
integer, parameter, public :: IFACE_NONE = 0      !! an ordinary procedure, or the body of a generic interface
integer, parameter, public :: IFACE_EXPLICIT = 1  !! the body of an unnamed interface block
integer, parameter, public :: IFACE_ABSTRACT = 2  !! the body of an abstract interface block

! how a variable relates to its procedure or type
integer, parameter, public :: VAR_PLAIN = 0       !! listed among the variables of its parent
integer, parameter, public :: VAR_ARGUMENT = 1    !! a dummy argument
integer, parameter, public :: VAR_RESULT = 2      !! the result of a function
integer, parameter, public :: VAR_HIDDEN = 3      !! not shown, such as `external` names
integer, parameter, public :: VAR_COMMON = 4      !! a member of a common block, shown with the block

type, public :: attribute_t
    !! An attribute given in a statement of its own, such as `public :: x`, waiting to be applied.
    character(len=:), allocatable :: name       !! the entity, in lowercase
    character(len=:), allocatable :: value      !! the attribute
end type attribute_t

type, public :: rename_t
    !! A name in the `only` list of a `use` statement.
    character(len=:), allocatable :: used   !! the name in the used module, in lowercase
    character(len=:), allocatable :: local  !! the name in the using scope, in lowercase
end type rename_t

type, public :: use_t
    !! A `use` statement.
    character(len=:), allocatable :: module     !! the module, in lowercase
    character(len=:), allocatable :: spec       !! what follows the module name
    integer :: target = 0                       !! the module's entity, if it is known
    logical :: only = .false.                   !! has an `only` list
    type(rename_t), allocatable :: renames(:)   !! the `only` list
end type use_t

type, public :: meta_t
    !! A metadata entry of a doc comment, such as `author: me`.
    character(len=:), allocatable :: key        !! the key, in lowercase
    type(string_t), allocatable :: values(:)    !! the values
end type meta_t

type, public :: entity_more_t
    !! What only some entities have. Most entities are variables, which share one of these, so an
    !! entity takes half the memory; this makes up most of the memory used on large projects.
    type(use_t), allocatable :: uses(:)         !! its `use` statements
    type(attribute_t), allocatable :: pending(:)    !! attributes waiting to be applied
    type(attribute_t), allocatable :: parameters(:) !! values from `parameter` statements
    type(string_t), allocatable :: public_list(:)   !! names known to be public, in lowercase
    character(len=:), allocatable :: ancestor       !! the ancestor module, in lowercase
    character(len=:), allocatable :: parent_submodule  !! the parent submodule, in lowercase
    integer, allocatable :: descendants(:)      !! the submodules directly below this entity
    type(string_t), allocatable :: args(:)      !! dummy argument names as written
    integer, allocatable :: arg_ids(:)          !! the entity of each argument
    character(len=:), allocatable :: result_name    !! the result's name as written
    character(len=:), allocatable :: bind_c     !! the text of `bind(...)`, if any
    type(string_t), allocatable :: calls(:)     !! `%`-separated names of what it calls, in lowercase
    type(string_t), allocatable :: unknown_calls(:) !! the names of calls that were not found
    type(string_t), allocatable :: modprocs(:)  !! procedures listed by `module procedure`
    integer, allocatable :: modproc_ids(:)      !! those procedures, once found
    character(len=:), allocatable :: extends    !! the parent type's name as written, if any
    type(string_t), allocatable :: parameters_list(:)   !! type parameter names
    integer, allocatable :: inherited(:)        !! the components and bindings it inherits
    type(string_t), allocatable :: bindings(:)  !! what it is bound to
    character(len=:), allocatable :: proto_text !! the interface in `procedure(...)`, if any
    integer, allocatable :: binding_ids(:)      !! what each of `bindings` is, once found
    logical :: saved_state = .false.            !! a procedure that keeps values between calls
    logical :: common_block = .false.          !! a procedure with a common block
end type entity_more_t

type, public :: entity_t
    !! A file, program unit, procedure, type, variable or the like. Which components apply depends on
    !! `kind`.
    type(entity_more_t), pointer :: more => null()  !! what only some entities have
    integer :: kind = E_NONE                    !! one of the `E_*` constants
    integer :: parent = 0                       !! the entity containing this one
    integer :: file = 0                         !! the file containing this entity
    integer :: line = 0                         !! line of its first statement in the text parsed
    character(len=:), allocatable :: name       !! its name as written
    character(len=:), allocatable :: permission !! `public`, `private` or `protected`
    type(string_t), allocatable :: doc(:)       !! its documentation, without metadata
    type(meta_t), allocatable :: meta(:)        !! the metadata of its documentation
    character(len=:), allocatable :: html       !! its documentation as HTML
    character(len=:), allocatable :: summary    !! its summary as HTML
    type(string_t), allocatable :: attribs(:)   !! its attributes
    integer, allocatable :: children(:)         !! the entities directly inside this one, in order
    logical :: visible = .true.                 !! false once pruned
    type(string_t), allocatable :: display(:)   !! the permissions shown inside this entity
    logical :: internals = .false.              !! the `proc_internals` setting that applies here
    character(len=:), allocatable :: external_url  !! where it is documented, if in another project
    character(len=:), allocatable :: page       !! the name of its page, unique in its directory, if it has one
    character(len=:), allocatable :: source_text !! the text of a file, as it is
    integer :: source_first = 0                 !! where its code starts in the text of its file, if found
    integer :: source_last = 0                  !! where its code ends

    ! containers
    logical :: in_contains = .false.            !! true after its `contains` statement
    character(len=:), allocatable :: child_permission  !! permission of what is declared from here on

    ! submodules
    integer :: ancestor_id = 0                  !! the ancestor module, once found
    integer :: parent_submodule_id = 0          !! the parent submodule, once found

    ! procedures
    integer :: result = 0                       !! the result variable of a function
    logical :: is_module = .false.              !! has the `module` prefix
    integer :: iface = IFACE_NONE               !! one of the `IFACE_*` constants
    integer, allocatable :: call_ids(:)         !! the procedures called, once found
    integer :: implements = 0                   !! the interface of a separate module procedure, once found
    integer :: bound_to = 0                     !! a type that this procedure is bound to, once found

    ! interfaces
    logical :: abstract = .false.               !! an `abstract interface` block
    logical :: generic = .false.                !! a generic interface or binding

    ! types
    logical :: sequence = .false.               !! has a `sequence` statement
    integer :: extends_id = 0                   !! the parent type, once found
    integer :: constructor = 0                  !! the procedure or interface named like it, once found
    integer :: inherit_state = 0                !! 0 not done, 1 in progress, 2 done

    ! bound procedures
    logical :: deferred = .false.               !! has the `deferred` attribute
    integer :: proto_id = 0                     !! the interface or type it refers to, once found

    ! variables
    integer :: var_role = VAR_PLAIN             !! one of the `VAR_*` constants
    character(len=:), allocatable :: vartype    !! its type keyword, in lowercase
    character(len=:), allocatable :: type_kind  !! its kind, if any
    character(len=:), allocatable :: strlen     !! its character length, if any
    type(string_t), allocatable :: proto(:)     !! the type or interface name, and its arguments
    character(len=:), allocatable :: intent     !! `in`, `out`, `inout` or empty
    logical :: optional = .false.               !! has the `optional` attribute
    logical :: parameter = .false.              !! has the `parameter` attribute
    logical :: points = .false.                 !! initialized with `=>`
    character(len=:), allocatable :: initial    !! its initial value, if any
    character(len=:), allocatable :: dimension  !! its dimensions, if declared after the name
end type entity_t

type, public :: entity_box_t
    !! Holds an entity, so that it can be moved without being copied.
    type(entity_t), allocatable :: e    !! the entity
end type entity_box_t

type, public :: entity_list_t
    !! A list of entities. Indices start at 1, and 0 means no entity.
    type(entity_box_t), allocatable :: box(:)   !! storage; only `box(1:n)` is in use
    integer :: n = 0                            !! number of entities
    type(name_index_t) :: names                 !! the first entity of each lowercase name, once built
    integer, allocatable :: next_named(:)       !! the next entity with the same name, or 0
contains
    procedure :: add => list_add                !! add an entity
end type entity_list_t


!> What all variables share. This is never written to.
type(entity_more_t), target, save, public :: no_more

contains

!> True if `kind` is a subroutine or function.
elemental logical function is_procedure(kind)
    integer, intent(in) :: kind     !! one of the `E_*` constants

    is_procedure = kind == E_SUBROUTINE .or. kind == E_FUNCTION
end function is_procedure

!> True if entity `id` is the body of a separate module procedure, whose interface has been found: a
!> `module procedure`, or a procedure with the `module` prefix that is not in an interface block.
pure logical function is_separate_body(list, id)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: id               !! the entity

    is_separate_body = .false.
    associate (e => list%box(id)%e)
        if (e%implements <= 0) return
        if (e%kind /= E_MODPROC) then
            if (e%iface /= IFACE_NONE) return
            if (e%parent > 0) then
                if (list%box(e%parent)%e%kind == E_INTERFACE) return
            end if
        end if
    end associate
    is_separate_body = .true.
end function is_separate_body

!> True if entity `id` is the interface of a separate module procedure whose body has been found.
pure logical function is_separate_interface(list, id)
    type(entity_list_t), intent(in) :: list !! the entities
    integer, intent(in) :: id               !! the entity

    is_separate_interface = list%box(id)%e%implements > 0 .and. .not. is_separate_body(list, id)
end function is_separate_interface

!> Add an entity of kind `kind` inside `parent`, returning its index.
integer function list_add(self, kind, parent) result(id)
    class(entity_list_t), intent(inout) :: self     !! the list
    integer, intent(in) :: kind                     !! one of the `E_*` constants
    integer, intent(in) :: parent                   !! the containing entity, or 0
    type(entity_box_t), allocatable :: grown(:)
    integer :: k

    if (.not. allocated(self%box)) allocate (self%box(64))
    if (self%n == size(self%box)) then
        allocate (grown(2*self%n))
        do k = 1, self%n
            call move_alloc(self%box(k)%e, grown(k)%e)
        end do
        call move_alloc(grown, self%box)
    end if
    self%n = self%n + 1
    id = self%n
    allocate (self%box(id)%e)
    self%box(id)%e%kind = kind
    if (kind == E_VARIABLE) then
        self%box(id)%e%more => no_more
    else
        allocate (self%box(id)%e%more)
    end if
    self%box(id)%e%parent = parent
    allocate (self%box(id)%e%children(0))
    if (parent > 0) then
        self%box(id)%e%file = self%box(parent)%e%file
        call push_int(self%box(parent)%e%children, id)
    end if
end function list_add

end module fdoc_model
