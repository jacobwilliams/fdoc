module fdoc_docs
!! Rendering the documentation of every entity from Markdown to HTML.
use fdoc_util
use fdoc_model
use fdoc_settings, only: settings_t, map_entry_t
use fdoc_project, only: project_t
use fdoc_markdown, only: markdown_to_html, markdown_options_t, dedent
use fdoc_urls, only: link_context_t, resolve_link
use fdoc_progress, only: progress_start, progress_tick, progress_finish
implicit none
private

public :: base_options, render_documentation, render_text, collect_abbreviations

contains

!> The Markdown options of a project.
subroutine base_options(s, root, options)
    type(settings_t), intent(in) :: s                   !! the settings
    character(len=*), intent(in) :: root                !! from the pages to the root of the site
    type(markdown_options_t), intent(out) :: options    !! the options
    integer :: k
    character(len=:), allocatable :: url

    url = root
    options%aliases = [s%alias, s%external, map_entry_t('url', url), map_entry_t('media', url//'/media'), &
                       map_entry_t('page', url//'/page')]
    options%base_path = s%md_base_dir
    options%resolver => resolve_link
    do k = 1, size(s%md_extensions)
        select case (s%md_extensions(k)%s)
        case ('markdown.extensions.toc', 'toc')
            options%toc = .true.
        case ('markdown.extensions.smarty', 'smarty')
            options%smarty = .true.
        end select
    end do
end subroutine base_options

!> Make the abbreviations defined in any documentation apply everywhere, as they do in FORD.
subroutine collect_abbreviations(p, options)
    type(project_t), intent(in) :: p                    !! the project
    type(markdown_options_t), intent(inout) :: options  !! the options
    type(map_entry_t), allocatable :: found(:)
    integer :: k, j, at

    allocate (found(0))
    do k = 1, p%list%n
        if (.not. allocated(p%list%box(k)%e%doc)) cycle
        associate (doc => p%list%box(k)%e%doc)
            do j = 1, size(doc)
                if (.not. starts_with(strip(doc(j)%s), '*[')) cycle
                at = index(doc(j)%s, ']:')
                if (at == 0) cycle
                found = [found, map_entry_t(doc(j)%s(index(doc(j)%s, '*[') + 2:at - 1), strip(doc(j)%s(at + 2:)))]
            end do
        end associate
    end do
    if (size(found) > 0) options%abbreviations = found
end subroutine collect_abbreviations

!> Render `text`, as if it belonged to entity `context`, for a page `prefix` away from the root.
subroutine render_text(p, base, context, prefix, text, html)
    type(project_t), intent(inout), target :: p         !! the project
    type(markdown_options_t), intent(in) :: base        !! the Markdown options
    integer, intent(in) :: context                      !! the entity, or 0
    character(len=*), intent(in) :: prefix              !! from the page to the root of the site
    character(len=*), intent(in) :: text                !! the Markdown
    character(len=:), allocatable, intent(out) :: html  !! the HTML
    type(markdown_options_t) :: options
    type(link_context_t), target :: link

    options = base
    link%list => p%list
    link%entity = context
    link%prefix = prefix
    options%resolver_data => link
    call markdown_to_html(text, options, html)
end subroutine render_text

!> The first paragraph of `html`, or `''`.
pure subroutine first_paragraph(html, out)
    character(len=*), intent(in) :: html                !! the HTML
    character(len=:), allocatable, intent(out) :: out   !! the paragraph
    integer :: a, b

    out = ''
    a = index(html, '<p>')
    if (a == 0) a = index(html, '<p ')
    if (a == 0) return
    b = index(html(a:), '</p>')
    if (b > 0) out = html(a:a + b + 2)
end subroutine first_paragraph

!> Render the documentation and summary of each entity.
subroutine render_documentation(p, s, options)
    type(project_t), intent(inout), target :: p         !! the project
    type(settings_t), intent(in) :: s                   !! the settings
    type(markdown_options_t), intent(in) :: options     !! the Markdown options
    integer :: k

    call progress_start('Converting Markdown', p%list%n)
    !$omp parallel do schedule(dynamic, 16) num_threads(max(1, s%parallel)) default(shared)
    do k = 1, p%list%n
        call render_entity(p, options, k)
        call progress_tick()
    end do
    !$omp end parallel do
    call borrow_summaries(p)
    call progress_finish()
end subroutine render_documentation

!> Give the interface or the body of a separate module procedure without a summary that of the other.
subroutine borrow_summaries(p)
    type(project_t), intent(inout) :: p     !! the project
    integer :: k, other

    do k = 1, p%list%n
        other = p%list%box(k)%e%implements
        if (other <= 0) cycle
        if (has_summary(p%list%box(k)%e) .or. .not. has_summary(p%list%box(other)%e)) cycle
        p%list%box(k)%e%summary = p%list%box(other)%e%summary
    end do
end subroutine borrow_summaries

!> True if `e` has a summary that is not blank.
pure logical function has_summary(e)
    type(entity_t), intent(in) :: e     !! the entity

    has_summary = .false.
    if (allocated(e%summary)) has_summary = len_trim(e%summary) > 0
end function has_summary

!> Render the documentation and summary of entity `id`.
subroutine render_entity(p, options, id)
    type(project_t), intent(inout), target :: p         !! the project
    type(markdown_options_t), intent(in) :: options     !! the Markdown options
    integer, intent(in) :: id                           !! the entity
    type(string_t), allocatable :: lines(:)
    integer :: k

    if (.not. allocated(p%list%box(id)%e%doc)) return
    if (p%list%box(id)%e%kind == E_NONE) return
    associate (e => p%list%box(id)%e)
        lines = e%doc
        call dedent(lines)
        call render_text(p, options, id, '../', join(lines, achar(10)), e%html)
        do k = 1, size(e%meta)
            if (e%meta(k)%key /= 'summary') cycle
            call render_text(p, options, id, '../', join(split_words(join(e%meta(k)%values, ' ')), ' '), e%summary)
        end do
        if (.not. allocated(e%summary)) call first_paragraph(e%html, e%summary)
    end associate
end subroutine render_entity

end module fdoc_docs
