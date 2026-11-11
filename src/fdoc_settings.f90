module fdoc_settings
!! The project settings: FORD's options, their defaults and their conversion from text.
!!
!! Every source of settings (project file, `fpm.toml`, `--config`, command line) goes through
!! [[set_option]], which takes the option's values as a list of strings, the form the project file
!! gives them.
use omp_lib, only: omp_get_num_procs
use json_module, only: json_core, json_value
use ffmt_strings, only: to_s
use fdoc_util
implicit none
private

public :: new_settings, set_option, check_settings, finish_settings, dump_settings, is_option

character(len=*), parameter :: STRING_OPTIONS = &
    ' author author_description author_pic bitbucket creation_date doc_license docmark docmark_alt email encoding'// &
    ' facebook github gitlab gitter_sidecar google_plus license linkedin predocmark predocmark_alt preprocessor'// &
    ' privacy_policy_url project project_bitbucket project_download project_github project_gitlab'// &
    ' project_sourceforge project_url project_website revision sort summary terms_of_service_url twitter version'// &
    ' website year '
character(len=*), parameter :: PATH_OPTIONS = &
    ' css favicon graph_dir logo mathjax_config md_base_dir media_dir output_dir page_dir '
character(len=*), parameter :: LOGICAL_OPTIONS = &
    ' coloured_edges dbg doxygen externalize fixed_length_limit force graph hide_undoc incl_src lower preprocess'// &
    ' print_creation_date proc_internals quiet search show_proc_parent source warn '
character(len=*), parameter :: INTEGER_OPTIONS = &
    ' graph_maxdepth graph_maxnodes graph_project_maxnodes max_frontpage_items parallel '
character(len=*), parameter :: LIST_OPTIONS = &
    ' copy_subdir display exclude exclude_dir extensions extra_vartypes fixed_extensions fpp_extensions'// &
    ' html_template_dir include macro md_extensions src_dir '
character(len=*), parameter :: PATH_LIST_OPTIONS = ' copy_subdir exclude_dir html_template_dir include src_dir '
character(len=*), parameter :: MAP_OPTIONS = ' alias external extra_mods extra_filetypes '

type, public :: map_entry_t
    !! A key and its value.
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value
end type map_entry_t

type, public :: file_type_t
    !! An `extra_filetypes` entry.
    character(len=:), allocatable :: extension
    character(len=:), allocatable :: comment
    character(len=:), allocatable :: lexer      !! not allocated if not given
end type file_type_t

type, public :: settings_t
    !! The project settings. Optional text options are not allocated unless set.
    character(len=:), allocatable :: author, author_description, author_pic, bitbucket, doc_license, email, facebook
    character(len=:), allocatable :: github, gitlab, gitter_sidecar, google_plus, license, linkedin, privacy_policy_url
    character(len=:), allocatable :: project_bitbucket, project_download, project_github, project_gitlab
    character(len=:), allocatable :: project_sourceforge, project_website, revision, summary, terms_of_service_url
    character(len=:), allocatable :: twitter, version, website
    character(len=:), allocatable :: creation_date, docmark, docmark_alt, encoding, predocmark, predocmark_alt
    character(len=:), allocatable :: preprocessor, project, project_url, sort, year
    character(len=:), allocatable :: css, favicon, graph_dir, logo, mathjax_config, md_base_dir, media_dir, output_dir, page_dir
    character(len=:), allocatable :: directory          !! the project's directory
    logical :: coloured_edges = .false., dbg = .true., doxygen = .true., externalize = .true.
    logical :: fixed_length_limit = .true., force = .false., graph = .false., hide_undoc = .false., incl_src = .true.
    logical :: lower = .false., preprocess = .true., print_creation_date = .false., proc_internals = .false.
    logical :: quiet = .false., search = .true., show_proc_parent = .false., source = .false., warn = .false.
    logical :: relative = .true.                        !! true if `project_url` was not given
    integer :: graph_maxdepth = 3, graph_maxnodes = 100, graph_project_maxnodes = 1000
    integer :: max_frontpage_items = 10
    integer :: parallel = 1                             !! threads to use: `new_settings` gives every core
    type(string_t), allocatable :: copy_subdir(:), display(:), exclude(:), exclude_dir(:), extensions(:)
    type(string_t), allocatable :: extra_vartypes(:), fixed_extensions(:), fpp_extensions(:), html_template_dir(:)
    type(string_t), allocatable :: include(:), macro(:), md_extensions(:), src_dir(:)
    type(map_entry_t), allocatable :: alias(:), external(:), extra_mods(:)
    type(file_type_t), allocatable :: extra_filetypes(:)
end type settings_t

contains

!> The defaults.
function new_settings() result(s)
    type(settings_t) :: s
    integer :: values(8)

    call date_and_time(values=values)
    s%creation_date = '%Y-%m-%dT%H:%M:%S.%f%z'
    s%parallel = omp_get_num_procs()
    s%docmark = '!'
    s%docmark_alt = '*'
    s%predocmark = '>'
    s%predocmark_alt = '|'
    s%encoding = 'utf-8'
    s%preprocessor = 'pcpp -D__GFORTRAN__ --passthru-comments'
    s%project = 'Fortran Program'
    s%project_url = ''
    s%sort = 'src'
    s%year = to_s(values(1))
    s%doc_license = ''
    s%license = ''
    s%md_base_dir = '.'
    s%output_dir = './doc'
    s%display = [string_t('public'), string_t('protected')]
    s%src_dir = [string_t('./src')]
    s%extensions = [string_t('f90'), string_t('f95'), string_t('f03'), string_t('f08'), string_t('f15')]
    s%fixed_extensions = [string_t('f'), string_t('for'), string_t('F'), string_t('FOR')]
    s%fpp_extensions = [string_t('F90'), string_t('F95'), string_t('F03'), string_t('F08'), string_t('F15'), &
                        string_t('F'), string_t('FOR')]
    allocate (s%copy_subdir(0), s%exclude(0), s%exclude_dir(0), s%extra_vartypes(0), s%html_template_dir(0))
    allocate (s%include(0), s%macro(0), s%md_extensions(0))
    allocate (s%alias(0), s%external(0), s%extra_mods(0), s%extra_filetypes(0))
end function new_settings

!> True if `key` is one of the blank-separated words of `set`, which starts and ends with a blank.
pure logical function in_set(set, key)
    character(len=*), intent(in) :: set     !! the words
    character(len=*), intent(in) :: key     !! the word to find
    in_set = index(set, ' '//key//' ') > 0
end function in_set

!> True if `key` is the name of an option.
pure logical function is_option(key)
    character(len=*), intent(in) :: key     !! the name
    is_option = len(key) > 0 .and. index(key, ' ') == 0 .and. (in_set(STRING_OPTIONS, key) .or. in_set(PATH_OPTIONS, key) &
                                                               .or. in_set(LOGICAL_OPTIONS, key) .or. &
                                                               in_set(INTEGER_OPTIONS, key) .or. in_set(LIST_OPTIONS, key) &
                                                               .or. in_set(MAP_OPTIONS, key))
end function is_option

!> Set option `key`, which must be one (see [[is_option]]), from its values as text.
subroutine set_option(s, key, values, stat, errmsg)
    type(settings_t), intent(inout) :: s                !! the settings
    character(len=*), intent(in) :: key                 !! the option
    type(string_t), intent(in) :: values(:)             !! its values
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg  !! what is wrong
    character(len=:), allocatable :: text
    logical :: flag, ok
    integer :: number

    stat = 0
    if (in_set(LOGICAL_OPTIONS, key)) then
        if (size(values) /= 1) then
            call fail("Could not convert option '"//key//"' to bool: expected a single value")
            return
        end if
        call parse_bool(values(1)%s, flag, ok)
        if (.not. ok) then
            call fail("Could not convert option '"//key//"' to bool: expected 'true'/'false', got: "//values(1)%s)
            return
        end if
        call set_logical(s, key, flag)
    else if (in_set(INTEGER_OPTIONS, key)) then
        call parse_int(values(1)%s, number, ok)
        if (.not. ok) then
            call fail("Could not convert option '"//key//"' to an integer: "//values(1)%s)
            return
        end if
        select case (key)
        case ('graph_maxdepth'); s%graph_maxdepth = number
        case ('graph_maxnodes'); s%graph_maxnodes = number
        case ('graph_project_maxnodes'); s%graph_project_maxnodes = number
        case ('max_frontpage_items'); s%max_frontpage_items = number
        case ('parallel'); s%parallel = number
        end select
    else if (in_set(LIST_OPTIONS, key)) then
        call set_list(s, key, values)
    else if (in_set(MAP_OPTIONS, key)) then
        call set_map(s, key, values, stat, errmsg)
    else
        text = join(values, achar(10))
        call set_text(s, key, text)
    end if

contains

    !> Record a failure.
    subroutine fail(msg)
        character(len=*), intent(in) :: msg     !! what is wrong
        stat = 1
        errmsg = msg
    end subroutine fail

end subroutine set_option

!> Set a logical option.
subroutine set_logical(s, key, value)
    type(settings_t), intent(inout) :: s    !! the settings
    character(len=*), intent(in) :: key     !! the option
    logical, intent(in) :: value            !! its value

    select case (key)
    case ('coloured_edges'); s%coloured_edges = value
    case ('dbg'); s%dbg = value
    case ('doxygen'); s%doxygen = value
    case ('externalize'); s%externalize = value
    case ('fixed_length_limit'); s%fixed_length_limit = value
    case ('force'); s%force = value
    case ('graph'); s%graph = value
    case ('hide_undoc'); s%hide_undoc = value
    case ('incl_src'); s%incl_src = value
    case ('lower'); s%lower = value
    case ('preprocess'); s%preprocess = value
    case ('print_creation_date'); s%print_creation_date = value
    case ('proc_internals'); s%proc_internals = value
    case ('quiet'); s%quiet = value
    case ('search'); s%search = value
    case ('show_proc_parent'); s%show_proc_parent = value
    case ('source'); s%source = value
    case ('warn'); s%warn = value
    end select
end subroutine set_logical

!> Set a text or path option.
subroutine set_text(s, key, value)
    type(settings_t), intent(inout) :: s    !! the settings
    character(len=*), intent(in) :: key     !! the option
    character(len=*), intent(in) :: value   !! its value

    select case (key)
    case ('author'); s%author = value
    case ('author_description'); s%author_description = value
    case ('author_pic'); s%author_pic = value
    case ('bitbucket'); s%bitbucket = value
    case ('creation_date'); s%creation_date = value
    case ('doc_license'); s%doc_license = value
    case ('docmark'); s%docmark = value
    case ('docmark_alt'); s%docmark_alt = value
    case ('email'); s%email = value
    case ('encoding'); s%encoding = value
    case ('facebook'); s%facebook = value
    case ('github'); s%github = value
    case ('gitlab'); s%gitlab = value
    case ('gitter_sidecar'); s%gitter_sidecar = value
    case ('google_plus'); s%google_plus = value
    case ('license'); s%license = value
    case ('linkedin'); s%linkedin = value
    case ('predocmark'); s%predocmark = value
    case ('predocmark_alt'); s%predocmark_alt = value
    case ('preprocessor'); s%preprocessor = value
    case ('privacy_policy_url'); s%privacy_policy_url = value
    case ('project'); s%project = value
    case ('project_bitbucket'); s%project_bitbucket = value
    case ('project_download'); s%project_download = value
    case ('project_github'); s%project_github = value
    case ('project_gitlab'); s%project_gitlab = value
    case ('project_sourceforge'); s%project_sourceforge = value
    case ('project_url'); s%project_url = value
    case ('project_website'); s%project_website = value
    case ('revision'); s%revision = value
    case ('sort'); s%sort = value
    case ('summary'); s%summary = value
    case ('terms_of_service_url'); s%terms_of_service_url = value
    case ('twitter'); s%twitter = value
    case ('version'); s%version = value
    case ('website'); s%website = value
    case ('year'); s%year = value
    case ('css'); s%css = value
    case ('favicon'); s%favicon = value
    case ('logo'); s%logo = value
    case ('graph_dir'); s%graph_dir = value
    case ('mathjax_config'); s%mathjax_config = value
    case ('md_base_dir'); s%md_base_dir = value
    case ('media_dir'); s%media_dir = value
    case ('output_dir'); s%output_dir = value
    case ('page_dir'); s%page_dir = value
    end select
end subroutine set_text

!> Set a list option.
subroutine set_list(s, key, values)
    type(settings_t), intent(inout) :: s    !! the settings
    character(len=*), intent(in) :: key     !! the option
    type(string_t), intent(in) :: values(:) !! its values

    select case (key)
    case ('copy_subdir'); s%copy_subdir = values
    case ('display'); s%display = values
    case ('exclude'); s%exclude = values
    case ('exclude_dir'); s%exclude_dir = values
    case ('extensions'); s%extensions = values
    case ('extra_vartypes'); s%extra_vartypes = values
    case ('fixed_extensions'); s%fixed_extensions = values
    case ('fpp_extensions'); s%fpp_extensions = values
    case ('html_template_dir'); s%html_template_dir = values
    case ('include'); s%include = values
    case ('macro'); s%macro = values
    case ('md_extensions'); s%md_extensions = values
    case ('src_dir'); s%src_dir = values
    end select
end subroutine set_list

!> Set a map option from strings of the form `key<sep>value`. Empty strings are ignored.
subroutine set_map(s, key, values, stat, errmsg)
    type(settings_t), intent(inout) :: s                !! the settings
    character(len=*), intent(in) :: key                 !! the option
    type(string_t), intent(in) :: values(:)             !! its values
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg  !! what is wrong
    type(map_entry_t), allocatable :: entries(:)
    type(file_type_t), allocatable :: files(:)
    type(string_t), allocatable :: words(:)
    character :: sep
    integer :: k, at

    stat = 0
    if (key == 'extra_filetypes') then
        allocate (files(0))
        do k = 1, size(values)
            if (len(values(k)%s) == 0) cycle
            words = split_words(values(k)%s)
            if (size(words) < 2 .or. size(words) > 3) then
                stat = 1
                errmsg = "Unexpected format for 'extra_filetype': expected 'extension comment [lexer]', got "//values(k)%s
                return
            end if
            files = [files, file_type_t(words(1)%s, words(2)%s)]
            if (size(words) == 3) files(size(files))%lexer = words(3)%s
        end do
        call move_alloc(files, s%extra_filetypes)
        return
    end if

    sep = merge(':', '=', key == 'extra_mods')
    allocate (entries(0))
    do k = 1, size(values)
        if (len(values(k)%s) == 0) cycle
        at = index(values(k)%s, sep)
        if (at == 0) then
            stat = 1
            errmsg = "Error setting option '"//key//"': expected '"//sep//"' in '"//values(k)%s//"'"
            return
        end if
        call add_entry(entries, strip(values(k)%s(:at - 1)), strip(values(k)%s(at + 1:)))
    end do
    select case (key)
    case ('alias'); call move_alloc(entries, s%alias)
    case ('external'); call move_alloc(entries, s%external)
    case ('extra_mods'); call move_alloc(entries, s%extra_mods)
    end select
end subroutine set_map

!> Set `key` in `entries`, replacing any entry it has.
subroutine add_entry(entries, key, value)
    type(map_entry_t), allocatable, intent(inout) :: entries(:) !! the map
    character(len=*), intent(in) :: key                         !! the key
    character(len=*), intent(in) :: value                       !! its value
    integer :: k

    do k = 1, size(entries)
        if (entries(k)%key == key) then
            entries(k)%value = value
            return
        end if
    end do
    entries = [entries, map_entry_t(key, value)]
end subroutine add_entry

!> Validate the project file's settings and derive what depends on them, before the command line
!> is applied.
subroutine check_settings(s, stat, errmsg)
    type(settings_t), intent(inout) :: s                !! the settings
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg  !! what is wrong
    type(string_t), allocatable :: marks(:)
    integer :: k, j

    stat = 0
    s%relative = s%project_url == ''
    do k = 1, size(s%fixed_extensions)
        if (contains_string(s%extensions, s%fixed_extensions(k)%s)) then
            stat = 1
            errmsg = "Fixed-form extension '"//s%fixed_extensions(k)%s//"' also appears in the free-form extension list"
            return
        end if
    end do
    do k = 1, size(s%extra_mods)
        do j = 1, size(s%external)
            if (s%extra_mods(k)%key == s%external(j)%key) then
                stat = 1
                errmsg = "extra-mod '"//s%extra_mods(k)%key//"' also appears in external mods"
                return
            end if
        end do
    end do
    do k = 1, size(s%display)
        s%display(k)%s = lowercase(s%display(k)%s)
    end do
    s%extensions = [s%extensions, s%fpp_extensions]
    call sort_strings(s%extensions)
    call push(s%exclude_dir, s%output_dir)
    call add_intrinsic_modules(s)

    marks = [string_t(s%docmark), string_t(s%predocmark), string_t(s%docmark_alt), string_t(s%predocmark_alt)]
    do k = 1, size(marks)
        do j = k + 1, size(marks)
            if (marks(k)%s == marks(j)%s .and. len(marks(k)%s) > 0) then
                stat = 1
                errmsg = "Doc comment markers are the same: '"//marks(k)%s//"'"
                return
            end if
        end do
    end do
end subroutine check_settings

!> Add the modules that come with compilers and common libraries to `extra_mods`.
subroutine add_intrinsic_modules(s)
    type(settings_t), intent(inout) :: s    !! the settings
    character(len=*), parameter :: WIKI = 'http://fortranwiki.org/fortran/show/'

    call add_entry(s%extra_mods, 'iso_fortran_env', WIKI//'iso_fortran_env')
    call add_entry(s%extra_mods, 'iso_c_binding', WIKI//'iso_c_binding')
    call add_entry(s%extra_mods, 'ieee_arithmetic', WIKI//'ieee_arithmetic')
    call add_entry(s%extra_mods, 'ieee_exceptions', WIKI//'IEEE+arithmetic')
    call add_entry(s%extra_mods, 'ieee_features', WIKI//'IEEE+arithmetic')
    call add_entry(s%extra_mods, 'openacc', &
                   'https://www.openacc.org/sites/default/files/inline-images/Specification/OpenACC.3.0.pdf#page=85')
    call add_entry(s%extra_mods, 'omp_lib', 'https://www.openmp.org/spec-html/5.1/openmpch3.html#x156-1890003')
    call add_entry(s%extra_mods, 'mpi', 'http://www.mpi-forum.org/docs/mpi-3.1/mpi31-report/node410.htm')
    call add_entry(s%extra_mods, 'mpi_f08', 'http://www.mpi-forum.org/docs/mpi-3.1/mpi31-report/node409.htm')
end subroutine add_intrinsic_modules

!> Make paths absolute and format dates, after all settings are applied.
subroutine finish_settings(s, directory, stat, errmsg)
    type(settings_t), intent(inout) :: s                !! the settings
    character(len=*), intent(in) :: directory           !! the project's directory
    integer, intent(out) :: stat                        !! nonzero on failure
    character(len=:), allocatable, intent(out) :: errmsg  !! what is wrong
    integer :: k

    stat = 0
    s%directory = resolve_path(directory, '.')
    if (s%md_base_dir == '.') s%md_base_dir = s%directory
    call normalise(s%css)
    call normalise(s%favicon)
    call normalise(s%graph_dir)
    call normalise(s%logo)
    call normalise(s%mathjax_config)
    call normalise(s%md_base_dir)
    call normalise(s%media_dir)
    call normalise(s%output_dir)
    call normalise(s%page_dir)
    call normalise_list(s%copy_subdir)
    call normalise_list(s%exclude_dir)
    call normalise_list(s%html_template_dir)
    call normalise_list(s%include)
    call normalise_list(s%src_dir)
    if (s%relative) s%project_url = s%output_dir
    s%creation_date = strftime(s%creation_date)

    do k = 1, size(s%src_dir)
        if (s%src_dir(k)%s == s%output_dir .or. starts_with(s%src_dir(k)%s, s%output_dir//'/')) then
            stat = 1
            errmsg = 'Source directory '//s%src_dir(k)%s//' is a subdirectory of output directory '//s%output_dir
            return
        end if
    end do
    if (.not. s%preprocess) s%fpp_extensions = [string_t ::]

contains

    !> Make `path` absolute, if it is set.
    subroutine normalise(path)
        character(len=:), allocatable, intent(inout) :: path    !! the path
        if (allocated(path)) path = resolve_path(s%directory, path)
    end subroutine normalise

    !> Make every path in `list` absolute.
    subroutine normalise_list(list)
        type(string_t), intent(inout) :: list(:)    !! the paths
        integer :: j
        do j = 1, size(list)
            list(j)%s = resolve_path(s%directory, list(j)%s)
        end do
    end subroutine normalise_list

end subroutine finish_settings

!> The settings as a JSON object, for comparison with FORD's. Unset options are `null`.
function dump_settings(s) result(text)
    type(settings_t), intent(in) :: s       !! the settings
    character(len=:), allocatable :: text
    type(json_core) :: json
    type(json_value), pointer :: root, group
    integer :: k

    call json%initialize()
    call json%create_object(root, '')
    call put_text('author', s%author)
    call put_text('author_description', s%author_description)
    call put_text('author_pic', s%author_pic)
    call put_text('bitbucket', s%bitbucket)
    call put_text('css', s%css)
    call put_text('directory', s%directory)
    call put_text('doc_license', s%doc_license)
    call put_text('docmark', s%docmark)
    call put_text('docmark_alt', s%docmark_alt)
    call put_text('email', s%email)
    call put_text('encoding', s%encoding)
    call put_text('facebook', s%facebook)
    call put_text('github', s%github)
    call put_text('gitlab', s%gitlab)
    call put_text('gitter_sidecar', s%gitter_sidecar)
    call put_text('google_plus', s%google_plus)
    call put_text('graph_dir', s%graph_dir)
    call put_text('license', s%license)
    call put_text('logo', s%logo)
    call put_text('linkedin', s%linkedin)
    call put_text('mathjax_config', s%mathjax_config)
    call put_text('md_base_dir', s%md_base_dir)
    call put_text('media_dir', s%media_dir)
    call put_text('output_dir', s%output_dir)
    call put_text('page_dir', s%page_dir)
    call put_text('predocmark', s%predocmark)
    call put_text('predocmark_alt', s%predocmark_alt)
    call put_text('preprocessor', s%preprocessor)
    call put_text('privacy_policy_url', s%privacy_policy_url)
    call put_text('project', s%project)
    call put_text('project_bitbucket', s%project_bitbucket)
    call put_text('project_download', s%project_download)
    call put_text('project_github', s%project_github)
    call put_text('project_gitlab', s%project_gitlab)
    call put_text('project_sourceforge', s%project_sourceforge)
    call put_text('project_url', s%project_url)
    call put_text('project_website', s%project_website)
    call put_text('revision', s%revision)
    call put_text('sort', s%sort)
    call put_text('summary', s%summary)
    call put_text('terms_of_service_url', s%terms_of_service_url)
    call put_text('twitter', s%twitter)
    call put_text('version', s%version)
    call put_text('website', s%website)
    call put_text('year', s%year)
    call put_logical('coloured_edges', s%coloured_edges)
    call put_logical('dbg', s%dbg)
    call put_logical('doxygen', s%doxygen)
    call put_logical('externalize', s%externalize)
    call put_logical('fixed_length_limit', s%fixed_length_limit)
    call put_logical('force', s%force)
    call put_logical('graph', s%graph)
    call put_logical('hide_undoc', s%hide_undoc)
    call put_logical('incl_src', s%incl_src)
    call put_logical('lower', s%lower)
    call put_logical('preprocess', s%preprocess)
    call put_logical('print_creation_date', s%print_creation_date)
    call put_logical('proc_internals', s%proc_internals)
    call put_logical('quiet', s%quiet)
    call put_logical('relative', s%relative)
    call put_logical('search', s%search)
    call put_logical('show_proc_parent', s%show_proc_parent)
    call put_logical('source', s%source)
    call put_logical('warn', s%warn)
    call json%add(root, 'graph_maxdepth', s%graph_maxdepth)
    call json%add(root, 'graph_maxnodes', s%graph_maxnodes)
    call json%add(root, 'max_frontpage_items', s%max_frontpage_items)
    call put_list('copy_subdir', s%copy_subdir)
    call put_list('display', s%display)
    call put_list('exclude', s%exclude)
    call put_list('exclude_dir', s%exclude_dir)
    call put_list('extensions', s%extensions)
    call put_list('extra_vartypes', s%extra_vartypes)
    call put_list('fixed_extensions', s%fixed_extensions)
    call put_list('fpp_extensions', s%fpp_extensions)
    call put_list('html_template_dir', s%html_template_dir)
    call put_list('include', s%include)
    call put_list('macro', s%macro)
    call put_list('md_extensions', s%md_extensions)
    call put_list('src_dir', s%src_dir)
    call put_map('alias', s%alias)
    call put_map('external', s%external)
    call put_map('extra_mods', s%extra_mods)
    call json%create_object(group, 'extra_filetypes')
    call json%add(root, group)
    do k = 1, size(s%extra_filetypes)
        call json%add(group, s%extra_filetypes(k)%extension, s%extra_filetypes(k)%comment)
    end do
    call json%serialize(root, text)
    call json%destroy(root)

contains

    !> Add a text option, `null` if it is not set.
    subroutine put_text(key, value)
        character(len=*), intent(in) :: key                 !! the option
        character(len=:), allocatable, intent(in) :: value  !! its value
        if (allocated(value)) then
            call json%add(root, key, value)
        else
            call json%add(root, key)
        end if
    end subroutine put_text

    !> Add a logical option.
    subroutine put_logical(key, value)
        character(len=*), intent(in) :: key     !! the option
        logical, intent(in) :: value            !! its value
        call json%add(root, key, value)
    end subroutine put_logical

    !> Add a list option.
    subroutine put_list(key, list)
        character(len=*), intent(in) :: key     !! the option
        type(string_t), intent(in) :: list(:)   !! its values
        type(json_value), pointer :: array
        integer :: j
        call json%create_array(array, key)
        call json%add(root, array)
        do j = 1, size(list)
            call json%add(array, '', list(j)%s)
        end do
    end subroutine put_list

    !> Add a map option.
    subroutine put_map(key, map)
        character(len=*), intent(in) :: key         !! the option
        type(map_entry_t), intent(in) :: map(:)     !! its entries
        type(json_value), pointer :: object
        integer :: j
        call json%create_object(object, key)
        call json%add(root, object)
        do j = 1, size(map)
            call json%add(object, map(j)%key, map(j)%value)
        end do
    end subroutine put_map

end function dump_settings

end module fdoc_settings
