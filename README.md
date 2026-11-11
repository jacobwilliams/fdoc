# fdoc

A documentation generator for modern Fortran, written in modern Fortran.

fdoc reads the same project files and doc comments as [FORD](https://github.com/Fortran-FOSS-Programmers/ford), so it can take over from FORD on an existing project without changing the code. It runs from a single binary, with no Python, and generates a site 50 to 200 times faster.

* **Fast.** Documentation for a 25,000-line project takes seconds, graphs included, where FORD takes minutes. See the [benchmarks](#benchmarks).
* **Compatible.** It reads FORD project files, doc comments, Markdown and `modules.json`, and generates the same page URLs, apart from a few [listed below](#differences-from-ford), so existing links keep working.
* **Clean output.** Light and dark themes, a layout that works on a phone, and instant search with typo tolerance. The site needs no framework, and its CSS and JavaScript are built into the program.
* **Small output.** Sites take about a third of the space of FORD's.

fdoc is in early development and has only been tested on macOS with gfortran. See [Differences from FORD](#differences-from-ford) for what isn't supported yet.

## Getting started

You need [gfortran](https://gcc.gnu.org/fortran/) 16 and [fpm](https://fpm.fortran-lang.org).

The easiest way to get all of these is [pixi](https://pixi.sh), which installs them for this repository:

```
git clone https://github.com/jacobwilliams/fdoc.git
cd fdoc
pixi run doc ~/my-project/ford.md
```

That builds fdoc and documents the project described by `ford.md`, writing to its `output_dir`, which is `./doc` next to the project file by default. FORD's output in that directory is replaced, so use `-o` to write somewhere else while comparing the two.

To install the binary:

```
pixi run fpm install --profile release --prefix ~/.local
```

## Usage

```
fdoc [options] [project_file]
```

For a program that shows the progress of fdoc, `--progress-lines` writes lines to standard output in place of the bars, one for each whole percent of each stage, as they happen. Warnings go to standard error, and a run that fails ends with a nonzero exit code and no `[end]` line.

```text
[  0%] Parsing files 0/198
[ 42%] Parsing files 83/198
[100%] Parsing files 198/198
[done] Parsing files in 0.22 s
[end] 3456 pages written to ./doc in 2.31 s
```

The options are FORD's, including `-o`, `-d`, `-p`, `-g`, `-m`, `-I`, `-L`, `-q` and `--config`. Run `fdoc --help` for the full list. Command line options override the project's settings. fdoc uses every core unless `parallel` says otherwise.

Without a project file, fdoc looks in the current directory for an `[extra.fdoc]` or `[extra.ford]` table in `fpm.toml`, and then for `ford.md`.

### Configuration

A FORD project file works as it is:

```
project: My Project
src_dir: ./src
output_dir: ./doc
display: public
         private
source: true
graph: true

The text of the front page, in Markdown.
```

Or put everything in `fpm.toml`, and have no project file at all. fpm ignores what's under `[extra]`:

```toml
[extra.fdoc]
project = "My Project"
src_dir = ["./src"]
output_dir = "./doc"
display = ["public", "private"]
source = true
graph = true
front_page_file = "README.md"
```

`front_page_file` names a Markdown file for the front page, relative to `fpm.toml`. To write the text there instead, use `front_page`. Both are specific to `[extra.fdoc]`, as FORD doesn't read that table. Its own `[extra.ford]` table also works.

### Doc comments

fdoc follows FORD's conventions:

```fortran
!> Add two numbers.
!!
!! The text is Markdown, and can link to other entities: see [[subtract]].
pure function add(a, b) result(c)
    real, intent(in) :: a   !! the first number
    real, intent(in) :: b   !! the second number
    real :: c               !! their sum
    c = a + b
end function add
```

`!!` documents what comes before it and `!>` what comes after it. The markers can be changed with `docmark`, `predocmark`, `docmark_alt` and `predocmark_alt`. Metadata such as `author:` and `summary:`, admonitions, aliases, doxygen commands such as `@brief` and `@param`, math, and file includes all work as they do in FORD.

### What it generates

* Pages for modules, submodules, procedures, interfaces, derived types, programs, block data, namelists and source files, plus static pages from `page_dir`.
* Tables of arguments, components and variables showing intent and attributes as coloured badges.
* Graphs of module use, procedure calls, type inheritance and file dependencies, with links, zoom, panning and a key to the colours.
* Syntax highlighted Fortran source, and JSON and TOML code blocks in the documentation.
* Badges for what procedures are, such as `pure` or `recursive`, and for those that keep state between calls, with arguments coloured by intent.
* A list of source files with their lines of code and comments, size and procedures, and totals, sorted by clicking a heading.
* A sidebar with the whole list a page belongs to, showing where that page is in it.
* [Mermaid](https://mermaid.js.org) diagrams from `mermaid` code blocks, drawn in the browser.
* On the page of a type, where it is used: the procedures that take or return it, the types that hold it and the module variables of that type.
* An index of all names from A to Z, a tree of the derived types by what they extend, and a link on each heading to copy.
* A logo on the main page, from the `logo` setting, which is a path to an image.
* Generic interfaces shown as call forms, with a table of the types and ranks each form takes and what it returns.
* A search box. Press `/` to use it.
* `modules.json`, so that other projects can link to this one. fdoc and FORD can read each other's.

## Differences from FORD

* **Fixed-form source is not supported yet.** Those files are skipped with a warning.
* **`logo` is fdoc's own setting.** FORD doesn't have it.
* **`graph_project_maxnodes` is fdoc's own setting too.** Whole-project graphs have no size limit unless you set one, which leaves out any graph over that many nodes.
* **`html_template_dir` has no effect**, because there are no templates. The HTML is different too, so CSS written for FORD's pages needs rewriting. `css:` still adds your own style sheet.
* **Of the Python-Markdown extensions in `md_extensions`,** only `toc` and `smarty` are available. Others are skipped with a warning.
* **fdoc doesn't run Python programs.** `pcpp` is replaced by `gfortran -E -cpp` when it's the preprocessor.
* **Fypp is built in, in part.** A `preprocessor` of `fypp` uses a port in Fortran of what the Fortran standard library needs, so Python isn't involved. A file using anything else, such as `#:del`, `@:` or Python beyond what stdlib uses, is read without preprocessing, with a warning naming what's missing. Other `preprocessor` commands run as given.
* **Search covers names and summaries,** not the full text of the documentation.
* **Text after a `<`, as in `x<y`, is kept.** FORD's Markdown drops it.
* **`externalize: false` is honored.** FORD's command line default overrides it.
* **A few page URLs differ.** A generic interface named after its only procedure gets its own page in `interface/`, where FORD uses `proc/`, and that can shift the `~2` numbers of other pages with the same name.
* **Private procedures that a shown type binds to are shown.** Whoever can see the type can call them through it, so each gets a page, which the binding links to. FORD lists the interface under the binding but gives it no page. The documentation of the body of a separate module procedure in a submodule that isn't shown goes on the page of its interface.
* **Calls are found from tokens,** so fdoc finds a few that FORD's regular expressions miss.

## Benchmarks

Time to generate each project's documentation, using the project's own settings:

| Project | Lines | FORD | fdoc | Speedup | FORD, no graphs | fdoc, no graphs | Speedup |
|---|---:|---:|---:|---:|---:|---:|---:|
| [fformat](https://github.com/jacobwilliams/fformat) | 6,902 | 27.1 s | 0.1 s | 226× | 3.1 s | <0.1 s | 78× |
| [bspline-fortran](https://github.com/jacobwilliams/bspline-fortran) | 13,222 | 22.8 s | 0.1 s | 163× | 6.5 s | 0.1 s | 72× |
| [rklib](https://github.com/jacobwilliams/rklib) | 17,025 | 62.3 s | 0.2 s | 271× | 8.2 s | 0.2 s | 55× |
| [json-fortran](https://github.com/jacobwilliams/json-fortran) | 17,715 | 64.5 s | 0.6 s | 115× | 6.8 s | 0.2 s | 38× |
| [Fortran-Astrodynamics-Toolkit](https://github.com/jacobwilliams/Fortran-Astrodynamics-Toolkit) | 25,174 | 60.3 s | 0.2 s | 274× | 9.0 s | 0.1 s | 75× |
| [qt-fortran](https://github.com/jacobwilliams/qt-fortran) | 25,831 | 314 s | 1.3 s | 243× | 25.3 s | 0.9 s | 29× |
| [stdlib](https://github.com/fortran-lang/stdlib) | 645,495 | 680 s | 2.3 s | 297× | 211 s | 2.1 s | 101× |

The lines of stdlib are those of its Fypp templates and Fortran sources that get documented, before the templates are expanded. FORD also needs the Python Fypp for it, and fdoc doesn't.

Two more third-party projects: [fpm](https://github.com/fortran-lang/fpm) 0.2 s against 9.0 s (50×), and [TOML Fortran](https://github.com/toml-f/toml-f) 0.1 s against 20.7 s (159×).

Peak memory and the size of the site, with graphs:

| Project | FORD memory | fdoc memory | FORD site | fdoc site |
|---|---:|---:|---:|---:|
| fformat | 134 MB | 22 MB | 17 MB | 5 MB |
| bspline-fortran | 175 MB | 33 MB | 17 MB | 5 MB |
| rklib | 318 MB | 67 MB | 25 MB | 10 MB |
| json-fortran | 310 MB | 72 MB | 83 MB | 30 MB |
| Fortran-Astrodynamics-Toolkit | 197 MB | 49 MB | 31 MB | 9 MB |
| qt-fortran | 425 MB | 115 MB | 100 MB | 33 MB |
| stdlib | 1,912 MB | 694 MB | 429 MB | 121 MB |

Measured on an Apple M5 with 10 cores and 24 GB of memory, with FORD 7.0.13, a release build of fdoc 0.0.1 made with gfortran 16.2, and Graphviz 14.1.2. FORD ran once and fdoc three times, taking the best. FORD's memory doesn't include `dot`.

The comparison isn't exactly like for like. FORD's search index holds the full text, and fdoc's only names and summaries. Graphs take little of fdoc's time on stdlib, about 0.2 s, but most of it on projects with deep call chains, such as json-fortran.

To run them yourself:

```
pixi run bench path/to/ford.md ...
```

## How it works

1. **Settings** come from the project file or `fpm.toml`, with [TOML Fortran](https://github.com/toml-f/toml-f), and the command line.
2. **Files** are found, preprocessed if needed, and read.
3. **Parsing** runs in parallel. [fformat](https://github.com/jacobwilliams/fformat) splits each file into tokens and statements and works out its structure, and fdoc reads the declarations and doc comments from those.
4. **Linking** connects names across the project: `use` statements, inheritance, interfaces, bindings and calls.
5. **Markdown** is converted to HTML in parallel, by a converter that follows the dialect of Python-Markdown.
6. **Graphs** are laid out in parallel by [fdot](https://github.com/jacobwilliams/fdot), a Fortran port of the layout of Graphviz `dot`, with no files and no other process.
7. **Pages** are rendered in parallel and written out.

## Development

The project is built with fpm, and pixi provides everything else:

```
pixi run build           # build fdoc
pixi run test            # run the tests
pixi run doc             # generate fdoc's own documentation in ./doc
pixi run check-docs      # check that every routine and argument is documented
pixi run check-threads   # check that code running in parallel is thread-safe
pixi run check-standard  # check that the code is standard Fortran, with no extensions
pixi run check-links doc # check the links of a generated site
pixi run test-projects   # run fdoc on real projects, each pinned to a commit
pixi run gen-assets      # regenerate src/fdoc_assets.f90 after changing assets/
```

FORD serves as the reference, and `compare` checks fdoc against it on real projects:

```
pixi run compare settings path/to/ford.md ...   # settings
pixi run compare modules path/to/ford.md ...    # modules.json
pixi run compare links path/to/ford.md ...      # uses, calls and inheritance
pixi run compare markdown path/to/ford.md ...   # conversion of doc comments
```

Every routine has an `!>` block and every argument an inline `!!`, which `check-docs` enforces.

Code that runs in parallel must not call functions returning `character(len=:), allocatable`. gfortran 15 and 16 keep the length of such a result in static storage, which threads share, so this gives wrong results in valid code. `example/slen_race.f90` shows the problem. Use an `intent(out)` argument, or a result with an explicit length, and `check-threads` will catch what gets through.

Internal `read` and `write` statements are best kept out of busy parallel loops too. Each one takes a lock in libgfortran, and on 10 threads a plain loop of them runs about 30 times slower than on one. `int_text` turns an integer into text without the lock. Whole files also shouldn't go through functions in parallel code, as the temporary result can overflow a thread's stack.

File statements in parallel code go inside `!$omp critical (file_io)`. In libgfortran 16, file I/O from several threads at once can crash, even on separate units, with a segfault in `check_for_recursive`. `example/io_race.f90` shows the problem.

## Dependencies

| Package | Used for |
|---|---|
| [fdot](https://github.com/jacobwilliams/fdot) | Laying out and drawing graphs |
| [fformat](https://github.com/jacobwilliams/fformat) | Tokens, statements and structure of Fortran source |
| [JSON-Fortran](https://github.com/jacobwilliams/json-fortran) | `modules.json` and the search index |
| [TOML Fortran](https://github.com/toml-f/toml-f) | Settings in `fpm.toml` |
| [fregex](https://github.com/jacobwilliams/fregex) | Exclude patterns, through fformat |
| OpenMP | Parallel parsing and rendering |
| [test-drive](https://github.com/fortran-lang/test-drive) | Tests |

fpm fetches the Fortran packages. FORD and Python are needed only for `compare` and `bench`, not to run fdoc.

Generated sites load [MathJax](https://www.mathjax.org) from a CDN, only on pages that contain math, and [Mermaid](https://mermaid.js.org) likewise, only on pages with a `mermaid` code block.

## License

fdoc is released under the [MIT License](https://github.com/jacobwilliams/fdoc/blob/master/LICENSE). It is an independent program, not derived from FORD's code, and owes its project file format and doc comment conventions to FORD. [src/fdoc_fypp.f90](https://github.com/jacobwilliams/fdoc/blob/master/src/fdoc_fypp.f90) is a partial port of [Fypp](https://github.com/aradi/fypp) by Bálint Aradi, and keeps its BSD 2-clause license notice. Binaries of fdoc include [fdot](https://github.com/jacobwilliams/fdot), a port of part of [Graphviz](https://graphviz.org), which is under the Eclipse Public License 2.0.
