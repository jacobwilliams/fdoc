#!/usr/bin/env python
"""Run fdoc on real projects, each pinned to a commit, and check the sites it generates.

    pixi run test-projects [NAME ...]

A project passes if fdoc succeeds, the site has the expected number of pages, and it has no more broken
internal links than the project's own text causes. Uses the debug build of ``pixi run test``, or another
with ``FDOC_PROFILE=release``. Projects are cloned to ``build/projects``.
"""

import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from check_links import check  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
WORK = ROOT / "build" / "projects"

# name: repository, commit, project file, pages, broken links that come from the project's own text
PROJECTS = {
    "fformat": ("jacobwilliams/fformat", "0073d0546b1c916590d03914f26f4d21ffff84f1", "ford.md", 248, 0),
    "bspline-fortran": ("jacobwilliams/bspline-fortran", "047c724451aeee7f883c0d5acba3f7bd22a5de01", "ford.md", 166, 0),
    "json-fortran": ("jacobwilliams/json-fortran", "d9ed2e4ee53745f4ed98d7536e17bee5198aa3b0", "ford.md", 382, 2),
    "toml-f": ("toml-f/toml-f", "f9ff54793b2d1a3edca7d6890548d18d48c98e36", "fpm.toml", 228, 0),
    "fpm": ("fortran-lang/fpm", "b30ca67abf4a616234d856ca565c7cfad0bcafac", "docs.md", 656, 1),
    "stdlib": ("fortran-lang/stdlib", "1903780b7c616eaf55e90e515214e9813dde3284", "API-doc-FORD-file.md", 3641, 229),
}


def fetch(name, repository, commit):
    """The checkout of a project at a commit, cloning it if needed."""
    path = WORK / name
    marker = path / ".fdoc-commit"
    if marker.exists() and marker.read_text() == commit:
        return path
    shutil.rmtree(path, ignore_errors=True)
    path.mkdir(parents=True)
    for arguments in (
        ["init", "-q"],
        ["fetch", "-q", "--depth", "1", f"https://github.com/{repository}.git", commit],
        ["-c", "advice.detachedHead=false", "checkout", "-q", "FETCH_HEAD"],
    ):
        subprocess.run(["git", *arguments], cwd=path, check=True)
    marker.write_text(commit)
    return path


def fdoc_binary():
    """The path of fdoc, built if needed."""
    profile = os.environ.get("FDOC_PROFILE", "debug")
    fpm = shutil.which("fpm")
    subprocess.run([fpm, "build", "--profile", profile], cwd=ROOT, check=True)
    result = subprocess.run(
        [fpm, "run", "fdoc", "--profile", profile, "--runner", "echo"],
        cwd=ROOT, check=True, capture_output=True, text=True,
    )
    return str(ROOT / result.stdout.split()[-1])


def main():
    names = sys.argv[1:] or list(PROJECTS)
    binary = fdoc_binary()
    failures = 0
    for name in names:
        repository, commit, project_file, pages, allowed = PROJECTS[name]
        checkout = fetch(name, repository, commit)
        site = WORK / f"{name}-site"
        shutil.rmtree(site, ignore_errors=True)
        start = time.time()
        result = subprocess.run([binary, "-q", "-o", str(site), project_file], cwd=checkout, capture_output=True, text=True)
        seconds = time.time() - start
        problems = []
        if result.returncode != 0:
            problems.append(f"fdoc exited with {result.returncode}: {result.stderr[-500:]}")
        else:
            failed = [line for line in (result.stdout + result.stderr).splitlines() if "preprocessing failed" in line]
            if failed:
                problems.append(f"{len(failed)} files could not be preprocessed, as in: {failed[0][:300]}")
            count, _, broken, _ = check(site)
            if count != pages:
                problems.append(f"{count} pages, expected {pages}")
            if sum(broken.values()) > allowed:
                problems.append(f"{sum(broken.values())} broken links, at most {allowed} expected")
        print(f"{name}: {seconds:.1f} s: " + ("; ".join(problems) if problems else "ok"), flush=True)
        failures += bool(problems)
    print(f"{failures} of {len(names)} failed")
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
