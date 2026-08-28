## Release information

This is a new submission: SpaDES.core was archived from CRAN on 2026-07-13
when its `Depends:` package `reproducible` was archived. The archival was not
caused by a problem in SpaDES.core itself, as CRAN's own comment records
("as requires archived package 'reproducible'").

Both of the dependencies involved are back on CRAN:

* `reproducible` 3.2.1, restored 2026-08-25
* `SpaDES.tools` 2.1.3, restored 2026-08-28

Version 3.2.0 is a minor release accumulated since 3.1.2. Highlights:

* Cached file-backed objects (e.g. `terra` `SpatRaster`) are now handled
  correctly across runs and machines.
* `simInit()` can resume after a partway failure; `restartSimInit()` rewinds
  and restarts the module that actually failed.
* New `codeCheckModules()` checks several modules at once; the module code
  checker gained `# nolint` support, `reqdPkgs` checks, and clearer reports.
* `spades.moduleCodeChecks` now defaults to `FALSE` (behaviour change).
* Event `cacheId` is now stable across machines and operating systems.
* `simInit()` no longer installs or loads the `box` package.
* Numerous bug fixes: progress-bar flooding, `debug` passed as a list,
  `saveSimList()` with non-empty `outputs()`, module metadata under
  `terra` >= 1.9-34, and the `Plots(useCache = TRUE)` ggplot digest.

See NEWS.md for the full list.

## Test environments

### win-builder
* Windows, R 4.5.3 (oldrelease)
* Windows, R 4.6.1 (release)
* Windows, R-devel (2026-08-27 r90452)

### GitHub Actions
* macOS,        R release
* Windows,      R devel, release, oldrel-1, oldrel-2
* Ubuntu 24.04, R devel, release, oldrel-1, oldrel-2
* Ubuntu 24.04, R release with `_R_CHECK_DEPENDS_ONLY_=true`
* Windows,      R release with `_R_CHECK_DEPENDS_ONLY_=true`

### Local
* Ubuntu 24.04, R 4.6.1

## R CMD check results

There were no ERRORs or WARNINGs.

One NOTE is expected and unavoidable:

* `New submission` / `Package was archived on CRAN`. As above, the 2026-07-13
  archival was solely a consequence of `reproducible` being archived, which has
  since been resolved.

A second NOTE appears on some platforms:

* `checking for detritus in the temp directory`. This is a directory left under
  the session temporary directory by the test suite. It is inside `tempdir()`
  and is reclaimed with it, so it does not persist beyond the R session.

## Downstream dependencies

There is one reverse dependency on CRAN, `NetLogoR` 1.0.6, which lists
SpaDES.core under `Suggests:`. It was checked against this version of
SpaDES.core and the result was `Status: OK` (no ERRORs, WARNINGs or NOTEs).

`NetLogoR` refers to SpaDES.core only in files under `inst/examples/`, which
R CMD check does not execute, so its checks do not load SpaDES.core at all.

The other reverse dependency, `SpaDES`, remains archived on CRAN (it was
archived alongside this package) and so could not be checked.
