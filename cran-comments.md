## Release information

This is a new submission: SpaDES.core was archived from CRAN when its
`Depends:` package `reproducible` was archived (2026-07-13). The archival
was not caused by a problem in SpaDES.core itself. `reproducible` has since
been fixed and is back on CRAN as of 2026-08-25
(<https://cran.r-project.org/web/packages/reproducible/index.html>), so this
package's dependencies are all available again.

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

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3

### Current R versions
* macOS 14.7.6                 (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Ubuntu 24.04                  (local), R 4.5.3
* Windows                      (GitHub), R 4.5.2

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel

<!-- TODO: add win-builder (oldrelease/release/devel) and macOS builder rows
     once those checks are run; they are deferred until `reproducible` is
     back on CRAN, since win-builder cannot install it otherwise. -->

## R CMD check results

There were no ERRORs or WARNINGs.

There is one NOTE, from the incoming feasibility check:

* `New submission` and `Package was archived on CRAN`. Both are expected: this
  package was archived on 2026-07-13 solely because `reproducible` was, as
  CRAN's own comment records ("as requires archived package 'reproducible'").
* Two possibly-invalid URLs, cited in `man/tryCatch.Rd` and
  `man/getModuleVersion.Rd`. Both load in a browser; Stack Overflow returns
  HTTP 403 to automated requests.
* `Suggests or Enhances not in mainstream repositories: SpaDES.tools`.
  SpaDES.tools was archived on 2026-07-13 for the same reason as this package,
  and is being resubmitted; it is used only conditionally, in examples and
  tests guarded by `requireNamespace()`.

## Downstream dependencies

The only reverse dependency, `SpaDES`, is currently archived on CRAN (it was
archived alongside this package). There are therefore no reverse dependencies
on CRAN to check against at this time.
