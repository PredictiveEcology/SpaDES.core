## Release information

This is a patch release that fixes the check ERRORs in 3.1.0 on
r-devel-linux-x86_64-fedora-gcc, the macOS builders, and M1mac.

Cause: the `randomLandscapes` sample module called
`SpaDES.tools::neutralLandscapeMap()` via a path that required the
non-mainstream package `NLMR` whenever the installed `SpaDES.tools` was the
CRAN version (2.1.1). Since CRAN does not install `NLMR`, the tests that run
that module errored.

Fix: the module's landscape generator now degrades gracefully with no hard
dependency — it uses the built-in `gaussian` generator with
`SpaDES.tools (>= 2.1.2)`, NLMR's `nlm_mpd` only if a user happens to have
`NLMR` installed, and otherwise a zero-dependency `terra` fallback. `NLMR`
has been removed entirely as a declared dependency: it is no longer in
`Suggests`, `Additional_repositories`, or the `Description` field.

Other highlights (carried from 3.1.0):

* New opt-in v2 static code-checking engine (`options(spades.codeCheckEngine = "v2")`),
  plus standalone `codeCheckModule()` / `checkModuleMetadata()` APIs.
* `Plots()` refactor: deterministic filenames, optional caching, and direct
  `ggplot` input.
* Per-event cache key change and a changed `restartSpades()` default
  (both documented in NEWS.md).
* Requires `reproducible` >= 3.0.0.
* Numerous documentation/vignette accuracy fixes and minor bugfixes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 14.7.6                 (GitHub), R 4.5.2
* Ubuntu 24.04                 (GitHub), R 4.5.2
* Ubuntu 24.04                  (local), R 4.5.3
* Windows                      (GitHub), R 4.5.2
* Windows                       (local), R 4.5.3
* Windows                 (win-builder), R 4.5.x

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel
* Ubuntu 24.04                  (local), R-devel
* Windows                 (win-builder), R-devel

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. (`NLMR` is no longer declared, so
the previous `Additional_repositories` / non-mainstream-Suggests NOTE no
longer applies.)

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results 
across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
