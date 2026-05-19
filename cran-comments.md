## Release information

This is a minor release. Highlights:

* New opt-in v2 static code-checking engine (`options(spades.codeCheckEngine = "v2")`),
  plus standalone `codeCheckModule()` / `checkModuleMetadata()` APIs.
* `NLMR` is no longer a hard dependency. Sample modules and vignettes call
  `SpaDES.tools::neutralLandscapeMap()`, which uses a built-in generator with
  `SpaDES.tools (>= 2.1.2)`; with the current CRAN `SpaDES.tools` (2.1.1) it
  falls back to a path that uses `NLMR`. `NLMR` is therefore retained as a
  Suggested package, available from the additional repository
  (<https://predictiveecology.r-universe.dev>). It can be dropped entirely
  once `SpaDES.tools` 2.1.2 reaches CRAN.
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

There were no ERRORs or WARNINGs.

There is one NOTE: the suggested package `NLMR` is not available from a
mainstream repository. It is available from the additional repository
declared in `DESCRIPTION` (<https://predictiveecology.r-universe.dev>),
and the `Description` field documents how to install it.

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results 
across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
