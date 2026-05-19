## Release information

This is a minor release. Highlights:

* New opt-in v2 static code-checking engine (`options(spades.codeCheckEngine = "v2")`),
  plus standalone `codeCheckModule()` / `checkModuleMetadata()` APIs.
* The `NLMR` dependency has been fully removed; sample modules and vignettes now
  use `SpaDES.tools::neutralLandscapeMap()`.
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
* Windows                 (win-builder), R-devel (TODO: fill exact revision from win-builder result)

## R CMD check results

There are no errors, warnings, or notes in any of the above.

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results 
across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
