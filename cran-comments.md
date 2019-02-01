## Updated release

This release includes various bug fixes and enhancements (see NEWS).

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.3.3
* Ubuntu 14.04        (travis-ci), R 3.4.4
* Windows              (appveyor), R 3.3.3
* Windows              (appveyor), R 3.4.4

### Current R versions
* macOS Mojave       (travis-ci), R 3.5.2
* macOS Mojave           (local), R 3.5.2
* Ubuntu 14.04       (travis-ci), R 3.5.2
* Ubuntu 18.04           (local), R 3.5.2
* Windows             (appveyor), R 3.5.2
* Windows          (win-builder), R 3.5.2
* Windows 7              (local), R 3.5.2

### Development R version
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-01-29 r76025)
* Ubuntu 18.04           (local), R 3.6.0 (2019-01-25 r76014)
* Windows             (appveyor), R 3.6.0 (2019-01-26 r76018)
* Windows          (win-builder), R 3.6.0 (2019-01-27 r76018)

## R CMD check results

There are no ERRORs nor WARNINGs.

## Downstream dependencies

We have run R CMD check on downstream dependencies, and all have passed except those noted below.

* Current CRAN versions of `SpaDES` and `SpaDES.addins` produce an ERROR due to a recent change in the dependency package `reproducible`, which has just been updated on CRAN (v0.2.6). These errors are fixed in this version of `SpaDES.core`.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.
