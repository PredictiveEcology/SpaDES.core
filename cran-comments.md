## Updated release

This release attempts to fix GLPK errors when GLPK is installed but `igraph` hasn't been compiled with it.
It also addresses compatibility with the forthcoming `RandomFields` v3.3.4.

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
* Ubuntu 14.04       (travis-ci), R 3.6.0 (2019-02-11 r76084)
* Ubuntu 18.04           (local), R 3.6.0 (2019-02-10 r76083)
* Windows             (appveyor), R 3.6.0 (2019-02-10 r76083)
* Windows          (win-builder), R 3.6.0 (2019-02-07 r76069)

## R CMD check results

There are no ERRORs nor WARNINGs.

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.
