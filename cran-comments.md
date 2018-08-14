## Updated release

This release fixes errors found in tests on macOS.

## Test environments

### Previous R versions
* Ubuntu 14.04     (travis-ci), R 3.1.0
* Ubuntu 14.04     (travis-ci), R 3.2.0
* Ubuntu 14.04     (travis-ci), R 3.3.0
* Ubuntu 14.04     (travis-ci), R 3.4.0
* Windows           (appveyor), R 3.1.0
* Windows           (appveyor), R 3.2.0
* Windows           (appveyor), R 3.3.0
* Windows           (appveyor), R 3.4.0

### Current R versions
* macOS El Capitan     (r-hub), R 3.5.0
* macOS Sierra     (travis-ci), R 3.5.0
* macOS High Sierra    (local), R 3.5.1
* Ubuntu 14.04     (travis-ci), R 3.5.1
* Ubuntu 18.04         (local), R 3.5.1
* Windows           (appveyor), R 3.5.1
* Windows        (win-builder), R 3.5.1
* Windows 7            (local), R 3.5.1

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-08-13 r75128)
* Ubuntu 18.04         (local), R 3.6.0 (2018-08-13 r75128)
* Windows           (appveyor), R 3.6.0 (2018-08-10 r75101)
* Windows        (win-builder), R 3.6.0 (2018-08-11 r75106)

## R CMD check results

There are no ERRORs nor WARNINGs.

There was one NOTE:

    Days since last update: 4

We are resubmitting this fixed version at CRAN's request.

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.
