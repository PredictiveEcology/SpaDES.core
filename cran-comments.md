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
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-20 r74923)
* Ubuntu 18.04         (local), R 3.6.0 (2018-07-30 r75011)
* Windows           (appveyor), R 3.6.0 (2018-07-23 r75001)
* Windows        (win-builder), R 3.6.0 (2018-07-23 r75001)

## R CMD check results

There are no ERRORs nor WARNINGs nor NOTEs.

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed, except as noted below.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.

* `NetLogoR` shows a note regarding a non-CRAN package in Suggests.

* `SpaDES` and `SpaDES.addins` both show a warning due to `DISPLAY` not being set in the headless session.
