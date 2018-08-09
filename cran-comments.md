## Updated release

We have introduced several new functions and bug fixes (see NEWS.md).

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
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-08-03 r75056)
* Ubuntu 18.04         (local), R 3.6.0 (2018-07-30 r75011)
* Windows           (appveyor), R 3.6.0 (2018-08-02 r75051)
* Windows        (win-builder), R 3.6.0 (2018-08-07 r75080)

## R CMD check results

There are no ERRORs nor WARNINGs nor NOTEs.

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed, except as noted below.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.

* `NetLogoR` shows a note regarding a non-CRAN package in Suggests.

* `SpaDES` and `SpaDES.addins` both show a warning due to `DISPLAY` not being set in the headless session.
