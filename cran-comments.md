## Maintenance release

This release fixes several issue detected by CRAN checks.
I was unable to reproduce the previous errors seen on Fedora and Solaris.
Checks using `rhub` for those platforms came back clean.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 3.6.3
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Windows                      (GitHub), R 3.6.3
* Windows                      (GitHub), R 4.0.5
* Windows                 (win-builder), R 4.0.5

### Current R versions
* macOS 10.15.7 Catalina       (GitHub), R 4.1.1
* macOS 11.1 Big Sur            (local), R 4.1.1
* macOs (m1) Big Sur             (rhub), R 4.1.1
* Ubuntu 20.04                 (GitHub), R 4.1.1
* Ubuntu 20.04                  (local), R 4.1.1
* Windows                      (GitHub), R 4.1.1
* Windows                       (local), R 4.1.1
* Windows                 (win-builder), R 4.1.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2021-09-26 r80964)
* Ubuntu 20.04                  (local), R-devel (2021-09-28 r80978)
* Windows                      (GitHub), R-devel (2021-09-26 r80964)
* Windows                 (win-builder), R-devel (2021-09-23 r80951)

## R CMD check results

There are no ERRORs nor WARNINGs.

There was 1 NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          workflow (6:58)
          workflows (10:64)

## Downstream dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
