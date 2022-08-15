## Maintenance release

This release fixes various issues with packages dependencies and CRAN check problems.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.0.5
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.0.5
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.6 Big Sur           (GitHub), R 4.2.1
* macOS 11.6 Big Sur            (local), R 4.2.1
* macOs (m1) Big Sur             (rhub), R 4.2.1
* Ubuntu 20.04                 (GitHub), R 4.2.1
* Ubuntu 20.04                  (local), R 4.2.1
* Windows                      (GitHub), R 4.2.1
* Windows                       (local), R 4.2.1
* Windows                 (win-builder), R 4.2.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2022-08-11 r82713)
* Ubuntu 20.04                  (local), R-devel (2022-08-11 r82713)
* Windows                      (GitHub), R-devel (2022-08-11 r82713)
* Windows                 (win-builder), R-devel (2022-08-11 r82713 ucrt)

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
