## Updated release

This release fixes problems with package installation and GLPK-`igraph` detection revealed during CRAN checks.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 18.04                 (GitHub), R 3.6.3
* Windows                      (GitHub), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.15.6 Catalina       (GitHub), R 4.0.3
* macOS 11.1 Big Sur            (local), R 4.0.3
* Ubuntu 18.04                 (GitHub), R 4.0.3
* Ubuntu 20.04                  (local), R 4.0.3
* Windows                      (GitHub), R 4.0.3
* Windows                 (win-builder), R 4.0.3

### Development R version
* Ubuntu 18.04                 (GitHub), R 4.1.0 (2021-01-25 r79883)
* Ubuntu 20.04                  (local), R 4.1.0 (2021-02-15 r80012)
* Windows                      (GitHub), R 4.1.0 (2021-02-11 r79990)
* Windows                 (win-builder), R 4.1.0 (2021-02-11 r79990)

## R CMD check results

There are no ERRORs nor WARNINGs.

There were 2 NOTEs:

1. Some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          workflow (6:58)
          workflows (10:64)

2. Unable to reach the default time server:

        > checking for future file timestamps ... NOTE
          unable to verify current time

## Downstream dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
