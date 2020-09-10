## Updated release

This release fixes an error in an example caused by a recent change in dependency `Require`.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 18.04                 (GitHub), R 3.6.3
* Windows                      (GitHub), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.15.6 Catalina       (GitHub), R 4.0.0
* macOS 10.15.5 Catalina        (local), R 4.0.0
* Ubuntu 18.04                 (GitHub), R 4.0.0
* Ubuntu 20.04                  (local), R 4.0.0
* Windows                      (GitHub), R 4.0.0
* Windows                 (win-builder), R 4.0.0

### Development R version
* Ubuntu 18.04                 (GitHub), R 4.1.0 (2020-08-17 r79033)
* Ubuntu 20.04                  (local), R 4.1.0 (2020-08-18 r79041)
* Windows                      (GitHub), R 4.1.0 (2020-08-17 r79033)
* Windows                 (win-builder), R 4.1.0 (2020-08-18 r79041)

## R CMD check results

There are no ERRORs nor WARNINGs.

There was 1 NOTE:

1. Some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          workflow (6:58)
          workflows (10:64)

## Downstream dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
