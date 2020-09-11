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
* Ubuntu 18.04                 (GitHub), R 4.1.0 (2020-09-08 r79165)
* Ubuntu 20.04                  (local), R 4.1.0 (2020-09-10 r79180)
* Windows                      (GitHub), R 4.1.0 (2020-09-09 r79174)
* Windows                 (win-builder), R 4.1.0 (2020-09-09 r79174)

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
