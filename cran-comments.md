## Updated release

This is a maintenance release.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.6.3
* Windows                    (appveyor), R 3.6.3
* Windows                 (win-builder), R 3.6.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 4.0.0
* macOS 10.15.4 Catalina        (local), R 4.0.0
* Ubuntu 16.04              (travis-ci), R 4.0.0
* Ubuntu 18.04                  (local), R 4.0.0
* Windows                    (appveyor), R 4.0.0
* Windows                 (win-builder), R 4.0.0

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.1.0 (2020-05-13 r78453)
* Ubuntu 18.04                  (local), R 4.1.0 (2020-05-13 r78456)
* Windows                    (appveyor), R 4.1.0 (2020-05-12 r78431)
* Windows                 (win-builder), R 4.1.0 (2020-05-11 r78411)

## R CMD check results

There are no ERRORs nor WARNINGs.

There was 1 NOTE:

1. Some words were flagged as possibly mispelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
          workflow (6:58)
          workflows (10:64)

## Downstream dependencies

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
