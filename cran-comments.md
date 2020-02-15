## Updated release

This is a major release that works with new backend of `reproducible` package. Other user-visible changes concern the converting of print and cat statements to messages. This enables a user suppressing these more easily, a feature requested by CRAN for one of our downstream package submissions, `SpaDES.experiment`.

See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 16.04              (travis-ci), R 3.5.3
* Windows                    (appveyor), R 3.5.3
* Windows                 (win-builder), R 3.5.3

### Current R versions
* macOS 10.13.3 High Sierra (travis-ci), R 3.6.1
* macOS 10.15.1 Catalina        (local), R 3.6.1
* Ubuntu 16.04              (travis-ci), R 3.6.1
* Ubuntu 18.04                  (local), R 3.6.1
* Windows                    (appveyor), R 3.6.1
* Windows                 (win-builder), R 3.6.1

### Development R version
* Ubuntu 16.04              (travis-ci), R 4.0.0 (2019-11-18 r77436)
* Ubuntu 18.04                  (local), R 4.0.0 (2019-11-21 r77446)
* Windows                    (appveyor), R 4.0.0 (2019-11-18 r77436)
* Windows                 (win-builder), R 4.0.0 (2019-11-20 r77445)

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
