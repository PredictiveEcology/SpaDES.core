## Release information

This is a resubmission to restore the package to CRAN following archival due to removal of dependency package `Require`, which has now been restored to CRAN.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Windows                       (local), R 4.1.3
* Windows                      (GitHub), R 4.1.3
* Windows                 (win-builder), R 4.1.3

### Current R versions
* macOS 11.7 Big Sur           (GitHub), R 4.2.2
* macOS 11.7 Big Sur            (local), R 4.2.2
* macOs (m1) Big Sur             (rhub), R 4.2.2
* Ubuntu 20.04                 (GitHub), R 4.2.2
* Ubuntu 20.04                  (local), 4.2.2 Patched (2022-11-10 r83330)
* Windows                      (GitHub), R 4.2.2
* Windows                       (local), R 4.2.2
* Windows                 (win-builder), R 4.2.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-01-11 r83598)
* Ubuntu 20.04                  (local), R-devel (2023-01-07 r83578)
* Windows                      (GitHub), R-devel (2023-01-11 r83598 ucrt)
* Windows                 (win-builder), R-devel (2023-01-11 r83598 ucrt))
* Windows                       (local), R-devel (2023-01-08 r83584 ucrt)

## R CMD check results

There are no errors, or warnings in any of the above.

There are some NOTEs:

The `NLMR` packages in Suggests are optionally installed from our R-universe repository
(until the maintainers of that package are able to get it back on CRAN).
Instructions for installation are provided in the README, DESCRIPTION, and via a message to the user.
We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          NLMR
        Availability using Additional_repositories specification:
          NLMR         yes   https://predictiveecology.r-universe.dev/

Related to the above note:

        The Description field contains
          "https://PredictiveEcology.r-universe.dev")'.
        Please enclose URLs in angle brackets (<...>).

This URL is included as part of the command used to install these additional packages.

Additionally, some words were flagged as possibly misspelled, but they are false positives:

        Possibly mis-spelled words in DESCRIPTION:
            DES (5:71, 7:14, 12:53)
            modularity (7:40)


## Downstream dependencies

Currently none, but we are working to resubmit our other packages that depend on this one, and they are passing.
