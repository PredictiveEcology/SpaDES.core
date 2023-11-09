## Release information

This is an update to deal with several minor bugfixes, plus to accommodate changes in upstream package dependencies with reproducible.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3 Monterey        (GitHub), R 4.3.2
* Ubuntu 20.04                 (GitHub), R 4.3.2
* Ubuntu 20.04                  (local), R 4.3.2
* Windows                      (GitHub), R 4.3.2
* Windows                       (local), R 4.3.2
* Windows                 (win-builder), R 4.3.2

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-11-08 r85496)
* Windows                      (GitHub), R-devel (2023-11-08 r85496 ucrt)
* Windows                 (win-builder), R-devel (2023-11-08 r85496 ucrt)
* Windows                       (local), R-devel (2023-11-08 r85496 ucrt)

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


## Downstream dependencies

Currently none.
