## Release information

This release drops support for R version 4.1 due to changes in dependencies.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3                 (GitHub), R 4.3.3
* macOS 13.3.1            (mac-builder), R 4.3.3
* macOS 14.4.1                  (local), R 4.3.3
* Ubuntu 20.04                 (GitHub), R 4.3.3
* Ubuntu 20.04                  (local), R 4.3.3
* Windows                      (GitHub), R 4.3.3
* Windows                       (local), R 4.3.3
* Windows                 (win-builder), R 4.3.3

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2024-04-09 r86391)
* Ubuntu 20.04                  (local), R-devel (2024-04-10 r86396)
* Windows                      (GitHub), R-devel (2024-04-09 r86391 ucrt)
* Windows                 (win-builder), R-devel (2024-04-09 r86391 ucrt)

## R CMD check results

There are no errors, or warnings in any of the above.

There are some NOTEs:

1. The `NLMR` packages in Suggests are optionally installed from our R-universe repository
  (until the maintainers of that package are able to get it back on CRAN).
  Instructions for installation are provided in the README, DESCRIPTION, and via a message to the user.
  We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          NLMR
        Availability using Additional_repositories specification:
          NLMR         yes   https://predictiveecology.r-universe.dev/

2. We have made an effort to reduce the package as much as possible, but the installed package size is larger than the 5MB limit.

        checking installed package size ... NOTE
            installed size is  6.4Mb
            sub-directories of 1Mb or more:
              R   4.7Mb

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
