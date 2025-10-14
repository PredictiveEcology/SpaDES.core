## Release information

This is a minor release to address an intermittent package failure on 
one of CRAN's flavours.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 13.3.1            (mac-builder), R 4.5.1
* macOS 14.7.6                 (GitHub), R 4.5.1
* macOS 26.0.1                  (local), R 4.5.1
* Ubuntu 24.04                 (GitHub), R 4.5.1
* Ubuntu 24.04                  (local), R 4.5.1
* Windows                      (GitHub), R 4.5.1
* Windows                       (local), R 4.5.1
* Windows                 (win-builder), R 4.5.1

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel (2025-10-08 r88906)
* Ubuntu 24.04                  (local), R-devel (2025-10-08 r88906)
* Windows                      (GitHub), R-devel (2025-10-08 r88906 ucrt)
* Windows                 (win-builder), R-devel (2025-10-08 r88906 ucrt)

## R CMD check results

There are no errors, or warnings in any of the above.

There is one NOTE:

1. The `NLMR` package in Suggests is optionally installed from our R-universe repository
  (until the maintainers of that package are able to get it back on CRAN).
  Instructions for installation are provided in the README, DESCRIPTION, and via a message to the user.
  We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          NLMR
        Availability using Additional_repositories specification:
          NLMR         yes   https://predictiveecology.r-universe.dev/


## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
