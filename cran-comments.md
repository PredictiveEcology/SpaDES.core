## Release information

This is a new submission of a package that was archived from CRAN due to removal of dependency package `Require`, which has now been restored to CRAN. There are many large changes that have occurred, including very important decreases in number of package dependencies, reducing the probability that future archivals will affect this package. See `NEWS.md` for a full list of changes. The Suggested package that is not on the standard CRAN repositories is supplied with `Additional_repositories`, with instructions provided in the DESCRIPTION.

## Test environments

### Previous R versions
* Ubuntu 20.04                 (GitHub), R 4.1.3
* Ubuntu 20.04                 (GitHub), R 4.2.3
* Windows                      (GitHub), R 4.1.3
* Windows                      (GitHub), R 4.2.3
* Windows                 (win-builder), R 4.2.3

### Current R versions
* macOS 12.6.3 Monterey        (GitHub), R 4.3.1
* macOS (M2) 13.2.1 Ventura     (local), R 4.3.1
* macOS (M1) Big Sur             (rhub), R 4.3.1
* Ubuntu 20.04                 (GitHub), R 4.3.1
* Ubuntu 20.04                  (local), R 4.3.1
* Windows                      (GitHub), R 4.3.1
* Windows                       (local), R 4.3.1
* Windows                 (win-builder), R 4.3.1

### Development R version
* Ubuntu 20.04                 (GitHub), R-devel (2023-07-03 r84633)
* Windows                      (GitHub), R-devel (2023-07-03 r84633 ucrt)
* Windows                 (win-builder), R-devel (2023-07-03 r84633 ucrt)
* Windows                       (local), R-devel (2023-07-03 r84633 ucrt)

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
