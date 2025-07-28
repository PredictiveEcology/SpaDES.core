## Release information

This is a maintenance release to fix use of package anchors in help files,
and deal with changes in dependency packages.
See `NEWS.md` for a full list of changes.

## Test environments

### Previous R versions
* Ubuntu 24.04                 (GitHub), R 4.3.3, 4.4.3
* Windows                      (GitHub), R 4.3.3, 4.4.3
* Windows                 (win-builder), R 4.4.3

### Current R versions
* macOS 13.3.1            (mac-builder), R 4.4.3
* macOS 14.7.6                 (GitHub), R 4.4.3
* macOS 15.5                    (local), R 4.4.3
* Ubuntu 24.04                 (GitHub), R 4.4.3
* Ubuntu 24.04                  (local), R 4.4.3
* Windows                      (GitHub), R 4.4.3
* Windows                       (local), R 4.4.3
* Windows                 (win-builder), R 4.4.3

### Development R version
* Ubuntu 24.04                 (GitHub), R-devel (2025-07-15 r88411)
* Ubuntu 24.04                  (local), R-devel (2025-07-15 r88411)
* Windows                      (GitHub), R-devel (2025-07-27 r88459 ucrt)
* Windows                 (win-builder), R-devel (2025-07-27 r88459 ucrt)

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

2. Found (possibly) invalid URLs for multiple DOIs, which appear to be inaccessible from the CRAN check machines but are available in a web browser.

3. We have made an effort to reduce the package as much as possible, but the installed package size is larger than the 5MB limit.

        checking installed package size ... NOTE
            installed size is  6.9Mb
            sub-directories of 1Mb or more:
              R   5.1Mb

## Downstream dependencies

We checked 1 reverse dependency (`SpaDES`) from CRAN, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
