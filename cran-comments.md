## resubmission

This is a minor update to our package.

## Test environments

### Previous R versions
* Ubuntu 14.04.5      (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.2
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3

### Current R versions
* OSX Sierra 10.12.6 (travis-ci), R 3.4.3 YES
* Ubuntu 14.04.5     (travis-ci), R 3.4.2 YES
* Debian 4.9.51          (local), R 3.4.3
* Windows             (appveyor), R 3.4.3
* Windows          (win-builder), R 3.4.3
* Windows 7              (local), R 3.4.3 YES

### Development R version
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2018-01-24 r74157)
* Windows                 (local), R 3.5.0 (2018-01-24 r74157)
* Windows           (win-builder), R 3.5.0 (2018-01-24 r74157)

## R CMD check results

There are no errors, warnings, or notes in any of the above.

There were 1 NOTE, which is not a new note for this package:

    Package suggested but not available for checking: 'fastshp'

We provide instructions for the user to install the suggested `fastshp` package.

The `fastshp` package in Suggests is optionally installed from Rforge and not required to use the package; it simply speeds up some of the plotting. Instructions for installation are provided in the README and via a message to the user. We believe this should satisfy the CRAN policy requirement regarding additional dependencies.

        Suggests or Enhances not in mainstream repositories:
          fastshp
      
        Availability using Additional_repositories specification:
          fastshp   yes   http://rforge.net



## Downstream dependencies

There are currently no downstream dependencies of this package.
However, as we submit further `SpaDES` spinoff packages, this package will become a dependency for the following packages:

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
