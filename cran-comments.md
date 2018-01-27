## Resubmission

This is a major update to our package. Many external and internal changes, as described in NEWS.md

## Test environments

### Previous R versions
* Ubuntu 14.04.5      (travis-ci), R 3.3.3
* Windows              (appveyor), R 3.3.2
* Windows              (appveyor), R 3.3.3
* Windows 7               (local), R 3.3.3
* Windows 7               (local), R 3.4.0
* Windows 7               (local), R 3.4.1
* Windows 7               (local), R 3.4.2

### Current R versions
* OSX Sierra 10.12.6 (travis-ci), R 3.4.3 
* Ubuntu 14.04.5     (travis-ci), R 3.4.2
* Debian 4.9.51          (local), R 3.4.3
* Windows             (appveyor), R 3.4.3
* Windows          (win-builder), R 3.4.3
* Windows 7              (local), R 3.4.3

### Development R version
* Ubuntu 14.04        (travis-ci), R 3.5.0 (2018-01-24 r74157)
* Windows                 (local), R 3.5.0 (2018-01-24 r74157) 

## R CMD check results

There is one error related to a missing tk85.dll on win-builder 32 bit. We have tested it on Windows 32 bit with R 3.4.0 (no error), R 3.4.1  (no error), R 3.4.2 (error) and R 3.4.3 (error), R 3.5 (R-devel) (no error) Windows, and it only occurs on R 3.4.3 . There is are no other combinations of Windows, Linux, Mac and R-old release, R-release, and R-devel to have the error. The error indicates that tk85.dll is not present; however, tk86.dll *is* present and delivered with R, so there is something in some internals somewhere that is searching for a mismatched dll between the one being requested and the one that is shipped with R. We feel that this is not something that our package can address. 

## Downstream dependencies

There are currently 2 downstream dependencies of this package. There are no errors, warnings or notes arising from this package on those downstream packages.

- `SpaDES` (Imports)
- `SpaDES.addins` (Imports)
