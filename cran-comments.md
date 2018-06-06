## Resubmission

This is a update to our package with several enhancments, bugfixes and internal changes, as described in NEWS.md

## Test environments

### Previous R versions
* Ubuntu 14.04        (travis-ci), R 3.1.0
* Ubuntu 14.04        (travis-ci), R 3.2.0
* Ubuntu 14.04        (travis-ci), R 3.3.0
* Ubuntu 14.04        (travis-ci), R 3.4.0
* Windows              (appveyor), R 3.1.0
* Windows              (appveyor), R 3.2.0
* Windows              (appveyor), R 3.3.0
* Windows              (appveyor), R 3.4.0

### Current R versions
* macOS High Sierra    (local), R 3.5.0
* OS X El Capitan  (travis-ci), R 3.5.0
* Ubuntu 14.04     (travis-ci), R 3.5.0
* Ubuntu 18.04         (local), R 3.5.0
* Windows           (appveyor), R 3.5.0
* Windows        (win-builder), R 3.5.0
* Windows 7            (local), R 3.5.0

### Development R version
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-05 r74851)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-05 r74852)
* Windows           (appveyor), R 3.6.0 (2018-06-05 r74852)
* Windows        (win-builder), R 3.6.0 (2018-06-05 r74852)

## R CMD check results

There is 1 *intermittent* ERROR related to a missing `tk85.dll` on windows R 3.4.3, 32 bit.
We have tested it on Windows 32 bit with R 3.4.0 (no error), R 3.4.1  (no error), R 3.4.2 (error) and R 3.4.3 (intermittent error), R 3.5.0 (no error) Windows, and it only occurs on R 3.4.3.
There is are no other combinations of Windows, Linux, Mac and R-old release, R-release, and R-devel to have the error.
The error indicates that `tk85.dll` is not present; however, `tk86.dll` *is* present and delivered with R, so there is something in some internals somewhere that is searching for a mismatched dll between the one being requested and the one that is shipped with R.
This error is occurring because of the package `RandomFields` from which we import one function.
We feel that this is not something that our package can address. 

There are no WARNINGs.

There was 1 NOTE:

1. Maintainer's email address has changed (notified CRAN 2018-05-28).

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Alex M Chubaty <alex.chubaty@gmail.com>'
        
    New maintainer:
      Alex M Chubaty <alex.chubaty@gmail.com>
    Old maintainer(s):
      Alex M Chubaty <alexander.chubaty@canada.ca>

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed, except as noted below.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.


`SpaDES` and `SpaDES.addins` both show a `Tk` DISPLAY warning due to being run in a headless environment:

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/SpaDES/SpaDES.core/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking whether package ‘SpaDES.addins’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/SpaDES/SpaDES.core/revdep/checks/SpaDES.addins.Rcheck/00install.out’ for details.
```
