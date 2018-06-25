## Updated release

This release fixes CRAN concerns about package declarations for tests and vignettes.

The maintainer email address has changed, about which I notified CRAN on March 28, 2018 and sent followup on June 12, 2018 (in response to `fpCompare` submission) and again on June 15, 2018 (in response to `SpaDES` submission).

> Chubaty, Alexander (NRCan/RNCan)
> Fri, Jun 15, 8:42 AM (3 days ago)
> to me, CRAN
>
> Hello CRAN maintainers,
>
> Please note the change in my maintainer email for the following packages:
>
> - fpCompare
> - SpaDES
> - SpaDES.core
> - SpaDES.tools
>
> My new email is alex.chubaty@gmail.com.
>
> Thank you,
> Alex

We also introduced several new functions and bug fixes (see NEWS.md).

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
* Ubuntu 14.04     (travis-ci), R 3.6.0 (2018-06-15 r74903)
* Ubuntu 18.04         (local), R 3.6.0 (2018-06-15 r74903)
* Windows           (appveyor), R 3.6.0 (2018-06-11 r74889)
* Windows        (win-builder), R 3.6.0 (2018-06-15 r74904)

## R CMD check results

There are no ERRORs nor WARNINGs.

There was 1 NOTE:

1. Maintainer's email address has changed (notified CRAN 2018-05-28).

    New maintainer:
      Alex M Chubaty <alex.chubaty@gmail.com>
    Old maintainer(s):
      Alex M Chubaty <alexander.chubaty@canada.ca>

## Downstream dependencies

I have run R CMD check on downstream dependencies and all have passed, except as noted below.

Summary at https://github.com/PredictiveEcology/SpaDES.core/blob/master/revdep/README.md.


* `SpaDES.addins` shows a `Tk` DISPLAY warning due to being run in a headless environment:

    ```
    checking whether package ‘SpaDES.addins’ can be installed ... WARNING
    Found the following significant warnings:
      Warning: no DISPLAY variable so Tk is not available
    See ‘/home/achubaty/Documents/GitHub/SpaDES/SpaDES.core/revdep/checks/SpaDES.addins.Rcheck/00install.out’ for details.
```

* `SpaDES` shows a warning due to `DISPLAY` not being set in the headless session.
  Three other warnings regarding replaced imports are caused by having depracated and moved these functions from `SpaDES.tools` to `reproducible`.
