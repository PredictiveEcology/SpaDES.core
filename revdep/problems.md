# SpaDES.addins

Version: 0.1.1

## Newly broken

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘devtools’ ‘rstudioapi’
      All declared Imports should be used.
    ```

## Newly fixed

*   checking whether package ‘SpaDES.addins’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES.addins/old/SpaDES.addins.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaDES.addins’ ...
** package ‘SpaDES.addins’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** help
*** installing help indices
** building package indices
** installing vignettes
** testing if installed package can be loaded
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
* DONE (SpaDES.addins)

```
### CRAN

```
* installing *source* package ‘SpaDES.addins’ ...
** package ‘SpaDES.addins’ successfully unpacked and MD5 sums checked
** R
** inst
** byte-compile and prepare package for lazy loading
Error : object ‘makeMemoiseable’ is not exported by 'namespace:reproducible'
ERROR: lazy loading failed for package ‘SpaDES.addins’
* removing ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES.addins/old/SpaDES.addins.Rcheck/SpaDES.addins’

```
