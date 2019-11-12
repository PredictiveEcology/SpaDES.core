# SpaDES

<details>

* Version: 2.0.4
* Source code: https://github.com/cran/SpaDES
* URL: http://spades.predictiveecology.org, https://github.com/PredictiveEcology/SpaDES
* BugReports: https://github.com/PredictiveEcology/SpaDES/issues
* Date/Publication: 2019-09-17 14:00:02 UTC
* Number of recursive dependencies: 168

Run `revdep_details(,"SpaDES")` for more info

</details>

## Newly broken

*   checking whether package ‘SpaDES’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES/new/SpaDES.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘SpaDES’ ...
** package ‘SpaDES’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
Warning: version 0.2.10.9007 of ‘reproducible’ masked by 0.2.10 in /home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/library/SpaDES
Error: package or namespace load failed for ‘SpaDES’:
 .onAttach failed in attachNamespace() for 'SpaDES', details:
  call: NULL
  error: package ‘reproducible’ 0.2.10 is loaded, but >= 0.2.10.9001 is required by ‘SpaDES.core’
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES/new/SpaDES.Rcheck/SpaDES’

```
### CRAN

```
* installing *source* package ‘SpaDES’ ...
** package ‘SpaDES’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** testing if installed package can be loaded from final location
Warning in fun(libname, pkgname) : couldn't connect to display ":99"
** testing if installed package keeps a record of temporary installation path
* DONE (SpaDES)

```
