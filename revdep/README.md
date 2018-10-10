# Setup

## Platform

|setting  |value                        |
|:--------|:----------------------------|
|version  |R version 3.5.1 (2018-07-02) |
|system   |x86_64, linux-gnu            |
|ui       |RStudio (1.1.456)            |
|language |(EN)                         |
|collate  |en_CA.UTF-8                  |
|tz       |America/Edmonton             |
|date     |2018-10-10                   |

## Packages

|package     |*  |version |date       |source        |
|:-----------|:--|:-------|:----------|:-------------|
|SpaDES.core |*  |0.2.2   |2018-08-13 |cran (@0.2.2) |

# Check results

3 packages

|package       |version | errors| warnings| notes|
|:-------------|:-------|------:|--------:|-----:|
|NetLogoR      |0.3.4   |      0|        0|     0|
|SpaDES.addins |0.1.1   |      0|        1|     0|
|SpaDES        |2.0.2   |      0|        2|     0|

## NetLogoR (0.3.4)
Maintainer: Sarah Bauduin <sarahbauduin@hotmail.fr>  
Bug reports: https://github.com/PredictiveEcology/NetLogoR/issues

0 errors | 0 warnings | 0 notes

## SpaDES.addins (0.1.1)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES.addins/issues

0 errors | 1 warning  | 0 notes

```
checking whether package ‘SpaDES.addins’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES.addins.Rcheck/00install.out’ for details.
```

## SpaDES (2.0.2)
Maintainer: Alex M Chubaty <alex.chubaty@gmail.com>  
Bug reports: https://github.com/PredictiveEcology/SpaDES/issues

0 errors | 2 warnings | 0 notes

```
checking whether package ‘SpaDES’ can be installed ... WARNING
Found the following significant warnings:
  Warning: no DISPLAY variable so Tk is not available
See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES.Rcheck/00install.out’ for details.

checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
loading reproducible     0.2.4
loading quickPlot        0.1.5.9000
loading SpaDES.core      0.2.3
loading SpaDES.tools     0.3.0.9001
loading SpaDES.addins    0.1.1

Default paths for SpaDES directories set to:
... 8 lines ...
randomLandscapes: defineParameter: '.useCache' is not of specified type 'logical'.
randomLandscapes: inputObjects: stackName is used from sim inside doEvent.randomLandscapes, but is not declared in inputObjects
/tmp/RtmpyLgmCc/revdep247d33434290/SpaDES.core/sampleModules/fireSpread/fireSpread.R
fireSpread: module code: landscape, testStats are declared in inputObjects, but no default(s) are provided in .inputObjects
fireSpread: inputObjects: stackName, DEM, Fires are used from sim inside doEvent.fireSpread, but are not declared in inputObjects
###### Module Code Checking ########
Failed with error:  'there is no package called 'RandomFields''
Quitting from lines 66-68 (iii-cache.Rmd) 
Error: processing vignette 'iii-cache.Rmd' failed with diagnostics:
The 'RandomFields' package is required but not installed.
Execution halted
```

