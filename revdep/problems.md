# CBMutils

<details>

* Version: 2.0.3.0010
* GitHub: https://github.com/PredictiveEcology/CBMutils
* Source code: https://github.com/cran/CBMutils
* Number of recursive dependencies: 205

Run `revdepcheck::revdep_details(, "CBMutils")` for more info

</details>

## In both

*   checking R code for possible problems ... NOTE
    ```
    NPPplot: no visible binding for global variable ‘totalNPP’
    calcRootC: no visible binding for global variable
      ‘..aboveGroundColumns’
    Undefined global functions or variables:
      ..aboveGroundColumns totalNPP
    ```

# fireSenseUtils

<details>

* Version: 0.0.5.9093
* GitHub: https://github.com/PredictiveEcology/fireSenseUtils
* Source code: https://github.com/cran/fireSenseUtils
* Number of recursive dependencies: 200

Run `revdepcheck::revdep_details(, "fireSenseUtils")` for more info

</details>

## In both

*   checking examples ... ERROR
    ```
    Running examples in ‘fireSenseUtils-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: readLightningData
    > ### Title: Read lightning data
    > ### Aliases: readLightningData
    > 
    > ### ** Examples
    > 
    > library(reproducible)
    > crsToUse <- "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
    > ras <- rast(ext(-1115000, -376750, 7267000, 7874000), res = 250, vals = 1,
    +             crs = crsToUse)
    Error in rast(ext(-1115000, -376750, 7267000, 7874000), res = 250, vals = 1,  : 
      could not find function "rast"
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      ‘Firesense_LCC_flammability.Rmd’ using ‘UTF-8’... failed
      ‘fireSense-tutorial.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Firesense_LCC_flammability.Rmd’
      ...
    ...downloading...
      Downloading
    
    Download of ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip failed. This may be a permissions issue. Please check the url and permissions are correct.
    ...
    +     ncol = raster_width))
    
    > set.seed(123)
    
    > landTypeOne <- gaussMap(r_template, scale = 300, var = 300)
    
      When sourcing ‘fireSense-tutorial.R’:
    Error: Random landscape generation functionality has been removed because the RandomFields packages is no longer maintained.
     See neutralLandscapeMap() or use the NLMR package for tools to generate various random/neutral landscapes.
    Execution halted
    ```

*   checking dependencies in R code ... WARNING
    ```
    '::' or ':::' import not declared from: ‘clusters’
    Unavailable namespace imported from by a ':::' call: ‘clusters’
      See the note in ?`:::` about the use of this operator.
    ```

*   checking Rd \usage sections ... WARNING
    ```
    Undocumented arguments in Rd file 'abbreviateSpNames.Rd'
      ‘df’
    
    Undocumented arguments in Rd file 'runDEoptim.Rd'
      ‘cachePath’
    
    Functions with \usage entries need to have the appropriate \alias
    entries, and all their arguments documented.
    The \usage entries must correspond to syntactically valid R code.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

*   checking R code for possible problems ... NOTE
    ```
    readLightningData: no visible binding for global variable ‘plotID’
    runDEoptim: no visible binding for '<<-' assignment to ‘cl’
    Undefined global functions or variables:
      plotID
    ```

# SpaDES

<details>

* Version: 2.0.11.9000
* GitHub: https://github.com/PredictiveEcology/SpaDES
* Source code: https://github.com/cran/SpaDES
* Number of recursive dependencies: 75

Run `revdepcheck::revdep_details(, "SpaDES")` for more info

</details>

## Newly broken

*   checking whether package ‘SpaDES’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘SpaDES.core::.parseElems’ by ‘quickPlot::.parseElems’ when loading ‘SpaDES’
    See ‘/home/achubaty/Documents/GitHub/PredictiveEcology/SpaDES.core/revdep/checks/SpaDES/new/SpaDES.Rcheck/00install.out’ for details.
    ```

# SpaDES.experiment

<details>

* Version: 0.0.2.9005
* GitHub: https://github.com/PredictiveEcology/SpaDES.experiment
* Source code: https://github.com/cran/SpaDES.experiment
* Number of recursive dependencies: 126

Run `revdepcheck::revdep_details(, "SpaDES.experiment")` for more info

</details>

## In both

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        8.     └─future.apply::future_mapply(...)
        9.       └─future.apply:::future_xapply(...)
       10.         └─base::tryCatch(...)
       11.           └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       12.             └─base (local) tryCatchOne(...)
       13.               └─value[[3L]](cond)
       14.                 └─future.apply:::onError(e, futures = fs, debug = debug)
      
      [ FAIL 10 | WARN 1 | SKIP 2 | PASS 53 ]
      Error: Test failures
      Execution halted
      Error in unserialize(node$con) : error reading from connection
      Calls: workRSOCK ... doTryCatch -> recvData -> recvData.SOCK0node -> unserialize
      Execution halted
      sh: 0: getcwd() failed: No such file or directory
    ```

*   checking S3 generic/method consistency ... WARNING
    ```
    as.data.table:
      function(x, keep.rownames, ...)
    as.data.table.simLists:
      function(x, vals, objectsFromSim, objectsFromOutputs, ...)
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    ```

