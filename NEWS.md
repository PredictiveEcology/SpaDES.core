Known issues: https://github.com/PredictiveEcology/SpaDES.core/issues

version 0.1.0.9000
==================

* uses reproducible::Require instead of SpaDES.core::loadPackages to load required packages. Currently, does not use version control for packages, but does use installing (from CRAN or GitHub), and loading (via require)
* environments in modules are now as follows: 
    
    - Functions defined in a module are sourced into an environment located here: `sim@.envir$<moduleName>`, and it is a is a child of `sim@.envir`. Functions can be found in this environment, but prefixing functions is not necessary, because modules functions are within this environment already. 
    - `sim@.envir` is a child of `SpaDES.core`
    
* scoping from within a function that is defined in a module is thus: 

    - `sim@.envir$<moduleName>` --> `sim@.envir` --> `SpaDES.core` --> all imported packages including `base` --> `.GlobalEnv` --> `search()`

* speed improvements:

    - the core DES is now built around lists, rather than data.table. For small objects (e.g., the eventQueue) that have fewer than 200 objects, lists are faster. Accessors (e.g., `events(sim)`, `completed(sim)`) of the event queues still show `data.table` objects, but these are made on the fly.
    - `.parseModule` and `.parseModuePartial` now put their parsed content into a temporary environment (`sim@.envir$.parsedFiles$<Full Filename>)` during the `simInit`, which gets re-used. Previously, files were parsed multiple times in a given `simInit` call. Several functions now have `envir` argument to pass this through (including `moduleVersion`, `packages`, `checkParams`)
    
* remove `grDevices` from Imports as it was not used (#1)
* remove `chron` and `CircStats` dependencies
* remove functions `dwrpnorm2` and move to package `SpaDES.tools`
* remove unused function `F()` due to conflicts with `F`/`FALSE`.
* improved download of module data: added new `quickCheck` argument
* improved download of modules: use fuzzy matching
* new option: `spades.switchPkgNamespaces` which allows the user to turn off the `SpaDES` feature that loads and unloads libraries specific to each module. While useful, it slows down computations, in some cases, by a lot.
* bug fixes:

    - in `zipModule` that omitted the checksum file from being included when `data = FALSE` (#3)
    - caching of `.inputObjects` functions was evaluating outputObjects instead of inputObjects. Now corrected.

* default `debug` option in `spades()` now uses the package option `spades.debug` and default is set to `FALSE` (#5)
* various other speed improvements and bug fixes
* convert `P` to a function, rather than S4 generic and method, for speed.
* importFrom only used functions from `utils` due to name conflicts with `raster::stack` and `utils::stack`
* new function `remoteFileSize` to check the size of remote files
* add crayon to imports -- now messages are more colour coded
* bug fix in 'inputs' for the case of loading objects from the global environment, either from the same object to the same object, or from different global objects overwriting on the same simList object


version 0.1.0
=============

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
