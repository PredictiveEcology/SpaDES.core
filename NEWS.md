Known issues: https://github.com/PredictiveEcology/SpaDES.core/issues

version 0.1.0.9000
==================

* environments in modules are now as follows: 
    
    - Functions defined in a module are sourced into an environment located here: `sim@.envir$._<moduleName>`, and it is a is a child of `sim@.envir`. This environment is hidden (with ls(sim)). To see it, `ls(envir(sim), all.names = TRUE)`
    - `sim@.envir` is a child of `SpaDES.core`
    
* scoping from within a function that is defined in a module is thus: 

    - `sim@.envir$._<moduleName>` --> `sim@.envir` --> `SpaDES.core` --> `base` --> `.GlobalEnv` --> `search()`

* the core DES is now built around lists, rather than data.table. For small objects (e.g., the eventQueue) that have fewer than 200 objects, lists are faster. Accessors (e.g., `events(sim)`, `completed(sim)`) of the event queues still show `data.table` objects, but these are made on the fly.
* remove `grDevices` from Imports as it was not used (#1)
* remove `chron` and `CircStats` dependencies
* remove functions `dwrpnorm2` and move to package `SpaDES.tools`
* remove unused function `F()` due to conflicts with `F`/`FALSE`.
* improved download of module data: added new `quickCheck` argument
* improved download of modules: use fuzzy matching
* new option: `spades.switchPkgNamespaces` which allows the user to turn off the `SpaDES` feature that loads and unloads libraries specific to each module. While useful, it slows down computations, in some cases, by a lot.
* fix bug in `zipModule` that omitted the checksum file from being included when `data = FALSE` (#3)
* default `debug` option in `spades()` now uses the package option `spades.debug` and default is set to `FALSE` (#5)
* various other speed improvements and bug fixes
* convert `P` to a function, rather than S4 generic and method, for speed.
* importFrom only used functions from `utils` due to name conflicts with `raster::stack` and `utils::stack`

version 0.1.0
=============

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
