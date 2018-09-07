Known issues: https://github.com/PredictiveEcology/SpaDES.core/issues

version 0.2.2.9000
=============

## package dependencies
* no changes

## new features
* none

## bugfixes
* resolved `.inputObjects()` name conflict (internal `.inputObjects` renamed to `._inputObjectsDF`; `.outputObjects` renamed to `._outputObjectsDF`)

version 0.2.2
=============

## package dependencies
* remove `sp` from imports

## new features
* none

## bugfixes
* fix issues with failing tests on macOS

version 0.2.1
=============

## package dependencies

* requires new version of `reproducible` (>=0.2.2)

## new features

* new option `spades.useRequire`: a logical which causes `simInit` to load packages with `Require` or `require`. Lower case is generally faster, but will not handle the case of uninstalled packages, so should only be used once all packages are installed.
*    - * new option `spades.keepCompleted`: a logical which causes `spades()` to keep (`TRUE`) or not keep a record of completed events. Keeping track of completed events when they are many (>1e5) gets slow enough that it may be worth turning it off.

* more robust and faster tests are used now, care of helper functions in `tests`
* `all.equal.simList` now removes all time dependent attributes, *e.g.*, `._startClockTime` and `.timestamp`
* speed enhancements for Discrete Event Simulator; now overhead is 1.3 seconds for 5000 events or, per event, 260 microseconds (185 microseconds if `options("spades.keepCompleted" = FALSE)`

## bugfixes

* Improvements to caching of functions with `simList` objects:

    - Cached functions with a `simList` in the arguments would erroneously return cached copies of functions. These now are copied through from argument `simList`, rather than cached `simList`. This means that changes to the function definitions in a module will persist (e.g., debugging via `browser()` will work correctly)
    - functions with `simList` in arguments that return a `simList` will now do a post digest of the output. This will be compared with the predigest, and only those object which changed in the `simList` will be modified.
    - caching of `.inputObjects` function was incorrect. Fixed.

version 0.2.0
=============

## new features

* module metadata now in *named* lists inside `depends(sim)`
* new debugging -- if debug is not `FALSE`, then any error will trigger a `browser()` call inside the event function. User can continue (`c`) or quit (`Q`) as per normal. `c` will trigger a reparse and events will continue as scheduled.
* introduction of code checking for modules, currently turned on or off by an option `spades.moduleCodeChecks`, which is `TRUE` by default. Code checking includes various types:
    
    - use `codetools` to check for various code problems
    - detects conflicts with known common functions (`raster::level`, `raster::scale`, `quickPlot::Plot`)
    - use `checkCodeEnv` on every function inside a module
    - checking for `sim$xxx` occurrences in modules, comparing to `outputObjects` in metadata if used in assignment (i.e., left hand side of assign operator), or comparing to `inputObjects` if used on the right hand side
    - check that all objects declared in `inputObjects` have default values assigned in the `.inputObjects` function
    - messages colour coded, and separated by file with absolute path explicit
    
* option `spades.debug` set to `TRUE` by default, instead of `FALSE`. This is better for new users.
* `moduleMetadata` argument order changed, so `sim` is first, more consistent with all other `simList` accessors.
* `downloadData` has changed dramatically, now it is a wrapper around `reproducible::prepInputs` which does more checking.
* `extractURL` will extract the sourceURL from metadata, given an object name.
* `makeMemoiseable` and `unmakeMemoisable`, new methods, each the inverse of the other, to deal with imperfect memoised returns under some cases of `simList`.
* new option, `spades.keepCompleted`, `TRUE` by default, which can be useful for dramatically speeding up the DES when there are many (>10,000) events.

## deprecated, defunct, and removed features

* remove `fileExt` -- use `tools::file_ext` instead

## bug fixes

* fix tests based on `data.table` changes (@mattdowle, #64).
* re-export `start` and `end`.
* `newModule` template modified slightly based on workshop feedback.
* `setPaths` now only sets the directories that are passed into it.
* `all.equal.simList` method strips a small number of attributes that are used internally that create false failures.
* speed enhancements.
* other minor bug fixes.

## package dependency changes

* add package imports `tools`, `pryr`.
* removed package imports `rgeos`, `RCurl` and `googledrive`.

version 0.1.1
=============

* uses `reproducible::Require` instead of `SpaDES.core::loadPackages` to load required packages. Currently, does not use version control for packages, but does use installing (from CRAN or GitHub), and loading (via require). This means a module can indicate a github package, e.g,. `achubaty/amc@development`
* environments in modules are now as follows: 
    
    - Functions defined in a module are sourced into an environment located here: `sim@.envir$<moduleName>`, and it is a is a child of `sim@.envir`. Functions can be found in this environment, but prefixing functions is not necessary, because modules functions are within this environment already. 
    - `sim@.envir` is a child of `SpaDES.core`
    
* scoping from within a function that is defined in a module is thus: 

    - `sim@.envir$<moduleName>` --> `sim@.envir` --> `SpaDES.core` --> all imported packages including `base` --> `.GlobalEnv` --> `search()`

* speed improvements:

    - the core DES is now built around lists, rather than `data.table` objects. For small objects (e.g., the eventQueue) that have fewer than 200 objects, lists are faster. Accessors (e.g., `events(sim)`, `completed(sim)`) of the event queues still show `data.table` objects, but these are made on the fly.
    - `.parseModule` and `.parseModuePartial` now put their parsed content into a temporary environment (`sim@.envir$.parsedFiles$<Full Filename>)` during the `simInit`, which gets re-used. Previously, files were parsed multiple times in a given `simInit` call. Several functions now have `envir` argument to pass this through (including `moduleVersion`, `packages`, `checkParams`)

* parsing of modules is now more intelligent, allowing for modules to contain functions (the current norm) and but they can also create objects at the module level. These can use the sim object in their definition. These objects can, for example, be used to help define parameters, for example, e.g., `startSimPlus1 <- start(sim) + 1` can be defined in the module and used in `defineModule`
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

* If `.inputObjects` contains arguments other than just sim, these will be evaluated as function inputs by the Cache mechanism (via .useCache), therefore correctly assessing when those inputs changed, e.g., if they are files and the arg is wrapped in `asPath`, then any change to the underlying file will cause a re-cache.  e.g., `.inputObjects <- function(sim, importantFile = asPath(file.path(inputPath(sim), "theFile.rdata"))) { ... }`
* default `debug` option in `spades()` now uses the package option `spades.debug` and default is set to `FALSE` (#5)
* various other speed improvements and bug fixes
* convert `P` to a function, rather than S4 generic and method, for speed.
* importFrom only used functions from `utils` due to name conflicts with `raster::stack` and `utils::stack`
* new function `remoteFileSize` to check the size of remote files
* new namespaced function `dataPath` will return `file.path(modulePath(sim), currentModule(sim), "data")`, which will return a different path, depending on which module it is placed inside.
* add crayon to imports -- now messages are more colour coded
* bug fix in 'inputs' for the case of loading objects from the global environment, either from the same object to the same object, or from different global objects overwriting on the same simList object


version 0.1.0
=============

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
