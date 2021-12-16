Known issues: <https://github.com/PredictiveEcology/SpaDES.core/issues>

version 1.0.10
=============

## new features
* `newModule` now correctly places the `SpaDES.core` package dependency in the `reqdPkgs` element of the metadata, instead of `version`. It will put the full Github reference if SpaDES.core was installed directly from GitHub.
* There is a bug in `qs` package: either `qsave` or `qread` converts `data.table` objects to `list` objects. `loadSimList` has a work around internally to convert these objects back to `data.table`, if the metadata indicate that the objects should be `data.table` objects.
* new function: `paramCheckOtherMods`. Can be used within a module to assert that a parameter has the same value as the same parameter in other modules. This is therefore a check of a parameter that might be considered a `.global` and passed within `simInit(..., params = list(.globals = list(someParam = "someValue")))`, but the user did not do that.
* now `spades` messaging when e.g., `debug = 1` can correctly accommodate nested `spades` calls, i.e., a SpaDES module calling `spades` internally.
* `newModule` now puts `SpaDES.core` dependency in the correct `reqdPkgs` instead of `version` metadata element

## dependency changes
* drop support for R 3.6 (#178)

## Changes
* the default behaviour was to keep a live pointer of the active `simList` in the hidden package environment (`asNamespace("SpaDES.core"):::.pkgEnv$.sim`). This may cause a memory leak. That pointer is now being put into the `.GlobalEnv`.

## bug fixes
* minor bugfix when `debug` arg of `spades` is set to an event type that is also in the core modules (e.g., save, load), such as "init"
* `Cache`-ing of a `simList`, when `quick` is a character vector, errored. Now fixed.
* `moduleMetadata` incorrectly dropped `defineModuleListItems` under certain signatures.

version 1.0.9
=============

## new features
* `moduleCoverage` has been rewritten to estimate code coverage of a module using `covr` package.
* `P` now has a replacement method. So, to update a parameter within a module (where it is namespaced, i.e., don't have to specify the module name): `P(sim, "paramName") <- 1`. If using this outside a module, then `module` (3rd argument) will have to be specified.
* `P` argument order changed to accommodate the fact that namespacing is used to detect module name: the user does not need to supply `module`, so it should not be second. This is for the normal `P` method and the new replace method above: it is now `P(sim, param, module)`; there are attempts to capture errors (i.e., parameter supplied that matches a module, but not a parameter; vice versa) and give a warning for user to change code. This may have little downstream effect as all known cases use the `P(sim)$paramName`, which will still work fine, instead of `P(sim, "paramName")`.
* `Plots` does a better job with `RasterStack` objects plotted to screen without `ggplot2`
* removed `.isFALSE`: use `base::isFALSE` now
* `Plots` can now omit the `data` argument; just use the named arguments in ...
* `defineParameter` now allows multi-line `desc` or multiple strings; `paste` is no longer needed for long `desc`
* `moduleCodeFiles` a new function that identifies all the code files in a collection of modules
* `.globals` functionality is modified. If a user specifies a `.globals` in the parameters object (passed into `simInit`), then all identical parameters in all modules will be overridden with these `.global` values
* `defineParameter`, `expectsInput` and `createsOuptut` can all now have multi-line `desc`, without needing to use `paste` or `paste0`. Extraneous spaces and carriage returns will all be stripped. This can either be using a single multi-line quote or via multiple lines, each with its own `""`. 
* the module environment in the `simList` is no longer "locked" with `lockBinding`. It is already hidden in `sim$.mods`, and since `sim$.mods` can be modified, this was a weak caution against user modification. Further, for `moduleCoverage`, the module environment needed to be unlocked, which is not allowed by CRAN.

## dependencies
* no changes

## bug fixes
* When `.inputObjects` was cached (via setting `useCache = '.inputObjects'` parameter), it was "too sensitive". Changes to any module's parameters, not just the current module, would cause rerun of `.inputObjects`. Now it correctly identifies parameter changes only in the current module. THIS WILL CAUSE some existing caches to trigger a rerun once; after this, it will be less sensitive
* `restartSpades` did not correctly deal with objects that did not yet exist prior to the event. Fixed with: 24b9cd12973aa81a9a4923a02225e095fa28f77a.
* `restartSpades` was losing the previous completed events list. This has been fixed; it is now kept after `restartSpades`
* Plots - fixed issues with plot layer names and plot overlaying when passing `data` only (e.g. `quickPlot::Plot`-like behaviour)
* `simInitAndSpades` now has `.plots` arg to match `spades`
* fix raster file name query for GDAL 3.3.2 (#174; @rsbivand)

version 1.0.8
=============

## new features
* `Plots` function can be used like `Plot`, but with `types` specified.
  The devices to save on disk will have some different behaviours to the screen representation, since "wiping" an individual plot on a device doesn't exist for a file device.

version 1.0.7
=============

## new features
* `Plots` function that will produce zero to 4 types of items that are relevant for plotting: 1) Visual on screen, 2) The plot object saved to disk, 3) The raw data that went into the plot and 4) The plot as one or more image files, e.g., `.png` or `.pdf` via `ggsave`
* `spades` now accepts an `events` argument, which will limit the events that are run to those specified in the argument. This seems to be most useful for the `init` case, e.g., `spades(sim, events = "init")`. See `?spades`.
* messaging during `simInit` now is prefixed with `Sys.time()` and `"simInit`"
* messaging during `spades` is simplified to take up fewer characters: `INFO::` has been removed
* `simInit` now checks for minimum version of `SpaDES.core` needed in a module and stops if it fails, giving instructions how to upgrade.
* several human-readable only elements of a module metadata are no longer enforced, including `spatialExtent`, as they are not used by the spades algorithms
* new function: `anyPlotting` to test whether plotting of one form or another should occur
* line-by-line messaging during `spades` call is now more informative, including module name (by default shortened -- can be changed with `options("spades.messagingNumCharsModule"))`) 
* `defineParameter` can now accept a vector of "class", so a parameter can be more than one class.
  Presumably this should generally not be used, but a good reason could be, say, `c("numeric", "function")`, where the use can pass either a numeric or a function that would calculate that numeric.
* new helper function `simFile` to generate file names for use with e.g., `saveSimList`
* `zipSimList` is now exported
* `spades` will now attempt to load `reqdPkgs`, which is already done in `simInit`.
  In cases where `simInit` was not run, e.g., `Cache(simInitAndSpades, ..., events = "init")`, then modules will not have access to packages.
  For cases where `simInit` was called, then this should add very little overhead.
* `saveSimList` will now convert file-backed `Raster*` class objects to memory if `fileBackend = 0`.
  Previously, it left them as is (on disk if on disk, in memory if in memory).
* For code/documentation contributors, markdown syntax is now turned on and can be used when writing function documentation  
* The first event will now run if it is scheduled to be prior to `time(sim)` in the case where it is equal to or after `end(sim)`. Previously, this would not run any events if `time(sim)` >= `end(sim)` && `events(sim)[[1]] < time(sim)`.
* minor documentation modifications
* Add optional `.seed` parameter for modules (#163)

## Bugfixes
* `defineParameter` was throwing `is.na(default)` warning when a parameter was not an atomic.
* recovery mode did not work correctly if the file-backed rasters were in the temporary directory, as it would collide with the temporary directory of the recovery mode mechanism. Now recovery mode uses a dedicated temporary directory
* other minor bugfixes, 

version 1.0.6
=============

## new features
* more informative message re: module package versions when `spades.useRequire = FALSE` (#141)
* now detects user-created memory leaks when a user adds a closure or formula to the `sim`; user informed with a `warning`

## dependencies
* no changes

## bug fixes
* use `try()` with `communities()` to skip tests on systems without `igraph` GLPK support.
* prevent package (re)installation during examples, tests, vignettes.
* fix failures on R-devel caused by `RandomFields` being unavailable.
* minor bug fixes

version 1.0.5
=============

## new features
* New experimental `spades.futureEvents` option. If set to `TRUE`, spades will run module events in a "future" (see `future` package), if they do not produce outputs for other modules.
* enable automated module code checking with GitHub Actions (`use_gha()` and corresponding vignette; #74)
* `newProject` creates Rstudio `.Rproj` file if invoked in Rstudio
* moved `paddedFloatToChar` to reproducible; but re-exported here, so still usable.
* modules can now use a parameter called `.seed` which is a named list where names are the events and the elements are the seed with which to run the event. During `doEvent`, `SpaDES.core` will now `set.seed(P(sim)$.seed[[currentEvent]])` and reset to random number stream afterwards.

## dependencies
* completely removed `dplyr`, `lubridate`, `R.utils`, `tools`, `backports` and `rlang` from dependencies
* move `tcltk` to `Suggests`
* remove `devtools`, `microbenchmark` from `Suggests`

## bug fixes
* minor bug fixes
* use new `all.equal(..., check.environment = FALSE)` for internal testing 

version 1.0.3
=============

## new features
* none

## dependencies
* completely removed `RCurl` dependency (#120)
* Suggests `sp` because it's linked in documentation (#120)

## bug fixes
* fix `pkgDeps` example for new version of `Require`
* minor bug fixes

version 1.0.2
=============

## new features
* `desc` argument in `defineParameter`, `expectsInput`, and `createsOutput` can now have extraneous spaces and End-of-Line characters.
  This means that they can now be written more easily with a single set of quotes, without needing `paste`.
  The accessor functions, `moduleParams`, `moduleInputs`, and `moduleOutputs` all will strip extraneous spaces and End-of-Line characters.
* new helper functions for debugging: `writeEventInfo()` and `writeRNGInfo()` to write info to file.

## dependencies
* drop support for R < 3.6
* removed imports from `stringi`

## bug fixes

* minor bug fixes in sample modules
* module template has full path instead of `..` for `moduleParams` etc. This is more accurate. 
* address changes to active bindings in R-devel
* fix CRAN check errors
* reduced the number of tests run on CRAN (extended tests still run on GitHub Actions)

version 1.0.1
=============

## new features

* `Par` is now an `activeBinding` (similar to `mod`) pointing to `P(sim)`; this allows for tab autocomplete to function correctly.
* new helper functions to extract parameters, inputs, and outputs tables from module metadata:
  `moduleParams()`, `moduleInputs()`, `moduleOutputs()`. These are now used in default `.Rmd` template.
* better testing of `memoryUse` functionality
* A pointer to `sim` is now created at `.pkgEnv$.sim` at the start of `spades` call, rather than `on.exit`; failures due to "out of memory" were not completing the `on.exit`
* improved templating of new modules, including support for automated module code checking using GitHub Actions (`newModule()` sets `useGitHub = TRUE` by default).

## dependencies
* add `usethis` to Suggests for use with GitHub Actions

## deprecated
* none

## bug fixes
* tests for `Filenames` function coming from `reproducible` package
* `options('spades.recoverMode')` was creating temp folders every event and not removing them; now it does.

version 1.0.0
=============

## new features

* several efforts made to reduce memory leaks over long simulations; if memory leaks are a problem, setting `options('spades.recoveryMode' = 0)` may further help
* Updates to deal with new backend with `reproducible`
* better assertions inside list elements of `simInit`, e.g., `simInit(times = list(start = "test"))` now fails because times must be a list of 2 `numeric` objects
* messaging is now all with `message` instead of a mixture of `message`, `cat` and `print`.
  This allows for easier suppressing of messaging, e.g., via `suppressMessages`.
  This was requested in a downstream package, `SpaDES.experiment` that was submitted to CRAN but rejected due to the now former inability to suppress messages.
* `restartR` saves simulation objects using `qs::qsave()` which is faster and creates smaller file sizes.

## dependencies

* moved packages from Imports to Suggests: `codetools`, `future`, `httr`, `logging`, and `tcltk`
* removed `archivist`
* `qs` now used for improved object serialization to disk

## deprecated

`.objSizeInclEnviros` and removed

## bug fixes

* removed mention of 'demo' from intro vignette (#110)
* `objectSynonyms` caused a breakage under some conditions related to recovering a module from `Cache`.

version 0.2.8
=============

## new features

* Changed all internal `print` and `cat` statements to message to allow use of `suppressMessages`, as recommended by CRAN
* added file based logging via `logging` package, invoked by setting `debug` argument in `spades` function call to a `list(...)`. `?spades` describes details

## bug fixes

* `restartR` minor bug fixes

version 0.2.7
=============

## dependencies

* Removed dependency packages `DEoptim`, `future.apply`, `Matrix`, `parallel`, `pryr`, `purrr`, and `rgenoud`, which are no longer required.
  See "deprecated" info below.
* added `whisker` to Imports to facilitate module file templating (#100)

## new features

* memory and peak memory estimation is now available for *nix-type systems, when `future` is installed.
  See new vignette `iv-advanced` and `?memoryUse`.
* new function and capacity: `restartR`.
  Restarts R mid-stream to deal with apparent memory leaks in R.
  In our experience with large projects that have long time horizons, there appears to be a memory leak at a low level in R (identified here: <https://github.com/r-lib/fastmap>).
  This has prevented projects from running to completion. Without diagnosing the root cause of the memory inflation, we have noticed that interrupting a simulation, saving the simList, restarting R, resets the memory consumption back to levels near the start of a simulation.
  The new functionality allows a user who is hitting this memory leak issue to restart R as a work around.
  See `?restartR` for instructions.
* new function `newProject` to initialize a SpaDES project with subdirectories `cache/`, `inputs/`, `modules/`, and `outputs/`, and `setPaths()` accordingly.

## bug fixes

* `newModule()` now uses `open = interactive()` as default to prevent files being left open during tests.
* various bug fixes and improvements.

## deprecated

* `experiment()`, `experiment2()`, and `POM()` have been moved to the `SpaDES.experiment` package

version 0.2.6
=============

## dependencies

* R 3.5.0 is the minimum version required for `SpaDES.core`.
  Too many dependency packages are not maintaining their backwards compatibility.
* added `backports` to Imports for R-oldrel support
* removed `googledrive` dependency (this functionality moved to `reproducible`)

## documentation

* improved documentation for `P`, `params`, and `parameters`, thanks to Louis-Etienne Robert.

## new features

* update `objSize.simList` method with 2 new arguments from `reproducible` package
* `.robustDigest` method for `simList` class objects now does only includes parameters that are listed within the module metadata, if `Cache` or `.robustDigest` is called within a module. This means that changes to parameter values in "other" modules will not affect the Caching of "the current" module.
* New function `outputObjectNames` will extract just the object names of all `outputObjects` across modules
* New function `restartSpades` and its associated `options(spades.recoveryMode = 1)`, the new default, which is still experimental. Its purpose is to be able to restart a simulation in the case of an error or interruption. 
* Now gives better errors if modules are missing main .R file or if they are missing entirely 
* More silent tests
* `mod` is now an active binding to `sim[[currentModule(sim)]]$.objects` (move from `sim[[currentModule(sim)]]`) and its parent environment is `emptyenv()`. This should cause no changes to users who use `mod$...`, but it will cause a change if user was calling objects directly via `sim[[currentModule(sim)]]$...`. This change is to separate the function enclosing environments and object enclosing environments, which should be different.
* `sim@completed` is now an environment instead of a list. Of the three event queues, this one can become the largest. The `list` would get increasingly slow as the number of completed events increased. There should be no user visible changes when using `completed(sim)`

## User visible changes to default options

`spades.debug` is now set to 1
`spades.recoveryMode` is new and set to 1 (i.e., the current event will be kept at its initial state)

## bug fixes

* Internal bugs during `simInit` especially in some weird cases of `childModules`.
* packages listed in `reqdPkgs` not being loaded when only listed in child modules. Fixed in `5cd79ac95bc8d190e954313f125928458b0108d2`.
* fixed issue with saving simulation outputs at simulation end time.

version 0.2.5
=============

* improved messaging and fixed test failures when GLPK installed but not used by `igraph`
* compatibility with `RandomFields` >= 3.3.4

version 0.2.4
=============

## package dependencies

* `archivist` and `devtools` added to Suggests because they are used in vignettes
* minimium `reproducible` version 0.2.6

## new features

* new vignette on caching `SpaDES` simulations moved from `SpaDES` package.
* `simList` environment now has `emptyenv()` as its `parent.env`. The biggest user-facing changes are:
  
    - functions placed in the `envir(sim)` (unusual, but may occur) won't find objects in the `.GlobalEnv`;
    - lighter memory footprint, as functions take RAM due to the objects in the `parent.env` in which they are defined (little know fact identified here: http://adv-r.had.co.nz/memory.html#gc identified as a possible source of memory leaks).

* module's function environment in the simList now has its parent `asNamespace("SpaDES.core")` instead of the `envir(sim)` (as mentioned above), i.e,. `parent.env(sim[[currentModule(sim)]])` is `asNamespace("SpaDES.core")`. The main user-noticeable changes of this are that module functions will not accidentally find objects in the `simList` unless they are actually passed in explicitly as arguments.
* New active binding, `mod` that works as a module-specific variable, similar to a private object, *i.e.*, `mod$a` is a local object inside the module that persists across events. It is a pointer to `sim[[currentModule(sim)]]$a`
* New function `scheduleConditionalEvent`, which allows an event to be scheduled based on a condition. Still experimental.
* An experimental new function and feature, `objectSynonyms`, which will create active bindings of two names to a single object
* User can now specify `modulePath` as a character vector, e.g., `simInit(..., paths = list(modulePath = c(".", "test")))`. This means that a user can organize the modules in different locations.
* `modulePath` now has a new argument, `module`, where user can specify (a) specific module(s)'s path. Modifications were implemented to `dataPath` to utilize this new feature
* `simInit` and `spades` now call `setPaths(paths)` or `setPaths(sim$paths)`, unsetting them `on.exit` internally to make the paths used for functions e.g., `reproducible::Cache` to use the correct path
* under-the-hood speed improvements for the DES (about 20% faster) -- 38 microseconds per event under ideal conditions
* improved default path settings in `.inputObjects` (#83)
* following `reproducible` package updates, now uses `data.table::setattr` internally to avoid copying of
objects (this may have very little/no effect on simList objects)
* `suppliedElsewhere` has a new argument, `returnWhere`, a logical which will cause a logical of length 3 to be returned, indicating in which of the 3 other places the object may have been supplied, instead of length 1, still the default.

## bug fixes

* fix to work with latest `data.table` v1.12.0 (#85, @mattdowle)
* several minor, including to `Copy` (error existed because function inheritance persisted even though the location of the function was moved)

version 0.2.3
=============

## package dependencies

* add `RandomFields` to Suggests, as it is in the Suggests of `SpaDES.tools` and used in examples/tests.

## new features

* new option and default setting: `options("spades.saveSimOnExit" = TRUE)`. This will save the state of the `simList` to an object as `SpaDES.core:::.pkgEnv$.sim`, with a message, if there is a hard exist. There is virtually no computational cost to this, as the object is already in RAM.
* `simList` internals changed. It now inherits from `environment`. Amongst other things, this means that tab autocomplete in RStudio now works for objects in the `simList`. Also, we removed several associated methods, `$`, `[[`, `ls`, `ls.str`, `objects`, as the defaults for environments work correctly with the `simList` now
* `debug` arg in `spades` call can now take numeric, currently 1 or 2, giving a few pre-packaged informative messaging each event
* new function `elapsedTime` which gives a summary of the clock time used by each module or event
* most metadata entries now have accessor of same name, e.g., inputObjects(sim) returns the inputObjects data.frame.
* new function `citation` replaces `utils::citation` with an S4 generic. If `package` arg is a `character`, it dispatches `utils::citation`; if a `simList`, it gives the citation for the module(s)
* improved messaging when GLPK not installed (*e.g.*, on macOS)
* `downloadModule()` now prints the module version downloaded (#77)
* 

## bug fixes

* resolved `.inputObjects()` name conflict (internal `.inputObjects` renamed to `._inputObjectsDF`; `.outputObjects` renamed to `._outputObjectsDF`)
* module `.inputObjects` evaluated based on module load order (#72)
* `.robustDigest` fix for simLists -- needed to omit `._startClockTime` and `.timestamp`

version 0.2.2
=============

## package dependencies

* remove `sp` from imports

## new features

* none

## bug fixes

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

## bug fixes

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
