# SpaDES.core (development version)

* use `qs2` package instead of `qs` (deprecated) for improved object serialization (#291; #316);
* documentation improvements;

# SpaDES.core 2.1.8

* changes to a sporadically failing test;

# SpaDES.core 2.1.6

* drop support for R 4.2;
* fixed issue with `Plots()` where plots were discarded if no filename was specified;
* fixed timeunit test failures (#297);
* add package anchors to Rd links (#300);
* minor documentation improvements;

# SpaDES.core 2.1.5

* fix issue with Windows short paths in tests;
* improved documentation;
* removal of `globals(sim)` in Sample modules

# SpaDES.core 2.1.2

* fixed bug in `restartSpades()`;

# SpaDES.core 2.1.1

## Bugfixes
* `newModule`, new `events` argument bugs that caused unwanted objects to be put in the module. Fixed.
* `.wrap.simList` did not anticipate objects with pointers in the metadata, so recovery from Cache failed if there was, e.g., a `SpatExtent` object in the metadata. Fixed.
* improved loading of `simList` objects, when loading e.g., lists of lists;

# SpaDES.core 2.1.0

* "sequential caching" can now be turned on with `options("spades.allowSequentialCaching" = TRUE)`. When a series of events of events in sequence are cached, setting this option will treat them as a single Cache, so it may be much faster. Experimental and should be used with caution.
* fix issue with event queue `colnames` in `completed<-` and `all.equal.simList` (#272);
* fixed issue saving `simList` objects when multiple paths were used (e.g., `length(modulePath) > 1`);
* events can be defined directly in calls to `newModule()`;
* checkpoints are assumed to be used locally, and no longer invoke simulation file archiving and re-extraction of files (i.e., uses `saveSimList(..., inputs = FALSE, outputs = FALSE, cache = FALSE, files = FALSE)`);
* improved recovery of interrupted simulations via `savedSimEnv()$.sim` -- `savedSimEnv()` is now exported for easier discovery -- an internal package environment is used, unless the user specifies `options(reproducible.memoisePersist = TRUE)`, which will use the global environment to store the `.sim` object;
* switch from `crayon` (superseded) to `cli` for message colours;

## Dependency changes

* require `reproducible` v2.1.0 or higher;

## Breaking changes

* due to upstream changes in `reproducible`, `loadSimList()` is incompatible with `simList` objects saved with earlier versions of `SpaDES.core`.

# SpaDES.core 2.0.5

* fix a failing test on R-devel and latest R-release (#275)

# SpaDES.core 2.0.4

* drop support for R 4.1 due to changes in dependency packages;
* improve `saveSimList` documentation (#260);
* `newModule` now correctly defaults to current working directory (#273);

# SpaDES.core 2.0.3

## Enhancements
* new accessor function `figurePath()` to get the directory of a module's output figures, which is now uses a separate subdirectory per module (i.e., `file.path(outputPath(sim), "figures", <moduleName>)`); `Plots()` defaults to using this path, and module developers are encouraged to update their module code to use `figurePath(sim)` where `Plots()` is not being used.
* re-Caching of a `simList` no longer triggers on changes to `.useCache` parameter, when it doesn't pertain to the event or module in question.
* many historical modules used `bind_rows` from `dplyr` within `expectsInput` or `createsOutput`. Now, if a module uses `bind_rows` and doesn't have `dplyr` installed, `SpaDES.core` will intercept and use `SpaDES.core::bindrows`.
* `saveSimList()` better handles relative paths and symbolic links (#263)
* `saveSimList()` does an improved job at handling file-backed objects (previously, tests were passing because object was still there; now object is deleted, `simList` reloaded, and file-backed objects put back into place)
* deal with upstream `reproducible` changes to `.wrap` 
* deal with upstream `reproducible` changes to `Cache` messaging, specifically, remove cases where `function` was a `userTag` for an outer function call. Now these will display as `otherFunction`, so that individual functions can be more easily isolated.
* overhaul of messaging during `simInit` and `spades` that allows for nested calls to `simInit` and/or `spades`
* elapsed time during `simInit` is now reported
* `elapsedTime` now displays the largest time unit, which may not be `secs`
* some parts of any warning messages are now muffled for clarity: e.g., `In modCall...` is removed.
* `options` that are either `RequireOptions()`, `spadesOptions()` or `reproducibleOptions()` can now be set during the `simInit` by passing them as arguments, e.g., `simInit(useMemoise = FALSE)`. See `?simInit`, specifically the `...` parameter description. This is not passed as an argument named `options`: these are just options. For convenience, user can omit the package prefix, e.g., `useMemoise` for `reproducible.useMemoise`
* all necessary updates to deal with `reproducible` updates to exported methods
* new function: `dmin` to go with the other `d*` SpaDES times.
* updates to unit tests to accommodate all these changes.

## Dependency Changes
* `dplyr` is removed (again)

## Bug fixes
* `coltab<-` from `terra` changed how it deals with multi-layer `SpatRasters`. Two sample modules have been modified to set colours on these multi-layer `SpatRasters`
* loading of `asc` raster-type files using `inputs` did not work; fixed.
* fixed bug in `simInit` parameter checking
* A number of new edge cases dealt with for reducing false positive and false negative `Cache`ing of events and modules. Now, for example, a change to the parameter `.useCache = c("init")` to `.useCache = c("init", ".inputObjects")` will not trigger a rerun of the `init` event. Also, `.inputObjects` is no longer evaluated for `Cache`ing of `doEvent`
* `Error in if (is.na(sim@params[[m]][[x]]))` fixed.
* `asc` spatial map files were incorrectly loaded by `terra` package: fixed.

# SpaDES.core 2.0.2

## Enhancements
* none

## Dependency Changes
* none

## Bug fixes
* new helpers for getting copies of sample files (`getSampleModules()` and `getMapPath()` ) now used throughout examples, vignettes, and tests to avoid writing files in package installation directories.

# SpaDES.core 2.0.0

## Enhancements
* `options(spades.futureEvents = TRUE)` has been reworked and now works under a wider array of conditions. 
* `Copy`, `.wrap`, `wrap` and `unwrap` all have fairly robust methods for `simList` class. The generics are in `reproducible` or `terra`
* updated `sampleModules` to use `Plots` and `.plot` parameter.
* new function: `registerOutputs` can be used by a developer to add saved files to the `outputs(sim)` data.frame.
* messaging during nested `simInit` or `spades` calls will now not duplicate time prefix
* `params` and `.globals` were previously not expected to change during `Cache`d events. Thus returned cached values were always the same as input as `params` and `.globals`. They are now assessed and returned as part of the `Cache`, as expected.
* updates to handle upstream changes in `Require` and `reproducible`, including renaming `cacheRepo` to `cachePath` in some inherited functions.
* updates to sample modules to use `SpaDES.tools::neutralLandscapeMap` instead of `NLMR` package directly
* migration complete to use `terra` and `sf` instead of `raster`, `sp`, `rgeos`, and `rgdal` as defaults. Attempts have been made to maintain backwards compatibility in all cases.
* `moduleMetadata` now handles multiple module paths
* updates to `memoryUse`
* new option: setting `options("spades.allowInitDuringSimInit" = TRUE)`, a user will have `init` events of *one ore more* modules run during the `simInit` call, but only if they have no upstream dependencies, i.e., their `expectsInputs` cannot be supplied by another module's `createsOutputs`. `simInit` will determine which modules have no upstream dependencies and *only* these will be selected for running *only* their `init` events. This can be useful e.g., if there is a module that `createsOutputs` for a `studyArea`.
* `.plots` arg in `spades` can be set to `NA` to turn of all plotting. This can also be set with `option(spades.plots = NA)`, 
* minor bugfixes
* `moduleMetadata` no longer runs `.inputObjects`. In addition to being unnecessary and slow, it was also failing with `reproducible (==1.2.16)` because it was trying to run `Cache`, which had a bug for this case. Now, `moduleMetadata` no longer runs the `.inputObjects` internally, so this bug is no longer relevant.
* two new options -- `spades.loadReqdPkgs`, so a user can turn off loading of packages, and `spades.dotInputObjects`, so a user can omit running of the `.inputObjects` function in modules during `simInit`. These are updated in `spadesOptions`.
* some tests and examples have been shortened, to fit within the CRAN guidelines
* improved documentation
* new hidden function `runScheduleEventsOnly` will extract only the `scheduleEvents` call; needed for `options(spades.futureEvents = TRUE)`
* new option: `spades.saveFileExtensions` which allows users to use the `outputs(sim)` mechanism for saving, for file extensions that are not already supported by `.saveFileExtensions()`
* many issues addressed and closed

## Dependency Changes
* drop support for R 4.0 as dependency packages no longer support it;
* require `reproducible` v2.0.5 or higher;
* require `quickPlot` v1.0.2 or higher;
* removed `googledrive` from Suggests.

## Bug Fixes
* several minor, e.g., `Plots` when not specifying `fn`, but `usePlot = FALSE`
* many examples that were protected behind `\dontrun` or `\donttest` were stale; these have been updated
* `saveFiles` bugfix: multiple objects names can now be passed to `.saveOutputs` module parameters.

## Deprecated, Defunct, and Removed Features
* several previously-deprecated functions have been made defunct: `remoteFileSize()`, `updateList()`. These will be removed by mid-2023.

# SpaDES.core 1.1.0

## Enhancements
* messaging in a module can now handle "same line" messages --> simply use the standard `"\b"` in the message, and it will occur on same line as previous message
* `Plots` now appends the filename any file saved during `Plots` to the `outputs` slot of the `sim`, i.e., it will show up in `outputs(sim)`
* `logPath` is now a function that points to a sub-folder of `file.path(outputPath(sim), "log")`
* `defineEvent` is a new function that allows a different way of specifying events than the `doEvent` function. This is not yet being used in the module templates, so does not appear with `newModule`.
* `spades` can now run correctly, with "incomplete" modules that don't have metadata or even a module file. Now, a "module" will work with `simInit` and `spades` if a `doEvent.XXX` exists somewhere e.g., in the `.GlobalEnv`. `spades` will find it through inheritance and no longer complain if specific structures are absent. This may make it easier to learn how to use `SpaDES` as it mimics a more normal user experience where functions are all in the `.GlobalEnv`.
* new option `spades.DTthreads` to limit the number of threads used by `data.table` (default 1).
  Users can override this default if needed; modules can `setDTthreads()` as needed,
  but should restore the original value `on.exit`.
* `saveSimList()` and `loadSimList()` accept `.qs` or `.rds` files
* `spades` and `simInit` now force UTF-8 encoding; this is reset `on.exit`. If a module needs a different character encoding, then it can be set within the module code.
* `.studyAreaName` parameter added to default module metadata when using `newModule`.
* changes to template module documentation - removal of "module usage" as it is not relevant *within* a module, and minor restructuring
* new option `spades.scratchPath`, to be used for e.g., temporary raster files and temporary SpaDES recovery mode objects.
* The default temporary `rasterTmpDir` has changed to be a subdirectory of `scratchPath`.
  **`rasterPath` will be deprecated in a future release.**
* New default temporary `terraTmpDir` set as a subdirectory of `scratchPath`.
* Old way of naming module functions with full module name plus "Init" ('non namespaced') no longer works. Message now converted to `stop`.
* use `README.md` instead of `README.txt` in new modules.

## Dependency Changes

* removed `RandomFields` dependency, as that package is no longer maintained;
* added `NLMR` to Suggests to provide random landscape generation capabilities previously provided by `RandomFields`.

## Bug Fixes

* `memoryUse` was not correctly handling timezones; if the system call to get time stamps was in a different timezone compared to the internal SpaDES event queue, then the memory stamps were not correctly associated with the correct events.
* improved handling of `data.table` objects using `loadSimList()`
* Fixed caching of `.inputObjects` to correctly capture objects that were assigned to `mod$xxx`.
* Fixed caching of `simList` objects where changes to functions appeared to be undetected, and so a Cache call would return a stale module with function code from the Cached `simList`, which was incorrect.
* fix recovery mode bug: use scratch directory specified by the user via `options(spades.scratchPath)` (see above).
* `objSize` could have infinite recursion problem if there are `simList` objects inside `simList` objects. Fixed with new `reproducible::objSize`, which uses `lobstr::obj_size`.
* several minor fixes, including in `Plots`
* fixes to `saveFiles` related to `data.table` assignment and use in `outputs(sim)`
* fix to `paramCheckOtherMods` to deal with `call` parameters

# SpaDES.core 1.0.10

## Enhancements

* **experimental new feature** `SpaDES` modules can now be R packages.
  The simplest way to convert a module to a package is using the new function `convertToPackage`.
  Benefits of doing this are so that a SpaDES module can benefit from the infrastructure of an R package (e.g., `devtools::document`, `devtools::check`, setting up Continuous Integration systems etc.).
  Any documentation written using `#'` i.e., `roxygen2` will be copied immediately above the function where it was sitting. 
* `newModule` now correctly places the `SpaDES.core` package dependency in the `reqdPkgs` element of the metadata, instead of `# SpaDES.core`. It will put the full GitHub reference if SpaDES.core was installed directly from GitHub.
* There is a bug in `qs` package: either `qsave` or `qread` converts `data.table` objects to `list` objects. `loadSimList` has a work around internally to convert these objects back to `data.table`, if the metadata indicate that the objects should be `data.table` objects.
* new function: `paramCheckOtherMods`.
  Can be used within a module to assert that a parameter has the same value as the same parameter in other modules.
  This is therefore a check of a parameter that might be considered a `.global` and passed within `simInit(..., params = list(.globals = list(someParam = "someValue")))`, but the user did not do that.
* now `spades` messaging when e.g., `debug = 1` can correctly accommodate nested `spades` calls, i.e., a SpaDES module calling `spades` internally.
* `newModule` now puts `SpaDES.core` dependency in the correct `reqdPkgs` instead of `# SpaDES.core` metadata element
* to further the transition to using `.plots` instead of `.plotInitialTime`, `Plots` will check whether `.plotInitialTime` is actually set in the module metadata first.
  Only if it is there, will it evaluate its value. Currently, modules get default values for `.plotInitialTime` even if the module developer did not include it in the module metadata. 

## Dependency Changes

* drop support for R 3.6 (#178)

## Bug Fixes

* minor bugfix when `debug` arg of `spades` is set to an event type that is also in the core modules (e.g., save, load), such as "init"
* `Cache`-ing of a `simList`, when `quick` is a character vector, errored. Now fixed.
* `moduleMetadata` incorrectly dropped `defineModuleListItems` under certain signatures.

# SpaDES.core 1.0.9

## Enhancements

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

## Dependency Changes
* no changes

## Bug Fixes
* When `.inputObjects` was cached (via setting `useCache = '.inputObjects'` parameter), it was "too sensitive". Changes to any module's parameters, not just the current module, would cause rerun of `.inputObjects`. Now it correctly identifies parameter changes only in the current module. THIS WILL CAUSE some existing caches to trigger a rerun once; after this, it will be less sensitive
* `restartSpades` did not correctly deal with objects that did not yet exist prior to the event. Fixed with: 24b9cd12973aa81a9a4923a02225e095fa28f77a.
* `restartSpades` was losing the previous completed events list. This has been fixed; it is now kept after `restartSpades`
* Plots - fixed issues with plot layer names and plot overlaying when passing `data` only (e.g. `quickPlot::Plot`-like behaviour)
* `simInitAndSpades` now has `.plots` arg to match `spades`
* fix raster file name query for GDAL 3.3.2 (#174; @rsbivand)

# SpaDES.core 1.0.8

## Enhancements
* `Plots` function can be used like `Plot`, but with `types` specified.
  The devices to save on disk will have some different behaviours to the screen representation, since "wiping" an individual plot on a device doesn't exist for a file device.

# SpaDES.core 1.0.7

## Enhancements
* `Plots` function that will produce zero to 4 types of items that are relevant for plotting: 1) Visual on screen, 2) The plot object saved to disk, 3) The raw data that went into the plot and 4) The plot as one or more image files, e.g., `.png` or `.pdf` via `ggsave`
* `spades` now accepts an `events` argument, which will limit the events that are run to those specified in the argument. This seems to be most useful for the `init` case, e.g., `spades(sim, events = "init")`. See `?spades`.
* messaging during `simInit` now is prefixed with `Sys.time()` and `"simInit`"
* messaging during `spades` is simplified to take up fewer characters: `INFO::` has been removed
* `simInit` now checks for minimum # SpaDES.core of `SpaDES.core` needed in a module and stops if it fails, giving instructions how to upgrade.
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

# SpaDES.core 1.0.6

## Enhancements
* more informative message re: module package # SpaDES.cores when `spades.useRequire = FALSE` (#141)
* now detects user-created memory leaks when a user adds a closure or formula to the `sim`; user informed with a `warning`

## Dependency Changes
* no changes

## Bug Fixes
* use `try()` with `communities()` to skip tests on systems without `igraph` GLPK support.
* prevent package (re)installation during examples, tests, vignettes.
* fix failures on R-devel caused by `RandomFields` being unavailable.
* minor bug fixes

# SpaDES.core 1.0.5

## Enhancements
* New experimental `spades.futureEvents` option. If set to `TRUE`, spades will run module events in a "future" (see `future` package), if they do not produce outputs for other modules.
* enable automated module code checking with GitHub Actions (`use_gha()` and corresponding vignette; #74)
* `newProject` creates Rstudio `.Rproj` file if invoked in Rstudio
* moved `paddedFloatToChar` to reproducible; but re-exported here, so still usable.
* modules can now use a parameter called `.seed` which is a named list where names are the events and the elements are the seed with which to run the event. During `doEvent`, `SpaDES.core` will now `set.seed(P(sim)$.seed[[currentEvent]])` and reset to random number stream afterwards.

## Dependency Changes
* completely removed `dplyr`, `lubridate`, `R.utils`, `tools`, `backports` and `rlang` from dependencies
* move `tcltk` to `Suggests`
* remove `devtools`, `microbenchmark` from `Suggests`

## Bug Fixes
* minor bug fixes
* use new `all.equal(..., check.environment = FALSE)` for internal testing 

# SpaDES.core 1.0.3

## Enhancements
* none

## Dependency Changes
* completely removed `RCurl` dependency (#120)
* Suggests `sp` because it's linked in documentation (#120)

## Bug Fixes
* fix `pkgDeps` example for new # SpaDES.core of `Require`
* minor bug fixes

# SpaDES.core 1.0.2

## Enhancements
* `desc` argument in `defineParameter`, `expectsInput`, and `createsOutput` can now have extraneous spaces and End-of-Line characters.
  This means that they can now be written more easily with a single set of quotes, without needing `paste`.
  The accessor functions, `moduleParams`, `moduleInputs`, and `moduleOutputs` all will strip extraneous spaces and End-of-Line characters.
* new helper functions for debugging: `writeEventInfo()` and `writeRNGInfo()` to write info to file.

## Dependency Changes
* drop support for R < 3.6
* removed imports from `stringi`

## Bug Fixes

* minor bug fixes in sample modules
* module template has full path instead of `..` for `moduleParams` etc. This is more accurate. 
* address changes to active bindings in R-devel
* fix CRAN check errors
* reduced the number of tests run on CRAN (extended tests still run on GitHub Actions)

# SpaDES.core 1.0.1

## Enhancements

* `Par` is now an `activeBinding` (similar to `mod`) pointing to `P(sim)`; this allows for tab autocomplete to function correctly.
* new helper functions to extract parameters, inputs, and outputs tables from module metadata:
  `moduleParams()`, `moduleInputs()`, `moduleOutputs()`. These are now used in default `.Rmd` template.
* better testing of `memoryUse` functionality
* A pointer to `sim` is now created at `.pkgEnv$.sim` at the start of `spades` call, rather than `on.exit`; failures due to "out of memory" were not completing the `on.exit`
* improved templating of new modules, including support for automated module code checking using GitHub Actions (`newModule()` sets `useGitHub = TRUE` by default).

## Dependency Changes
* add `usethis` to Suggests for use with GitHub Actions

## Deprecated, Defunct, and Removed Features
* none

## Bug Fixes
* tests for `Filenames` function coming from `reproducible` package
* `options('spades.recoverMode')` was creating temp folders every event and not removing them; now it does.

# SpaDES.core 1.0.0

## Enhancements

* several efforts made to reduce memory leaks over long simulations; if memory leaks are a problem, setting `options('spades.recoveryMode' = 0)` may further help
* Updates to deal with new backend with `reproducible`
* better assertions inside list elements of `simInit`, e.g., `simInit(times = list(start = "test"))` now fails because times must be a list of 2 `numeric` objects
* messaging is now all with `message` instead of a mixture of `message`, `cat` and `print`.
  This allows for easier suppressing of messaging, e.g., via `suppressMessages`.
  This was requested in a downstream package, `SpaDES.experiment` that was submitted to CRAN but rejected due to the now former inability to suppress messages.
* `restartR` saves simulation objects using `qs::qsave()` which is faster and creates smaller file sizes.

## Dependency Changes

* moved packages from Imports to Suggests: `codetools`, `future`, `httr`, `logging`, and `tcltk`
* removed `archivist`
* `qs` now used for improved object serialization to disk

## Deprecated, Defunct, and Removed Features

`.objSizeInclEnviros` and removed

## Bug Fixes

* removed mention of 'demo' from intro vignette (#110)
* `objectSynonyms` caused a breakage under some conditions related to recovering a module from `Cache`.

# SpaDES.core 0.2.8

## Enhancements

* Changed all internal `print` and `cat` statements to message to allow use of `suppressMessages`, as recommended by CRAN
* added file based logging via `logging` package, invoked by setting `debug` argument in `spades` function call to a `list(...)`. `?spades` describes details

## Bug Fixes

* `restartR` minor bug fixes

# SpaDES.core 0.2.7

## Dependency Changes

* Removed dependency packages `DEoptim`, `future.apply`, `Matrix`, `parallel`, `pryr`, `purrr`, and `rgenoud`, which are no longer required.
  See "deprecated" info below.
* added `whisker` to Imports to facilitate module file templating (#100)

## Enhancements

* memory and peak memory estimation is now available for *nix-type systems, when `future` is installed.
  See new vignette `iv-advanced` and `?memoryUse`.
* new function and capacity: `restartR`.
  Restarts R mid-stream to deal with apparent memory leaks in R.
  In our experience with large projects that have long time horizons, there appears to be a memory leak at a low level in R (identified here: <https://github.com/r-lib/fastmap>).
  This has prevented projects from running to completion. Without diagnosing the root cause of the memory inflation, we have noticed that interrupting a simulation, saving the `simList`, restarting R, resets the memory consumption back to levels near the start of a simulation.
  The new functionality allows a user who is hitting this memory leak issue to restart R as a work around.
  See `?restartR` for instructions.
* new function `newProject` to initialize a SpaDES project with subdirectories `cache/`, `inputs/`, `modules/`, and `outputs/`, and `setPaths()` accordingly.

## Bug Fixes

* `newModule()` now uses `open = interactive()` as default to prevent files being left open during tests.
* various bug fixes and improvements.

## Deprecated, Defunct, and Removed Features

* `experiment()`, `experiment2()`, and `POM()` have been moved to the `SpaDES.experiment` package

# SpaDES.core 0.2.6

## Dependency Changes

* R 3.5.0 is the minimum # SpaDES.core required for `SpaDES.core`.
  Too many dependency packages are not maintaining their backwards compatibility.
* added `backports` to Imports for R-oldrel support
* removed `googledrive` dependency (this functionality moved to `reproducible`)

## documentation

* improved documentation for `P`, `params`, and `parameters`, thanks to Louis-Etienne Robert.

## Enhancements

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

## Bug Fixes

* Internal bugs during `simInit` especially in some weird cases of `childModules`.
* packages listed in `reqdPkgs` not being loaded when only listed in child modules. Fixed in `5cd79ac95bc8d190e954313f125928458b0108d2`.
* fixed issue with saving simulation outputs at simulation end time.

# SpaDES.core 0.2.5

* improved messaging and fixed test failures when GLPK installed but not used by `igraph`
* compatibility with `RandomFields` >= 3.3.4

# SpaDES.core 0.2.4

## package dependencies

* `archivist` and `devtools` added to Suggests because they are used in vignettes
* minimum `reproducible`

# SpaDES.core 0.2.6

## Enhancements

* new vignette on caching `SpaDES` simulations moved from `SpaDES` package.
* `simList` environment now has `emptyenv()` as its `parent.env`. The biggest user-facing changes are:
  
    - functions placed in the `envir(sim)` (unusual, but may occur) won't find objects in the `.GlobalEnv`;
    - lighter memory footprint, as functions take RAM due to the objects in the `parent.env` in which they are defined (little know fact identified here: http://adv-r.had.co.nz/memory.html#gc identified as a possible source of memory leaks).

* module's function environment in the `simList` now has its parent `asNamespace("SpaDES.core")` instead of the `envir(sim)` (as mentioned above), i.e,. `parent.env(sim[[currentModule(sim)]])` is `asNamespace("SpaDES.core")`. The main user-noticeable changes of this are that module functions will not accidentally find objects in the `simList` unless they are actually passed in explicitly as arguments.
* New active binding, `mod` that works as a module-specific variable, similar to a private object, *i.e.*, `mod$a` is a local object inside the module that persists across events. It is a pointer to `sim[[currentModule(sim)]]$a`
* New function `scheduleConditionalEvent`, which allows an event to be scheduled based on a condition. Still experimental.
* An experimental new function and feature, `objectSynonyms`, which will create active bindings of two names to a single object
* User can now specify `modulePath` as a character vector, e.g., `simInit(..., paths = list(modulePath = c(".", "test")))`. This means that a user can organize the modules in different locations.
* `modulePath` now has a new argument, `module`, where user can specify (a) specific module(s)'s path. Modifications were implemented to `dataPath` to utilize this new feature
* `simInit` and `spades` now call `setPaths(paths)` or `setPaths(sim$paths)`, unsetting them `on.exit` internally to make the paths used for functions e.g., `reproducible::Cache` to use the correct path
* under-the-hood speed improvements for the DES (about 20% faster) -- 38 microseconds per event under ideal conditions
* improved default path settings in `.inputObjects` (#83)
* following `reproducible` package updates, now uses `data.table::setattr` internally to avoid copying of
objects (this may have very little/no effect on `simList` objects)
* `suppliedElsewhere` has a new argument, `returnWhere`, a logical which will cause a logical of length 3 to be returned, indicating in which of the 3 other places the object may have been supplied, instead of length 1, still the default.

## Bug Fixes

* fix to work with latest `data.table` v1.12.0 (#85, @mattdowle)
* several minor, including to `Copy` (error existed because function inheritance persisted even though the location of the function was moved)

# SpaDES.core 0.2.3

## package dependencies

* add `RandomFields` to Suggests, as it is in the Suggests of `SpaDES.tools` and used in examples/tests.

## Enhancements

* new option and default setting: `options("spades.saveSimOnExit" = TRUE)`. This will save the state of the `simList` to an object as `SpaDES.core:::.pkgEnv$.sim`, with a message, if there is a hard exist. There is virtually no computational cost to this, as the object is already in RAM.
* `simList` internals changed. It now inherits from `environment`. Amongst other things, this means that tab autocomplete in RStudio now works for objects in the `simList`. Also, we removed several associated methods, `$`, `[[`, `ls`, `ls.str`, `objects`, as the defaults for environments work correctly with the `simList` now
* `debug` arg in `spades` call can now take numeric, currently 1 or 2, giving a few pre-packaged informative messaging each event
* new function `elapsedTime` which gives a summary of the clock time used by each module or event
* most metadata entries now have accessor of same name, e.g., `inputObjects(sim)` returns the `inputObjects` data.frame.
* new function `citation` replaces `utils::citation` with an S4 generic. If `package` arg is a `character`, it dispatches `utils::citation`; if a `simList`, it gives the citation for the module(s)
* improved messaging when GLPK not installed (*e.g.*, on macOS)
* `downloadModule()` now prints the module # SpaDES.core downloaded (#77)
* 

## Bug Fixes

* resolved `.inputObjects()` name conflict (internal `.inputObjects` renamed to `._inputObjectsDF`; `.outputObjects` renamed to `._outputObjectsDF`)
* module `.inputObjects` evaluated based on module load order (#72)
* `.robustDigest` fix for `simList` objects -- needed to omit `._startClockTime` and `.timestamp`

# SpaDES.core 0.2.2

## package dependencies

* remove `sp` from imports

## Enhancements

* none

## Bug Fixes

* fix issues with failing tests on macOS

# SpaDES.core 0.2.1

## package dependencies

* requires new # SpaDES.core of `reproducible` (>=0.2.2)

## Enhancements

* new option `spades.useRequire`: a logical which causes `simInit` to load packages with `Require` or `require`. Lower case is generally faster, but will not handle the case of uninstalled packages, so should only be used once all packages are installed.
*    - * new option `spades.keepCompleted`: a logical which causes `spades()` to keep (`TRUE`) or not keep a record of completed events. Keeping track of completed events when they are many (>1e5) gets slow enough that it may be worth turning it off.

* more robust and faster tests are used now, care of helper functions in `tests`
* `all.equal.simList` now removes all time dependent attributes, *e.g.*, `._startClockTime` and `.timestamp`
* speed enhancements for Discrete Event Simulator; now overhead is 1.3 seconds for 5000 events or, per event, 260 microseconds (185 microseconds if `options("spades.keepCompleted" = FALSE)`

## Bug Fixes

* Improvements to caching of functions with `simList` objects:

    - Cached functions with a `simList` in the arguments would erroneously return cached copies of functions. These now are copied through from argument `simList`, rather than cached `simList`. This means that changes to the function definitions in a module will persist (e.g., debugging via `browser()` will work correctly)
    - functions with `simList` in arguments that return a `simList` will now do a post digest of the output. This will be compared with the predigest, and only those object which changed in the `simList` will be modified.
    - caching of `.inputObjects` function was incorrect. Fixed.

# SpaDES.core 0.2.0

## Enhancements

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
* `extractURL` will extract the `sourceURL` from metadata, given an object name.
* `makeMemoiseable` and `unmakeMemoisable`, new methods, each the inverse of the other, to deal with imperfect memoised returns under some cases of `simList`.
* new option, `spades.keepCompleted`, `TRUE` by default, which can be useful for dramatically speeding up the DES when there are many (>10,000) events.

## Deprecated, Defunct, and Removed Features

* remove `fileExt` -- use `tools::file_ext` instead

## Bug Fixes

* fix tests based on `data.table` changes (@mattdowle, #64).
* re-export `start` and `end`.
* `newModule` template modified slightly based on workshop feedback.
* `setPaths` now only sets the directories that are passed into it.
* `all.equal.simList` method strips a small number of attributes that are used internally that create false failures.
* speed enhancements.
* other minor bug fixes.

## Dependency Changes

* add package imports `tools`, `pryr`.
* removed package imports `rgeos`, `RCurl` and `googledrive`.

# SpaDES.core 0.1.1

* uses `reproducible::Require` instead of `SpaDES.core::loadPackages` to load required packages. Currently, does not use SpaDES.core control for packages, but does use installing (from CRAN or GitHub), and loading (via require). This means a module can indicate a GitHub package, e.g,. `achubaty/amc@development`
* environments in modules are now as follows: 
    
    - Functions defined in a module are sourced into an environment located here: `sim@.envir$<moduleName>`, and it is a is a child of `sim@.envir`. Functions can be found in this environment, but prefixing functions is not necessary, because modules functions are within this environment already. 
    - `sim@.envir` is a child of `SpaDES.core`
    
* scoping from within a function that is defined in a module is thus: 

    - `sim@.envir$<moduleName>` --> `sim@.envir` --> `SpaDES.core` --> all imported packages including `base` --> `.GlobalEnv` --> `search()`

* speed improvements:

    - the core DES is now built around lists, rather than `data.table` objects. For small objects (e.g., the event queue) that have fewer than 200 objects, lists are faster. Accessors (e.g., `events(sim)`, `completed(sim)`) of the event queues still show `data.table` objects, but these are made on the fly.
    - `.parseModule` and `.parseModuePartial` now put their parsed content into a temporary environment (`sim@.envir$.parsedFiles$<Full Filename>)` during the `simInit`, which gets re-used. Previously, files were parsed multiple times in a given `simInit` call. Several functions now have `envir` argument to pass this through (including `module# SpaDES.core`, `packages`, `checkParams`)

* parsing of modules is now more intelligent, allowing for modules to contain functions (the current norm) and but they can also create objects at the module level. These can use the `sim` object in their definition. These objects can, for example, be used to help define parameters, for example, e.g., `startSimPlus1 <- start(sim) + 1` can be defined in the module and used in `defineModule`
* remove `grDevices` from Imports as it was not used (#1)
* remove `chron` and `CircStats` dependencies
* remove functions `dwrpnorm2` and move to package `SpaDES.tools`
* remove unused function `F()` due to conflicts with `F`/`FALSE`.
* improved download of module data: added new `quickCheck` argument
* improved download of modules: use fuzzy matching
* new option: `spades.switchPkgNamespaces` which allows the user to turn off the `SpaDES` feature that loads and unloads libraries specific to each module. While useful, it slows down computations, in some cases, by a lot.
* bug fixes:

    - in `zipModule` that omitted the checksum file from being included when `data = FALSE` (#3)
    - caching of `.inputObjects` functions was evaluating `outputObjects` instead of `inputObjects`. Now corrected.

* If `.inputObjects` contains arguments other than just `sim`, these will be evaluated as function inputs by the Cache mechanism (via `.useCache`), therefore correctly assessing when those inputs changed, e.g., if they are files and the arg is wrapped in `asPath`, then any change to the underlying file will cause a re-cache.  e.g., `.inputObjects <- function(sim, importantFile = asPath(file.path(inputPath(sim), "theFile.rdata"))) { ... }`
* default `debug` option in `spades()` now uses the package option `spades.debug` and default is set to `FALSE` (#5)
* various other speed improvements and bug fixes
* convert `P` to a function, rather than S4 generic and method, for speed.
* `@importFrom` only used functions from `utils` due to name conflicts with `raster::stack` and `utils::stack`
* new function `remoteFileSize` to check the size of remote files
* new namespaced function `dataPath` will return `file.path(modulePath(sim), currentModule(sim), "data")`, which will return a different path, depending on which module it is placed inside.
* add `crayon` to Imports -- now messages are more colour-coded;
* bug fix in 'inputs' for the case of loading objects from the global environment, either from the same object to the same object, or from different global objects overwriting on the same `simList` object;


# SpaDES.core 0.1.0

* A new package, which takes all core DES functionality out of the `SpaDES` package:

    - see `?SpaDES.core` for an overview

* various speed improvements and bug fixes
