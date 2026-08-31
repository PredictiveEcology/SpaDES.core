#' `SpaDES.core` options
#'
#' These provide top-level, powerful settings for a comprehensive SpaDES workflow.
#' To see defaults, run `spadesOptions()`.
#' See Details below.
#'
#' @export
#' @return named list of the *default* package options.
#'
#' @details
#'
#' Below are options that can be set with `options("spades.xxx" = newValue)`,
#' where `xxx` is one of the values below, and `newValue` is a new value to
#' give the option. Sometimes these options can be placed in the user's `.Rprofile`
#' file so they persist between sessions.
#'
#' The following options are likely of interest to most users
#' \tabular{lcl}{
#'   *OPTION* \tab *DEFAULT VALUE* \tab *DESCRIPTION* \cr
#'   `spades.allowInitDuringSimInit` \tab `FALSE`
#'      \tab New feature as of `SpaDES.core > 1.1.1.9001`; If set to `TRUE`,
#'      `simInit` will evaluate the dependencies in the metadata objects and determine whether
#'      there are modules whose `init` events can be run safely prior to
#'      the `.inputObjects` of other modules, i.e., if a module's `expectsInput`
#'      is not being supplied by any other module's `createsOutput`.\cr
#'
#'
#'   `spades.browserOnError` \tab `FALSE` \tab If `TRUE`, the default, then any
#'   error rerun the same event with `debugonce` called on it to allow editing
#'   to be done. When that browser is continued (e.g., with 'c'), then it will save it
#'   re-parse it into the `simList` and rerun the edited version.
#'   This may allow a `spades()` call to be recovered on error,
#'   though in many cases that may not be the correct behaviour.
#'   For example, if the `simList` gets updated inside that event in an iterative
#'   manner, then each run through the event will cause that iteration to occur.
#'   When this option is `TRUE`, then the event will be run at least 3 times: the
#'   first time makes the error, the second time has `debugonce` and the third time
#'   is after the error is addressed. `TRUE` is likely somewhat slower.\cr
#'
#'   `reproducible.cachePath` \tab `getOption('reproducible.cachePath')`
#'      \tab The default local directory in which to cache simulation outputs.
#'   Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/cache`).\cr
#'
#'   `spades.cacheChaining` \tab `FALSE`
#'      \tab If set to `TRUE`, then sequential events that are cached can re-use
#'      their `digest` step. The second event will only digest the functions and
#'      parameters, but not objects. Every event will add a tag or remove that tag
#'      if it is cached or not cached, so this will not mistakenly `cacheChain` when
#'      it isn't appropriate.\cr
#'
#'   `spades.codeCheckEngine` \tab `"v1"`
#'     \tab Which module code checker `simInit()` uses when
#'     `spades.moduleCodeChecks` is on. `"v1"` is the legacy checker; `"v2"` is
#'     the structured checker behind [codeCheckModule()], which reports findings
#'     as a table and honours `# nolint` comments. `"v2"` needs \pkg{xmlparsedata}.\cr
#'
#'   `spades.compressionLevel` \tab `1L`
#'     \tab The compression level `saveSimList()` passes to \pkg{archive} when it
#'     writes a `.tar.gz` bundle. Higher is smaller but slower.\cr
#'
#'   `spades.debug` \tab `TRUE`
#'     \tab  The default debugging value `debug` argument in `spades()`.\cr
#'
#'   `spades.dotInputObjects` \tab `TRUE`
#'     \tab  This is used in `simInit`; if set to `TRUE` then the `.inputObjects`
#'           function will be run; if `FALSE`, then it will be skipped.\cr
#'
#'   `spades.DTthreads` \tab `1L`
#'     \tab  The default number of \pkg{data.table} threads to use.
#'     See also `?data.table::setDTthreads`.\cr
#'
#'   `spades.evalPostEvent` \tab `NULL`
#'     \tab  User can put any `quote`d code to evaluate after each event
#'      (`.inputObjects` included), e.g., `quote(print(.robustDigest(sim$rstLCC)))`
#'      will print the digest value of the `sim$rstLCC` after each event so a
#'      user can monitor its changes.\cr
#'
#'   `spades.futureEvents` \tab `FALSE`
#'     \tab  If set to `TRUE`, the event simulator will attempt to spawn events
#'     whose outputs are not needed (by other events in the `simList`) into a future.
#'     In some cases, this will speed up simulations, by running some events in parallel.
#'     Still VERY experimental. Use cautiously.\cr
#'
#'   `spades.futurePlan` \tab `"callr"`
#'     \tab The [future::plan()] used to run the memory-use monitor started by
#'     `spades.memoryUseInterval`. Must be something other than `"sequential"`,
#'     otherwise `spades()` errors when memory monitoring is requested. If a
#'     `future::plan()` is already set, that plan wins and this option is
#'     updated to match.\cr
#'
#'   `spades.logPath`
#'      \tab Defaults to a subdirectory (`log/`) of the simulation output directory.
#'      \tab The default local directory to write simulation log files.\cr
#'
#'   `spades.inputPath`
#'      \tab Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/inputs`)
#'      \tab The default local directory in which to look for simulation inputs.\cr
#'
#'   `spades.keepCompleted` \tab `TRUE`
#'     \tab Whether `spades()` records each event in the completed-event list.
#'     Set to `FALSE` for very long simulations where the bookkeeping itself
#'     becomes a measurable cost; `completed()` is then empty. See also
#'     `spades.nCompleted`.\cr
#'
#'   `spades.loadReqdPkgs`
#'      \tab Default is `TRUE`
#'      \tab If `TRUE`, any `reqdPkgs` will be loaded via `Require` or `require`.
#'      If `FALSE`, no package loading will occur. This will mean that
#'      modules must prefix every function call from a package with that package name
#'      with double colon (`::`).\cr
#'
#'   `spades.lowMemory`
#'     \tab `FALSE`
#'     \tab If true, some functions will use more memory efficient (but slower) algorithms.\cr
#'
#'   `spades.memoryUseInterval`
#'     \tab `FALSE`
#'     \tab A numeric in seconds indicating how often sample the memory use. This will
#'     be run in a separate `future` process so it can monitor the main process.
#'     To access the resulting memory use table, use `memoryUse(sim)` after the simulation
#'     has terminated.\cr
#'
#'   `spades.messagingNumCharsModule`
#'     \tab `21`
#'     \tab The number of characters to use for the messaging preamble on each
#'     line of the messaging during spades calls.\cr
#'
#'   `spades.moduleCodeChecks`
#'     \tab `FALSE`
#'     \tab Should the various module code checks be run during `simInit`.
#'   **As of `SpaDES.core` 3.1.2.9014 the default is `FALSE`** (checks no longer
#'   run automatically during `simInit`, which they previously slowed). To run the
#'   checks, call [codeCheckModule()] / [codeCheckModules()] manually (no
#'   `simInit()` needed). To restore in-`simInit` checking, set this option to a
#'   named list of toggles, e.g.
#'   `list(suppressParamUnused = FALSE, suppressUndefined = TRUE,
#'   suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE, skipWith = TRUE)`
#'   (or `TRUE` for the defaults); these are passed through to
#'   `codetools::checkUsage()`.\cr
#'
#'   `spades.moduleDocument` \tab  `TRUE`
#'     \tab  When a module is an R package e.g., via `convertToPackage`,
#'     it will, by default, rebuild documentation and reparse during `simInit`.
#'     Since rebuilding documentation (from the `roxygen2` tags) can be time consuming,
#'     a user may wish to prevent this from happening each `simInit` call.
#'     If so, set this option to `FALSE`.\cr
#'
#'   `spades.modulePath` \tab `file.path(tempdir(), "SpaDES", "modules")`)
#'     \tab The default local directory where modules and data will be downloaded and stored.
#'     Default is a temporary directory.\cr
#'
#'   `spades.moduleRepo` \tab  `"PredictiveEcology/SpaDES-modules"`
#'     \tab  The default GitHub repository to use when
#'     downloading modules via `downloadModule`.\cr
#'
#'   `spades.nCompleted` \tab `1000L` \tab The maximum number of completed events to
#'     retain in the `completed` event queue.\cr
#'
#'   `spades.outputPath`
#'     \tab `file.path(tempdir(), "SpaDES", "outputs")`
#'     \tab The default local directory in which to save simulation outputs.\cr
#'
#'   `spades.plots`
#'     \tab The value of this will passed to `.plots` within every module; it will thus
#'     override all module parameter values for `.plots`. This can, e.g., be used
#'     to turn off all plotting.
#'     \tab The default is `NULL`, meaning accept the module-level parameter.\cr
#'
#'   `spades.qsThreads` \tab `1L`
#'     \tab The number of threads `saveSimList()`/`loadSimList()` pass to
#'     \pkg{qs2} when reading or writing a `.qs` file.\cr
#'
#'   `spades.recoveryMode` \tab `1L` \tab
#'   If this is a numeric greater than 0 or TRUE, then the
#'   discrete event simulator will take a snapshot of the objects in the `simList`
#'   that might change (based on metadata `outputObjects` for that module), prior to
#'   initiating every event. This will allow the
#'   user to be able to recover in case of an error or manual interruption (e.g., `Esc`).
#'   If this is numeric, a copy of that number of "most
#'   recent events" will be maintained so that the user can recover and restart
#'   more than one event in the past, i.e., redo some of the "completed" events.
#'   Default is `TRUE`, i.e., it will keep the state of the `simList`
#'   at the start of the current event. This can be recovered with `restartSpades`
#'   and the differences can be seen in a hidden object in the stashed `simList`.
#'   The same mechanism applies during `simInit`: a snapshot is taken before each
#'   module's `.inputObjects` runs, so an interrupted `simInit` can be recovered
#'   with `restartSimInit` (see `?restartSimInit`).
#'   There is a message which describes how to find that.\cr
#'
#'   `spades.reqdPkgsDontLoad` \tab `NULL` \tab Specify any packages that should not
#'   be \emph{loaded} i.e., no `library` or `require`, but they should be installed if
#'   listed in a module's `reqdPkgs`.\cr
#'
#'   `spades.restartRInterval` \tab `0`
#'     \tab How often, in simulation time units, `spades()` restarts R to
#'     reclaim leaked memory. `0`, the default, never restarts. See
#'     [restartR()].\cr
#'
#'   `spades.restartR.clearFiles` \tab `TRUE`
#'     \tab Whether [restartR()] deletes the temporary files it wrote to carry
#'     state across the restart. Set to `FALSE` to keep them for debugging.\cr
#'
#'   `spades.restartR.RDataFilename` \tab `"sim_restartR.RData"`
#'     \tab The filename [restartR()] saves the `simList` to before restarting.\cr
#'
#'   `spades.restartR.restartDir` \tab `file.path(tempdir(), "SpaDES", "outputs")`
#'     \tab The directory [restartR()] writes that file into. See
#'     `?restartR` for how this interacts with `outputPath`.\cr
#'
#'   `spades.saveFileExtensions` \tab `NULL` \tab
#'   a `data.frame` with 3 columns, `exts`, `fun`, and `package` indicating which
#'   file extension, and which function from which package will be used when
#'   using the `outputs` mechanism for saving files during a `spades` call. e.g.,
#'   `options(spades.saveFileExtensions = data.frame(exts = "shp", fun = "st_write",
#'   package = "sf")`.
#'   Then specify e.g.,
#'   `simInit(outputs = data.frame(objectName = "caribou", fun = "st_write", package = "sf"))`
#'   \cr
#'
#'   `spades.saveSimOnExit` \tab `TRUE`
#'     \tab Whether, when an event throws an error, `simInit()`/`spades()` save
#'     the `simList` so it can be recovered instead of lost. Works together with
#'     `spades.recoveryMode`.\cr
#'
#'   `spades.scratchPath` \tab `file.path(tempdir(), "SpaDES", "scratch")`)
#'     \tab The default local directory where transient files from modules and data will written.
#'     This includes temporary `raster` and `terra` files, as well as `SpaDES` recovery mode files.
#'     Default is a temporary directory.\cr
#'
#'   `spades.sessionInfo` \tab `TRUE`)
#'     \tab Assigns the [utils::sessionInfo()] to the `simList` during `simInit` with
#'     the name `sim$._sessionInfo`. This takes about 75 milliseconds, which may be
#'     undesirable for some situations where speed is critical. If `FALSE`, then
#'     this is not assigned to the `simList`.\cr
#'
#'   `spades.switchPkgNamespaces` \tab Defunct. \tab Do not use.\cr
#'
#'   `spades.testMemoryLeaks` \tab `TRUE`.
#'     \tab  There is a very easy way to create a memory leak with R and `SpaDES`,
#'         by adding formulas or functions to `sim$` when the enclosing environment of the
#'         formula or function contained a large object, most relevant here is the `sim` object.
#'         `SpaDES.core` now tests for likely culprits for this and suggests alternatives
#'         with a warning.\cr
#'
#'   `spades.tolerance` \tab `.Machine$double.eps^0.5`.
#'     \tab  The default tolerance value used for floating
#'     point number comparisons.\cr
#'
#'   `spades.urlLog` \tab `TRUE` \tab
#'   If `TRUE` (the default), any files or web addresses (URLs) that modules
#'   download through `prepInputs()` or `preProcess()` during `simInit()` or
#'   `spades()` are recorded, and each one is tagged with the module and event
#'   that asked for it. This makes it easy to see where a simulation's input
#'   data came from. Set to `FALSE` to turn the recording off.\cr
#'
#'   `spades.useragent` \tab `"https://github.com/PredictiveEcology/SpaDES"`.
#'     \tab The default user agent to use for downloading modules from GitHub.\cr
#'
#'   `spades.useRequire` \tab `!tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false"`
#'     \tab The default for that environment variable is unset, so this returns
#'     `TRUE`. If this is `TRUE`, then during the `simInit` call, when packages are
#'     identified as being required, these will be installed if missing, only if
#'     `spades.useRequire` option is `TRUE`, otherwise, `simInit` will fail because
#'     packages are not available.\cr
#'
#' }
#'
spadesOptions <- function() {
  list(
    spades.allowInitDuringSimInit = FALSE,
    spades.browserOnError = FALSE,
    spades.cacheChaining = FALSE,
    spades.compressionLevel = 1L,
    # spades.cachePath = reproCachePath,
    spades.debug = 1, ## TODO: is this the best default? see discussion in #5
    spades.dotInputObjects = TRUE,
    spades.DTthreads = 1L,
    spades.futureEvents = FALSE,
    spades.futurePlan = "callr",
    spades.inputPath = file.path(.spadesTempDir(), "inputs"),
    spades.loadReqdPkgs = TRUE,
    spades.lowMemory = FALSE,
    spades.memoryUseInterval = 0,
    spades.messagingNumCharsModule = 21,
    # Module code checks are OFF by default as of SpaDES.core 3.1.2.9014. They
    # slowed every simInit() and most users do not need them on every run. Run
    # them manually instead with codeCheckModule()/codeCheckModules(). Set this to
    # the named list of toggles (see ?spadesOptions) to re-enable in-simInit checks.
    spades.moduleCodeChecks = FALSE,
    spades.codeCheckEngine = "v1",
    spades.modulePath = file.path(.spadesTempDir(), "modules"),
    spades.moduleRepo = "PredictiveEcology/SpaDES-modules",
    spades.moduleDocument = TRUE,
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(.spadesTempDir(), "outputs"),
    spades.plots = NULL,
    spades.evalPostEvent = NULL,
    spades.qsThreads = 1L,
    spades.recoveryMode = 1,
    spades.reqdPkgsDontLoad = NULL,
    spades.restartRInterval = 0,
    spades.restartR.clearFiles = TRUE,
    spades.restartR.RDataFilename = "sim_restartR.RData",
    spades.restartR.restartDir = file.path(.spadesTempDir(), "outputs"),
    spades.saveFileExtensions = data.frame(
      exts = character(),
      fun = character(),
      package = character()
    ),
    spades.saveSimOnExit = TRUE,
    spades.scratchPath = file.path(.spadesTempDir(), "scratch"),
    spades.sessionInfo = TRUE,
    spades.testMemoryLeaks = TRUE,
    spades.tolerance = .Machine$double.eps^0.5,
    spades.urlLog = TRUE,
    spades.useragent = "https://github.com/PredictiveEcology/SpaDES",
    spades.useRequire = !tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false",
    spades.keepCompleted = TRUE
  )
}
