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
#'      is not being supplied by any other module's `createsOutput`. \cr
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
#'   is after the error is addressed. `TRUE` is likely somewhat slower. \cr
#'
#'   `reproducible.cachePath` \tab `getOption('reproducible.cachePath')`
#'      \tab The default local directory in which to cache simulation outputs.
#'   Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/cache`).\cr
#'
#'   `spades.debug` \tab `TRUE`
#'     \tab  The default debugging value `debug` argument in `spades()` \cr
#'
#'   `spades.dotInputObjects` \tab `TRUE`
#'     \tab  This is used in `simInit`; if set to `TRUE` then the `.inputObjects`
#'           function will be run; if `FALSE`, then it will be skipped.\cr
#'
#'   `spades.DTthreads` \tab `1L`
#'     \tab  The default number of \pkg{data.table} threads to use.
#'     See also `?data.table::setDTthreads`. \cr
#'
#'   `spades.evalPostEvent` \tab `NULL`
#'     \tab  User can put any `quote`d code to evaluate after each event
#'      (.inputObjects included), e.g., `quote(print(.robustDigest(sim$rstLCC)))`
#'      will print the digest value of the `sim$rstLCC` after each event so a
#'      user can monitor its changes \cr
#'
#'   `spades.futureEvents` \tab `FALSE`
#'     \tab  If set to `TRUE`, the event simulator will attempt to spawn events
#'     whose outputs are not needed (by other events in the `simList`) into a future.
#'     In some cases, this will speed up simulations, by running some events in parallel.
#'     Still VERY experimental. Use cautiously. \cr
#'
#'   `spades.logPath`
#'      \tab Defaults to a subdirectory (`logs/`) of the simulation output directory.
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   `spades.inputPath`
#'      \tab Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/inputs`)
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   `spades.loadReqdPkgs`
#'      \tab Default is `TRUE`
#'      \tab Any `reqdPkgs` will be loaded via `Require` or `require`.
#'      If `FALSE`, no package loading will occur. This will mean that
#'      modules must prefix every function call from a package with that package name
#'      with double colon (`::`).\cr
#'
#'   `spades.lowMemory`
#'     \tab `FALSE`
#'     \tab If true, some functions will use more memory
#'     efficient (but slower) algorithms.\cr
#'
#'   `spades.memoryUseInterval`
#'     \tab `FALSE`
#'     \tab A numeric in seconds indicating how often sample the memory use. This will
#'     be run in a separate "future" process so it can monitor the main process.
#'     To access the resulting memory use table, use `memoryUse(sim)` after the simulation
#'     has terminated. \cr
#'
#'   `spades.messagingNumCharsModule`
#'     \tab `21`
#'     \tab The number of characters to use for the messaging preamble on each
#'     line of the messaging during spades calls.\cr
#'
#'   `spades.moduleCodeChecks`
#'     \tab `list(suppressParamUnused = FALSE,
#'   suppressUndefined = TRUE, suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
#'   skipWith = TRUE)`
#'     \tab Should the various code checks be run
#'   during `simInit`. These are passed to `codetools::checkUsage()`.
#'   Default is given by the function, plus these: \cr
#'
#'   `spades.moduleDocument` \tab  `TRUE`
#'     \tab  When a module is an R package e.g., via `convertToPackage`,
#'     it will, by default, rebuild documentation and reparse during `simInit`.
#'     Since rebuilding documentation (from the `roxygen2` tags) can be time consuming,
#'     a user may wish to prevent this from happening each `simInit` call. If so,
#'     set this option to `FALSE` \cr
#'
#'   `spades.modulePath` \tab `file.path(tempdir(), "SpaDES", "modules")`)
#'     \tab The default local directory where modules and data will be downloaded and stored.
#'     Default is a temporary directory  \cr
#'
#'   `spades.moduleRepo` \tab  `"PredictiveEcology/SpaDES-modules"`
#'     \tab  The default GitHub repository to use when
#'     downloading modules via `downloadModule` \cr
#'
#'   `spades.nCompleted` \tab `1000L` \tab The maximum number of completed events to
#'     retain in the `completed` event queue. \cr
#'
#'   `spades.outputPath`
#'     \tab `file.path(tempdir(), "SpaDES", "outputs")`
#'     \tab The default local directory in which to save simulation outputs.\cr
#'
#'   `spades.plots`
#'     \tab The value of this will passed to `.plots` within every module; it will thus
#'     override all module parameter values for `.plots`. This can, e.g., be used
#'     to turn off all plotting.
#'     \tab The default is NULL, meaning accept the module-level parameter\cr
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
#'   There is a message which describes how to find that. \cr
#'
#'   `spades.reqdPkgsDontLoad` \tab `"box"` \tab Specify any packages that should not
#'   be \emph{loaded} i.e., no `library` or `require`, but they should be installed if
#'   listed. The default (`"box"`) is a package that returns a warning if it is
#'   loaded, and so it is excluded from loading.\cr
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
#'   `spades.scratchPath` \tab `file.path(tempdir(), "SpaDES", "scratch")`)
#'     \tab The default local directory where transient files from modules and data will written.
#'     This includes temporary `raster` and `terra` files, as well as SpaDES recovery mode files.
#'     Default is a temporary directory. \cr
#'
#'   `spades.sessionInfo` \tab `TRUE`)
#'     \tab Assigns the [utils::sessionInfo()] to the `simList` during `simInit` with
#'     the name `sim$._sessionInfo`. This takes about 75 milliseconds, which may be
#'     undesirable for some situations where speed is critical. If `FALSE`, then
#'     this is not assigned to the `simList`.\cr
#'
#'   `spades.switchPkgNamespaces` \tab Defunct. \tab Do not use \cr
#'
#'   `spades.testMemoryLeaks` \tab `TRUE`.
#'     \tab  There is a very easy way to create a memory leak with R and SpaDES,
#'         by adding formulas or functions to `sim$` when the enclosing environment
#'         of the formula or function contained a large object, most relevant here is
#'         the `sim` object. SpaDES.core now tests for likely culprits for this
#'         and suggests alternatives with a warning \cr
#'
#'   `spades.tolerance` \tab `.Machine$double.eps^0.5`.
#'     \tab  The default tolerance value used for floating
#'     point number comparisons. \cr
#'
#'   `spades.useragent` \tab `"https://github.com/PredictiveEcology/SpaDES"`.
#'     \tab The default user agent to use for downloading modules from GitHub. \cr
#'
#'   `spades.useBox` \tab FALSE
#'     \tab Unimplemented while memory problems with `box` are resolved.
#'     When it is turned on, this option determines
#'     whether to manage which packages are loaded using the package `box`.
#'     This will have as an effect that `reqdPkgs` will be strict; if a given
#'     module is missing a `reqdPkgs`, then the module will fail to run, with
#'     an error saying the package/function doesn't exist. Without `box`,
#'     modules may run, even though `reqdPkgs` is incorrect, because other modules
#'     may have specified their own packages, which cover the needs of another
#'     package. `useBox = TRUE` will force modules to be accurate with their
#'     `reqdPkgs` \cr
#'
#'   `spades.useRequire` \tab `!tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false"`
#'     \tab : The default for that environment variable is unset, so this returns
#'     `TRUE`. If this is `TRUE`, then during the `simInit` call, when packages are
#'     identified as being required, these will be installed if missing, only if
#'     `spades.useRequire` option is `TRUE`, otherwise, `simInit` will fail because
#'     packages are not available.\cr
#'
#' }
#'
spadesOptions <- function() {
  list( # nolint
    spades.allowInitDuringSimInit = FALSE,
    spades.browserOnError = FALSE,
    spades.compressionLevel = 1L,
    #spades.cachePath = reproCachePath,
    spades.debug = 1, # TODO: is this the best default? see discussion in #5
    spades.dotInputObjects = TRUE,
    spades.DTthreads = 1L,
    spades.futureEvents = FALSE,
    spades.futurePlan = "callr",
    spades.inputPath = file.path(.spadesTempDir(), "inputs"),
    spades.loadReqdPkgs = TRUE,
    spades.lowMemory = FALSE,
    spades.memoryUseInterval = 0,
    spades.messagingNumCharsModule = 21,
    spades.moduleCodeChecks = list(
      skipWith = TRUE,
      suppressNoLocalFun = TRUE,
      suppressParamUnused = FALSE,
      suppressPartialMatchArgs = FALSE,
      suppressUndefined = TRUE
    ),
    spades.modulePath = file.path(.spadesTempDir(), "modules"),
    spades.moduleRepo = "PredictiveEcology/SpaDES-modules",
    spades.moduleDocument = TRUE,
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(.spadesTempDir(), "outputs"),
    spades.plots = NULL,
    spades.qsThreads = 1L,
    spades.recoveryMode = 1,
    spades.reqdPkgsDontLoad = "box",
    spades.restartRInterval = 0,
    spades.restartR.clearFiles = TRUE,
    spades.restartR.RDataFilename = "sim_restartR.RData",
    spades.restartR.restartDir = file.path(.spadesTempDir(), "outputs"),
    spades.saveFileExtensions = data.frame(exts = character(), fun = character(),
                                           package = character()),
    spades.saveSimOnExit = TRUE,
    spades.scratchPath = file.path(.spadesTempDir(), "scratch"),
    spades.sessionInfo = TRUE,
    spades.testMemoryLeaks = TRUE,
    spades.tolerance = .Machine$double.eps ^ 0.5,
    spades.useragent = "https://github.com/PredictiveEcology/SpaDES",
    # spades.useBox = FALSE,
    spades.useRequire = !tolower(Sys.getenv("SPADES_USE_REQUIRE")) %in% "false",
    spades.keepCompleted = TRUE
  )
}
