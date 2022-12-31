#' `SpaDES.core` options
#'
#' These provide top-level, powerful settings for a comprehensive
#' SpaDES workflow. To see defaults, run `spadesOptions()`.
#' See Details below.
#'
#' @export
#' @noMd
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
#'   `spades.DTthreads` \tab `1L`
#'     \tab  The default number of \pkg{data.table} threads to use.
#'     See also `?data.table::setDTthreads`. \cr
#'
#'   `spades.futureEvents` \tab `FALSE`
#'     \tab  If set to `TRUE`, the event simulator will attempt to spawn events
#'     whose outputs are not needed (by other events in the sim) into a future. In some
#'     cases, this will speed up simulations, by running some events in parallel.
#'     Still VERY experimental. Use cautiously. \cr
#'
#'   `spades.inputPath`
#'      \tab Default is a temporary directory (typically `/tmp/RtmpXXX/SpaDES/inputs`)
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   `spades.lowMemory` \tab `FALSE`
#'     \tab If true, some functions will use more memory
#'     efficient (but slower) algorithms. \cr
#'
#'   `spades.memoryUseInterval` \tab `FALSE`
#'     \tab A numeric in seconds indicating how often sample the memory use. This will
#'     be run in a separate "future" process so it can monitor the main process.
#'     To access the resulting memory use table, use `memoryUse(sim)` after the simulation
#'     has terminated. \cr
#'
#'   `spades.messagingNumCharsModule` \tab `21`
#'     \tab The number of characters to use for the messaging preamble on each
#'     line of the messaging during spades calls.\cr
#'
#'   `spades.moduleCodeChecks`
#'     \tab `list(suppressParamUnused = FALSE,
#'   suppressUndefined = TRUE, suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
#'   skipWith = TRUE)`
#'     \tab Should the various code checks be run
#'   during `simInit`. These are passed to codetools::checkUsage.
#'   Default is given by the function, plus these: \cr
#'
#'   `moduleDocument` \tab  `NULL`
#'     \tab  When a module is an R package e.g., via `convertToPackage`,
#'     it will not, by default, rebuild documentation during `simInit`.
#'     If the user would like this to happen on every call to `simInit`,
#'     set this option to `TRUE` \cr
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
#'   If this a numeric > 0 or TRUE, then the
#'   discrete event simulator will take a snapshot of the objects in the `simList`
#'   that might change (based on metadata `outputObjects` for that module), prior to
#'   initiating every event. This will allow the
#'   user to be able to recover in case of an error or manual interruption (e.g., `Esc`).
#'   If this is numeric, a copy of that number of "most
#'   recent events" will be maintained so that the user can recover and restart
#'   > 1 event in the past, i.e., redo some of the "completed" events. Default is
#'   `TRUE`, i.e., it will keep the state of the `simList`
#'   at the start of the current event. This can be recovered with `restartSpades`
#'   and the differences can be seen in a hidden object in the stashed `simList`.
#'   There is a message which describes how to find that. \cr
#'
#'   `spades.scratchPath` \tab `file.path(tempdir(), "SpaDES", "scratch")`)
#'     \tab The default local directory where transient files from modules and data will written.
#'     This includes temporary `raster` and `terra` files, as well as SpaDES recovery mode files.
#'     Default is a temporary directory. \cr
#'
#'   `spades.switchPkgNamespaces` \tab `FALSE` to keep computational
#'   overhead down. \tab Should the search path be modified
#'     to ensure a module's required packages are listed first?
#'     If `TRUE`, there should be no name conflicts among package objects,
#'     but it is much slower, especially if the events are themselves fast. \cr
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
#'     \tab : The default user agent to use for downloading modules from GitHub. \cr
#'
#'   `spades.useRequire` \tab `!Sys.getenv("SPADES_USE_REQUIRE") %in% "false"`
#'     \tab : The default for that environment variable is unset, so this returns
#'     `TRUE`. If this is `TRUE`, then during the `simInit` call, when pacakges are
#'     identified as being required, these will be installed if missing, only if
#'     `spades.useRequire` option is `TRUE`, otherwise, `simInit` will fail because
#'     packages are not available.\cr
#'
#' }
#'
spadesOptions <- function() {
  list( # nolint
    spades.browserOnError = FALSE,
    #spades.cachePath = reproCachePath,
    spades.debug = 1, # TODO: is this the best default? see discussion in #5
    spades.DTthreads = 1L,
    spades.futureEvents = FALSE,
    spades.futurePlan = "callr",
    spades.inputPath = file.path(.spadesTempDir(), "inputs"),
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
    spades.moduleDocument = NULL,
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(.spadesTempDir(), "outputs"),
    spades.plots = NULL,
    spades.qsThreads = 1L,
    spades.recoveryMode = 1,
    spades.restartRInterval = 0,
    spades.restartR.clearFiles = TRUE,
    spades.restartR.RDataFilename = "sim_restartR.RData",
    spades.restartR.restartDir = file.path(.spadesTempDir(), "outputs"),
    spades.saveSimOnExit = TRUE,
    spades.scratchPath = file.path(.spadesTempDir(), "scratch"),
    spades.switchPkgNamespaces = FALSE,
    spades.testMemoryLeaks = TRUE,
    spades.tolerance = .Machine$double.eps ^ 0.5,
    spades.useragent = "https://github.com/PredictiveEcology/SpaDES",
    spades.useRequire = !Sys.getenv("SPADES_USE_REQUIRE") %in% "false",
    spades.keepCompleted = TRUE
  )
}
