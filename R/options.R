#' \code{SpaDES.core} options
#'
#' These provide top-level, powerful settings for a comprehensive
#' SpaDES workflow. To see defaults, run \code{spadesOptions()}.
#' See Details below.
#'
#' @export
#' @details
#'
#' Below are options that can be set with \code{options("spades.xxx" = newValue)},
#' where \code{xxx} is one of the values below, and \code{newValue} is a new value to
#' give the option. Sometimes these options can be placed in the user's \code{.Rprofile}
#' file so they persist between sessions.
#'
#' The following options are likely of interest to most users
#' \tabular{lcl}{
#'   \emph{OPTION} \tab \emph{DEFAULT VALUE} \tab \emph{DESCRIPTION} \cr
#'   \code{spades.browserOnError} \tab \code{FALSE} \tab If \code{TRUE}, the default, then any
#'   error rerun the same event with \code{debugonce} called on it to allow editing
#'   to be done. When that browser is continued (e.g., with 'c'), then it will save it
#'   reparse it into the simList and rerun the edited version. This may allow a spades
#'   call to be recovered on error, though in many cases that may not be the correct
#'   behaviour. For example, if the simList gets updated inside that event in an iterative
#'   manner, then each run through the event will cause that iteration to occur.
#'   When this option is \code{TRUE}, then the event will be run at least 3 times: the
#'   first time makes the error, the second time has \code{debugonce} and the third time
#'   is after the error is addressed. \code{TRUE} is likely somewhat slower. \cr
#'
#'   \code{reproducible.cachePath} \tab \code{getOption('reproducible.cachePath')}
#'      \tab The default local directory in which to cache simulation outputs.
#'   Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/cache}).\cr
#'
#'   \code{spades.inputPath}
#'      \tab Default is a temporary directory (typically \code{/tmp/RtmpXXX/SpaDES/inputs})
#'      \tab The default local directory in which to look for simulation inputs.  \cr
#'
#'   \code{spades.debug} \tab \code{TRUE}
#'     \tab  The default debugging value \code{debug} argument in \code{spades()} \cr
#'
#'   \code{spades.lowMemory} \tab \code{FALSE}
#'     \tab If true, some functions will use more memory
#'     efficient (but slower) algorithms. \cr
#'
#'   \code{spades.moduleCodeChecks}
#'     \tab \code{list(suppressParamUnused = FALSE,
#'   suppressUndefined = TRUE, suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
#'   skipWith = TRUE)}
#'     \tab Should the various code checks be run
#'   during \code{simInit}. These are passed to codetools::checkUsage.
#'   Default is given by the function, plus these  \cr
#'
#'   \code{spades.modulePath} \tab \code{file.path(tempdir(), "SpaDES", "modules")})
#'     \tab The default local directory where modules and data will be downloaded and stored.
#'     Default is a temporary directory  \cr
#'
#'   \code{spades.moduleRepo} \tab  \code{"PredictiveEcology/SpaDES-modules"}
#'     \tab  The default GitHub repository to use when
#'     downloading modules via \code{downloadModule} \cr
#'
#'   \code{spades.nCompleted} \tab \code{1000L} \tab The maximum number of completed events to
#'     retain in the \code{completed} event queue\cr
#'
#'   \code{spades.outputPath}
#'     \tab \code{file.path(tempdir(), "SpaDES", "outputs")}
#'     \tab The default local directory in which to save simulation outputs.\cr
#'
#'   \code{spades.recoveryMode} \tab \code{1L} \tab
#'   If this a numeric > 0 or TRUE, then the
#'   discrete event simulator will take a snapshot of the objects in the simList
#'   that might change (based on metadata \code{outputObjects} for that module), prior to
#'   initiating every event. This will allow the
#'   user to be able to recover in case of an error or manual interruption (e.g., \code{Esc}).
#'   If this is numeric, a copy of that number of "most
#'   recent events" will be maintained so that the user can recover and restart
#'   > 1 event in the past, i.e., redo some of the "completed" events. Default is
#'   \code{TRUE}, i.e., it will keep the state of the \code{simList}
#'   at the start of the current event. This can be recovered with \code{restartSpades}
#'   and the differences can be seen in a hidden object in the stashed simList.
#'   There is a message which describes how to find that. \cr
#'
#'   \code{spades.switchPkgNamespaces} \tab \code{FALSE} to keep computational
#'   overhead down. \tab Should the search path be modified
#'     to ensure a module's required packages are listed first?
#'     If \code{TRUE}, there should be no name conflicts among package objects,
#'     but it is much slower, especially if the events are themselves fast. \cr
#'
#'   \code{spades.tolerance} \tab \code{.Machine$double.eps^0.5}.
#'     \tab  The default tolerance value used for floating
#'     point number comparisons. \cr
#'
#'   \code{spades.useragent} \tab \code{"https://github.com/PredictiveEcology/SpaDES"}.
#'     \tab : The default user agent to use for downloading modules from GitHub. \cr
#'
#' }
#'
spadesOptions <- function() {
  list( # nolint
    spades.browserOnError = FALSE,
    #spades.cachePath = reproCachePath,
    spades.debug = 1, # TODO: is this the best default? see discussion in #5
    spades.futurePlan = "callr",
    spades.inputPath = file.path(.spadesTempDir, "inputs"),
    spades.lowMemory = FALSE,
    spades.memoryUseInterval = 0,
    spades.moduleCodeChecks = list(
      skipWith = TRUE,
      suppressNoLocalFun = TRUE,
      suppressParamUnused = FALSE,
      suppressPartialMatchArgs = FALSE,
      suppressUndefined = TRUE
    ),
    spades.modulePath = file.path(.spadesTempDir, "modules"),
    spades.moduleRepo = "PredictiveEcology/SpaDES-modules",
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(.spadesTempDir, "outputs"),
    spades.recoveryMode = 1,
    spades.restartRInterval = 0,
    spades.restartR.clearFiles = TRUE,
    spades.restartR.RDataFilename = "sim_restartR.RData",
    spades.restartR.restartDir = file.path(.spadesTempDir, "outputs"),
    spades.saveSimOnExit = TRUE,
    spades.switchPkgNamespaces = FALSE,
    spades.tolerance = .Machine$double.eps ^ 0.5,
    spades.useragent = "https://github.com/PredictiveEcology/SpaDES",
    spades.useRequire = TRUE,
    spades.keepCompleted = TRUE
  )
}
