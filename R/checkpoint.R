#' Simulation checkpoints
#'
#' Save and reload the current state of the simulation,
#' including the state of the random number generator,
#' by scheduling checkpoint events.
#'
#' @note Checkpoint files are intended to be used locally, and do not invoke
#' the simulation archiving tools to bundle and subsequently extract simulation
#' files (e.g., file-backed rasters).
#'
#' RNG save code adapted from:
#' <http://www.cookbook-r.com/Numbers/Saving_the_state_of_the_random_number_generator/>
#' and <https://stackoverflow.com/q/13997444/1380598>
#'
#' @param sim           A `simList` simulation object.
#'
#' @param eventTime    A numeric specifying the time of the next event.
#'
#' @param eventType      A character string specifying the type of event: one of
#'                       either `"init"`, `"load"`, or `"save"`.
#'
#' @param debug         Optional logical flag determines whether `sim` debug info
#'                      will be printed (default `debug = FALSE`).
#'
#' @return Returns the modified `simList` object.
#'
#' @seealso [.Random.seed].
#'
#' @author Alex Chubaty
#'
#' @export
#' @importFrom quickPlot .objectNames
#' @importFrom reproducible checkPath
#' @include environment.R
#' @include priority.R
#' @rdname checkpoint
doEvent.checkpoint <- function(sim, eventTime, eventType, debug = FALSE) {
  ### determine whether to use checkpointing
  ### default is not to use checkpointing if unspecified
  ### - this default is set when a new simList object is initialized

  useChkpnt <- !any(is.na(P(sim)))

  ### determine checkpoint file location, for use in events below
  if (useChkpnt) {
    if (is.null(checkpointFile(sim))) {
      checkpointFile <- "checkpoint.qs"
    } else {
      checkpointFile <- checkpointFile(sim)
    }

    if (isAbsolutePath(checkpointFile)) {
      checkpointDir <- checkPath(dirname(checkpointFile), create = TRUE)
    } else {
      checkpointDir <- checkPath(outputPath(sim), create = TRUE)
    }

    checkpointFile <- file.path(checkpointDir, basename(checkpointFile))
  }

  ### event definitions
  if (eventType == "init") {
    if (useChkpnt) {
      sim <- scheduleEvent(sim, 0.00, "checkpoint", "save", .last())
    }
  } else if (eventType == "save") {
    if (useChkpnt) {
      .checkpointSave(sim, checkpointFile)

      # schedule the next save
      timeNextSave <- time(sim, timeunit(sim)) + checkpointInterval(sim)
      sim <- scheduleEvent(sim, timeNextSave, "checkpoint", "save", .last())
    }
  } else {
    warning(paste(
      "Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  }
  return(invisible(sim))
}

#' @param file The checkpoint file.
#'
#' @export
#' @importFrom tools file_ext
#' @rdname checkpoint
checkpointLoad <- function(file) {
  stopifnot(tools::file_ext(file) == "qs")

  ## check for previous checkpoint files
  file <- checkArchiveAlternative(file)
  if (file.exists(file[1])) {
    sim <- loadSimList(file[1])

    do.call("RNGkind", as.list(sim$._rng.kind))
    assign(".Random.seed", sim$._rng.state, envir = .GlobalEnv)
    rm(list = c("._rng.kind", "._rng.state", "._timestamp"), envir = sim@.xData)
    return(invisible(sim))
  } else {
    stop("checkpoint file ", file, " not found.")
  }
}

#' @importFrom stats runif
#' @rdname checkpoint
.checkpointSave <- function(sim, file) {
  sim$._timestamp <- Sys.time() # nolint
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  sim$._rng.state <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) # nolint
  sim$._rng.kind <- RNGkind() # nolint

  tmpEnv <- new.env(parent = emptyenv())
  assign(.objectNames("spades", "simList", "sim")[[1]]$objs, sim, envir = tmpEnv)

  if (file.exists(file[1])) {
    unlink(file)
  }
  saveSimList(.objectNames("spades", "simList", "sim")[[1]]$objs, filename = file,
              ## checkpoints are **local** only, so do not require archiving
              ## e.g., raster files along with the simList
              ## setting these to TRUE will actually break things because the files
              ## get deleted before they are "put back" and are incorrectly linked.
              inputs = FALSE, outputs = FALSE, cache = FALSE, files = FALSE,
              envir = tmpEnv, paths = paths(sim))

  invisible(TRUE) # return "success" invisibly
}
