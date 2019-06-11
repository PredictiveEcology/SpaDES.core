if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

#' Restart an interrupted simulation
#'
#' This is very experimental and hasn't been tested. Use with caution.
#' This function will reparse a single module (currently) into the \code{simList}
#' where its source code should reside, and then optionally restart
#' a simulation that stopped on an error, presumably
#' after the developer has modified the sourcde code of the module that caused the
#' break. This will restart the simulation at the next event in the event queue
#' (i.e., returned by \code{events(sim)}). Because of this, this function will
#' not do anything if the event queue is empty.
#'
#' @details
#' This will only parse the source code from the named module. It will not affect any
#' objects that are in the \code{mod} or \code{sim}.
#'
#' @note
#' This will only work reliably
#' \emph{if the simList was not modified yet during the event which caused the error}.
#' The \code{simList} will be in the state it was at the time of the error.
#'
#' @param sim A simList. If not supplied (the default), this will take the sim from
#'    \code{SpaDES.core:::.pkgEnv$.sim}, i.e., the one that was interrupted
#' @param module A character string length one naming the module that caused the error and
#'   whose source code was fixed. This module will be reparsed and placed into the simList
#' @param restart Logical. If \code{TRUE}, then the call to \code{spades} will be made, i.e.,
#'   restarting the simulation. If \code{FALSE}, then it will return a new \code{simList}
#'   with the module code parsed into the \code{simList}
#' @param numEvents Numeric. Default is Inf (i.e., all available). In the simList, if
#'   \code{options('spades.recoverMode')} is set to \code{TRUE} or a numeric, then
#'   there will be a list in the \code{simList} called .recoverableObjs. These will be
#'   replayed backwards in time to reproduce the initial state of the simList before
#'   the event that is \code{numEvents} back from the first event in \code{events(sim)}.
#' @param ... Passed to \code{spades}, e.g., \code{debug}, \code{.plotInitialTime}
#' @export
#' @return
#' A simList as if \code{spades} had been called on a simList.
#'
#' @examples
#' \dontrun{
#' s <- simInit()
#' s <- spades(s, debug = 1)
#' s <- restartSpades(s, debug = 1)
#' }
restartSpades <- function(sim = NULL, module = NULL, numEvents = Inf,
                          restart = TRUE, ...) {
  message("This is very experimental and will only work if the event that caused the error has not yet changed the simList.\n",
          "This should be used with caution")

  if (is.null(sim)) {
    sim <- .pkgEnv$.sim
  }

  if (is.null(module)) {
    # Source the file you changed, into the correct location in the simList
    module <- events(sim)[["moduleName"]][1]
  }

  if (exists("aaa")) browser()
  # move "completed" back into event queue
  numMods <- min(length(sim$.recoverableObjs), numEvents)
  simCompletedList <- as.list(sim@completed)
  simCompletedList <- simCompletedList[order(as.integer(names(simCompletedList)))]
  eventsToReverse <- tail(simCompletedList, numMods - 1)

  sim@events <- append(unname(lapply(eventsToReverse, function(x) x[1:4])), sim@events)
  rm(list = names(eventsToReverse), envir = sim@completed)

  eventsToReplayDT <- events(sim)[seq_len(numMods)]
  if (numEvents > length(sim$.recoverableObjs))
    message("Cannot replay ", numEvents, " events as requested by numEvents; ",
            "there are only ", length(sim$.recoverableObjs),
            " that can be recovered.")
  if (numMods < length(sim$.recoverableObjs))
    sim$.recoverableObjs <- sim$.recoverableObjs[seq_len(numMods)]
  names(sim$.recoverableObjs) <- eventsToReplayDT$moduleName

  modules <- eventsToReplayDT$moduleName
  modules <- unique(modules)
  names(modules) <- modules
  modules <- modules[!modules %in% unlist(.coreModules())]
  # move objects back in place
  eventIndices <- seq_len(NROW(eventsToReplayDT))
  eventIndicesRev <- rev(eventIndices)
  out <- lapply(eventIndices, function(event) {
    objNames <- names(sim$.recoverableObjs[[event]])
    if (!is.null(objNames)) {
      # only take objects that changed -- determine which ones are changed
      fd1 <- unlist(lapply(sim$.recoverableObjs[[event]], function(obj) fastdigest(obj)))
      objNames <- objNames[objNames %in% ls(sim@.xData)]
      fd2 <- unlist(lapply(mget(objNames, envir = sim@.xData), function(obj) fastdigest(obj)))
      fd1 <- fd1[match(names(fd2), names(fd2))]
      stopifnot(all.equal(names(fd1), names(fd2)))
      fd1 <- fd1[fd1 != fd2]
      list2env(sim$.recoverableObjs[[event]][names(fd1)], envir = sim@.xData)
    }
    message(crayon::blue("Reversing", paste(collapse = ", ",
                                            paste(names(eventsToReplayDT),
                                                  unname(eventsToReplayDT[eventIndicesRev[event]])))))
    invisible()
  })

  # modules <- if (!is.list(module)) as.list(module) else module
  opt <- options("spades.moduleCodeChecks" = FALSE)

  out <- lapply(modules, function(module) {
    pp <- list()
    moduleFolder <- file.path(modulePath(sim), module)
    if (file.exists(file.path(moduleFolder, paste0(module, ".R")))) {
      pp[[1]] <- parse(file.path(moduleFolder, paste0(module, ".R")))
      subFiles <- dir(file.path(moduleFolder, "R"), full.names = TRUE)
      if (length(subFiles)) {
        pp[seq_len(length(subFiles)) + 1] <- lapply(subFiles, function(ff) parse(ff))
      }
      #ee <- new.env()
      #ee$sim <- sim
      # sim@.xData[[module]]$sim <- sim
      lapply(pp, function(pp1) evalWithActiveCode(pp1,
                                                  sim@.xData[[module]],
                                                  sim = sim))
      message(crayon::blue("Reparsing", module, "source code"))
    }
    #rm(list = "sim", envir = ee)
    #list2env(as.list(ee, all.names = TRUE), envir = sim@.xData[[module]])
    invisible()
  })
  options(opt)

  # reset activeBinding mod
  out <- lapply(modules, function(mod) {
    makeModActiveBinding(sim = sim, mod = mod)
  })

  # Remove all added events that occurred during the events, i.e., via scheduleEvent
  sim@events <- setdiff(sim@events, unlist(sim$.addedEvents[seq_len(numMods)], recursive = FALSE))

  if (restart)
    sim <- spades(sim, ...)
  return(sim)
}
