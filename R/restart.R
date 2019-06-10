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
restartSpades <- function(sim = NULL, module = NULL, restart = TRUE, ...) {
  message("This is very experimental and will only work if the event that caused the error has not yet changed the simList.\n",
          "This should be used with caution")

  if (is.null(sim)) {
    sim <- .pkgEnv$.sim
  }

  if (is.null(module)) {
    # Source the file you changed, into the correct location in the simList
    module <- events(sim)[["moduleName"]][1]
  }

  pp <- list()
  moduleFolder <- file.path(modulePath(sim), module)
  pp[[1]] <- parse(file.path(moduleFolder, paste0(module, ".R")))
  subFiles <- dir(file.path(moduleFolder, "R"), full.names = TRUE)
  if (length(subFiles)) {
    pp[seq(subFiles + 1)] <- lapply(subFiles, function(ff) parse(ff))
  }
  ee <- new.env()
  ee$sim <- sim
  lapply(pp, function(pp1) eval(pp1, envir = ee))
  rm(list = "sim", envir = ee)
  list2env(as.list(ee, all.names = TRUE), envir = sim[[module]])

  if (restart)
    sim <- spades(sim, ...)
  return(sim)
}
