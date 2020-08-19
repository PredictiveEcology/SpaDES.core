#' @importFrom stats na.omit
doEvent.progress <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ids <- na.omit(match(names(P(sim, ".progress")), c("type", "interval")))
    if (interactive()) {
      tu <- sim@simtimes[["timeunit"]]
      defaults <- list(type = "text", interval = (end(sim, tu) - start(sim, tu)) / (end(sim, tu) - start(sim, tu)))

      # Check whether a .progress is specified in the simList
      if ( is.null(P(sim, ".progress")$type) &&
             is.null(P(sim, ".progress")$interval) ) {
        sim@params[[".progress"]] <- defaults
      } else {
        sim@params[[".progress"]][names(defaults)[-ids]] <- defaults[-ids]
      }
    } else {
      # don't use progress bar when non-interactive (this is already set during simInit)
      #  NOTE: this has been sorted in simInit because of updateList -- use sorted order here
      sim@params[[".progress"]] <- .pkgEnv$.progressEmpty[ids]
    }

    # if NA then don't use progress bar
    if (any(!is.na(P(sim, ".progress")))) {
      newProgressBar(sim)
      sim <- scheduleEvent(sim, start(sim, tu), "progress", "set", .last())
      sim <- scheduleEvent(sim, end(sim, tu), "progress", "set", .last())
    }
  } else if (eventType == "set") {
      # update progress bar
      setProgressBar(sim)

      # schedule the next save
      timeNextUpdate <- time(sim, sim@simtimes[["timeunit"]]) + P(sim, ".progress")$interval

      sim <- scheduleEvent(sim, timeNextUpdate, "progress", "set", .last())
  } else {
    warning(paste(
      "Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""
    ))
  }
  return(invisible(sim))
}

################################################################################
#' Progress bar
#'
#' Shows a progress bar that is scaled to simulation end time.
#'
#' The progress bar object is stored in a separate environment, #' \code{.pkgEnv}.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @include environment.R
#' @rdname progressBar
#'
newProgressBar <- function(sim) {
  if (exists(".pb", envir = .pkgEnv)) {
    close(get(".pb", envir = .pkgEnv))
  }
  tu <- sim@simtimes[["timeunit"]]
  OS <- tolower(Sys.info()["sysname"])
  wantGraphical <- isTRUE(P(sim, ".progress")$type == "graphical")
  if (!requireNamespace("tcltk")) {
    if (wantGraphical && (OS != "windows")) {
      warning("Can't use graphical progress bar without tcltk package: ",
              "install.packages('tcltk')\n",
              "Using text progress bar")
      wantGraphical <- FALSE
    }

  }
  if (wantGraphical) {
    if (OS == "windows") {
      pb <- utils::winProgressBar(min = start(sim, tu), max = end(sim, tu),
                           initial = start(sim, tu))
    } else {
      pb <- tcltk::tkProgressBar(min = start(sim, tu), max = end(sim, tu),
                          initial = start(sim, tu))
    }
  } else if (P(sim, ".progress")$type == "shiny") {
    ## see https://shiny.rstudio.com/articles/progress.html
    stop("shiny progress bar not yet implemented")
  } else  if (P(sim, ".progress")$type == "text") {
    pb <- utils::txtProgressBar(min = start(sim, tu), max = end(sim, tu),
                         initial = start(sim, tu), char = ".", style = 3)
  }
  assign(".pb", pb, envir = .pkgEnv)
}

#' @importFrom utils globalVariables
#' @rdname progressBar
setProgressBar <- function(sim) {
  OS <- tolower(Sys.info()["sysname"])
  tu <- sim@simtimes[["timeunit"]]
  pb <- get(".pb", envir = .pkgEnv)
  wantGraphical <- isTRUE(P(sim, ".progress")$type == "graphical")
  if (!requireNamespace("tcltk")) {
    if (wantGraphical && (OS != "windows")) {
      warning("Can't use graphical progress bar without tcltk package: ",
              "install.packages('tcltk')\n",
              "Using text progress bar")
      wantGraphical <- FALSE
    }

  }
  if (wantGraphical) {
    if (OS == "windows") {
      utils::setWinProgressBar(
        pb, time(sim, tu),
        title = paste("Current simulation time:", tu, round(time(sim, tu), 3),
                      "of total", end(sim, tu))
      )
    } else {
      tcltk::setTkProgressBar(pb, time(sim, tu),
                       title = paste("Current simulation time:", tu,
                                     round(time(sim, tu), 3),
                                     "of total", end(sim, tu)))
    }
  } else if (P(sim, ".progress")$type == "shiny") {
    ## see https://shiny.rstudio.com/articles/progress.html
    stop("shiny progress bar not yet implemented")
  } else if (P(sim, ".progress")$type == "text") {
    utils::setTxtProgressBar(pb, round(time(sim, tu), 3))
  }
  assign(".pb", pb, envir = .pkgEnv)
}
