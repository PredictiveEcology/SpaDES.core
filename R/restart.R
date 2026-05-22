utils::globalVariables(c(
  ".", ".attachedPkgsFilename", "et", ".First", ".oldWd",
  ".spadesCall", ".spades.restartRInterval", ".spades.simFilename"
))

doEvent.restartR <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (is.null(P(sim)$.restartRInterval))
      params(sim)$restartR$.restartRInterval <- getOption("spades.restartRInterval")
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + P(sim)$.restartRInterval,
                         "restartR", "restartR", .last())

  } else if (eventType == "restartR") {
    nextTime <- time(sim, timeunit(sim)) + P(sim)$.restartRInterval

    # This next step of creating this list is critical -- it is the trigger for on.exit in spades
    sim$._restartRList <- list()
    sim$._restartRList$endOrig <- end(sim)

    if (nextTime < end(sim, timeunit(sim))) {
      sim <- scheduleEvent(sim, nextTime, "restartR", "restartR", .last() + 10) # very last
    }

    # This triggers the end of the spades call
    end(sim) <- time(sim)
  }

  return(invisible(sim))
}

#' Restart an interrupted simulation
#'
#' **This is experimental and has not been thoroughly tested. Use with caution.**
#' If there is an error during an event, this function will rewind the simulation to a state
#' `numEvents` prior to the event that led to the error. The developer may then modify the
#' source code of the module that caused the break and resume the simulation.
#'
#' @details
#' If `options('spades.recoveryMode')` is set to `TRUE` or a numeric (default 1), then
#' there will be a list in the `simList` called `.recoverableObjs`.
#' These record the elements of simList that have  changed over a number of events equal
#' to the number chosen for `options('spades.recoveryMode')`.
#' The `restartSpades` function then uses this list to rewind `numEvents` backwards from the
#' first event in `events(sim)` (likely the one that caused the error).
#'
#' The random number seed will be reset to the state it was at the start of the
#' earliest event recovered, thereby returning to the exact stochastic simulation trajectory.
#'
#' @note The `simList` will be in the state it was `numEvents` prior to the event
#' that led to the error (although some objects, e.g., on disk, may have already been modified).
#'
#' @param sim A `simList` or a filename that will load a `simList`, e.g., from
#'    `saveState` or `saveSimList`. If not supplied (the default),
#'    this will take the `sim` from
#'    `savedSimEnv()$.sim`, i.e., the one that was interrupted
#'
#' @param module A character string length one naming the module that caused the error and
#'   whose source code was fixed. This module will be re-parsed and placed into the `simList`
#'
#' @param restart Logical. If `TRUE`, then the call to `spades` will be made, i.e.,
#'   restarting the simulation. If `FALSE`, then it will return a new `simList`
#'   with the module code parsed into the `simList`
#'
#' @param numEvents Numeric. Default is `1L` (rewind a single event). Use `Inf`
#'   to rewind all available events.
#'   The number of events to be rewound.
#'   In the `simList`, if `options('spades.recoveryMode')` is set to `TRUE` or a numeric,
#'   then there will be a list in the `simList` called `.recoverableObjs`.
#'   These will be replayed backwards in time to reproduce the initial state of the `simList`
#'   before the event that is `numEvents` prior to the first event in `events(sim)`.
#'
#' @param ... Passed to `spades`, e.g., `debug`, `.plotInitialTime`. If the
#'   interrupted `spades()` call used an `events` filter (to run only certain
#'   events), the same filter is reused automatically on restart; pass a new
#'   `events` argument here to override it.
#'
#' @return A `simList` as if `spades` had been called on a `simList`.
#'
#' @export
#' @importFrom reproducible Cache
#' @importFrom cli col_blue
#' @inheritParams paramCheckOtherMods
#'
#' @examples
#' \donttest{
#' # options("spades.recoveryMode" = 1) # now the default
#' s <- simInit()
#' s <- spades(s) # if this is interrupted or fails
#' ## the following line will not work if the previous line didn't fail:
#'
#' ## don't need to specify `sim` if previous line fails;
#' ## will take from savedSimEnv()$.sim automatically
#' s <- restartSpades(s)
#'
#' }
restartSpades <- function(sim = NULL, module = NULL, numEvents = 1L, restart = TRUE,
                          verbose = getOption("reproducible.verbose", 1L), ...) {
  message("This is experimental and should be used with caution.")

  # browser(expr = exists("._restartSpades_1"))
  if (is.null(sim)) {
    sim <- savedSimEnv()$.sim
    messageVerbose("sim not supplied, using \n",
                   "sim <- savedSimEnv()$.sim", verbose = verbose)
  }
  if (is.character(sim)) {
    sim <- SpaDES.core::loadSimList(sim)
  }

  if (!is(sim, "simList"))
    stop("The simList does not exist or is corrupt; please pass a simList")

  ## A simList interrupted during simInit()'s .inputObjects (rather than during a
  ##   spades() event) carries a saved simInit context. Let a user "lazily" call
  ##   restartSpades() in that case -- delegate to restartSimInit().
  if (!is.null(sim@.xData[["._simInitContext"]])) {
    message("This simList was interrupted during simInit()'s .inputObjects, not during ",
            "a spades() event; delegating to restartSimInit().")
    return(restartSimInit(sim = sim, module = module, numEvents = numEvents,
                          restart = restart, verbose = verbose, ...))
  }

  if (is.null(module)) {
    # Source the file you changed, into the correct location in the simList
    module <- events(sim)[["moduleName"]][1]
  }

  # move "completed" back into event queue
  numMods <- min(length(sim$.recoverableObjs), numEvents)
  
  if (numMods == 0) {
    message("There no saved state prior to any changes that happened in ",
            module, ". Would you like to proceed from the last state of the ",
            "simList anyway? i.e., any changes that had already happened ",
            "inside the module: ", module, " before the fail will be kept...")
    continue <- readline("Would you like to restart anyway? (y or n) ")
    continue <- tolower(substr(continue, start = 1, stop = 1))
    if (continue %in% "y")
      numMods <- 1
  }
  if (numMods > 0) {
    com <- completed(sim)
    etSecs <- sum(com[, et := difftime(get(._txtClockTime), get(._txtPrevEventTimeFinish), units = "secs"),
                      by = seq_len(NROW(com))]$et)

    ## remove the times of the completed events - 1 because the restartSpaDES includes the incompleted event
    # et <- difftime(tail(com$._clockTime, numMods - 1)[1], com$._clockTime[1])
    st <- Sys.time()
    sim[[._txtStartClockTime]] <- st - etSecs

    simCompletedList <- as.list(sim@completed)
    simCompletedList <- simCompletedList[order(as.integer(names(simCompletedList)))]
    eventsToReverse <- tail(simCompletedList, numMods - 1)

    sim@events <- append(unname(lapply(eventsToReverse, function(x) x[1:4])), sim@events)
    rm(list = names(eventsToReverse), envir = sim@completed)

    last <- as.character(length(sim@completed))
    sim@completed[[last]][[._txtClockTime]] <- st

    eventsToReplayDT <- events(sim)[seq_len(numMods)]
    if (numMods < length(sim$.recoverableObjs))
      sim$.recoverableObjs <- sim$.recoverableObjs[seq_len(numMods)]
    eventIndices <- seq_len(NROW(eventsToReplayDT))
    eventIndicesRev <- rev(eventIndices)
    # names(sim$.recoverableObjs) <- eventsToReplayDT$moduleName[eventIndicesRev]

    modules <- eventsToReplayDT$moduleName[eventIndicesRev]
    # modules <- modules
    names(modules) <- modules
    modules <- modules[!modules %in% unlist(.coreModules())]
    ## move objects back in place
    # browser(expr = exists("._restartSpades_2"))
    out <- lapply(eventIndices, function(event) {
      objNames <- names(sim$.recoverableObjs[[event]])
      notYetCreated <- setdiff(outputObjects(sim)[[modules[event]]]$objectName, objNames)
      names(notYetCreated) <- notYetCreated
      notYetCreatedList <- lapply(notYetCreated, function(x) NULL)

      ## need to overwrite with NULL if the object was not yet created
      sim$.recoverableObjs[[event]] <- append(sim$.recoverableObjs[[event]], notYetCreatedList)
      # sim$.recoverableObjs[[event]]
      objsToCopy <- sim$.recoverableObjs[[event]]

      objNames <- names(objsToCopy)
      # objNames <- setdiff(objNames, notYetCreated)
      if (!is.null(objNames)) {
        ## only take objects that changed -- determine which ones are changed
        whNULLs <- sapply(objsToCopy, is.null)
        objsWONULLSs <- objsToCopy[!whNULLs]
        if (any(whNULLs)) {
          NULLed <- names(whNULLs)[whNULLs]
          keeps <- names(whNULLs)[!whNULLs]
          a <- suppressWarnings(rm(list = NULLed, envir = sim@.xData))
          objsToCopy <- objsToCopy[keeps]
        }

        # Turns out it is much slower in most cases to digest, then "only copy changed"
        if (FALSE) {
          # fd1 is the current state of the objects in the recoveryObjs, i.e., the ones to get
          # fd2 is the state of the objects in the sim
          # Don't use `digest::digest` (of course) because it fails on SpatRaster, for example
          # fd1 <- lapply(objsToCopy, function(obj) obj2 <- digest::digest(obj, algo = "xxhash64"))
          fd1 <- lapply(objsToCopy, function(obj) .robustDigest(obj))
          # the dots in the sim include e.g., .recoverableObjs now; so no ls(all.names = TRUE)
          objNames <- objNames[objNames %in% ls(sim@.xData)]
          fd2 <- lapply(mget(objNames, envir = sim@.xData), function(obj) .robustDigest(obj))
          # fd2 <- lapply(mget(objNames, envir = sim@.xData), function(obj) obj2 <- digest::digest(obj, algo = "xxhash64"))
          if (!is.null(fd2)) {
            changed <- setdiffNamed(fd1, fd2)
            fd1 <- changed
          }
          ## move the changed ones to the simList
          if (NROW(fd1)) {
            # list2env(objsToCopy[names(fd1)], envir = sim@.xData)
            list2env(Copy(objsToCopy[names(fd1)]), envir = sim@.xData)
          }
        }
        if (NROW(objsToCopy)) {
          message(cli::col_blue("Setting all changed objects to their values at the start of ", modules[event]))
          list2env(Copy(objsToCopy), envir = sim@.xData)
        } else {
          message(cli::col_blue("no objects to reset/recover in ", modules[event], ":",
                                rev(tail(completed(sim), max(eventIndices))$eventType)[event]))
        }


      }

      if (length(sim$.recoverableModObjs)) {
        if (FALSE) {
          modObjNames <- names(sim$.recoverableModObjs[[event]])
          modObjsToCopy <- sim$.recoverableModObjs[[event]]
          modObjEnv <- sim[[dotObjs]][[modules[event]]] # $.objects
          modObjLs <- ls(modObjEnv)
          fd1 <- lapply(modObjsToCopy, function(obj) .robustDigest(obj))
          modObjNames <- modObjNames[modObjNames %in% modObjLs]
          fd2 <- lapply(mget(modObjNames, envir = modObjEnv), function(obj) .robustDigest(obj))
          if (!is.null(fd2)) {
            changed <- setdiffNamed(fd1, fd2)
            fd1 <- changed
          }

          if (NROW(fd1)) {
            # list2env(objsToCopy[names(fd1)], envir = sim@.xData)
            list2env(Copy(modObjsToCopy[names(fd1)]), envir = modObjEnv)
          }
        }
        modObjEnv <- sim[[dotObjs]][[modules[event]]] # $.objects
        modObjsToCopy <- sim$.recoverableModObjs[[event]]
        list2env(Copy(modObjsToCopy), envir = modObjEnv)

      }

      message(cli::col_blue("Reversing event: ",
                            paste(collapse = " ",
                                  paste(unname(eventsToReplayDT[eventIndicesRev[event]])))))
      invisible()
    })

    # modules <- if (!is.list(module)) as.list(module) else module

    ## reset activeBinding mod
    out <- lapply(modules, function(mod) {
      makeModActiveBinding(sim = sim, mod = mod)
    })
    out <- lapply(modules, function(mod) {
      makeParActiveBinding(sim = sim, mod = mod)
    })

    ## Remove all added events that occurred during the events, i.e., via scheduleEvent
    sim@events <- setdiff(sim@events, unlist(sim$.addedEvents[seq_len(numMods)], recursive = FALSE))
    sim@current <- list()
    assign(".Random.seed", sim@.xData$._randomSeed[[numMods]], envir = .GlobalEnv)
  } else {
    modules <- modules(sim)
  }

  .reparseModules(sim, modules)

  ## Once reversed, remove the .recoverableObjs
  sim$.recoverableObjs <- NULL

  if (restart) {
    # All packages are guaranteed already loaded in this session; skip the
    # Require() call in loadPkgs which can hang non-interruptibly (dyn.load on
    # an NFS-backed .so) when packages are touched again unnecessarily.
    opts <- options(spades.loadReqdPkgs = FALSE)
    on.exit(options(opts), add = TRUE)
    ## reuse the `events` filter from the interrupted spades call (issue #354),
    ## unless the user supplied a new one to restartSpades(...)
    dots <- list(...)
    if (!"events" %in% ...names() && !is.null(sim@.xData[["._spadesEvents"]]))
      dots$events <- sim@.xData[["._spadesEvents"]]
    sim <- do.call(spades, append(list(sim), dots))
  }
  # } else {
  #   message("There was no interrupted spades call; returning sim as is")
  # }
  return(sim)
}

#' Re-parse (fixed) module source into a `simList`
#'
#' Shared by [restartSpades()] and [restartSimInit()]: re-`parse()`s the named modules'
#' source files and evaluates them into the `simList`'s module environments (picking up
#' any fixes the developer made). Operates by side effect on `sim@.xData` (an
#' environment), so the input `sim` is mutated in place.
#'
#' @param sim A `simList`.
#' @param modules Character vector of (non-core) module names to re-parse.
#' @return `sim`, invisibly.
#' @keywords internal
#' @importFrom cli col_blue
.reparseModules <- function(sim, modules) {
  names(modules) <- modules
  opt <- options("spades.moduleCodeChecks" = FALSE)
  on.exit(options(opt), add = TRUE)
  lapply(modules, function(module) {
    pp <- list()
    moduleFolder <- file.path(modulePath(sim, module = module), module)
    if (file.exists(file.path(moduleFolder, paste0(module, ".R")))) {
      pp[[1]] <- parse(file.path(moduleFolder, paste0(module, ".R")))
      subFiles <- dir(file.path(moduleFolder, "R"), full.names = TRUE)

      doesntUseNamespacing <- !.isNamespaced(sim, module)

      ## evaluate the rest of the parsed file
      sim <- currentModuleTemporary(sim, module)
      pkgs = slot(slot(depends(sim), "dependencies")[[module]], "reqdPkgs")
      if (doesntUseNamespacing) {
        evalWithActiveCode(pp[[1]], sim@.xData, sim = sim, pkgs = pkgs)
      }

      if (length(subFiles)) {
        pp[seq_len(length(subFiles)) + 1] <- lapply(subFiles, function(ff) parse(ff))
      }
      lapply(pp, function(pp1)
        evalWithActiveCode(pp1, sim@.xData[[dotMods]][[module]], sim = sim, pkgs = pkgs))
      message(cli::col_blue("Reparsing ", module, " source code"))
    }
    invisible()
  })
  invisible(sim)
}

#' @export
#' @rdname restartSpades
#' @param filename The filename to save the sim state.
#'
#' `saveState` is a wrapper around `restartSpades` and `saveSimList`. You can
#' pass arguments to the `...` that will be passed to `saveSimList`, such as
#' `modules`, `inputs`, `outputs`.
saveState <- function(filename, ...){
  sim <- restartSpades(restart = FALSE)
  saveSimList(sim, filename, ...)
  message("Saved! ", filename)
}

#' Restart an interrupted `simInit`
#'
#' **This is experimental and has not been thoroughly tested. Use with caution.**
#' This is the `simInit`/`.inputObjects` analogue of [restartSpades()]. If a module's
#' `.inputObjects` errors during [simInit()] and `options('spades.recoveryMode')` was
#' set to `TRUE` or a numeric (the default, `1`), the interrupted `simList` is saved to
#' `savedSimEnv()$.sim` with a list `.recoverableObjs` recording the state of each
#' module's input objects at the start of its `.inputObjects`. `restartSimInit()` rewinds
#' the `simList` to the start of the interrupted `.inputObjects` (i.e., `numEvents` prior),
#' re-parses the (presumably fixed) module source, then resumes `simInit` by running the
#' remaining modules' `.inputObjects` and completing the initialization.
#'
#' @details
#' The random number seed is reset to its state at the start of the earliest recovered
#' `.inputObjects`, so that any stochastic defaults are reproduced exactly.
#'
#' @inheritParams restartSpades
#' @param module A character string naming the module whose `.inputObjects` caused the
#'   error and whose source code was fixed. This module will be re-parsed into the
#'   `simList`. If `NULL` (default), it is taken from the most recent recovery snapshot.
#' @param numEvents Numeric. Default `1L` (rewind only the interrupted module's
#'   `.inputObjects`). Use `Inf` to rewind all recoverable `.inputObjects`.
#'
#' @return A `simList` as if [simInit()] had completed (when `restart = TRUE`), or the
#'   rewound `simList` with the fixed module re-parsed (when `restart = FALSE`).
#'
#' @export
#' @rdname restartSpades
#' @importFrom reproducible Copy
#' @importFrom cli col_blue
restartSimInit <- function(sim = NULL, module = NULL, numEvents = 1L, restart = TRUE,
                           verbose = getOption("reproducible.verbose", 1L), ...) {
  message("This is experimental and should be used with caution.")

  if (is.null(sim)) {
    sim <- savedSimEnv()$.sim
    messageVerbose("sim not supplied, using \n",
                   "sim <- savedSimEnv()$.sim", verbose = verbose)
  }
  if (is.character(sim)) {
    sim <- SpaDES.core::loadSimList(sim)
  }
  if (!is(sim, "simList"))
    stop("The simList does not exist or is corrupt; please pass a simList")

  ctx <- sim@.xData[["._simInitContext"]]
  if (is.null(ctx))
    stop("This simList has no saved simInit context to restart from. ",
         "restartSimInit() only works after a simInit() call that was interrupted ",
         "during a module's .inputObjects, with options('spades.recoveryMode') > 0.")

  ## recoverModePre names each snapshot by its module, most-recent first, so the first
  ##   element corresponds to the module whose .inputObjects was interrupted.
  recoverNames <- names(sim$.recoverableObjs)
  if (is.null(module)) {
    module <- if (length(recoverNames)) recoverNames[[1]] else sim@current[["moduleName"]]
  }

  ## ---- rewind the simList objects to the start of the interrupted .inputObjects ----
  numMods <- min(length(sim$.recoverableObjs), numEvents)
  modulesRewound <- character()
  if (numMods > 0) {
    modulesRewound <- recoverNames[seq_len(numMods)]
    if (numMods < length(sim$.recoverableObjs)) {
      sim$.recoverableObjs <- sim$.recoverableObjs[seq_len(numMods)]
      if (length(sim$.recoverableModObjs))
        sim$.recoverableModObjs <- sim$.recoverableModObjs[seq_len(numMods)]
    }
    out <- lapply(seq_len(numMods), function(event) {
      mod <- modulesRewound[event]
      objsToCopy <- sim$.recoverableObjs[[event]]
      objNames <- names(objsToCopy)
      if (!is.null(objNames)) {
        ## objects captured as NULL did not exist at the start of .inputObjects; remove them
        whNULLs <- vapply(objsToCopy, is.null, logical(1))
        if (any(whNULLs)) {
          suppressWarnings(rm(list = objNames[whNULLs], envir = sim@.xData))
          objsToCopy <- objsToCopy[!whNULLs]
        }
        if (NROW(objsToCopy)) {
          message(cli::col_blue("Setting all changed objects to their values at the start of ",
                                mod, "'s .inputObjects"))
          list2env(Copy(objsToCopy), envir = sim@.xData)
        }
      }
      if (length(sim$.recoverableModObjs) >= event) {
        modObjEnv <- sim[[dotObjs]][[mod]]
        modObjsToCopy <- sim$.recoverableModObjs[[event]]
        if (!is.null(modObjEnv) && !is.null(modObjsToCopy) && NROW(modObjsToCopy))
          list2env(Copy(modObjsToCopy), envir = modObjEnv)
      }
      invisible()
    })
    ## reset the random seed to the start of the earliest recovered .inputObjects
    if (!is.null(sim@.xData[["._randomSeed"]]))
      assign(".Random.seed", sim@.xData[["._randomSeed"]][[numMods]], envir = .GlobalEnv)
  }

  ## ---- reparse the (fixed) module source(s) -- same approach as restartSpades ----
  modulesToReparse <- setdiff(unique(c(module, modulesRewound)), unlist(.coreModules()))
  .reparseModules(sim, modulesToReparse)

  sim$.recoverableObjs <- NULL
  sim$.recoverableModObjs <- NULL

  if (!isTRUE(restart))
    return(sim)

  ## restore the simInit-time session state that simInit's on.exit cleared, so the
  ##   resumed .inputObjects run in the same environment as the original simInit:
  ##   the options('spades.*Path') (from sim@paths) and the timing bookkeeping that
  ##   debugMessage() relies on.
  oldGetPaths <- getPaths()
  do.call(setPaths, append(sim@paths, list(silent = TRUE)))
  on.exit(do.call(setPaths, append(list(silent = TRUE), oldGetPaths)), add = TRUE)
  sim@.xData[["._startClockTime"]] <- Sys.time()
  sim$._simInitElapsedTime <- 0

  ## ---- resume: re-run the remaining .inputObjects, then finish simInit ----
  completedDT <- completed(sim)
  succeeded <- if (NROW(completedDT))
    completedDT[completedDT[["eventType"]] == ".inputObjects", ][["moduleName"]] else character()

  ## rewound modules that had previously completed (numEvents > 1): drop their stale
  ##   completed .inputObjects entry and their scheduled init event so they re-run cleanly
  rewoundCompleted <- intersect(modulesRewound, succeeded)
  if (length(rewoundCompleted)) {
    for (k in ls(sim@completed)) {
      ev <- get(k, envir = sim@completed)
      if (identical(ev[["eventType"]], ".inputObjects") &&
          ev[["moduleName"]] %in% rewoundCompleted)
        rm(list = k, envir = sim@completed)
    }
    sim@events <- Filter(
      function(e) !(identical(e[["eventType"]], "init") && e[["moduleName"]] %in% rewoundCompleted),
      sim@events)
  }
  ## modules still needing a (re)run, in load order
  succeeded <- setdiff(succeeded, modulesRewound)
  modulesToRun <- ctx$loadOrder[!ctx$loadOrder %in% succeeded]

  debug <- ctx$debug
  ## Pass A' -- re-run .inputObjects only for modules that have not completed it (the
  ##   interrupted module + any not-yet-run, plus any rewound ones); succeeded modules
  ##   keep their results.
  for (m in modulesToRun) {
    if (isTRUE(getOption("spades.dotInputObjects", TRUE)))
      sim <- .runInputObjects(sim, m, ctx$objects, ctx$notOlderThan, debug = debug)
  }

  ## Pass B' -- schedule each module's init + fill dotParams, mirroring simInit()'s Pass B.
  ##   simInit schedules init only *after* the whole .inputObjects phase, so an interrupted
  ##   simInit left no user init events; (re)schedule them here for every module that still
  ##   needs one (i.e., whose init did not already run via allowInitDuringSimInit), skipping
  ##   any that somehow already have one queued.
  needIO <- !(ctx$loadOrder %in% sim@.xData$._ranInitDuringSimInit)
  alreadyScheduledInit <- vapply(sim@events, function(e)
    if (identical(e[["eventType"]], "init")) e[["moduleName"]] else NA_character_, character(1))
  for (idx in seq_along(ctx$loadOrder)) {
    m <- ctx$loadOrder[idx]
    if (needIO[idx] && !(m %in% alreadyScheduledInit))
      sim <- scheduleEvent(sim, sim@simtimes[["start"]], m, "init", .first())
    sim <- .fillDotParams(sim, m, ctx$dotParamsReal)
  }

  ## reconstruct the loaded-modules list the same shape simInit builds (core modules +
  ##   user modules, named by full path), since the interrupted run never finished it
  modulesLoaded <- as.list(c(ctx$core, ctx$loadOrderBase))
  names(modulesLoaded) <- c(ctx$core, ctx$loadOrderNames)
  sim <- .finishSimInit(sim, ctx, modulesLoaded)

  ## this is now a complete simList; drop the recovery bookkeeping
  sim@.xData[["._simInitContext"]] <- NULL
  return(sim)
}

#' Finish initialization of a `simList` after `.inputObjects` have run
#'
#' The post-loop finalization shared by [simInit()] and [restartSimInit()]'s resume
#' path: stores the loaded modules, applies user-supplied `objects`, `inputs`, and
#' `outputs`, and checks parameters. Keeping it in one place ensures the normal and
#' resumed paths cannot drift apart.
#'
#' @param sim A `simList`.
#' @param ctx The context list stored at `sim@.xData[["._simInitContext"]]` during
#'   `simInit` (supplies `core`, `loadOrderBase`, `objects`, `objNames`, `inputs`,
#'   `outputs`, `verbose`, `dotParams`, and `parentChildGraph`).
#' @param modulesLoaded The named list of loaded modules (core + user), as assembled by
#'   the caller.
#' @return A finished `simList`.
#' @keywords internal
.finishSimInit <- function(sim, ctx, modulesLoaded) {
  ## check that modules all loaded correctly and store result
  if (all(append(ctx$core, ctx$loadOrderBase) %in% basename2(unlist(modulesLoaded)))) {
    modules(sim) <- modulesLoaded
  } else {
    stop("There was a problem loading some modules.")
  }
  attr(sim@modules, "modulesGraph") <- ctx$parentChildGraph

  objects <- ctx$objects
  objNames <- ctx$objNames
  inputs <- ctx$inputs
  outputs <- ctx$outputs
  verbose <- ctx$verbose

  ## END OF MODULE PARSING AND LOADING
  if (length(objects)) {
    if (is.list(objects)) {
      if (length(objNames) == length(objects)) {
        if (isTRUE(getOption("spades.allowInitDuringSimInit") &&
                   getOption("spades.dotInputObjects", TRUE))) {
          inputObjectsAllMods <- inputObjects(sim)
          if (is(inputObjectsAllMods, "list"))
            inputObjectsAllMods <- inputObjectsAllMods |> rbindlist()
          inputObjectsAllMods <- unique(inputObjectsAllMods$objectName)
          objectNamesToUse <- inputObjectsAllMods[inputObjectsAllMods %in% sim$.userSuppliedObjNames]
          objectsToUse <- objects[objectNamesToUse]
          objectsToUse <- objectsToUseUpdatesFromPrevInits(sim, objectsToUse)
        } else {
          objectsToUse <- objects
        }
        if (length(objectsToUse) && verbose) {
          messageNewObjects(
            objectsToUse[order(names(objectsToUse))], verbose = verbose,
            prefix = "User-supplied objects passed into sim for spades call:")
        }
        if (NROW(objectsToUse))
          objs(sim) <- objectsToUse
      } else {
        stop(
          paste(
            "objects must be a character vector of object names",
            "to retrieve from the .GlobalEnv, or a named list of",
            "objects"
          )
        )
      }
    } else {
      newInputs <- data.frame(
        objectName = objNames,
        loadTime = as.numeric(sim@simtimes[["current"]]),
        stringsAsFactors = FALSE
      ) |>
        .fillInputRows(startTime = start(sim))
      inputs(sim) <- newInputs
    }
  }

  ## load files in the filelist
  if (NROW(inputs) | NROW(inputs(sim))) {
    inputs(sim) <- rbind(inputs(sim), inputs)
    if (NROW(events(sim)[moduleName == "load" &
                         eventType == "inputs" &
                         eventTime == start(sim)]) > 0) {
      sim <- doEvent.load(sim, sim@simtimes[["current"]], "inputs")
      events(sim) <- events(sim)[!(eventTime == time(sim) &
                                     moduleName == "load" &
                                     eventType == "inputs"), ]
    }
    if (any(events(sim)[["eventTime"]] < start(sim))) {
      warning(
        paste0(
          "One or more objects in the inputs filelist was ",
          "scheduled to load before start(sim). ",
          "It is being be removed and not loaded. To ensure loading, loadTime ",
          "must be start(sim) or later. See examples using ",
          "loadTime in ?simInit"
        )
      )
      events(sim) <- events(sim)[eventTime >= start(sim)]
    }
  }

  if (length(outputs)) {
    outputs(sim) <- outputs
  }

  ## check the parameters supplied by the user
  checkParams(sim, ctx$dotParams, unlist(sim@paths[["modulePath"]]))
  return(sim)
}

#' Restart R programmatically
#'
#' This will attempt to restart the R session, reloading all packages, and
#' saving and reloading the `simList`.
#' Currently, this is not intended for general use: it has many specialized
#' pieces for using inside a `spades` call.
#' The main purpose for doing this is to clear memory leaks (possibly deep
#' in R <https://github.com/r-lib/fastmap>) that are not fully diagnosed.
#' *This is still very experimental*.
#' This should only be used if there are RAM limitations being hit with long running simulations.
#' It has been tested to work Linux within Rstudio and at a terminal R session.
#' The way to initiate restarting of R is simply setting the `spades.restartRInterval` or
#' setting the equivalent parameter in the `restartR` core module via:
#' `simInit(..., params = list(.restartR = list(.restartRInterval = 1)), ...)`
#' greater than 0, which is the default,
#' e.g., `options("spades.restartRInterval" = 100)`.
#' This is only intended to restart a simulation in exactly the same place as it was
#' (i.e., cannot change machines), and because of the restart, the assignment of the `spades`
#' call will be either to `sim` or the user must make such an assignment manually,
#' e.g., `sim <- savedSimEnv()$.sim`, or perhaps, the safer `sim <- Copy(savedSimEnv()$.sim)`.
#' This is stated in a message.
#'
#' @details
#' The process responds to several options. Though under most cases,
#' the default behaviour should suffice. These are of 3 types: `restartRInterval`
#' the arguments to `restartR` and the arguments to `saveSimList`, these latter two
#' using a dot to separate the function name and its argument. The defaults for
#' two key options are: `options("spades.restartR.restartDir" = NULL`, meaning
#' use `file.path(restartDir, "restartR", paste0(sim[[._txtStartClockTime]], "_", .rndString))`
#' and `options("spades.saveSimList.fileBackend" = 0)`, which means don't do anything
#' with raster-backed files.
#' See specific functions for defaults and argument meanings.
#' The only difference from the default function values is with `saveSimList` argument
#' `fileBackend = FALSE` during `restartR` by default, because it is assumed that
#' the file backends will still be intact after a restart, so no need to move them all to memory.
#'
#' @note
#' Because of the restarting, the object name of the original assignment of the
#' `spades` call can not be preserved.
#' The `spades` call will be assigned to `sim` in the `.GlobalEnv`.
#'
#' Because this function is focused on restarting during a `spades` call,
#' it will remove all objects in the `.GlobalEnv`, emulating `q("no")`.
#' If the user wants to keep those objects, then they should be saved to disk
#' immediately before the `spades` call.
#' This can then be recovered immediately after the return from the `spades` call.
#'
#' To keep the saved `simList`, use `options("spades.restartR.clearFiles" = TRUE)`.
#' The default is to treat these files as temporary files and so will be removed.
#'
#' @param sim Required. A `simList` to be retained through the restart.
#'
#' @param reloadPkgs Logical. If `TRUE`, it will attempt to reload all the packages
#'    as they were in previous session, in the same order. If `FALSE`, it will
#'    load no packages beyond normal R startup. Default `TRUE`.
#'
#' @param .First A function to save to \file{~/.qs2} which will
#'    be loaded at restart from \file{~/.qs2} and run.
#'    Default is `NULL`, meaning it will use the non-exported `SpaDES.core:::First`.
#'    If a user wants to make a custom `First` file, it should built off that one.
#'
#' @param .RDataFile A filename for saving the `simList`.
#'     Defaults to `getOption("spades.restartR.filename")`, and the directory will
#'     be in `restartDir`. The simulation time will be mid-pended to this
#'     name, as in: `basename(file), "_time",`
#'     `paddedFloatToChar(time(sim), padL = nchar(as.character(end(sim))))))`.
#'
#' @param restartDir A character string indicating root directory to
#'     save `simList` and other ancillary files during restart.
#'     Defaults to `getOption("spades.restartR.restartDir", NULL)`.
#'     If `NULL`, then it will try, in order, `outputPath(sim)`,
#'     `modulePath(sim)`, `inputPath(sim)`, `cachePath(sim)`,
#'     taking the first one that is not inside the `tempdir()`, which will
#'     disappear during restart of R.
#'     The actual directory for a given `spades` call that is restarting will be:
#'     `file.path(restartDir, "restartR", paste0(sim[[._txtStartClockTime]], "_", .rndString))`.
#'     The random string is to prevent parallel processes that started at the same clock
#'     time from colliding.
#'
#' @return invoked for side effect of restarting the R session
#'
#' @export
#' @importFrom cli bg_blue col_white
#' @importFrom reproducible checkPath
restartR <- function(sim, reloadPkgs = TRUE, .First = NULL,
                     .RDataFile = getOption("spades.restartR.RDataFilename"),
                     restartDir = getOption("spades.restartR.restartDir", NULL)) {
  if (missing(sim)) stop("sim is currently a required argument")
  restartDir <- checkAndSetRestartDir(restartDir, sim = sim)

  .rndString <- sim@.xData[["._simRndString"]]

  vanillaPkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                   "package:grDevices", "package:utils", "package:datasets", "package:methods",
                   "Autoloads", "package:base", "devtools_shims")
  srch <- search()
  attached <- srch
  attached <- grep("package:", attached, value = TRUE)
  attached <- unlist(lapply(attached, function(x) gsub(x, pattern = "package:", replacement = "")))
  .newDir <- file.path(restartDir, "restartR", gsub(":| ", "_", paste0(sim[[._txtStartClockTime]], "_",
                                                                       .rndString))) |>
    checkPath(create = TRUE)
  .attachedPkgsFilename <- file.path(.newDir, '.attachedPkgs.RData')
  save(file = .attachedPkgsFilename, attached)
  if (is.null(.First)) {
    .First <- getFromNamespace("First", "SpaDES.core")
  }

  .oldWd <- getwd()
  setwd(restartDir)

  if (is.null(sim$._restartRList)) sim$._restartRList <- list()
  sim$._restartRList$envvars <- as.list(Sys.getenv())

  o <- options()
  rmBCFullEnvir <- grep("env", names(o))
  o[rmBCFullEnvir] <- NULL
  sim$._restartRList$opts <- o
  if ("raster" %in% attached) {
    invisible(capture.output({
      sim$._restartRList$optsRaster <- raster::rasterOptions()
    }))
    sim$._restartRList$optsRaster$depracatedwarnings <- sim$._restartRList$optsRaster$depwarning
    sim$._restartRList$optsRaster$depwarning <- NULL
  }

  filename <- basename(tempfile())
  sim$._restartRList$simFilename <- file.path(.newDir, paste0(
    filename, "_time",
    paddedFloatToChar(time(sim), padL = nchar(as.character(end(sim))))))

  ## ensure correct file extension
  sim$._restartRList$simFilename <- paste0(sim$._restartRList$simFilename, ".qs2")

  # sim$._restartRList$endOrig <- end(sim)
  sim$._restartRList$startOrig <- start(sim)

  sim$._restartRList$wd <- asPath(getwd())

  withTmpPaths <- grepl(tempdir(), paths(sim))
  if (any(withTmpPaths)) {
    message("Some paths in the simList, ",
            paste(names(paths(sim))[withTmpPaths], collapse = ", "),
            ", are in temporary locations.",
            "These will not persist after restart as these locations disappear.")
  }
  saveSimListFormals <- formals(saveSimList)
  saveSimList(
    sim,
    filename = getOption("spades.saveSimList.filename", sim$._restartRList$simFilename)#,
    # fileBackend = getOption("spades.saveSimList.fileBackend", 0),
    # filebackedDir = getOption("spades.saveSimList.filebackedDir", saveSimListFormals$filebackedDir)
  )

  # from pryr::mem_used
  #if (requireNamespace("pryr", quietly = TRUE)) {
  mu <- sum(gc()[, 1] * c(as.integer(8 * .Machine$sizeof.pointer - .Machine$sizeof.pointer),
                          as.integer(8)))
  class(mu) <- "object_size"
  message(cli::bg_blue(cli::col_white(format(mu, units = "auto"))))
  #}

  .spadesCall <- sim$._restartRList$.spadesCall
  .spades.simFilename <- sim$._restartRList$simFilename
  .spadesCall$sim <- as.name("sim") # user may not have called the object "sim" ... now it is for restarting
  .spades.restartRInterval <- getOption("spades.restartRInterval")
  # save .First function and the .oldWd
  #if (isTRUE(reloadPkgs))
  .reloadPkgs <- reloadPkgs
  file <- file.path(.newDir, ".RData")
  save(file = file, .First, .oldWd, .spadesCall, .spades.restartRInterval, .spades.simFilename,
       .reloadPkgs, .rndString, .attachedPkgsFilename, eval.promises = TRUE)

  if (isTRUE(Sys.getenv("RSTUDIO") == "1")) {
    needInstall("rstudioapi",
                messageStart = "Running RStudio. To restart it this way, you must install: ")

    lapply(setdiff(srch, vanillaPkgs), function(pkg)
      detach(pkg, character.only = TRUE, unload = TRUE, force = TRUE))
    rm(list = ls(all.names = TRUE, envir = .GlobalEnv), envir = .GlobalEnv)

    # Need load to get custom .First fn
    rstudioapi::restartSession(paste0("{browser();
                                      load('", .RDataFile, "'); ",
                                      "sim <- .First(); ",
                                      "sim <- eval(.spadesCall)}"), clean = TRUE)
  } else {
    #reg.finalizer(.GlobalEnv, function(e) system("R --no-save"), TRUE)
    # R cmd line loads .RData first, then .First, if there is one.
    .First <- FirstFromR

    # # if there is an .RData file, keep it -- will be put back later.
    # if (file.exists(file.path("~", ".RData")))
    #   file.link(file.path("~",".RData"), paste0(file.path("~",".RData"), .rndString))
    save(file = ".RData", .First)
    #out <- reg.finalizer(as.environment("package:SpaDES.core"), function(e) system(paste0("R --no-save --args ", .rndString)), TRUE)
    .spades.simFilename <- gsub("\ ", "\\ ", .spades.simFilename, fixed = TRUE)
    # instead of .Last
    out <- reg.finalizer(.GlobalEnv, function(e)
      system(paste0("R --no-save --args ", .rndString, " ", .spades.simFilename)), TRUE)
    q("no")
  }
}


#' `restartOrSimInitAndSpades` is a wrapper that runs either `restartSpades` or
#' `simInitAndSpades`. It determines which one should run by, first, assessing whether
#' an identical `ll` has already been passed in a previous call to this function.
#' If an identical `ll` has never been passed, then this will run
#' `simInitAndSpades`. If a previous `ll` as been run, then this will 2)
#' assess whether there is a copy of an `simList` at `SpaDES.core:::savedSimEnv()$.sim`
#' (i.e., like `restartSpades`). If there is, then it will run `restartSpades()`.
#' If there is no `simList` at `SpaDES.core:::savedSimEnv()$.sim`, then it will
#' pass the `file` argument to `restartSpades(file)`.
#'
#' @return A `simList`, that has been "executed" until `end(sim)`, if it does not
#' hit an error.
#'
#' @rdname restartSpades
#' @export
#' @param ll A list of elements that would be passed to `simInit`, such as `modules`.
#' @param file An optional file that has a saved `simList`, e.g., from `saveSimList`
#'   or `saveState`.
#' @param reset Logical. If `TRUE`, then it will force `simInitAndSpades` to be called
#'   even if there is saved `sim` available.
restartOrSimInitAndSpades <- function(ll, file,
                                      reset = getOption("spades.resetRestart")) {
  # there are tempdir paths
  pathsOrig <- ll$paths
  ll$paths <- sapply(ll$paths, grep, invert = TRUE, value = TRUE, pattern = tempdir(), simplify = TRUE)
  fn <- function(ll) ll
  cached <- attr(reproducible::Cache(fn(ll), .functionName = "restartOrSimInitAndSpades"), ".Cache")$newCache %in% FALSE
  if (isTRUE(reset))
    cached <- FALSE
  ll$paths <- pathsOrig
  hasSavedToRAMState <- !is.null(savedSimEnv()$.sim)
  hasSavedToFileState <- file.exists(file)
  if (!cached || !(hasSavedToFileState || hasSavedToRAMState)) {
    message("ll has changed; rerunning simInitAndSpades")
    sim <- doCallSafe(SpaDES.core::simInitAndSpades, ll)
  } else {
    message("ll has not changed; trying restartSpades")
    if (isFALSE(hasSavedToRAMState)) {
      sim <- SpaDES.core::restartSpades(file)
    } else  {
      sim <- SpaDES.core::restartSpades()
    }
  }
}


#' @keywords internal
FirstFromR <- function(...) {
  ca <- commandArgs()
  .rndString <- ca[4]
  .spades.simFilename <- ca[5]
  First(.rndString = .rndString)
}

#' @importFrom cli col_green
#' @keywords internal
First <- function(...) {
  # From Rstudio, it gets all the correct, session-specific files.
  #   From R, it does not. Only has the commandArgs -- must rebuild objects
  fromRCmd <- FALSE
  browser()
  if (!exists(".attachedPkgsFilename")) {
    fromRCmd <- TRUE
    .rndString <- list(...)$.rndString
    .newDir <- tail(sort(dir("restartR", pattern = .rndString, full.names = TRUE)))
    load(file.path(.newDir, ".RData"))
  }

  # setwd(.oldWd)

  # attachedPkgsFilename <- file.path("~", paste0(".", .rndString), ".attachedPkgs.RData")
  load(.attachedPkgsFilename) # for "attached" object
  lapply(rev(attached), function(x) require(x, character.only = TRUE))
  sim <- loadSimList(.spades.simFilename)

  do.call(Sys.setenv, sim$._restartRList$envvars)

  do.call(options, sim$._restartRList$opts)
  if ("raster" %in% attached) {
    do.call(raster::rasterOptions, sim$._restartRList$optsRaster)
  }

  sim@paths <- Map(p = paths(sim), n = names(paths(sim)), function(p,n) {
    if (!dir.exists(p)) {
      newPath <- file.path(tempdir(), n)
      checkPath(newPath, create = TRUE)
      sim@paths[[n]] <- newPath
    } else {
      p
    }
  })
  # Removed -- now in params(sim)$restartR$.restartRInterval
  # options("spades.restartRInterval" = .spades.restartRInterval)

  # Moved -- now in spades call
  # assign(".Random.seed", sim@.xData$._randomSeed, envir = .GlobalEnv)
  # do.call("RNGkind", as.list(sim$._rng.kind))

  rm("._restartRList", envir = envir(sim))
  on.exit({
    if (fromRCmd)
      try(file.remove('~/.RData') )

    if (getOption("spades.restartR.clearFiles", TRUE))
      unlink(dirname(.attachedPkgsFilename), recursive = TRUE, force = TRUE)

    if (!fromRCmd) {
      objsToDelete <- c(".First", ".oldWd", ".spades.restartRInterval", ".spades.simFilename",
                        ".reloadPkgs", ".rndString", ".attachedPkgsFilename")
      try(rm(list = objsToDelete, envir = .GlobalEnv))
    }
  })
  if (!(Sys.getenv("RSTUDIO") == "1")) {
    sim <- eval(.spadesCall)
    message(cli::col_green("Because restartR was used, the simList is located in the location above.",
                           " It should be assigned to an object immediately: e.g.,\n",
                           "sim <- Copy(savedSimEnv()$.sim)"))
  } else {
    message(cli::col_green("Because restartR was used, the simList is now saved in the .GlobalEnv",
                           " named 'sim' (which may not be the same as the original assignment)"))
  }
  return(sim)
}

checkAndSetRestartDir <- function(sim, restartDir = outputPath(sim)) {
  if (is.call(restartDir)) {
    restartDir <- eval(restartDir)
  }

  if (is.null(restartDir)) {
    restartDir <- outputPath(sim)
  }

  usingSimPaths <- identical(restartDir, outputPath(sim))

  if (grepl(dirname(tempdir()), restartDir)) {
    restartDir <- outputPath(sim)
    usingSimPaths <- TRUE
  }

  if (grepl(dirname(tempdir()), restartDir)) {
    restartDir <- modulePath(sim)
    usingSimPaths <- TRUE
  }

  if (grepl(dirname(tempdir()), restartDir)) {
    restartDir <- inputPath(sim)
    usingSimPaths <- TRUE
  }

  if (grepl(dirname(tempdir()), restartDir)) {
    restartDir <- cachePath(sim)
    usingSimPaths <- TRUE
  }

  if (grepl(dirname(tempdir()), restartDir)) {
    if (usingSimPaths)
      warning("The supplied restartDir is in a temporary directory, as are all paths in the sim. ",
              " These will disappear at restart; please try again with a non-temporary path")
  }

  restartDir <- checkPath(restartDir, create = TRUE)
  return(restartDir)
}
