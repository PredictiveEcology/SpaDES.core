utils::globalVariables(c(
  ".", ".attachedPkgsFilename", "et", ".First", ".oldWd",
  ".spadesCall", ".spades.restartRInterval", ".spades.simFilename"
))

doEvent.restartR <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (is.null(P(sim, module = ".restartR")$.restartRInterval))
      params(sim)$restartR$.restartRInterval <- getOption("spades.restartRInterval")
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + P(sim, module = ".restartR")$.restartRInterval,
                         "restartR", "restartR", .last())

  } else if (eventType == "restartR") {
    nextTime <- time(sim, timeunit(sim)) + P(sim, module = ".restartR")$.restartRInterval

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
#' This is very experimental and has not been thoroughly tested. Use with caution.
#' This function will re-parse a single module (currently) into the `simList`
#' where its source code should reside, and then optionally restart a simulation
#' that stopped on an error, presumably after the developer has modified the
#' source code of the module that caused the break.
#' This will restart the simulation at the next event in the event queue
#' (i.e., returned by `events(sim)`). Because of this, this function will
#' not do anything if the event queue is empty.
#'
#' @details
#' This will only parse the source code from the named module. It will not affect any
#' objects that are in the `mod` or `sim`.
#'
#' The random number seed will be reset to the state it was at the start of the
#' earliest event recovered, thereby returning to the exact stochastic simulation
#' trajectory.
#'
#' @note This will only work reliably
#' *if the `simList` was not modified yet during the event which caused the error*.
#' The `simList` will be in the state it was at the time of the error.
#'
#' @param sim A `simList` or a filename that will load a `simList`, e.g., from
#'    `saveState` or `saveSimList`. If not supplied (the default),
#'    this will take the `sim` from
#'    `savedSimEnv()$.sim`, i.e., the one that was interrupted
#'
#' @param module A character string length one naming the module that caused the error and
#'   whose source code was fixed. This module will be re-parsed and placed into the `simList`.
#'
#' @param restart Logical. If `TRUE`, then the call to `spades` will be made, i.e.,
#'   restarting the simulation. If `FALSE`, then it will return a new `simList`
#'   with the module code parsed into the `simList`.
#'
#' @param numEvents Numeric. Default is Inf (i.e., all available). In the `simList`, if
#'   `options('spades.recoveryMode')` is set to `TRUE` or a numeric, then
#'   there will be a list in the `simList` called `.recoverableObjs`. These will be
#'   replayed backwards in time to reproduce the initial state of the `simList` before
#'   the event that is `numEvents` back from the first event in `events(sim)`.
#'
#' @param verbose Controls the level of information messaging that is printed.
#'   See [reproducible::reproducibleOptions()].
#'
#' @param ... Passed to `spades`, e.g., `debug`, `.plotInitialTime`.
#'
#' @return A `simList` as if `spades` had been called on a `simList`.
#'
#' @export
#' @importFrom cli col_blue
#' @importFrom reproducible Cache
#'
#' @examples
#' \donttest{
#' # options("spades.recoveryMode" = 1) # now the default
#' s <- simInit()
#' s <- spades(s) # if this is interrupted or fails
#' # the following line will not work if the previous line didn't fail
#' s <- restartSpades(s) # don't need to specify `sim` if previous line fails
#'                      # will take from savedSimEnv()$.sim automatically
#' }
restartSpades <- function(sim = NULL, module = NULL, numEvents = Inf, restart = TRUE,
                          verbose = getOption("reproducible.verbose", 1L), ...) {
  message("Running restartSpades ... this is very experimental; ",
          "this should be used with caution.")

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

  if (is.null(module)) {
    # Source the file you changed, into the correct location in the simList
    module <- events(sim)[["moduleName"]][1]
  }

  # move "completed" back into event queue
  numMods <- min(length(sim$.recoverableObjs), numEvents)
  if (numMods > 0) {
    com <- completed(sim)
    etSecs <- sum(com[, et := difftime(get(._txtClockTime), get(._txtPrevEventTimeFinish), units = "secs"),
                      by = seq_len(NROW(com))]$et)

    # remove the times of the completed events - 1 because the restartSpaDES includes the incompleted event
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
    if (numMods > length(sim$.recoverableObjs))
      message("Cannot replay ", numMods, " events as requested by numMods; ",
              "there are only ", length(sim$.recoverableObjs), " that can be recovered.")
    if (numMods < length(sim$.recoverableObjs))
      sim$.recoverableObjs <- sim$.recoverableObjs[seq_len(numMods)]
    eventIndices <- seq_len(NROW(eventsToReplayDT))
    eventIndicesRev <- rev(eventIndices)
    # names(sim$.recoverableObjs) <- eventsToReplayDT$moduleName[eventIndicesRev]

    modules <- eventsToReplayDT$moduleName[eventIndicesRev]
    modules <- unique(modules)
    names(modules) <- modules
    modules <- modules[!modules %in% unlist(.coreModules())]
    # move objects back in place
    # browser(expr = exists("._restartSpades_2"))
    out <- lapply(eventIndices, function(event) {
      objNames <- names(sim$.recoverableObjs[[event]])
      notYetCreated <- setdiff(outputObjects(sim)[[modules[event]]]$objectName, objNames)
      names(notYetCreated) <- notYetCreated
      notYetCreatedList <- lapply(notYetCreated, function(x) NULL)

      # need to overwrite with NULL if the object was not yet created
      sim$.recoverableObjs[[event]] <- append(sim$.recoverableObjs[[event]], notYetCreatedList)
      # sim$.recoverableObjs[[event]]
      objsToCopy <- sim$.recoverableObjs[[event]]

      objNames <- names(objsToCopy)
      # objNames <- setdiff(objNames, notYetCreated)
      if (!is.null(objNames)) {
        # only take objects that changed -- determine which ones are changed
        whNULLs <- sapply(objsToCopy, is.null)
        objsWONULLSs <- objsToCopy[!whNULLs]
        if (any(whNULLs)) {
          NULLed <- names(whNULLs)[whNULLs]
          keeps <- names(whNULLs)[!whNULLs]
          a <- suppressWarnings(rm(list = NULLed, envir = sim@.xData))
          objsToCopy <- objsToCopy[keeps]
        }

        fd1 <- lapply(objsToCopy, function(obj) .robustDigest(obj))
        objNames <- objNames[objNames %in% ls(sim@.xData)]
        fd2 <- lapply(mget(objNames, envir = sim@.xData), function(obj) .robustDigest(obj))
        if (!is.null(fd2)) {
          fd1 <- fd1[match(names(fd2), names(fd1))]
          stopifnot(all.equal(sort(names(fd1)), sort(names(fd2))))
          fd1 <- fd1[!unlist(unname(fd1)) %in% unlist(unname(fd2))]
        }
        # move the changed ones to the simList
        if (NROW(fd1))
          list2env(objsToCopy[names(fd1)], envir = sim@.xData)
      }

      if (length(sim$.recoverableModObjs)) {
        modObjNames <- names(sim$.recoverableModObjs[[event]])
        modObjEnv <- sim[[dotObjs]][[modules[event]]] # $.objects
        modObjLs <- ls(modObjEnv)
        if (length(modObjLs)) { # there are some --> maybe need to delete them
          toDelete <- setdiff(modObjLs, modObjNames)
          if (length(toDelete)) {
            rm(list = toDelete, envir = modObjEnv)
          }
        }
      }

      message(cli::col_blue("Reversing event: ",
                           paste(collapse = " ",
                                 paste(unname(eventsToReplayDT[eventIndicesRev[event]])))))
      invisible()
    })

    # Once reversed, remove the .recoverableObjs
    sim$.recoverableObjs <- NULL

    # modules <- if (!is.list(module)) as.list(module) else module

    # reset activeBinding mod
    out <- lapply(modules, function(mod) {
      makeModActiveBinding(sim = sim, mod = mod)
    })
    out <- lapply(modules, function(mod) {
      makeParActiveBinding(sim = sim, mod = mod)
    })

    # Remove all added events that occurred during the events, i.e., via scheduleEvent
    sim@events <- setdiff(sim@events, unlist(sim$.addedEvents[seq_len(numMods)], recursive = FALSE))
    sim@current <- list()
    assign(".Random.seed", sim@.xData$._randomSeed[[numMods]], envir = .GlobalEnv)
  } else {
    modules <- modules(sim)
  }

  opt <- options("spades.moduleCodeChecks" = FALSE)


  out <- lapply(modules, function(module) {
    pp <- list()
    moduleFolder <- file.path(modulePath(sim, module = module), module)
    if (file.exists(file.path(moduleFolder, paste0(module, ".R")))) {
      # pp[[1]] <- .parseConditional(sim, file.path(moduleFolder, paste0(module, ".R")))
      pp[[1]] <- parse(file.path(moduleFolder, paste0(module, ".R")))
      subFiles <- dir(file.path(moduleFolder, "R"), full.names = TRUE)

      doesntUseNamespacing <- !.isNamespaced(sim, module)

      # evaluate the rest of the parsed file
      sim <- currentModuleTemporary(sim, module)
      pkgs = slot(slot(depends(sim), "dependencies")[[module]], "reqdPkgs")
      if (doesntUseNamespacing) {
        out1 <- evalWithActiveCode(pp[[1]], sim@.xData, sim = sim, pkgs = pkgs)
      }


      if (length(subFiles)) {
        pp[seq_len(length(subFiles)) + 1] <- lapply(subFiles, function(ff) parse(ff))
      }
      #ee <- new.env()
      #ee$sim <- sim
      # sim@.xData[[module]]$sim <- sim
      lapply(pp, function(pp1)
        evalWithActiveCode(pp1, sim@.xData[[dotMods]][[module]], sim = sim, pkgs = pkgs))
      message(cli::col_blue("Reparsing ", module, " source code"))
    }
    #rm(list = "sim", envir = ee)
    #list2env(as.list(ee, all.names = TRUE), envir = sim@.xData[[module]])
    invisible()
  })
  options(opt)

  if (restart)
    sim <- spades(sim, ...)
  # } else {
  #   message("There was no interrupted spades call; returning sim as is")
  # }
  return(sim)
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
#' `spades` call can not be preserved. The `spades` call will be
#' assigned to `sim` in the `.GlobalEnv`.
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
#' @param sim Required. A `simList` to be retained through the restart
#'
#' @param reloadPkgs Logical. If `TRUE`, it will attempt to reload all the packages
#'    as they were in previous session, in the same order. If `FALSE`, it will
#'    load no packages beyond normal R startup. Default `TRUE`
#'
#' @param .First A function to save to \file{~/.qs} which will
#'    be loaded at restart from \file{~/.qs} and run. Default is `NULL`,
#'    meaning it will use the non-exported `SpaDES.core:::First`. If a
#'    user wants to make a custom `First` file, it should built off that one.
#'
#' @param .RDataFile A filename for saving the `simList`.
#'     Defaults to `getOption("spades.restartR.filename")`, and the directory will
#'     be in `restartDir`. The simulation time will be mid-pended to this
#'     name, as in: `basename(file), "_time",`
#'     `paddedFloatToChar(time(sim), padL = nchar(as.character(end(sim))))))`
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

  sim$._restartRList$opts <- options()
  if ("raster" %in% attached) {
    invisible(capture.output({
      sim$._restartRList$optsRaster <- raster::rasterOptions()
    }))
    sim$._restartRList$optsRaster$depracatedwarnings <- sim$._restartRList$optsRaster$depwarning
    sim$._restartRList$optsRaster$depwarning <- NULL
  }

  sim$._restartRList$simFilename <- file.path(.newDir, paste0(
    basename(file), "_time",
    paddedFloatToChar(time(sim), padL = nchar(as.character(end(sim))))))

  ## ensure correct file extension
  sim$._restartRList$simFilename <- raster::extension(sim$._restartRList$simFilename, ".qs")

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
    filename = getOption("spades.saveSimList.filename", sim$._restartRList$simFilename),
    fileBackend = getOption("spades.saveSimList.fileBackend", 0),
    filebackedDir = getOption("spades.saveSimList.filebackedDir", saveSimListFormals$filebackedDir)
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
    rstudioapi::restartSession(paste0("{load('", .RDataFile, "'); ",
                                      "sim <- .First(); ",
                                      "sim <- eval(.spadesCall)}"))
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
  if (!exists(".attachedPkgsFilename")) {
    fromRCmd <- TRUE
    .rndString <- list(...)$.rndString
    .newDir <- tail(sort(dir("restartR", pattern = .rndString, full.names = TRUE)))
    load(file.path(.newDir, ".RData"))
  }

  setwd(.oldWd)

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
