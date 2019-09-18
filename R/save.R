if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("saved", "saveTime", "fun", "package", "attached",
                           ".spades.restartRInterval", ".First", ".oldWd", ".spadesCall"))
}

# Just checks for paths, creates them if they do not exist
doEvent.save <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (NROW(outputs(sim)) > 0) {
      firstSave <- min(outputs(sim)[, "saveTime"], na.rm = TRUE)
      attributes(firstSave)$unit <- sim@simtimes[["timeunit"]]
      sim <- scheduleEvent(sim, firstSave, "save", "spades", .last())
      #sim <- scheduleEvent(sim, end(sim, sim@simtimes[["timeunit"]]), "save", "end", .last())
    }
    checkPath(sim@paths$outputPath, create = TRUE)
  } else if (eventType == "spades") {
    sim <- saveFiles(sim)
  } else if (eventType == "later") {
    sim <- saveFiles(sim)
  } else if (eventType == "end") {
    sim <- saveFiles(sim)
    message(crayon::green(paste0("Files saved. Use outputs(your simList) for details")), sep = "")
  }

  return(invisible(sim))
}

doEvent.restartR <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + getOption("spades.restartRInterval"), "restartR", "restartR", .last())

  } else if (eventType == "restartR") {
    nextTime <- time(sim, timeunit(sim)) + getOption("spades.restartRInterval")
    sim$.restartRList <- list()
    sim$.restartRList$endOrig <- end(sim)
    # sim$.restartRList$simFilename <- getOption("spades.restartR.RDataFilename")
    # sim$.restartRList$startOrig <- start(sim)
    # sim$.restartRList$wd <- asPath(getwd())
    # withTmpPaths <- grepl(tempdir(), paths(sim))
    # if (any(withTmpPaths)) {
    #   message("Some paths in the simList, ",
    #           paste(names(paths(sim))[withTmpPaths], collapse = ", "),
    #           ", are in temporary locations. These will not",
    #           " persist after restart as these locations disappear.")
    # }

    if (nextTime < end(sim, timeunit(sim))) {
      sim <- scheduleEvent(sim, nextTime, "restartR", "restartR", .last())
    }
    end(sim) <- time(sim)
    # stop("Exiting spades call to restart R")
  }

  return(invisible(sim))
}

##############################################################
#' Save objects using \code{.saveObjects} in \code{params} slot of \code{simInit}
#'
#' In the \code{\link{simInit}} call, a parameter called \code{.saveObjects} can be provided in
#' each module.
#' This must be a character string vector of all object names to save. These objects will
#' then be saved whenever a call to \code{saveFiles} is made.
#'
#' The file names will be equal to the object name plus \code{time(sim)} is
#' appended at the end.
#' The files are saved as \code{.rds} files, meaning, only one object gets
#' saved per file.
#'
#'
#' For objects saved using this function, the module developer must create save
#' events that schedule a call to \code{saveFiles}.
#'
#' If this function is used outside of a module, it will save all files in the
#' outputs(sim) that are scheduled to be saved at the current time in the simList.
#'
#' There are 3 ways to save objects using \code{SpaDES}.
#'
#' @section 1. Model-level saving:
#'
#' Using the \code{outputs} slot in the \code{\link{simInit}} call.
#' See example in \code{\link{simInit}}.
#' This can be convenient because it gives overall control of many modules at a
#' time, and it gets automatically scheduled during the
#' \code{\link{simInit}} call.
#'
#' @section 2. Module-level saving:
#'
#' Using the \code{saveFiles} function inside a module.
#' This must be accompanied by a \code{.saveObjects} list element in the
#' \code{params} slot in the \code{\link{simList}}.
#' Usually a module developer will create this method for future users of
#' their module.
#'
#' @section 3. Custom saving:
#'
#' A module developer can save any object at any time inside their module, using
#' standard R functions for saving R objects (e.g., \code{save} or \code{saveRDS}).
#' This is the least modular approach, as it will happen whether a module user
#' wants it or not.
#'
#' @author Eliot McIntire
#' @author Alex Chubaty
#' @note It is not possible to schedule separate saving events for each object
#' that is listed in the \code{.saveObjects}.
#'
#' @param sim A \code{simList} simulation object.
#'
#' @importFrom data.table data.table
#' @importFrom dplyr bind_rows
#' @export
#' @rdname saveFiles
#'
#' @examples
#' \dontrun{
#'
#' # This will save the "caribou" object at the save interval of 1 unit of time
#' #  in the outputPath location
#' outputPath <- file.path(tempdir(), "test_save")
#' times <- list(start = 0, end = 6, "month")
#' parameters <- list(
#'   .globals = list(stackName = "landscape"),
#'   caribouMovement = list(
#'     .saveObjects = "caribou",
#'     .saveInitialTime = 1, .saveInterval = 1
#'   ),
#'   randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20))
#'
#' modules <- list("randomLandscapes", "caribouMovement")
#' paths <- list(
#'   modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'   outputPath = savePath
#' )
#' mySim <- simInit(times = times, params = parameters, modules = modules,
#'                  paths = paths)
#'
#' # The caribou module has a saveFiles(sim) call, so it will save caribou
#' spades(mySim)
#' dir(outputPath)
#'
#' # remove the files
#' file.remove(dir(savePath, full.names = TRUE))
#'
#' }
saveFiles <- function(sim) {
  curTime <- time(sim, sim@simtimes[["timeunit"]])
  # extract the current module name that called this function
  moduleName <- sim@current[["moduleName"]]
  if (length(moduleName) == 0) {
    moduleName <- "save"
    if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime, ])) {
      outputs(sim)[outputs(sim)$saveTime == curTime, "saved"] <- NA
    }
  }

  if (moduleName != "save") {
    # i.e., a module driven save event
    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] %>%
      data.frame(objectName = ., saveTime = curTime, file = ., stringsAsFactors = FALSE)
    toSave <- .fillOutputRows(toSave)
    outputs(sim) <- rbind(outputs(sim), toSave)

    # don't need to save exactly same thing more than once - use data.table here because distinct
    # from dplyr does not do as expected
    outputs(sim) <- data.table(outputs(sim)) %>%
      unique(., by = c("objectName", "saveTime", "file", "fun", "package")) %>%
      data.frame()
  }

  if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved), "saved"]) > 0) {
    wh <- which(outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      if (exists(outputs(sim)[i, "objectName"], envir = sim@.xData)) {
        args <- append(list(get(outputs(sim)[i, "objectName"], envir = sim@.xData),
                            file = outputs(sim)[i, "file"]),
                       outputArgs(sim)[[i]])
        args <- args[!sapply(args, is.null)]
        args <- suppressWarnings(args[!unlist(lapply(args, function(j) {
          isTRUE(tryCatch(is.na(j), error = function(e) FALSE))
        }))])

        # The actual save line
        do.call(outputs(sim)[i, "fun"], args = args,
                envir = getNamespace(outputs(sim)[i, "package"]))

        outputs(sim)[i, "saved"] <- TRUE
      } else {
        warning(paste(outputs(sim)$obj[i], "is not an object in the simList. Cannot save."))
        outputs(sim)[i, "saved"] <- FALSE
      }
    }
  }

  # Schedule an event for the next time in the saveTime column
  if (any(is.na(outputs(sim)[outputs(sim)$saveTime > curTime, "saved"]))) {
    nextTime <- min(outputs(sim)[is.na(outputs(sim)$saved), "saveTime"], na.rm = TRUE)
    attributes(nextTime)$unit <- sim@simtimes[["timeunit"]]
    if (time(sim) == end(sim)) {
      sim <- scheduleEvent(sim, nextTime, "save", "end", .last())
    } else {
      sim <- scheduleEvent(sim, nextTime, "save", "later", .last())
    }
  }
  return(invisible(sim))
}

#' File extensions map
#'
#' A \code{data.frame} with information on how to load various types of files in R,
#' containing the columns:
#' \itemize{
#'   \item \code{exts}: the file extension;
#'   \item \code{fun}: the function to use for files with this file extension;
#'   \item \code{package}: the package from which to load \code{fun}.
#' }
#'
#' @export
#' @importFrom data.table setnames
#' @rdname loadFiles
.saveFileExtensions <- function() {
  .sFE <- data.frame(matrix(ncol = 3, byrow = TRUE, c(
    "rds", "saveRDS", "base",
    "txt", "write.table", "utils",
    "csv", "write.csv", "utils",
    "grd", "writeRaster", "raster"
  )), stringsAsFactors = FALSE)
  setnames(.sFE, new = c("exts", "fun", "package"), old = paste0("X", 1:3))
  .sFE <- .sFE[order(.sFE$package, .sFE$fun), ]
  return(.sFE)
}


#' Save a whole \code{simList} object to disk
#'
#' Because of the environment slot, this is not quite as straightforward as
#' just saving the object. This also has option for file-backed Rasters.
#'
#' @inheritParams spades
#'
#' @param sim Either a \code{simList} or a character string of the name
#'        of a \code{simList} that can be found in \code{envir}. Using
#'        a character string will assign that object name to the saved
#'        \code{simList}, so when it is recovered it will be given that
#'        name.
#' @param envir If \code{sim} is a character string, then this must be provided.
#'        It is the environment where the object named \code{sim} can
#'        be found.
#'
#' @param filename Character string with the path for saving \code{simList}
#'
#' @param fileBackendToMem Logical. If there are file-backed \code{Raster}
#'        objects, should they be converted to memory objects, or should
#'        they be kept as file-backed rasters w.
#'        or loaded into RAM and saved within the \code{.RData} file.
#'        If \code{TRUE} (default), then the files will be copied to
#'        \code{file.path(dirname(filename), "rasters")}.
#' @param filebackedDir Character string, indicating a path to save
#'        the files to in file-backed objects (currently, only Raster* objects)
#' @param ... Passed to \code{save}, e.g., \code{compression}
#'
#' @return A saved \code{.RData} file in \code{filename} location.
#'
#' @export
#' @rdname saveSimList
#' @seealso zipSimList
#'
saveSimList <- function(sim, filename, fileBackendToMem = TRUE, filebackedDir = NULL, envir, ...) {
  if (is.character(sim)) {
    simName <- sim
    sim <- get(simName, envir = envir)
  }
  isRaster <- unlist(lapply(sim@.xData, function(x) is(x, "Raster")))
  if (any(isRaster)) {
    InMem <- unlist(lapply(mget(names(isRaster)[isRaster], envir = SpaDES.core::envir(sim)), function(x) inMemory(x)))
    needModifying <- isTRUE(fileBackendToMem || ( (!all(InMem)) && !is.null(filebackedDir)))
    if (needModifying) {
      sim <- Copy(sim, filebackedDir = filebackedDir)
      if (fileBackendToMem) {
        # Need to copy it because the moving to memory affects the original simList
        for (x in names(isRaster)[isRaster][!InMem])
          sim[[x]][] <- sim[[x]][]
      } else {
        rasterNamesNotInMem <- names(isRaster)[isRaster][!InMem]
        list2env(Copy(mget(rasterNamesNotInMem, envir = sim@.xData),
                      filebackedDir = filebackedDir),
                 envir = sim@.xData)# don't want to mess with rasters that are on disk for original
      }

    }

  }
  if (exists("simName", inherits = FALSE)) {
    tmpEnv <- new.env(parent = emptyenv())
    assign(simName, sim, envir = tmpEnv)
    save(list = simName, envir = tmpEnv, file = filename, ...)
  } else {
    save(sim, file = filename, ...)
  }
  return(invisible())
}

#' Zip many of the files in a simList
#'
#' Currently, this will save the raster-backed files, outputs(sim), inputs(sim).
#' It will add these to a temp file, using \code{Copy}, where appropriate
#' to not affect the original \code{simList}. VERY experimental. Unlikely
#' to work at the moment.
#'
#' @inheritParams saveSimList
#' @param sim A simList at the core of the zipping.
#' @param ... passed to \code{saveSimList}, including important ones
#'    such as \code{filename}.
#' @param zipfile A character string indicating the filename for the
#'    zipfile. Passed to \code{zip}
#' @param outputs Logical. If \code{TRUE}, all files identified in
#'    \code{outputs(sim)} will be included in the zip.
#' @param inputs Logical. If \code{TRUE}, all files identified in
#'    \code{inputs(sim)} will be included in the zip.
#' @param cache Logical. If \code{TRUE}, all files in
#'    \code{cachePath(sim)} will be included in the zip. Defaults to
#'    \code{FALSE} as this could be large, and may include many
#'    out of date elements. See Details. Not yet implemented.
#'
#' @details
#' If \code{cache} is used, it is likely that it should be trimmed before
#' zipping, to include only cache elements that are relevant.
zipSimList <- function(sim, zipfile, ..., outputs = TRUE, inputs = TRUE,
                       cache = FALSE) {
  dots <- list(...)
  if (is.null(dots$filename)) dots$filename <- paste0(rndstr(1, 6), ".RData")
  tmpDir <- file.path(tempdir(), rndstr(1, 6))
  tmpRData <- file.path(tmpDir, basename(dots$filename))
  if (is.null(dots$filebackedDir)) dots$filebackedDir <- paste0("rasters")
  if (is.null(dots$fileBackendToMem)) dots$fileBackendToMem <- formals(saveSimList)$fileBackendToMem
  tmpRasters <- file.path(tmpDir, basename(dots$filebackedDir))
  saveSimList(sim, filename = tmpRData, filebackedDir = tmpRasters, fileBackendToMem = dots$fileBackendToMem)

  newnamesOutputs <- NULL
  if (isTRUE(outputs)) {
    if (NROW(outputs(sim)) > 0) {
      tmpOutputs <- file.path(tmpDir, "outputs")
      checkPath(tmpOutputs, create = TRUE)
      newnamesOutputs <- file.path(tmpOutputs, gsub(outputPath(sim), "", outputs(sim)$file))
      newnamesOutputs <- gsub("//", "/", newnamesOutputs)
      newnamesOutputs <- gsub("\\\\\\\\", "\\\\", newnamesOutputs)
      file.symlink(outputs(sim)$file, newnamesOutputs)
    }
  }
  newnamesInputs <- NULL
  if (isTRUE(inputs)) {
    if (NROW(inputs(sim)) > 0) {
      tmpInputs <- file.path(tmpDir, "inputs")
      checkPath(tmpInputs, create = TRUE)
      newnamesInputs <- file.path(tmpInputs, gsub(inputPath(sim), "", inputs(sim)$file))
      newnamesInputs <- gsub("//", "/", newnamesInputs)
      newnamesInputs <- gsub("\\\\\\\\", "\\\\", newnamesInputs)
      file.symlink(inputs(sim)$file, newnamesInputs)
    }
  }
  zip(zipfile = zipfile, files = c(tmpRData, dir(tmpRasters, full.names = TRUE, recursive = TRUE),
                                   newnamesOutputs, newnamesInputs))


}

#' Restart R programmatically
#'
#' This will attempt to restart the R session, reloading all packages.
#' Currently, this is not intended for general use: it has many specialized
#' pieces for using inside a \code{spades} call.
#' The main purpose for doing this is to clear memory leaks (possibly deep
#' in R \url{https://github.com/r-lib/fastmap}) that are not
#' fully diagnosed. This is still very experimental. USE AT YOUR OWN RISK. This
#' should only be used if there are RAM limitations being hit with long running
#' simulations. Currently only works within Rstudio. The way to initiate
#' restarting of R is simply setting the \code{spades.restartRInterval}
#' greater than 0, which is the default,
#' e.g., \code{options("spades.restartRInterval" = 100)}. This is only intended
#' to restart a simulation in exactly the same place as it was (i.e., can\'t change
#' machines), and because of the restart, the output of the \code{spades} call
#' will be either to \code{sim} or the user must make such an assignment
#' manually, e.g., \code{sim <- SpaDES.core:::.pkgEnv$.sim}. This is stated in
#' a message.
#'
#' @export
#' @param reloadPkgs Logical. If \code{TRUE}, it will attempt to reload all the packages
#'   as they were in previous session, in the same order. If \code{FALSE}, it will
#'   load no packages beyond normal R startup. Default \code{TRUE}
#' @param .First A function to save to \code{~/.RData} which will
#'    be loaded at restart from \code{~/.RData} and run. Default is \code{NULL},
#'    meaning it will use the nonexported \code{SpaDES.core:::First}. If a
#'    user wants to make a custom \code{.First} file, it should build off that one.
#' @param .RDataFile A filename for temporary storage of simList. Defaults to
#' \code{getOption("spades.restartR.RDataFilename")}, but with a random string
#' added to the base filename to prevent collisions from parallel processes.
#' @param restartDir A character string indicating which working directory to
#'     use during restart. Defaults to \code{getwd()}, unless it is a
#'     directory in \code{tempdir()}, which may not exist through a restart.
#'     In that case, it will default to \code{"~"}.
#' @param sim Required. A \code{simList} to be retained through the restart
#'
#' @details
#' The process responds to several options. Though under most cases,
#' the default behaviour should suffice. These are of 3 types: \code{restartRInterval}
#' the arguments to \code{restartR} and the arguments to \code{saveSimList}, these latter two
#' using a dot to separate the function name and its argument,
#' e.g., \code{options("spades.restartR.restartDir" = "~"} and
#' \code{options("spades.saveSimList.fileBackendToMem" = FALSE)}. See specific functions for
#' defaults and argument meanings. The only difference from the default function values
#' is with \code{saveSimList} argument \code{fileBackendToMem = FALSE} during \code{restartR}
#' by default, because it is assumed that the file backends will still be intact after a
#' restart, so no need to move them all to memory.
#'
#' @note
#' Because of the restarting, the object name of the original assignment of the
#' \code{spades} call can not be preserved. The \code{spades} call will be
#' assigned to \code{sim} in the \code{.GlobalEnv}.
#'
#' Because this function is focused on restarting during a \code{spades} call,
#' it will remove all objects in the \code{.GlobalEnv}, emulating \code{q("no")}.
#' If the user wants to keep those objects, then they should be saved to disk
#' immediately before the \code{spades} call. This can then be recovered immediately
#' after the return from the \code{spades} call.
#'
restartR <- function(reloadPkgs = TRUE, .First = NULL,
                     .RDataFile = getOption("spades.restartR.RDataFilename"),
                     restartDir = NULL, sim) {
  .rndString <- rndstr(1, 9)

  if (missing(sim)) stop("sim is currently a required argument")
  vanillaPkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                   "package:grDevices", "package:utils", "package:datasets", "package:methods",
                   "Autoloads", "package:base", "devtools_shims")
  srch <- search()
  attached <- srch
  attached <- grep("package:", attached, value = TRUE)
  attached <- unlist(lapply(attached, function(x) gsub(x, pattern = 'package:', replacement = '')))
  .newDir <- file.path("~", paste0(".", .rndString))
  checkPath(.newDir, create = TRUE)
  .attachedPkgsFilename <- file.path(.newDir, '.attachedPkgs.RData')
  save(file = .attachedPkgsFilename, attached)
  if (is.null(.First)) {
    .First <- getFromNamespace("First", "SpaDES.core")
  }

  .oldWd <- getwd()
  if (is.null(restartDir)) {
    restartDir <- if (grepl(tempdir(), getwd())) {
      "~"
    } else {
      getwd()
    }
  }
  setwd(restartDir)


  if (is.null(sim$.restartRList)) sim$.restartRList <- list()
  sim$.restartRList$simFilename <- file.path(.newDir, basename(getOption("spades.restartR.RDataFilename")))
  # sim$.restartRList$endOrig <- end(sim)
  sim$.restartRList$startOrig <- start(sim)
  sim$.restartRList$wd <- asPath(getwd())
  withTmpPaths <- grepl(tempdir(), paths(sim))
  if (any(withTmpPaths)) {
    message("Some paths in the simList, ",
            paste(names(paths(sim))[withTmpPaths], collapse = ", "),
            ", are in temporary locations. These will not",
            " persist after restart as these locations disappear.")
  }
  saveSimListFormals <- formals(saveSimList)
  saveSimList(sim,
              filename = getOption("spades.saveSimList.filename", sim$.restartRList$simFilename),
              fileBackendToMem = getOption("spades.saveSimList.fileBackendToMem", FALSE),
              filebackedDir = getOption("spades.saveSimList.filebackedDir", saveSimListFormals$filebackedDir))
  if (requireNamespace("pryr")) {
    mu <- getFromNamespace("mem_used", "pryr")()
    class(mu) <- "object_size"
    message(crayon::bgBlue(crayon::white(format(mu, units = "auto"))))
  }

  .spadesCall <- sim$.restartRList$.spadesCall
  .spades.simFilename <- sim$.restartRList$simFilename
  .spadesCall$sim <- as.name("sim") # user may not have called the object "sim" ... now it is for restarting
  .spades.restartRInterval <- getOption("spades.restartRInterval")
  # save .First function and the .oldWd
  #if (isTRUE(reloadPkgs))
  .reloadPkgs <- reloadPkgs
  .RDataFile <- file.path(.newDir, ".RData")
  save(file = .RDataFile, .First, .oldWd, .spadesCall, .spades.restartRInterval, .spades.simFilename,
       .reloadPkgs, .rndString, .attachedPkgsFilename, eval.promises = TRUE)

  if (isTRUE(Sys.getenv("RSTUDIO") == "1")) {
    if (requireNamespace("rstudioapi")) {
      lapply(setdiff(srch, vanillaPkgs), function(pkg) detach(pkg, character.only = TRUE, unload = TRUE, force = TRUE))
      rm(list = ls(all.names = TRUE, envir = .GlobalEnv), envir = .GlobalEnv)

      # Need load to get custom .First fn
      rstudioapi::restartSession(paste0("{load('",.RDataFile,"'); sim <- .First(); sim <- eval(.spadesCall)}")) #
      #rstudioapi::restartSession(if (reloadPkgs) paste0("{load('~/.RData'); sim <- .First(); sim <- eval(.spadesCall)}") else ""}")
    } else {
      message("Running RStudio. To restart it this way, you must run: install.packages('rstudioapi')")
    }
  } else {
    #reg.finalizer(.GlobalEnv, function(e) system("R --no-save"), TRUE)
    # R cmd line loads .RData first, then .First, if there is one.
    .First <- SpaDES.core:::FirstFromR

    # if there is an .RData file, keep it -- will be put back later.
    if (file.exists(file.path("~", ".RData")))
      file.link(file.path("~",".RData"), paste0(file.path("~",".RData"), .rndString))
    save(file = "~/.RData", .First)
    out <- reg.finalizer(as.environment("package:SpaDES.core"), function(e) system(paste0("R --no-save --args ", .rndString)), TRUE)
    q("no")
  }

}

FirstFromR <- function(...) {
  .rndString <- commandArgs()[4]
  SpaDES.core:::First(.rndString = .rndString)
}

First <- function(...) {
  # From Rstudio, it gets all the correct, session-specific files.
  #   From R, it does not. Only has the commandArgs -- must rebuild objects
  fromRCmd <- FALSE
  if (!exists(".attachedPkgsFilename")) {
    fromRCmd <- TRUE
    .rndString <- list(...)$.rndString
    # if there was an .RData file, move it back
    if (file.exists(paste0(file.path("~",".RData"), .rndString))) {
      rdataPath <- file.path("~",".RData")
      file.remove(rdataPath)
      file.link(paste0(file.path("~",".RData"), .rndString), rdataPath)
    }

    .newDir <- file.path("~", paste0(".", .rndString))
    load(file.path(.newDir, ".RData"))
  }

  #attachedPkgsFilename <- file.path("~", paste0(".", .rndString), '.attachedPkgs.RData')
  load(.attachedPkgsFilename) # for "attached" object
  lapply(rev(attached), function(x) require(x, character.only = TRUE))
  load(.spades.simFilename)
  #load(getOption('spades.restartR.RDataFilename', "~/.restartR.RData")) # load "sim" here
  sim@paths <- Map(p = paths(sim), n = names(paths(sim)), function(p,n) {
      if (!dir.exists(p)) {
        newPath <- file.path(tempdir(), n)
        checkPath(newPath, create = TRUE)
        sim@paths[[n]] <- newPath
      } else {
        p
      }
  })
  options("spades.restartRInterval" = .spades.restartRInterval)

  end(sim) <- sim$.restartRList$endOrig
  assign(".Random.seed", sim$.restartRList$.randomSeed, envir = .GlobalEnv)

  rm(".restartRList", envir = envir(sim))
  on.exit({
    if (fromRCmd)
      try(file.remove('~/.RData') )
    unlink(dirname(.attachedPkgsFilename), recursive = TRUE, force = TRUE)
                #getOption("spades.restartR.RDataFilename"))
    if (!fromRCmd) {
      objsToDelete <- c(".First", ".oldWd", ".spades.restartRInterval", ".spades.simFilename",
                        ".reloadPkgs", ".rndString", ".attachedPkgsFilename")
      try(rm(list = objsToDelete, envir = .GlobalEnv))
    }
  })
  if (!(Sys.getenv("RSTUDIO") == "1")) {
    sim <- eval(.spadesCall)
    message(crayon::green("Because restartR was used, the simList is located in the location above.",
                            " It should be assigned to an object immediately: e.g.,\n",
                            "sim <- SpaDES.core:::.pkgEnv$.sim"))
  } else {
    message(crayon::green("Because restartR was used, the simList is now saved in the .GlobalEnv",
                            " named 'sim' (which may not be the same as the original assignment)"))
  }
  setwd(.oldWd)
  return(sim)
}


