utils::globalVariables(c("attached", "fun", "package", "saved", "saveTime"))

# Just checks for paths, creates them if they do not exist
doEvent.save <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (NROW(outputs(sim)) > 0) {
      firstSave <- min(outputs(sim)[, "saveTime"], na.rm = TRUE)
      firstSaveWh <- which.min(outputs(sim)[, "saveTime"])
      if ("eventPriority" %in% colnames(outputs(sim))) {
        firstPriority <- outputs(sim)[firstSaveWh, "eventPriority"]
      }
      if (!exists("firstPriority", inherits = FALSE))
        firstPriority <- .last()
      attributes(firstSave)$unit <- sim@simtimes[["timeunit"]]
      sim <- scheduleEvent(sim, firstSave, "save", "spades", firstPriority)
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
    isNA <- is.na(outputs(sim)$saved)
    nextTime <- min(outputs(sim)[isNA, "saveTime"], na.rm = TRUE)
    nextTimeWh <- which.min(outputs(sim)[isNA, "saveTime"])
    if ("eventPriority" %in% colnames(outputs(sim))) {
      nextPriority <- outputs(sim)[isNA, "eventPriority"][nextTimeWh]
    }
    if (!exists("nextPriority", inherits = FALSE))
      nextPriority <- .last()

    attributes(nextTime)$unit <- sim@simtimes[["timeunit"]]
    if (time(sim) == end(sim)) {
      sim <- scheduleEvent(sim, nextTime, "save", "end", nextPriority)
    } else {
      sim <- scheduleEvent(sim, nextTime, "save", "later", nextPriority)
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
    "qs", "qsave", "qs",
    "txt", "write.table", "utils",
    "csv", "write.csv", "utils",
    "grd", "writeRaster", "raster"
  )), stringsAsFactors = FALSE)
  setnames(.sFE, new = c("exts", "fun", "package"), old = paste0("X", 1:3))
  .sFE <- .sFE[order(.sFE$package, .sFE$fun), ]
  return(.sFE)
}

#' Generate simulation file name
#'
#' Assists with saving and retrieving simulations
#' (e.g., with \code{saveSimList} and \code{loadSimList}).
#'
#' @param name Object name (e.g., \code{"mySimOut"})
#' @param path Directory location in where the file will be located (e.g., an \code{outputPath}).
#' @param time Optional simulation time to use as filename suffix. Default \code{NULL}.
#' @param ext  The file extension to use (default \code{"rds"}).
#'
#' @export
#' @importFrom reproducible normPath
simFile <- function(name, path, time = NULL, ext = "rds") {
  if (is.null(time))
    file.path(normPath(path), paste0(name, ".", ext))
  else {
    file.path(normPath(path), paste0(name, "_", paddedFloatToChar(time, padL = 4), ".", ext))
  }
}

#' Save a whole \code{simList} object to disk
#'
#' Because of the environment slot, this is not quite as straightforward as
#' just saving the object. This also has option for file-backed Rasters.
#'
#' @param sim Either a \code{simList} or a character string of the name
#'        of a \code{simList} that can be found in \code{envir}. Using
#'        a character string will assign that object name to the saved
#'        \code{simList}, so when it is recovered it will be given that
#'        name.
#' @param envir If \code{sim} is a character string, then this must be provided.
#'        It is the environment where the object named \code{sim} can be found.
#'
#' @param filename Character string with the path for saving \code{simList}
#'
#' @param fileBackend Numeric. \code{0} means don't do anything with
#'        file backed rasters. Leave their file intact as is, in its place.
#'        \code{1} means save a copy of the file backed rasters in \code{fileBackedDir}.
#'        \code{2} means move all data in file-backed rasters to memory. This
#'        means that the objects will be part of the main \code{qs} file
#'        of the \code{simList}. Default is \code{0}.
#' @param filebackedDir Only used if \code{fileBackend} is 1.
#'        \code{NULL}, the default, or Character string. If \code{NULL}, then then the
#'        files will be copied to the directory:
#'        \code{file.path(dirname(filename), "rasters")}. A character string
#'        will be interpreted as a path to copy all rasters to.
#' @param ... Passed to \code{save}, e.g., \code{compression}
#'
#' @return A saved \code{.qs} file in \code{filename} location.
#'
#' @export
#' @importFrom qs qsave
#' @importFrom stats runif
#' @rdname saveSimList
#' @seealso \code{\link{zipSimList}}
saveSimList <- function(sim, filename, fileBackend = 0, filebackedDir = NULL, envir, ...) {
  if (is.character(sim)) {
    simName <- sim
    sim <- get(simName, envir = envir)
  }
  if (isTRUE(fileBackend[1] > 0)) {
    sim <- Copy(sim, filebackedDir = filebackedDir, ...)
    if (isTRUE(fileBackend[1] == 2)) {
      sim <- rasterToMemory(sim)
    }
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  sim@.xData$._randomSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  sim@.xData$._rng.kind <- RNGkind()
  if (exists("simName", inherits = FALSE)) {
    tmpEnv <- new.env(parent = emptyenv())
    assign(simName, sim, envir = tmpEnv)
    #save(list = simName, envir = tmpEnv, file = filename)
    qs::qsave(get(simName, envir = tmpEnv), file = filename)
  } else {
    #save(sim, file = filename)
    qs::qsave(sim, file = filename)
  }
  return(invisible())
}

#' Zip many of the files in a \code{simList}
#'
#' Currently, this will save the raster-backed files, \code{outputs(sim)}, \code{inputs(sim)}.
#' It will add these to a temp file, using \code{Copy}, where appropriate
#' to not affect the original \code{simList}.
#' VERY experimental; unlikely to work perfectly at the moment.
#'
#' @param sim A simList at the core of the zipping.
#' @param ... passed to \code{saveSimList}, including important ones such as \code{filename}.
#' @param zipfile A character string indicating the filename for the zip file. Passed to \code{zip}.
#' @param outputs Logical. If \code{TRUE}, all files identified in
#'    \code{outputs(sim)} will be included in the zip.
#' @param inputs Logical. If \code{TRUE}, all files identified in
#'    \code{inputs(sim)} will be included in the zip.
#' @param cache Logical. Not yet implemented. If \code{TRUE}, all files in \code{cachePath(sim)} will be included in the
#'    zip archive. Defaults to \code{FALSE} as this could be large, and may include many
#'    out of date elements. See Details.
#' @export
#'
#' @details
#' If \code{cache} is used, it is likely that it should be trimmed before
#' zipping, to include only cache elements that are relevant.
zipSimList <- function(sim, zipfile, ..., outputs = TRUE, inputs = TRUE, cache = FALSE) {
  dots <- list(...)
  # if (is.null(dots$filename)) dots$filename <- paste0(rndstr(1, 6), ".qs")
  # tmpDir <- file.path(tempdir(), rndstr(1, 6))
  # tmpf <- file.path(tmpDir, basename(dots$filename))
  if (is.null(dots$filebackedDir)) dots$filebackedDir <- paste0("TransferFolder")
  if (is.null(dots$fileBackend)) dots$fileBackend <- 1
  # tmpRasters <- file.path(tmpDir, basename(dots$filebackedDir))
  fnOrig <- Filenames(sim)
  fnOrigSingle <- Filenames(sim, allowMultiple = FALSE)

  rasters <- reproducible:::isOrHasRaster(sim)
  rasterObjNames <- names(rasters)[unlist(lapply(rasters, function(r) any(unlist(r))))]

  sim@.xData$._rasterFilenames <- list(filenames = fnOrig, filenamesSingle = fnOrigSingle,
                                       topLevelObjs = rasterObjNames)
  do.call(saveSimList, append(list(sim), dots))

  tmpf <- dots[["filename"]]
  fbd <- dots[["filebackedDir"]]

  outputFNs <- NULL
  if (isTRUE(outputs)) {
    outputFNs <- outputs(sim)$file
  }
  inputFNs <- NULL
  if (isTRUE(inputs)) {
    inputFNs <- inputs(sim)$file
  }
  # rasterFns <- Filenames(sim, allowMultiple = TRUE)
  # if (all(nchar(rasterFns) == 0))
  #   rasterFns <- NULL

  fbdFns <- if (!is.null(fbd)) {
    dir(fbd, full.names = TRUE, recursive = TRUE)
  } else {
    NULL
  }
  if (file.exists(zipfile)) unlink(zipfile)
  fns <- c(tmpf, # rasterFns,
           fbdFns, outputFNs, inputFNs)
  checkPath(dirname(zipfile), create = TRUE)

  zip(zipfile = zipfile, files = unname(unlist(fns)))
}



#' @export
#' @param zipfile Filename of a zipped simList
#' @details
#' If \code{cache} is used, it is likely that it should be trimmed before
#' zipping, to include only cache elements that are relevant.
unzipSimList <- function(zipfile, load = TRUE, paths = getPaths(), ...) {

  zipfile = normPath(zipfile)
  outFilenames <- unzip(zipfile = zipfile, list = TRUE)

  browser()
  dots <- list(...)
  dots <- modifyList(
    list(exdir = tempdir2(sub = "TransferFolder2"),
         junkpaths = TRUE),
         dots)
  dots <- modifyList(list(zipfile = zipfile),
                     dots)
  checkPath(dots$exdir, create = TRUE)
  unzippedFiles <- do.call(unzip, dots)
  if (is.null(dots$exdir)) {
    on.exit({
      unlink(unzippedFiles, recursive = TRUE, force = TRUE)
      unlink(dots$exdir, recursive = TRUE, force = TRUE)
    })
  }

  if (isTRUE(load)) {
    sim <- loadSimList(file.path(dots$exdir, basename(outFilenames[["Name"]])[[1]]))
    originalPaths <- paths(sim)
    sim@paths <- paths
    fnsSingle <- Filenames(sim, allowMultiple = FALSE)
    newFns <- Filenames(sim)

    fnsObj <- sim@.xData$._rasterFilenames
    origFns <- fnsObj$filenames
    objNames <- fnsObj$topLevelObjs
    objNames <- setNames(objNames, objNames)

    reworkedRas <- lapply(objNames, function(objName) {
      namedObj <- grep(objName, names(origFns), value = TRUE)
      newPath <- dirname(origFns[namedObj])
      names(newPath) <- names(origFns[namedObj])
      dups <- duplicated(newPath)
      if (any(dups))
        newPaths <- newPath[!dups]

      dups2ndLayer <- duplicated(newPaths)
      if (any(dups2ndLayer))
        stop("Cannot unzip and rebuild lists with rasters with multiple different paths; ",
                                 "Please simplify the list of Rasters so they all share a same dirname(Filenames(ras))")
      fns <- Filenames(sim[[objName]], allowMultiple = FALSE)
      currentFname <- unlist(lapply(fns, function(fn) {
        grep(basename(fn),
             unzippedFiles, value = TRUE)
      }))
      currentDir <- unique(dirname(currentFname))
      # nfn <- file.path(newPaths, basename(fns))
      sim[[objName]] <- reproducible:::updateFilenameSlots.Raster(sim[[objName]],
                                                                  newFilenames = currentDir)
      Copy(sim[[objName]], fileBackend = 1, filebackedDir = newPaths)
    })
    list2env(reworkedRas, envir = envir(sim))
    return(sim)
  }
  return(unzippedFiles)
}
