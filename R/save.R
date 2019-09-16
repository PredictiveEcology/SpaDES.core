if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("saved", "saveTime", "fun", "package"))
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
    simFilename <- "~/sim.Rdata"
    saveSimList(sim, simFilename, fileBackendToMem = FALSE, filebackedDir = NULL)
    sim <- scheduleEvent(sim, time(sim, timeunit(sim)) + getOption("spades.restartRInterval"), "restartR", "restartR", .last())
    on.exit({
      restartR(reloadPkgs = TRUE, .First = NULL, .RdataFile = simFilename)
    })
    stop("Restarting R")
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
#' @rdname loadFiles
#' @seealso zipSimList
#'
saveSimList <- function(sim, filename, fileBackendToMem = TRUE, filebackedDir = NULL, ...) {
  isRaster <- unlist(lapply(sim@.xData, function(x) is(x, "Raster")))
  if (any(isRaster)) {
    InMem <- unlist(lapply(mget(names(isRaster)[isRaster], envir = envir(sim)), function(x) inMemory(x)))
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
  save(sim, file = filename, ...)
}

#' Zip many of the files in a simList
#'
#' Currently, this will save the raster-backed files, outputs(sim), inputs(sim).
#' It will add these to a temp file, using \code{Copy}, where appropriate
#' to not affect the original \code{simList}.
#'
#' @inheritParams saveSimList
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
  if (is.null(dots$filename)) dots$filename <- paste0(rndstr(1, 6), ".Rdata")
  tmpDir <- file.path(tempdir(), rndstr(1, 6))
  tmpRdata <- file.path(tmpDir, basename(dots$filename))
  if (is.null(dots$filebackedDir)) dots$filebackedDir <- paste0("rasters")
  if (is.null(dots$fileBackendToMem)) dots$fileBackendToMem <- formals(saveSimList)$fileBackendToMem
  tmpRasters <- file.path(tmpDir, basename(dots$filebackedDir))
  saveSimList(sim, filename = tmpRdata, filebackedDir = tmpRasters, fileBackendToMem = dots$fileBackendToMem)

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
  zip(zipfile = zipfile, files = c(tmpRdata, dir(tmpRasters, full.names = TRUE, recursive = TRUE),
                                   newnamesOutputs, newnamesInputs))


}

#' Restart R programmatically
#'
#' This will attempt to restart the R session, reloading all packages.
#' The main purpose for doing this is to clear memory leaks that are not
#' yet diagnosed.
#'
#' @export
#' @param reloadPkgs Logical. If \code{TRUE}, it will attempt to reload all the packages
#'   as they were in previous session, in the same order. If \code{FALSE}, it will
#'   load no packages beyond normal R startup. Default \code{TRUE}
#'
restartR <- function(reloadPkgs = TRUE, .First = NULL, .RdataFile = ".toLoad.RData") {
  isRStudio <- Sys.getenv("RSTUDIO") == "1"

  vanillaPkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
                   "package:grDevices", "package:utils", "package:datasets", "package:methods",
                   "Autoloads", "package:base", "devtools_shims")
  srch <- search()
  attached <- srch
  attached <- grep("package:", attached, value = TRUE)
  attached <- unlist(lapply(attached, function(x) gsub(x, pattern = 'package:', replacement = '')))
  save(file = file.path('~', '.toLoad.Rdata'), attached)
  if (is.null(.First)) {
    .First <- SpaDES.core:::.First
  }

  if (isTRUE(reloadPkgs))
    save(file = ".RData", .First)
  #   cat("x <- 1:10", file = file.path("~", ".resume.R"))
  #   cat("
  # #$#$#$#$#$#$#$#$#$#$#$
  # .First <- function() {
  # try(source(file.path('~', '.resume.R')))
  # load(file.path('~', '.toLoad.Rdata'))
  #
  # options('defaultPackages' = attached)
  # lapply(rev(attached), function(x) require(x, character.only = TRUE))
  # }
  # ", file = file.path("~", ".Rprofile"),
  #       append = TRUE)

  if (isTRUE(isRStudio)) {
    if (requireNamespace("rstudioapi")) {
      lapply(setdiff(srch, vanillaPkgs), function(pkg) detach(pkg, character.only = TRUE, unload = TRUE, force = TRUE))
      rstudioapi::restartSession(if (reloadPkgs) "{load('.RData'); .First()}" else "")
    } else {
      message("Running RStudio. To restart it this way, you must run: install.packages('rstudioapi')")
    }
  } else {
    .Last <<- function() system("R --no-save")
    q("no")
  }

}

.First <- function() {
  try(source(file.path('~', '.resume.R')))
  load(file.path('~', 'sim.Rdata'), envir = .GlobalEnv)
  load(file.path('~', '.toLoad.Rdata')) # for "attached" object
  lapply(rev(attached), function(x) require(x, character.only = TRUE))
  unlink('.RData');
  rm(.First, envir = .GlobalEnv)
}
