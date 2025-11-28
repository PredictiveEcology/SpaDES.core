utils::globalVariables(c("attached", "fun", "package", "saved", "saveTime"))

## Just checks for paths, creates them if they do not exist
doEvent.save <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    if (NROW(outputs(sim)) > 0) {
      firstSave <- min(outputs(sim)[["saveTime"]], na.rm = TRUE)
      firstSaveWh <- which.min(outputs(sim)[["saveTime"]])
      if ("eventPriority" %in% colnames(outputs(sim))) {
        firstPriority <- outputs(sim)[["eventPriority"]][firstSaveWh]
      }
      if (!exists("firstPriority", inherits = FALSE)) {
        firstPriority <- .last()
      }
      attributes(firstSave)$unit <- sim@simtimes[["timeunit"]]
      sim <- scheduleEvent(sim, firstSave, "save", "spades", firstPriority)
      # sim <- scheduleEvent(sim, end(sim, sim@simtimes[["timeunit"]]), "save", "end", .last())
    }
    checkPath(sim@paths$outputPath, create = TRUE)
  } else if (eventType == "spades") {
    sim <- saveFiles(sim)
  } else if (eventType == "later") {
    sim <- saveFiles(sim)
  } else if (eventType == "end") {
    sim <- saveFiles(sim)
    message(cli::col_green(paste0("Files saved. Use outputs(your simList) for details")), sep = "")
  }

  return(invisible(sim))
}

##############################################################
#' Save objects using `.saveObjects` in `params` slot of `simInit`
#'
#' In the [simInit()] call, a parameter called `.saveObjects` can be provided in
#' each module.
#' This must be a character string vector of all object names to save. These objects will
#' then be saved whenever a call to `saveFiles` is made.
#'
#' The file names will be equal to the object name plus `time(sim)` is
#' appended at the end.
#' The files are saved as `.rds` files, meaning, only one object gets
#' saved per file.
#'
#' For objects saved using this function, the module developer must create save
#' events that schedule a call to `saveFiles`.
#'
#' If this function is used outside of a module, it will save all files in the
#' `outputs(sim)` that are scheduled to be saved at the current time in the `simList.`
#'
#' There are several ways to save objects using `SpaDES`.
#'
#' @section Model-level saving:
#'
#' Using the `outputs` slot in the [simInit()] call.
#' See example in [simInit()].
#' This can be convenient because it gives overall control of many modules at a
#' time, and it gets automatically scheduled during the [simInit()] call.
#'
#' @section Module-level saving:
#'
#' Using the `saveFiles` function inside a module.
#' This must be accompanied by a `.saveObjects` vector or list element in the
#' `params` slot in the [simList()].
#' Usually a module developer will create this method for future users of their module.
#'
#' @section Custom saving:
#'
#' A module developer can save any object at any time inside their module, using
#' standard R functions for saving R objects (e.g., `save` or `saveRDS`).
#' This is the least modular approach, as it will happen whether a module user
#' wants it or not.
#'
#' @note It is not possible to schedule separate saving events for each object
#' that is listed in the `.saveObjects`.
#'
#' @param sim A `simList` simulation object.
#'
#' @return (invisibly) the modified `sim` object.
#'         invoked for side effect of saving the simulation to file.
#'
#' @export
#' @author Eliot McIntire and Alex Chubaty
#' @importFrom data.table data.table
#' @rdname saveFiles
#'
#' @examples
#' \donttest{
#' if (requireNamespace("SpaDES.tools", quietly = TRUE) &&
#'     requireNamespace("NLMR", quietly = TRUE)) {
#' ## This will save the "caribou" object at the save interval of 1 unit of time
#' ## in the outputPath location
#'   outputPath <- file.path(tempdir(), "test_save")
#'   times <- list(start = 0, end = 1, "month")
#'
#'   modules <- list("randomLandscapes", "caribouMovement")
#'   paths <- list(
#'     modulePath = getSampleModules(tempdir()),
#'     outputPath = outputPath
#'   )
#'  opts <- options("spades.moduleCodeChecks" = FALSE,
#'                  "spades.useRequire" = FALSE) # not necessary for example
#'
#'   ## save multiple outputs
#'   parameters <- list(
#'     .globals = list(stackName = "landscape"),
#'     caribouMovement = list(
#'       .saveObjects = c("caribou", "habitatQuality"),
#'       .saveInitialTime = 1, .saveInterval = 1
#'     ),
#'     randomLandscapes = list(.plots = NA, nx = 20, ny = 20))
#'
#'   mySim <- simInit(times = times, params = parameters, modules = modules,
#'                  paths = paths)
#'
#'   spades(mySim, .plotInitialTime = NA) # plotting not relevant for this example
#'   dir(outputPath)
#'   # remove the files
#'   file.remove(dir(outputPath, full.names = TRUE))
#'
#'   options(opts) # clean up
#' }}
saveFiles <- function(sim) {
  curTime <- time(sim, sim@simtimes[["timeunit"]])
  ## extract the current module name that called this function
  moduleName <- sim@current[["moduleName"]]

  if (length(moduleName) == 0) {
    moduleName <- "save"
    if (NROW(outputs(sim)[outputs(sim)$saveTime == curTime, ])) {
      outputs(sim)[["saved"]][outputs(sim)$saveTime == curTime] <- NA
    }
  }

  if (moduleName != "save") {
    ## i.e., a module driven save event
    toSave <- lapply(params(sim), function(y) return(y$.saveObjects))[[moduleName]] |>
      unlist() |>
      (function(x) data.table(objectName = x, saveTime = curTime, file = x, stringsAsFactors = FALSE))() |>
      as.data.frame()
    toSave <- .fillOutputRows(toSave)
    outputs(sim) <- rbind(outputs(sim), toSave)

    ## don't need to save exactly same thing more than once - use data.table here because distinct
    outputs(sim) <- data.table(outputs(sim)) |>
      unique(by = c("objectName", "saveTime", "file", "fun", "package")) |>
      data.frame()
  }

  if (NROW(outputs(sim)[["saved"]][outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved)]) > 0) {
    wh <- which(outputs(sim)$saveTime == curTime & is.na(outputs(sim)$saved))
    for (i in wh) {
      objExists <- exists(outputs(sim)[["objectName"]][i], envir = sim@.xData)
      isSimList <- identical(outputs(sim)[["objectName"]][i], "sim")
      if (objExists || isSimList) {
        if (objExists) {
          args <- append(list(get(outputs(sim)[["objectName"]][i], envir = sim@.xData),
                              file = outputs(sim)[["file"]][i]),
                         outputArgs(sim)[[i]])
          args <- args[!sapply(args, is.null)]
          args <- suppressWarnings(args[!unlist(lapply(args, function(j) {
            isTRUE(tryCatch(is.na(j), error = function(e) FALSE))
          }))])

          ## The actual save line
          do.call(outputs(sim)[["fun"]][i], args = args,
                  envir = getNamespace(outputs(sim)[["package"]][i]))

          ## using @ works when outputs is a DT
        } else {
          saveSimList(sim, filename = outputs(sim)[["file"]][i])
        }
        # the next line, if using the accessor outputs(sim) <-,  modifies any filenames that don't have names;
        #   the i indexing doesn't work here as expected; use direct
        sim@outputs[["saved"]][i] <- TRUE
        # outputs(sim)[["saved"]][i] <- TRUE
      } else {
        warning(paste(outputs(sim)[["objectName"]][i], "is not an object in the simList. Cannot save."))
        sim@outputs[["saved"]][i] <- TRUE # see a few lines above for comment about this
        # outputs(sim)[["saved"]][i] <- FALSE
      }
    }
  }

  ## Schedule an event for the next time in the saveTime column
  if (any(is.na(outputs(sim)[["saved"]][outputs(sim)$saveTime > curTime]))) {
    isNA <- is.na(outputs(sim)$saved)
    nextTime <- min(outputs(sim)[["saveTime"]][isNA], na.rm = TRUE)
    nextTimeWh <- which.min(outputs(sim)[["saveTime"]][isNA])
    if ("eventPriority" %in% colnames(outputs(sim))) {
      nextPriority <- outputs(sim)[["eventPriority"]][isNA][nextTimeWh]
    }
    if (!exists("nextPriority", inherits = FALSE)) {
      nextPriority <- .last()
    }

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
#' A `data.frame` with information on how to load various types of files in R,
#' containing the columns:
#' \itemize{
#'   \item `exts`: the file extension;
#'   \item `fun`: the function to use for files with this file extension;
#'   \item `package`: the package from which to load `fun`.
#' }
#'
#' @return `data.frame`
#'
#' @export
#' @importFrom data.table setnames
#' @rdname loadFiles
.saveFileExtensions <- function() {
  ## TODO: try to guess other file types -- see .guessPkgFun
  # fmt: skip
  .sFE <- data.frame(matrix(ncol = 3, byrow = TRUE, c(
    "rds", "saveRDS", "base",
    "qs", "qsave", "qs",
    "qs2", "qs_save", "qs2",
    "txt", "write.table", "utils",
    "csv", "write.csv", "utils",
    "grd", "writeRaster", "raster",
    "shp", "writeVector", "terra",
    "tif", "writeRaster", "terra"
  )), stringsAsFactors = FALSE)
  setnames(.sFE, new = c("exts", "fun", "package"), old = paste0("X", 1:3))
  .sFE <- .sFE[order(.sFE$package, .sFE$fun), ]
  if (NROW(getOption("spades.saveFileExtensions")) > 0) {
    if (!identical(colnames(.sFE), colnames(getOption("spades.saveFileExtensions")))) {
      stop("The column names of `getOption('spades.saveFileExtensions') must be: ",
           paste(colnames(.sFE), collapse = ", "), "; they are currently ",
           paste(collapse = ", ", colnames(getOption("spades.saveFileExtensions"))))
    }
    ## remove initial dot
    .sFE <- rbind(getOption("spades.saveFileExtensions"), .sFE)
    .sFE[["exts"]] <- gsub("^\\.", "", .sFE[["exts"]])
    ## remove if there are 2 extensions for same fun and package
    dups <- duplicated(.sFE[, c("fun", "package")])
    .sFE <- .sFE[!dups, ]
  }
  return(.sFE)
}

#' Generate simulation file name
#'
#' Assists with saving and retrieving simulations (e.g., with `saveSimList` and `loadSimList`).
#'
#' @param name Object name (e.g., `"mySimOut"`)
#' @param path Directory location in where the file will be located (e.g., an `outputPath`).
#' @param time Optional simulation time to use as filename suffix. Default `NULL`.
#' @param ext  The file extension to use (default `"rds"`).
#'
#' @return character string giving a file path for a simulation file
#'
#' @export
#' @importFrom reproducible normPath
simFile <- function(name, path, time = NULL, ext = "rds") {
  if (is.null(time)) {
    file.path(normPath(path), paste0(name, ".", ext))
  } else {
    file.path(normPath(path), paste0(name, "_", paddedFloatToChar(time, padL = 4), ".", ext))
  }
}
