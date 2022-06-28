#' Save a whole \code{simList} object to disk
#'
#' Saving a \code{simList} may not work using the standard approaches (e.g.,
#' `save`, `saveRDS`, and `qs::qsave`). There are 2 primary reasons why this doesn't
#' work as expected: the `activeBindings` that are in place within modules (these
#' allow the `mod` and `Par` to exist), and file-backed rasters. Because of these,
#' a user should use `saveSimList` and `loadSimList` (and the `zipSimList`/`unzipSimList`
#' alternatives).
#' The most robust way if there are file-backed Raster* objects seems to be to
#' set `fileBackend = 2`, though this may not be desirable if there are many
#' large `Raster*` objects. When using `fileBackend = 0` or `fileBackend = 1`, and
#' when errors are noticed, please file a bug report on GitHub.
#'
#' @param sim Either a \code{simList} or a character string of the name
#'        of a \code{simList} that can be found in \code{envir}. Using
#'        a character string will assign that object name to the saved
#'        \code{simList}, so when it is recovered it will be given that
#'        name.
#' @param envir If \code{sim} is a character string, then this must be provided.
#'        It is the environment where the object named \code{sim} can be found.
#'
#' @param filename Character string with the path for saving \code{simList} to or
#'   reading the `simList` from. Currently, only `.rds` and `.qs` filetypes are supported.
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
#' @return
#' [saveSimList()]: A saved \code{.qs} file in \code{filename} location.
#'
#' [zipSimList()]: A saved \code{.zip} file in \code{zipfile} location.
#'
#' @details
#' There is a family of 4 functions that are mutually useful for saving and
#' loading `simList` objects and their associated files (e.g., file-backed
#' `Raster*`, `inputs`, `outputs`, `cache`) [saveSimList()], [loadSimList()],
#' [zipSimList()], [unzipSimList()]
#'
#' @aliases saveSim
#' @export
#' @importFrom qs qsave
#' @importFrom stats runif
#' @importFrom tools file_ext
#' @rdname saveSimList
saveSimList <- function(sim, filename, fileBackend = 0, filebackedDir = NULL, envir, ...) {
  stopifnot(tolower(tools::file_ext(filename)) %in% c("qs", "rds"))

  dots <- list(...)

  quiet <- if (is.null(dots$quiet)) {
    FALSE
  } else {
    if (isTRUE(dots$quiet)) TRUE else FALSE
  }

  # clean up misnamed arguments
  if (!is.null(dots$fileBackedDir)) if (is.null(filebackedDir)) {
    filebackedDir <- dots$fileBackedDir
    dots$fileBackedDir <- NULL
  }
  if (!is.null(dots$filebackend)) if (is.null(fileBackend)) {
    fileBackend <- dots$filebackend
    dots$filebackend <- NULL
  }

  if (is.character(sim)) {
    simName <- sim
    sim <- get(simName, envir = envir)
  }
  if (isTRUE(fileBackend[1] > 0)) {
    mess <- capture.output(type = "message", {
      sim <- do.call(Copy, append(list(sim, filebackedDir = filebackedDir), dots))
    })
    mess <- grep("Hardlinked version", mess, invert = TRUE)
    if (length(mess))
      lapply(mess, message)

    if (isTRUE(fileBackend[1] == 2)) {
      sim <- rasterToMemory(sim)
    }
  }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  sim@.xData$._randomSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  sim@.xData$._rng.kind <- RNGkind()

  if (isFALSE(quiet)) message("Saving simList object to file '", filename, "'.")

  if (exists("simName", inherits = FALSE)) {
    tmpEnv <- new.env(parent = emptyenv())
    assign(simName, sim, envir = tmpEnv)
    if (tolower(tools::file_ext(filename)) == "rds") {
      save(list = simName, envir = tmpEnv, file = filename)
    } else if (tolower(tools::file_ext(filename)) == "qs") {
      qs::qsave(get(simName, envir = tmpEnv), file = filename)
    }
  } else {
    if (tolower(tools::file_ext(filename)) == "rds") {
      save(sim, file = filename)
    } else if (tolower(tools::file_ext(filename)) == "qs") {
      qs::qsave(sim, file = filename)
    }
  }

  if (isFALSE(quiet)) message("    ... saved!")

  return(invisible())
}

#' Zip a `simList` and various files
#'
#' `zipSimList` will save the `simList` and file-backed `Raster*` objects, plus,
#' optionally, files identified in \code{outputs(sim)} and \code{inputs(sim)}.
#' This uses \code{Copy} under the hood, to not affect the original
#' \code{simList}.
#' **VERY experimental**.
#'
#' @param ... passed to \code{\link{saveSimList}}, including non-optional ones
#'    such as \code{filename}. Also see \code{fileBackend} and \code{filebackedDir}
#'    arguments in that function.
#' @param zipfile A character string indicating the filename for the zip file. Passed to \code{zip}.
#' @param outputs Logical. If \code{TRUE}, all files identified in
#'    \code{outputs(sim)} will be included in the zip.
#' @param inputs Logical. If \code{TRUE}, all files identified in
#'    \code{inputs(sim)} will be included in the zip.
#' @param cache Logical. Not yet implemented. If \code{TRUE}, all files in \code{cachePath(sim)} will be included in the
#'    zip archive. Defaults to \code{FALSE} as this could be large, and may include many
#'    out of date elements. See Details.
#' @export
#' @md
#' @rdname saveSimList
#' @seealso [loadSimList()], [unzipSimList()]
#' @details
#' ## Save - Move - Load
#'
#' There are 3 different workflows for "save - move files - load" that work in our tests:
#'
#' 3. `filebackend = 0`: No renaming of file-backed rasters, on recovery attempts to rebuild
#'
#'     This approach is attempting to emulate a "relative filenames" approach,
#'     i.e., attempt to treat the file-backed raster file names as if they were
#'     relative (which they are not -- raster package forces absolute file
#'     paths). To do this, all the renaming occurs within `loadSimList` or
#'     `unzipSimList`. These function will use the `paths` argument to rewrite
#'     the paths of the files that are identified with `Filenames(sim)` so that
#'     they are in the equivalent (relative) position as they were. This will
#'     only work if all files were in one of the `paths` of the original
#'     `simList`, so that they can be matched up with the new `paths` passed in
#'     `loadSimList`. This is not guaranteed to work correctly, though it works
#'     in a wide array of testing.
#'
#'     ```
#'     zipSimList(sim, zipfile = tmpZip, filename = "sim.qs")
#'     pths <- getPaths(mySim)
#'     out <- unzipSimList(tmpZip, paths = pths)
#'     ```
#'
#' 1. `filebackend = 1`: On the fly renaming of file-backed rasters;
#'
#'     1. Save the sim object with a filename, e.g.,  `file`,
#'     2. make a copy of all file-backed rasters to `fileBackedDir`,
#'     3. update all the pointers to those files so that they are correct in the raster metadata
#'
#'     ```
#'     saveSimList(sim, file = "sim.qs", fileBackend = 1, fileBackedDir = "here")
#'     simNew <- loadSimList(file = "sim.qs")
#'     ```
#' 2. `filebackend = 2`: On the fly bringing to memory of all rasters
#'
#'     All rasters are brought to memory, and then saved into \code{sim.qs}
#'
#'     ```
#'     saveSimList(sim, file = "sim.qs", fileBackend = 2)
#'     simNew <- loadSimList(file = "sim.qs")
#'     ```
#'
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

  rasters <- getFromNamespace("isOrHasRaster", ns = "reproducible")(sim)
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

#' Load a saved \code{simList} and ancillary files
#'
#' Loading a `simList` from file can be problematic as there are non-standard
#' objects that must be rebuilt. See description in [saveSimList()] for details.
#'
#' @param filename Character giving the name of a saved simulation file.
#'   Currently, only filetypes `.qs` or `.rds` are supported.
#' @param paths A list of character vectors for all the `simList` paths. When
#'   loading a \code{simList}, this will replace the paths of everything to
#'   these new paths. Experimental still.
#' @param otherFiles A character vector of (absolute) file names locating each of the
#'   existing file-backed `Raster*` files that are the real paths for the possibly
#'   incorrect paths in `Filenames(sim)` if the the `file` being read in is from
#'   a different computer, path, or drive. This could be the output from `unzipSimList`
#'   (which is calls `loadSimList` internally, passing the unzipped filenames)
#'
#' @return
#'
#' * [loadSimList()]: A `simList` object.
#' * [unzipSimList()]: Either a character vector of file names unzipped (if `load = FALSE`),
#'   or a `simList` object.
#'
#' @export
#' @rdname loadSimList
#' @seealso [saveSimList()], [zipSimList()]
#' @importFrom qs qread
#' @importFrom reproducible updateFilenameSlots
#' @importFrom tools file_ext
loadSimList <- function(filename, paths = getPaths(), otherFiles = "") {
  stopifnot(tolower(tools::file_ext(filename)) %in% c("qs", "rds"))

  if (tolower(tools::file_ext(filename)) == "rds") {
    load(filename)
  } else if (tolower(tools::file_ext(filename)) == "qs") {
    sim <- qs::qread(filename, nthreads = getOption("spades.nThreads", 1))

    # Work around for bug in qs that recovers data.tables as lists
    objectName <- ls(sim)
    names(objectName) <- objectName
    objectClassInSim <- lapply(objectName, function(x) is(get(x, envir = sim))[1])
    dt <- data.table(objectName, objectClassInSim)

    io <- inputObjects(sim)
    oo <- outputObjects(sim)
    if (is(io, "list")) io <- rbindlist(io, fill = TRUE)
    if (is(oo, "list")) oo <- rbindlist(oo, fill = TRUE)
    objs <- rbindlist(list(io, oo), fill = TRUE)
    objs <- unique(objs, by = "objectName")[, c("objectName", "objectClass")]

    objs <- objs[dt, on = "objectName"]
    objs <- objs[objectClass == "data.table"]
    objs <- objs[objectClass != objectClassInSim]
    if (NROW(objs)) {
      message("There is a bug in qs package that recovers data.table objects incorrectly when in a list")
      message("Converting all known data.table objects (according to metadata) from list to data.table")
      simEnv <- envir(sim)
      out <- lapply(objs$objectName, function(on) {
        tryCatch(assign(on, copy(as.data.table(sim[[on]])), envir = simEnv),
                 error = function(e) warning(e))
      })
    }
  }

  mods <- setdiff(sim@modules, .coreModules())
  ## TODO: this should be unnecessary after June 2020 R-devel fix for active bindings
  lapply(mods, function(mod) {
    if (!is.null(sim$.mods[[mod]]))
      rm("mod", envir = sim$.mods[[mod]], inherits = FALSE)
    makeModActiveBinding(sim = sim, mod = mod)
  })

  # Deal with all the RasterBacked Files that will be wrong
  if (any(nchar(otherFiles) > 0)) {
    pathsInOldSim <- paths(sim)
    sim@paths <- paths
    fnsSingle <- Filenames(sim, allowMultiple = FALSE)
    newFns <- Filenames(sim)

    fnsObj <- sim@.xData$._rasterFilenames
    origFns <- normPath(fnsObj$filenames)
    objNames <- fnsObj$topLevelObjs
    objNames <- setNames(objNames, objNames)

    newFns <- vapply(origFns, function(fn) {
      fnParts <- strsplit(fn, split = "\\/")[[1]]
      relParts <- vapply(fnParts, grepl, x = unlist(pathsInOldSim),
                         logical(length(pathsInOldSim))) # 5 paths components
      whRel <- which(apply(relParts, 2, sum) == 0)
      whAbs <- whRel[1] - 1
      whAbs <- which.max(apply(relParts, 1, sum))
      # use new paths as base for newFns
      newPath <- file.path(paths[[whAbs]], fnParts[whRel[1]], basename(fn))
    }, character(1))

    reworkedRas <- lapply(objNames, function(objName) {
      namedObj <- grep(objName, names(newFns), value = TRUE)
      newPaths <- dirname(newFns[namedObj])
      names(newPaths) <- names(newFns[namedObj])
      dups <- duplicated(newPaths)
      if (any(dups)) {
        newPaths <- newPaths[!dups]
      }

      dups2ndLayer <- duplicated(newPaths)
      if (any(dups2ndLayer)) {
        stop("Cannot unzip and rebuild lists with rasters with multiple different paths; ",
             "Please simplify the list of Rasters so they all share a same dirname(Filenames(ras))")
      }

      # These won't exist because they are the filenames from the old
      #   (possibly temporary following saveSimList) simList
      fns <- Filenames(sim[[objName]], allowMultiple = FALSE)

      # Now match them with the files that exist from unzipping
      currentFname <- unlist(lapply(fns, function(fn) {
        grep(basename(fn),
             otherFiles, value = TRUE)
      }))
      currentDir <- unique(dirname(currentFname))

      # First must update the filename slots so that they point to real files (in the exdir)
      sim[[objName]] <- updateFilenameSlots(sim[[objName]],
                                                           newFilenames = currentDir)
      mess <- capture.output(type = "message", {
        sim[[objName]] <- (Copy(sim[[objName]], fileBackend = 1, filebackedDir = newPaths))
      })
      mess <- grep("Hardlinked version", mess, invert = TRUE)
      if (length(mess))
        lapply(mess, message)
      return(sim[[objName]])
    })

    list2env(reworkedRas, envir = envir(sim))
  }

  return(sim)
}


#' `unzipSimList` will unzip a zipped `simList`
#'
#' `unzipSimList` is a convenience wrapper around `unzip` and `loadSimList` where
#' all the files are correctly identified and passed to
#' `loadSimList(..., otherFiles = xxx)`. See [zipSimList] for details.
#'
#' @export
#' @param zipfile Filename of a zipped simList
#' @param load Logical. If \code{TRUE}, the default, then the simList will
#'   also be loaded into R.
#' @param ... passed to `unzip`
#' @rdname loadSimList
#' @details
#' If \code{cache} is used, it is likely that it should be trimmed before
#' zipping, to include only cache elements that are relevant.
unzipSimList <- function(zipfile, load = TRUE, paths = getPaths(), ...) {
  zipfile <- normPath(zipfile)
  outFilenames <- unzip(zipfile = zipfile, list = TRUE)

  dots <- list(...)
  dots <- modifyList2(
    list(exdir = tempdir2(sub = "TransferFolder2"),
         junkpaths = TRUE),
    dots)
  dots <- modifyList2(list(zipfile = zipfile),
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
    sim <- loadSimList(file.path(dots$exdir, basename(outFilenames[["Name"]])[[1]]),
                       paths = paths,
                       otherFiles = unzippedFiles)
    return(sim)
  }
  return(unzippedFiles)
}
