#' Save a whole `simList` object to disk
#'
#' Saving a `simList` may not work using the standard approaches
#' (e.g., `save`, `saveRDS`, and `qs::qsave`).
#' There are 2 primary reasons why this doesn't work as expected:
#' the `activeBindings` that are in place within modules
#' (these allow the `mod` and `Par` to exist), and file-backed objects,
#' such as `SpatRaster` and `Raster*`.
#' Because of these, a user should use `saveSimList` and `loadSimList`.
#' These will save the object and recover the object using the `filename` supplied,
#' if there are no file-backed objects.
#' If there are file-backed objects, then it will save an archive
#' (default is `.tar.gz` using the `archive` package for non-Windows and [zip()]
#' if using Windows, as there is currently an unidentified bug in `archive*` on Windows).
#' The user does not need to specify the filename any differently,
#' as the code will search based on the filename without the file extension.
#'
#' @details
#' There is a family of 2 functions that are mutually useful for saving and
#' loading `simList` objects and their associated files (e.g., file-backed
#' `Raster*`, `inputs`, `outputs`, `cache`) [saveSimList()], [loadSimList()].
#'
#' @param sim Either a `simList` or a character string of the name
#'        of a `simList` that can be found in `envir`.
#'        Using a character string will assign that object name to the saved
#'        `simList`, so when it is recovered it will be given that name.
#'
#' @param envir If `sim` is a character string, then this must be provided.
#'        It is the environment where the object named `sim` can be found.
#'
#' @param filename Character string with the path for saving `simList` to or
#'   reading the `simList` from. Currently, only `.rds` and `.qs` file types are supported.
#'
#' @param outputs Logical. If `TRUE`, all files identified in
#'    `outputs(sim)` will be included in the zip.
#'
#' @param inputs Logical. If `TRUE`, all files identified in
#'    `inputs(sim)` will be included in the zip.
#'
#' @param cache Logical. Not yet implemented. If `TRUE`, all files in `cachePath(sim)`
#'    will be included in the archive.
#'    Defaults to `FALSE` as this could be large, and may include many out of date elements.
#'    See Details.
#'
#' @param projectPath Should be the "top level" or project path for the `simList`.
#'    Defaults to `getwd()`. All other paths will be made relative with respect to
#'    this if nested within this.
#'
#' @param ... Not used.
#'
#' @return
#' Invoked for side effects of saving a `.qs` or `.rds` file,
#' or a `.tar.gz` (non-Windows) or `.zip` (Windows).
#'
#' @aliases saveSim
#' @export
#' @importFrom qs qsave
#' @importFrom stats runif
#' @importFrom Require messageVerbose
#' @importFrom tools file_ext
#' @rdname saveSimList
saveSimList <- function(sim, filename, projectPath = getwd(),
                        outputs = TRUE, inputs = TRUE, cache = FALSE, envir,
                        ...) {

  checkSimListExts(filename)

  dots <- list(...)

  verbose <- if (is.null(dots$verbose)) {
    if (is.null(dots$quiet)) {
      getOption("reproducible.verbose")
    } else {
      !isTRUE(dots$quiet)
    }
  } else {
    isTRUE(dots$verbose)
  }

  # clean up misnamed arguments
  if (!is.null(dots$fileBackedDir)) {
    if (is.null(filebackedDir)) {
      filebackedDir <- dots$fileBackedDir
      dots$fileBackedDir <- NULL
    }
  }
  if (!is.null(dots$filebackend))
    if (is.null(dots$fileBackend)) {
      dots$fileBackend <- dots$filebackend
      dots$filebackend <- NULL
    }

  if (!is.null(dots$fileBackend)) {
    warning(warnDeprecFileBacked("fileBackend"))
    fileBackend <- 0
  }

  if (!is.null(dots$filebackedDir)) {
    warning(warnDeprecFileBacked("filebackedDir"))
    fileBackend <- 0
  }


  if (is.character(sim)) {
    simName <- sim
    sim <- get(simName, envir = envir)
  }

  # if (isTRUE(fileBackend[1] == 2)) {
  #   sim <- rasterToMemory(sim)
  # } else if (isTRUE(fileBackend[1] == 1)) {
  #   mess <- capture.output(type = "message", {
  #     sim <- do.call(Copy, append(list(sim, filebackedDir = filebackedDir), dots))
  #   })
  #   mess <- grep("Hardlinked version", mess, invert = TRUE)
  #   if (length(mess))
  #     lapply(mess, message)
  #
  # }
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  sim@.xData$._randomSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  sim@.xData$._rng.kind <- RNGkind()

  messageVerbose("Saving simList object to file '", filename, "'.", verbose = verbose)

  if (exists("simName", inherits = FALSE)) {
    tmpEnv <- new.env(parent = emptyenv())
    assign(simName, sim, envir = tmpEnv)
    sim <- get(simName, envir = tmpEnv)
  }
  # if (tolower(tools::file_ext(filename)) == "rds") {
  #   save(list = simName, envir = tmpEnv, file = filename)
  # } else if (tolower(tools::file_ext(filename)) == "qs") {
  #   qs::qsave(get(simName, envir = tmpEnv), file = filename)
  # }
  # } else {
  fns <- Filenames(sim)
  sim <- .wrap(sim, cachePath = projectPath) # makes a copy of filebacked object files
  sim@current <- list() # it is presumed that this event should be considered finished prior to saving
  empties <- nchar(fns) == 0
  if (any(empties)) {
    fns <- fns[!empties]
    fnsInSubFolders <- grepl(checkPath(dirname(filename)), fns)
  }

  ## This forces it to be qs  (if not rds) instead of zip or tar.gz
  if (tools::file_ext(filename) != "rds") {
    filename <- archiveConvertFileExt(filename, "qs")
  }

  paths(sim) <- as.list(relativizePaths(paths(sim)))

  # filename <- gsub(tools::file_ext(filename), "qs", filename)
  if (tolower(tools::file_ext(filename)) == "rds") {
    saveRDS(sim, file = filename)
  } else if (tolower(tools::file_ext(filename)) == "qs") {
    filename <- gsub(tools::file_ext(filename), "qs", filename)
    qs::qsave(sim, file = filename, nthreads = getOption("spades.qsThreads", 1))
  }

  srcFiles <- mapply(mod = modules(sim), mp = modulePath(sim),
                 function(mod, mp) {
                   files <- dir(file.path(mp, mod), recursive = TRUE, full.names = TRUE)
                   files <- grep("^\\<data\\>", invert = TRUE, value = TRUE, files)
                 })
  srcFilesRel <- makeRelative(srcFiles, absoluteBase = projectPath)
  if (any(isAbsolutePath(srcFilesRel))) {# means not inside the projectPath
    # try modulePath first
    srcFilesRel <- makeRelative(srcFiles, absoluteBase = dirname(modulePath(sim)))
    tmpSrcFiles <- file.path(projectPath, srcFilesRel)
    linkOrCopy(srcFiles, tmpSrcFiles, verbose = verbose - 1)
    on.exit(unlink(tmpSrcFiles))
    srcFiles <- tmpSrcFiles
  }

  if (length(fns)) {
    fileToDelete <- filename

    otherFns <- c()
    if (isTRUE(outputs)) {
      os <- outputs(sim)
      if (NROW(os)) {
        outputFNs <- os[os$saved %in% TRUE]$file
        otherFns <- c(otherFns, outputFNs)
      }
    }
    inputFNs <- NULL
    if (isTRUE(inputs)) {
      ins <- inputs(sim)
      if (NROW(ins)) {
        ins[ins$loaded %in% TRUE]$file
        otherFns <- c(otherFns, inputFNs)
      }
    }

    allFns <- c(fns, otherFns, srcFilesRel)

    relFns <- makeRelative(c(fileToDelete, allFns), absoluteBase = projectPath)

    archiveWrite(filename, relFns, verbose)

    unlink(fileToDelete)
  }

  messageVerbose("    ... saved!", verbose = verbose)

  return(invisible())
}

#' Zip a `simList` and various files
#'
#' `zipSimList` will save the `simList` and file-backed `Raster*` objects, plus,
#' optionally, files identified in `outputs(sim)` and `inputs(sim)`.
#' This uses `Copy` under the hood, to not affect the original `simList`.
#' **VERY experimental**.
#'
#' @param zipfile A character string indicating the filename for the zip file. Passed to `zip`.
#'
#' @export
#' @md
#' @rdname saveSimList
#' @seealso [loadSimList()]
zipSimList <- function(sim, zipfile, ..., outputs = TRUE, inputs = TRUE, cache = FALSE) {
  .Deprecated("saveSimList")
  saveSimList(sim, filename = zipfile)
  # dots <- list(...)
  # # if (is.null(dots$filename)) dots$filename <- paste0(rndstr(1, 6), ".qs")
  # # tmpDir <- file.path(tempdir(), rndstr(1, 6))
  # # tmpf <- file.path(tmpDir, basename(dots$filename))
  # if (is.null(dots$filebackedDir)) dots$filebackedDir <- paste0("TransferFolder")
  # if (is.null(dots$fileBackend)) dots$fileBackend <- 1
  # # tmpRasters <- file.path(tmpDir, basename(dots$filebackedDir))
  # fnOrig <- Filenames(sim)
  # fnOrigSingle <- Filenames(sim, allowMultiple = FALSE)
  #
  # rasters <- getFromNamespace("isOrHasRaster", ns = "reproducible")(sim)
  # rasterObjNames <- names(rasters)[unlist(lapply(rasters, function(r) any(unlist(r))))]
  #
  # sim@.xData$._rasterFilenames <- list(filenames = fnOrig, filenamesSingle = fnOrigSingle,
  #                                      topLevelObjs = rasterObjNames)
  # do.call(saveSimList, append(list(sim), dots))
  #
  # tmpf <- dots[["filename"]]
  # fbd <- dots[["filebackedDir"]]
  #
  # outputFNs <- NULL
  # if (isTRUE(outputs)) {
  #   outputFNs <- outputs(sim)$file
  # }
  # inputFNs <- NULL
  # if (isTRUE(inputs)) {
  #   inputFNs <- inputs(sim)$file
  # }
  # # rasterFns <- Filenames(sim, allowMultiple = TRUE)
  # # if (all(nchar(rasterFns) == 0))
  # #   rasterFns <- NULL
  #
  # fbdFns <- if (!is.null(fbd)) {
  #   dir(fbd, full.names = TRUE, recursive = TRUE)
  # } else {
  #   NULL
  # }
  # if (file.exists(zipfile)) unlink(zipfile)
  # fns <- c(tmpf, # rasterFns,
  #          fbdFns, outputFNs, inputFNs)
  # checkPath(dirname(zipfile), create = TRUE)
  #
  # zip(zipfile = zipfile, files = unname(unlist(fns)))
}

#' Load a saved `simList` and ancillary files
#'
#' Loading a `simList` from file can be problematic as there are non-standard
#' objects that must be rebuilt. See description in [saveSimList()] for details.
#'
#' @param filename Character giving the name of a saved simulation file.
#'   Currently, only file types `.qs` or `.rds` are supported.
#' @param projectPath An optional path for the project within which the `simList`
#'   exists. This is used to identify relative paths for saving and loading the `simList`.
#' @param paths A list of character vectors for all the `simList` paths. When
#'   loading a `simList`, this will replace the paths of everything to
#'   these new paths. Experimental still.
#' @param otherFiles A character vector of (absolute) file names locating each of the
#'   existing file-backed `Raster*` files that are the real paths for the possibly
#'   incorrect paths in `Filenames(sim)` if the the `file` being read in is from
#'   a different computer, path, or drive. This could be the output from `unzipSimList`
#'   (which is calls `loadSimList` internally, passing the unzipped filenames)
#' @param tempPath A character string specifying the new base directory for the
#'   temporary paths maintained in a `simList`.
#' @inheritParams reproducible::Cache
#'
#' @return For [loadSimList()], a `simList` object.
#'         For [unzipSimList()], either a character vector of file names unzipped
#'         (if `load = FALSE`), or a `simList` object.
#'
#' @export
#' @rdname loadSimList
#' @seealso [saveSimList()], [zipSimList()]
#' @importFrom qs qread
#' @importFrom reproducible updateFilenameSlots linkOrCopy
#' @importFrom tools file_ext
loadSimList <- function(filename, projectPath = getwd(), tempPath = tempdir(),
                        paths = NULL, otherFiles = "",
                        verbose = getOption("reproducible.verbose")) {
  checkSimListExts(filename)

  filename <- checkArchiveAlternative(filename)

  if (grepl(archiveExts, tolower(tools::file_ext(filename)))) {
    td <- tempdir2(sub = .rndstr())
    filename <- archiveExtract(filename, exdir = td)
    filenameRel <- gsub(paste0(td, "/"), "", filename[-1])

    # This will put the files to relative path of getwd()
    newFns <- file.path(projectPath, filenameRel)
    linkOrCopy(filename[-1], newFns, verbose = verbose - 1)
  } else {
    filenameRel <- gsub(paste0(projectPath, "/"), "", filename)
  }

  if (tolower(tools::file_ext(filename[1])) == "rds") {
    sim <- readRDS(filename[1])
  } else if (tolower(tools::file_ext(filename[1])) == "qs") {
    sim <- qs::qread(filename[1], nthreads = getOption("spades.qsThreads", 1))
  }
  paths(sim) <- absolutizePaths(paths(sim), projectPath, tempPath)
  if (!is.null(paths)) {
    paths <- lapply(paths, normPath)
  } else {
    paths <- list()
  }
  paths(sim) <- modifyList2(paths(sim), paths)

  sim <- .unwrap(sim, cachePath = projectPath) # convert e.g., PackedSpatRaster

  # Work around for bug in qs that recovers data.tables as lists
  sim <- recoverDataTableFromQs(sim)

  mods <- setdiff(sim@modules, .coreModules())

  # Deal with all the RasterBacked Files that will be wrong
  if (any(nchar(otherFiles) > 0)) {
    .dealWithRasterBackends(sim) # no need to assign to sim b/c uses list2env
  }

  return(sim)
}


#' `unzipSimList` will unzip a zipped `simList`
#'
#' `unzipSimList` is a convenience wrapper around `unzip` and `loadSimList` where
#' all the files are correctly identified and passed to
#' `loadSimList(..., otherFiles = xxx)`. See [zipSimList] for details.
#'
#' @details
#' If `cache` is used, it is likely that it should be trimmed before
#' zipping, to include only cache elements that are relevant.
#'
#' @param zipfile Filename of a zipped `simList`
#' @param load Logical. If `TRUE`, the default, then the `simList` will
#'   also be loaded into R.
#' @param ... passed to `unzip`
#'
#' @export
#' @rdname loadSimList
unzipSimList <- function(zipfile, load = TRUE, paths = getPaths(), ...) {
  .Deprecated("loadSimList")
  sim <- loadSimList(zipfile, ...)
  return(sim)

}

checkArchiveAlternative <- function(filename) {
  if (!file.exists(filename[1])) {
    baseN <- tools::file_path_sans_ext(basename(filename))
    possZips <- dir(dirname(filename), pattern = paste0(baseN, ".", archiveExts),
                    full.names = TRUE)
    if (length(possZips)) {
      filename <- possZips[1]
    }

  }
  filename
}

archiveExts <- "(tar$|tar\\.gz$|zip$|gz$)"

recoverDataTableFromQs <- function(sim) {
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
  sim
}

.dealWithRasterBackends <- function(otherFiles, sim, paths) {
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

checkSimListExts <- function(filename) {
  stopifnot(grepl(paste0("(qs$|rds$)|", archiveExts), tolower(tools::file_ext(filename))))
}

warnDeprecFileBacked <- function(arg) {
  switch(tolower(arg),
         filebackeddir =
           paste0("filebackedDir is deprecated; use projectPath and optionally ",
                  "set individual path arguments, such as modulePath."),
         filebackend =
           paste0("fileBackend argument is deprecated; file-backed objects are ",
                  "now maintained; for memory only objects, convert them to RAM objects ",
                  "prior to saveSimList"),
         stop("No deprecation warning with that arg: ", arg)
         )
}

archiveExtract <- function(archiveName, exdir) {
  if (requireNamespace("archive") && !isWindows()) {
    archiveName <- archiveConvertFileExt(archiveName, "tar.gz")
    filename <- archive::archive_extract(archiveName)
  } else {
    filename <- unzip(archiveName, exdir = exdir)
  }
  filename
}

archiveWrite <- function(archiveName, relFns, verbose) {
  if (requireNamespace("archive") && !isWindows()) {
    archiveName <- archiveConvertFileExt(archiveName, "tar.gz")
    # archiveName <- gsub(tools::file_ext(archiveName), "tar.gz", archiveName)
    compLev <- getOption("spades.compressionLevel", 1)
    archive::archive_write_files(archiveName, relFns, options = paste0("compression-level=", compLev))
    # archive::archive_write_files(archiveName, files = relFns)
  } else {
    archiveName <- archiveConvertFileExt(archiveName, "zip")
    # archiveName <- gsub(tools::file_ext(archiveName), "zip", archiveName)
    # the qs file doesn't deflate at all
    extras <- list("--compression-method store", NULL)
    if (verbose <= 0) {
      extras <- lapply(extras, function(ex) c(ex, "--quiet"))
    }
    zip(archiveName, files = relFns[1], extras = extras[[1]])
    zip(archiveName, files = relFns[-1], extras = extras[[2]])
  }
}

archiveConvertFileExt <- function(filename, convertTo = "tar.gz") {
  if (!(endsWith(filename, "tar.gz") && identical(convertTo, "tar.gz"))) {
    filename <- gsub(tools::file_ext(filename), convertTo, filename)
  }
  filename
}

relativizePaths <- function(paths) {
  p <- normPath(paths)
  projectPath <- dirname(p["modulePath"])
  p[corePaths] <- makeRelative(unlist(p)[corePaths], projectPath)
  p[tmpPaths] <- makeRelative(unlist(p)[tmpPaths], p["scratchPath"])
  p
}

absolutizePaths <- function(paths, projectPath, tempdir = tempdir()) {
  p <- normPath(unlist(paths))
  isAbs <- isAbsolutePath(p)
  nonAbsCorePaths <- intersect(names(isAbs)[!isAbs], corePaths)
  nonAbsTmpPaths <- intersect(names(isAbs)[!isAbs], tmpPaths)
  p[!isAbs][nonAbsCorePaths] <- makeAbsolute(p[!isAbs][nonAbsCorePaths], projectPath)
  p[!isAbs][nonAbsTmpPaths] <- makeAbsolute(p[!isAbs][nonAbsTmpPaths], tempdir)
  p
}
