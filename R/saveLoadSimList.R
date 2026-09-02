#' Save a whole `simList` object to disk
#'
#' Saving a `simList` may not work using the standard approaches
#' (e.g., `save`, `saveRDS`, and `qs2::qs_save`).
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
#' @section Two ways to use this:
#'
#' \describe{
#'   \item{1. Portable -- take the simulation with you}{
#'     Save everything: the objects, the metadata, and the files behind any
#'     file-backed objects. Use this to move a simulation to another machine
#'     or hand it to someone else.
#'
#'     `saveSimList(sim, "mySim.rds", projectPath = projectPath)`
#'
#'     `projectPath` is the root that everything is stored relative to, so the
#'     whole thing can be unpacked somewhere else and still resolve. A
#'     file-backed object is re-rooted on load if it lives under `projectPath`
#'     or under one of the sim's own paths (`outputPath`, `inputPath`,
#'     `cachePath`, ...).
#'   }
#'   \item{2. In place -- keep the metadata, leave the files alone}{
#'     Save the information but not the file bundle, on the assumption you
#'     still have the paths the run used -- above all `outputPath(sim)`.
#'
#'     `saveSimList(sim, "mySim.rds", projectPath = projectPath, files = FALSE)`
#'
#'     Reload it and use `outputs(sim)` to see what the run wrote, then read
#'     back whatever you actually need. This is usually the practical choice:
#'     a real run writes far too many objects to bundle into an archive.
#'   }
#' }
#'
#' @details
#' There is a family of 2 functions that are mutually useful for saving and
#' loading `simList` objects and their associated files (e.g., file-backed
#' `Raster*`, `inputs`, `outputs`, `cache`) [saveSimList()], [loadSimList()].
#'
#' The `sim@.xData$._sim` slot (a circular reference used internally during a
#' running simulation) is removed before saving to avoid redundant data.
#' It is not needed for a saved/restored `simList`.
#'
#' Additional arguments may be passed via `...`, including:
#' - `files`: logical indicating whether files should be included in the archive.
#'            if `FALSE`, will override `cache`, `inputs`, `outputs`, setting them to `FALSE`.
#' - `symlinks`: a named list of paths corresponding to symlinks, which will be used to substitute
#'               normalized absolute paths of files.
#'               Names should correspond to the names in `paths()`;
#'               values should be project-relative paths.
#'               E.g., `list(cachePath = "cache", inputPath = "inputs", outputPath = "outputs")`.
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
#'   reading the `simList` from. Currently, only `.rds` and `.qs2` file types are supported.
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
#' @param files Logical. Should all the files in the optional `outputs`, `inputs`,
#'   `cache` be saved. If this is `TRUE`, then the resulting `filename` will be
#'   silently converted to an archive file with the appropriate extension e.g.,
#'   `.zip` or `.tar.gz`. This will automatically be `TRUE` if any of the `outputs`,
#'   `inputs` or `cache` are `TRUE`. Setting this to `FALSE` will turn off the
#'   saving of files specified in `inputs(sim)`, `outputs(sim)` or the cache.
#'
#' @param lazy Logical. If `TRUE`, the objects are written one per file into a
#'   sibling `<filename>_lazy/` directory instead of into the `simList` file
#'   itself, and [loadSimList()] binds each as a promise that reads its file on
#'   first access. This covers both the user objects in `sim@.xData` and every
#'   module's `mod` objects (`sim@.xData$.modObjs`); `.mods` and the internal
#'   dot-prefixed bindings stay in the file, being small and always needed. The
#'   result is a `simList` file of a few MB that opens in seconds, from which
#'   you pay only for the objects you actually touch.
#'
#'   One file per object rather than a single lazy-load database because a value
#'   larger than 2 GB cannot be put in one at all (`long vectors not supported
#'   yet`), and `mod` objects are routinely larger than that.
#'
#'   Note that promises are forced by whole-environment operations -- `as.list()`,
#'   `get()`, `mget()`, `eapply()`, and so anything that deep-copies the
#'   `simList`, such as [Copy()] -- but not by `ls()`, `exists()`, or reading
#'   metadata such as [outputs()]. Defaults to `FALSE`.
#' @param ... Additional arguments. See Details.
#'
#' @return
#' Invoked for side effects of saving both a `.qs2` (or `.rds`) file,
#' and a compressed archive (one of `.tar.gz` if using non-Windows OS or `.zip` on Windows).
#'
#' @examples
#' ## ---- 1. Portable: take everything with you ----
#' projectPath <- file.path(tempdir(), "myProject")
#' outPath <- file.path(projectPath, "outputs")
#' dir.create(outPath, recursive = TRUE, showWarnings = FALSE)
#'
#' sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"),
#'                paths = list(outputPath = outPath))
#' sim$anObject <- 1:10
#'
#' saveSimList(sim, file.path(projectPath, "portable.rds"),
#'             projectPath = projectPath)
#'
#' ## unpack it anywhere -- pass the new location as projectPath
#' sim2 <- loadSimList(file.path(projectPath, "portable.rds"),
#'                     projectPath = projectPath)
#' sim2$anObject
#'
#' ## ---- 2. In place: metadata only, files stay put ----
#' ## `files = FALSE` skips bundling; the run's own files are left where
#' ## they are, and the reloaded simList tells you where to find them
#' saveSimList(sim, file.path(projectPath, "inPlace.rds"),
#'             projectPath = projectPath, files = FALSE)
#'
#' sim3 <- loadSimList(file.path(projectPath, "inPlace.rds"),
#'                     projectPath = projectPath)
#' outputPath(sim3)   # where the run wrote its outputs
#' outputs(sim3)      # the manifest of what it wrote
#'
#' unlink(projectPath, recursive = TRUE)
#'
#' @aliases saveSim
#' @export
#' @importFrom fs path_common
#' @importFrom qs2 qs_save
#' @importFrom stats runif
#' @importFrom reproducible makeRelative .wrap
#' @importFrom Require messageVerbose
#' @importFrom tools file_ext
#' @importFrom utils modifyList
#' @rdname saveSimList
#' @seealso [loadSimList()]
saveSimList <- function(sim, filename, projectPath = getwd(),
                        outputs = TRUE, inputs = TRUE, cache = FALSE, envir,
                        files = TRUE, ..., lazy = FALSE) {
  checkSimListExts(filename)

  dots <- list(...)

  ## user can explicitly override archiving files if FALSE
  if (isFALSE(files)) {
    files <- cache <- inputs <- outputs <- FALSE
  } else {
    files <- TRUE
  }

  symlinks <- dots$symlinks

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
    ## `filebackedDir` is not a formal of this function; referring to it bare
    ## errored with "object 'filebackedDir' not found" instead of normalising
    ## the misspelling. Mirror the filebackend/fileBackend block below.
    if (is.null(dots$filebackedDir)) {
      dots$filebackedDir <- dots$fileBackedDir
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

  ## Break every pass-by-reference link into the caller's sim. simList
  ## state lives almost entirely in environments (@.xData and the nested
  ## .mods / .modObjs / .objects envs), and the subsequent rebindings
  ## (._randomSeed/._rng.kind, .wrapResiliently's per-object Path wrapping,
  ## paths() rewrite, lazy rm) would otherwise mutate the caller's state.
  ## We avoid reproducible::Copy(sim) here because its SpatRaster path
  ## eagerly duplicates backing files (filebackedDir = NULL is documented
  ## but not honored by the ANY method). .cloneSimEnvs recursively clones
  ## every environment but leaves leaf values (SpatRasters, data.tables,
  ## etc.) alone — saveSimList rebinds names, not nested object internals,
  ## so leaf-pointer sharing is harmless.
  sim <- .cloneSimEnvs(sim)

  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  sim@.xData$._randomSeed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  sim@.xData$._rng.kind <- RNGkind()

  messageVerbose("Saving simList object to file '", filename, "'.", verbose = verbose)

  if (exists("simName", inherits = FALSE)) {
    tmpEnv <- new.env(parent = emptyenv())
    assign(simName, sim, envir = tmpEnv)
    sim <- get(simName, envir = tmpEnv)
  }

  ## Say so now if any file-backed object cannot be anchored. Only when we are
  ## actually bundling files -- with `files = FALSE` the user has opted into a
  ## metadata-only save and is not expecting portability.
  if (isTRUE(files)) .warnUnanchoredFiles(sim, projectPath = projectPath)

  ## Pre-wrap file-backed objects one-by-one so a single inaccessible backing file
  ## does not abort the entire save; failed objects are saved as NULL with a warning.
  sim <- .wrapResiliently(sim, projectPath = projectPath)
  ## wrap remaining / non-file-backed; `projectPath` is offered as an anchor so a
  ## file-backed object that sits under it -- but under none of the sim's named
  ## paths -- can still be re-rooted on load instead of silently becoming NULL
  sim <- .wrap(sim, cachePath = NULL, paths = .wrapAnchors(sim, projectPath))
  sim@.xData$._sim <- NULL # remove circular reference; sim is already a Copy here
  sim@current <- list() # it is presumed that this event should be considered finished prior to saving

  if (isTRUE(files)) {
    fns <- Filenames(sim)
    empties <- nchar(fns) == 0
    if (any(empties)) {
      fns <- fns[!empties]
    }
  }

  ## This forces it to be qs2 (if not rds) instead of zip or tar.gz
  if (tools::file_ext(filename) != "rds") {
    filename <- archiveConvertFileExt(filename, "qs2")
  }

  origPaths <- paths(sim)
  relPaths <- if (is.null(symlinks)) {
    origPaths |>
      relativizePaths(projectPath) |>
      as.list()
  } else {
    origPaths |>
      modifyList(symlinks) |>
      relativizePaths(projectPath) |>
      as.list()
  }
  ## Assign the slot directly rather than through `paths<-`: that setter ends
  ## with checkPath(sim@paths$cachePath, create = TRUE), which would take the
  ## now-*relative* "cache" and create a stray directory in whatever directory
  ## the caller happens to be in. These paths are being relativized only so they
  ## serialize portably; nothing here wants directories created.
  sim@paths <- relPaths

  if (isTRUE(lazy)) {
    ext <- tools::file_ext(filename)
    ## Defer the user objects AND every module's `mod` objects to sidecar files;
    ## see .writeLazyObjs(). What is left in `sim` is the shell: slots, `.mods`
    ## and the dot-prefixed internals.
    lazyDir <- .lazyDirName(filename)
    lazyFiles <- .writeLazyObjs(sim, lazyDir, ext = ext)

    if (ext == "rds") {
      saveRDS(sim, file = filename)
    } else {
      qs2::qs_save(sim, file = filename, nthreads = getOption("spades.qsThreads", 1))
    }

    if (isTRUE(files) && length(fns)) {
      fileToDelete <- filename
      otherFns <- c()
      if (isTRUE(outputs)) {
        os <- outputs(sim)
        if (NROW(os)) otherFns <- c(otherFns, os[os$saved %in% TRUE, ]$file)
      }
      if (isTRUE(inputs)) {
        ins <- inputs(sim)
        if (NROW(ins)) otherFns <- c(otherFns, ins[ins$loaded %in% TRUE, ]$file)
      }

      srcFiles <- mapply(mod = modules(sim), mp = modulePath(sim),
                         function(mod, mp) {
                           fls <- dir(file.path(mp, mod), recursive = TRUE, full.names = TRUE)
                           grep("^\\<data\\>", invert = TRUE, value = TRUE, fls)
                         })
      srcFilesRel <- makeRelative(srcFiles, projectPath)
      if (any(isAbsolutePath(srcFilesRel))) {
        guessProjPath <- fs::path_common(origPaths["modulePath"]) |> unique() |> dirname()
        srcFilesRel <- makeRelative(srcFiles, guessProjPath)
        tmpSrcFiles <- file.path(projectPath, srcFilesRel)
        linkOrCopy(srcFiles, tmpSrcFiles, verbose = verbose - 1)
        on.exit(unlink(tmpSrcFiles))
        srcFiles <- tmpSrcFiles
      }

      lazyExisting <- lazyFiles[file.exists(lazyFiles)]
      allFns <- c(fns, otherFns, srcFilesRel)
      if (!is.null(symlinks)) {
        for (p in names(symlinks)) {
          allFns <- gsub(origPaths[[p]], symlinks[[p]], allFns)
        }
      }
      allFns <- na.omit(allFns)

      relFns <- makeRelative(c(fileToDelete, lazyExisting, allFns), projectPath) |> unname()
      archiveWrite(filename, relFns, verbose, projectPath = projectPath)
      unlink(fileToDelete)
      unlink(lazyDir, recursive = TRUE)
    }

    messageVerbose("    ... saved!", verbose = verbose)
    return(invisible())
  }

  # filename <- gsub(tools::file_ext(filename), "qs2", filename)
  if (tolower(tools::file_ext(filename)) == "rds") {
    saveRDS(sim, file = filename)
  } else if (tolower(tools::file_ext(filename)) == "qs2") {
    filename <- gsub(tools::file_ext(filename), "qs2", filename)
    qs2::qs_save(sim, file = filename, nthreads = getOption("spades.qsThreads", 1))
  }

  if (isTRUE(files)) {
    srcFiles <- mapply(mod = modules(sim), mp = modulePath(sim),
                   function(mod, mp) {
                     files <- dir(file.path(mp, mod), recursive = TRUE, full.names = TRUE)
                     files <- grep("^\\<data\\>", invert = TRUE, value = TRUE, files)
                   })
    srcFilesRel <- makeRelative(srcFiles, projectPath)
    if (any(isAbsolutePath(srcFilesRel))) {
      ## means not inside the projectPath
      guessProjPath <- fs::path_common(origPaths["modulePath"]) |> unique() |> dirname()
      srcFilesRel <- makeRelative(srcFiles, guessProjPath)
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
          outputFNs <- os[os$saved %in% TRUE, ]$file
          otherFns <- c(otherFns, outputFNs)
        }
      }
      inputFNs <- NULL
      if (isTRUE(inputs)) {
        ins <- inputs(sim)
        if (NROW(ins)) {
          inputFNs <- ins[ins$loaded %in% TRUE, ]$file
          otherFns <- c(otherFns, inputFNs)
        }
      }

      allFns <- c(fns, otherFns, srcFilesRel)
      if (!is.null(symlinks)) {
        for (p in names(symlinks)) {
          allFns <- gsub(origPaths[[p]], symlinks[[p]], allFns)
        }
      }
      allFns <- na.omit(allFns)

      relFns <- makeRelative(c(fileToDelete, allFns), projectPath) |> unname()

      archiveWrite(filename, relFns, verbose, projectPath = projectPath)

      unlink(fileToDelete)
    }
  }
  messageVerbose("    ... saved!", verbose = verbose)

  return(invisible())
}

#' Zip a `simList` and various files
#'
#' `zipSimList` will save the `simList` and file-backed `Raster*` objects, plus,
#' optionally, files identified in `outputs(sim)` and `inputs(sim)`.
#' This uses `Copy` under the hood, to not affect the original `simList`.
#'
#' @inheritParams saveSimList
#' @param zipfile A character string indicating the filename for the zip file. Passed to `zip`.
#'
#' @export
#' @rdname deprecated
zipSimList <- function(sim, zipfile, ..., outputs = TRUE, inputs = TRUE, cache = FALSE) {
  .Deprecated("saveSimList")
  saveSimList(sim, filename = zipfile)
}

#' Load a saved `simList` and ancillary files
#'
#' Loading a `simList` from file can be problematic as there are non-standard
#' objects that must be rebuilt. See description in [saveSimList()] for details.
#'
#' @param filename Character giving the name of a saved simulation file.
#'   Currently, only file types `.qs2` or `.rds` are supported.
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
#' @importFrom qs2 qs_read
#' @importFrom reproducible linkOrCopy remapFilenames updateFilenameSlots .unwrap
#' @importFrom tools file_ext
loadSimList <- function(filename, projectPath = getwd(), tempPath = tempdir(),
                        paths = NULL, otherFiles = "",
                        verbose = getOption("reproducible.verbose")) {
  checkSimListExts(filename)

  filename <- checkArchiveAlternative(filename)

  if (grepl(archiveExts, tolower(tools::file_ext(filename)))) {
    td <- tempdir2(sub = .rndstr())
    filename <- archiveExtract(filename, exdir = td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)

    baseNameNoExt <- tools::file_path_sans_ext(basename(filename[1]))
    lazyDirName <- paste0(baseNameNoExt, "_lazy")
    isLazy <- grepl(paste0("(^|/)", lazyDirName, "/"), filename[-1])

    filenameRel <- gsub(paste0(td, "/"), "", filename[-1][!isLazy])  ## TODO: WRONG!
    ## This will put the files to relative path of projectPath
    newFns <- file.path(projectPath, filenameRel)
    linkOrCopy(filename[-1][!isLazy], newFns, verbose = verbose - 1)

    ## Persist the sidecar objects beyond tempdir cleanup: the promises bound
    ## below resolve long after this call returns, and `td` is unlinked on exit.
    lazyDir <- file.path(tempPath, lazyDirName)
    srcs <- filename[-1][isLazy]
    if (length(srcs)) {
      unlink(lazyDir, recursive = TRUE)
      dir.create(lazyDir, recursive = TRUE, showWarnings = FALSE)
      file.copy(srcs, file.path(lazyDir, basename(srcs)))
    }
  } else {
    # filenameRel <- gsub(paste0(projectPath, "/"), "", filename) ## TODO: WRONG!
    filenameRel <- getRelative(filename, projectPath)
    lazyDir <- .lazyDirName(filename[1])
  }

  if (tolower(tools::file_ext(filename[1])) == "rds") {
    tmpsim <- readRDS(filename[1])
  } else if (tolower(tools::file_ext(filename[1])) == "qs2") {
    tmpsim <- qs2::qs_read(filename[1], nthreads = getOption("spades.qsThreads", 1))
  }
  if (!is.null(paths)) {
    paths <- lapply(paths, normPath)
  } else {
    paths <- list()
  }

  ## TODO: figure out what is inserting 'NA' into some paths during saveSimList
  ## Assign the slot directly: at this point the sim still carries the *relative*
  ## paths it was serialized with ("cache", "inputs", ...), and `paths<-` ends
  ## with checkPath(sim@paths$cachePath, create = TRUE), which would create a
  ## stray `cache/` in the caller's working directory. The very next assignment
  ## absolutizes them and goes through `paths<-`, so the directories that should
  ## exist are still created -- under projectPath, where they belong.
  tmpsim@paths <- paths(tmpsim) |>
    # sapply(function(pth) {
    #   if (fs::path_has_parent(pth, "NA")) {
    #     gsub("NA/", "./", pth) |> fs::path_norm() |> as.character()
    #   } else {
    #     pth
    #   }
    # }, simplify = FALSE) |>
    modifyList2(paths)

  paths(tmpsim) <- absolutizePaths(paths(tmpsim), projectPath, tempPath)

  ## remap all the file-backed objects. their paths in the objects will point
  ## to their old locations, but they are now at newFns, which is remapped to projectPath
  oldFns <- Filenames(tmpsim, returnList = TRUE)
  oldFns <- FilterRecursive(length, oldFns) ## handles nested lists (e.g. scfm objs, biomassModel)

  for (nam in names(oldFns)) {
    tags <- attr(tmpsim[[nam]], "tags")
    if (!is.null(tags)) {
      if (identical(projectPath, getwd())) {
        pths <- paths(tmpsim)
      } else {
        ## include the sim's own paths too, so an object anchored to a named
        ## path (e.g. outputPath) still resolves, not just projectPath ones
        pths <- c(paths(tmpsim), list(projectPath = projectPath))
      }

      newFiles <- remapFilenames(tags = tags, cachePath = NULL, paths = pths)

      if (is(tmpsim[[nam]], "list")) {
        # lists are weird; historicalClimateLayers was length 2 list; had 4 filenames, 2 repeated twice
        #   need to fix this
        newNames <- unique(newFiles$newName)
        for (elem in names(tmpsim[[nam]])) {
          fileHere <- tmpsim[[nam]][[elem]] # should only have 1 element's file(s)
          if (!is.character(fileHere) || !length(fileHere)) next # NULL'd by .wrapResiliently
          dirToFileHere <- dirname(fileHere)
          nParents <- attr(fileHere, "nParentDirs")
          1
          #if (nParents > 0) {
          for (nPar in rev(seq(nParents + 1))) {
            dirToFileHere <- dirname(dirToFileHere)
          }
          newNames1 <- fs::path_rel(newNames, dirToFileHere)
          thisFile <- fs::path_rel(fileHere, dirToFileHere)
          newNames1 <- newNames1[newNames1 %in% thisFile]
          newNamesHere <- file.path(dirToFileHere, newNames1)
          tmpsim[[nam]][[elem]][] <- unique(newNamesHere)
        }
      } else {
        tmpsim[[nam]][] <- newFiles$newName[]
      }
    }
  }

  ## A lazily saved simList carries its objects -- user objects and every module's
  ## `mod` objects alike -- as sidecar files rather than in the shell. Nothing to
  ## strip here: the shell has none of them, and each is bound as a promise below,
  ## after the module environments exist. `.unwrap` therefore never runs eagerly
  ## over them, so a `mod` object whose backing file is missing now fails on
  ## access, in its own tryCatch, instead of aborting the load.
  isLazyLoad <- file.exists(file.path(lazyDir, .lazyManifestName))

  tmpsim <- .unwrapResiliently(tmpsim, paths(tmpsim))
  tmpsim <- .unwrap(tmpsim, cachePath = NULL, paths = paths(tmpsim))

  ## Work around for bug in qs that recovers data.tables as lists
  # tmpsim <- recoverDataTableFromQs(tmpsim)

  ## Deal with all the RasterBacked Files that will be wrong
  if (any(nchar(otherFiles) > 0)) {
    .dealWithRasterBackends(tmpsim) # no need to assign to sim b/c uses list2env
  }
  makeSimListActiveBindings(tmpsim)

  ## Restore the module functions. `saveSimList()` carries objects, metadata,
  ## paths and the event queue, but not the module code -- see .reparseModules()
  ## -- so without this a reloaded simList cannot be run.
  tmpsim <- .reparseModules(tmpsim, modules(tmpsim), verbose = verbose)

  ## Lazy loading: bind a promise per sidecar object, into @.xData for user
  ## objects and into .modObjs[[module]] for `mod` objects. Done after
  ## .reparseModules() so the module environments and their `mod` active
  ## bindings are already in place -- a module then reaches its `mod` object
  ## exactly as it would have before, and pays for it only on first access.
  if (isLazyLoad) {
    tmpsim <- .attachLazyObjs(tmpsim, lazyDir, projectPath)
  }

  return(tmpsim)
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

## TODO: is this still needed when using qs2??
#' @importFrom data.table as.data.table data.table rbindlist
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
  objs <- objs[objectClass == "data.table" & objectClassInSim != "disk.frame"]
  objs <- objs[objectClass != objectClassInSim]
  if (NROW(objs)) {
    message("There is a bug in qs package that recovers data.table objects incorrectly when in a list")
    message("Converting all known data.table objects (according to metadata) from list to data.table")
    simEnv <- envir(sim)
    out <- lapply(objs$objectName, function(on) {
      tryCatch({
        assign(on, copy(as.data.table(sim[[on]])), envir = simEnv)
      }, error = function(e) warning(e))
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

## Recursively clone every environment reachable from a simList so that
## subsequent rebindings inside saveSimList don't mutate the caller's state.
## Leaf values (SpatRasters, data.tables, lists, etc.) are kept by reference
## — saveSimList rebinds names in the cloned envs rather than mutating the
## internals of those leaves, and avoiding leaf duplication is the whole
## point of doing this instead of reproducible::Copy(sim) (which eagerly
## duplicates SpatRaster backing files).
.cloneSimEnvs <- function(sim) {
  sim@.xData    <- .cloneEnvDeep(sim@.xData)
  sim@.envir    <- sim@.xData
  sim@completed <- .cloneEnvDeep(sim@completed)
  sim
}

## Recursive environment cloner. For each binding: if it's an environment,
## recurse; otherwise pass it through. Preserves attributes.
.cloneEnvDeep <- function(env, .seen = NULL) {
  if (!is.environment(env)) return(env)
  if (is.null(.seen)) .seen <- new.env(parent = emptyenv())
  key <- format(env)
  if (exists(key, envir = .seen, inherits = FALSE)) {
    return(get(key, envir = .seen, inherits = FALSE))
  }
  out <- new.env(parent = parent.env(env))
  assign(key, out, envir = .seen)  # cycle guard before recursing
  for (nm in ls(env, all.names = TRUE)) {
    if (bindingIsActive(nm, env)) {
      ## Re-attach the same active binding (its backing function is what
      ## defines its behavior; copying the function is enough).
      makeActiveBinding(nm, activeBindingFunction(nm, env), out)
    } else {
      val <- get(nm, envir = env, inherits = FALSE)
      assign(nm, .cloneEnvDeep(val, .seen = .seen), envir = out)
    }
  }
  attributes(out) <- attributes(env)
  out
}

## ---- lazy sidecar objects ------------------------------------------------
##
## `lazy = TRUE` defers the big objects: each is written to its own file under
## `<filename sans ext>_lazy/`, and loadSimList() binds a promise (delayedAssign)
## for it instead of reading it. Two reasons this is one-file-per-object rather
## than tools::makeLazyLoadDB(): a single value larger than 2 GB cannot go into a
## lazy load DB at all ("long vectors not supported yet: connections.c"), and
## `mod` objects routinely are that large; and a file per object lets a reader
## pay for exactly what it touches.
##
## Deferred: the user objects in `sim@.xData` (undotted) and every `mod` object
## in `sim@.xData$.modObjs[[module]]`. `.mods` and the dot-prefixed internals
## stay in the shell -- together a few MB, and always needed.
##
## NOTE: promises are forced by bulk environment operations -- `as.list()`,
## `get()`, `mget()`, `eapply()` -- but not by `ls()` or `exists()`. So a whole
## simList `Copy()` materializes everything, while reading metadata or touching
## one object does not.

.lazyDirName <- function(filename) paste0(tools::file_path_sans_ext(filename), "_lazy")

.lazyManifestName <- "_manifest.rds"

.saveOneLazy <- function(obj, file, ext) {
  if (identical(tolower(ext), "qs2")) {
    qs2::qs_save(obj, file = file, nthreads = getOption("spades.qsThreads", 1))
  } else {
    saveRDS(obj, file = file)
  }
}

.readOneLazy <- function(file) {
  if (identical(tolower(tools::file_ext(file)), "qs2")) {
    qs2::qs_read(file, nthreads = getOption("spades.qsThreads", 1))
  } else {
    readRDS(file)
  }
}

## The environment a manifest row belongs to. `where` is "" for objects that live
## directly in sim@.xData, otherwise the module name whose .modObjs env holds it.
## Creates the .modObjs env if it is not there (loading into a fresh shell).
.lazyEnvFor <- function(sim, where) {
  if (!nzchar(where)) return(sim@.xData)
  if (!is.environment(sim@.xData[[dotObjs]]))
    sim@.xData[[dotObjs]] <- new.env(parent = emptyenv())
  if (!is.environment(sim@.xData[[dotObjs]][[where]]))
    sim@.xData[[dotObjs]][[where]] <- new.env(parent = emptyenv())
  sim@.xData[[dotObjs]][[where]]
}

## Everything that gets deferred, as a manifest.
.lazyEntries <- function(sim) {
  ents <- list()
  userObjNames <- ls(sim@.xData, all.names = FALSE)
  if (length(userObjNames))
    ents[[length(ents) + 1]] <- data.frame(where = "", name = userObjNames)

  modObjsEnv <- sim@.xData[[dotObjs]]
  if (is.environment(modObjsEnv)) {
    for (m in ls(modObjsEnv, all.names = TRUE)) {
      e <- modObjsEnv[[m]]
      if (!is.environment(e)) next
      nms <- ls(e, all.names = TRUE)
      if (length(nms))
        ents[[length(ents) + 1]] <- data.frame(where = m, name = nms)
    }
  }
  if (!length(ents)) return(data.frame(where = character(), name = character()))
  do.call(rbind, ents)
}

## Write each deferred object to its own file and remove it from the simList, so
## the shell that gets serialized carries only metadata. Returns the files
## written (manifest included), for the archive.
.writeLazyObjs <- function(sim, dir, ext = "rds") {
  ents <- .lazyEntries(sim)
  unlink(dir, recursive = TRUE)
  if (!NROW(ents)) return(character())

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ## index keeps the name unique; the readable part is only for humans browsing
  ## the directory -- the manifest is what load reads.
  ents$file <- sprintf("%04d-%s.%s", seq_len(NROW(ents)),
                       gsub("[^A-Za-z0-9._-]", "_", ents$name), ext)

  for (i in seq_len(NROW(ents))) {
    env <- .lazyEnvFor(sim, ents$where[i])
    .saveOneLazy(get(ents$name[i], envir = env, inherits = FALSE),
                 file.path(dir, ents$file[i]), ext)
    rm(list = ents$name[i], envir = env)
  }
  saveRDS(ents, file.path(dir, .lazyManifestName))

  file.path(dir, c(ents$file, .lazyManifestName))
}

## Bind a promise per manifest row. The body matches what loadSimList() does for
## an eagerly loaded object -- remap file-backed paths, then unwrap -- so a lazy
## object is indistinguishable from an eager one once touched.
.attachLazyObjs <- function(sim, dir, projectPath) {
  mf <- file.path(dir, .lazyManifestName)
  if (!file.exists(mf)) return(sim)

  ents <- readRDS(mf)
  simPaths <- paths(sim)
  for (i in seq_len(NROW(ents))) {
    env <- .lazyEnvFor(sim, ents$where[i])
    local({
      .f  <- file.path(dir, ents$file[i])
      .nm <- ents$name[i]
      .pp <- projectPath
      .sp <- simPaths
      delayedAssign(.nm, tryCatch({
        obj <- .readOneLazy(.f)
        obj <- .remapFileBackedObj(obj, .pp, .sp)
        .unwrap(obj, cachePath = NULL, paths = .sp)
      }, error = function(e) {
        warning("Could not load lazy object '", .nm, "' from '", .f, "': ",
                conditionMessage(e), call. = FALSE)
        NULL
      }), eval.env = environment(), assign.env = env)
    })
  }
  sim
}

## Anchors offered to .wrap()/.unwrap() when deciding what a file-backed
## object's location should be recorded relative to. reproducible's
## relativeToWhat() sorts anchors longest-path-first and takes the first that is
## a prefix, so adding the (usually outermost) projectPath never displaces a
## more specific anchor such as outputPath -- it only catches files that would
## otherwise have no anchor at all.
.wrapAnchors <- function(sim, projectPath = NULL) {
  anchors <- paths(sim)
  ## reproducible already appends `getwd = getwd()` as a final anchor, so only
  ## add projectPath when it is a DIFFERENT directory. Adding it when they are
  ## the same creates two anchors for one directory and which name wins becomes
  ## order-dependent, changing the paths recorded for file-backed objects.
  if (!is.null(projectPath) && nzchar(projectPath) &&
      !identical(normPath(projectPath), normPath(getwd())))
    anchors <- c(anchors, list(projectPath = projectPath))
  anchors
}

## Warn, at save time, about file-backed objects that cannot be re-rooted on
## load. A backing file that lies under none of the sim's named paths, nor
## under `projectPath`, nor under the working directory, has no anchor: .wrap()
## records its absolute path, nothing puts it into the archive, and
## loadSimList() elsewhere gets either NULL or a path that exists only on the
## machine that saved it. The failure otherwise surfaces far from its cause --
## at load, on another machine, as a silent NULL. See #389.
.warnUnanchoredFiles <- function(sim, projectPath = NULL) {
  anchors <- unlist(c(.wrapAnchors(sim, projectPath), list(getwd = getwd())),
                    use.names = FALSE)
  anchors <- unique(normPath(anchors[nzchar(anchors) & !is.na(anchors)]))
  if (!length(anchors)) return(invisible(NULL))

  isAnchored <- function(f) any(vapply(anchors, function(a) fs::path_has_parent(f, a),
                                       logical(1)))

  unanchored <- list()
  for (nm in ls(sim@.xData, all.names = FALSE)) {
    fns <- tryCatch(Filenames(sim@.xData[[nm]]), error = function(e) character(0))
    fns <- unique(fns[nzchar(fns) & !is.na(fns)])
    if (!length(fns)) next
    bad <- fns[!vapply(normPath(fns), isAnchored, logical(1))]
    if (length(bad)) unanchored[[nm]] <- bad
  }
  if (!length(unanchored)) return(invisible(NULL))

  warning("saveSimList: ", length(unanchored), " file-backed object(s) lie outside ",
          "`projectPath` and every path in `paths(sim)`, so they cannot be ",
          "re-rooted when this simList is loaded elsewhere:\n",
          paste0("  ", names(unanchored), ": ",
                 vapply(unanchored, function(x) paste(x, collapse = ", "), character(1)),
                 collapse = "\n"),
          "\n  They are saved by absolute path and are not bundled. Move them under ",
          "`projectPath`, or add their directory to `paths(sim)`, to make this ",
          "simList portable.", call. = FALSE)
  invisible(names(unanchored))
}

## Pre-wrap each file-backed object in sim@.xData individually so that one
## inaccessible backing file does not abort saveSimList. Failed objects are
## replaced with NULL and a warning is issued; the subsequent monolithic
## .wrap(sim, ...) then succeeds on the remaining objects.
.wrapResiliently <- function(sim, projectPath = NULL) {
  nms <- ls(sim@.xData, all.names = FALSE)
  simPaths <- .wrapAnchors(sim, projectPath)
  for (nm in nms) {
    obj <- sim@.xData[[nm]]
    fns <- tryCatch(Filenames(obj), error = function(e) character(0))
    if (length(fns) && any(nchar(fns) > 0L)) {
      sim@.xData[[nm]] <- tryCatch(
        .wrap(obj, cachePath = NULL, paths = simPaths),
        error = function(e) {
          warning("saveSimList: could not wrap '", nm,
                  "' (backing file inaccessible); saving as NULL.\n",
                  "  ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
    }
  }
  sim
}

## Pre-unwrap each file-backed object in sim@.xData individually so that one
## inaccessible backing file does not abort loadSimList. Failed objects are
## replaced with NULL and a warning is issued; the subsequent monolithic
## .unwrap(sim, ...) then succeeds on the remaining objects. Mirror image of
## .wrapResiliently — load-time failures are independent of save-time
## failures (e.g. backing files may have been present at save time but
## missing on the machine doing the load).
.unwrapResiliently <- function(sim, simPaths) {
  nms <- ls(sim@.xData, all.names = FALSE)
  for (nm in nms) {
    obj <- sim@.xData[[nm]]
    if (is.null(attr(obj, "tags"))) next  # not wrapped
    fns <- tryCatch(Filenames(obj), error = function(e) character(0))
    if (length(fns) && any(nchar(fns) > 0L)) {
      sim@.xData[[nm]] <- tryCatch(
        .unwrap(obj, cachePath = NULL, paths = simPaths),
        error = function(e) {
          warning("loadSimList: could not unwrap '", nm,
                  "' (backing file inaccessible); loading as NULL.\n",
                  "  ", conditionMessage(e), call. = FALSE)
          NULL
        }
      )
    }
  }
  sim
}

## Remap file paths in a single wrapped object (mirrors the per-object logic in
## loadSimList's remap loop, extracted so lazy promises can reuse it).
.remapFileBackedObj <- function(obj, projectPath, simPaths) {
  tags <- attr(obj, "tags")
  if (is.null(tags)) return(obj)
  ## Only remap objects that are truly file-backed (mirrors the Filenames filter in
  ## the non-lazy remap loop — wrapped non-file objects have tags but no filenames).
  fns <- tryCatch(Filenames(obj), error = function(e) character(0))
  if (!length(fns) || all(nchar(fns) == 0L)) return(obj)
  ## Same anchor set as the non-lazy remap loop in loadSimList(): when
  ## `projectPath` is not the working directory it is an ADDITIONAL anchor, not
  ## a replacement. Using it alone dropped the sim's named paths, so a lazily
  ## saved object anchored to e.g. outputPath had nothing to resolve against.
  pths <- if (identical(projectPath, getwd())) {
    simPaths
  } else {
    c(simPaths, list(projectPath = projectPath))
  }
  newFiles <- remapFilenames(tags = tags, cachePath = NULL, paths = pths)
  if (is(obj, "list")) {
    newNames <- unique(newFiles$newName)
    for (elem in names(obj)) {
      fileHere      <- obj[[elem]]
      if (!is.character(fileHere) || !length(fileHere)) next # NULL'd by .wrapResiliently
      dirToFileHere <- dirname(fileHere)
      nParents      <- attr(fileHere, "nParentDirs")
      for (nPar in rev(seq(nParents + 1))) {
        dirToFileHere <- dirname(dirToFileHere)
      }
      newNames1    <- fs::path_rel(newNames, dirToFileHere)
      thisFile     <- fs::path_rel(fileHere, dirToFileHere)
      newNames1    <- newNames1[newNames1 %in% thisFile]
      newNamesHere <- file.path(dirToFileHere, newNames1)
      obj[[elem]][] <- unique(newNamesHere)
    }
  } else {
    obj[] <- newFiles$newName[]
  }
  obj
}

checkSimListExts <- function(filename) {
  stopifnot(grepl(paste0("(qs2$|rds$)|", archiveExts), tolower(tools::file_ext(filename))))
}

warnDeprecFileBacked <- function(arg) {
  switch(
    tolower(arg),
    filebackeddir = paste0(
      "filebackedDir is deprecated; use projectPath and optionally ",
      "set individual path arguments, such as modulePath."
    ),
    filebackend = paste0(
      "fileBackend argument is deprecated; file-backed objects are ",
      "now maintained; for memory only objects, convert them to RAM objects ",
      "prior to saveSimList"
    ),
    stop("No deprecation warning with that arg: ", arg)
  )
}

archiveExtract <- function(archiveName, exdir) {
  if (requireNamespace("archive") && !isWindows()) {
    archiveName <- archiveConvertFileExt(archiveName, "tar.gz")
    ## `dir` defaults to "."; without it this extracts into the working
    ## directory and ignores `exdir`, diverging from the unzip() branch below
    ## and scattering the archive's own directory names (cache/, outputs/,
    ## modules/) into whatever directory the caller happened to be in.
    filename <- archive::archive_extract(archiveName, dir = exdir)
    ## archive_extract() returns paths relative to `dir`; unzip() returns them
    ## rooted at `exdir`. Make the two branches agree.
    filename <- file.path(exdir, filename)
  } else {
    filename <- unzip(archiveName, exdir = exdir)
  }
  filename
}

archiveWrite <- function(archiveName, relFns, verbose, projectPath = getwd()) {
  relFns <- unname(relFns)

  ## `relFns` are relative to `projectPath`. Both archive::archive_write_files()
  ## and zip() resolve relative paths against the working directory, and store
  ## them in the archive as given -- so we have to be sitting in `projectPath`
  ## while writing, or the paths neither resolve nor land in the archive with
  ## the right internal structure. `archiveName` is made absolute first so it
  ## still points at the same file once we have moved.
  archiveName <- fs::path_abs(archiveName) |> as.character()
  if (!identical(fs::path_norm(projectPath), fs::path_norm(getwd()))) {
    owd <- setwd(projectPath)
    on.exit(setwd(owd), add = TRUE)
  }

  if (requireNamespace("archive") && !isWindows()) {
    archiveName <- archiveConvertFileExt(archiveName, "tar.gz")
    # archiveName <- gsub(tools::file_ext(archiveName), "tar.gz", archiveName)
    compLev <- getOption("spades.compressionLevel", 1)
    archive::archive_write_files(
      archiveName,
      relFns,
      options = paste0("compression-level=", compLev)
    )
    # archive::archive_write_files(archiveName, files = relFns)
  } else {
    archiveName <- archiveConvertFileExt(archiveName, "zip")
    # archiveName <- gsub(tools::file_ext(archiveName), "zip", archiveName)
    ## the qs2 file doesn't deflate at all
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

#' @importFrom fs path_common path_norm
#' @importFrom reproducible getRelative makeRelative
relativizePaths <- function(paths, projectPath = NULL) {
  # p <- normPath(paths)
  p <- sapply(paths, fs::path_norm, USE.NAMES = TRUE)
  if (is.null(projectPath)) {
    projectPath <- fs::path_common(p[["modulePath"]]) |> unique() |> dirname()
  }
  ## fs::path_rel, not getRelative: absolutizePaths() inverts this with
  ## fs::path_abs(start = projectPath), and only path_rel is its true inverse.
  ## getRelative("<base>/outs", "<base>/proj") returns "outs" -- dropping the
  ## fact that it is a SIBLING of projectPath -- which re-absolutizes to
  ## "<base>/proj/outs". Any named path outside projectPath (an outputPath
  ## elsewhere, say) therefore came back pointing at a directory that never
  ## existed, and its file-backed objects loaded as NULL. path_rel gives
  ## "../outs", which round-trips. For paths under projectPath the two agree.
  p[corePaths] <- sapply(p[corePaths], function(x)
    as.character(fs::path_rel(x, start = projectPath)))
  p[tmpPaths] <- makeRelative(p[tmpPaths], p[["scratchPath"]])

  ## TODO: recombine paths, e.g. modulePath1, modulePath2 into modulePath
  p
}

#' @importFrom fs path_abs
absolutizePaths <- function(paths, projectPath, tempdir = tempdir()) {
  p <- paths
  p[corePaths] <- sapply(paths[corePaths], fs::path_abs, start = projectPath)
  p[tmpPaths] <- sapply(paths[tmpPaths], fs::path_abs, start = tempdir)
  lapply(p, normPath)
}
