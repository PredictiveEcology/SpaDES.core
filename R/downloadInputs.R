if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expectedFile", "objName", "V1"))
}


#' Assess whether an object has or will be supplied from elsewhere
#'
#' When loading objects into a \code{simList}, especially during the
#' \code{simInit} call, and inside the \code{.inputObjects} functions of modules,
#' it is often useful to know if an object in question will or has been
#' by the user via the \code{inputs} or \code{objects} arguments, or by another
#' module's \code{.inputObjects} while preparing its expected inputs (via
#' \code{expectsInputs} in metadata), or if it will be supplied by another
#' module during its \code{"init"} event. In all these cases, it may not
#' be necessary for a given module to load any default value for its \code{expectsInputs}.
#' This function can be used as a check to determine whether the module needs
#' to proceed in getting and assigning its default value.
#'
#' @param object Character vector or sim object in the form sim$objName
#' @param sim A \code{simList} in which to evaluated whether the object is supplied elsewhere
#' @param where Character vector with one to three of "sim", "user", or "initEvent".
#'        Default is all three. Partial matching is used. See details.
#' @export
#'
#' @details
#'
#' \code{where} indicates which of three places to search, either \code{"sim"} i.e.,
#' the \code{simList}, which would be equivalent to \code{is.null(sim\$objName)}, or
#' \code{"user"} which would be supplied by the user in the \code{simInit} function
#' call via \code{outputs} or \code{inputs} (equivalent to
#' \code{(!('defaultColor' \%in\% sim$.userSuppliedObjNames))}),
#' or \code{"initEvent"}, which would test whether a module that gets loaded \bold{before}
#' the present one \bold{will} create it as part of its outputs (i.e., as indicated by
#' \code{createsOutputs} in that module's metadata). There is a caveat to this test,
#' however; if that other event also has the object as an \code{expectsInput}, then
#' it would fail this test, as it \emph{also} needs it as an input.
#' This final one (\code{"initEvent"})
#' does not explicitly test that the object will be created in the "init" event, only that
#' it is in the outputs of that module, and that it is a module that is loaded prior to
#' this one.
#' @examples
#' mySim <- simInit()
#' suppliedElsewhere("test", mySim) # FALSE
#'
#' # supplied in the simList
#' mySim$test <- 1
#' suppliedElsewhere("test", mySim) # TRUE
#' test <- 1
#'
#' # supplied from user at simInit time -- note, this object would eventually get into the simList
#' #   but the user supplied values come *after* the module's .inputObjects, so
#' #   a basic is.null(sim$test) would return TRUE even though the user supplied test
#' mySim <- simInit(objects = list("test" = test))
#' suppliedElsewhere("test", mySim) # TRUE
#'
suppliedElsewhere <- function(object, sim, where = c("sim", "user", "initEvent")) {
  partialMatching <- c("s", "i", "u")
  where <- partialMatching[which(!is.na(pmatch(partialMatching, where)))]
  if (length(where) == 0) stop("where must be either sim, user or initEvent")
  objDeparsed <- substitute(object)
  if (missing(sim)) {
    theCall <- as.call(parse(text = deparse(objDeparsed)))
    objDeparsedIfHasSim <- .parsingSim(theCall[[1]], "assign")
    if (length(objDeparsedIfHasSim))
      objDeparsed <- objDeparsedIfHasSim
    env <- parent.frame()
    isSimList <- unlist(lapply(theCall[[1]], function(x)
      isTRUE(try(is(eval(x, envir = env), "simList"), silent = TRUE))))
    if (!all(isSimList)) {
      sim <- get("sim", envir = env)
    } else {
      sim <- eval(theCall[[1]][isSimList][[1]], envir = env)
    }

  }

  # if object was actually a variable of character names of objects inside sim
  objDeparsed <- tryCatch(eval(objDeparsed, parent.frame()), error = function(y) objDeparsed)

  objDeparsed <- as.character(objDeparsed)

  # Equivalent to !is.null(sim$xxx)
  inPrevDotInputObjects <- if ("s" %in% where) {
    match(objDeparsed, names(sim@.envir), nomatch = 0L) > 0L
  } else {
    FALSE
  }
  # Equivalent to !(names(sim) %in% sim$.userSuppliedObjNames)
  inUserSupplied <- if ("u" %in% where) {
    objDeparsed %in% sim$.userSuppliedObjNames
  } else {
    FALSE
  }

  # If one of the modules that has already been loaded has this object as an output,
  #   then don't create this
  inFutureInit <- if ("i" %in% where) {
      # The next line is subtle -- it must be provided by another module, previously loaded (thus in the depsEdgeList),
      #   but that does not need it itself. If it needed it itself, then it would have loaded it already in the simList
      #   which is checked in a different test of suppliedElsewhere -- i.e., "sim"
      isTRUE(depsEdgeList(sim)[!(from %in% c("_INPUT_", currentModule(sim))), ][
        objName == objDeparsed][, all(from != to), by = from][V1==TRUE]$V1)

  } else {
    FALSE
  }

  (inUserSupplied | inPrevDotInputObjects | inFutureInit)
}

#' Download file from web databases
#'
#' This function can be used to download a file from a web database listed in
#'\link[webDatabases]{webDatabases}.
#'
#' @param filename Character string naming the file to be downloaded.
#'
#' @param filepath Character string giving the path where the file will be written.
#'
#' @param dataset Optional character string representing the dataset of interest
#' for download. Allows for restricting the lookup for the url to a dataset,
#' thus avoiding filename collision.
#'
#' @inheritParams prepInputs
#'
#' @author Jean Marchal
#' @importFrom httr authenticate GET http_error progress write_disk
#' @importFrom webDatabases webDatabases
#' @importFrom stats runif
#' @rdname downloadFromWebDB
#'
downloadFromWebDB <- function(filename, filepath, dataset = NULL, quickCheck = FALSE) {
  urls <- webDatabases(local = quickCheck)

  if (!is.null(set <- dataset))
    urls <- urls[grepl(dataset, pattern = set, fixed = TRUE)]

  if (any(wh <- filename == urls$files)) {
    authenticate <- if (!is.na(urls$password[wh])) {
      split <- strsplit(urls$password[wh], split = "[:]")[[1]]
      httr::authenticate(split[1L], split[2L])
    }

    url <- urls$url[wh]

    if (httr::http_error(url))
      stop("Can not access url", url)

    message("  Downloading ", filename)

    httr::GET(
      url = paste0(url, filename),
      authenticate,
      httr::progress(),
      httr::write_disk(filepath, overwrite = TRUE)
    )
  }
}

#' Extract files from archive.
#'
#' Extract zip or tar archive files, possibly nested in other zip or tar
#' archives.
#'
#' @param archive Character string giving the path of the archive
#' containing the \code{file} to be extracted.
#'
#' @param destinationPath Character string giving the path where \code{needed} will be
#' extracted. Defaults to the archive directory.
#'
#' @param needed Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally.
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal
#' @importFrom reproducible Cache
#' @importFrom tools file_ext
#' @rdname extractFromArchive
#'
extractFromArchive <- function(archive, destinationPath = dirname(archive),
                               needed, extractedArchives = NULL) {
  args <- list(archive[1], exdir = destinationPath[1])

  funWArgs <- .whichExtractFn(archive[1], args)

  filesInArchive <- funWArgs$fun(archive[1], list = TRUE)
  if ("Name" %in% names(filesInArchive)) filesInArchive <- filesInArchive$Name # for zips

  if (length(archive) > 1) {
    .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = basename(archive[2]))
    # recursion, removing one archive
    extractFromArchive(archive[-1], destinationPath = destinationPath,
                       needed = needed, extractedArchives = extractedArchives)
  } else if (any(needed %in% basename(filesInArchive)) || is.null(needed)) {
    message(paste("  Extracting from archive:", basename(archive[1])))
    .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = filesInArchive[basename(filesInArchive) %in% needed])
  } else {
    # don't have a 2nd archive, and don't have our needed file
    isArchive <- grepl(file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

    if (any(isArchive)) {
      arch <- filesInArchive[isArchive]
      .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = arch)

      # lapply(file.path(destinationPath, arch), function(archi)
      #   extractFromArchive(archi, destinationPath, needed, extractedArchives))

      extractedArchives <- c(
        extractedArchives,
        unlist(
          lapply(file.path(destinationPath, arch), function(ap)
            extractFromArchive(archive = ap, destinationPath = destinationPath,
                               needed = needed, extractedArchives = extractedArchives))
        )
      )
    }

  }
  c(extractedArchives, archive)
}

#' Add a prefix or suffix to the basename part of a file path
#'
#' Prepend (or postpend) a filename with a prefix (or suffix).
#' If the directory name of the file cannot be ascertained from its path,
#' it is assumed to be in the current working directory.
#'
#' @param f       A character string giving the name/path of a file.
#' @param prefix  A character string to prepend to the filename.
#' @param suffix  A character string to postpend to the filename.
#'
#' @author Jean Marchal
#' @author Alex Chubaty
#' @export
#' @importFrom tools file_ext file_path_sans_ext
#' @rdname prefix
#'
#' @examples
#' # file's full path is specified (i.e., dirname is known)
#' myFile <- file.path("~/data", "file.tif")
#' .prefix(myFile, "small_")    ## "/home/username/data/small_file.tif"
#' .suffix(myFile, "_cropped") ## "/home/username/data/myFile_cropped.shp"
#'
#' # file's full path is not specified
#' .prefix("myFile.shp", "small")    ## "./small_myFile.shp"
#' .suffix("myFile.shp", "_cropped") ## "./myFile_cropped.shp"
#'
.prefix <- function(f, prefix = "") {
  file.path(dirname(f), paste0(prefix, basename(f)))
}

#' @export
#' @rdname prefix
.suffix <- function(f, suffix = "") {
  file.path(dirname(f), paste0(tools::file_path_sans_ext(basename(f)), suffix,
                               ".", tools::file_ext(f)))
}

#' Download and optionally reproject, crop, mask raw data and output module inputs
#'
#' This function can be used to prepare module inputs from raw data. It
#' runs several other functions, conditionally and sequentially:
#' \code{downloadFromWebDB} or \code{downloadData} if used within a module. If used
#' outside of a SpaDES module, then it will use \code{file.download} or
#' \code{googledrive::drive_download} if the \code{url} has \code{https://drive.google.com}.
#' If the download is a .zip or .tar file (i.e., an archive), then the function will
#' run \code{extractFromArchive}. Then it will sequentially try to load the extracted file (if
#' passed as \code{targetFile}). If the default fun and pkg are not left as is,
#' the function will try to use those. If the file is not a raster file, it will try
#' using raster::shapefile. NOTE: This function is still experimental: use
#' with caution.
#' with caution.
#'
#' @param targetFile Character string giving the path to the eventual
#'                   file (raster, shapefile, csv, etc.) that will be
#'                   downloaded, extracted.
#'
#' @param archive Optional character string giving the path of an archive
#' containing \code{targetFile}, or a vector giving a set of nested archives
#' (e.g., \code{c("xxx.tar", "inner.zip")}). If there is/are (an) inner archive(s),
#' but they are unknown, the function will try all until it finds the
#' \code{targetFile}
#'
#' @param url Optional character string indicating the URL to download from. Normally,
#' if used within a module, this url should be explicitly given as sourceURL for an
#' \code{expectsInput}. In that case, it will use the module's checksums file to
#' confirm that the download occurred correctly. If URL is used here, an ad hoc
#' checksums will be created in the \code{destinationPath}. This will be used in
#' subsequent calls to \code{prepInputs}, comparing the file on hand with the ad hoc
#' \code{checksums.txt}.
#'
#' @param alsoExtract Optional character string naming files other than
#' \code{targetFile} that must be extracted from the \code{archive}.
#'
#' @param dataset Optional character string representing the dataset of interest
#' for download. Allows for restricting the lookup for the url to a dataset,
#' thus avoiding filename collision.
#'
#' @param destinationPath Character string of where to download to, and do
#'                        all writing of files in.
#'
#' @param fun Character string indicating the function to use to load #' \code{targetFile}.
#'
#' @param pkg Character string indicating the package in which to find \code{fun}.
#'
#' @inheritParams cropReprojInputs
#'
#' @inheritParams writeInputsOnDisk
#'
#' @param writeCropped Logical. Should the output be written to disk?
#'
#' @inheritParams reproducible::Cache
#'
#' @param quickCheck Logical. If \code{TRUE}, then all Caching that occurs will
#'                   be based on the much faster but less robust file.info. \code{FALSE},
#'                   the default, uses \code{\link[digest]{digest}}
#'
#' @param cacheTags Character vector with Tags. These Tags will be added to the
#' repository along with the artifact.
#'
#' @param purge When prepInputs is called from outside a module, it will write a \code{CHECKSUMS.txt}
#'              file. If there is an incorrect \code{CHECKSUMS.txt}, this will purge it.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur even if they
#'                  pass the checksums or the files are all there.
#'
#' @param ... Passed to pkg::fun
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom reproducible Cache compareNA asPath
#' @importFrom googledrive drive_get drive_auth drive_download as_id
#' @rdname prepInputs
#' @examples
#' # This function works within a module, when "sourceURL" is supplied
#' #   in the metadata in the "expectsInputs(..., sourceURL = ""), but can
#' # also run outside a module, e.g., with url argument.
#' \dontrun{
#' # Put chunks like this in your .inputObjects
#' if (!suppliedElsewhere("test", sim))
#'   sim$test <- Cache(prepInputs, "raster.tif", "downloadedArchive.zip",
#'                     destinationPath = dataPath(sim), studyArea = sim$studyArea,
#'                     rasterToMatch = sim$otherRasterTemplate, overwrite = TRUE)
#'
#' # download a zip file from internet, unzip all files, load as shapefile, Cache the call
#' # First time: don't know all files - prepInputs will guess, if download file is an archive,
#' #   then extract all files, then if there is a .shp, it will load with raster::shapefile
#' dPath <- file.path(tempdir(), "ecozones")
#' shpEcozone <- prepInputs(destinationPath = dPath,
#'                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#'
#' # Once this is done, can be more precise in operational code:
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' ecozoneFiles <- dir(dPath, pattern = "ecozones.") # not CHECKSUMS.txt or .zip file
#' shpEcozone <- Cache(prepInputs,
#'                     url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                     targetFile = asPath(ecozoneFilename),
#'                     alsoExtract = asPath(ecozoneFiles),
#'                     fun = "shapefile", destinationPath = dPath)
#'
#' #' # Add a study area to Crop and Mask to
#' # Create a "study area"
#' library(SpaDES.tools)
#' StudyArea <- randomPolygon(x = sp::SpatialPoints(matrix(c(-110, 60), ncol=2)), 1e8)
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' ecozoneFiles <- dir(dPath, pattern = "ecozones.") # not CHECKSUMS.txt or .zip file
#' shpEcozoneSm <- Cache(prepInputs,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                          targetFile = asPath(ecozoneFilename),
#'                          alsoExtract = asPath(ecozoneFiles),
#'                          studyArea = StudyArea,
#'                          fun = "shapefile", destinationPath = dPath)
#'
#' dev();
#' Plot(shpEcozone)
#' Plot(shpEcozoneSm, addTo = "shpEcozone", col = "red")
#'
#' # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
#' #   but the StudyArea does get reprojected, need to use rasterToMatch
#' lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
#' url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
#'                  "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
#' dPath <- file.path(tempdir(), "LCC")
#' LCC2005 <- Cache(prepInputs, url = url,
#'                      #targetFile = lcc2005Filename,
#'                      #archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#' Plot(LCC2005)
#'
#' # Specifying more args
#' LCC2005 <- Cache(prepInputs, url = url,
#'                      targetFile = lcc2005Filename,
#'                      archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#' }
#'
prepInputs <- function(targetFile, url = NULL, archive = NULL, alsoExtract = NULL,
                       dataset = NULL, destinationPath = ".", fun = "raster",
                       pkg = "raster", studyArea = NULL, rasterToMatch = NULL,
                       rasterInterpMethod = "bilinear", rasterDatatype = "INT2U",
                       writeCropped = TRUE, addTagsByObject = NULL, overwrite = FALSE,
                       quickCheck = getOption("reproducible.quick", FALSE),
                       cacheTags = "", purge = FALSE, ...) {
  if (!missing(targetFile)) {
    targetFile <- basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
  } else {
    targetFile <- NULL
    targetFilePath <- NULL
  }

  if (purge) unlink(file.path(destinationPath, "CHECKSUMS.txt"))

  if (!dir.exists(destinationPath)) checkPath(destinationPath, create = TRUE)
  message("Preparing: ", targetFile)

  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  emptyChecksums <- data.table(expectedFile = character(), result = character())

  if (!is.null(archive)) {
    archive <- file.path(destinationPath, basename(archive))
    filesToCheck <- c(targetFilePath, archive)
  } else {
    if (!is.null(url)) {
      possibleArchiveName <- file.path(destinationPath, basename(url))
      archive <- .isArchive(possibleArchiveName)
    }
    filesToCheck <- targetFilePath
  }

  # If quickCheck, then use file.info as part of cache/memoise ... otherwise,
  #   pass a random real number to make a new memoise
  if (is.null(url)) {
    fileinfo <- if (quickCheck) file.info(filesToCheck) else runif(1)
    if (file.exists(checkSumFilePath)) {
      out <- .checkSumsMem(asPath(filesToCheck), fileinfo,
                   asPath(checkSumFilePath),
                   quick = quickCheck, cacheTags, quickCheck)
      moduleName <- out$moduleName
      modulePath <- out$modulePath
      checkSums <- out$checkSums
    } else {
      checkSums <- out <- emptyChecksums
    }

  } else {
    out <- tryCatch(checksums(path = destinationPath, write = FALSE), error = function(p) {
      emptyChecksums
      on.exit({checksums(path = destinationPath, write = TRUE)})
    })
    checkSums <- out
  }
  rm(out)

  if (!is.null(targetFile)) {
    result <- checkSums[checkSums$expectedFile == targetFile, ]$result
  } else {
    result <- unique(checkSums$result)
  }
  mismatch <- !isTRUE(result == "OK")

  if (mismatch) {
    fileToDownload <- if (is.null(archive[1])) {
      if (is.null(dataset)) {
        targetFile
      } else {
        downloadFromWebDB(targetFile, file.path(destinationPath, targetFile), quickCheck = quickCheck)
        NULL
      }
    } else {
      result <- checkSums[checkSums$expectedFile == basename(archive), ]$result
      mismatch <- !isTRUE(result == "OK")

      if (mismatch) {
        if (is.null(dataset)) {
          archive[1]
        } else {
          downloadFromWebDB(basename(archive), archive, quickCheck = quickCheck)
          NULL
        }
      } else {
        NULL
      }
    }

    # The download step
      if (exists("moduleName")) { # means it is inside a SpaDES module
        if (!is.null(fileToDownload)) {
          downloadData(moduleName, modulePath, files = fileToDownload,
                     checked = checkSums, quickCheck = quickCheck, overwrite = TRUE)
        }
      } else {
        # The ad hoc case
        if (!is.null(fileToDownload) || is.null(targetFile)) {
          if (!is.null(url)) {
            if (grepl("drive.google.com", url)) {
              googledrive::drive_auth() ## needed for use on e.g., rstudio-server
              if (is.null(archive)) {
                fileAttr <- googledrive::drive_get(googledrive::as_id(url))
                archive <- .isArchive(fileAttr$name)
                archive <- file.path(destinationPath, basename(archive))
                downloadFilename <- archive
                if (is.null(archive)) {
                  if (is.null(targetFile)) {
                    # make the guess
                    targetFile <- fileAttr$name
                    downloadFilename <- targetFile # override if the targetFile is not an archive
                  }
                }
              }
              destFile <- file.path(tempdir(), basename(downloadFilename))
              message("Downloading from google drive")
              googledrive::drive_download(googledrive::as_id(url), path = destFile,
                                          overwrite = TRUE, verbose = TRUE)
            } else {
              destFile <- file.path(tempdir(), basename(url))
              download.file(url, destfile = destFile)
            }
            file.copy(destFile, destinationPath)
            file.remove(destFile)
            on.exit({checksums(path = destinationPath, write = TRUE)})
          }
        }
      }

    if (!is.null(archive)) {
      extractFromArchive(archive = archive, destinationPath = destinationPath,
                         needed = c(targetFile, if (!is.null(alsoExtract)) basename(alsoExtract)))
      if (is.null(targetFile)) {
        on.exit({checksums(path = destinationPath, write = TRUE)})
      }
    }
  }

  # Now that all files are downloaded and extracted from archive, deal with missing targetFilePath
  if (is.null(targetFilePath)) {
    filesInDestPath <- dir(destinationPath)
    isShapefile <- grepl("shp", fileExt(filesInDestPath))
    isRaster <- fileExt(filesInDestPath) %in% c("tif", "grd")
    message("targetFile was not specified. ", if (any(isShapefile)) {
              c(" Trying raster::shapefile on ", filesInDestPath[isShapefile],
                ". If that is not correct, please specify different targetFile",
                " and/or pkg & fun")
            } else {
              c(" Trying ", pkg, "::", fun,
                ". If that is not correct, please specify a targetFile",
                " and/or different pkg & fun. The current files in the destinationPath are: \n",
                paste(filesInDestPath, collapse = "\n"))
            })
    if (fun == "raster") { #i.e., the default
      if (any(isShapefile)) {
        fun <- "shapefile"
      }
    }

    guessAtFileToLoad <- if ("shapefile" %in% fun ) {
      filesInDestPath[isShapefile]
    } else {
      if (any(isRaster)) {
        filesInDestPath[isRaster]
      } else {
        message("Don't know which file to load. Please specify targetFile")
      }

    }
    message("Trying ", guessAtFileToLoad, " with ", pkg, "::", fun)
    targetFile <- guessAtFileToLoad
    targetFilePath <- file.path(destinationPath, targetFile)
  }


  f <- getFromNamespace(fun, pkg)

  if (fun == "raster" && pkg == "raster") {
    x <- f(targetFilePath, ...)
  } else {
    x <- Cache(f, asPath(targetFilePath), userTags = cacheTags, ...)
  }

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <- if (!is.null(rasterToMatch)) {
        crs(rasterToMatch)
      } else if (!is.null(studyArea)) {
        crs(studyArea)
      } else {
        if (is(x, "sf"))
          if (requireNamespace("sf")) {
            CRS(sf::st_crs(x)$proj4string)
          } else {
            stop("Please install sf package: https://github.com/r-spatial/sf")
          }
        else
          crs(x)

      }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS,
                           quick = quickCheck, userTags = cacheTags)
      }
    }

    x <-
      cropReprojInputs(
      x = x,
      studyArea = studyArea,
      rasterToMatch = rasterToMatch,
      rasterInterpMethod = rasterInterpMethod)

    if (!is.null(studyArea)) {
      if (is(x, "RasterLayer") ||
          is(x, "RasterStack") ||
          is(x, "RasterBrick")) {
        x <-
          maskInputs( # don't Cache because slow to write
            x = x,
            studyArea = if (identical(raster::crs(studyArea), raster::crs(x))) {
              studyArea
            } else {
              sp::spTransform(x = studyArea, CRSobj = raster::crs(x),
                              userTags = cacheTags)
            }
          )
      }
    }

    if (writeCropped) {
      smallFN <- .prefix(targetFilePath, "Small")

      xTmp <- Cache(
        writeInputsOnDisk,
        x = x,
        filename = smallFN,
        rasterDatatype = rasterDatatype,
        quick = quickCheck,
        userTags = cacheTags,
        notOlderThan = Sys.time() # Too many reasons why this doesn't work properly
      )

      if (is(xTmp, "Raster")) { # Rasters need to have their disk-backed value assigned, but not shapefiles
        # This is a bug in writeRaster was spotted with crs of xTmp became
        # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
        # should have stayed at
        # +proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0
        if (!identical(crs(xTmp), crs(x)))
          crs(xTmp) <- crs(x)

        x <- xTmp
      }
    }
  }
  x
}

#' Reproject, crop module inputs
#'
#' This function can be used to crop or reproject module inputs from raw data.
#'
#' @param x A \code{Spatial*}, \code{sf}, or \code{Raster*} object.
#'
#' @param studyArea Template \code{SpatialPolygons*} object used for masking, after cropping.
#'                  If not in same CRS, then it will be \code{spTransform}ed to
#'                  CRS of \code{x} before masking.
#'
#' @param rasterToMatch Template \code{Raster*} object used for reprojecting and
#' cropping.
#'
#' @param rasterInterpMethod Method used to compute values for the new
#' \code{RasterLayer}. See \code{\link[raster]{projectRaster}}. Defaults to bilinear.
#'
#' @param addTagsByObject Pass any object in there for which there is a
#' \code{.tagsByClass} function
#'
#' @param cacheTags Character vector with Tags. These Tags will be added to the
#' repository along with the artifact.
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster buffer crop crs extent projectRaster res crs<-
#' @importFrom rgeos gIsValid
#' @importFrom reproducible Cache
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @rdname cropReprojInputs
#'
cropReprojInputs <- function(x, studyArea = NULL, rasterToMatch = NULL,
                             rasterInterpMethod = "bilinear",
                             addTagsByObject = NULL, cacheTags = NULL) {
  message("  Cropping, reprojecting")

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <- if (!is.null(rasterToMatch)) {
      crs(rasterToMatch)
    # } else if (!is.null(studyArea)) {
    #   crs(studyArea)
    } else {
      if (is(x, "sf")) {
        if (requireNamespace("sf")) {
          CRS(sf::st_crs(x)$proj4string)
        } else {
          stop("Please install sf package: https://github.com/r-spatial/sf")
        }
      }

      else
        crs(x)
    }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        studyArea <- spTransform(x = studyArea, CRSobj = targetCRS, userTags = cacheTags)
      }
    }

    if (is(x, "RasterLayer") ||
        is(x, "RasterStack") ||
        is(x, "RasterBrick")) {

      if (!is.null(studyArea)) {
        x <-
          crop( # don't Cache because slow to write
          x = x,
          y = if (identical(raster::crs(studyArea), raster::crs(x))) {
            studyArea
          } else {
            sp::spTransform(x = studyArea, CRSobj = raster::crs(x),
                  userTags = cacheTags)
          }
        )
      }

      if (!is.null(rasterToMatch)) {
        if (!identical(crs(x), targetCRS) |
            !identical(res(x), res(rasterToMatch)) |
            !identical(extent(x), extent(rasterToMatch))) {
          x <- projectRaster(from = x, to = rasterToMatch,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      } else {
        if (!identical(crs(x), targetCRS)) {
          x <- projectRaster(from = x, crs = targetCRS, res = res(x),
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      }
    } else if (inherits(x, "SpatialPoints") ||
               inherits(x, "SpatialLines") ||
               inherits(x, "SpatialPolygons")) {
      if (inherits(x, "SpatialPolygons") && !suppressWarnings(gIsValid(x))) {
        xValid <- buffer(x, dissolve = FALSE, width = 0)#, userTags = cacheTags)
        x <- if (.hasSlot(x, "data")) xValid
             else SpatialPolygonsDataFrame(Sr = xValid,
                        data = as.data.frame(x))#, userTags = cacheTags)
      }

      if (!identical(targetCRS, crs(x)))
        x <- spTransform(x = x, CRSobj = targetCRS)#, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- crop(x, studyArea, userTags = cacheTags) # don't Cache because slow to write
      }

      if (!is.null(rasterToMatch)) {
        x <- crop(x, rasterToMatch, userTags = cacheTags) # don't Cache because slow to write
      }
    } else if (is(x, "sf")) {
      if (requireNamespace("sf")) {
        if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(x))) {
          x[!isValid] <- Cache(sf::st_buffer, x[!isValid], dist = 0, userTags = cacheTags)
        }

        if (!identical(sf::st_crs(targetCRS@projargs), sf::st_crs(x)))
          x <- Cache(sf::st_transform, x = x, crs = sf::st_crs(targetCRS@projargs), userTags = cacheTags)

        if (!is.null(studyArea)) {
          x <- Cache(sf::st_intersection, x, sf::st_as_sf(studyArea), userTags = cacheTags)
        }

        if (!is.null(rasterToMatch)) {
          x <- Cache(sf::st_intersection, x, sf::st_as_sf(as(extent(rasterToMatch), "SpatialPolygons")),
                     userTags = cacheTags)
        }
      } else {
        stop("Please install sf package: https://github.com/r-spatial/sf")
      }
    } else {
      stop("Class '", class(x), "' is not supported.")
    }
  }
  x
}

#' Mask module inputs
#'
#' This function can be used to mask module inputs from raw data.
#'
#' @param x          A \code{Raster*} object
#'
#' @param studyArea  A \code{SpatialPolygons*} object
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom reproducible Cache
#' @importFrom SpaDES.tools fastMask
#' @rdname maskInputs
#'
maskInputs <- function(x, studyArea) {
  message("  Masking")

  if (!is(studyArea, "SpatialPolygonsDataFrame")) {
    studyArea <- SpatialPolygonsDataFrame(Sr = studyArea, data = data.frame(ID = seq(length(studyArea))),
                                  match.ID = FALSE)
  }
  fastMask(x = x, y = studyArea)
}

#' Write module inputs on disk
#'
#' Can be used to write prepared inputs on disk.
#'
#' @param x  A \code{Spatial*}, \code{sf} or \code{Raster*} object.
#'
#' @inheritParams raster::writeRaster
#'
#' @param rasterDatatype Output data type. Passed to \code{\link[raster]{writeRaster}}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @importFrom reproducible Cache
#' @rdname writeInputsOnDisk
#'
writeInputsOnDisk <- function(x, filename, rasterDatatype = NULL) {
  if (is(x, "RasterLayer") ||
      is(x, "RasterStack") ||
      is(x, "RasterBrick")) {

    writeRaster(x = x, overwrite = TRUE, format = "GTiff",
                        datatype = rasterDatatype, filename = filename)
  } else if (inherits(x, "SpatialPoints") ||
             inherits(x, "SpatialLines") ||
             inherits(x, "SpatialPolygons")) {
    shapefile(x = x, overwrite = TRUE, filename = filename)
  } else if (is(x, "sf")) {
    if (requireNamespace("sf")) {
      sf::st_write(obj = x, delete_dsn = TRUE, dsn = filename)
    } else {
      stop("Please install sf package: https://github.com/r-spatial/sf")
    }

  } else {
    stop("Don't know how to write object of class ", class(x), " on disk.")
  }
}

#' @importFrom utils untar unzip
.whichExtractFn <- function(archive, args) {
  ext <- tolower(file_ext(archive))
  if (ext == "zip") {
    fun <- unzip
    args <- c(args, list(junkpaths = TRUE))
  } else if (ext == "tar") {
    fun <- untar
  }
  return(list(fun = fun, args = args))
}

.unzipOrUnTar <- function(fun, args, files) {
  do.call(fun, c(args, list(files = files)))
}


.checkSums <- function(filesToCheck, fileinfo, chksumsFilePath, quick, cacheTags, quickCheck) {
  if (missing(chksumsFilePath)) {
    chksumsFilePath <- file.path(dirname(filesToCheck), "CHECKSUMS.txt")
  }
  moduleName <- basename(dirname(dirname(chksumsFilePath)))
  modulePath <- dirname(dirname(dirname(chksumsFilePath)))
  checkSums <- checksums(files = filesToCheck,
                         module = moduleName,
                         path = modulePath,
                         checksumFile = asPath(chksumsFilePath),
                         write = FALSE,
                         quickCheck = quickCheck
  )
  list(moduleName = moduleName, modulePath = modulePath, checkSums = checkSums)
}

.checkSumsMem <- memoise::memoise(.checkSums)

.isArchive <- function(filename) {
  archive = if (fileExt(filename) %in% c("zip", "tar")) {
    filename
  } else {
    NULL
  }

}
