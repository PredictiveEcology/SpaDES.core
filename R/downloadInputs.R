if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("expectedFile", "objName", "V1"))
}

#' Download and optionally post process files
#'
#' This function can be used to prepare R objects from remote or local data sources.
#' The object of this function is to provide a reproducible version of a series of
#' commonly used steps for getting, loading, and processing data. In the case of
#' \code{Spatial*} and \code{Raster*} objects, then the postprocessing may include
#' cropping, reprojecting, masking, if the appropriate arguments are used. See examples.
#'
#' \code{prepInputs} has some functionality of its own; however, its primary purpose
#' is to run in sequence: \code{downloadFile}, \code{extractFromArchive}, \code{fun},
#' \code{postProcess}.
#' runs several other functions, conditionally and sequentially:
#' \code{downloadFromWebDB} or \code{downloadData} if used within a module. If used
#' outside of a SpaDES module, then it will use \code{file.download} or
#' \code{googledrive::drive_download} if the \code{url} has \code{https://drive.google.com}.
#' If the download is a .zip or .tar file (i.e., an archive), then the function will
#' run \code{extractFromArchive}. Then it will sequentially try to load the extracted file (if
#' passed as \code{targetFile}). If the default \code{fun} is not left as is,
#' the function will try to use those. If the file is not a raster file, it will try
#' using raster::shapefile. NOTE: This function is still experimental: use
#' with caution.
#'
#'
#' @section 3. postProcessing of \code{Raster*} and \code{Spatial*} objects:
#'
#' If \code{rasterToMatch} or \code{studyArea} are used, then this will trigger several
#' subsequent functions, specifically the sequence, \emph{Crop, reproject, mask}, which appears
#' to be a common sequence in spatial simulation. See \code{\link{postProcess.spatialObjects}}.
#'
#'
#' @param targetFile Character string giving the path to the eventual
#'                   file (raster, shapefile, csv, etc.) after downloading and extracting from
#'                   a zip or tar archive. This is the file \emph{before}
#'                   it is passed to \code{postProcess}. Currently, the internal
#'                   checksumming does not checksum the file after it is
#'                   \code{postProcess}ed (e.g., cropped/reprojected/masked).
#'                   Using \code{Cache} around \code{prepInputs} will do a
#'                   sufficient job in these cases.
#'
#' @param archive Optional character string giving the path of an archive
#'                containing \code{targetFile}, or a vector giving a set of nested archives
#'                (e.g., \code{c("xxx.tar", "inner.zip")}). If there is/are (an) inner archive(s),
#'                but they are unknown, the function will try all until it finds the
#'                \code{targetFile}
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
#' @param destinationPath Character string of where to download to, and do
#'                        all writing of files in.
#'
#' @param fun Character string indicating the function to use to load \code{targetFile} into
#'            an \code{R} object.
#'
#' @param quick Logical. This is passed internally to \code{\link{checksums}} and
#'                   \code{\link{downloadData}} (the quickCheck argument for both),
#'                   and to \code{\link{Cache}} (the quick argument). This results in
#'                   faster, though less robust checking of inputs. See the respective
#'                   functions.
#'
#' @param purge When prepInputs is called from outside a module, it will write a \code{CHECKSUMS.txt}
#'              file. If there is an incorrect \code{CHECKSUMS.txt}, this will purge it.
#'
#' @param overwrite Logical. Should downloading and all the other actions occur even if they
#'                  pass the checksums or the files are all there.
#'
#' @param ... Additional arguments that can be passed to
#'            \code{fun}, \code{\link{fixErrors}} and \code{\link{postProcess}} and
#'            also \code{\link[reproducible]{Cache}}
#'            (if \code{useCache = TRUE}, which is the default unless
#'            \code{options(reproducible.useCache = FALSE)}). Since \code{...} is passed to
#'            \code{\link{postProcess}}, these may be passed into other functions.
#'            See details and examples.
#'
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom reproducible Cache compareNA asPath
#' @importFrom R.utils isAbsolutePath
#' @importFrom utils methods
#' @importFrom googledrive drive_get drive_auth drive_download as_id
#' @rdname prepInputs
#' @seealso \code{\link{downloadFile}}, \code{\link{extractFromArchive}},
#'          \code{\link{downloadFile}},  \code{\link{postProcess}}.
#' @examples
#' # This function works within a module, however, currently,
#' #   "sourceURL" is not yet working as desired. Use url.
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
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#'
#' # Robust to partial file deletions:
#' unlink(dir(dPath, full.names = TRUE)[1:3])
#' shpEcozone <- prepInputs(destinationPath = dPath,
#'                      url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip")
#' unlink(dPath, recursive = TRUE)
#'
#' # Once this is done, can be more precise in operational code:
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozone <- prepInputs(targetFile = asPath(ecozoneFilename),
#'                     url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                     alsoExtract = asPath(ecozoneFiles),
#'                     fun = "shapefile", destinationPath = dPath)
#' unlink(dPath, recursive = TRUE)
#'
#' #' # Add a study area to Crop and Mask to
#' # Create a "study area"
#' library(SpaDES.tools)
#' StudyArea <- randomPolygon(x = sp::SpatialPoints(matrix(c(-110, 60), ncol=2)), 1e8)
#'
#' #  specify targetFile, alsoExtract, and fun, wrap with Cache
#' ecozoneFilename <- file.path(dPath, "ecozones.shp")
#' # Note, you don't need to "alsoExtract" the archive... if the archive is not there, but the
#' #   targetFile is there, it will not redownload the archive.
#' ecozoneFiles <- c("ecozones.dbf", "ecozones.prj",
#'                   "ecozones.sbn", "ecozones.sbx", "ecozones.shp", "ecozones.shx")
#' shpEcozoneSm <- Cache(prepInputs,
#'                          url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
#'                          targetFile = asPath(ecozoneFilename),
#'                          alsoExtract = asPath(ecozoneFiles),
#'                          studyArea = StudyArea,
#'                          fun = "shapefile", destinationPath = dPath)
#'
#' dev();
#' Plot(shpEcozone)
#' Plot(shpEcozoneSm, addTo = "shpEcozone", gp = gpar(col = "red"))
#' unlink(dPath)
#'
#' # Big Raster, with crop and mask to Study Area - no reprojecting (lossy) of raster,
#' #   but the StudyArea does get reprojected, need to use rasterToMatch
#' lcc2005Filename <- file.path(dPath, "LCC2005_V1_4a.tif")
#' url <- file.path("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover",
#'                  "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip")
#' dPath <- file.path(tempdir(), "LCC")
#'
#' # messages received below may help for filling in more arguments in the subsequent call
#' LCC2005 <- prepInputs(url = url,
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#'
#' Plot(LCC2005)
#'
#' # Specifying more args -- can wrap with Cache
#' LCC2005 <- Cache(prepInputs, url = url,
#'                      targetFile = lcc2005Filename,
#'                      archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#'                      destinationPath = asPath(dPath),
#'                      studyArea = StudyArea)
#' }
#'
prepInputs <- function(targetFile, url = NULL, archive = NULL, alsoExtract = NULL,
                       destinationPath = ".", fun = NULL,
                       quick = getOption("reproducible.quick"),
                       overwrite = FALSE, purge = FALSE,
                       ...) {

  dots <- list(...)

  if (!is.null(dots$cacheTags))  {
    message("cacheTags is deprecated; use userTags which will pass directly to Cache")
    dots$userTags <- dots$cacheTags
    dots$cacheTags <- NULL
  }
  if (!is.null(dots$rasterInterpMethod))  {
    message("cacheTags is deprecated; use userTags which will pass directly to Cache")
    dots$method <- dots$rasterInterpMethod
    dots$rasterInterpMethod <- NULL
  }
  if (!is.null(dots$rasterDatatype))  {
    message("cacheTags is deprecated; use userTags which will pass directly to Cache")
    dots$datatype <- dots$rasterDatatype
    dots$rasterDatatype <- NULL
  }
  if (!is.null(dots$pkg))  {
    message("pkg is deprecated; name the package and function directly, ",
            "if needed, e.g., 'pkg::fun'")
    fun <- paste0(dots$pkg, "::", fun)
    dots$pkg <- NULL
  }
  # remove trailing slash -- causes unzip fail if it is there
  destinationPath <- gsub("\\\\$|/$", "", destinationPath)

  if (!missing(targetFile)) {
    targetFile <- basename(targetFile)
    targetFilePath <- file.path(destinationPath, targetFile)
  } else {
    targetFile <- NULL
    targetFilePath <- NULL
  }

  checkSumFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  if (purge) unlink(checkSumFilePath)

  if (!dir.exists(destinationPath)) checkPath(destinationPath, create = TRUE)
  message("Preparing: ", targetFile)

  emptyChecksums <- data.table(expectedFile = character(), result = character())
  needChecksums <- 0

  if (!is.null(archive)) {
    archive <- file.path(destinationPath, basename(archive))
    filesToCheck <- c(targetFilePath, archive)
  } else {
    if (!is.null(url)) {
      archive <- .isArchive(file.path(destinationPath, basename(url)))
    }
    filesToCheck <- targetFilePath
  }

  # If quick, then use file.info as part of cache/memoise ... otherwise,
  #   pass a random real number to make a new memoise
  moduleName <- NULL
  modulePath <- NULL
  if (is.null(url)) { # the only way for this to be useful is if there is a SpaDES module
                      # and url can be gotten during downloadData from module metadata
    fileinfo <- if (quick) file.info(filesToCheck) else runif(1)
    if (file.exists(checkSumFilePath)) {
      out <- .checkSumsMem(asPath(filesToCheck), fileinfo,
                           asPath(checkSumFilePath), quick = quick)
      moduleName <- out$moduleName
      modulePath <- out$modulePath
      checkSums <- out$checkSums
    } else {
      checkSums <- out <- emptyChecksums
    }

  } else {
    checkSums <- try(checksums(path = destinationPath, write = FALSE)#, checksumFile = checkSumFilePath)
                     , silent = TRUE)
    if (is(checkSums, "try-error")) {
      needChecksums <- 1
      checkSums <- emptyChecksums
    }
  }

  neededFiles <- c(targetFile, if (!is.null(alsoExtract)) basename(alsoExtract))
  # Download
  downloadFileResult <- downloadFile(archive, targetFile, neededFiles = neededFiles,
               destinationPath, quick, checkSums, url, needChecksums = needChecksums,
               overwrite = overwrite, moduleName = moduleName, modulePath = modulePath)
  needChecksums <- downloadFileResult$needChecksums
  neededFiles <- downloadFileResult$neededFiles
  if (is.null(archive)) archive <- downloadFileResult$archive

  filesToChecksum <- if (is.null(archive)) character() else basename(archive)
  on.exit({
    if (needChecksums > 0) {
      if (needChecksums == 2) { # a checksums file already existed, need to keep some of it
        cs <- try(read.table(checkSumFilePath, header = TRUE), silent = TRUE)
        if (is(cs, "try-error")) { # meant that it was an empty CHECKSUMS.txt file -- rebuild it
          needChecksums <- 1
        } else {
          nonCurrentFiles <- cs %>%
            filter(!file %in% filesToChecksum)
        }
      }
      currentFiles <- checksums(path = destinationPath, write = TRUE, #checksumFile = checkSumFilePath,
                                files = file.path(destinationPath, filesToChecksum))
      if (needChecksums == 2) { # a checksums file already existed, need to keep some of it
        currentFiles <- rbind(nonCurrentFiles, currentFiles)
        writeChecksumsTable(currentFiles, checkSumFilePath, dots = list())
      }
    }
  })
  # Extract from archive
  filesExtracted <- extractFromArchive(archive = archive, destinationPath = destinationPath,
                       neededFiles = neededFiles,
                     checkSums = checkSums, needChecksums = needChecksums, ...)
  filesToChecksum <- unique(c(filesToChecksum, targetFile, alsoExtract,
                              basename(filesExtracted$filesExtracted)))
  needChecksums <- filesExtracted$needChecksums


  #targetFilePath might still be NULL, need destinationPath too
  targetParams <- .guessAtTargetAndFun(targetFilePath, destinationPath,
                                 filesExtracted$filesExtracted,
                                 fun) # passes through if all known
  targetFile <- basename(targetParams$targetFilePath)
  targetFilePath <- targetParams$targetFilePath
  fun <- targetParams$fun

  # Now that all files are downloaded and extracted from archive, deal with missing targetFilePath
  tryRasterFn <- if (endsWith(suffix = "raster", fun)) TRUE else FALSE

  # fun is a charcter string, convert to function
  if (grepl("::", fun)) {
    fun2 <- strsplit(fun, "::")[[1]]
    pkg <- fun2[1]
    fun <- fun2[2]
    fun <- getFromNamespace(fun, pkg)
  } else {
    fun <- get(fun)
  }


  # dots will contain too many things for some functions -- need to remove those that are known going
  #   into prepInputs
  argsToRemove <- unique(c(names(formals(prepInputs)),
                           names(formals(fixErrors)),
                           names(formals(writeRaster)),
                           names(formals(projectRaster)),
                           unlist(lapply(methods("postProcess"), function(x) names(formals(x))))))
  args <- dots[!(names(dots) %in% argsToRemove)]
  if (length(args) == 0) args <- NULL

  if (tryRasterFn) {
    # Don't cache the reading of a raster -- normal reading of raster on disk is fast b/c only reads metadata
    x <- do.call(fun, append(list(asPath(targetFilePath)), args))
  } else {
    x <- Cache(do.call, fun, append(list(asPath(targetFilePath)), args))
    #x <- Cache(fun, asPath(targetFilePath), ...)
  }

  # fix errors if methods available
  fixErrors(x, targetFile = targetFile, ...)

  # postProcess
  out <- postProcess(x, targetFilePath = targetFilePath, destinationPath = destinationPath,
                     ...)
  return(out)
}


#' Do some minor error fixing
#'
#' These must be very common for this function to be useful. Currently, the only
#' meaningful method is on SpatialPolygons, and it runs \code{rgeos::gIsValid}. If
#' \code{FALSE}, then it runs a buffer of width 0.
#' @inheritParams prepInputs
#' @param x Any object that could be fixed for errors.
#'          See \code{\link{fixErrors.SpatialPolygons}}
#' @export
#' @keywords internal
#' @param ... None used currently
#' @param attemptErrorFixes Will attempt to fix known errors. Currently only some failures
#'        for SpatialPolygons* are attempted. Notably with \code{raster::buffer(..., width = 0)}.
#'        Default \code{TRUE}, though this may not be the right action for all cases.
#'  @examples
fixErrors <- function(x, targetFile, attemptErrorFixes = TRUE, ...) {
  UseMethod("fixErrors")
}

#' @export
#' @keywords internal
fixErrors.default <- function(x, targetFile, attemptErrorFixes = TRUE, ...) {
  x
}

#' Fix \code{rgeos::gIsValid} failures in \code{SpatialPolygons}
#'
#' This uses \code{raster::buffer(..., width = 0)} internally, which fixes some
#' failures to \code{rgeos::gIsValid}
#'
#' @export
#' @param x A \code{SpatialPolygons} object
#' @inheritParams fixErrors
fixErrors.SpatialPolygons <- function(x, targetFile, attemptErrorFixes = TRUE, ...) {
  if (attemptErrorFixes) {
    if (is(x, "SpatialPolygons")) {
      message("Checking for errors in ", targetFile)
      if (suppressWarnings(any(!rgeos::gIsValid(x, byid = TRUE)))) {
        message("Found errors in ", targetFile, ". Attempting to correct.")
        x1 <- try(raster::buffer(x, width = 0, dissolve = FALSE))
        if (is(x1, "try-error")) {
          message("There are errors with ", targetFile,
                  ". Couldn't fix them with raster::buffer(..., width = 0)")
        } else {
          x <- x1
          message("  Some or all of the errors fixed")
        }

      } else {
        message("  Found no errors")
      }
    }

  }
  x
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
downloadFromWebDB <- function(filename, filepath, dataset = NULL, quick = FALSE, overwrite = TRUE) {
  urls <- webDatabases(local = quick)

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
      httr::write_disk(filepath, overwrite = overwrite)
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
#' @param destinationPath Character string giving the path where \code{neededFiles} will be
#' extracted. Defaults to the archive directory.
#'
#' @param neededFiles Character string giving the name of the file(s) to be extracted.
#'
#' @param extractedArchives Used internally.
#' @param checkSums A checksums file, e.g., created by checksums(..., write = TRUE)
#' @param needChecksums A numeric, with \code{0} indicating do not write a new checksums,
#'                      \code{1} write a new one,
#'                      \code{2} append new information to existing one.
#' @param ... Passed to \code{unzip} or \code{untar}, e.g., \code{overwrite}
#'
#' @return A character vector listing the paths of the extracted archives.
#'
#' @author Jean Marchal
#' @author Eliot McIntire
#' @importFrom reproducible Cache compareNA
#' @importFrom tools file_ext
#'
extractFromArchive <- function(archive, destinationPath = dirname(archive),
                               neededFiles, extractedArchives = NULL, checkSums, needChecksums,
                               ...) {
  result <- if (!is.null(neededFiles)) {
    checkSums[checkSums$expectedFile %in% basename(neededFiles), ]$result
  } else {
    "NotOK"
  }
  filesExtracted <- character()

  # needs to pass checkSums & have all neededFiles files
  if (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile))) {
    if (!is.null(archive)) {
      args <- list(archive[1], exdir = destinationPath[1])

      funWArgs <- .whichExtractFn(archive[1], args)

      filesInArchive <- funWArgs$fun(archive[1], list = TRUE)

      if ("Name" %in% names(filesInArchive)) {
        filesInArchive <- filesInArchive[filesInArchive$Length != 0,]$Name # for zips, rm directories (length = 0)
      }

      # recheck, now that we have the whole file liast
      if (is.null(neededFiles)) {
        result <- checkSums[checkSums$expectedFile %in% basename(filesInArchive), ]$result
      }
      if (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile)) ||
          NROW(result) == 0) { # don't extract if we already have all files and they are fine
        if (needChecksums == 0) needChecksums <- 2 # use binary addition -- 1 is new file, 2 is append

        if (length(archive) > 1) {
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = basename(archive[2])),
                              ...)
          # recursion, removing one archive
          extractFromArchive(archive[-1], destinationPath = destinationPath,
                             neededFiles = neededFiles, extractedArchives = extractedArchives,
                             checkSums, needChecksums, ...)
        } else if (any(neededFiles %in% basename(filesInArchive)) || is.null(neededFiles)) {
          extractingTheseFiles <- paste(basename(filesInArchive[basename(filesInArchive) %in% neededFiles]),
                                        collapse = ", ")
          if (!nzchar(extractingTheseFiles))
            extractingTheseFiles <- paste0("all files: ", paste(basename(filesInArchive), collapse = ", "))
          message("From:", basename(archive[1]),
                  "  Extracting ", extractingTheseFiles)
          filesExtracted <- c(filesExtracted,
                              .unzipOrUnTar(funWArgs$fun, funWArgs$args,
                                          files = filesInArchive[basename(filesInArchive) %in% neededFiles]))
        } else {
          # don't have a 2nd archive, and don't have our neededFiles file
          isArchive <- grepl(file_ext(filesInArchive), pattern = "(zip|tar)", ignore.case = TRUE)

          if (any(isArchive)) {
            arch <- filesInArchive[isArchive]
            filesExtracted <- c(filesExtracted,
                                .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = arch))

            # lapply(file.path(destinationPath, arch), function(archi)
            #   extractFromArchive(archi, destinationPath, neededFiles, extractedArchives))

            extractedArchives <- c(
              extractedArchives,
              unlist(
                lapply(file.path(destinationPath, arch), function(ap)
                  extractFromArchive(archive = ap, destinationPath = destinationPath,
                                     neededFiles = neededFiles, extractedArchives = extractedArchives,
                                     checkSums, needChecksums, ...))
              )
            )
          }
        }
      } else {
        message("Skipping extractFromArchive because all files already extracted")
      }
    }
  } else {
    message("Skipping extractFromArchive because targetFile already extracted")
  }
  list(extractedArchives = c(extractedArchives, archive),
       filesExtracted = filesExtracted,
       needChecksums = needChecksums)
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





#' Try to pick a file to load
#'
#' @keywords internal
#' @rdname guessAtTarget
#' @name guessAtTarget
#' @inheritParams postProcess
#' @param filesExtracted A character vector of all files that have been extracted (e.g.,
#'                       from an archive)
#' @param destinationPath Full path of the directory where the target file should be
.guessAtTargetAndFun <- function(targetFilePath, destinationPath, filesExtracted, fun) {
  #if (is.null(targetFilePath)) {
    #filesExtracted <- dir(destinationPath)
    possibleFiles <- basename(unique(c(targetFilePath, filesExtracted)))
    isShapefile <- grepl("shp", fileExt(possibleFiles))
    isRaster <- fileExt(possibleFiles) %in% c("tif", "grd")
    if (is.null(fun)) { #i.e., the default
      fun <-if (any(isShapefile)) {
        "raster::shapefile"
      } else {
        "raster::raster"
      }
    }

    message("targetFile was not specified. ", if (any(isShapefile)) {
      c(" Trying raster::shapefile on ", possibleFiles[isShapefile],
        ". If that is not correct, please specify different targetFile",
        " and/or fun")
    } else {
      c(" Trying ", fun,
        ". If that is not correct, please specify a targetFile",
        " and/or different fun. The current files in the targetFilePath's ",
        "directory are: \n",
        paste(possibleFiles, collapse = "\n"))
    })

    guessAtFileToLoad <- if (endsWith(suffix = "shapefile", fun )) {
      possibleFiles[isShapefile]
    } else {
      if (any(isRaster)) {
        possibleFiles[isRaster]
      } else {
        message("Don't know which file to load. Please specify targetFile")
      }

    }
    if (length(guessAtFileToLoad) > 1)  {
      message("More than one possible files to load, ", paste(guessAtFileToLoad, collapse = ", "),
              " Picking the first one. If not correct, specify a targetFile")
      guessAtFileToLoad <- guessAtFileToLoad[1]
    } else {
      message("Trying ", guessAtFileToLoad, " with ", fun)
    }
    targetFile <- guessAtFileToLoad
    targetFilePath <- file.path(destinationPath, targetFile)
  #}

    list(targetFilePath = targetFilePath, fun = fun)
}

#' Generic function to post process objects
#'
#' There may be many methods developed. See e.g.,
#' \code{\link{postProcess.spatialObjects}}
#' @export
#' @keywords internal
#' @param x  An object of postProcessing. See individual methods.
#' @importClassesFrom quickPlot spatialObjects
#' @seealso \code{prepInputs}, \code{\link{postProcess.spatialObjects}}
#' @param targetFilePath Full path of the target file
#' @param ... Passed to internal functions. None implemented for the generic.
#' @inheritParams prepInputs
#'
postProcess <- function(x, ...) {
  UseMethod("postProcess")
}

#' @export
#' @keywords internal
postProcess.default <- function(x, ...) {
  x
}

#' Post processing for \code{spatialObjects}
#'
#' The method for spatialObjects (\code{Raster*} and \code{Spatial*}) will
#' crop, reproject, and mask, in that order.  This function is a wrapper for
#' \code{\link{cropReprojInputs}}, \code{\link{maskInputs}} and
#' \code{\link{writeInputsOnDisk}}, with a decent amount of data manipulating
#' between these calls so that the crs match.
#'
#' @export
#' @inheritParams prepInputs
#' @inheritParams cropReprojInputs
#' @param x A \code{Spatial*}, \code{sf} or \code{Raster*} object.
#' @param targetFilePath Character string. This is used if \code{writeCropped}
#'                       is \code{TRUE}. The resulting cropped filename will be
#'                       \code{.prefix(basename(targetFilePath), "Small")}.
#' @param writeCropped Logical or character string (a file path). If logical,
#'                     then the cropped/masked raster will
#'                     be written to disk with the original targetFile name,
#'                     with \code{croppedFilenamePrefix} prefixed to the
#'                     basename(targetFilename).
#'                     If a character string, it will be the path of the saved raster.
#'                     It will be tested whether it is an absolute or relative path and used
#'                     as is if absolute or prepended with \code{destinationPath} if
#'                     relative.
#'
#' @param ... \code{\link{cropReprojInputs}}, \code{\link{writeInputsOnDisk}} and
#'            \code{\link{Cache}}. These each may then pass \code{...} into
#'            \code{\link[raster]{writeRaster}}, \code{\link[raster]{shapefile}}, or
#'            \code{sf::st_write}. This might include potentially important
#'            arguments like \code{datatype}, \code{format}. Also passed to \code{projectRaster},
#'            with likely important arguments such as \code{method = "bilinear"}
postProcess.spatialObjects <- function(x, targetFilePath, studyArea = NULL, rasterToMatch = NULL, quick,
                                       writeCropped = TRUE, overwrite = TRUE,
                                       destinationPath = tempdir(), ...) {
  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    message("Starting postProcessing")
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
        studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS, ...)
      }
    }

    x <-
      cropReprojInputs(
        x = x,
        studyArea = studyArea,
        rasterToMatch = rasterToMatch, ...)

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
              sp::spTransform(x = studyArea, CRSobj = raster::crs(x))
            }
          )
      }
    }

    # Some assertions
    if (!(is.logical(writeCropped) || is.character(writeCropped))) {
      stop("writeCropped must be logical or character string")
    }

    if (!identical(writeCropped, FALSE)) { # allow TRUE or path
      smallFN <- if (isTRUE(writeCropped) ) {
        .prefix(targetFilePath, "Small")
      } else {
        if (isAbsolutePath(writeCropped)) {
          writeCropped
        } else {
          file.path(destinationPath, basename(writeCropped))
        }

      }

      xTmp <- writeInputsOnDisk(
        x = x,
        filename = smallFN,
        overwrite = overwrite,
        #rasterDatatype = rasterDatatype,
        #notOlderThan = Sys.time(), # Too many reasons why this doesn't work properly
        ...
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

#' Reproject, crop a \code{Spatial*} or \code{Raster*} object
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
#' @param ... Passed to \code{projectRaster} and \code{Cache}
#' cropping.
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
cropReprojInputs <- function(x, studyArea = NULL, rasterToMatch = NULL, ...) {
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
        studyArea <- spTransform(x = studyArea, CRSobj = targetCRS)
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
              sp::spTransform(x = studyArea, CRSobj = raster::crs(x))
            }
          )
      }

      if (!is.null(rasterToMatch)) {
        if (!identical(crs(x), targetCRS) |
            !identical(res(x), res(rasterToMatch)) |
            !identical(extent(x), extent(rasterToMatch))) {
          x <- projectRaster(from = x, to = rasterToMatch, ...)
        }
      } else {
        if (!identical(crs(x), targetCRS)) {
          x <- projectRaster(from = x, crs = targetCRS, res = res(x), ...)
        }
      }
    } else if (inherits(x, "SpatialPoints") ||
               inherits(x, "SpatialLines") ||
               inherits(x, "SpatialPolygons")) {
      if (inherits(x, "SpatialPolygons") && !suppressWarnings(gIsValid(x))) {
        xValid <- buffer(x, dissolve = FALSE, width = 0)
        x <- if (.hasSlot(x, "data")) xValid
        else SpatialPolygonsDataFrame(Sr = xValid,
                                      data = as.data.frame(x))
      }

      if (!identical(targetCRS, crs(x)))
        x <- spTransform(x = x, CRSobj = targetCRS)

      if (!is.null(studyArea)) {
        x <- crop(x, studyArea) # don't Cache because slow to write
      }

      if (!is.null(rasterToMatch)) {
        x <- crop(x, rasterToMatch) # don't Cache because slow to write
      }
    } else if (is(x, "sf")) {
      if (requireNamespace("sf")) {
        if (any(sf::st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- sf::st_is_valid(x))) {
          x[!isValid] <- Cache(sf::st_buffer, x[!isValid], dist = 0, ...)
        }

        if (!identical(sf::st_crs(targetCRS@projargs), sf::st_crs(x)))
          x <- Cache(sf::st_transform, x = x, crs = sf::st_crs(targetCRS@projargs), ...)

        if (!is.null(studyArea)) {
          x <- Cache(sf::st_intersection, x, sf::st_as_sf(studyArea), ...)
        }

        if (!is.null(rasterToMatch)) {
          x <- Cache(sf::st_intersection, x, sf::st_as_sf(as(extent(rasterToMatch), "SpatialPolygons")), ...)
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
#' @inheritParams postProcess
#' @param filename The filename to save the output object to disk (a \code{Raster*} or
#'                 \code{Spatial*} object)
#' @param ... Passed to \code{\link[raster]{writeRaster}}, such as \code{datatype},
#'            and \code{\link[raster]{shapefile}}
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom methods is
#' @importFrom raster shapefile writeRaster
#' @importFrom reproducible Cache
#' @rdname writeInputsOnDisk
#'
writeInputsOnDisk <- function(x, filename, ...) {
  if (is(x, "RasterLayer") ||
      is(x, "RasterStack") ||
      is(x, "RasterBrick")) {

    writeRaster(x = x, filename = filename, ...)
  } else if (inherits(x, "SpatialPoints") ||
             inherits(x, "SpatialLines") ||
             inherits(x, "SpatialPolygons")) {
    shapefile(x = x, filename = filename, ...)
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

.unzipOrUnTar <- function(fun, args, files, overwrite = TRUE) {
  do.call(fun, c(args, list(files = files, overwrite = overwrite)))
}


.checkSums <- function(filesToCheck, fileinfo, chksumsFilePath, quick) {
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
                         quickCheck = quick
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


#' A wrapper around a set of downloading functions
#'
#' Currently, this only deals with \code{\link[googledrive]{drive_download}},
#' \code{\link{downloadData}}, and \code{\link[utils]{download.file}}.
#'
#' @export
#' @inheritParams prepInputs
#' @inheritParams extractFromArchive
#' @param moduleName Character string indicating SpaDES module name from which prepInputs is
#'                    being called
#'                    being called
#' @param modulePath Character string of the path where the \code{moduleName} is located.
#' @author Eliot McIntire
downloadFile <- function(archive, targetFile, neededFiles, destinationPath, quick,
                         checkSums, url, needChecksums, overwrite = TRUE, moduleName, modulePath, ...) {

  if ("shp" %in% fileExt(neededFiles)) { # if user wants .shp file, needs other anciliary files
    shpfileBase <- gsub(".shp$", "", neededFiles[fileExt(neededFiles) %in% "shp"])
    otherShpfiles <- paste0(shpfileBase, ".", c("shx", "dbf", "prj", "sbx", "cpg", "shp.xml", "sbn"))
    neededFiles <- unique(c(neededFiles, otherShpfiles))
  }


  if (!is.null(neededFiles)) {
    result <- checkSums[checkSums$expectedFile %in% neededFiles, ]$result
  } else {
    result <- unique(checkSums$result)
  }
  missingNeededFiles <- (!(all(compareNA(result, "OK")) && all(neededFiles %in% checkSums$expectedFile)) ||
                           is.null(targetFile) || is.null(neededFiles))
  if (missingNeededFiles) {
    if (needChecksums == 0) needChecksums <- 2 # use binary addition -- 1 is new file, 2 is append
  }

  if (missingNeededFiles) {
    fileToDownload <- if (is.null(archive[1])) {
      "All"
    } else {
      result <- checkSums[checkSums$expectedFile == basename(archive), ]$result
      missingArchive <- !isTRUE(result == "OK")
      if (missingArchive) {
        archive[1]
      } else {
        NULL # means nothing to download because the archive is already in hand
      }
    }
    skipDownloadMsg <- "Skipping download of url; local copy already exists and passes checksums"

    # The download step
    if (!is.null(moduleName)) { # means it is inside a SpaDES module
      if (!is.null(fileToDownload)) {
        downloadData(moduleName, modulePath, files = fileToDownload,
                     checked = checkSums, quickCheck = quick, overwrite = overwrite)
      }
    } else {
      # The ad hoc case
      if (!is.null(fileToDownload) ) {#|| is.null(targetFile)) {
        if (!is.null(url)) {
          if (grepl("drive.google.com", url)) {
            googledrive::drive_auth() ## neededFiles for use on e.g., rstudio-server
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
            } else {
              downloadFilename <- archive
            }
            destFile <- file.path(tempdir(), basename(downloadFilename))
            if (!isTRUE(checkSums[ checkSums$expectedFile ==  basename(destFile), ]$result == "OK")) {
              message("Downloading from google drive")
              googledrive::drive_download(googledrive::as_id(url), path = destFile,
                                          overwrite = overwrite, verbose = TRUE)
            } else {
              message(skipDownloadMsg)
              needChecksums <- 0
            }
          } else {
            destFile <- file.path(tempdir(), basename(url))
            download.file(url, destfile = destFile)
          }
          # if destinationPath is tempdir, then don't copy and remove
          if (!(identical(dirname(destFile),
                          normalizePath(destinationPath, winslash = "/", mustWork = FALSE)))) {
            suppressWarnings(file.copy(destFile, destinationPath))
            suppressWarnings(file.remove(destFile))
          }

        }
      } else {
        message(skipDownloadMsg)
        needChecksums <- 0
      }
    }
  } else {
    if (is.null(targetFile)) {
      message("Skipping download because all files listed in CHECKSUMS.txt file are present.",
              " If this is not correct, rerun prepInputs with purge = TRUE")
    } else {
      message("Skipping download because targetFile already present")
    }

  }
  archiveReturn <- if (is.null(archive)) archive else file.path(destinationPath, basename(archive))
  list(needChecksums = needChecksums, archive = archiveReturn, neededFiles = neededFiles)
}
