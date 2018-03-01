if (getRversion() >= "3.1.0") {
  utils::globalVariables("expectedFile")
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

  filesInArchive <- Cache(funWArgs$fun, archive[1], list = TRUE)
  if ("Name" %in% names(filesInArchive)) filesInArchive <- filesInArchive$Name # for zips

  if (length(archive) > 1) {
    .unzipOrUnTar(funWArgs$fun, funWArgs$args, files = basename(archive[2]))
    # recursion, removing one archive
    extractFromArchive(archive[-1], destinationPath = destinationPath,
                       needed = needed, extractedArchives = extractedArchives)
  } else if (any(needed %in% basename(filesInArchive))) {
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
#' \code{downloadFromWebDB} or \code{downloadData},
#' \code{extractFromArchive}.  NOTE: This function is still experimental: use
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
#' @param ... Passed to pkg::fun
#'
#' @author Eliot McIntire
#' @author Jean Marchal
#' @export
#' @importFrom data.table data.table
#' @importFrom digest digest
#' @importFrom methods is
#' @importFrom reproducible Cache compareNA asPath
#' @importFrom sf st_buffer st_crs st_intersection st_is st_is_valid st_transform st_write
#' @rdname prepInputs
#'
prepInputs <- function(targetFile, archive = NULL, alsoExtract = NULL,
                       dataset = NULL, destinationPath = NULL, fun = "raster",
                       pkg = "raster", studyArea = NULL, rasterToMatch = NULL,
                       rasterInterpMethod = "bilinear", rasterDatatype = "INT2U",
                       writeCropped = TRUE, addTagsByObject = NULL,
                       quickCheck = FALSE, cacheTags = "", notOlderThan = NULL, ...) {
  message("Preparing: ", targetFile)
  # destinationPath <- file.path(modulePath, moduleName, "data")

  targetFile <- basename(targetFile)
  targetFilePath <- file.path(destinationPath, targetFile)

  if (!is.null(archive)) {
    archive <- file.path(destinationPath, basename(archive))
    filesToCheck <- c(targetFilePath, archive)
  } else {
    filesToCheck <- targetFilePath
  }

  # Here we assume that if destinationPath has not been updated checksums don't need to
  # be rerun. This is useful for WEB apps.
  capturedOutput <- capture.output(
    tmp <- Cache(file.info, asPath(dir(destinationPath, full.names = TRUE)), userTags = cacheTags),
    type = "message"
  )

  notOlderThan <- if (length(capturedOutput) == 0) Sys.time()

  chksumsFilePath <- file.path(destinationPath, "CHECKSUMS.txt")
  moduleName <- basename(dirname(dirname(chksumsFilePath)))
  modulePath <- dirname(dirname(dirname(chksumsFilePath)))
  checkSums <- checksums(files = filesToCheck,
          module = moduleName,
          path = modulePath,
          checksumFile = asPath(chksumsFilePath),
          write = FALSE,
          quickCheck = quickCheck
  )
  result <- checkSums[checkSums$expectedFile == targetFile, ]$result
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
      }
    }

    # The download step
    if (!is.null(fileToDownload))
      downloadData(moduleName, modulePath, files = fileToDownload,
                   checked = checkSums, quickCheck = quickCheck)

    if (!is.null(archive)) {
      extractFromArchive(archive = archive, destinationPath = destinationPath,
                         needed = c(targetFile, alsoExtract))
    }
  }

  f <- getFromNamespace(fun, pkg)

  if (fun == "raster" && pkg == "raster") {
    x <- f(targetFilePath, ...)
  } else {
    x <- Cache(f, targetFilePath, userTags = cacheTags, ...)
  }

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <- if (!is.null(rasterToMatch)) {
        crs(rasterToMatch)
      } else if (!is.null(studyArea)) {
        crs(studyArea)
      } else {
        if (is(x, "sf"))
          CRS(sf::st_crs(x)$proj4string)
        else
          crs(x)
      }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS,
                           quick = quickCheck, userTags = cacheTags)
      }
    }

    x <- Cache(
      cropReprojInputs,
      x = x,
      studyArea = studyArea,
      rasterToMatch = rasterToMatch,
      rasterInterpMethod = rasterInterpMethod,
      quick = quickCheck,
      cacheTags = cacheTags,
      userTags = cacheTags
    )

    if (!is.null(studyArea)) {
      if (is(x, "RasterLayer") ||
          is(x, "RasterStack") ||
          is(x, "RasterBrick")) {
        x <- Cache(
          maskInputs,
          x = x,
          studyArea = studyArea,
          userTags = cacheTags,
          quick = quickCheck
        )
      }
    }

    if (writeCropped) {
      smallFN <- .prefix(targetFilePath, "Small")

      Cache(
        writeInputsOnDisk,
        x = x,
        filename = smallFN,
        rasterDatatype = rasterDatatype,
        quick = quickCheck,
        userTags = cacheTags,
        notOlderThan = if (!file.exists(asPath(smallFN))) Sys.time()
      )
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
#' @param studyArea Template \code{SpatialPolygons*} object used for reprojecting
#' and cropping.
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
#' @importFrom raster buffer crop crs extent projectRaster res
#' @importFrom rgeos gIsValid
#' @importFrom reproducible Cache
#' @importFrom sp SpatialPolygonsDataFrame spTransform CRS
#' @importFrom sf st_as_sf st_crs st_is_valid st_buffer st_transform
#' @rdname cropReprojInputs
#'
cropReprojInputs <- function(x, studyArea = NULL, rasterToMatch = NULL,
                             rasterInterpMethod = "bilinear",
                             addTagsByObject = NULL, cacheTags = NULL) {
  message("  Cropping, reprojecting")

  if (!is.null(studyArea) || !is.null(rasterToMatch)) {
    targetCRS <- if (!is.null(rasterToMatch)) {
      crs(rasterToMatch)
    } else if (!is.null(studyArea)) {
      crs(studyArea)
    } else {
      if (is(x, "sf"))
        CRS(sf::st_crs(x)$proj4string)
      else
        crs(x)
    }

    if (!is.null(studyArea)) {
      if (!identical(targetCRS, crs(studyArea))) {
        studyArea <- Cache(spTransform, x = studyArea, CRSobj = targetCRS, userTags = cacheTags)
      }
    }

    if (is(x, "RasterLayer") ||
        is(x, "RasterStack") ||
        is(x, "RasterBrick")) {

      if (!is.null(studyArea)) {
        x <- Cache(
          crop,
          x = x,
          y = if (identical(raster::crs(studyArea), raster::crs(x))) {
            studyArea
          } else {
            Cache(sp::spTransform, x = studyArea, CRSobj = raster::crs(x),
                  userTags = cacheTags)
          },
          userTags = cacheTags
        )
      }

      if (!is.null(rasterToMatch)) {
        if (!identical(crs(x), targetCRS) |
            !identical(res(x), res(rasterToMatch)) |
            !identical(extent(x), extent(rasterToMatch))) {
          x <- Cache(projectRaster, from = x, to = rasterToMatch,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      } else {
        if (!identical(crs(x), targetCRS)) {
          x <- Cache(projectRaster, from = x, crs = targetCRS,
                     method = rasterInterpMethod, userTags = cacheTags)
        }
      }
    } else if (inherits(x, "SpatialPoints") ||
               inherits(x, "SpatialLines") ||
               inherits(x, "SpatialPolygons")) {
      if (inherits(x, "SpatialPolygons") && !suppressWarnings(gIsValid(x))) {
        xValid <- Cache(buffer, x, dissolve = FALSE, width = 0, userTags = cacheTags)
        x <- if (.hasSlot(x, "data")) xValid
             else Cache(SpatialPolygonsDataFrame, Sr = xValid,
                        data = as.data.frame(x), userTags = cacheTags)
      }

      if (!identical(targetCRS, crs(x)))
        x <- Cache(spTransform, x = x, CRSobj = targetCRS, userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(crop, x, studyArea, userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(crop, x, rasterToMatch, userTags = cacheTags)
      }
    } else if (is(x, "sf")) {
      if (any(st_is(x, c("POLYGON", "MULTIPOLYGON"))) && !any(isValid <- st_is_valid(x))) {
        x[!isValid] <- Cache(st_buffer, x[!isValid], dist = 0, userTags = cacheTags)
      }

      if (!identical(st_crs(targetCRS@projargs), st_crs(x)))
        x <- Cache(st_transform, x = x, crs = st_crs(targetCRS@projargs), userTags = cacheTags)

      if (!is.null(studyArea)) {
        x <- Cache(st_intersection, x, st_as_sf(studyArea), userTags = cacheTags)
      }

      if (!is.null(rasterToMatch)) {
        x <- Cache(st_intersection, x, st_as_sf(as(extent(rasterToMatch), "SpatialPolygons")),
                   userTags = cacheTags)
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
#' @param studyArea  A \cohe{SpatialPolygons*} object
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

    fastMask(x = x, polygon = studyArea)
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
#' @importFrom sf st_write
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
  } else if (is(x, "sf"))
  {
    st_write(obj = x, delete_dsn = TRUE, dsn = filename)
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
