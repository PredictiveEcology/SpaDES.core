### deal with spurious httr warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(#"actualFile",
                           "checksum.x", "checksum.y", #"content",
                           "expectedFile", "filesize.x", "filesize.y", "result"))
}

checksums <- function(module, path, TODO)

#' Determine the size of a remotely hosted file
#'
#' Query a remote web server to determine the size of a remote file.
#'
#' @param url  The url of the remote file.
#'
#' @return A numeric indicating the size of the remote file in bytes.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom RCurl url.exists
#'
#' @examples
#' urls <- c("https://www.alexchubaty.com/uploads/2011/11/open-forest-science-journal.csl",
#'           "https://www.alexchubaty.com/uploads/2011/08/models_GUI_2011-08-07.zip",
#'           "http://example.com/doesntexist.csv")
#' try(remoteFileSize(urls))
#'
remoteFileSize <- function(url) {
  contentLength <- vapply(url, function(u) {
    header <- RCurl::url.exists(u, .header = TRUE)
    status <- tryCatch(as.numeric(header[["status"]]), error = function(e) 0)
    if (status == 200) {
      as.numeric(header[["Content-Length"]])
    } else {
      0
    }
  }, numeric(1))

  return(contentLength)
}

################################################################################
#' Download module data
#'
#' Download external data for a module if not already present in the module
#' directory, or if there is a checksum mismatch indicating that the file is not
#' the correct one.
#'
#' \code{downloadData} requires a checksums file to work, as it will only download
#'  the files specified therein. Hence, module developers should make sure they
#'  have manually downloaded all the necessary data and ran \code{checksums} to
#'  build a checksums file.
#'
#' There is an experimental attempt to use the \pkg{googledrive} package to download
#' data from a shared (publically or with individual users) file.
#' To try this, put the Google Drive URL in \code{sourceURL} argument of
#' \code{expectsInputs} in the module metadata, and put the filename once downloaded
#' in the \code{objectName} argument.
#' If using Rstudio Server, you may need to use "out of band" authentication by
#' setting \code{options(httr_oob_default = TRUE)}.
#' To avoid caching of Oauth credentials, set \code{options(httr_oauth_cache = TRUE)}.
#'
#' There is also an experimental option for the user to make a new \file{CHECKSUMS.txt}
#' file if there is a \code{sourceURL} but no entry for that file.
#' This is experimental and should be used with caution.
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#'
#' @param quiet   Logical. This is passed to \code{download.file}. Default is FALSE.
#'
#' @param quickCheck Logical. If \code{TRUE}, then the check with local data will only
#'                   use \code{file.size} instead of \code{digest::digest}.
#'                   This is faster, but potentially much less robust.
#'
#' @param overwrite Logical. Should local data files be overwritten in case they exist?
#'                  Default is FALSE
#'
#' @param files A character vector of length 1 or more if only a subset of files should be
#'              checked in the CHECKSUMS.txt file
#'
#' @param checked The result of a previous checksums(...) call. This should only be used when
#'         there is no possibility that the file has changed, i.e., if \code{downloadData} is
#'         called from inside another function
#'
#' @param urls Character vector of urls from which to get the data. This is automatically
#'             found from module metadata when this function invoked with
#'            \code{SpaDES.core::downloadModule(..., data = TRUE)}. See also
#'            \code{\link{prepInputs}}
#'
#' @param children The character vector of child modules (without path) to also
#'                 run \code{downloadData} on
#'
#' @return Invisibly, a list of downloaded files.
#'
#' @seealso \code{\link{prepInputs}}, \code{checksums} and \code{downloadModule} in
#'       \code{SpaDES.core} package for downloading modules and building a checksums file.
#'
#' @author Alex Chubaty
#' @author Eliot McIntire
#' @export
#' @importFrom dplyr mutate bind_rows
#' @importFrom googledrive as_id drive_auth drive_download
#' @importFrom reproducible checkPath compareNA
#' @importFrom RCurl url.exists
#' @importFrom utils download.file
#' @rdname downloadData
#' @examples
#' \dontrun{
#' # For a Google Drive example
#' # In metadata:
#' expectsInputs("theFilename.zip", "NA", "NA",
#'   sourceURL = "https://drive.google.com/open?id=1Ngb-jIRCSs1G6zcuaaCEFUwldbkI_K8Ez")
#' # create the checksums file
#' checksums("thisModule", "there", write = TRUE)
#' downloadData("thisModule", "there", files = "theFilename.zip")
#' }
#'
setGeneric("downloadData", function(module, path, quiet, quickCheck = FALSE,
                                    overwrite = FALSE, files = NULL, checked = NULL,
                                    urls = NULL, children = NULL) {
  standardGeneric("downloadData")
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "logical",
                quickCheck = "ANY", overwrite = "ANY", files = "ANY", checked = "ANY",
                urls = "ANY", children = "ANY"),
  definition = function(module, path, quiet, quickCheck, overwrite, files, checked, urls, children) {
    cwd <- getwd()
    path <- checkPath(path, create = FALSE)
    inputs <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                  defineModuleElement = "inputObjects")
    urls <- inputs$sourceURL

    # parsedModule <- parse(file = file.path(modulePath, moduleName, paste0(moduleName, '.R')))
    # urls <- .getSourceURL(pattern = fileToDownload, x = parsedModule)

    if (is.call(urls)) {
      # This is the case where it can't evaluate the .parseModulePartial because of a reference
      #  to the sim object that isn't available. Because sourceURL is unlikely to use
      #  a call to sim object, then try to evaluate again here, just the one column
      urls <- eval(urls)
      #urls <- moduleMetadata(module, path)$inputObjects$sourceURL
    }

    ##

    reproducible::prepInputs(url = urls, path = path)

    ##

    ids <- which(urls == "" | is.na(urls))
    to.dl <- if (length(ids)) urls[-ids] else urls
    if (is.null(checked)) {
      chksums <- checksums(module, path, quickCheck = quickCheck, files = files)
    } else {
      chksums <- checked
    }
    chksums <- chksums %>%
      dplyr::mutate(renamed = NA, module = module)
    dataDir <- file.path(path, module, "data" )

    if (!is.null(files)) {
      chksums <- chksums[chksums$expectedFile %in% basename(files), ]
      fileMatching <- agrepl(basename(files), basename(to.dl))
      if (!any(fileMatching)) {
        fileMatching <- agrepl(basename(files), names(to.dl))
        chksums <- chksums[chksums$expectedFile %in% names(to.dl)[fileMatching], ]
        if (!any(fileMatching)) {
          stop("Could not match the SourceURLs for ", to.dl[fileMatching],
               " to particular files in the CHECKSUMS.txt.",
               "\nPerhaps add an entry in .inputObjects whose downloaded filename is closer",
               " to the online dataset name.")
        }
      }
      to.dl <- to.dl[fileMatching]
    }

    allInChecksums <- TRUE
    doDownload <- TRUE
    if (!(NROW(chksums) > 0 && all(compareNA(chksums$result, "OK")))) {
    #if (!((any(chksums$result == "FAIL") | any(is.na(chksums$result))) )) {
      if (length(to.dl) == 0) {
        message(crayon::magenta("  No data to download for module ", module, ".", sep = ""))
        doDownload <- FALSE
        allInChecksums <- FALSE
      } else {
        message(crayon::magenta("  There is no checksums value for file(s): ",
                                paste(to.dl, collapse = ", "),
                                ". Perhaps you need to run\n",
                                "checksums(\"", module, "\", path = \"", path,
                                "\", write = TRUE). Downloading it anyway.", sep = ""))
        out <- "Yes"
        # if (interactive())  {
        #   out <- readline(prompt = "Would you like to download it now anyway? (Y)es or (N)o: ")
        # } else {
        #   out = "No"
        # }
        if (!isTRUE(any(pmatch("Y", toupper(out) )))) {
          message(crayon::magenta("  No data to download for module ", module, ".", sep = ""))
          doDownload <- FALSE
        }
        allInChecksums <- FALSE
      }
    }

    if (doDownload) {
      setwd(path); on.exit(setwd(cwd), add = TRUE)

      files1 <- sapply(seq(to.dl), function(xInd) {
        googleDrive <- FALSE
        if (grepl("drive.google.com", to.dl[xInd])) {
          message("Using 'googledrive' package, expecting the filename to be provided",
                  " as the objectName in 'expectsInputs'")
          xFile <- names(to.dl[xInd])
          googleDrive <- TRUE
        } else {
          xFile <- gsub("[?!]", "_", basename(to.dl[xInd]))
        }
        destfile <- file.path(dataDir, xFile)

        if (allInChecksums) {
          id <- which(chksums$expectedFile == xFile)
        } else {
          id <- NA_integer_
        }
        if (length(id) == 0) {
          fuzzy <- integer()
          md <- 1
          while (length(fuzzy) == 0) {
            md <- md + 1
            fuzzy <- agrep(xFile, chksums$expectedFile, max.distance = md, ignore.case = TRUE)
            if (md >= nchar(xFile)) fuzzy <- 0
          }
          if (all(fuzzy == 0)) {
            stop("  downloadData() requires that basename(sourceURL) name",
                 " and local filename be at least somewhat similar.")
          } else {
            id <- fuzzy
            message(crayon::magenta("  Used fuzzy matching of filenames. Assuming\n    ",
                                    xFile, " is the source for\n      ",
                                    paste(chksums$expectedFile[id], collapse = ",\n      "), sep = ""))
          }
        }
        # Only do files that were requested, but allow fuzzy matching
        if (!is.null(files)) {
          xFile <- xFile[xFile %in% basename(files)]
        }
        if ((any(chksums$result[id] == "FAIL") | any(is.na(chksums$actualFile[id]))) | overwrite) {
          tmpFile <- file.path(tempdir(), "SpaDES_module_data") %>%
            checkPath(create = TRUE) %>%
            file.path(., xFile)

          if (!RCurl::url.exists(to.dl[xInd])) {
            ## if the URL doesn't work allow the user to retrieve it manually
            message(crayon::magenta("Cannot download ", xFile, " for module ", module, ":\n",
                                    "\u2937 cannot open URL '", to.dl[xInd], "'.", sep = ""))

            if (interactive()) {
              readline(prompt = paste0("Try downloading this file manually and put it in ",
                                       file.path(path, module, "data"),
                                       "/\nPress [enter] to continue"))
            }

            ## re-checksums
            chksums <- checksums(module, path, files = files, quickCheck = quickCheck) %>%
              dplyr::mutate(renamed = NA, module = module)
          } else {
            needNewDownload <- TRUE
            if (googleDrive) {
              message(crayon::magenta("Downloading from Google Drive:"))

              ## allow options `httr_oob_default` and `httr_oauth_cache` to be used:
              googledrive::drive_auth() ## needed for use on e.g., rstudio-server

              googledrive::drive_download(googledrive::as_id(to.dl[xInd]), path = tmpFile,
                                          overwrite = overwrite, verbose = TRUE)
            } else {
              ## check whether file needs to be downloaded
              remoteFileSize <- remoteFileSize(to.dl[xInd])
              if (file.exists(destfile)) {
                if (remoteFileSize > 0)
                  if (round(file.size(destfile)) == remoteFileSize)
                    needNewDownload <- FALSE
              }

              ## download if needed, using Cache in case multiple objects use same url
              ## (e.g., in a .tar file)
              if (needNewDownload) {
                message(crayon::magenta("Downloading ", basename(to.dl[xInd]), " for module ",
                                        module, ":", sep = ""))
                download.file(to.dl[xInd], destfile = tmpFile, mode = "wb", quiet = quiet)
              }
            }
            if (needNewDownload) {
              copied <- file.copy(from = tmpFile, to = destfile, overwrite = overwrite)
            }
            destfile
          }
        } else {
          message(crayon::magenta("  Download data step skipped for ", basename(to.dl[xInd]),
                                  " in module ", module, ". Local copy exists.", sep = ""))
        }
      })

      if (!allInChecksums) {
        # if (interactive()) {
        #   readline("If download was successful, would you like to run a new checksums? (Y)es or (N)o: ")
        # } else {
        #   out = "No"
        # }
        # if (isTRUE(any(pmatch("Y", toupper(out) )))) {
          message("Updating new checksums file. If this is not desired, ",
                  "manually run checksums(..., write = TRUE)")
          checkSumFolder <- file.path(path, module, "data")
          appendChecksumsTable(destinationPath = checkSumFolder, filesToChecksum = basename(to.dl),
                               append = TRUE,
                               checkSumFilePath = file.path(checkSumFolder, "CHECKSUMS.txt"))

          # path = destinationPath, write = TRUE, #checksumFile = checkSumFilePath,
          # files = file.path(destinationPath, filesToChecksum)
          # checksums(module = module, path = path, write = TRUE)
        #}

      }

      chksums <- checksums(module, path, quickCheck = quickCheck, files = files) %>%
        dplyr::mutate(renamed = NA, module = module)
    } else if (NROW(chksums) > 0) {
      message(crayon::magenta("  Download data step skipped for module ", module,
                              ". Local copy exists.", sep = ""))
    }

    # There are at least 2 options if the expected doesn't match actual
    # Next line: left logical: if there is no expectation, then doesn't matter,
    #            right logical: if there is no actualFile, then don't change its name
    wh <- ((match(chksums$actualFile, chksums$expectedFile) %>% is.na()) &
             !(chksums$actualFile %>% is.na() )) %>%
      which()
    if (length(wh)) {
      chksums[wh, "renamed"] <- sapply(wh, function(id) {
        if (!is.na(chksums$expectedFile[id])  ) {
          renamed <- file.rename(
            from = file.path(dataDir, chksums$actualFile[id]),
            to = file.path(dataDir, chksums$expectedFile[id])
          )
        } else {
          message("  downloadData(", module, "): file ",
                  chksums$expectedFile[id], " wasn't downloaded directly during download.")
        }
      })
    }

    # after download, check for childModules that also require downloading
    chksums2 <- chksums[0,]
    #children <- moduleMetadata(module, path)$childModules
    if (!is.null(children)) {
      if (length(children)) {
        if ( all( nzchar(children) & !is.na(children) ) ) {
          chksums2 <- lapply(children, downloadData, path = path, quiet = quiet,
                             quickCheck = quickCheck) %>%
            bind_rows()
        }
      }
    }

    return(bind_rows(chksums, chksums2))
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "missing", quiet = "missing", quickCheck = "ANY",
                overwrite = "ANY", files = "ANY", checked = "ANY", urls = "ANY", children = "ANY"),
  definition = function(module, quickCheck, overwrite, files, checked, urls, children) {
    downloadData(module = module, path = getOption("spades.modulePath"), quiet = FALSE,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked, urls = urls, children = children)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "missing", quiet = "logical", quickCheck = "ANY",
                overwrite = "ANY", files = "ANY", checked = "ANY", urls = "ANY", children = "ANY"),
  definition = function(module, quiet, quickCheck, overwrite, files, checked, urls, children) {
    downloadData(module = module, path = getOption("spades.modulePath"), quiet = quiet,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked, urls = urls, children = children)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "missing", quickCheck = "ANY",
                overwrite = "ANY", files = "ANY", checked = "ANY", urls = "ANY", children = "ANY"),
  definition = function(module, path, quickCheck, overwrite, files, checked, urls, children) {
    downloadData(module = module, path = path, quiet = FALSE,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked, urls = urls, children = children)
})

