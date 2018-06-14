if (getRversion() >= "3.1.0") {
  utils::globalVariables(c(
    "checksum.x", "checksum.y", "expectedFile", "filesize.x", "filesize.y", "result"
  ))
}

if (!isGeneric("extractURL")) {
  setGeneric(
    "extractURL",
    function(x) {
      standardGeneric("extractURL")
    })
}

#' Extract a URL
#'
#' @inheritParams reproducible::extractURL
#'
#' @author Eliot McIntire
#' @export
#' @exportMethod extractURL
#' @importFrom fastdigest fastdigest
#' @importFrom reproducible extractURL
#' @importMethodsFrom reproducible extractURL
#' @rdname extractURL
setMethod(
  "extractURL",
  signature = "missing",
  definition = function(x) {
  browser()
})

#' @export
#' @rdname extractURL
setMethod(
  "extractURL",
  signature = "simList",
  definition = function(x) {
    browser()
})

#' @export
#' @rdname extractURL
setMethod(
  "extractURL",
  signature = "NULL",
  definition = function(x) {
    browser()
})

#' Calculate checksum for a module's data files
#'
#' Verify (and optionally write) checksums for data files in a module's
#' \file{data/} subdirectory. The file \file{data/CHECKSUMS.txt} contains the
#' expected checksums for each data file.
#' Checksums are computed using \code{SpaDES.tools:::.digest}, which is simply a
#' wrapper around \code{digest::digest}.
#'
#' Modules may require data that for various reasons cannot be distributed with
#' the module source code. In these cases, the module developer should ensure
#' that the module downloads and extracts the data required. It is useful to not
#' only check that the data files exist locally but that their checksums match
#' those expected.
#'
#' @note In version 1.2.0 and earlier, two checksums per file were required
#' because of differences in the checksum hash values on Windows and Unix-like
#' platforms. Recent versions use a different (faster) algorithm and only require
#' one checksum value per file.
#' To update your \file{CHECKSUMS.txt} files using the new algorithm, see
#' \url{https://github.com/PredictiveEcology/SpaDES/issues/295#issuecomment-246513405}.
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#' @param ... Passed to \code{\link[reproducible]{Checksums}}, notably, \code{write},
#'            \code{quickCheck},  \code{checksumFile} and \code{files}.
#' @importFrom reproducible Checksums
checksums <- function(module, path, ...) {
  path <- if (length(module)) {
    file.path(path, module, "data")
  } else {
    file.path(path)
  }
  result <- Checksums(path, ...)
}

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
#' @author Alex Chubaty & Eliot McIntire
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

    if (is.null(urls)) {
      inputs <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                    defineModuleElement = "inputObjects")
      urls <- inputs$sourceURL
    }

    # parsedModule <- parse(file = file.path(path, module, paste0(module, '.R')))
    # urls <- .getSourceURL(pattern = fileToDownload, x = parsedModule)

    if (is.call(urls)) {
      # This is the case where it can't evaluate the .parseModulePartial because of a reference
      #  to the sim object that isn't available. Because sourceURL is unlikely to use
      #  a call to sim object, then try to evaluate again here, just the one column
      urls <- eval(urls)
      #urls <- moduleMetadata(module, path)$inputObjects$sourceURL
    }

    res <- Map(url = urls, reproducible::preProcess, MoreArgs = list(quick = quickCheck, overwrite = overwrite,
                             destinationPath = file.path(path, module, "data")))

    chksums <- rbindlist(lapply(res, function(x) x$checkSums))
    chksums <- chksums[order(-result)]
    chksums <- unique(chksums, by = "expectedFile")

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

