utils::globalVariables(c("expectedFile", "result"))

#' Extract a url from module metadata
#'
#' This will get the `sourceURL` for the object named.
#'
#' @param objectName A character string of the object name in the metadata.
#' @param sim A `simList` object from which to extract the `sourceURL`
#' @param module An optional character string of the module name whose metadata is
#'               to be used. If omitted, the function will use the `currentModule(sim)`,
#'               if defined.
#'
#' @return The url.
#'
#' @author Eliot McIntire
#' @export
#' @exportMethod extractURL
#' @rdname extractURL
setGeneric(
  "extractURL",
  function(objectName, sim, module) {
    standardGeneric("extractURL")
})

#' @export
#' @exportMethod extractURL
#' @importFrom quickPlot whereInStack
#' @rdname extractURL
setMethod(
  "extractURL",
  signature = c(objectName = "character", sim = "missing"),
  definition = function(objectName, sim, module) {
    i <- 0
    lenSC <- length(sys.calls())
    # This will get the simList that is closest in the call stack, noting that
    #  in this first one (i.e., this function), sim will be missing
    while (missing(sim) && i < lenSC) {
      i <- i + 1
      simEnv <- whereInStack("sim", -i)
      sim <- simEnv$sim
    }
    extractURL(objectName = objectName, sim = sim, module = module)
})

#' @export
#' @rdname extractURL
setMethod(
  "extractURL",
  signature = c(objectName = "character", sim = "simList"),
  definition = function(objectName, sim, module) {
    if (missing(module)) {
      module <- currentModule(sim)
    }

    io <- .parseModulePartial(sim, modules = list(module), defineModuleElement = "inputObjects" )
    wh <- io[[module]][["objectName"]] == objectName
    io[[module]][wh, ]$sourceURL
})

#' Calculate checksum for a module's data files
#'
#' Verify (and optionally write) checksums for data files in a module's
#' \file{data/} subdirectory. The file \file{data/CHECKSUMS.txt} contains the
#' expected checksums for each data file.
#' Checksums are computed using `reproducible:::.digest`, which is simply a
#' wrapper around `digest::digest`.
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
#' To update your \file{CHECKSUMS.txt} files using the new algorithm:
#' \enumerate{
#'   \item specify your module (`moduleName <- "my_module"`);
#'   \item use a temporary location to ensure all modules get fresh copies of the data
#'         (`tmpdir <- file.path(tempdir(), "SpaDES_modules")`);
#'   \item download your module's data to the temp dir (`downloadData(moduleName, tmpdir)`);
#'   \item initialize a dummy simulation to ensure any 'data prep' steps in the
#'         `.inputObjects` section are run (`simInit(modules = moduleName)`);
#'   \item recalculate your checksums and overwrite the file
#'         (`checksums(moduleName, tmpdir, write = TRUE)`);
#'   \item copy the new checksums file to your working module directory
#'         (the one not in the temp dir)
#'         (`file.copy(from = file.path(tmpdir, moduleName, 'data', 'CHECKSUMS.txt'),
#'                to = file.path('path/to/my/moduleDir', moduleName, 'data', 'CHECKSUMS.txt'),
#'                overwrite = TRUE)`).
#' }
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#' @param ... Passed to [reproducible::Checksums()], notably, `write`,
#'            `quickCheck`,  `checksumFile` and `files`.
#' @importFrom reproducible Checksums
checksums <- function(module, path, ...) {
  path <- if (length(module)) {
    fp <- file.path(path, module, "data")
    checkPath(fp, create = TRUE)
    fp
  } else {
    file.path(path)
  }
  result <- Checksums(path, ...)
}

#' Determine the size of a remotely hosted file
#'
#' Defunct. Will be removed by mid-2023.
#'
#' @param url  The url of the remote file.
#'
#' @return A numeric indicating the size of the remote file in bytes.
#'
#' @author Eliot McIntire and Alex Chubaty
#'
#' @export
remoteFileSize <- function(url) {
  .Defunct()
  # contentLength <- vapply(url, function(u) {
  #   header <- RCurl::url.exists(u, .header = TRUE)
  #   status <- tryCatch(as.numeric(header[["status"]]), error = function(e) 0)
  #   if (status == 200) {
  #     as.numeric(header[["Content-Length"]])
  #   } else {
  #     0
  #   }
  # }, numeric(1))
  #
  # return(contentLength)
}

################################################################################
#' Download module data
#'
#' Download external data for a module if not already present in the module
#' directory, or if there is a checksum mismatch indicating that the file is not
#' the correct one.
#'
#' `downloadData` requires a checksums file to work, as it will only download
#'  the files specified therein. Hence, module developers should make sure they
#'  have manually downloaded all the necessary data and ran `checksums` to
#'  build a checksums file.
#'
#' There is an experimental attempt to use the \pkg{googledrive} package to download
#' data from a shared (publicly or with individual users) file.
#' To try this, put the Google Drive URL in `sourceURL` argument of
#' `expectsInputs` in the module metadata, and put the filename once downloaded
#' in the `objectName` argument.
#' If using RStudio Server, you may need to use "out of band" authentication by
#' setting `options(httr_oob_default = TRUE)`.
#' To avoid caching of Oauth credentials, set `options(httr_oauth_cache = TRUE)`.
#'
#' There is also an experimental option for the user to make a new \file{CHECKSUMS.txt}
#' file if there is a `sourceURL` but no entry for that file.
#' This is experimental and should be used with caution.
#'
#' @param module  Character string giving the name of the module.
#'
#' @param path    Character string giving the path to the module directory.
#'
#' @param quiet   Logical. This is passed to `download.file`. Default is FALSE.
#'
#' @param quickCheck Logical. If `TRUE`, then the check with local data will only
#'                   use `file.size` instead of `digest::digest`.
#'                   This is faster, but potentially much less robust.
#'
#' @param overwrite Logical. Should local data files be overwritten in case they exist?
#'                  Default is `FALSE`.
#'
#' @param files A character vector of length 1 or more if only a subset of files should be
#'              checked in the \file{CHECKSUMS.txt} file.
#'
#' @param checked The result of a previous `checksums` call. This should only be used when
#'         there is no possibility that the file has changed, i.e., if `downloadData` is
#'         called from inside another function.
#'
#' @param urls Character vector of urls from which to get the data. This is automatically
#'             found from module metadata when this function invoked with
#'            `SpaDES.core::downloadModule(..., data = TRUE)`. See also
#'            [prepInputs()].
#'
#' @param children The character vector of child modules (without path) to also
#'                 run `downloadData` on
#'
#' @param ... Passed to [reproducible::preProcess()], e.g., `purge`
#'
#' @return Invisibly, a list of downloaded files.
#'
#' @seealso [prepInputs()], [checksums()], and [downloadModule()]
#' for downloading modules and building a checksums file.
#'
#' @author Alex Chubaty & Eliot McIntire
#' @export
#' @importFrom reproducible compareNA
#' @importFrom reproducible checkPath
#' @importFrom utils download.file
#' @rdname downloadData
#' @examples
#' \donttest{
#' # In metadata, each expectsInput has a sourceURL; downloadData will look for
#' # that and download if it defined; however this sample module has all
#' # NAs for sourceURL, so nothing to download
#' modulePath <- system.file("sampleModules", package = "SpaDES.core")
#' downloadData("caribouMovement", path = modulePath)
#' }
#'
setGeneric("downloadData", function(module, path, quiet, quickCheck = FALSE,
                                    overwrite = FALSE, files = NULL, checked = NULL,
                                    urls = NULL, children = NULL, ...) {
  standardGeneric("downloadData")
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "logical",
                quickCheck = "ANY", overwrite = "ANY", files = "ANY", checked = "ANY",
                urls = "ANY", children = "ANY"),
  definition = function(module, path, quiet, quickCheck, overwrite, files, checked,
                        urls, children, ...) {
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

    targetFiles <- if (is.null(files)) {
      lapply(urls, function(x) NULL)
    } else {
      files
    }
    notNAs <- !unlist(lapply(urls, is.na))
    dPath <- file.path(path, module, "data")
    if (any(notNAs)) {
      # This requires googledrive in reproducible 1.2.16; even if not a googledrive url
      res <- Map(reproducible::preProcess,
                 targetFile = targetFiles[notNAs],
                 url = urls[notNAs],
                 MoreArgs = append(
                   list(
                     quick = quickCheck,
                     overwrite = overwrite,
                     destinationPath = dPath
                   ),
                   list(...)
                 )
      )
      chksums <- rbindlist(lapply(res, function(x) x$checkSums))
      chksums <- chksums[order(-result)]
      chksums <- unique(chksums, by = "expectedFile")
    } else {
      unlinkAfter <- !dir.exists(dPath) # next line will make the folder and put nothing in it
      chksums <- Checksums(dPath, write = TRUE)
      if (isTRUE(unlinkAfter))
        unlink(dPath, recursive = TRUE)
    }

    # after download, check for childModules that also require downloading
    #children <- moduleMetadata(module, path)$childModules
    if (!is.null(children)) {
      if (length(children)) {
        if (all(nzchar(children) & !is.na(children))) {
          chksums2 <- bindrows(lapply(children, downloadData, path = path, quiet = quiet,
                             quickCheck = quickCheck))
          chksums <- bindrows(chksums, chksums2)
        }
      }
    }

    return(chksums)
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
