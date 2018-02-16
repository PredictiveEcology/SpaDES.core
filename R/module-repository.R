### deal with spurious httr warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("actualFile", "checksum.x", "checksum.y", "content",
                           "expectedFile", "filesize.x", "filesize.y", "result"))
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
    status <- tryCatch(as.numeric(header[["status"]]), error = function(x) 0)
    if (status == 200) {
      as.numeric(header[["Content-Length"]])
    } else {
      0
    }
  }, numeric(1))

  return(contentLength)
}

#' Find the latest module version from a SpaDES module repository
#'
#' Modified from \url{http://stackoverflow.com/a/25485782/1380598}.
#'
#' @param name  Character string giving the module name.
#'
#' @param repo  GitHub repository name, specified as \code{"username/repo"}.
#'              Default is \code{"PredictiveEcology/SpaDES-modules"}, which is
#'              specified by the global option \code{spades.moduleRepo}.
#'              Only \code{master} branches can be used at this point.
#'
#' @importFrom httr content GET stop_for_status
#' @export
#' @rdname getModuleVersion
#'
#' @details \code{getModuleVersion} extracts a module's most recent version by
#'          looking at the module \file{.zip} files contained in the module directory.
#'          It takes the most recent version, based on the name of the zip file.
#'
#'          See the modules vignette for details of module directory structure
#'          (\url{http://spades-core.predictiveecology.org/articles/ii-modules.html#module-directory-structure-modulename}),
#'          and see our SpaDES-modules repo for details of module repository structure
#'          (\url{https://github.com/PredictiveEcology/SpaDES-modules}).
#'
#' @author Alex Chubaty
#'
#' @seealso \code{\link{zipModule}} for creating module \file{.zip} folders.
#'
# igraph exports %>% from magrittr
setGeneric("getModuleVersion", function(name, repo) {
  standardGeneric("getModuleVersion")
})

#' @rdname getModuleVersion
setMethod(
  "getModuleVersion",
  signature = c(name = "character", repo = "character"),
  definition = function(name, repo) {
    if (length(name) > 1) {
      warning("name contains more than one module. Only the first will be used.")
      name <- name[1]
    }
    moduleFiles <- checkModule(name, repo)
    zipFiles <- grep(paste0(name, "_+.+.zip"), moduleFiles, value = TRUE) # moduleName_....zip only
    zipFiles <- grep(file.path(name, "data"), zipFiles, invert = TRUE, value = TRUE) # remove any zip in data folder
    # all zip files is not correct behaviour, only
    versions <- strsplit(zipFiles, "_") %>%
      unlist() %>%
      grep("[.]zip$", ., value = TRUE) %>%
      strsplit(., "[.]zip$") %>%
      unlist() %>%
      as.numeric_version()
    currentVersion <- sort(versions, decreasing = TRUE)[1]

    return(currentVersion)
})

#' @rdname getModuleVersion
setMethod("getModuleVersion",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- getModuleVersion(name, getOption("spades.moduleRepo"))
            return(v)
})

################################################################################
#' Check for the existence of a remote module
#'
#' Looks in the remote \code{repo} for a module named \code{name}.
#'
#' @param name  Character string giving the module name.
#'
#' @param repo  GitHub repository name.
#'              Default is \code{"PredictiveEcology/SpaDES-modules"}, which is
#'              specified by the global option \code{spades.moduleRepo}.
#'
#' @importFrom httr config content GET stop_for_status user_agent
#' @export
#' @rdname checkModule
#'
#' @author Eliot McIntire and Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("checkModule", function(name, repo) {
  standardGeneric("checkModule")
})

#' @rdname checkModule
setMethod(
  "checkModule",
  signature = c(name = "character", repo = "character"),
  definition = function(name, repo) {
    if (length(name) > 1) {
      warning("name contains more than one module. Only the first will be used.")
      name <- name[1]
    }
    apiurl <- paste0("https://api.github.com/repos/", repo, "/git/trees/master?recursive=1") # nolint
    ua <- user_agent(getOption("spades.useragent"))
    pat <- Sys.getenv("GITHUB_PAT")
    request <- if (identical(pat, "")) {
      GET(apiurl, ua)
    } else {
      message(crayon::magenta("Using GitHub PAT from envvar GITHUB_PAT", sep = ""))
      GET(apiurl, ua, config = list(config(token = pat)))
    }
    stop_for_status(request)
    allFiles <- unlist(lapply(content(request)$tree, "[", "path"), use.names = FALSE)
    moduleFiles <- grep(paste0("^modules/", name), allFiles, value = TRUE)
    if (length(moduleFiles) == 0) {
      agrep(name, allFiles, max.distance = 0.25, value = TRUE,
            ignore.case = FALSE) %>%
        strsplit(., split = "/") %>%
        lapply(., function(x) x[2]) %>%
        unique() %>%
        unlist() %>%
        paste(., collapse = ", ") %>%
        stop("Module ", name, " does not exist in the repository. ",
             "Did you mean: ", ., "?")
    }
    return(invisible(moduleFiles))
})

#' @rdname checkModule
setMethod("checkModule",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- checkModule(name, getOption("spades.moduleRepo"))
            return(v)
})

################################################################################
#' Check for the existence of a module locally
#'
#' Looks the module path for a module named \code{name}, and checks for existence
#' of all essential module files listed below.
#'
#' \itemize{
#'   \item \file{data/CHECKSUMS.txt}
#'   \item \file{name.R}
#' }
#'
#' @param name  Character string giving the module name.
#'
#' @param path  Local path to modules directory.
#'              Default is specified by the global option \code{spades.modulePath}.
#'
#' @param version Character specifying the desired module version.
#'
#' @return Logical indicating presence of the module (invisibly).
#'
#' @export
#' @rdname checkModuleLocal
#'
#' @author Alex Chubaty
#'
# igraph exports %>% from magrittr
setGeneric("checkModuleLocal", function(name, path, version) {
  standardGeneric("checkModuleLocal")
})

#' @rdname checkModuleLocal
setMethod(
  "checkModuleLocal",
  signature = c(name = "character", path = "character", version = "character"),
  definition = function(name, path, version) {
    if (length(name) > 1) {
      warning("name contains more than one module. Only the first will be used.")
      name <- name[1]
    }

    essentialFiles <- c(
      "data/CHECKSUMS.txt",
      paste0(name, ".R")
    ) %>%
      file.path(path, name, .)

    moduleFiles <- file.path(path, name) %>%
      list.files(full.names = TRUE, recursive = TRUE) %>%
      unlist(use.names = FALSE)

    result <- FALSE
    # check whether any module files exist locally
    if (length(moduleFiles > 0)) {
      # check all essential files exist locally
      if (all(essentialFiles %in% moduleFiles)) {
        # check that local module version matches that desired
        # if desired version is NA then we need to download most recent version
        if (!is.na(version)) {
          v <- .parseModulePartial(filename = file.path(path, name, paste0(name, ".R")),
                                   defineModuleElement = "version")
          result <- ifelse(v == numeric_version(version), TRUE, FALSE)
        }
      }
    }

    return(invisible(result))
})

#' @rdname checkModuleLocal
setMethod(
  "checkModuleLocal",
  signature = c(name = "character", path = "ANY", version = "ANY"),
  definition = function(name, path, version) {
    if (missing(path)) path <- getOption("spades.modulePath")
    if (missing(version)) version <- NA_character_

    result <- checkModuleLocal(name, path, version)
    return(invisible(result))
})

################################################################################
#' Download a module from a SpaDES module GitHub repository
#'
#' Download a .zip file of the module and extract (unzip) it to a user-specified location.
#'
#' Currently only works with a public GitHub repository, where modules are in
#' a \code{modules} directory in the root tree on the \code{master} branch.
#' Module .zip files' names should contain the version number and be inside their
#' respective module folders (see \code{\link{zipModule}} for zip compression of modules).
#'
#' @note \code{downloadModule} uses the \code{GITHUB_PAT} environment variable
#' if a value is set. This alleviates 403 errors caused by too-frequent downloads.
#' Generate a GitHub personal access token at \url{https://github.com/settings/tokens}.
#'
#' @note The default is to overwrite any existing files in the case of a conflict.
#'
#' @seealso \code{\link{zipModule}} for creating module .zip folders.
#'
#' @inheritParams getModuleVersion
#' @inheritParams downloadData
#'
#' @param path    Character string giving the location in which to save the
#'                downloaded module.
#'
#' @param version The module version to download. (If not specified, or \code{NA},
#'                the most recent version will be retrieved.)
#'
#' @param data    Logical. If \code{TRUE}, then the data that is identified in the
#'                module metadata will be downloaded, if possible. Default \code{FALSE}.
#'
#' @param quiet   Logical. This is passed to \code{download.file} (default \code{FALSE}).
#'
#' @param quickCheck Logical. If \code{TRUE}, then the check with local data will only
#'                   use \code{file.size} instead of \code{digest::digest}.
#'                   This is faster, but potentially much less robust.
#'
#' @param overwrite Logical. Should local module files be overwritten in case they exist?
#'                  Default \code{FALSE}.
#'
#' @return A list of length 2. The first element is a character vector containing
#'    a character vector of extracted files for the module. The second element is
#'    a \code{tbl} with details about the data that is relevant for the function,
#'    including whether it was downloaded or not, and whether it was renamed
#'    (because there was a local copy that had the wrong file name).
#'
#' @importFrom httr config GET stop_for_status user_agent write_disk
#' @export
#' @rdname downloadModule
#'
#' @author Alex Chubaty
#'
#'
setGeneric("downloadModule", function(name, path, version, repo, data, quiet,
                                      quickCheck = FALSE, overwrite = FALSE) {
  standardGeneric("downloadModule")
})

#' @rdname downloadModule
#' @importFrom reproducible checkPath
#' @importFrom utils unzip zip
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "character", data = "logical", quiet = "logical",
                quickCheck = "ANY", overwrite = "logical"),
  definition = function(name, path, version, repo, data, quiet, quickCheck,
                        overwrite) {
    path <- checkPath(path, create = TRUE)

    # check locally for module. only download if doesn't exist locally,
    # or if overwrite is wanted
    if (!checkModuleLocal(name, path, version) | overwrite) {
      # check remotely for module
      checkModule(name, repo)
      if (is.na(version)) version <- getModuleVersion(name, repo)

      #versionWarning(name, version)

      zip <- paste0("https://raw.githubusercontent.com/", repo,
                    "/master/modules/", name, "/", name, "_", version, ".zip") # nolint
      localzip <- file.path(path, basename(zip))

      ##download.file(zip, destfile = localzip, mode = "wb", quiet = quiet)
      ua <- user_agent(getOption("spades.useragent"))
      pat <- Sys.getenv("GITHUB_PAT")
      request <- if (identical(pat, "")) {
        GET(zip, ua, write_disk(localzip, overwrite = overwrite))
      } else {
        message(crayon::magenta("Using GitHub PAT from envvar GITHUB_PAT", sep = ""))
        GET(zip, ua, config = list(config(token = pat)), write_disk(localzip, overwrite = overwrite))
      }
      stop_for_status(request)

      files <- unzip(localzip, exdir = file.path(path), overwrite = TRUE)
    } else {
      files <- list.files(file.path(path, name))
    }

    # after download, check for childModules that also require downloading
    files2 <- list()
    children <- .parseModulePartial(filename = file.path(path, name, paste0(name, ".R")),
                                    defineModuleElement = "childModules")
    childVersions <- .parseModulePartial(filename = file.path(path, name, paste0(name, ".R")),
                                         defineModuleElement = "version")

    dataList2 <- data.frame(result = character(0), expectedFile = character(0),
                            actualFile = character(0), checksum.x = character(0),
                            checksum.y = character(0), algorithm.x = character(0),
                            algorithm.y = character(0),
                            stringsAsFactors = FALSE)
    if (!is.null(children)) {
      if (all(nzchar(children) & !is.na(children))) {
        tmp <- lapply(children, function(x) {
          f <- if (is.null(childVersions[[x]])) {
            downloadModule(x, path = path, data = data, version = childVersions[[x]],
                           quickCheck = quickCheck, overwrite = overwrite)
          } else {
            downloadModule(x, path = path, data = data, quickCheck = quickCheck,
                           overwrite = overwrite)
          }
          files2 <<- append(files2, f[[1]])
          dataList2 <<- bind_rows(dataList2, f[[2]])
        })
      }
    }

    if (data) {
      dataList <- downloadData(module = name, path = path, quiet = quiet,
                               quickCheck = quickCheck)
    } else {
      dataList <- checksums(module = name, path = path, quickCheck = quickCheck)
    }
    message(crayon::magenta("Download complete for module ", name, ".", sep = ""))

    return(list(c(files, files2), bind_rows(dataList, dataList2)))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "missing", version = "missing",
                repo = "missing", data = "missing", quiet = "missing",
                quickCheck = "ANY", overwrite = "ANY"),
  definition = function(name, quickCheck, overwrite) {
    files <- downloadModule(name, path = getOption("spades.modulePath"),
                            version = NA_character_,
                            repo = getOption("spades.moduleRepo"),
                            data = FALSE, quiet = FALSE,
                            quickCheck = quickCheck, overwrite = overwrite)
    return(invisible(files))
})

#' @rdname downloadModule
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "ANY", version = "ANY",
                repo = "ANY", data = "ANY", quiet = "ANY", quickCheck = "ANY",
                overwrite = "ANY"),
  definition = function(name, path, version, repo, data, quiet, quickCheck,
                        overwrite) {
    if (missing(path)) path <- getOption("spades.modulePath")
    if (missing(version)) version <- NA_character_
    if (missing(repo)) repo <- getOption("spades.moduleRepo")
    if (missing(data)) data <- FALSE
    if (missing(quiet)) quiet <- FALSE
    if (missing(quickCheck)) quickCheck <- FALSE
    if (missing(overwrite)) overwrite <- FALSE

    files <- downloadModule(name, path, version, repo, data, quiet, quickCheck, overwrite)
    return(invisible(files))
})

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
#' @return Invisibly, a list of downloaded files.
#'
#' @seealso \code{\link{checksums}} for building a checksums file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom dplyr mutate
#' @importFrom RCurl url.exists
#' @importFrom utils download.file
#' @include moduleMetadata.R
#' @rdname downloadData
#'
setGeneric("downloadData", function(module, path, quiet, quickCheck = FALSE,
                                    overwrite = FALSE, files = NULL,
                                    checked = NULL) {
  standardGeneric("downloadData")
})

#' @rdname downloadData
#' @importFrom reproducible checkPath
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "logical",
                quickCheck = "ANY", overwrite = "ANY", files = "ANY", checked = "ANY"),
  definition = function(module, path, quiet, quickCheck, overwrite, files, checked) {
    cwd <- getwd()
    path <- checkPath(path, create = FALSE)
    urls <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                defineModuleElement = "inputObjects")$sourceURL
    if (is.call(urls)) {
      # This is the case where it can't evaluate the .parseModulePartial because of a reference
      #  to the sim object that isn't available. Because sourceURL is unlikely to use
      #  a call to sim object, then try to evaluate again here, just the one column
      urls <- eval(urls)
      #urls <- moduleMetadata(module, path)$inputObjects$sourceURL
    }

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
      chksums <- chksums[chksums$expectedFile %in% basename(files),]
    }

    if ((any(chksums$result == "FAIL") | any(is.na(chksums$result))) | overwrite) {
      setwd(path); on.exit(setwd(cwd), add = TRUE)

      files <- sapply(to.dl, function(x) {
        xFile <- gsub("[?!]", "_", basename(x))
        destfile <- file.path(dataDir, xFile)
        id <- which(chksums$expectedFile == xFile)
        if (length(id) == 0) {
          fuzzy <- integer()
          md <- 1
          while(length(fuzzy) == 0) {
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
        xFile <- xFile[xFile %in% basename(files)]
        if ((any(chksums$result[id] == "FAIL") | any(is.na(chksums$actualFile[id]))) | overwrite) {
          tmpFile <- file.path(tempdir(), "SpaDES_module_data") %>%
            checkPath(create = TRUE) %>%
            file.path(., xFile)

          if (!RCurl::url.exists(x)) {
            ## if the URL doesn't work allow the user to retrieve it manually
            message(crayon::magenta("Cannot download ", xFile, " for module ", module, ":\n",
                    "\u2937 cannot open URL '", x, "'.", sep = ""))

            if (interactive()) {
              readline(prompt = paste0("Try downloading this file manually and put it in ",
                                       module, "/data/\nPress [enter] to continue"))
            }

            ## re-checksums
            chksums <- checksums(module, path, files = files, quickCheck = quickCheck) %>%
              dplyr::mutate(renamed = NA, module = module)
          } else {
            ## check whether file needs to be downloaded
            remoteFileSize <- remoteFileSize(x)
            needNewDownload <- TRUE
            if (file.exists(destfile)) {
              if (remoteFileSize > 0)
                if (round(file.size(destfile)) == remoteFileSize)
                  needNewDownload <- FALSE
            }

            ## download if needed, using Cache in case multiple objects use same url
            ## (e.g., in a .tar file)
            if (needNewDownload) {
              message(crayon::magenta("Downloading ", basename(x), " for module ", module, ":", sep = ""))
              download.file(x, destfile = tmpFile, mode = "wb", quiet = quiet)
              copied <- file.copy(from = tmpFile, to = destfile, overwrite = TRUE)
            }
            destfile
          }
        } else {
          message(crayon::magenta("  Download data step skipped for ", basename(x),
                  " in module ", module, ". Local copy exists.", sep = ""))
        }
      })

      chksums <- checksums(module, path, quickCheck = quickCheck, files = files) %>%
        dplyr::mutate(renamed = NA, module = module)
    } else if (NROW(chksums) > 0) {
      message(crayon::magenta("  Download data step skipped for module ", module,
                              ". Local copy exists.", sep = ""))
    } else {
      message(crayon::magenta("  No data to download for module ", module, sep = ""))
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
    children <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                    defineModuleElement = "childModules")
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
                overwrite = "ANY", files = "ANY", checked = "ANY"),
  definition = function(module, quickCheck, overwrite, files, checked) {
    downloadData(module = module, path = getOption("spades.modulePath"), quiet = FALSE,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "missing", quiet = "logical", quickCheck = "ANY",
                overwrite = "ANY", files = "ANY", checked = "ANY"),
  definition = function(module, quiet, quickCheck, overwrite, files, checked) {
    downloadData(module = module, path = getOption("spades.modulePath"), quiet = quiet,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked)
})

#' @rdname downloadData
setMethod(
  "downloadData",
  signature = c(module = "character", path = "character", quiet = "missing", quickCheck = "ANY",
                overwrite = "ANY", files = "ANY", checked = "ANY"),
  definition = function(module, path, quickCheck, overwrite, files, checked) {
    downloadData(module = module, path = path, quiet = FALSE,
                 quickCheck = quickCheck, overwrite = overwrite, files = files,
                 checked = checked)
})

################################################################################
#' Calculate the hashes of multiple files
#'
#' Internal function. Wrapper for \code{\link[digest]{digest}} using md5sum.
#'
#' @param file  Character vector of file paths.
#' @inheritParams downloadData
#' @param ...   Additional arguments to \code{digest::digest}.
#'
#' @return A character vector of hashes.
#'
#' @importFrom digest digest
#' @keywords internal
#' @rdname digest
#'
#' @author Alex Chubaty
#'
setGeneric(".digest", function(file, quickCheck, ...) {
  standardGeneric(".digest")
})

#' @rdname digest
setMethod(
  ".digest",
  signature = c(file = "character"),
  definition = function(file, quickCheck, algo = "xxhash64", ...) {
    if (quickCheck) {
      file.size(file) %>%
        as.character() # need as.character for empty case
    } else {
      lapply(file, function(f) {
        digest::digest(object = f, file = TRUE, algo = algo, ...)
      }) %>% unlist() %>% unname() %>% as.character() # need as.character for empty case
    }
})

################################################################################
#' Calculate checksums for a module's data files
#'
#' Verify (and optionally write) checksums for data files in a module's
#' \code{data/} subdirectory. The file \code{data/CHECKSUMS.txt} contains the
#' expected checksums for each data file.
#' Checksums are computed using \code{SpaDES.core:::.digest}, which is simply a
#' wrapper around \code{digest::digest}.
#'
#' Modules may require data that for various reasons cannot be distributed with
#' the module source code. In these cases, the module developer should ensure
#' that the module downloads and extracts the data required. It is useful to not
#' only check that the data files exist locally but that their checksums match
#' those expected. See also \code{\link{downloadData}}.
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
#'
#' @param write   Logical indicating whether to overwrite \code{CHECKSUMS.txt}.
#'                Default is \code{FALSE}, as users should not change this file.
#'                Module developers should write this file prior to distributing
#'                their module code, and update accordingly when the data change.
#'
#' @param checksumFile The filename of the checksums file to read or write to. The default
#'                     is CHECKSUMS.txt located at
#'                     \code{file.path(path, module, "data", checksumFile)}. It is likely
#'                     not a good idea to change this, and should only be used in
#'                     cases such as
#'                     \code{Cache}, which can evaluate if the checksumFile has changed.
#'
#' @inheritParams downloadData
#'
#' @param ...     Passed to \code{\link[digest]{digest}}, notably \code{algo}, so
#'                the digest algorithm can be specified.
#'
#' @return A data.frame with columns: result, expectedFile, actualFile, and checksum.
#'
#' @include moduleMetadata.R
#' @importFrom dplyr arrange desc filter group_by left_join mutate rename row_number select
#' @export
#' @rdname checksums
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#' moduleName <- "my_module"
#' modulePath <- file.path("path", "to", "modules")
#'
#' ## verify checksums of all data files
#' checksums(moduleName, modulePath)
#'
#' ## write new CHECKSUMS.txt file
#'
#' # 1. verify that all data files are present (and no extra files are present)
#' list.files(file.path(modulePath, moduleName, "data"))
#'
#' # 2. calculate file checksums and write to file (this will overwrite CHECKSUMS.txt)
#' checksums(moduleName, modulePath, write = TRUE)
#' }
#'
setGeneric("checksums", function(module, path, write, quickCheck = FALSE,
                                 checksumFile = file.path(path, "CHECKSUMS.txt"),
                                 files = NULL, ...) {
  standardGeneric("checksums")
})

#' @rdname checksums
#' @importFrom reproducible checkPath
#' @importFrom utils read.table write.table
setMethod(
  "checksums",
  signature = c(module = "character", path = "character", quickCheck = "ANY",
                write = "logical", files = "ANY"),
  definition = function(module, path, write, quickCheck, checksumFile, files, ...) {
    defaultHashAlgo <- "xxhash64"
    defaultWriteHashAlgo <- "xxhash64"
    dots <- list(...)
    path <- checkPath(path, create = FALSE) %>% file.path(., module, "data")
    #checksumFile <- file.path(path, "CHECKSUMS.txt")

    if (!write) {
      stopifnot(file.exists(checksumFile))
    } else if (!file.exists(checksumFile)) {
      file.create(checksumFile)
    }

    if (is.null(files)) {
      files <- list.files(path, full.names = TRUE) %>%
        grep(basename(checksumFile), ., value = TRUE, invert = TRUE)
    }

    txt <- if (!write && file.info(checksumFile)$size > 0) {
      read.table(checksumFile, header = TRUE, stringsAsFactors = FALSE)
    } else {
      data.frame(file = character(0), checksum = character(0), filesize = character(0),
                 stringsAsFactors = FALSE)
    }

    if (is.null(dots$algo)) {
      if (NROW(files)) {
        if (write) {
          dots$algo <- defaultWriteHashAlgo
        } else {
          dots$algo <- defaultHashAlgo
        }
      } else {
        dots$algo <- character()
      }
    }

    if (!is.null(txt$algorithm)) {
      if (!write) dots$algo <- unique(txt$algorithm)[1]
    } else {
      if (NROW(txt)) {
        txt$algorithm <- defaultWriteHashAlgo
      } else {
        txt$algorithm <- character()
      }
    }

    message(crayon::magenta("Checking local files...", sep = ""))
    filesToCheck <-  if (length(txt$file) & length(files)) {
      files[basename(files) %in% txt$file]
    } else {
      files
    }
    filesToCheck <- filesToCheck[file.exists(filesToCheck)]

    if (is.null(txt$filesize)) {
      quickCheck <- FALSE
      message(crayon::magenta("  Not possible to use quickCheck in downloadData;\n ",
              "    checksums.txt file does not have filesizes", sep = ""))
    }
    checksums <- rep(list(rep("", length(filesToCheck))), 2)
    if (quickCheck | write) {
      checksums[[2]] <- do.call(.digest,
                                args = append(list(file = filesToCheck, quickCheck = TRUE),
                                              dots))
    }

    if (!quickCheck | write) {
      checksums[[1]] <- do.call(.digest,
                                args = append(list(file = filesToCheck, quickCheck = FALSE),
                                              dots))
    }
    message(crayon::magenta("Finished checking local files.", sep = ""))

    out <- if (length(filesToCheck)) {
      data.frame(file = basename(filesToCheck), checksum = checksums[[1]],
                 filesize = checksums[[2]], algorithm = dots$algo, stringsAsFactors = FALSE)
    } else {
      data.frame(file = character(0), checksum = character(0), filesize = character(0),
                 algorithm = character(0), stringsAsFactors = FALSE)
    }

    if (write) {
      write.table(out, checksumFile, eol = "\n", col.names = TRUE, row.names = FALSE)
      return(out)
    } else {
      results.df <- out %>%
        dplyr::mutate(actualFile = file) %>%
        dplyr::left_join(txt, ., by = "file") %>%
        dplyr::rename(expectedFile = file) %>%
        dplyr::group_by(expectedFile) %>%
        {
          if (quickCheck) {
            mutate(., result = ifelse(filesize.x != filesize.y, "FAIL", "OK"))
          } else {
            mutate(., result = ifelse(checksum.x != checksum.y, "FAIL", "OK"))
          }
        } %>%
        dplyr::arrange(desc(result)) %>%
        {
          if (quickCheck) {
            select(., "result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
                   "algorithm.x", "algorithm.y", "filesize.x", "filesize.y")
          } else {
            select(., "result", "expectedFile", "actualFile", "checksum.x", "checksum.y",
                   "algorithm.x", "algorithm.y")
          }
        } %>%
        dplyr::filter(row_number() == 1L)

      return(results.df)
    }
})

#' @rdname checksums
setMethod(
  "checksums",
  signature = c(module = "character", path = "character", quickCheck = "ANY",
                write = "missing", files = "ANY"),
  definition = function(module, path, quickCheck, files, ...) {
    checksums(module, path, write = FALSE, quickCheck = quickCheck, files = files, ...)
})
