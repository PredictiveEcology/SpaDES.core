utils::globalVariables(c(
  "actualFile", "checksum.x", "checksum.y", "expectedFile", "filesize.x", "filesize.y", "result"
))

defaultGitRepoToSpaDESModules <- "PredictiveEcology/SpaDES-modules"

#' Find the latest module version from a SpaDES module repository
#'
#' Modified from \url{https://stackoverflow.com/a/25485782/1380598}.
#'
#' @param name  Character string giving the module name.
#'
#' @param repo  GitHub repository name, specified as \code{"username/repo"}.
#'              Default is \code{"PredictiveEcology/SpaDES-modules"}, which is
#'              specified by the global option \code{spades.moduleRepo}.
#'              Only \code{master} branches can be used at this point.
#'
#' @export
#' @rdname getModuleVersion
#'
#' @details
#' \code{getModuleVersion} extracts a module's most recent version by
#' looking at the module \file{.zip} files contained in the module directory.
#' It takes the most recent version, based on the name of the zip file.
#'
#' See the modules vignette for details of module directory structure
#' (\url{https://spades-core.predictiveecology.org/articles/ii-modules.html#module-directory-structure-modulename}),
#' and see our SpaDES-modules repo for details of module repository structure
#' (\url{https://github.com/PredictiveEcology/SpaDES-modules}).
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
            v <- getModuleVersion(name, getOption("spades.moduleRepo",
                                                  defaultGitRepoToSpaDESModules))
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
#' @export
#' @rdname checkModule
#' @importFrom utils packageVersion
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
    goAhead <- FALSE
    if (requireNamespace("httr", quietly = TRUE)) {
      if (packageVersion("httr") >= "1.2.1") {
        goAhead <- TRUE
      }}
    if (goAhead) {

      if (length(name) > 1) {
        warning("name contains more than one module. Only the first will be used.")
        name <- name[1]
      }
      apiurl <- paste0("https://api.github.com/repos/", repo, "/git/trees/master?recursive=1") # nolint
      ua <- httr::user_agent(getOption("spades.useragent"))
      pat <- Sys.getenv("GITHUB_PAT")
      request <- if (identical(pat, "")) {
        httr::GET(apiurl, ua)
      } else {
        message(crayon::magenta("Using GitHub PAT from envvar GITHUB_PAT", sep = ""))
        httr::GET(apiurl, ua, config = list(httr::config(token = pat)))
      }
      httr::stop_for_status(request)
      allFiles <- unlist(lapply(httr::content(request)$tree, "[", "path"), use.names = FALSE)
      moduleFiles <- grep(paste0("^modules/", name), allFiles, value = TRUE)
      if (length(moduleFiles) == 0) {
        moduleFiles <- grep(paste0("^", name), allFiles, value = TRUE)
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
      }
    } else {
      stop("checkModule does not work without httr package: ",
              "install.packages('httr')")
    }
    return(invisible(moduleFiles))
})

#' @rdname checkModule
setMethod("checkModule",
          signature = c(name = "character", repo = "missing"),
          definition = function(name) {
            v <- checkModule(name, getOption("spades.moduleRepo",
                                             defaultGitRepoToSpaDESModules))
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
          if (isTRUE(length(v) > length(name))) {
            v <- v[names(v) %in% name]
          }
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
#' Currently only works with GitHub repositories where modules are located in
#' a \code{modules} directory in the root tree on the \code{master} branch.
#' Module .zip files' names should contain the version number and be inside their
#' respective module folders (see \code{\link{zipModule}} for zip compression of modules).
#'
#' @note \code{downloadModule} uses the \code{GITHUB_PAT} environment variable
#' if a value is set. This alleviates 403 errors caused by too-frequent downloads.
#' Generate a GitHub personal access token with no additional permissions at
#' \url{https://github.com/settings/tokens}, and add this key to \file{.Renviron}
#' as \code{GITHUB_PAT=<your-github-pat-here>}.
#'
#' @note The default is to overwrite any existing files in the case of a conflict.
#'
#' @seealso \code{\link{zipModule}} for creating module .zip folders.
#'
#' @inheritParams getModuleVersion
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
#' @importFrom Require checkPath
#' @importFrom utils unzip zip
#' @importFrom data.table setDF rbindlist
setMethod(
  "downloadModule",
  signature = c(name = "character", path = "character", version = "character",
                repo = "character", data = "logical", quiet = "logical",
                quickCheck = "ANY", overwrite = "logical"),
  definition = function(name, path, version, repo, data, quiet, quickCheck,
                        overwrite) {
    if (requireNamespace("httr", quietly = TRUE)) {
    path <- checkPath(path, create = TRUE)
    checkPath(file.path(path, name), create = TRUE)

    # check locally for module. only download if doesn't exist locally,
    # or if overwrite is wanted
    if (!checkModuleLocal(name, path, version) | overwrite) {
      # check remotely for module
      checkModule(name, repo)
      if (is.na(version)) version <- getModuleVersion(name, repo)

      innerPaths <- c(paste0("/master/modules/", name, "/"), "/master/")
      for (tries in 1:2) {
        innerPath <- innerPaths[tries]

        zip <- paste0("https://raw.githubusercontent.com/", repo,
                      innerPath, name, "_", version, ".zip") # nolint
        localzip <- file.path(path, basename(zip))

        ua <- httr::user_agent(getOption("spades.useragent"))
        pat <- Sys.getenv("GITHUB_PAT")
        request <- if (identical(pat, "")) {
          httr::GET(zip, ua, httr::write_disk(localzip, overwrite = overwrite))
        } else {
          message(crayon::magenta("Using GitHub PAT from envvar GITHUB_PAT", sep = ""))
          httr::GET(zip, ua, config = list(httr::config(token = pat)),
                    httr::write_disk(localzip, overwrite = overwrite))
        }
        status1 <- try(httr::stop_for_status(request), silent = TRUE)
        if (!is(status1, "try-error")) break
        if (is(status1, "try-error") && tries == 2) stop(status1)
      }

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
    dataList3 <- dataList2
    if (!is.null(children)) {
      if (all(nzchar(children) & !is.na(children)) && length(children)) {
        tmp <- lapply(children, function(x) {
          f <- if (!is.null(childVersions[[x]])) {
            downloadModule(x, path = path, repo = repo, data = data, version = childVersions[[x]],
                           quickCheck = quickCheck, overwrite = overwrite)
          } else {
            downloadModule(x, path = path,  repo = repo, data = data, quickCheck = quickCheck,
                           overwrite = overwrite)
          }
          files2 <<- append(files2, f[[1]])
          dataList2 <<- setDF(rbindlist(list(dataList2, f[[2]]), use.names = TRUE, fill = TRUE))
        })
      }
    }

    if (data) {
      moduleFilename <- file.path(path, name, paste0(name, ".R"))
      inputs <- .parseModulePartial(filename = moduleFilename,
                                    defineModuleElement = "inputObjects")
      urls <- inputs$sourceURL
      objNames <- if (is.call(inputs$objectName)) {
        unlist(lapply(tail(parse(text = inputs$objectName), length(urls)), function(x) deparse(x)))
      } else {
        inputs$objectName
      }
      names(urls) <- objNames

      children <- .parseModulePartial(filename = moduleFilename,
                                      defineModuleElement = "childModules")

      dataList <- downloadData(module = name, path = path, quiet = quiet,
                               quickCheck = quickCheck, urls = urls, children = children)
    } else {
      dataList <- checksums(module = name, path = path, quickCheck = quickCheck)
    }
    message(crayon::magenta("Download complete for module ", name,
                            " (v", version, " at '", path,"').", sep = ""))
    } else{
      stop("downloadModule does not work without httr package: ",
           "install.package('httr')")
    }

    return(list(c(files, files2),
                setDF(rbindlist(list(dataList, dataList2), use.names = TRUE, fill = TRUE))))
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
                            repo = getOption("spades.moduleRepo",
                                             defaultGitRepoToSpaDESModules),
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
    if (missing(repo)) repo <- getOption("spades.moduleRepo",
                                         defaultGitRepoToSpaDESModules)
    if (missing(data)) data <- FALSE
    if (missing(quiet)) quiet <- FALSE
    if (missing(quickCheck)) quickCheck <- FALSE
    if (missing(overwrite)) overwrite <- FALSE

    files <- downloadModule(name, path, version, repo, data, quiet, quickCheck, overwrite)
    return(invisible(files))
})
