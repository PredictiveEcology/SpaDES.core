gitSite1 <- c("PredictiveEcology/SpaDES.experiment@development",
             "PredictiveEcology/SpaDES.experiment@master")

deprecatedMsg <- function(fnName = as.character(match.call()[[1]]),
                          newPackage = "SpaDES.experiment",
                          gitSite = gitSite1) {
  paste0(fnName, " has been moved to ", newPackage, ". ",
         "Please install with ",
         paste0("devtools::install_github('",gitSite1, "')", collapse = " or "))
}

#' Deprecated functions
#'
#' These functions have been moved to `SpaDES.experiment` package.
#'
#' @export
#' @param ... Unused.
#' @rdname deprecated
experiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}


#' @export
#' @rdname deprecated
experiment2 <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}

#' @export
#' @rdname deprecated
POM <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}

################################################################################
#' Load packages.
#'
#' Deprecated. Please use [Require::Require()]
#'
#' @param packageList A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#' already installed, should they be installed?
#'
#' @param quiet Logical flag. Should the final "packages loaded"
#' message be suppressed?
#'
#' @return Specified packages are loaded and attached using `require()`,
#'         invisibly returning a logical vector of successes.
#'
#' @seealso [require()].
#'
#' @export
#' @rdname loadPackages
#' @importFrom utils install.packages installed.packages
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   pkgs <- list("raster", "lme4")
#'   loadPackages(pkgs) # loads packages if installed
#'   loadPackages(pkgs, install = TRUE) # loads packages after installation (if needed)
#' }
#'
setGeneric("loadPackages", function(packageList, install = FALSE, quiet = TRUE) {
  standardGeneric("loadPackages")
})

#' @rdname loadPackages
setMethod(
  "loadPackages",
  signature = "character",
  definition = function(packageList, install, quiet) {
    .Deprecated("Require", "Require")
    packageList <- na.omit(packageList) %>% as.character()
    if (length(packageList)) {
      if (install) {
        repos <- getOption("repos")
        if (is.null(repos) | any(repos == "")) {
          repos <- "https://cran.rstudio.com"
        }
        installed <- unname(installed.packages()[, "Package"])
        toInstall <- packageList[packageList %in% installed]
        install.packages(toInstall, repos = repos)
      }

      loaded <- suppressMessages(sapply(packageList, require, character.only = TRUE,
                                        quiet = TRUE, warn.conflicts = FALSE))
      if (any(!loaded)) {
        alreadyLoaded <- unlist(lapply(packageList[!loaded], isNamespaceLoaded))
        if (!all(alreadyLoaded)) {
          stop("Some packages required for the simulation are not installed:\n",
               "    ", paste(names(loaded[-which(loaded)]), collapse = "\n    "))
        } else {
          message("Older version(s) of ",
                  paste(collapse = ", ", packageList[!loaded]), " already loaded")
        }
      }

      if (!quiet) {
        message(paste("Loaded", length(which(loaded == TRUE)), "of",
                      length(packageList), "packages.", sep = " "))
      }
    } else {
      loaded <- character(0)
    }
    return(invisible(loaded))
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "list",
          definition = function(packageList, install, quiet) {
            loadPackages(unlist(packageList), install, quiet)
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "NULL",
          definition = function(packageList, install, quiet) {
            return(invisible(character(0)))
})
