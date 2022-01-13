#' Convert standard module code into an R package
#'
#' \emph{EXPERIMENTAL -- USE WITH CAUTION}
#'
#' This will move all functions that are not already in an \code{.R} file
#' in the \code{R} folder into that folder, one function per file, including the
#' \code{doEvent.xxx} function. It will not
#' touch any other functions already in the \code{"R"} folder. It will also create
#' and fill a minimal \code{DESCRIPTION} file. This will leave the `defineModule`
#' function call as the only code in the main module file. This \code{defineModule}
#' and a \code{doEvent.xxx} are the only 2 elements that are required for an R
#' package to be considered a SpaDES module. With these changes, the module should
#' still function normally (still experimental), but will be able to act like an
#' R package, e.g., for writing function documentation, using the \code{testthat}
#' infrastructure, etc.
#'
#' This function is intended to be run once for a module that was created using
#' the "standard" SpaDES module structure (e.g., from a \code{newModule} call). There
#' is currently no way to "revert" the changes from R (though it can be done using
#' version control utilities if all files are under version control, e.g., GitHub).
#' Currently \code{SpaDES.core} identifies a module as being a package if it has
#' a \code{DESCRIPTION} file, or if it has been installed to the \code{.libPaths()}
#' e.g., via \code{devtools::install} or the like. So one can simply remove the
#' package from \code{.libPaths} and delete the \code{DESCRIPTION} file and
#' \code{SpaDES.core} will treat it as a normal module.
#'
#' @return
#' This is run for its side effects. There will be a new or modified
#' \code{DESCRIPTION} file in the root directory of the module. Any functions that
#' were in the main module script (i.e., the .R file whose filename is the name of
#' the module and is in the root directory of the module) will be moved to individual
#' \code{.R} files in the \code{R} folder. Any function with a dot prefix will have the
#' dot removed in its respective filename, but the function name is unaffected.
#'
#' Currently, \code{SpaDES.core} will not install the package, but will load it via
#' \code{pkgdown::load_all}, and build documentation via \code{roxygen2::roxygenise}
#' within the \code{simInit} call. This means that any modifications to source code
#' will be read in at the next \code{simInit} call, as is the practice when a module
#' is not a package.
#'
#' The only function that will be exported by default is the \code{doEvent.xxx},
#' where \code{xxx} is the module name. If any other module is to be exported, it must
#' be explicitly exported with e.g., \code{@export}, and then building the \code{NAMESPACE}
#' file, e.g., via \code{devtools::document(moduleRootPath)}.
#'
#' @section DESCRIPTION:
#'
#' The \code{DESCRIPTION} file that is created with this function will have
#' several elements that a user may wish to change. Notably, all packages that were
#' in \code{reqdPkgs} in the SpaDES module metadata will be in the \code{Depends}
#' section of the \code{DESCRIPTION}. A user should likely follow standard R package
#' best practices and use \code{@importFrom} to identify the specific functions that
#' are required within external packages. These packages can then be moved from
#' \code{Depends} to \code{Imports} in the \code{DESCRIPTION} file.
#'
#' Other elements of a standard \code{DESCRIPTION} file that will be missing or possibly
#' inappropriately short are \code{Title}, \code{Description}, \code{URL},
#' \code{BugReports}.
#'
#' A user may wish to run \code{devtools::document} to build documentation.
#'
#' @export
#' @param module Character string of module name, without path
#' @param path Character string of modulePath. Defaults to
#'   \code{getOption("spades.modulePath")}
convertToPackage <- function(module = NULL, path = getOption("spades.modulePath")) {
  mainModuleFile <- file.path(path, unlist(module), paste0(unlist(module), ".R"))
  aa <- parse(mainModuleFile)
  defModule <- grepl("^defineModule", aa)
  whDefModule <- which(defModule)
  whNotDefModule <- which(!defModule)

  RsubFolder <- file.path(dirname(mainModuleFile), "R")
  checkPath(RsubFolder, create = TRUE)
  fileNames <- lapply(whNotDefModule, function(element) {
    fn <- aa[[element]][[2]]

    filePath <- file.path(dirname(mainModuleFile), "R", paste0(gsub("\\.", "", fn), ".R"))
    if (isTRUE(grepl("^doEvent", fn))) {
      if (!any(grepl("@export", aa[[element]][[3]])))
        cat("#' @export", file = filePath, sep = "\n", append = FALSE)
    }

    cat(format(aa[[element]]), file = filePath, sep = "\n", append = TRUE)
  })

  filePathImportSpadesCore <- file.path(dirname(mainModuleFile), "R", "zzz.R")
  cat(file = filePathImportSpadesCore,
      "#' @import SpaDES.core
      NULL
      ", fill = TRUE)

  cat(format(aa[[whDefModule]]), file = mainModuleFile, sep = "\n")
  md <- aa[[whDefModule]][[3]]
  d <- list()
  d$Package <- .moduleNameNoUnderscore(module)
  d$Type <- "Package"

  d$Title <- md$name
  d$Description <- md$description
  d$Version <- as.character(eval(md$version[[2]]))
  d$Date <- Sys.Date()
  d$Authors <- md$authors
  d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
  deps <- unlist(eval(md$reqdPkgs))
  versionNumb <- Require::extractVersionNumber(deps)
  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  missingSpace <- !grepl("[[:space:]]", inequality)
  if (any(missingSpace))
    inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])
  d$Depends <- Require::extractPkgName(deps)
  d$Depends[hasVersionNumb] <- paste(d$Depends[hasVersionNumb], inequality)

  dFile <- file.path(dirname(mainModuleFile), "DESCRIPTION")

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", d$Description), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Depends))
    cat(c("Depends:", paste("   ", d$Depends, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)
  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
  cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
  cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
  cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
  cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)



  return(invisible())

}
