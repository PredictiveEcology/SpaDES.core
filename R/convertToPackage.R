#' Convert standard module code into an R package
#'
#' \emph{EXPERIMENTAL -- USE WITH CAUTION}. This function will only create the
#' necessary source files so that all the code can be used (and installed) like an R package.
#' This function does not install anything (e.g., \code{devtools::install}). After
#' running this function, \code{simInit} will automatically detect that this is now
#' a package and will load the functions (via \code{pkgload::load_all}) from the source files.
#' This will have the effect that it emulates the "non-package" behaviour of a
#' SpaDES module exactly. After running this function, current tests show no
#' impact on module behaviour, other than event-level and module-level Caching will show
#' changes and will be rerun. Function-level Caching appears unaffected.
#' In other words, this should cause no changes to running the module code via
#' \code{simInit} and \code{spades}.
#'
#' This will move all functions that are not already in an \code{.R} file
#' in the \code{R} folder into that folder, one function per file, including the
#' \code{doEvent.xxx} function. It will not
#' touch any other functions already in the \code{"R"} folder. It will also create
#' and fill a minimal \code{DESCRIPTION} file. This will leave the `defineModule`
#' function call as the only code in the main module file. This \code{defineModule}
#' and a \code{doEvent.xxx} are the only 2 elements that are required for an R
#' package to be considered a SpaDES module. With these changes, the module should
#' still function normally, but will be able to act like an
#' R package, e.g., for writing function documentation with \code{roxygen2},
#' using the \code{testthat} infrastructure, etc.
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
#' Currently, \code{SpaDES.core} does not install the package under any circumstances.
#' It will load it via
#' \code{pkgdown::load_all}, and optionally (option("spades.moduleDocument" = TRUE))
#' build documentation via \code{roxygen2::roxygenise}
#' within the \code{simInit} call. This means that any modifications to source code
#' will be read in during the \code{simInit} call, as is the practice when a module
#' is not a package.
#'
#' @section Exported functions:
#'
#' The only function that will be exported by default is the \code{doEvent.xxx},
#' where \code{xxx} is the module name. If any other module is to be exported, it must
#' be explicitly exported with e.g., \code{@export}, and then building the \code{NAMESPACE}
#' file, e.g., via \code{devtools::document(moduleRootPath)}. NOTE: as long as all
#' the functions are being used inside each other, and they all can be traced back
#' to a call in \code{doEvent.xxx}, then there is no need to export anything else.
#'
#' @section DESCRIPTION:
#'
#' The \code{DESCRIPTION} file that is created (destroying any existing \code{DESCRIPTION}
#' file) with this function will have
#' several elements that a user may wish to change. Notably, all packages that were
#' in \code{reqdPkgs} in the SpaDES module metadata will be in the \code{Imports}
#' section of the \code{DESCRIPTION}. To accommodate the need to see these functions,
#' a new R script, \code{imports.R} will be created with \code{@import} for each
#' package in \code{reqdPkgs} of the module metadata. However, if a module already has used
#' \code{@importFrom} for importing a function from a package, then the generic
#' \code{@import} will be omitted for that (those) package(s).
#' So, a user should likely follow standard R package
#' best practices and use \code{@importFrom} to identify the specific functions that
#' are required within external packages, thereby limiting function name collisions
#' (and the warnings that come with them).
#'
#' Other elements of a standard \code{DESCRIPTION} file that will be missing or possibly
#' inappropriately short are \code{Title}, \code{Description}, \code{URL},
#' \code{BugReports}.
#'
#' @section Installing as a package:
#'
#' There is no need to "install" the source code as a package because \code{simInit}
#' will load it on the fly. But, there may be reasons to install it, e.g., to have
#' access to individual functions, help manual, running tests etc. To do this,
#' simply use the \code{devtools::install(pathToModuleRoot)}. Even if it is installed,
#' \code{simInit} will nevertheless run \code{pkgload::load_all} to ensure the
#' \code{spades} call will be using the current source code.
#'
#' @export
#' @param module Character string of module name, without path
#' @param path Character string of modulePath. Defaults to
#'   \code{getOption("spades.modulePath")}
#' @param buildDocuments A logical. If \code{TRUE}, the default, then the documentation
#'   will be built, if any exists, using \code{roxygen2::roxygenise}
convertToPackage <- function(module = NULL, path = getOption("spades.modulePath"),
                             buildDocuments = TRUE) {
  mainModuleFile <- file.path(path, unlist(module), paste0(unlist(module), ".R"))
  aa <- parse(mainModuleFile)

  defModule <- grepl("^defineModule", aa)
  whDefModule <- which(defModule)
  whNotDefModule <- which(!defModule)

  NAMESPACEFile <- file.path(dirname(mainModuleFile), "NAMESPACE")
  hasNamespaceFile <- file.exists(NAMESPACEFile)

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

  filePathImportSpadesCore <- file.path(dirname(mainModuleFile), "R", "imports.R")

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
  d$Imports <- Require::extractPkgName(deps)
  versionNumb <- Require::extractVersionNumber(deps)
  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  missingSpace <- !grepl("[[:space:]]", inequality)
  if (any(missingSpace))
    inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])
  hasSC <- grepl("SpaDES.core", d$Imports)
  if (all(!hasSC))
    d$Imports <- c("SpaDES.core", d$Imports)

  namespaceImports <- d$Imports
  # Create "import all" for each of the packages, unless it is already in an @importFrom
  if (hasNamespaceFile) {
    nsTxt <- readLines(NAMESPACEFile)
    hasImportFrom <- grepl("importFrom", nsTxt)
    if (any(hasImportFrom)) {
      pkgsNotNeeded <- unique(gsub(".+\\((.+)\\,.+\\)", "\\1", nsTxt[hasImportFrom]))
      namespaceImports <- grep(paste(pkgsNotNeeded, collapse = "|"),
                               namespaceImports, invert = TRUE, value = TRUE)
    }
  }

  cat(paste0("#' @import ", namespaceImports, "\nNULL\n"), sep = "\n",
      file = filePathImportSpadesCore, fill = TRUE)

  d$Imports[hasVersionNumb] <- paste(d$Imports[hasVersionNumb], inequality)

  dFile <- file.path(dirname(mainModuleFile), "DESCRIPTION")

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", d$Description), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Imports))
    cat(c("Imports:", paste("   ", d$Imports, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

  Suggests = c('knitr', 'rmarkdown')
  cat(c("Suggests:", paste("   ", Suggests, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)

  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
  cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
  cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
  cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
  cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)


  message("New/updated DESCRIPTION file is: ", dFile)

  if (isTRUE(buildDocuments)) {
    message("Building documentation")
    m <- dirname(mainModuleFile)
    roxygen2::roxygenise(m, roclets = NULL) # This builds documentation, but also exports all functions ...
    pkgload::dev_topic_index_reset(m)
    pkgload::unload(.moduleNameNoUnderscore(basename2(m))) # so, unload here before reloading without exporting
  }

  RBuildIgnoreFile <- file.path(dirname(mainModuleFile), ".Rbuildignore")
  cat("^.*\\.Rproj$
^\\.Rproj\\.user$
^_pkgdown\\.yml$
.*\\.tar\\.gz$
.*\\.toc$
.*\\.zip$
^\\.lintr$
CONTRIBUTING\\.md
cran-comments\\.md
^docs$
^LICENSE$
vignettes/.*_cache$
vignettes/.*\\.log$
^\\.httr-oauth$
^revdep$
^\\.github$
^codecov\\.yml$
^CRAN-RELEASE$
^data/*
^.git
^.gitignore
^.gitmodules
    ", sep = "\n",
      file = RBuildIgnoreFile, fill = TRUE)



  return(invisible())

}
