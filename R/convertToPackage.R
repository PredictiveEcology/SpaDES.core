#' Convert standard module code into an R package
#'
#' *EXPERIMENTAL -- USE WITH CAUTION*. This function will only create the
#' necessary source files so that all the code can be used (and installed) like an R package.
#' This function does not install anything (e.g., `devtools::install`). After
#' running this function, `simInit` will automatically detect that this is now
#' a package and will load the functions (via `pkgload::load_all`) from the source files.
#' This will have the effect that it emulates the "non-package" behaviour of a
#' SpaDES module exactly. After running this function, current tests show no
#' impact on module behaviour, other than event-level and module-level Caching will show
#' changes and will be rerun. Function-level Caching appears unaffected.
#' In other words, this should cause no changes to running the module code via
#' `simInit` and `spades`.
#'
#' This will move all functions that are not already in an `.R` file
#' in the `R` folder into that folder, one function per file, including the
#' `doEvent.xxx` function. It will not
#' touch any other functions already in the `"R"` folder. It will also create
#' and fill a minimal `DESCRIPTION` file. This will leave the `defineModule`
#' function call as the only code in the main module file. This `defineModule`
#' and a `doEvent.xxx` are the only 2 elements that are required for an R
#' package to be considered a SpaDES module. With these changes, the module should
#' still function normally, but will be able to act like an
#' R package, e.g., for writing function documentation with `roxygen2`,
#' using the `testthat` infrastructure, etc.
#'
#' This function is intended to be run once for a module that was created using
#' the "standard" SpaDES module structure (e.g., from a `newModule` call). There
#' is currently no way to "revert" the changes from R (though it can be done using
#' version control utilities if all files are under version control, e.g., GitHub).
#' Currently `SpaDES.core` identifies a module as being a package if it has
#' a `DESCRIPTION` file, or if it has been installed to the `.libPaths()`
#' e.g., via `devtools::install` or the like. So one can simply remove the
#' package from `.libPaths` and delete the `DESCRIPTION` file and
#' `SpaDES.core` will treat it as a normal module.
#'
#' @return
#' This is run for its side effects. There will be a new or modified
#' `DESCRIPTION` file in the root directory of the module. Any functions that
#' were in the main module script (i.e., the .R file whose filename is the name of
#' the module and is in the root directory of the module) will be moved to individual
#' `.R` files in the `R` folder. Any function with a dot prefix will have the
#' dot removed in its respective filename, but the function name is unaffected.
#'
#' Currently, `SpaDES.core` does not install the package under any circumstances.
#' It will load it via `pkgdown::load_all`, and optionally (`option("spades.moduleDocument" = TRUE)`)
#' build documentation via `roxygen2::roxygenise` within the `simInit` call.
#' This means that any modifications to source code
#' will be read in during the `simInit` call, as is the practice when a module
#' is not a package.
#'
#' @section Exported functions:
#'
#' The only function that will be exported by default is the `doEvent.xxx`,
#' where `xxx` is the module name. If any other module is to be exported, it must
#' be explicitly exported with e.g., `@export`, and then building the `NAMESPACE`
#' file, e.g., via `devtools::document(moduleRootPath)`. NOTE: as long as all
#' the functions are being used inside each other, and they all can be traced back
#' to a call in `doEvent.xxx`, then there is no need to export anything else.
#'
#' @section DESCRIPTION:
#'
#' The `DESCRIPTION` file that is created (destroying any existing `DESCRIPTION`
#' file) with this function will have
#' several elements that a user may wish to change. Notably, all packages that were
#' in `reqdPkgs` in the SpaDES module metadata will be in the `Imports`
#' section of the `DESCRIPTION`. To accommodate the need to see these functions,
#' a new R script, `imports.R` will be created with `@import` for each
#' package in `reqdPkgs` of the module metadata. However, if a module already has used
#' `@importFrom` for importing a function from a package, then the generic
#' `@import` will be omitted for that (those) package(s).
#' So, a user should likely follow standard R package
#' best practices and use `@importFrom` to identify the specific functions that
#' are required within external packages, thereby limiting function name collisions
#' (and the warnings that come with them).
#'
#' Other elements of a standard `DESCRIPTION` file that will be missing or possibly
#' inappropriately short are `Title`, `Description`, `URL`,
#' `BugReports`.
#'
#' @section Installing as a package:
#'
#' There is no need to "install" the source code as a package because `simInit`
#' will load it on the fly. But, there may be reasons to install it, e.g., to have
#' access to individual functions, help manual, running tests etc. To do this,
#' simply use the `devtools::install(pathToModuleRoot)`. Even if it is installed,
#' `simInit` will nevertheless run `pkgload::load_all` to ensure the
#' `spades` call will be using the current source code.
#'
#' @param module Character string of module name, without path
#'
#' @param path Character string of modulePath. Defaults to  `getOption("spades.modulePath")`.
#'
#' @param buildDocuments A logical. If `TRUE`, the default, then the documentation
#'   will be built, if any exists, using `roxygen2::roxygenise`.
#'
#' @return invoked for the side effect of converting a module to a package
#'
#' @export
convertToPackage <- function(module = NULL, path = getOption("spades.modulePath"),
                             buildDocuments = TRUE) {
  mainModuleFile <- file.path(path, unlist(module), paste0(unlist(module), ".R"))
  packageFolderName <- dirname(mainModuleFile)
  aa <- parse(mainModuleFile, keep.source = TRUE)
  rlaa <- readLines(mainModuleFile)
  gpd <- getParseData(aa)

  defModule <- grepl("^defineModule", aa)
  whDefModule <- which(defModule)
  whNotDefModule <- which(!defModule)

  NAMESPACEFile <- filenameFromFunction(packageFolderName, "NAMESPACE", fileExt = "")
  hasNamespaceFile <- file.exists(NAMESPACEFile)

  RsubFolder <- file.path(packageFolderName, "R")
  checkPath(RsubFolder, create = TRUE)

  parseWithRoxygen <- gpd[grep("#'", gpd$text), ]
  linesWithRoxygen <- parseWithRoxygen[, "line1"]

  fileNames <- lapply(whNotDefModule, function(element) {
    fn <- aa[[element]][[2]]
    filePath <- filenameFromFunction(packageFolderName, fn, "R")
    fnCh <- as.character(fn)
    parseWithFn <- gpd[which(gpd$text == fnCh & gpd$token == "SYMBOL"),]
    lineWithFn <- parseWithFn[, "line1"]
    wh <- which((lineWithFn - linesWithRoxygen) == 1) # is the roxygen next to function
    if (length(wh)) {
      # This means there is a roxygen block for this function -- must keep it with the function code
      lastRoxygenLine <- lineWithFn - 1 == linesWithRoxygen
      ff <- diff(linesWithRoxygen)
      ff[ff == 1] <- 0
      ff[ff > 0] <- 1
      ff <- cumsum(ff)
      ff <- c(0, ff)
      roxygenLinesForThisFn <- linesWithRoxygen[ff == ff[lastRoxygenLine]]

      # This removes lines if they are put into a file. That means, if there are
      #   any left over at the end, we will put them into their own file
      linesWithRoxygen <<- setdiff(linesWithRoxygen, roxygenLinesForThisFn)
      cat(rlaa[roxygenLinesForThisFn], file = filePath, sep = "\n", append = FALSE)
    }

    if (isTRUE(grepl("^doEvent", fn))) {
      if (!any(grepl("@export", aa[[element]][[3]])))
        cat("#' @export", file = filePath, sep = "\n", append = TRUE)
    }

    cat(format(aa[[element]]), file = filePath, sep = "\n", append = TRUE)
  })

  otherStuffFn <- filenameFromFunction(packageFolderName, "other", "R")
  cat("
makeActiveBinding('mod', SpaDES.core:::activeModBindingFunction, ",
paste0('asNamespace(SpaDES.core:::.moduleNameNoUnderscore(\'',module,'\'))'),")

makeActiveBinding('Par', SpaDES.core:::activeParBindingFunction, ",
paste0('asNamespace(SpaDES.core:::.moduleNameNoUnderscore(\'',module,'\'))'),")

", file = otherStuffFn)

  if (length(linesWithRoxygen) > 0) {
    message("There was some roxygen2 documentation that was not immediately above ",
            "a function; it is being saved in R/documentation.R ... please confirm that ",
            "the documentation is correct.")
    cat(rlaa[linesWithRoxygen], file = filenameFromFunction(packageFolderName, "documentation", "R")
          , sep = "\n", append = FALSE)
    linesWithRoxygen <- character()
  }

  filePathImportSpadesCore <- filenameFromFunction(packageFolderName, "imports", "R")# file.path(dirname(mainModuleFile), "R", "imports.R")

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

  dFile <- filenameFromFunction(packageFolderName, "DESCRIPTION", fileExt = "")

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
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
    m <- packageFolderName
    roxygen2::roxygenise(m, roclets = NULL) # This builds documentation, but also exports all functions ...
    pkgload::dev_topic_index_reset(m)
    pkgload::unload(.moduleNameNoUnderscore(basename2(m))) # so, unload here before reloading without exporting
  }

  RBuildIgnoreFile <- filenameFromFunction(packageFolderName, ".Rbuildignore", fileExt = "")
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

filenameFromFunction <- function(packageFolderName, fn = "", subFolder = "", fileExt = ".R") {
  normPath(file.path(packageFolderName, subFolder, paste0(gsub("\\.", "", fn), fileExt)))
}
