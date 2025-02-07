#' Convert standard module code into an R package
#'
#' *EXPERIMENTAL -- USE WITH CAUTION*. This function will create a `DESCRIPTION`
#' file if one does not exist, based on the module metadata. If one exists, it will
#' update it with any additional information: **it will not remove packages that are
#' removed from the metadata**. It will create, if one does not
#' exist, or update a `.Rbuildignore` file. If `importAll = TRUE` It will create a file named `R/imports.R`,
#' which will import all functions.  This function will make no changes to
#' any existing source file of a SpaDES.module. If `buildDocumentation = TRUE`,
#' it will build documentation `.Rd` files from `roxygen2` tags.
#'
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
#' This function will create
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
#' @section Reverting:
#' Currently, this is not a reversible process. We recommend trying one module at
#' a time, running your code. If all seems to work, then great. Commit the changes.
#' If things don't seem to work, then revert the changes and continue on as before.
#' Ideally, file a bug report on the `SpaDES.core` GitHub.com pages.
#'
#' Currently
#' @return Invoked for its side effects. There will be a new or modified
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
#' @param path Character string of `modulePath`. Defaults to  `getOption("spades.modulePath")`.
#'
#' @param buildDocuments A logical. If `TRUE`, the default, then the documentation
#'   will be built, if any exists, using `roxygen2::roxygenise`.
#' @param importAll A logical. If `TRUE`, then every package named in `reqdPkgs` will
#' have an `@importFrom <pkgName>`, meaning **every** function from every package will
#' be imported. If `FALSE`, then only functions explicitly imported using
#' `@importFrom <pkgName> <functionName>` will be imported.
#' @inheritParams reproducible::Cache
#'
#' @return invoked for the side effect of creating DESCRIPTION file, a `.Rbuildingore`
#' file and possibly building documentatation from roxygen tags.
#'
#' @export
#' @examples
#' if (requireNamespace("ggplot2") && requireNamespace("pkgload") ) {
#'   tmpdir <- tempdir2()
#'   newModule("test", tmpdir, open = FALSE)
#'   createDESCRIPTIONandDocs("test", path = tmpdir)
#' }
createDESCRIPTIONandDocs <- function(module = NULL, path = getOption("spades.modulePath"),
                                     importAll = TRUE,
                                     buildDocuments = TRUE,
                                     verbose = getOption("Require.verbose")) {
  stopifnot(
    requireNamespace("pkgload", quietly = TRUE),
    requireNamespace("roxygen2", quietly = TRUE)
  )

  mainModuleFile <- file.path(path, unlist(module), paste0(unlist(module), ".R"))
  packageFolderName <- dirname(mainModuleFile)
  aa <- parse(mainModuleFile, keep.source = TRUE)
  rlaa <- readLines(mainModuleFile)
  gpd <- getParseData(aa)

  defModule <- grepl("^defineModule", aa)
  whDefModule <- which(defModule)
  whNotDefModule <- which(!defModule)

  linesWithDefModule <- gpd[grep("defineModule", gpd$text) - 1, ][, c("line1", "line2")]

  doEvent <- grepl(paste0("^doEvent.", module), aa)
  whDoEvent <- which(doEvent)
  whNoDoEvent <- which(!doEvent & !defModule)

  # file.copy(mainModuleFile, file.path(path, unlist(module), "R", paste0(unlist(module), ".R")))

  NAMESPACEFile <- filenameFromFunction(packageFolderName, "NAMESPACE", fileExt = "")
  hasNamespaceFile <- file.exists(NAMESPACEFile)

  RsubFolder <- file.path(packageFolderName, "R")
  checkPath(RsubFolder, create = TRUE)

  parseWithRoxygen <- gpd[grep("#'", gpd$text), ]
  linesWithRoxygen <- parseWithRoxygen[, "line1"]
  nextElement <- c(whNotDefModule[-1], Inf)


  filePathImportSpadesCore <- filenameFromFunction(packageFolderName, "imports", "R")# file.path(dirname(mainModuleFile), "R", "imports.R")

  md <- aa[[whDefModule]][[3]]
  deps <- unlist(eval(md$reqdPkgs))

  dFile <- DESCRIPTIONfileFromModule(module, md, deps, hasNamespaceFile, NAMESPACEFile, filePathImportSpadesCore,
                                     packageFolderName, verbose = verbose)

  if (isTRUE(buildDocuments)) {
    message("Building documentation")
    m <- packageFolderName
    tmpSrcForDoc <- "R/tmp.R"
    cat(rlaa[-(linesWithDefModule[[1]]:linesWithDefModule[[2]])], sep = "\n", file = tmpSrcForDoc)
    on.exit(unlink(tmpSrcForDoc))
    roxygen2::roxygenise(m, roclets = NULL) # This builds documentation, but also exports all functions ...
    pkgload::dev_topic_index_reset(m)
    pkgload::unload(.moduleNameNoUnderscore(basename2(m))) # so, unload here before reloading without exporting
  }

  RBuildIgnoreFile <- filenameFromFunction(packageFolderName, "", fileExt = ".Rbuildignore")

  rbi <- paste("^.*\\.Rproj$
^\\.Rproj\\.user$
^_pkgdown\\.yml$
.*\\.tar\\.gz$
.*\\.toc$
.*\\.zip$
^\\.lintr$
CONTRIBUTING\\.md
cran-comments\\.md
^docs$
citation.*
figures
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
^.gitmodules", sep = "\n")
  rbi <- strsplit(rbi, split = "\n")[[1]]

  modFiles <- c(paste0(module, ".*"), ".*zip")

  if (file.exists(RBuildIgnoreFile)) {
    startCat <- readLines(RBuildIgnoreFile)
    rbi <- unique(c(startCat, rbi))
  }

  rbi <- unique(c(rbi, modFiles))
  cat(rbi, file = RBuildIgnoreFile, fill = TRUE, sep = "\n")

  return(invisible())
}

filenameFromFunction <- function(packageFolderName, fn = "", subFolder = "", fileExt = ".R") {
  normPath(file.path(packageFolderName, subFolder, paste0(gsub("\\.", "", fn), fileExt)))
}

filenameForMainFunctions <- function(module, modulePath = ".")
  normPath(file.path(modulePath, unlist(module), "R", paste0(unlist(basename(module)), "Fns.R")))




DESCRIPTIONfileFromModule <- function(module, md, deps, hasNamespaceFile, NAMESPACEFile, filePathImportSpadesCore,
                                      packageFolderName, verbose = getOption("Require.verbose")) {
  d <- list()
  d$Package <- .moduleNameNoUnderscore(module)
  d$Type <- "Package"

  d$Title <- md$name
  d$Description <- md$description
  d$Version <- as.character(eval(md$version[[2]]))
  d$Date <- Sys.Date()
  d$Authors <- md$authors
  d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])


  hasSC <- grepl("SpaDES.core", deps)
  if (all(!hasSC))
    deps <- c("SpaDES.core", deps)

  d$Imports <- Require::extractPkgName(deps)
  versionNumb <- Require::extractVersionNumber(deps)
  needRemotes <- which(!is.na(Require::extractPkgGitHub(deps)))
  d$Remotes <- Require::trimVersionNumber(deps[needRemotes])

  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  missingSpace <- !grepl("[[:space:]]", inequality)
  if (any(missingSpace))
    inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])

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
  origDESCtxt <- if (file.exists(dFile)) read.dcf(dFile) else character()

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", paste(d$Description, collapse = " ")), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

  if (length(d$Imports) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Imports, fieldName = "Imports", dFile)

  suggs <- c('knitr', 'rmarkdown', 'testthat', 'withr', 'roxygen2')
  if (length(suggs) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = suggs, fieldName = "Suggests", dFile)

  if (length(d$Remotes) || length(origDESCtxt))
    mergeField(origDESCtxt = origDESCtxt, field = d$Remotes, fieldName = "Remotes", dFile)

  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
  cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
  cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
  cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
  cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)
  cat(paste0("RoxygenNote: ", as.character(packageVersion("roxygen2"))), sep = "\n", file = dFile, append = TRUE)


  messageVerbose("New/updated DESCRIPTION file is: ", dFile, verbose = verbose)
  return(dFile)
}

mergeField <- function(origDESCtxt, field, dFile, fieldName = "Imports") {
  fieldVals <- character()
  if (fieldName %in% colnames(origDESCtxt))
    fieldVals <- strsplit(origDESCtxt[, fieldName], split = ",+\n")[[1]]
  if (length(field)) {
    field <- trimRedundancies(unique(c(field, fieldVals)))
  }
  cat(c(paste0(fieldName, ":"), paste("   ", sort(field$packageFullName), collapse = ",\n")),
      sep = "\n", file = dFile, append = TRUE)
}
