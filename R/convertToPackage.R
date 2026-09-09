#' Convert standard module code into an R package
#'
#' *EXPERIMENTAL -- USE WITH CAUTION*. This function attempts to convert a
#' SpaDES module to the closest rendition of the same functionality, but in an
#' R package. The main change is that instead of `SpaDES.core` parsing the functions
#' using custom parsing tools, it will use `pkgload::load_all` on the package functions.
#' These
#'
#' `convertToPackage` will:
#' \enumerate{
#'   \item **copy** the functions defined in the main module file (`moduleName.R`)
#'         into `R/READONLYFromMainModuleFile.R`. The main module file is **not**
#'         modified: it still holds both `defineModule(...)` and the function
#'         definitions afterwards. The `R/` copy is what package tooling
#'         (`pkgload::load_all`, `covr`) sees; the module itself continues to be
#'         parsed from `moduleName.R` as before. Only written when
#'         `buildDocuments = TRUE`;
#'   \item build documentation from all the \pkg{roxygen2} tags;
#'   \item write a `DESCRIPTION` file from the SpaDES module metadata, via
#'         [DESCRIPTIONfromModule()];
#'   \item write `R/imports.R` with one `@import` per `reqdPkgs` entry;
#'   \item write a `NAMESPACE` file from the \pkg{roxygen2} tags. Note this only
#'         happens when `buildDocuments = TRUE`; with `FALSE` you get a
#'         `DESCRIPTION` and `R/imports.R` but no `NAMESPACE` and no function
#'         copies, which is *not* enough for `devtools::test()` or
#'         `covr::package_coverage()` to work;
#'   \item write an `.Rbuildignore`.
#' }
#'
#' @section Testing a module as a package:
#'
#' `convertToPackage()` cannot be called from `tests/testthat/setup.R`:
#' `devtools::test()` and `devtools::check()` both require a `DESCRIPTION` before
#' `testthat` ever sources `setup.R`, so the conversion has to happen *before*
#' they are called, not from inside them.
#'
#' Use `destinationPath` to build the package rendition somewhere disposable
#' (e.g. `tempdir()`), leaving the module directory untouched:
#'
#' ```
#' pkg <- convertToPackage("myModule", path = "..", destinationPath = tempdir())
#' devtools::test(pkg)
#' covr::package_coverage(pkg)
#' ```
#'
#' Two things commonly block `R CMD INSTALL` (and therefore
#' `covr::package_coverage()`) on the result, both inherited from the module:
#' an `authors` field written as `structure(list(...), class = "person")` rather
#' than a call to `person()`, which R rejects as an unsafe call in `Authors@R`;
#' and the `tests/unitTests.R` file left behind by [newModule()], which
#' `R CMD check` runs and which fails with `No test files found` because it
#' hardcodes a relative path. [newModule()] also writes no `tests/testthat.R`,
#' so without one `R CMD check` runs no tests at all.
#'
#' A user can continue to use the module code as before, i.e., by editing it and
#' putting `browser()` etc. It will be parsed during `simInit`. Because the functions
#' are "in a package", they are automatically namespaced with each other, so that
#' when you want to use a function from that package, there is no need to put a
#' prefix with the package name.
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
#'   will be built, if any exists, using `roxygen2::roxygenise`. Note that with
#'   `FALSE` no `NAMESPACE` and no function copies are written, which is not
#'   enough for `devtools::test()` or `covr::package_coverage()`.
#'
#' @param destinationPath Character string. Optional. If supplied, the module
#'   directory is first copied to `file.path(destinationPath, module)` and the
#'   package rendition is built *there*, leaving the original module untouched.
#'   This is the non-destructive way to test or measure coverage on a module --
#'   see the *Testing a module as a package* section. If `NULL` (default) the
#'   module is converted in place, which is **not reversible**.
#'
#' @return Invoked for its side effects. Invisibly returns the path of the
#'   directory that now holds the package rendition -- the module directory
#'   itself, or the copy under `destinationPath`.
#'
#' @export
#' @examples
#' if (requireNamespace("ggplot2") && requireNamespace("pkgload") ) {
#'   tmpdir <- tempdir2()
#'   newModule("test", tmpdir, open = FALSE)
#'   convertToPackage("test", path = tmpdir)
#' }
#'
convertToPackage <- function(module = NULL, path = getOption("spades.modulePath"),
                             buildDocuments = TRUE, destinationPath = NULL) {
  stopifnot(
    requireNamespace("pkgload", quietly = TRUE),
    requireNamespace("roxygen2", quietly = TRUE)
  )

  module <- unlist(module)
  if (length(module) != 1L)
    stop("convertToPackage() converts one module at a time; got ", length(module))

  # Building into `destinationPath` leaves the module untouched. Everything below
  # writes relative to `packageFolderName`, and documentModule() re-derives the
  # main module file from it, so the whole module directory has to be copied --
  # not just <module>.R -- for the copy to be a working package.
  if (!is.null(destinationPath)) {
    srcDir <- file.path(path, module)
    if (!dir.exists(srcDir))
      stop("no module directory at ", srcDir)
    destDir <- file.path(destinationPath, module)
    unlink(destDir, recursive = TRUE)
    checkPath(destinationPath, create = TRUE)
    if (!all(file.copy(srcDir, destinationPath, recursive = TRUE)))
      stop("could not copy ", srcDir, " to ", destinationPath)
    path <- destinationPath
  }

  mainModuleFile <- file.path(path, module, paste0(module, ".R"))
  packageFolderName <- dirname(mainModuleFile)
  aa <- parse(mainModuleFile, keep.source = TRUE)
  gpd <- getParseData(aa)

  rlaa <- readLines(mainModuleFile)

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

  # fileNames <- Map(element = whDefModule, nextElement = whNotDefModule[1],
  #                  function(element, nextElement) {
  #                    i <- 0
  #                    fn <- filePath <- fnCh <- parseWithFn <- lineWithFn <- list()
  #                    for (elem in c(element, nextElement)) {
  #                      i <- i + 1
  #                      if (is.infinite(elem)) {
  #                        lineWithFn[[i]] <- length(rlaa) + 1
  #                        break
  #                      }
  #                      fn[[i]] <- aa[[elem]][[2]]
  #                      filePath[[i]] <- filenameFromFunction(packageFolderName, fn[[i]], "R")
  #                      fnCh[[i]] <- as.character(fn[[i]])
  #                      gpdLines <- which(gpd$text == fnCh[[i]] & gpd$token == "SYMBOL")
  #                      if (length(gpdLines) > 1)
  #                        for (gl in gpdLines) {
  #                          line1 <- gpd[gl, "line1"]
  #                          isTop <- any(gpd[gpd[, "line1"] == line1, "parent"] == 0)
  #                          if (isTRUE(isTop)) {
  #                            gpdLines <- gl
  #                            break
  #                          }
  #                        }
  #                      parseWithFn[[i]] <- gpd[gpdLines, ]
  #                      lineWithFn[[i]] <- parseWithFn[[i]][, "line1"]
  #                      if (length(lineWithFn[[i]]) > 1) {
  #                        if (i == 1) {
  #                          lineWithFn[[1]] <- lineWithFn[[1]][1]
  #                        } else {
  #                          whAfterLine1 <- which(lineWithFn[[2]] > lineWithFn[[1]])
  #                          if (length(whAfterLine1))
  #                            lineWithFn[[2]] <- lineWithFn[[2]][whAfterLine1[1]]
  #                        }
  #                      }
  #                    }
  #                    fn <- filenameForMainFunctions(module, path)
  #                    cat("#' @export", file = fn, sep = "\n", append = FALSE)
  #                    cat(rlaa[lineWithFn[[2]]:length(rlaa)],
  #                        file = fn, sep = "\n", append = TRUE)
  #                    cat(rlaa[1:(lineWithFn[[2]] - 1)], file = mainModuleFile,
  #                        sep = "\n", append = FALSE)
  #                  })

  filePathImportSpadesCore <- filenameFromFunction(packageFolderName, "imports", "R")# file.path(dirname(mainModuleFile), "R", "imports.R")

  md <- aa[[whDefModule]][[3]]
  deps <- unlist(eval(md$reqdPkgs))

  dFile <- DESCRIPTIONfromModule(module, metadataList = list(aa),
                                 modulePath = dirname(packageFolderName))

  # The "import everything" stub is convertToPackage's business, not a
  # DESCRIPTION writer's, so it stays here rather than being a side effect of
  # generating the DESCRIPTION.
  writeImportSpadesCore(deps, hasNamespaceFile, NAMESPACEFile, filePathImportSpadesCore)

  if (isTRUE(buildDocuments)) {
    documentModule(packageFolderName, gpd, linesWithDefModule)
  }

  RBuildIgnoreFile <- filenameFromFunction(packageFolderName, "", fileExt = ".Rbuildignore")

  startCat <- if (file.exists(RBuildIgnoreFile)) readLines(RBuildIgnoreFile) else character()

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

  rbi <- unique(c(startCat, rbi, modFiles))
  cat(rbi, file = RBuildIgnoreFile, fill = TRUE, sep = "\n")

  invisible(packageFolderName)
}

filenameFromFunction <- function(packageFolderName, fn = "", subFolder = "", fileExt = ".R") {
  normPath(file.path(packageFolderName, subFolder, paste0(gsub("\\.", "", fn), fileExt)))
}

filenameForMainFunctions <- function(module, modulePath = ".")
  normPath(file.path(modulePath, unlist(module), "R", paste0(unlist(basename(module)), "Fns.R")))


# Write the `#' @import <pkg>` stub that lets a converted module see its
# dependencies without per-function @importFrom tags. Extracted from the former
# DESCRIPTIONfileFromModule(), which conflated this with writing a DESCRIPTION;
# the DESCRIPTION half is now DESCRIPTIONfromModule(), shared with
# SpaDES.project::makeDESCRIPTION().
writeImportSpadesCore <- function(deps, hasNamespaceFile, NAMESPACEFile,
                                  filePathImportSpadesCore) {
  if (all(!grepl("SpaDES.core", deps)))
    deps <- c("SpaDES.core", deps)
  namespaceImports <- Require::extractPkgName(deps)

  # Skip any package already covered by an explicit @importFrom.
  if (isTRUE(hasNamespaceFile)) {
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
  invisible(namespaceImports)
}



mergeField <- function(origDESCtxt, field, dFile, fieldName = "Imports") {
  fieldVals <- character()
  if (fieldName %in% colnames(origDESCtxt))
    fieldVals <- strsplit(origDESCtxt[, fieldName], split = ",+\n")[[1]]
  if (length(field)) {
    field <- trimRedundancies(unique(c(field, fieldVals)))
    cat(c(paste0(fieldName, ":"), paste("   ", sort(field$packageFullName), collapse = ",\n")),
        sep = "\n", file = dFile, append = file.exists(dFile))
  }
  field
}

documentModule <- function(packageFolderName, gpd, linesWithDefModule) {
  message("Building documentation")
  m <- packageFolderName
  tmpSrcForDoc <- file.path(m, "R/READONLYFromMainModuleFile.R")
  mainModuleFile <- file.path(m, paste0(basename(m), ".R"))
  # if (missing(rlaa)) {
  rlaa <- readLines(mainModuleFile)
  # }

  if (missing(linesWithDefModule)) {
    if (missing(gpd)) {
      aa <- parse(mainModuleFile, keep.source = TRUE)
      gpd <- getParseData(aa)
    }
    linesWithDefModule <- gpd[grep("defineModule", gpd$text) - 1, ][, c("line1", "line2")]
  }
  if (!dir.exists(file.path(m, "R")))
    dir.create(file.path(m, "R"))
  cat(paste0("#% Generated by SpaDES.core: do not edit by hand
#% Please edit documentation in\n#%", mainModuleFile),
      file = tmpSrcForDoc)
  cat(rlaa[-(linesWithDefModule[[1]]:linesWithDefModule[[2]])], sep = "\n",
      file = tmpSrcForDoc, append = TRUE)
  # on.exit(unlink(tmpSrcForDoc))
  roxygen2::roxygenise(m, roclets = NULL) # This builds documentation, but also exports all functions ...
  pkgload::dev_topic_index_reset(m)
  pkgload::unload(.moduleNameNoUnderscore(basename2(m))) # so, unload here before reloading without exporting
}
