utils::globalVariables(c(
  "VersionOnRepos", "i.packageFullName", "packageFullName", "hasHEAD"
))

#' Evaluate a metadata field that may be an unevaluated call
#'
#' Module metadata is captured unevaluated, so a field written as
#' `description = paste("a", "b")` is a call, not a string. Pasting the call
#' deparses it and leaks the function name into the field -- which is how
#' `Description: paste Fit statistical models ...` reached generated
#' DESCRIPTIONs. Evaluate, and fall back to the deparsed form if that fails.
#' @keywords internal
.evalMetadataField <- function(x) {
  v <- try(eval(x), silent = TRUE)
  if (inherits(v, "try-error")) paste(deparse(x), collapse = " ")
  else paste(v, collapse = " ")
}

#' Make `DESCRIPTION` file(s) from SpaDES module metadata
#'
#' @description
#' A SpaDES module declares its dependencies in `reqdPkgs` inside `defineModule()`,
#' not in a `DESCRIPTION`. This translates that metadata into a `DESCRIPTION`:
#' it is what [convertToPackage()] needs in order to turn a module into an R
#' package, and what `SpaDES.project::makeDESCRIPTION()` uses to give a whole
#' project one dependency manifest that `pak` or `renv` can act on.
#'
#' Note that module CI does *not* need this: the shared `render-module-rmd`
#' workflow resolves a module's dependencies with [packages()] and
#' `Require::Require()` directly from the metadata.
#'
#' `Imports` comes from `reqdPkgs`, with version inequalities preserved and
#' `SpaDES.core` added if absent. Any `reqdPkgs` entry naming a GitHub
#' repository also becomes a `Remotes` entry, so the resulting `DESCRIPTION` is
#' installable by `pak` without further help. A `(HEAD)` specification is
#' resolved to `>= <version currently on the repository>`, since `HEAD` is not
#' something a `DESCRIPTION` can express.
#'
#' @param modules A character vector of module names.
#' @param modulePath Character. The path containing the modules, usually
#'   `modulePath()` or `paths$modulePath`.
#' @param projectPath Character. Only used when `singleDESCRIPTION = TRUE`.
#' @param singleDESCRIPTION Logical. If `TRUE`, one `DESCRIPTION` is written for
#'   all `modules` combined, with redundant `reqdPkgs` entries trimmed across
#'   them. If `FALSE` (default), one per module, written beside the module.
#' @param package,title,date,description,version,authors Optional overrides for
#'   the corresponding `DESCRIPTION` fields. Each defaults to the value in the
#'   module metadata.
#' @param suggests Character vector written to `Suggests`.
#' @param merge Logical. If `TRUE` (default) and a `DESCRIPTION` already exists
#'   at the destination, its `Imports`/`Suggests`/`Remotes` are merged with the
#'   generated ones rather than replaced, so entries added by hand survive
#'   regeneration.
#' @param write Logical. If `TRUE` (default), write the file(s); if `FALSE`,
#'   write to a temporary file instead, which is useful for inspection and in CI.
#' @param verbose Numeric or logical. Passed to `Require`'s verbosity.
#' @param metadataList Optional pre-parsed module source, as from
#'   `parse(<module>.R, keep.source = TRUE)`. Supply it to avoid re-parsing.
#' @param ... Currently unused.
#'
#' @return Invisibly, the path(s) of the `DESCRIPTION` file(s) written.
#' @export
#' @rdname DESCRIPTIONfromModule
DESCRIPTIONfromModule <- function(modules, modulePath, projectPath = ".",
                                  singleDESCRIPTION = FALSE,
                                  package, title, date, description, version, authors,
                                  suggests = c("knitr", "rmarkdown", "testthat",
                                               "withr", "roxygen2"),
                                  merge = TRUE, write = TRUE,
                                  verbose = getOption("Require.verbose"),
                                  metadataList, ...) {
  if (is.null(verbose)) verbose <- 1L

  if (missing(metadataList)) {
    mainModuleFile <- file.path(modulePath, unlist(modules), paste0(unlist(modules), ".R"))
    packageFolderName <- dirname(mainModuleFile)
    metadataList <- lapply(mainModuleFile, function(file) parse(file, keep.source = TRUE))
  } else {
    # packageFolderName is where a per-module DESCRIPTION gets written; it used to
    # be defined only in the branch above, so passing metadataList left it undefined.
    packageFolderName <- if (!missing(modulePath)) {
      file.path(modulePath, unlist(modules))
    } else {
      rep(projectPath, length(unlist(modules)))
    }
  }

  defModule <- lapply(metadataList, function(x) grepl(pattern = "^defineModule", x[[1]]))
  whDefModule <- lapply(defModule, function(x) which(x[[1]]))
  mds <- Map(whDefMod = whDefModule, defMod = metadataList,
             function(whDefMod, defMod) defMod[[whDefMod]][[3]])
  names(mds) <- modules

  mods <- if (singleDESCRIPTION) "Project" else modules

  # missing() only answers for the frame that owns the formal, so resolve these
  # here rather than inside the per-module closure below.
  hasPackage     <- !missing(package)
  hasTitle       <- !missing(title)
  hasDescription <- !missing(description)
  hasVersion     <- !missing(version)
  hasDate        <- !missing(date)
  hasAuthors     <- !missing(authors)

  # One `d` per DESCRIPTION to be written. A single shared `d` meant that, with
  # several modules and singleDESCRIPTION = FALSE, every file got the LAST
  # module's metadata.
  dList <- lapply(mods, function(module) {
    md <- mds[[module]]
    d <- list()
    d$Package <- if (hasPackage) package else .moduleNameNoUnderscore(module)
    d$Type <- "Package"
    d$Title <- if (hasTitle) title else .evalMetadataField(md$name)
    d$Description <- if (hasDescription) description else .evalMetadataField(md$description)
    # Module metadata carries `version = list(<module> = "x.y.z")` as an
    # unevaluated call. Take this module's entry; pasting the call itself
    # vectorises over it and emits two "Version:" lines ("list", then "1.2.3").
    d$Version <- if (hasVersion) {
      version
    } else {
      v <- try(eval(md$version), silent = TRUE)
      if (inherits(v, "try-error") || !length(v)) {
        NA_character_
      } else {
        as.character(if (module %in% names(v)) v[[module]] else v[[1]])
      }
    }
    d$Date <- if (hasDate) date else format(Sys.Date())
    d$Authors <- if (hasAuthors) authors else md$authors
    d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
    d
  })
  names(dList) <- mods

  pfnAllList <- Map(md = mds, function(md) toPkgDTFull(unlist(eval(md$reqdPkgs))))
  if (singleDESCRIPTION)
    pfnAllList <- list(data.table::rbindlist(pfnAllList, fill = TRUE, use.names = TRUE))

  # Each output needs its own metadata and its own destination folder; with
  # singleDESCRIPTION there is exactly one of each.
  folders <- if (singleDESCRIPTION) projectPath else packageFolderName

  dFiles <- Map(pfnAll = pfnAllList, d = dList, folder = folders,
                f = function(pfnAll, d, folder) {
    pfnAll <- trimRedundancies(pfnAll)

    # `HEAD` is not expressible in a DESCRIPTION; pin it to what the repository
    # currently offers.
    pfnAll[, hasHEAD := grepl("\\(HEAD\\)", packageFullName)]
    whHEAD <- grep("\\(HEAD\\)", pfnAll$packageFullName)
    if (length(whHEAD)) {
      pkgDT <- getVersionOnRepos(pfnAll[whHEAD], repos = getOption("repos"), purge = FALSE)
      pkgDT[which(hasHEAD),
            packageFullName := gsub("HEAD", paste0(">=", VersionOnRepos), packageFullName)]
      pfnAll[pkgDT[, c("Package", "packageFullName")],
             packageFullName := i.packageFullName, on = "Package"]
    }

    deps <- pfnAll$packageFullName
    imports <- pfnAll$Package

    versionNumb <- Require::extractVersionNumber(deps)
    hasVersionNumb <- !is.na(versionNumb)
    inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
    missingSpace <- !grepl("[[:space:]]", inequality)
    if (any(missingSpace))
      inequality[missingSpace] <- gsub("([=><]+)", "\\1 ", inequality[missingSpace])
    imports[hasVersionNumb] <- paste(imports[hasVersionNumb], inequality)
    if (all(!grepl("SpaDES.core", imports)))
      imports <- c("SpaDES.core", imports)
    d$Imports <- imports

    d$Suggests <- suggests

    # extractPkgGitHub() rather than grepl("/"): a version inequality or a local
    # path can contain a slash without naming a GitHub repository.
    needRemotes <- which(!is.na(Require::extractPkgGitHub(deps)))
    d$Remotes <- if (length(needRemotes)) Require::trimVersionNumber(deps[needRemotes])

    dFile <- if (isTRUE(write)) {
      if (singleDESCRIPTION) file.path(projectPath, "DESCRIPTION")
      else filenameFromFunction(folder, "DESCRIPTION", fileExt = "")
    } else {
      Require::tempfile2()
    }

    # Read before truncating, so hand-added entries can be merged back in.
    origDESCtxt <- if (isTRUE(merge) && file.exists(dFile)) read.dcf(dFile) else character()

    cat(paste("Package:", d$Package), file = dFile, sep = "\n")
    cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
    cat(paste("Description:", paste(d$Description, collapse = " ")),
        file = dFile, sep = "\n", append = TRUE)
    cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
    cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)

    mergeField(origDESCtxt = origDESCtxt, field = d$Imports, fieldName = "Imports", dFile)
    mergeField(origDESCtxt = origDESCtxt, field = d$Suggests, fieldName = "Suggests", dFile)
    if (length(d$Remotes) || length(origDESCtxt))
      mergeField(origDESCtxt = origDESCtxt, field = d$Remotes, fieldName = "Remotes", dFile)

    cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)
    cat("License: GPL-3", sep = "\n", file = dFile, append = TRUE)
    cat("VignetteBuilder: knitr, rmarkdown", sep = "\n", file = dFile, append = TRUE)
    cat("ByteCompile: yes", sep = "\n", file = dFile, append = TRUE)
    cat("Roxygen: list(markdown = TRUE)", sep = "\n", file = dFile, append = TRUE)
    if (requireNamespace("roxygen2", quietly = TRUE))
      cat(paste0("RoxygenNote: ", as.character(utils::packageVersion("roxygen2"))),
          sep = "\n", file = dFile, append = TRUE)

    messageVerbose("DESCRIPTION file written to ", dFile, verbose = verbose)
    dFile
  })

  invisible(unlist(dFiles))
}
