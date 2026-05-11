## Public API for the v2 module code checker.
##
## Two entry points:
##   * codeCheckModule(path = ...) -- standalone, runs against a module
##     directory on disk. No simInit required. Useful while authoring.
##   * .runCodeChecks2(sim, m, k) -- internal entry called from
##     simulation-parseModule.R when getOption("spades.codeCheckEngine")
##     is "v2" (the default once wired in). v1 (.runCodeChecks) remains
##     in code-checking.R and is reachable by setting the option to "v1".

## ---------------------------------------------------------------------------
## Standalone API
## ---------------------------------------------------------------------------

#' Statically check a SpaDES module's source code (v2)
#'
#' Walks the module's source via `xmlparsedata`, collects every reference to
#' `sim$x` / `sim[["x"]]` / `get("x", envir = envir(sim))` and to parameters
#' (`Par$x`, `P(sim)$x`, `params(sim)$mod$x`), then compares those uses to
#' the module's `defineModule()` metadata. Reports any mismatches as a
#' structured tibble of findings, optionally printed as grouped tables.
#'
#' This is the v2 implementation, selectable at `simInit()` time via
#' `options(spades.codeCheckEngine = "v2")` (the default). The legacy v1
#' checker is still available via `options(spades.codeCheckEngine = "v1")`.
#'
#' @param path Path to a module directory (containing `<modName>/<modName>.R`,
#'   and optionally an `R/` subfolder of helper scripts) or to a single `.R`
#'   file. If a directory, the module name is the directory's basename.
#' @param print Logical; print the grouped report. Default `TRUE`.
#' @param enable,disable Optional character vectors of rule IDs to restrict
#'   the run. See `names(SpaDES.core:::.CC_RULES)` for the catalogue.
#' @return A `data.frame` of findings (one row per problem). Returned
#'   invisibly. Empty if the module is clean.
#' @export
#' @rdname codeCheckModule
codeCheckModule <- function(path, print = TRUE, enable = NULL, disable = NULL) {
  info <- .cc_resolveModulePath(path)
  meta <- .cc_metadataFromSource(info$mainFile, info$module)
  uses <- .cc_collectModule(files = info$files, currentModule = info$module)
  findings <- .cc_runRules(uses, c(meta, list(module = info$module,
                                              moduleEnv = NULL)),
                           enable = enable, disable = disable)
  if (isTRUE(print)) .cc_report(findings, module = info$module)
  invisible(findings)
}

## ---------------------------------------------------------------------------
## simInit-time entry
## ---------------------------------------------------------------------------

## Drop-in replacement for .runCodeChecks(). Same signature.
.runCodeChecks2 <- function(sim, m, k, hadPrevMessage = FALSE) {
  dep <- sim@depends@dependencies[[k]]
  modEnv <- sim@.xData$.mods[[m]]

  ## file paths -- main module file + any R/ scripts under the module dir
  mainFile <- modEnv[["._sourceFilename"]]
  if (is.null(mainFile) || is.na(mainFile)) mainFile <- m
  files <- mainFile
  ## parsedFiles in sim@.xData[[".parsedFiles"]] is keyed by full path; pull
  ## the actual disk path from dep@filename if available
  modPath <- tryCatch(
    sim@modules[[m]] %||% NULL,
    error = function(e) NULL
  )

  ## try to locate other R/ files alongside the main file
  if (file.exists(mainFile)) {
    rDir <- file.path(dirname(mainFile), "R")
    if (dir.exists(rDir)) {
      extra <- list.files(rDir, pattern = "\\.[Rr]$", full.names = TRUE)
      files <- c(files, extra)
    }
  }

  meta <- list(
    module  = m,
    inputs  = stats::na.omit(dep@inputObjects$objectName),
    outputs = stats::na.omit(dep@outputObjects$objectName),
    params  = if (NROW(dep@parameters) > 0) dep@parameters$paramName else character(),
    otherModuleParams = .cc_otherModuleParams(sim, m),
    moduleEnv = modEnv,
    codetoolsOpts = .cc_codetoolsOpts(),
    files = files
  )

  uses <- .cc_collectModule(files = files, currentModule = m)
  findings <- .cc_runRules(uses, meta)
  ## stash on simList for tests / programmatic access
  cur <- sim@.xData[[".codeCheck"]] %||% list()
  cur[[m]] <- findings
  sim@.xData[[".codeCheck"]] <- cur
  .cc_report(findings, module = m)
  invisible()
}

## Build a named list module->params from already-parsed sibling modules.
.cc_otherModuleParams <- function(sim, currentModule) {
  out <- list()
  deps <- sim@depends@dependencies
  for (nm in names(deps)) {
    if (identical(nm, currentModule)) next
    p <- deps[[nm]]@parameters
    if (NROW(p) > 0) out[[nm]] <- p$paramName
  }
  out
}

.cc_codetoolsOpts <- function() {
  opt <- getOption("spades.moduleCodeChecks")
  if (isTRUE(opt) || is.null(opt)) {
    list(skipWith = TRUE, suppressNoLocalFun = TRUE,
         suppressParamUnused = FALSE, suppressPartialMatchArgs = FALSE,
         suppressUndefined = TRUE)
  } else if (is.list(opt)) {
    opt[intersect(names(opt), c("skipWith", "suppressNoLocalFun",
                                "suppressParamUnused",
                                "suppressPartialMatchArgs",
                                "suppressUndefined"))]
  } else {
    list()
  }
}

## ---------------------------------------------------------------------------
## Module path resolution + metadata sniffing for the standalone API
## ---------------------------------------------------------------------------

.cc_resolveModulePath <- function(path) {
  if (file.info(path)$isdir) {
    modName <- basename(normalizePath(path, mustWork = TRUE))
    main <- file.path(path, paste0(modName, ".R"))
    if (!file.exists(main)) {
      stop("Cannot find ", main, call. = FALSE)
    }
    rDir <- file.path(path, "R")
    extra <- if (dir.exists(rDir))
      list.files(rDir, pattern = "\\.[Rr]$", full.names = TRUE) else character()
    list(module = modName, mainFile = main, files = c(main, extra))
  } else {
    modName <- sub("\\.[Rr]$", "", basename(path))
    list(module = modName, mainFile = path, files = path)
  }
}

## Parse the metadata block from a module source file WITHOUT evaluating any
## sim-dependent expressions. We harvest:
##   - inputs:  objectName arg of every expectsInput()
##   - outputs: objectName arg of every createsOutput()
##   - params:  name arg of every defineParameter()
.cc_metadataFromSource <- function(mainFile, modName) {
  parsed <- .cc_parseFile(file = mainFile)
  doc <- parsed$doc
  list(
    inputs  = .cc_namesOfArg(doc, "expectsInput", "objectName"),
    outputs = .cc_namesOfArg(doc, "createsOutput", "objectName"),
    params  = .cc_namesOfArg(doc, "defineParameter", "name"),
    otherModuleParams = list()
  )
}

## Find first positional or named arg `argName` of every `fnName(...)` call
## and return its STR_CONST values (those statically resolvable).
.cc_namesOfArg <- function(doc, fnName, argName) {
  calls <- xml2::xml_find_all(
    doc,
    sprintf("//expr[expr/SYMBOL_FUNCTION_CALL[text()='%s']]", fnName)
  )
  out <- character()
  for (cal in calls) {
    val <- .cc_argValueStr(cal, argName)
    if (!is.na(val)) out <- c(out, val)
  }
  out
}

## Extract the string value of arg `argName` from a call expr. Tries named
## form first; falls back to first positional arg (after the function name).
.cc_argValueStr <- function(callExpr, argName) {
  named <- xml2::xml_find_first(
    callExpr,
    sprintf("SYMBOL_SUB[text()='%s']/following-sibling::expr[1]/STR_CONST",
            argName)
  )
  if (length(named) > 0 && !is.na(xml2::xml_text(named))) {
    return(gsub('^["\']|["\']$', "", xml2::xml_text(named)))
  }
  ## first positional arg = first <expr> after the function-name <expr>, that
  ## isn't preceded by SYMBOL_SUB on its left sibling
  positional <- xml2::xml_find_first(
    callExpr,
    "expr[2]/STR_CONST"
  )
  if (length(positional) > 0 && !is.na(xml2::xml_text(positional))) {
    return(gsub('^["\']|["\']$', "", xml2::xml_text(positional)))
  }
  NA_character_
}
