## Public API for the v2 module code checker.
##
## Two entry points:
##   * codeCheckModule(path = ...) -- standalone, runs against a module
##     directory on disk. No simInit required. Useful while authoring.
##     codeCheckModules() is the vectorized form (one or more modules).
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
#' `options(spades.codeCheckEngine = "v2")`. The legacy v1 checker is the
#' default; set the option to opt in to v2.
#'
#' @section Silencing findings:
#' Each finding in the printed report is tagged with its **rule id** in
#' brackets, e.g. `[conflicting_fn_unqualified]`; that id (or the `• <group>`
#' name it is printed under) is what you reference to silence it. Findings can
#' be suppressed three ways (all honoured both here and during `simInit()`):
#' \itemize{
#'   \item **Inline `# nolint` (module developer).** Put a `# nolint` comment on
#'     the offending source line to silence every rule there, or
#'     `# nolint: <rule_id>[, <rule_id>]` to silence only specific rules (a
#'     group name such as `globals` is accepted in place of a rule id). For a
#'     metadata finding such as `in_no_default`, place it anywhere within the
#'     `expectsInput()` / `createsOutput()` / `defineParameter()` declaration,
#'     e.g. `expectsInput("cloudFolderID", "character", desc = "...") # nolint: in_no_default`.
#'     This travels with the module and documents the intent.
#'   \item **`options(spades.codeChecksIgnore = ...)` (module user).** A named
#'     list keyed by rule id (or group name) whose values are object names to
#'     ignore, e.g.
#'     `options(spades.codeChecksIgnore = list(in_no_default = c("cloudFolderID", "ecoregionRst")))`.
#'     Lets someone running another author's module quiet specific findings
#'     without editing its source.
#'   \item **`options(spades.moduleCodeChecks = list(disable = ...))`.** Disable
#'     whole rules by id (or restrict with `enable = ...`).
#' }
#'
#' A related developer hint is `# nolint: vars a, b`: placed on a dynamic
#' bulk-assign line whose names can't be seen statically (e.g.
#' `list2env(someList, envir(sim))`), it asserts that objects `a`, `b` are
#' produced there, so they aren't reported as `out_declared_unused`.
#'
#' @section Rule catalogue:
#' The rule ids (printed in brackets in the report), grouped by the bucket they
#' appear under:
#' \itemize{
#'   \item **inputObjects** — `in_declared_unused` (declared input never used),
#'     `in_used_undeclared` (`sim$x` read but not in `inputObjects`),
#'     `in_no_default` (declared input has no default in `.inputObjects()`).
#'   \item **outputObjects** — `out_declared_unused` (declared output never
#'     assigned), `out_used_undeclared` (`sim$x <-` but not in `outputObjects`).
#'   \item **parameters** — `param_declared_unused`, `param_used_undeclared`,
#'     `param_used_other_module`.
#'   \item **module functions** — `must_return_sim` (a `doEvent.*` must return
#'     `sim`), `must_assign_to_sim`, `module_named_object` (`sim$<module>`
#'     collides with the module name), `clashing_module_fn`.
#'   \item **globals** — `conflicting_fn_unqualified` (a bare function name
#'     collides with a `raster::` namesake; qualify it, e.g. `raster::scale`).
#'   \item **unresolved** — `unresolved_accessor` (an accessor whose name could
#'     not be resolved statically).
#'   \item **codetools** — `codetools` (findings relayed from
#'     `codetools::checkUsageEnv`).
#'   \item **reqdPkgs** — `reqd_pkg_duplicate` (a package declared more than
#'     once in `reqdPkgs`, especially with conflicting source/version),
#'     `reqd_pkg_undeclared` (a `pkg::fn` whose `pkg` is not in `reqdPkgs`),
#'     `reqd_pkg_no_source` (best-effort, info: bare calls with no apparent
#'     source among the declared packages — only when all declared packages are
#'     installed).
#' }
#'
#' `codeCheckModule()` checks a single module. `codeCheckModules()` is the
#' vectorized form: it runs `codeCheckModule()` on each path in `paths` and
#' returns a list of findings named by module. When `paths` is not supplied it
#' defaults to every module directory under `getOption("spades.modulePath")`,
#' so `codeCheckModules()` with no arguments checks the whole project. It
#' replaces the manual idiom
#' `Map(codeCheckModule, dir(getOption("spades.modulePath"), full.names = TRUE))`.
#'
#' @param path Path to a module directory (containing `<modName>/<modName>.R`,
#'   and optionally an `R/` subfolder of helper scripts) or to a single `.R`
#'   file. If a directory, the module name is the directory's basename.
#' @param paths A character vector of module directories (or `.R` files), as
#'   accepted by `path`. Defaults to
#'   `dir(getOption("spades.modulePath"), full.names = TRUE)`.
#' @param print Logical; print the grouped report. Default `TRUE`.
#' @param enable,disable Optional character vectors of rule IDs to restrict
#'   the run. See `names(SpaDES.core:::.CC_RULES)` for the catalogue.
#' @return `codeCheckModule()` returns a `data.frame` of findings (one row per
#'   problem), invisibly; empty if the module is clean. `codeCheckModules()`
#'   returns, invisibly, a named list of such `data.frame`s (named by module).
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

#' @export
#' @rdname codeCheckModule
codeCheckModules <- function(paths = dir(getOption("spades.modulePath"), full.names = TRUE),
                             print = TRUE, enable = NULL, disable = NULL) {
  names(paths) <- basename(paths)
  findings <- lapply(paths, codeCheckModule, print = print,
                     enable = enable, disable = disable)
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
    reqdPkgs = .cc_reqdPkgsFromDep(dep, file = if (file.exists(mainFile)) mainFile else NA_character_),
    mainFile = if (file.exists(mainFile)) mainFile else NA_character_,
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
    otherModuleParams = list(),
    reqdPkgs = .cc_reqdPkgsFromSource(doc, file = mainFile),
    mainFile = mainFile
  )
}

## Harvest the `reqdPkgs` list from a `defineModule()` block. Returns a
## data.frame(spec, pkg, file, line): `spec` is the raw entry (e.g.
## "PredictiveEcology/SpaDES.core@branch (>= 3.0.4)"), `pkg` the bare package
## name via Require::extractPkgName(). Empty data.frame if none found.
.cc_reqdPkgsFromSource <- function(doc, file = NA_character_) {
  empty <- data.frame(spec = character(), pkg = character(),
                      file = character(), line = integer(),
                      stringsAsFactors = FALSE)
  valExpr <- xml2::xml_find_first(
    doc, "//SYMBOL_SUB[text()='reqdPkgs']/following-sibling::expr[1]")
  if (length(valExpr) == 0) return(empty)
  strs <- xml2::xml_find_all(valExpr, ".//STR_CONST")
  if (length(strs) == 0) return(empty)
  spec <- gsub('^["\']|["\']$', "", xml2::xml_text(strs))
  pkg <- tryCatch(Require::extractPkgName(spec), error = function(e) spec)
  data.frame(spec = spec, pkg = pkg, file = file %||% NA_character_,
             line = as.integer(xml2::xml_attr(strs, "line1")),
             stringsAsFactors = FALSE)
}

## Same as .cc_reqdPkgsFromSource() but from an already-parsed module
## dependency object (simInit path). `dep@reqdPkgs` is an unevaluated list of
## strings; line numbers aren't available, so `line` is NA.
.cc_reqdPkgsFromDep <- function(dep, file = NA_character_) {
  empty <- data.frame(spec = character(), pkg = character(),
                      file = character(), line = integer(),
                      stringsAsFactors = FALSE)
  spec <- tryCatch(unlist(eval(dep@reqdPkgs)), error = function(e) NULL)
  spec <- spec[nzchar(spec)]
  if (length(spec) == 0) return(empty)
  pkg <- tryCatch(Require::extractPkgName(spec), error = function(e) spec)
  data.frame(spec = spec, pkg = pkg, file = file %||% NA_character_,
             line = NA_integer_, stringsAsFactors = FALSE)
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
