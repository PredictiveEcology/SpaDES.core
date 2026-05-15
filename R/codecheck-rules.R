## Code-checking rules v2.
##
## Each rule is a function (uses, meta) -> data.frame of Findings, where:
##   uses : data.frame from .cc_collectModule()
##   meta : list(module = chr,
##               inputs  = chr,         # objectName from inputObjects
##               outputs = chr,         # objectName from outputObjects
##               params  = chr,         # paramName from defineParameter rows
##               otherModuleParams = named list(module -> chr param names),
##               moduleEnv = environment or NULL  (used for codetools rule),
##               files = chr)
##
## Rules are dispatched by .cc_runRules(); each is enabled unless disabled via
## options(spades.moduleCodeChecks = list(disable = c("rule_id", ...))).

## Rule catalogue ------------------------------------------------------------

.CC_RULES <- list(
  out_declared_unused      = function(uses, meta) .ccr_out_declared_unused(uses, meta),
  out_used_undeclared      = function(uses, meta) .ccr_out_used_undeclared(uses, meta),
  in_declared_unused       = function(uses, meta) .ccr_in_declared_unused(uses, meta),
  in_used_undeclared       = function(uses, meta) .ccr_in_used_undeclared(uses, meta),
  in_no_default            = function(uses, meta) .ccr_in_no_default(uses, meta),
  param_declared_unused    = function(uses, meta) .ccr_param_declared_unused(uses, meta),
  param_used_undeclared    = function(uses, meta) .ccr_param_used_undeclared(uses, meta),
  param_used_other_module  = function(uses, meta) .ccr_param_used_other_module(uses, meta),
  unresolved_accessor      = function(uses, meta) .ccr_unresolved_accessor(uses, meta),
  must_return_sim          = function(uses, meta) .ccr_must_return_sim(uses, meta),
  must_assign_to_sim       = function(uses, meta) .ccr_must_assign_to_sim(uses, meta),
  module_named_object      = function(uses, meta) .ccr_module_named_object(uses, meta),
  conflicting_fn_unqualified = function(uses, meta) .ccr_conflicting_fn(uses, meta),
  clashing_module_fn       = function(uses, meta) .ccr_clashing_fn(uses, meta),
  codetools                = function(uses, meta) .ccr_codetools(uses, meta)
)

## Public entry: returns a Findings data.frame
.cc_runRules <- function(uses, meta, enable = NULL, disable = NULL) {
  ids <- names(.CC_RULES)
  if (!is.null(enable)) ids <- intersect(ids, enable)
  if (!is.null(disable)) ids <- setdiff(ids, disable)
  out <- lapply(ids, function(id) {
    fn <- .CC_RULES[[id]]
    tryCatch(fn(uses, meta),
             error = function(e) {
               .cc_finding(id = id, severity = "info", module = meta$module,
                           message = paste0("rule errored: ", conditionMessage(e)))
             })
  })
  out <- out[lengths(out) > 0]
  if (length(out) == 0) return(.cc_emptyFindings())
  do.call(rbind, out)
}

## Helpers -------------------------------------------------------------------

## Names of sim objects/params that are SpaDES-internal and should never be
## flagged. Mirrors v1's ignoreObjectsGet / ignoreObjectsAssign and the
## dotted-param convention.
.CC_IGNORE_OBJECT_NAMES <- c(".userSuppliedObjNames", ".mods", ".modObjs",
                             ".parsedFiles", ".envir")

## Param names that SpaDES core machinery reads -- the user does not have to
## reference these in module code for the param to "count" as used.
.cc_isInternalParam <- function(name) {
  ## anything starting with "." is by convention reserved for SpaDES internals
  startsWith(name, ".")
}

## Restrict uses to those that occurred inside a top-level module function
## (excludes metadata block).
.cc_inFn <- function(uses) uses[!is.na(uses$fn), , drop = FALSE]

## Restrict to .inputObjects function only.
.cc_inDotInputObjects <- function(uses) {
  uses[!is.na(uses$fn) & uses$fn == ".inputObjects", , drop = FALSE]
}

.cc_outsideDotInputObjects <- function(uses) {
  uses[!is.na(uses$fn) & uses$fn != ".inputObjects", , drop = FALSE]
}

## Build a generic finding for a "declared but unused" object (no source pos).
.cc_declaredUnused <- function(id, severity, module, name, kind) {
  message <- switch(
    id,
    in_no_default = sprintf("input '%s' has no fallback default in .inputObjects(); the simulation will fail unless it is supplied via simInit() or another module",
                            name),
    sprintf("'%s' is declared in metadata %s but is not %s in module code",
            name, kind,
            if (grepl("unused$", id)) "used" else "assigned")
  )
  .cc_finding(id = id, severity = severity, module = module,
              where = "<metadata only>", name = name,
              message = message,
              suggestion = switch(
                id,
                out_declared_unused   = sprintf("either remove '%s' from outputObjects, or add `sim$%s <- ...` in an event function",
                                                name, name),
                in_declared_unused    = sprintf("either remove '%s' from inputObjects, or add `<- sim$%s` in an event function",
                                                name, name),
                param_declared_unused = sprintf("either remove `defineParameter('%s', ...)` or add `Par$%s` (or P(sim)$%s) in module code",
                                                name, name, name),
                in_no_default         = sprintf("if a default is appropriate, add `if (!suppliedElsewhere('%s', sim)) sim$%s <- <default>` to .inputObjects(); otherwise ignore",
                                                name, name),
                NA_character_
              ))
}

## Build a finding tied to a Use row (so it has line/col).
.cc_findingFromUse <- function(id, severity, module, useRow, message,
                               suggestion = NA_character_) {
  .cc_finding(id = id, severity = severity, module = module,
              where = useRow$fn, name = useRow$name,
              fn = useRow$fn, file = useRow$file,
              line = useRow$line, col = useRow$col,
              message = message, suggestion = suggestion)
}

## Rules ---------------------------------------------------------------------

.ccr_out_declared_unused <- function(uses, meta) {
  if (length(meta$outputs) == 0) return(.cc_emptyFindings())
  outsideInit <- .cc_outsideDotInputObjects(uses)
  assigns <- outsideInit[outsideInit$kind == "sim_assign" & !is.na(outsideInit$name), , drop = FALSE]
  missing <- setdiff(meta$outputs, assigns$name)
  if (length(missing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(missing, function(n)
    .cc_declaredUnused("out_declared_unused", "warning", meta$module, n, "outputObjects")))
}

.ccr_out_used_undeclared <- function(uses, meta) {
  outsideInit <- .cc_outsideDotInputObjects(uses)
  assigns <- outsideInit[outsideInit$kind == "sim_assign" & !is.na(outsideInit$name), , drop = FALSE]
  bad <- assigns[!(assigns$name %in% meta$outputs) &
                   !(assigns$name %in% .CC_IGNORE_OBJECT_NAMES), , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    .cc_findingFromUse("out_used_undeclared", "warning", meta$module, u,
                       message = sprintf("`sim$%s <- ...` in %s but '%s' is not in outputObjects",
                                         u$name, u$fn, u$name),
                       suggestion = sprintf("add `createsOutput('%s', '<class>', desc = '...')` to outputObjects",
                                            u$name))
  }))
}

.ccr_in_declared_unused <- function(uses, meta) {
  if (length(meta$inputs) == 0) return(.cc_emptyFindings())
  outsideInit <- .cc_outsideDotInputObjects(uses)
  reads <- outsideInit[outsideInit$kind == "sim_get" & !is.na(outsideInit$name), , drop = FALSE]
  ## also count .inputObjects assigns to see if it's at least filled in
  initAssigns <- .cc_inDotInputObjects(uses)
  initAssigns <- initAssigns[initAssigns$kind == "sim_assign" & !is.na(initAssigns$name), , drop = FALSE]
  used <- unique(c(reads$name, initAssigns$name))
  missing <- setdiff(meta$inputs, used)
  if (length(missing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(missing, function(n)
    .cc_declaredUnused("in_declared_unused", "warning", meta$module, n, "inputObjects")))
}

.ccr_in_used_undeclared <- function(uses, meta) {
  outsideInit <- .cc_outsideDotInputObjects(uses)
  reads <- outsideInit[outsideInit$kind == "sim_get" & !is.na(outsideInit$name), , drop = FALSE]
  declared <- c(meta$inputs, meta$outputs)
  bad <- reads[!(reads$name %in% declared) &
                 !(reads$name %in% .CC_IGNORE_OBJECT_NAMES), , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    .cc_findingFromUse("in_used_undeclared", "warning", meta$module, u,
                       message = sprintf("`sim$%s` read in %s but '%s' is not in inputObjects nor outputObjects",
                                         u$name, u$fn, u$name),
                       suggestion = sprintf("add `expectsInput('%s', '<class>', desc = '...')` to inputObjects",
                                            u$name))
  }))
}

.ccr_in_no_default <- function(uses, meta) {
  if (length(meta$inputs) == 0) return(.cc_emptyFindings())
  initAssigns <- .cc_inDotInputObjects(uses)
  initAssigns <- initAssigns[initAssigns$kind == "sim_assign" & !is.na(initAssigns$name), , drop = FALSE]
  missing <- setdiff(meta$inputs, initAssigns$name)
  if (length(missing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(missing, function(n)
    .cc_declaredUnused("in_no_default", "note", meta$module, n, "inputObjects")))
}

.ccr_param_declared_unused <- function(uses, meta) {
  if (length(meta$params) == 0) return(.cc_emptyFindings())
  ## a param is "used" if there's a param Use with module = current module
  pUses <- uses[uses$kind == "param" &
                  !is.na(uses$name) &
                  (is.na(uses$module) | uses$module == meta$module), , drop = FALSE]
  used <- unique(pUses$name)
  missing <- setdiff(meta$params, used)
  ## drop dotted params -- SpaDES-internal (.plots/.seed/.plotInitialTime/...)
  ## that core machinery reads, not the module body
  missing <- missing[!startsWith(missing, ".")]
  if (length(missing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(missing, function(n)
    .cc_declaredUnused("param_declared_unused", "warning", meta$module, n, "parameters")))
}

.ccr_param_used_undeclared <- function(uses, meta) {
  pUses <- uses[uses$kind == "param" &
                  !is.na(uses$name) &
                  (is.na(uses$module) | uses$module == meta$module), , drop = FALSE]
  if (nrow(pUses) == 0) return(.cc_emptyFindings())
  ## drop dotted names (.plots, .useCloud, .studyAreaName, ...) -- SpaDES
  ## core machinery defines these implicitly; modules legitimately reference
  ## them without declaring in defineParameter()
  pUses <- pUses[!.cc_isInternalParam(pUses$name), , drop = FALSE]
  bad <- pUses[!(pUses$name %in% meta$params), , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    .cc_findingFromUse("param_used_undeclared", "warning", meta$module, u,
                       message = sprintf("parameter '%s' used (via %s) but not declared in defineParameter()",
                                         u$name, u$extra),
                       suggestion = sprintf("add `defineParameter('%s', '<class>', <default>, NA, NA, '<desc>')` to parameters",
                                            u$name))
  }))
}

.ccr_param_used_other_module <- function(uses, meta) {
  pUses <- uses[uses$kind == "param" &
                  !is.na(uses$name) &
                  !is.na(uses$module) & uses$module != meta$module, , drop = FALSE]
  if (nrow(pUses) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(pUses)), function(i) {
    u <- pUses[i, ]
    declaredInOther <- !is.null(meta$otherModuleParams[[u$module]]) &&
      u$name %in% meta$otherModuleParams[[u$module]]
    if (declaredInOther) return(NULL)
    .cc_findingFromUse("param_used_other_module", "note", meta$module, u,
                       message = sprintf("parameter '%s' looked up in module '%s' (not a sibling we can verify)",
                                         u$name, u$module),
                       suggestion = "ensure that module is loaded in the same simInit, or qualify the call differently")
  })) -> res
  if (is.null(res)) .cc_emptyFindings() else res
}

.ccr_unresolved_accessor <- function(uses, meta) {
  bad <- uses[!uses$resolved & !is.na(uses$fn), , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  ## Aggregate: one finding per (fn, kind) showing all line numbers, so the
  ## report doesn't spew one row per occurrence. Tests can still see the raw
  ## Uses on .codeCheck if they want.
  by <- split(bad, list(bad$fn, bad$kind), drop = TRUE)
  do.call(rbind, lapply(by, function(g) {
    lines <- paste(g$line, collapse = ", ")
    u <- g[1, ]
    .cc_findingFromUse("unresolved_accessor", "info", meta$module, u,
                       message = sprintf("%d unresolved %s accessor(s) in %s (lines %s) \u2014 skipped",
                                         nrow(g), u$kind, u$fn, lines),
                       suggestion = "if these objects should be checked, declare them explicitly in inputObjects/outputObjects")
  }))
}

.ccr_must_return_sim <- function(uses, meta) {
  ## fns whose names match doEvent.* must return sim. We have a return_sim Use
  ## per fn whose last expression is sim. If a doEvent.* fn has none, it's bad.
  if (is.null(meta$moduleEnv)) return(.cc_emptyFindings())
  fnNames <- ls(meta$moduleEnv)
  fnNames <- fnNames[grepl("^doEvent\\.", fnNames)]
  if (length(fnNames) == 0) return(.cc_emptyFindings())
  ok <- uses$fn[uses$kind == "return_sim"]
  bad <- setdiff(fnNames, ok)
  if (length(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(bad, function(n)
    .cc_finding("must_return_sim", "error", meta$module, where = n, name = n, fn = n,
                message = sprintf("function '%s' must end with `sim` / `return(sim)` / `return(invisible(sim))`",
                                  n),
                suggestion = "add `return(invisible(sim))` as the last statement")))
}

.ccr_must_assign_to_sim <- function(uses, meta) {
  bad <- uses[uses$kind == "assign_to_sim", , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    .cc_findingFromUse("must_assign_to_sim", "error", meta$module, u,
                       message = sprintf("call to `%s()` in %s must be assigned to sim, e.g., `sim <- %s(sim, ...)`",
                                         u$name, u$fn, u$name),
                       suggestion = sprintf("change `%s(...)` to `sim <- %s(...)`", u$name, u$name))
  }))
}

.ccr_module_named_object <- function(uses, meta) {
  bad <- uses[uses$kind == "sim_assign" & !is.na(uses$name) & uses$name == meta$module,
              , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    .cc_findingFromUse("module_named_object", "error", meta$module, u,
                       message = sprintf("`sim$%s <- ...` collides with the module name; not allowed",
                                         u$name),
                       suggestion = "rename the object")
  }))
}

.ccr_conflicting_fn <- function(uses, meta) {
  bad <- uses[uses$kind == "global" & !is.na(uses$extra) & uses$extra == "conflict",
              , drop = FALSE]
  if (nrow(bad) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    qual <- switch(u$name,
                   levels    = "raster::levels",
                   scale     = "raster::scale",
                   which.max = "raster::which.max",
                   u$name)
    .cc_findingFromUse("conflicting_fn_unqualified", "warning", meta$module, u,
                       message = sprintf("`%s()` is ambiguous (collides with raster:: namesake)",
                                         u$name),
                       suggestion = sprintf("use the qualified form, e.g., `%s(...)` for the raster variant",
                                            qual))
  }))
}

.ccr_clashing_fn <- function(uses, meta) {
  if (is.null(meta$moduleEnv)) return(.cc_emptyFindings())
  fns <- ls(meta$moduleEnv)
  clashing <- intersect(fns, c("Plot"))
  if (length(clashing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(clashing, function(n)
    .cc_finding("clashing_module_fn", "warning", meta$module, where = n, name = n,
                message = sprintf("module defines `%s`, which clashes with quickPlot::Plot",
                                  n),
                suggestion = "rename the module function to avoid the clash")))
}

.ccr_codetools <- function(uses, meta) {
  if (is.null(meta$moduleEnv)) return(.cc_emptyFindings())
  if (!requireNamespace("codetools", quietly = TRUE)) return(.cc_emptyFindings())
  opts <- meta$codetoolsOpts %||% list(
    skipWith = TRUE, suppressNoLocalFun = TRUE,
    suppressParamUnused = FALSE, suppressPartialMatchArgs = FALSE,
    suppressUndefined = TRUE
  )
  msgs <- tryCatch(
    utils::capture.output(do.call(codetools::checkUsageEnv,
                                  c(list(env = meta$moduleEnv), opts))),
    error = function(e) character()
  )
  ## drop the noisy doEvent parameter complaints (matches v1 behavior)
  msgs <- grep("doEvent.*: parameter", msgs, value = TRUE, invert = TRUE)
  msgs <- msgs[nzchar(msgs)]
  if (length(msgs) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(msgs, function(m) {
    .cc_finding("codetools", "note", meta$module,
                message = m, suggestion = NA_character_)
  }))
}
