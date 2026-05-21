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
## options(spades.moduleCodeChecks = list(disable = c("rule_id", ...))). Per
## finding, output is further filtered by .cc_applySuppression(): inline
## `# nolint` markers in the module source, and
## options(spades.codeChecksIgnore = list(<rule_id> = c("obj", ...))).

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
  codetools                = function(uses, meta) .ccr_codetools(uses, meta),
  reqd_pkg_duplicate       = function(uses, meta) .ccr_reqd_pkg_duplicate(uses, meta),
  reqd_pkg_undeclared      = function(uses, meta) .ccr_reqd_pkg_undeclared(uses, meta),
  reqd_pkg_no_source       = function(uses, meta) .ccr_reqd_pkg_no_source(uses, meta)
)

## Base-priority packages that are always available and never declared in
## reqdPkgs.
.CC_BASE_PKGS <- c("base", "compiler", "datasets", "graphics", "grDevices",
                   "grid", "methods", "parallel", "splines", "stats", "stats4",
                   "tcltk", "tools", "utils")

## Display bucket each rule id is reported under (the `• <group>` headers).
## Shared by the reporter and by `# nolint` / codeChecksIgnore suppression, so
## either the rule id or its group name can be used to silence a finding.
.CC_RULE_GROUPS <- c(
  out_declared_unused        = "outputObjects",
  out_used_undeclared        = "outputObjects",
  in_declared_unused         = "inputObjects",
  in_used_undeclared         = "inputObjects",
  in_no_default              = "inputObjects",
  param_declared_unused      = "parameters",
  param_used_undeclared      = "parameters",
  param_used_other_module    = "parameters",
  unresolved_accessor        = "unresolved",
  must_return_sim            = "module functions",
  must_assign_to_sim         = "module functions",
  module_named_object        = "module functions",
  conflicting_fn_unqualified = "globals",
  clashing_module_fn         = "module functions",
  codetools                  = "codetools",
  reqd_pkg_duplicate         = "reqdPkgs",
  reqd_pkg_undeclared        = "reqdPkgs",
  reqd_pkg_no_source         = "reqdPkgs"
)

## Public entry: returns a Findings data.frame
.cc_runRules <- function(uses, meta, enable = NULL, disable = NULL) {
  ids <- names(.CC_RULES)
  ## Honour rule enable/disable supplied via the option, in addition to the
  ## function arguments: options(spades.moduleCodeChecks = list(disable = ...)).
  opt <- getOption("spades.moduleCodeChecks")
  if (is.list(opt)) {
    enable  <- c(enable,  opt[["enable"]])
    disable <- c(disable, opt[["disable"]])
  }
  if (length(enable))  ids <- intersect(ids, enable)
  if (length(disable)) ids <- setdiff(ids, disable)
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
  findings <- do.call(rbind, out)
  findings <- .cc_appendNolintHint(findings)
  .cc_applySuppression(findings, uses)
}

## Every suggestion ends by telling the developer how to acknowledge the
## finding: `otherwise add # nolint: <rule_id>`. Replaces the vaguer
## "otherwise ignore" and is appended to suggestions that don't already mention
## `nolint`, using each finding's own rule id.
.cc_appendNolintHint <- function(findings) {
  if (nrow(findings) == 0) return(findings)
  findings$suggestion <- vapply(seq_len(nrow(findings)), function(i) {
    s <- findings$suggestion[i]
    if (is.na(s)) return(NA_character_)
    if (grepl("nolint", s, fixed = TRUE)) return(s)   # already mentions it
    s <- sub("[[:space:];,]*otherwise ignore\\.?[[:space:]]*$", "", s)
    paste0(s, "; otherwise add `# nolint: ", findings$id[i], "`")
  }, character(1))
  findings
}

## Drop findings silenced either by an inline `# nolint` marker (carried on
## `uses` as the "nolint"/"declLines" attributes) or by the user option
## options(spades.codeChecksIgnore = list(<rule_id> = c("obj1", "obj2"))).
.cc_applySuppression <- function(findings, uses) {
  if (nrow(findings) == 0) return(findings)
  nolint    <- attr(uses, "nolint")
  declLines <- attr(uses, "declLines")
  ignore    <- getOption("spades.codeChecksIgnore")

  ## candidate (file, line) locations a `# nolint` could sit on to silence
  ## finding `i`: its own source line, or — for metadata-only findings with no
  ## line — every line of the matching declaration's span.
  candLines <- function(i) {
    if (!is.na(findings$line[i])) {
      return(data.frame(file = findings$file[i], line = findings$line[i],
                        stringsAsFactors = FALSE))
    }
    if (is.null(declLines) || is.na(findings$name[i])) return(NULL)
    dl <- declLines[declLines$name == findings$name[i], , drop = FALSE]
    if (nrow(dl) == 0) return(NULL)
    do.call(rbind, Map(function(f, a, b)
      data.frame(file = f, line = seq.int(a, b), stringsAsFactors = FALSE),
      dl$file, dl$line1, dl$line2))
  }

  keep <- vapply(seq_len(nrow(findings)), function(i) {
    fid <- findings$id[i]; fname <- findings$name[i]
    ## a finding can be referenced by its rule id or by its group name
    fkeys <- c(fid, .CC_RULE_GROUPS[[fid]])
    ## user option: ignore named objects for a given rule (or group)
    if (is.list(ignore) && !is.na(fname) &&
        any(vapply(fkeys, function(k) fname %in% ignore[[k]], logical(1)))) {
      return(FALSE)
    }
    ## inline `# nolint`
    if (!is.null(nolint) && nrow(nolint)) {
      cand <- candLines(i)
      if (!is.null(cand) && nrow(cand)) {
        for (j in seq_len(nrow(nolint))) {
          sameFile <- (is.na(nolint$file[j]) & is.na(cand$file)) |
            (!is.na(nolint$file[j]) & nolint$file[j] == cand$file)
          if (any(sameFile & nolint$line[j] == cand$line)) {
            r <- nolint$rules[[j]]
            if (all(is.na(r)) || any(fkeys %in% r)) return(FALSE)
          }
        }
      }
    }
    TRUE
  }, logical(1))
  findings[keep, , drop = FALSE]
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

## Everything that is not provably inside .inputObjects(). A use whose
## enclosing function could not be identified (fn = NA, e.g. a function wrapped
## in compiler::cmpfun()/Cache()) is treated as outside, so an unrecognised
## wrapper never produces a false "declared but unused" finding.
.cc_outsideDotInputObjects <- function(uses) {
  uses[is.na(uses$fn) | uses$fn != ".inputObjects", , drop = FALSE]
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
                in_no_default         = sprintf("if a default is appropriate, add `if (!suppliedElsewhere('%s', sim)) sim$%s <- <default>` to .inputObjects()",
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
  assignedNames <- assigns$name
  ## A bulk write into envir(sim) -- e.g. list2env(mget(outputNames,
  ## environment()), envir(sim)) -- assigns outputs by run-time name. In that
  ## case treat an output that is computed as a same-named local variable as
  ## produced. (mget() would error at run time if such a local were missing, so
  ## this is reliable; outputs with no local assignment are still flagged.)
  if (any(uses$kind == "sim_bulk_assign")) {
    assignedNames <- c(assignedNames,
                       uses$name[uses$kind == "local_assign" & !is.na(uses$name)])
  }
  ## developer assertions via `# nolint: vars a, b` (e.g. on a list2env line
  ## whose list element names can't be seen statically) -- treat as produced
  assignedNames <- c(assignedNames,
                     uses$name[uses$kind == "declared_var" & !is.na(uses$name)])
  missing <- setdiff(meta$outputs, assignedNames)
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
  ## `# nolint: vars a, b` asserts a, b are assigned (e.g. a dynamic
  ## sim[[namPlural]] <- ... in .inputObjects whose name can't be seen
  ## statically); treat them as having a default.
  declaredVars <- uses$name[uses$kind == "declared_var" & !is.na(uses$name)]
  ## an input guarded by `suppliedElsewhere("x", sim)` (then assigned a default
  ## OR stop()ped if absent) is intentionally handled -- not a missing default.
  suppliedElsewhere <- uses$name[uses$kind == "supplied_elsewhere" & !is.na(uses$name)]
  missing <- setdiff(meta$inputs, c(initAssigns$name, declaredVars, suppliedElsewhere))
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
  ## paramCheckOtherMods(sim, "x") deliberately reads parameters owned by other
  ## modules, so such uses must not be reported as "used but not declared here"
  pUses <- pUses[is.na(pUses$extra) | pUses$extra != "paramCheckOtherMods()", , drop = FALSE]
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
  ## Classify the kind of dynamic access so the message can be specific. The
  ## get-family (get/mget/exists/assign with a computed name) and `sim[[var]]`
  ## are inherently un-checkable statically -- the developer must decide whether
  ## the access is intentional (add `# nolint: unresolved_accessor`) or a bug.
  getFamily <- c("get()", "mget()", "exists()", "assign()")
  access <- ifelse(bad$extra %in% getFamily, "getfam",
                   ifelse(bad$kind %in% c("sim_get", "sim_assign"),
                          "bracket", "other"))
  nolintHint <- paste0("cannot be checked statically (the object name is ",
                       "computed at run time); if intentional, add ",
                       "`# nolint: unresolved_accessor` on the line(s), ",
                       "otherwise use a literal name (`sim$x`) or declare ",
                       "the object in inputObjects/outputObjects")
  ## One finding per occurrence with a generic, location-free message, so the
  ## report collapses same-kind accesses (across functions) into a single info
  ## with one line per location.
  do.call(rbind, lapply(seq_len(nrow(bad)), function(i) {
    u <- bad[i, ]
    msgSug <- switch(
      access[i],
      getfam  = list(msg = "dynamic `get()`/`mget()`-family access of `sim`",
                     sug = nolintHint),
      bracket = list(msg = "dynamic `sim[[<var>]]` access",
                     sug = nolintHint),
      list(msg = sprintf("unresolved %s accessor", u$kind),
           sug = "if these objects should be checked, declare them explicitly in inputObjects/outputObjects"))
    .cc_findingFromUse("unresolved_accessor", "info", meta$module, u,
                       message = msgSug$msg, suggestion = msgSug$sug)
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

## reqdPkgs rules ------------------------------------------------------------

## A package declared more than once in reqdPkgs. Different source/version
## specs for the same package (e.g. CRAN `SpaDES.core (>= 3.0.1)` plus
## `PredictiveEcology/SpaDES.core@branch (>= 3.0.4)`) are a real conflict
## (warning); exact repeats are a note.
.ccr_reqd_pkg_duplicate <- function(uses, meta) {
  rp <- meta$reqdPkgs
  if (is.null(rp) || NROW(rp) == 0) return(.cc_emptyFindings())
  dups <- unique(rp$pkg[duplicated(rp$pkg)])
  if (length(dups) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(dups, function(p) {
    entries <- rp[rp$pkg == p, , drop = FALSE]
    specs <- unique(entries$spec)
    conflict <- length(specs) > 1
    .cc_finding(
      "reqd_pkg_duplicate", if (conflict) "warning" else "note", meta$module,
      where = "reqdPkgs", name = p,
      file = entries$file[1], line = entries$line[1],
      message = sprintf("package '%s' is declared %d times in reqdPkgs%s: %s",
                        p, nrow(entries),
                        if (conflict) " with differing source/version" else "",
                        paste(specs, collapse = " | ")),
      suggestion = "keep a single declaration (the most specific source/version) and remove the rest")
  }))
}

## A package referenced via `pkg::fn` but not present in reqdPkgs (and not a
## base package). Likely a missing declaration.
.ccr_reqd_pkg_undeclared <- function(uses, meta) {
  ns <- uses[uses$kind == "ns_call" & !is.na(uses$name), , drop = FALSE]
  if (nrow(ns) == 0) return(.cc_emptyFindings())
  declared <- if (is.null(meta$reqdPkgs)) character() else unique(meta$reqdPkgs$pkg)
  missing <- setdiff(unique(ns$name), c(declared, .CC_BASE_PKGS))
  if (length(missing) == 0) return(.cc_emptyFindings())
  do.call(rbind, lapply(missing, function(p) {
    fns <- unique(stats::na.omit(ns$extra[ns$name == p]))
    eg <- paste0(p, "::", utils::head(fns, 3), "()", collapse = ", ")
    u <- ns[ns$name == p, , drop = FALSE][1, ]
    .cc_findingFromUse(
      "reqd_pkg_undeclared", "warning", meta$module, u,
      message = sprintf("package '%s' is used via `::` (e.g. %s) but not declared in reqdPkgs",
                        p, eg),
      suggestion = sprintf("add '%s' to reqdPkgs", p))
  }))
}

## Quiet, best-effort: bare function calls that have no apparent source among
## the declared reqdPkgs (their installed namespace exports), base packages,
## SpaDES.core, or functions defined locally in the module. Only runs when
## every declared (non-base) package is installed -- otherwise we can't tell
## where bare calls come from, so we stay silent. Info-level.
.ccr_reqd_pkg_no_source <- function(uses, meta) {
  bare <- unique(uses$name[uses$kind == "bare_call" & !is.na(uses$name)])
  if (length(bare) == 0) return(.cc_emptyFindings())
  declared <- if (is.null(meta$reqdPkgs)) character() else unique(meta$reqdPkgs$pkg)
  declared <- setdiff(declared, .CC_BASE_PKGS)
  if (length(declared) &&
      !all(vapply(declared, requireNamespace, logical(1), quietly = TRUE))) {
    return(.cc_emptyFindings())   # incomplete export info -> stay quiet
  }
  ## exclude tcltk -- loading its namespace warns when there is no DISPLAY
  exportPkgs <- setdiff(unique(c("SpaDES.core", .CC_BASE_PKGS, declared)), "tcltk")
  exports <- suppressWarnings(unlist(lapply(exportPkgs, function(p)
    tryCatch(getNamespaceExports(p), error = function(e) character()))))
  ## locally-defined names (functions and other locals) and enclosing fn names
  localNames <- unique(c(uses$name[uses$kind == "local_assign"], uses$fn))
  localNames <- localNames[!is.na(localNames)]
  noSource <- setdiff(bare, c(exports, localNames))
  if (length(noSource) == 0) return(.cc_emptyFindings())
  .cc_finding(
    "reqd_pkg_no_source", "info", meta$module,
    where = "reqdPkgs", file = meta$mainFile %||% NA_character_,
    message = sprintf("within the named packages in reqdPkgs, there is no apparent source for the function(s): %s",
                      paste(sort(noSource), collapse = ", ")),
    suggestion = "declare the providing package in reqdPkgs, qualify the call as pkg::fn, or ignore if defined elsewhere")
}
