## Code-checking engine v2.
## Parses module sources via xmlparsedata, walks the XML tree to collect Use
## records, then applies rule functions that return Finding rows.
##
## All public symbols start with `.cc` to keep this private until P4 wires the
## user-facing API. v1 (R/code-checking.R) is left fully intact; this file adds
## a parallel engine selectable via getOption("spades.codeCheckEngine").

## ---------------------------------------------------------------------------
## Data structures
## ---------------------------------------------------------------------------

## A Use is a single static reference to a sim/parameter accessor in a function
## body. Stored as a row in a data.frame.
##
## kind:
##   "sim_get"        — RHS read of sim$x / sim[["x"]] / get("x", envir=envir(sim))
##   "sim_assign"     — LHS write to sim$x / sim[["x"]] / assign("x", ...)
##   "param"          — Par$x / P(sim)$x / P(sim, module="m")$x / params(sim)$m$x
##   "return_sim"     — last expression of a function body returns sim
##   "assign_to_sim"  — call to scheduleEvent/saveFiles whose result is/isn't
##                      assigned back to sim
##   "global"         — codetools-style global symbol use (function calls)
##
## name:     the object/parameter name, or NA if unresolved
## fn:       enclosing top-level function name
## file:     path of source file (relative or absolute as supplied)
## line/col: source position (line1/col1 from getParseData)
## resolved: TRUE if `name` was a literal symbol/string; FALSE if it was an
##           expression we couldn't statically resolve
## module:   for kind="param", the module the parameter is being looked up in
##           (current module if not specified)
## extra:    short text snippet (for unresolved cases or call args)

.cc_use <- function(kind, name = NA_character_, fn = NA_character_,
                    file = NA_character_, line = NA_integer_, col = NA_integer_,
                    resolved = TRUE, module = NA_character_,
                    extra = NA_character_) {
  data.frame(
    kind = kind, name = name, fn = fn, file = file,
    line = as.integer(line), col = as.integer(col),
    resolved = resolved, module = module, extra = extra,
    stringsAsFactors = FALSE
  )
}

## A Finding is the output of a rule. id is a stable string used by tests and
## by the suggestion dispatch; severity drives presentation/exit code.
.cc_finding <- function(id, severity = c("error", "warning", "note", "info"),
                        module, where = NA_character_, name = NA_character_,
                        fn = NA_character_, file = NA_character_,
                        line = NA_integer_, col = NA_integer_,
                        message = NA_character_, suggestion = NA_character_) {
  severity <- match.arg(severity)
  data.frame(
    id = id, severity = severity, module = module, where = where,
    name = name, fn = fn, file = file,
    line = as.integer(line), col = as.integer(col),
    message = message, suggestion = suggestion,
    stringsAsFactors = FALSE
  )
}

.cc_emptyFindings <- function() .cc_finding("x", "info", "x")[0, ]
.cc_emptyUses <- function() .cc_use("sim_get")[0, ]

## ---------------------------------------------------------------------------
## Parsing
## ---------------------------------------------------------------------------

## Parse one module source file (or a vector of source lines) into:
##   list(doc = xml2 document, file = file)
.cc_parseFile <- function(file = NULL, text = NULL) {
  .cc_needPkgs()
  if (is.null(text)) {
    parsed <- parse(file, keep.source = TRUE)
  } else {
    parsed <- parse(text = text, keep.source = TRUE)
  }
  xml <- xmlparsedata::xml_parse_data(parsed, includeText = TRUE)
  list(doc = xml2::read_xml(xml), file = if (is.null(file)) "<text>" else file)
}

.cc_needPkgs <- function() {
  for (pkg in c("xmlparsedata", "xml2")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required for spades.moduleCodeChecks v2. ",
           "Install it, or set options(spades.codeCheckEngine = 'v1').",
           call. = FALSE)
    }
  }
}

## ---------------------------------------------------------------------------
## XML helpers
## ---------------------------------------------------------------------------

## Extract source position from an xml node.
.cc_pos <- function(node) {
  list(
    line = as.integer(xml2::xml_attr(node, "line1") %||% NA),
    col  = as.integer(xml2::xml_attr(node, "col1")  %||% NA)
  )
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

## Find the enclosing top-level function-definition expression for a node, and
## return its bound name (the SYMBOL on the LHS of the `<-` that defines it),
## or NA if the node isn't inside a named function.
.cc_enclosingFn <- function(node) {
  ## Walk up: any expr whose immediate child is FUNCTION marks a function def.
  ancs <- xml2::xml_find_all(node,
    "ancestor::expr[FUNCTION][1]")
  if (length(ancs) == 0) return(NA_character_)
  fnExpr <- ancs[[length(ancs)]]
  ## The function definition expr is typically the RHS of a top-level
  ## `name <- function(...) {...}` assignment. The LHS SYMBOL is two siblings
  ## back: <expr><SYMBOL>name</SYMBOL></expr><LEFT_ASSIGN>...<expr>function...
  parent <- xml2::xml_parent(fnExpr)
  if (xml2::xml_name(parent) != "expr") return(NA_character_)
  lhs <- xml2::xml_find_first(parent, "expr[1]/SYMBOL")
  if (length(lhs) == 0 || is.na(xml2::xml_text(lhs))) return(NA_character_)
  xml2::xml_text(lhs)
}

## Is `node` (an expr) the LHS of an assignment? An expr is an LHS iff its
## parent is also an expr whose first expr child is `node` AND whose second
## child is one of LEFT_ASSIGN / RIGHT_ASSIGN / EQ_ASSIGN.
##
## Note: for `sim$x[1] <- v` we treat the inner `sim$x` as a get (it's read
## then [<- is applied), matching the spirit of v1's behavior.
.cc_isLhsOfAssign <- function(node) {
  parent <- xml2::xml_parent(node)
  if (xml2::xml_name(parent) != "expr") return(FALSE)
  kids <- xml2::xml_children(parent)
  ## must be the first expr child
  exprKids <- which(xml2::xml_name(kids) == "expr")
  if (length(exprKids) == 0) return(FALSE)
  if (xml2::xml_text(node, trim = FALSE) != xml2::xml_text(kids[[exprKids[1]]],
                                                           trim = FALSE)) {
    ## Use identity via xml ptr instead — text equality is unreliable
    if (!identical(xml2::xml_path(node), xml2::xml_path(kids[[exprKids[1]]]))) {
      return(FALSE)
    }
  }
  ## look for an assignment op as a sibling of the expr children
  assignOps <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  any(xml2::xml_name(kids) %in% assignOps)
}

## ---------------------------------------------------------------------------
## Collectors
## ---------------------------------------------------------------------------

## Each collector returns a data.frame of Use rows.

## sim$x  and  sim[["x"]]  (and sim[[expr]] flagged unresolved)
.cc_collect_simAccess <- function(parsed) {
  doc <- parsed$doc; file <- parsed$file
  out <- list()

  ## sim$<SYMBOL>
  dollarNodes <- xml2::xml_find_all(
    doc,
    "//expr[OP-DOLLAR and expr/SYMBOL[text()='sim'] and SYMBOL]"
  )
  for (n in dollarNodes) {
    sym <- xml2::xml_find_first(n, "SYMBOL")
    name <- xml2::xml_text(sym)
    pos <- .cc_pos(n)
    fn <- .cc_enclosingFn(n)
    kind <- if (.cc_isLhsOfAssign(n)) "sim_assign" else "sim_get"
    out[[length(out) + 1]] <- .cc_use(kind, name = name, fn = fn, file = file,
                                      line = pos$line, col = pos$col,
                                      resolved = TRUE)
  }

  ## sim[["x"]]  (string literal)  AND  sim[[<expr>]]  (unresolved)
  bbNodes <- xml2::xml_find_all(
    doc,
    "//expr[LBB and expr[1]/SYMBOL[text()='sim']]"
  )
  for (n in bbNodes) {
    ## the index is the second expr child (after sim)
    indexExpr <- xml2::xml_find_first(n, "expr[2]")
    if (length(indexExpr) == 0) next
    str <- xml2::xml_find_first(indexExpr, "STR_CONST")
    pos <- .cc_pos(n)
    fn <- .cc_enclosingFn(n)
    kind <- if (.cc_isLhsOfAssign(n)) "sim_assign" else "sim_get"
    if (length(str) > 0 && !is.na(xml2::xml_text(str))) {
      ## strip surrounding quotes that STR_CONST preserves
      raw <- xml2::xml_text(str)
      name <- gsub('^["\']|["\']$', "", raw)
      out[[length(out) + 1]] <- .cc_use(kind, name = name, fn = fn, file = file,
                                        line = pos$line, col = pos$col,
                                        resolved = TRUE)
    } else {
      ## unresolved (e.g., sim[[Par$stackName]])
      out[[length(out) + 1]] <- .cc_use(
        kind, name = NA_character_, fn = fn, file = file,
        line = pos$line, col = pos$col, resolved = FALSE,
        extra = xml2::xml_text(indexExpr)
      )
    }
  }

  ## get("x", envir = envir(sim))   /   assign("x", value, envir = envir(sim))
  ## /   exists("x", envir = envir(sim))   /   mget(c("x","y"), envir = envir(sim))
  ## (heuristic: any of these calls whose `envir` arg contains an `envir(sim)`
  ## subexpression is treated as a sim accessor)
  callNodes <- xml2::xml_find_all(
    doc,
    "//expr[expr/SYMBOL_FUNCTION_CALL[text()='get' or text()='assign' or text()='exists' or text()='mget']]"
  )
  for (n in callNodes) {
    fnName <- xml2::xml_text(
      xml2::xml_find_first(n, "expr/SYMBOL_FUNCTION_CALL")
    )
    ## find an `envir = envir(sim)` arg
    envirCalls <- xml2::xml_find_all(
      n,
      ".//expr[expr/SYMBOL_FUNCTION_CALL[text()='envir'] and expr/SYMBOL[text()='sim']]"
    )
    if (length(envirCalls) == 0) next

    ## first positional arg is the name; for assign/get it's the first STR_CONST
    nameNode <- xml2::xml_find_first(n, ".//STR_CONST[1]")
    pos <- .cc_pos(n)
    fn <- .cc_enclosingFn(n)
    kind <- switch(fnName,
                   get = "sim_get", mget = "sim_get", exists = "sim_get",
                   assign = "sim_assign")
    if (length(nameNode) > 0 && !is.na(xml2::xml_text(nameNode))) {
      raw <- xml2::xml_text(nameNode)
      name <- gsub('^["\']|["\']$', "", raw)
      out[[length(out) + 1]] <- .cc_use(kind, name = name, fn = fn, file = file,
                                        line = pos$line, col = pos$col,
                                        resolved = TRUE,
                                        extra = paste0(fnName, "()"))
    } else {
      out[[length(out) + 1]] <- .cc_use(kind, name = NA_character_, fn = fn,
                                        file = file, line = pos$line,
                                        col = pos$col, resolved = FALSE,
                                        extra = paste0(fnName, "()"))
    }
  }

  if (length(out) == 0) return(.cc_emptyUses())
  do.call(rbind, out)
}

## Par$x  /  P(sim)$x  /  P(sim, module="m")$x  /  params(sim)$m$x
.cc_collect_params <- function(parsed, currentModule = NA_character_) {
  doc <- parsed$doc; file <- parsed$file
  out <- list()

  ## Par$<SYMBOL>
  parNodes <- xml2::xml_find_all(
    doc,
    "//expr[OP-DOLLAR and expr/SYMBOL[text()='Par'] and SYMBOL]"
  )
  for (n in parNodes) {
    sym <- xml2::xml_find_first(n, "SYMBOL")
    pos <- .cc_pos(n)
    out[[length(out) + 1]] <- .cc_use(
      "param", name = xml2::xml_text(sym),
      fn = .cc_enclosingFn(n), file = file,
      line = pos$line, col = pos$col, resolved = TRUE,
      module = currentModule, extra = "Par"
    )
  }

  ## P(sim)$x  or  P(sim, module = "m")$x
  ##   structure: <expr> <expr>P(...)</expr> <OP-DOLLAR/> <SYMBOL>x</SYMBOL> </expr>
  pNodes <- xml2::xml_find_all(
    doc,
    "//expr[OP-DOLLAR and SYMBOL and expr/expr/SYMBOL_FUNCTION_CALL[text()='P']]"
  )
  for (n in pNodes) {
    sym <- xml2::xml_find_first(n, "SYMBOL")
    callExpr <- xml2::xml_find_first(n, "expr")
    mod <- .cc_extractModuleArg(callExpr) %||% currentModule
    pos <- .cc_pos(n)
    out[[length(out) + 1]] <- .cc_use(
      "param", name = xml2::xml_text(sym),
      fn = .cc_enclosingFn(n), file = file,
      line = pos$line, col = pos$col, resolved = TRUE,
      module = mod, extra = "P()"
    )
  }

  ## params(sim)$mod$x   AND   params(sim)[["mod"]][["x"]]
  ##
  ## We look for any `expr` that has OP-DOLLAR (or LBB) and whose first child
  ## expr (recursively) eventually reaches a SYMBOL_FUNCTION_CALL=params.
  ## For a $-chain `params(sim)$mod$x`, both intermediate and outer match;
  ## we keep only the OUTER (the one whose RHS symbol is the parameter name,
  ## not the module name).
  paramsRoots <- xml2::xml_find_all(
    doc,
    "//expr[(OP-DOLLAR or LBB) and .//expr/SYMBOL_FUNCTION_CALL[text()='params'] and not(ancestor::expr[(OP-DOLLAR or LBB) and .//expr/SYMBOL_FUNCTION_CALL[text()='params']])]"
  )
  for (root in paramsRoots) {
    parsed2 <- .cc_parseParamsChain(root)
    if (is.null(parsed2)) next
    pos <- .cc_pos(root)
    out[[length(out) + 1]] <- .cc_use(
      "param", name = parsed2$param, fn = .cc_enclosingFn(root),
      file = file, line = pos$line, col = pos$col,
      resolved = parsed2$resolved,
      module = parsed2$module %||% currentModule,
      extra = "params()"
    )
  }

  if (length(out) == 0) return(.cc_emptyUses())
  do.call(rbind, out)
}

## Walk a `params(sim)$<mod>$<param>` (or `[["mod"]][["param"]]`) chain.
## Returns list(module, param, resolved) or NULL if the chain isn't shaped
## the way we expect.
.cc_parseParamsChain <- function(root) {
  ## Outer: <expr><expr>...inner...</expr><OP-DOLLAR or LBB>...<SYMBOL or STR_CONST>x
  outerName <- .cc_chainTail(root)
  if (is.null(outerName)) return(NULL)
  inner <- xml2::xml_find_first(root, "expr[1]")
  if (length(inner) == 0) return(NULL)
  ## If inner is itself a $/[[ off params(sim), tail of inner is the module.
  innerHasAccessor <- length(xml2::xml_find_all(
    inner, "OP-DOLLAR | LBB")) > 0
  if (innerHasAccessor) {
    modName <- .cc_chainTail(inner)
    list(module = modName$name,
         param = outerName$name,
         resolved = outerName$resolved && (is.null(modName) || modName$resolved))
  } else {
    ## `params(sim)$x` — no module given; param goes against current module
    list(module = NULL, param = outerName$name, resolved = outerName$resolved)
  }
}

## Pull the trailing `$name` or `[["name"]]` off a chain expr. Returns
## list(name, resolved) or NULL.
.cc_chainTail <- function(expr) {
  kids <- xml2::xml_children(expr)
  if (length(kids) < 3) return(NULL)
  op <- kids[[2]]
  opName <- xml2::xml_name(op)
  if (opName == "OP-DOLLAR") {
    sym <- xml2::xml_find_first(expr, "SYMBOL")
    if (length(sym) == 0) return(NULL)
    return(list(name = xml2::xml_text(sym), resolved = TRUE))
  } else if (opName == "LBB") {
    str <- xml2::xml_find_first(expr, "expr[2]/STR_CONST")
    if (length(str) > 0 && !is.na(xml2::xml_text(str))) {
      return(list(name = gsub('^["\']|["\']$', "", xml2::xml_text(str)),
                  resolved = TRUE))
    }
    return(list(name = NA_character_, resolved = FALSE))
  }
  NULL
}

## Pull `module = "..."` from a P(sim, module = "m") call expr; returns NULL
## if the arg is missing or unresolved.
.cc_extractModuleArg <- function(callExpr) {
  ## Find any EQ_SUB child whose preceding SYMBOL_SUB has text "module"
  args <- xml2::xml_find_all(
    callExpr,
    "SYMBOL_SUB[text()='module']/following-sibling::*[1]/self::EQ_SUB/following-sibling::expr[1]"
  )
  if (length(args) == 0) {
    ## fall back: positional arg #2 — we don't try to map positionally
    return(NULL)
  }
  argExpr <- args[[1]]
  str <- xml2::xml_find_first(argExpr, "STR_CONST")
  if (length(str) > 0 && !is.na(xml2::xml_text(str))) {
    return(gsub('^["\']|["\']$', "", xml2::xml_text(str)))
  }
  NULL
}

## return(sim)  /  return(invisible(sim))  /  trailing `sim`
##
## Emits one Use per top-level function whose last body expression is sim or
## return(sim) / return(invisible(sim)). Other functions get no Use; the rule
## then complains for those whose names match `mustBeReturnSim`.
.cc_collect_returnSim <- function(parsed) {
  doc <- parsed$doc; file <- parsed$file
  out <- list()
  ## All function definitions assigned to a name at top-level scope
  ##   <expr>                              <- assignment expr (parent)
  ##     <expr><SYMBOL>NAME</SYMBOL></expr>
  ##     <LEFT_ASSIGN/>
  ##     <expr>                            <- function-def expr (this)
  ##       <FUNCTION/>
  ##       ...formals...
  ##       <expr>                          <- body (a `{...}` block or single expr)
  ##         (<expr>...</expr>)*
  ##       </expr>
  ##     </expr>
  ##   </expr>
  fnDefs <- xml2::xml_find_all(doc, "//expr[FUNCTION]")
  for (fnExpr in fnDefs) {
    parent <- xml2::xml_parent(fnExpr)
    if (xml2::xml_name(parent) != "expr") next
    nameNode <- xml2::xml_find_first(parent, "expr[1]/SYMBOL")
    if (length(nameNode) == 0 || is.na(xml2::xml_text(nameNode))) next
    nm <- xml2::xml_text(nameNode)
    ## body = last child element of the function-def expr
    kids <- xml2::xml_children(fnExpr)
    body <- kids[[length(kids)]]
    ## If body is a `{...}` expr, take its last sub-expr; else body itself.
    bodyKids <- xml2::xml_find_all(body, "expr")
    last <- if (length(bodyKids) > 0) bodyKids[[length(bodyKids)]] else body
    if (.cc_isReturnSim(last)) {
      pos <- .cc_pos(last)
      out[[length(out) + 1]] <- .cc_use(
        "return_sim", name = "sim", fn = nm, file = file,
        line = pos$line, col = pos$col, resolved = TRUE
      )
    }
  }
  if (length(out) == 0) return(.cc_emptyUses())
  do.call(rbind, out)
}

## Structural check: is `expr` one of  sim  /  return(sim)  /  invisible(sim)  /
## return(invisible(sim))  / etc. -- i.e., does the chain of unary calls bottom
## out at a bare SYMBOL=sim?
.cc_isReturnSim <- function(expr) {
  if (xml2::xml_name(expr) == "SYMBOL") {
    return(identical(xml2::xml_text(expr), "sim"))
  }
  if (xml2::xml_name(expr) != "expr") return(FALSE)
  ## bare <expr><SYMBOL>sim</SYMBOL></expr> ?
  syms <- xml2::xml_find_all(expr, "SYMBOL")
  exprs <- xml2::xml_find_all(expr, "expr")
  if (length(syms) == 1 && length(exprs) == 0) {
    return(identical(xml2::xml_text(syms[[1]]), "sim"))
  }
  ## call form: <expr><expr><SYMBOL_FUNCTION_CALL>return|invisible</SYMBOL_FUNCTION_CALL></expr> ( <expr>arg</expr> )
  callName <- xml2::xml_find_first(expr, "expr[1]/SYMBOL_FUNCTION_CALL")
  if (length(callName) == 0) return(FALSE)
  if (!(xml2::xml_text(callName) %in% c("return", "invisible"))) return(FALSE)
  ## first non-name expr arg
  arg <- xml2::xml_find_first(expr, "expr[2]")
  if (length(arg) == 0) return(FALSE)
  Recall(arg)
}

## scheduleEvent / saveFiles — must assign back to sim.
##
## Emits a Use for every call site that is NOT assigned to sim. Rule turns
## those into errors.
.cc_collect_assignToSim <- function(parsed,
                                    fnNames = c("scheduleEvent", "saveFiles")) {
  doc <- parsed$doc; file <- parsed$file
  out <- list()
  for (fnName in fnNames) {
    calls <- xml2::xml_find_all(
      doc,
      sprintf("//expr[expr/SYMBOL_FUNCTION_CALL[text()='%s']]", fnName)
    )
    for (n in calls) {
      ## OK if this call is the RHS of `sim <- <here>` -- i.e., parent expr
      ## has LEFT_ASSIGN and the LHS expr's only SYMBOL child is "sim".
      parent <- xml2::xml_parent(n)
      ok <- FALSE
      if (xml2::xml_name(parent) == "expr") {
        kids <- xml2::xml_children(parent)
        nms <- xml2::xml_name(kids)
        if (any(nms %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
          exprKids <- kids[xml2::xml_name(kids) == "expr"]
          lhsExpr <- if ("RIGHT_ASSIGN" %in% nms) {
            exprKids[[length(exprKids)]]
          } else exprKids[[1]]
          ## A bare `sim` LHS is an expr whose only element child is SYMBOL[sim]
          sym <- xml2::xml_find_first(lhsExpr, "SYMBOL")
          if (length(sym) > 0 && identical(xml2::xml_text(sym), "sim")) {
            ok <- TRUE
          }
        }
      }
      if (!ok) {
        pos <- .cc_pos(n)
        out[[length(out) + 1]] <- .cc_use(
          "assign_to_sim", name = fnName, fn = .cc_enclosingFn(n), file = file,
          line = pos$line, col = pos$col, resolved = TRUE
        )
      }
    }
  }
  if (length(out) == 0) return(.cc_emptyUses())
  do.call(rbind, out)
}

## Conflicting/clashing function uses (raster::levels, quickPlot::Plot, etc.).
## These are flagged by name only -- we don't care about positions for the
## clashing-defined-fn check (handled via env name listing, not parsing).
.cc_collect_globalsConflicts <- function(parsed,
                                         conflictBareNames = c("levels", "scale",
                                                               "which.max")) {
  doc <- parsed$doc; file <- parsed$file
  out <- list()
  for (nm in conflictBareNames) {
    calls <- xml2::xml_find_all(
      doc,
      sprintf("//SYMBOL_FUNCTION_CALL[text()='%s']", nm)
    )
    for (cal in calls) {
      ## skip if it's qualified, i.e., parent expr has NS_GET / NS_GET_INT
      par <- xml2::xml_parent(xml2::xml_parent(cal))
      qual <- xml2::xml_find_first(par, "NS_GET | NS_GET_INT")
      if (length(qual) > 0) next
      pos <- .cc_pos(cal)
      out[[length(out) + 1]] <- .cc_use(
        "global", name = nm, fn = .cc_enclosingFn(cal), file = file,
        line = pos$line, col = pos$col, resolved = TRUE,
        extra = "conflict"
      )
    }
  }
  if (length(out) == 0) return(.cc_emptyUses())
  do.call(rbind, out)
}

## ---------------------------------------------------------------------------
## Driver: collect uses for an entire module (multiple files possible)
## ---------------------------------------------------------------------------

.cc_collectModule <- function(files, currentModule = NA_character_,
                              text = NULL) {
  parses <- if (!is.null(text)) {
    list(.cc_parseFile(text = text))
  } else {
    lapply(files, function(f) .cc_parseFile(file = f))
  }
  uses <- list()
  for (p in parses) {
    uses[[length(uses) + 1]] <- .cc_collect_simAccess(p)
    uses[[length(uses) + 1]] <- .cc_collect_params(p, currentModule)
    uses[[length(uses) + 1]] <- .cc_collect_returnSim(p)
    uses[[length(uses) + 1]] <- .cc_collect_assignToSim(p)
    uses[[length(uses) + 1]] <- .cc_collect_globalsConflicts(p)
  }
  do.call(rbind, uses)
}
