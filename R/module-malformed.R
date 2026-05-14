## Detection of malformed module metadata.
##
## Catches common authoring mistakes that R's default parse error or
## defineModule()'s downstream errors report only cryptically (e.g.,
## "unexpected symbol" at a random line).
##
## Architecture: a small registry of detectors. Each detector is a function
## that takes a single list of inputs and returns either NULL (no problem
## found) or a single character string that is a clear, actionable error
## message. `.checkMalformedMetadata()` is the single entry point that runs
## the registry and stops with the first hit. Adding a new detector means
## adding one named element to `.CC_MALFORMED_CHECKS`.

## Public, standalone API ----------------------------------------------------

#' Statically check a SpaDES module file for malformed metadata
#'
#' Scans a module source file for common authoring mistakes that produce
#' cryptic R parse errors or surface only deep inside `simInit()`. Returns
#' a character vector of findings, or invisibly an empty vector if the file
#' looks clean.
#'
#' The detector set is intentionally modular: to add a new check, add an
#' entry to the internal `.CC_MALFORMED_CHECKS` registry — each entry is a
#' function `(input) -> NULL | character(1)`.
#'
#' @param file Path to a module's main `.R` file.
#' @param stopOnFirst Logical. If `TRUE` (default), throw an error on the
#'   first detector that fires. If `FALSE`, run every detector and return
#'   the full vector of findings.
#'
#' @return A character vector of findings (empty if none). When
#'   `stopOnFirst = TRUE` and a problem is found, throws an error instead.
#' @export
#' @rdname checkModuleMetadata
checkModuleMetadata <- function(file, stopOnFirst = FALSE) {
  findings <- .runMalformedChecks(file)
  if (length(findings) == 0) return(invisible(character()))
  if (isTRUE(stopOnFirst)) stop(findings[[1]], call. = FALSE)
  findings
}

## Internal entry point called from `.parseConditional()` --------------------

## Called once at the start of the parsing sequence in
## `simulation-parseModule.R::.parseConditional`. Throws on the first
## detected problem (which is what we want at simInit time — a single,
## clear error beats a list of warnings).
.checkMalformedMetadata <- function(file) {
  findings <- .runMalformedChecks(file)
  if (length(findings) > 0) stop(findings[[1]], call. = FALSE)
  invisible(NULL)
}

## Detector engine ----------------------------------------------------------

## Build the input that every detector receives. We parse once here, and
## fan out both the parsed expression list (when parse succeeded) and the
## raw source lines (always). Detectors decide which they need.
.makeMalformedInput <- function(file) {
  src <- tryCatch(readLines(file, warn = FALSE), error = function(e) character())
  pe <- NULL  ## the parse-error condition, if any
  parsed <- tryCatch(
    parse(file, keep.source = TRUE),
    error = function(e) {
      pe <<- e
      NULL
    }
  )
  pd <- if (!is.null(parsed)) utils::getParseData(parsed) else NULL
  list(
    file        = file,
    src         = src,
    parsed      = parsed,
    parseData   = pd,
    parseError  = pe,
    parseFailed = !is.null(pe)
  )
}

.runMalformedChecks <- function(file) {
  input <- .makeMalformedInput(file)
  findings <- character()
  for (id in names(.CC_MALFORMED_CHECKS)) {
    fn <- .CC_MALFORMED_CHECKS[[id]]
    out <- tryCatch(fn(input),
                    error = function(e) NULL) ## a buggy detector mustn't break parsing
    if (!is.null(out) && nzchar(out)) {
      findings <- c(findings, paste0("[", id, "] ", out))
    }
  }
  findings
}

## Detector registry --------------------------------------------------------
##
## Each entry MUST be of the form:
##   function(input) { ... return(NULL | character(1)) }
## where `input` is the list built by `.makeMalformedInput()`.
##
## The character string returned is the error message shown to the user. It
## should be specific and actionable: name the file, give the line if known,
## and suggest the fix.

.CC_MALFORMED_CHECKS <- list(

  ## A trailing comma inside `defineModule(sim, list(...,))` causes parse to
  ## succeed but produces an extra NULL entry in the metadata list. We catch
  ## it from the parse output rather than the raw text so quoted/commented
  ## commas don't trip us up.
  trailing_comma_in_defineModule = function(input) {
    if (input$parseFailed) return(NULL)
    if (is.null(input$parsed)) return(NULL)
    for (expr in input$parsed) {
      if (!is.call(expr)) next
      if (!identical(expr[[1]], as.name("defineModule"))) next
      ## defineModule(sim, list(...)) -> expr has 3 elements (fn, sim, list)
      ## A trailing comma typically lands as an empty arg, which R deparses as
      ## "" and shows up as length(expr) > 3 OR as an empty `[[2]]` inside the
      ## list. The defineModule formals are `sim, x`, so anything beyond 3 is
      ## extra.
      if (length(expr) > 3L) {
        return(sprintf(
          "%s: a trailing or duplicate comma in `defineModule(sim, list(...))` produces an extra argument. Remove the offending comma; the call should be `defineModule(sim, list(...))` with exactly two arguments after the function name.",
          basename(input$file)
        ))
      }
    }
    NULL
  },

  ## When `parse()` fails on a line that looks like the close of a metadata
  ## row (defineParameter / expectsInput / createsOutput / bindrows /
  ## rbind), the most common cause is a missing comma at the end of the
  ## *previous* row. R reports the error on the line AFTER the missing
  ## comma, which is confusing. Detect and rephrase.
  missing_comma_between_metadata_rows = function(input) {
    if (!input$parseFailed) return(NULL)
    pe <- input$parseError
    line <- if (!is.null(pe$line) && length(pe$line)) pe$line else
            .extractParseErrorLine(conditionMessage(pe))
    if (is.null(line) || is.na(line) || line < 2L || line > length(input$src)) {
      return(NULL)
    }
    prev <- input$src[line - 1L]
    rowEnders <- c("defineParameter", "expectsInput", "createsOutput",
                   "bindrows", "rbind")
    pat <- paste0("(", paste(rowEnders, collapse = "|"), ")\\(.*\\)\\s*$")
    if (grepl(pat, prev)) {
      return(sprintf(
        "%s (parse error around line %d): the previous line\n    %s\nlooks like a metadata row that is missing a trailing comma. Inside `rbind(...)` or `bindrows(...)`, each row must end with a comma except the last one.",
        basename(input$file), line, trimws(prev)
      ))
    }
    NULL
  },

  ## defineParameter(name, ...) -- the first arg must be a string literal.
  ## A bare symbol like `defineParameter(stackName, ...)` (no quotes) is a
  ## common copy-paste mistake.
  param_name_not_quoted = function(input) {
    bad <- .findBadFirstArg(input$parseData, callName = "defineParameter",
                            allowDynamic = FALSE)
    if (length(bad) == 0) return(NULL)
    rows <- vapply(bad, function(b)
      sprintf("    line %d:  defineParameter(%s, ...)", b$line, b$expr),
      character(1))
    sprintf(
      "%s: %d `defineParameter()` call(s) where the first argument is not a quoted name:\n%s\nThe first argument must be a character string, e.g. `defineParameter(\"%s\", ...)`.",
      basename(input$file), length(bad), paste(rows, collapse = "\n"),
      bad[[1]]$expr
    )
  },

  ## expectsInput() / createsOutput() first arg (objectName) should be a
  ## string OR an expression that resolves to one (e.g.,
  ## `P(sim, module="...")$stackName`).
  obj_name_not_quoted = function(input) {
    bad <- c(
      .findBadFirstArg(input$parseData, callName = "expectsInput",
                       allowDynamic = TRUE),
      .findBadFirstArg(input$parseData, callName = "createsOutput",
                       allowDynamic = TRUE)
    )
    if (length(bad) == 0) return(NULL)
    rows <- vapply(bad, function(b)
      sprintf("    line %d:  %s(%s, ...)", b$line, b$call, b$expr),
      character(1))
    sprintf(
      "%s: %d `expectsInput()`/`createsOutput()` call(s) with an `objectName` that is neither a string nor a recognised dynamic name:\n%s\nThe `objectName` argument should be a string, e.g. `expectsInput(\"%s\", \"<class>\", \"<desc>\")`.",
      basename(input$file), length(bad), paste(rows, collapse = "\n"),
      bad[[1]]$expr
    )
  }
)

## Helpers ------------------------------------------------------------------

## Pull a line number out of a parse-error message like
## "<text>:42:3: unexpected ')'".
.extractParseErrorLine <- function(msg) {
  m <- regmatches(msg, regexpr("[0-9]+:[0-9]+:", msg))
  if (length(m) == 0 || !nzchar(m)) return(NA_integer_)
  as.integer(sub(":.*$", "", m))
}

## Find calls to `callName(...)` in the parse data whose first positional
## argument is a bare SYMBOL rather than a STR_CONST. Returns a list of
## list(line, expr, call) entries.
##
## `parseData` is the result of getParseData(parse(file, keep.source=TRUE)).
## When `allowDynamic = TRUE`, recognise a handful of dynamic-name patterns
## (P(sim, ...), params(sim), paste/paste0, sprintf, sim$x) and treat them
## as OK.
##
## Implementation: walk every SYMBOL_FUNCTION_CALL row whose `text == callName`.
## For each, find its enclosing `expr` parent, then look at the first
## non-positional child `expr` after the function-name expr — that's the
## first argument. If it's a single SYMBOL (with no STR_CONST around it),
## it's a bare symbol → flag.
.findBadFirstArg <- function(parseData, callName, allowDynamic = FALSE) {
  if (is.null(parseData) || NROW(parseData) == 0) return(list())
  pd <- parseData
  calls <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == callName)
  out <- list()
  for (ci in calls) {
    fnCallId   <- pd$id[ci]
    fnExprId   <- pd$parent[ci]                    ## <expr> wrapping the SYMBOL_FUNCTION_CALL
    callExprId <- pd$parent[which(pd$id == fnExprId)]   ## <expr> wrapping the whole call
    if (length(callExprId) == 0 || is.na(callExprId)) next
    siblings <- pd[pd$parent == callExprId, , drop = FALSE]
    argExprs <- siblings[siblings$token == "expr", , drop = FALSE]
    argExprs <- argExprs[argExprs$id != fnExprId, , drop = FALSE]
    if (NROW(argExprs) == 0) next
    ## first positional arg = first expr child of the call. Named-arg form
    ## puts SYMBOL_SUB + EQ_SUB between siblings; argExprs[1, ] is still the
    ## value, which is what we want.
    firstArg <- argExprs[1, ]
    line <- as.integer(firstArg$line1)
    ## `text` is empty for expr parents; reconstruct from child terminals.
    text <- .exprText(pd, firstArg$id)
    if (.looksLikeStringConst(pd, firstArg$id)) next     ## "..." literal — OK
    if (allowDynamic && .looksLikeRecognisedDynamic(pd, firstArg$id, text)) next
    if (.looksLikeBareSymbol(pd, firstArg$id)) {
      out[[length(out) + 1]] <- list(
        line = line, expr = .shortText(text), call = callName
      )
    }
  }
  out
}

.looksLikeStringConst <- function(pd, exprId) {
  kids <- pd[pd$parent == exprId, , drop = FALSE]
  ## an expr whose only token is STR_CONST
  any(kids$token == "STR_CONST")
}

.looksLikeBareSymbol <- function(pd, exprId) {
  kids <- pd[pd$parent == exprId, , drop = FALSE]
  identical(kids$token, "SYMBOL")
}

## Recognise expressions whose value is determined at runtime but is
## conventionally a string in SpaDES module metadata. Conservative: only
## explicitly-listed patterns are accepted.
.looksLikeRecognisedDynamic <- function(pd, exprId, text) {
  if (grepl("^(SpaDES\\.core::)?P\\s*\\(", text)) return(TRUE)
  if (grepl("^params\\s*\\(", text)) return(TRUE)
  if (grepl("^paste0?\\s*\\(", text)) return(TRUE)
  if (grepl("^sprintf\\s*\\(", text)) return(TRUE)
  if (grepl("^sim\\$", text)) return(TRUE)
  FALSE
}

## Concatenate all terminal `text` entries descended from an expr node.
.exprText <- function(pd, exprId) {
  ## terminals = nodes that no other node points at as parent
  ids <- exprId
  out <- character()
  repeat {
    kids <- pd[pd$parent %in% ids, , drop = FALSE]
    if (NROW(kids) == 0) break
    terms <- kids[!(kids$id %in% pd$parent), , drop = FALSE]
    out <- c(out, terms$text)
    ids <- kids$id[!(kids$id %in% terms$id)]
    if (length(ids) == 0) break
  }
  paste(out, collapse = "")
}

.shortText <- function(x) {
  x <- gsub("\\s+", " ", x)
  if (nchar(x) > 80L) paste0(substr(x, 1, 77), "...") else x
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 0L)) b else a
