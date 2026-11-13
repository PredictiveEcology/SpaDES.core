## Static reader for event-flow documentation. It parses a module's source
## files and metadata and finds the event dispatch; no module code is evaluated
## (the metadata tables are the one exception, as `.parseModulePartial()`
## evaluates them, as it does everywhere else in SpaDES.core).

## Signal a condition that says why a module could not be read.
.ef_stop <- function(module, msg) {
  stop(structure(
    class = c("moduleEventFlowError", "error", "condition"),
    list(message = paste0("Cannot read module '", module, "': ", msg), call = NULL)
  ))
}

## `f <- function(...)`, or a wrapped one such as `f <- compiler::cmpfun(function(...))`.
## Returns the `function` call, or NULL.
.ef_fnDef <- function(rhs) {
  if (!is.call(rhs)) return(NULL)
  if (identical(rhs[[1]], quote(`function`))) return(rhs)
  if (length(rhs) >= 2 && is.call(rhs[[2]]) && identical(rhs[[2]][[1]], quote(`function`))) {
    return(rhs[[2]])
  }
  NULL
}

.ef_isEmptyArg <- function(x) identical(x, quote(expr = ))

## All calls to one of `heads` anywhere inside `e`.
.ef_findCalls <- function(e, heads) {
  out <- list()
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- x[[1]]
    if (is.name(h) && as.character(h) %in% heads) out[[length(out) + 1L]] <<- x
    for (j in seq_along(x)) {
      if (!.ef_isEmptyArg(x[[j]])) rec(x[[j]])
    }
  }
  rec(e)
  out
}

## Named functions defined at the top level of the parsed files.
.ef_readFns <- function(parsed) {
  fns <- list()
  for (p in parsed) {
    ex <- p$exprs
    srs <- attr(ex, "srcref")
    for (i in seq_along(ex)) {
      e <- ex[[i]]
      if (!is.call(e) || !as.character(e[[1]])[1] %in% c("<-", "=") || !is.name(e[[2]])) next
      fd <- .ef_fnDef(e[[3]])
      if (is.null(fd)) next
      fns[[as.character(e[[2]])]] <- list(
        body = fd[[3]], file = p$file,
        first = srs[[i]][1], last = srs[[i]][3]
      )
    }
  }
  fns
}

## parameters, inputs, outputs and timeunit from the defineModule() call, without
## evaluating the parameter defaults (they are kept as expressions)
.ef_readMeta <- function(module, mainFile) {
  tmp <- .parseConditional(filename = mainFile)
  dm <- tmp[["pf"]]
  params <- list()
  timeunit <- NA_character_
  if (length(dm) == 0) .ef_stop(module, paste0("no defineModule() call in ", mainFile))
  spec <- dm[[1]][[3]]
  tmplP <- function(name, class, default, min, max, desc, ...) NULL
  for (cl in .ef_findCalls(spec$parameters, "defineParameter")) {
    m <- match.call(tmplP, cl)
    nm <- tryCatch(eval(m$name, baseenv()), error = function(e) NA_character_)
    if (is.character(nm) && !is.na(nm)) params[nm] <- list(list(expr = m$default))
  }
  tu <- tryCatch(eval(spec$timeunit, baseenv()), error = function(e) NA_character_)
  if (is.character(tu) && length(tu) == 1) timeunit <- tu
  objNames <- function(element) {
    bad <- function(why) {
      .ef_stop(module, paste0("cannot read ", element, " in ", mainFile, ": ", why))
    }
    ## .parseModulePartial() returns a "try-error" value for metadata it cannot evaluate
    x <- tryCatch(.parseModulePartial(filename = mainFile, defineModuleElement = element),
                  error = function(e) bad(conditionMessage(e)))
    if (inherits(x, "try-error")) bad(conditionMessage(attr(x, "condition")))
    unique(as.character(x$objectName))
  }
  list(params = params, inputs = objNames("inputObjects"),
       outputs = objNames("outputObjects"), timeunit = timeunit)
}

## The line range of a block, from the srcref the parser keeps on `{`.
.ef_lines <- function(x) {
  sr <- if (is.call(x) && identical(x[[1]], quote(`{`))) attr(x, "srcref")[[1]]
  if (is.null(sr)) c(NA_integer_, NA_integer_) else as.integer(c(sr[1], sr[3]))
}

.ef_branch <- function(x, file) {
  if (is.call(x)) {
    attr(x, "file") <- file
    attr(x, "lines") <- .ef_lines(x)
  }
  x
}

## Find the `eventType` dispatch in the body of doEvent.<module>: a switch(), or
## an if / else if chain on `eventType == "name"`. Returns the statements before
## it (`pre`) and the code of each event (`branches`); switch fall-through
## (`Growth = , Grow2 = {...}`) gives both names the same code.
.ef_findDispatch <- function(body) {
  stmts <- as.list(body)[-1]
  findSwitch <- function(s) {
    if (!is.call(s)) return(NULL)
    if (identical(s[[1]], quote(switch)) && identical(s[[2]], quote(eventType))) return(s)
    if (as.character(s[[1]])[1] %in% c("<-", "=", "return", "invisible", "(")) {
      for (j in seq_along(s)[-1]) {
        r <- findSwitch(s[[j]])
        if (!is.null(r)) return(r)
      }
    }
    NULL
  }
  eventValue <- function(cond) {
    if (is.call(cond) && identical(cond[[1]], quote(`==`))) {
      if (identical(cond[[2]], quote(eventType)) && is.character(cond[[3]])) return(cond[[3]])
      if (identical(cond[[3]], quote(eventType)) && is.character(cond[[2]])) return(cond[[2]])
    }
    NULL
  }
  for (i in seq_along(stmts)) {
    s <- stmts[[i]]
    sw <- findSwitch(s)
    if (!is.null(sw)) {
      xs <- as.list(sw)[-(1:2)]
      nms <- names(xs) %||% rep("", length(xs))
      br <- list()
      pend <- character()
      for (k in seq_along(xs)) {
        if (!nzchar(nms[k])) next
        if (.ef_isEmptyArg(xs[[k]])) {
          pend <- c(pend, nms[k])
          next
        }
        for (n in c(pend, nms[k])) br[n] <- list(xs[[k]])
        pend <- character()
      }
      return(list(pre = stmts[seq_len(i - 1)], branches = br))
    }
    if (is.call(s) && identical(s[[1]], quote(`if`)) && !is.null(eventValue(s[[2]]))) {
      br <- list()
      while (is.call(s) && identical(s[[1]], quote(`if`))) {
        v <- eventValue(s[[2]])
        if (is.null(v)) break
        br[v] <- list(s[[3]])
        s <- if (length(s) == 4) s[[4]] else NULL
      }
      return(list(pre = stmts[seq_len(i - 1)], branches = br))
    }
  }
  NULL
}

## Read a module's sources, metadata and event dispatch, statically.
##
## Returns list(name, path, files, parsed, fns, meta, pre, branches):
##   fns:      named list(body, file, first, last) of the top-level functions
##   meta:     list(params = named list(expr), inputs, outputs, timeunit)
##   pre:      statements of doEvent.<module> before the dispatch
##   branches: named list of the code of each event, with attributes "file" and
##             "lines" (c(first, last), NA when unknown)
## A module with no `doEvent.<module>` but per-event `doEvent.<module>.<event>`
## functions has those as its branches; one with neither has none.
## Errors with class `moduleEventFlowError` when the module cannot be read.
.ef_readModule <- function(module, path) {
  mainFile <- file.path(path, module, paste0(module, ".R"))
  if (!file.exists(mainFile)) .ef_stop(module, paste0("no file ", mainFile))
  files <- moduleCodeFiles(list(modulePath = path), module)
  files <- files[file.exists(files)]
  parsed <- lapply(files, function(f) {
    tryCatch(.cc_parseFile(f), error = function(e) .ef_stop(module, conditionMessage(e)))
  })
  fns <- .ef_readFns(parsed)
  meta <- tryCatch(.ef_readMeta(module, mainFile),
                   error = function(e) .ef_stop(module, conditionMessage(e)))

  pre <- list()
  branches <- list()
  prefix <- makeEventFn(module, "")
  deName <- substr(prefix, 1L, nchar(prefix) - 1L)
  if (!is.null(fns[[deName]])) {
    disp <- .ef_findDispatch(fns[[deName]]$body)
    if (!is.null(disp)) {
      pre <- disp$pre
      branches <- lapply(disp$branches, .ef_branch, file = fns[[deName]]$file)
    }
  } else {
    perEvent <- {
      nms <- names(fns) %||% character()
      nms[startsWith(nms, prefix) & nchar(nms) > nchar(prefix)]
    }
    for (nm in perEvent) {
      b <- .ef_branch(fns[[nm]]$body, fns[[nm]]$file)
      if (is.call(b)) attr(b, "lines") <- c(fns[[nm]]$first, fns[[nm]]$last)
      branches[[substring(nm, nchar(prefix) + 1L)]] <- b
    }
  }
  list(name = module, path = path, files = files, parsed = parsed, fns = fns,
       meta = meta, pre = pre, branches = branches)
}
