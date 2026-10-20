## Event cache keys that contain only what the event's code can reach.
##
## With `options(spades.cacheKeyReachable = TRUE)`, the key of a cached event E of module M holds
##   * the module functions (and module-level objects) reachable from E's code: E's branch of
##     `doEvent.M` (or `.inputObjects`), then everything those call, to any depth;
##   * the module parameters that reachable code reads, plus `.seed`;
##   * E's branch of `doEvent.M` itself, in place of the whole `doEvent.M`;
## and no longer the module `version` or `reqdPkgs` strings: the reachable function bodies carry
## code changes, and the strings never carried the installed package code.
##
## Static reading of R code cannot always be complete. Whenever reachable code uses something the
## analysis cannot resolve, E keeps the full key it has always had (all parameters, all module
## functions, version, reqdPkgs). A narrowed key that missed a dependency would return a stale
## result, so narrowing only happens when every parameter access and every call is accounted for.

._txtCacheKeyReachable <- "spades.cacheKeyReachable"

## Report each narrowed key, or why it was not narrowed, also when showSimilar is off
.verboseKeyScope <- function() isTRUE(getOption("reproducible.verbose", 1) >= 2)

## SpaDES.core and base functions that may receive the simList without reading its module
## parameters. A call that passes `sim` to any other function that is not a module function
## makes the key fall back, since that function could read any parameter through `P(sim)`.
.simArgSafeFns <- c(
  ## SpaDES.core accessors that do not read parameters
  "time", "start", "end", "currentModule", "scheduleEvent", "scheduleConditionalEvent",
  "inputPath", "outputPath", "dataPath", "modulePath", "cachePath", "figurePath", "rasterPath",
  "scratchPath", "logPath", "paths", "timeunit", "timeunits", "suppliedElsewhere", "events",
  "current", "completed", "outputs", "inputs", "envir", "modules", "depends", "packages",
  "registerOutputs", "checkObject", "objectSynonyms", "outputObjectNames", "inputObjectNames",
  "mod", "elapsedTime", "globals", "extractURL", "outputObjects", "inputObjects",
  "paramCheckOtherMods", "saveFiles", "Plots",
  ## base: language, tests and returns
  "<-", "=", "<<-", "$", "[[", "[", "@", "{", "(", "if", "for", "while", "repeat", "return",
  "invisible", "is.null", "missing", "identical", "is", "inherits", "class", "isS4",
  "stopifnot", "print", "message", "warning", "stop", "names", "ls", "length"
)

## Functions that call a function argument with the remaining arguments (`lapply(x, f, sim)`):
## passing `sim` to them is safe only when that function is a module function or a function
## literal, both of which are read by the analysis.
## Each entry gives the position and possible names of that function argument.
.simArgForwarders <- list(lapply = list(2L, c("FUN")), sapply = list(2L, c("FUN")),
                          vapply = list(2L, c("FUN")), Map = list(1L, c("f")),
                          mapply = list(1L, c("FUN")), do.call = list(1L, c("what")),
                          Cache = list(1L, c("FUN")), Reduce = list(1L, c("f")),
                          Filter = list(1L, c("f")))

## Calls whose meaning depends on values only known at run time
.dynamicCallFns <- c("eval", "evalq", "eval.parent", "parse", "str2lang", "str2expression",
                     "source", "sys.source", "sys.function", "body<-", "formals<-",
                     "environment<-", "attach", "eval_tidy", "inject")

## Parameter classes (defineParameter) whose value is code: `eval(P(sim)$x)` then runs that code
.languageClasses <- c("call", "language", "name", "symbol", "expression", "formula", "function",
                      "closure")

## Calls that read a function's code without running it: `deparse(f)` in a `.cacheExtra`
.fnCodeReaders <- c("deparse", "body", "formals")

## Calls that name an object or function by a string: resolvable only for a literal
.byNameFns <- c("get", "get0", "mget", "exists", "match.fun", "getFunction", "do.call",
                "getFromNamespace")

## SpaDES.core functions that read module parameters of the current module themselves
.fnsReadingParams <- list(Plots = c(".plots", ".plotInitialTime"), saveFiles = ".saveObjects")

## Universal parameters SpaDES.core acts on for every event, whether or not module code reads them
.paramsAlwaysInKey <- ".seed"

## The cache key scope of one event. Returns NULL when the option is off; otherwise a list:
##   narrowed  TRUE when the key can be narrowed, FALSE when it falls back
##   reasons   why it falls back (character, empty when narrowed)
##   objects   names in the module environment reachable from the event (functions and objects)
##   params    module parameters the event's reachable code reads
##   eventCode the event's branch of `doEvent.<module>`, deparsed (NULL for `.inputObjects`)
## `paramClasses` is the module's declared parameter classes (see `.paramClasses()`): without them,
## `eval()` of a parameter is a fallback.
.cacheKeyScope <- function(fnEnv, module, eventType, paramClasses = NULL) {
  if (!isTRUE(getOption(._txtCacheKeyReachable, FALSE))) return(NULL)
  .cacheKeyScopeAnalyse(fnEnv, module, eventType, paramClasses)
}

## The declared class(es) of each parameter of `module`, as a named list
.paramClasses <- function(sim, module) {
  p <- sim@depends@dependencies[[module]]@parameters
  if (is.null(p)) return(NULL)
  setNames(as.list(p$paramClass), p$paramName)
}

.cacheKeyScopeAnalyse <- function(fnEnv, module, eventType, paramClasses = NULL) {
  fallback <- function(reasons)
    list(narrowed = FALSE, reasons = unique(reasons), objects = character(), params = character(),
         eventCode = NULL)
  if (is.null(fnEnv)) return(fallback("no module environment"))
  envNames <- ls(fnEnv, all.names = TRUE)
  isFn <- vapply(envNames, function(n) is.function(get0(n, envir = fnEnv, inherits = FALSE)),
                 logical(1))
  modFns <- envNames[isFn]

  eventCode <- NULL
  if (identical(eventType, ".inputObjects")) {
    start <- get0(".inputObjects", envir = fnEnv, inherits = FALSE)
    if (!is.function(start)) return(fallback("no .inputObjects function"))
    startName <- ".inputObjects"
  } else {
    startName <- paste0("doEvent.", module)
    doEv <- get0(startName, envir = fnEnv, inherits = FALSE)
    if (!is.function(doEv)) return(fallback(paste0("no function ", startName)))
    pruned <- .pruneEventSwitch(body(doEv), eventType)
    if (pruned$n == 0L)
      return(fallback(paste0(startName, " has no switch(eventType, ...)")))
    start <- doEv
    body(start) <- pruned$expr
    ## formals and the one branch, without srcref, so only code changes alter it
    eventCode <- paste(deparse(call("function", formals(doEv), pruned$expr), width.cutoff = 500L),
                       collapse = "\n")
  }

  seen <- character()
  objects <- character()
  params <- .paramsAlwaysInKey
  reasons <- character()
  calledFormals <- list()
  valueRefs <- character()
  modCalls <- list()
  ## the start is digested by name for `.inputObjects`, and as `eventCode` (one branch) for events
  toVisit <- list(list(name = startName, fn = start, isStart = !is.null(eventCode)))
  while (length(toVisit)) {
    item <- toVisit[[1L]]
    toVisit <- toVisit[-1L]
    if (item$name %in% seen && !isTRUE(item$isStart)) next
    if (!isTRUE(item$isStart)) {
      seen <- c(seen, item$name)
      objects <- c(objects, item$name)
    }
    u <- .keyCodeUses(item$fn, module = module, modFns = modFns, fnEnv = fnEnv,
                      paramClasses = paramClasses)
    params <- c(params, u$params)
    reasons <- c(reasons, if (length(u$reasons)) paste0(item$name, ": ", u$reasons))
    if (length(u$calledFormals)) calledFormals[[item$name]] <- u$calledFormals
    valueRefs <- c(valueRefs, u$valueRefs)
    modCalls <- c(modCalls, u$modCalls)
    refs <- setdiff(intersect(c(u$globals, .charLiteralsIn(body(item$fn))), envNames),
                    c(seen, "mod", "Par", ".objects"))
    for (r in refs) {
      obj <- get0(r, envir = fnEnv, inherits = FALSE)
      if (is.function(obj)) {
        toVisit[[length(toVisit) + 1L]] <- list(name = r, fn = obj)
      } else {
        seen <- c(seen, r)
        objects <- c(objects, r)
      }
    }
  }
  reasons <- c(reasons, .calledFormalReasons(calledFormals, valueRefs, modCalls, fnEnv, modFns))
  if (length(reasons)) return(fallback(reasons))
  list(narrowed = TRUE, reasons = character(), objects = sort(unique(objects)),
       params = sort(unique(params)), eventCode = eventCode)
}

## Replace each `switch(eventType, ...)` by the branch `eventType` takes (with R's fall-through
## for empty alternatives). An unmatched event takes the unnamed default, if any.
.pruneEventSwitch <- function(expr, eventType) {
  n <- 0L
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], quote(switch)) && length(e) > 2L && identical(e[[2L]], quote(eventType))) {
        n <<- n + 1L
        alts <- as.list(e)[-(1:2)]
        nms <- names(alts) %||% rep("", length(alts))
        idx <- match(eventType, nms)
        if (is.na(idx)) idx <- which(!nzchar(nms))[1L]
        if (is.na(idx)) return(NULL)
        isEmpty <- vapply(alts, function(a) identical(a, quote(expr = )), logical(1))
        while (idx <= length(alts) && isEmpty[idx]) idx <- idx + 1L
        if (idx > length(alts)) return(NULL)
        return(walk(alts[[idx]]))
      }
      for (i in seq_along(e)[-1L]) {
        if (!identical(e[[i]], quote(expr = ))) {
          new <- walk(e[[i]])
          if (is.null(new)) e[i] <- list(NULL) else e[[i]] <- new
        }
      }
    }
    e
  }
  list(expr = walk(expr), n = n)
}

## What one function's code reaches: the globals it references, the module parameters it reads,
## and the reasons (if any) the analysis cannot account for all of them.
##   `fnEnv` is the module environment; module functions found there are looked into when the
##   module's whole parameter list is passed to one of them.
.keyCodeUses <- function(fn, module, modFns, fnEnv = NULL, paramClasses = NULL) {
  globals <- tryCatch(codetools::findGlobals(fn, merge = TRUE), error = function(e) NULL)
  if (is.null(globals))
    return(list(globals = character(), params = character(), reasons = "could not be analysed"))
  b <- body(fn)
  fnLiterals <- .functionLiterals(fn)
  formalsAll <- unique(unlist(lapply(fnLiterals, function(f) names(f$formals))))
  locals <- unique(c(formalsAll, unlist(lapply(fnLiterals, function(f)
    codetools::findFuncLocals(f$formals, f$body)))))
  localFns <- .assignedNames(b, function(rhs) is.call(rhs) && identical(rhs[[1L]], quote(`function`)))
  fromObjects <- .assignedNames(b, function(rhs) is.call(rhs) &&
                                  as.character(rhs[[1L]])[1L] %in% c("$", "[[", "[", "@"))
  simNames <- c("sim", .assignedNames(b, function(rhs) identical(rhs, quote(sim))))
  curModNames <- .assignedNames(b, .isCurrentModuleExpr)
  isWholeOwnList <- function(e) .isWholeParamList(e, module, curModNames)
  parAliases <- setdiff(.assignedNames(b, isWholeOwnList), "Par")
  consts <- .stringConstants(b, exclude = formalsAll) # `k <- "x"` then `Par[[k]]` reads `x`

  params <- character()
  reasons <- character()
  addReason <- function(...) reasons <<- c(reasons, paste0(...))
  dep <- function(e) paste(deparse(e, width.cutoff = 60L)[1L], collapse = "")
  modKey <- function(k, dollar = FALSE) .moduleKey(k, module, curModNames, dollar)
  isListName <- function(e) is.name(e) && as.character(e) %in% c("Par", parAliases)
  selName <- function(e) .selectedName(e, consts)
  pParam <- function(e) .constOf(.pParam(e), consts)
  ## the parameter a literal read `Par$x`, `P(sim)$x`, `P(sim, "x")` names, or NA
  literalParamRead <- function(a) {
    if (.isSelector(a) && (isListName(a[[2L]]) || (.isFnCall(a[[2L]], "P") &&
                                                   is.null(.pParam(a[[2L]])) &&
                                                   .pModule(a[[2L]], modKey) == "own")))
      return(selName(a))
    if (.isFnCall(a, "P") && .pModule(a, modKey) == "own") {
      p <- pParam(a)
      if (is.character(p) && length(p) == 1L) return(p)
    }
    NA_character_
  }
  calledFormals <- character() # formals this function calls as functions
  valueRefs <- character()     # module functions used as values, not called
  modCalls <- list()           # calls to module functions, with this function's local functions

  ## `dt`: inside the i, j, by of `x[...]`, where get(col) reads a data.table column
  walk <- function(e, dt = FALSE) {
    if (is.name(e)) {
      nm <- as.character(e)
      if (nm %in% modFns && !nm %in% locals) valueRefs <<- c(valueRefs, nm)
      if (nm %in% c("Par", parAliases)) addReason(nm, " used whole")
      else if (nm %in% c("P", "params") && !nm %in% locals) addReason(nm, " used as a value")
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    head <- e[[1L]]
    sel <- .isSelector(e)

    ## Par$x, p$x where p <- P(sim)
    if (sel && isListName(e[[2L]])) {
      p <- selName(e)
      if (is.na(p)) addReason("non-literal ", dep(e)) else params <<- c(params, p)
      if (identical(head, quote(`[[`))) walk(e[[3L]])
      return(invisible())
    }
    ## P(sim)$x, P(sim)[["x"]]
    if (sel && .isFnCall(e[[2L]], "P") && is.null(.pParam(e[[2L]]))) {
      pm <- .pModule(e[[2L]], modKey)
      p <- selName(e)
      if (pm == "unknown") addReason("P() with a non-literal module: ", dep(e))
      else if (pm == "own") {
        if (is.na(p)) addReason("non-literal ", dep(e)) else params <<- c(params, p)
      }
      for (i in seq_along(e[[2L]])[-1L]) walk(e[[2L]][[i]])
      if (identical(head, quote(`[[`))) walk(e[[3L]])
      return(invisible())
    }
    ## params(sim)[[m]]$x, params(sim)$m$x
    if (sel && .isSelector(e[[2L]]) && .isFnCall(e[[2L]][[2L]], "params")) {
      inner <- e[[2L]]
      mk <- .selectorModule(inner, modKey)
      p <- selName(e)
      if (mk == "unknown") addReason("params() with a non-literal module: ", dep(e))
      else if (mk == "own") {
        if (is.na(p)) addReason("non-literal ", dep(e)) else params <<- c(params, p)
      }
      for (i in seq_along(inner[[2L]])[-1L]) walk(inner[[2L]][[i]])
      if (identical(inner[[1L]], quote(`[[`))) walk(inner[[3L]])
      if (identical(head, quote(`[[`))) walk(e[[3L]])
      return(invisible())
    }
    ## another module's whole parameter list: not part of this key, as before
    if (sel && .isFnCall(e[[2L]], "params") && .selectorModule(e, modKey) == "other")
      return(invisible())
    if (.isFnCall(e, "P")) {
      pm <- .pModule(e, modKey)
      p <- pParam(e)
      if (pm == "unknown") addReason("P() with a non-literal module: ", dep(e))
      else if (pm == "own") {
        if (is.null(p)) addReason("P(sim) used whole: ", dep(e))
        else if (is.character(p) && length(p) == 1L) params <<- c(params, p)
        else addReason("non-literal ", dep(e))
      }
      for (i in seq_along(e)[-1L]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
      return(invisible())
    }
    if (.isFnCall(e, "params")) {
      addReason("params(sim) used whole: ", dep(e))
      return(invisible())
    }
    if (identical(head, quote(`@`)) && identical(e[[3L]], quote(params))) {
      addReason("sim@params")
      return(invisible())
    }
    if (identical(head, quote(slot)) && length(e) >= 3L && identical(e[[3L]], "params")) {
      addReason("slot(sim, \"params\")")
      return(invisible())
    }
    if (identical(head, quote(paramCheckOtherMods)) ||
        identical(head, quote(SpaDES.core::paramCheckOtherMods))) {
      p <- if (length(e) >= 3L) e[[3L]] else NULL
      if (is.character(p) && length(p) == 1L) params <<- c(params, p)
      else addReason("non-literal ", dep(e))
    }

    headName <- .callHeadName(head)
    headTxt <- dep(head)
    if (!is.na(headName) && headName %in% names(.fnsReadingParams))
      params <<- c(params, .fnsReadingParams[[headName]])

    ## eval() of something that is not code: a data.table column name (`DT[, eval(col) := v]`,
    ## like get(col)), a package function's formal default, or a parameter declared as a value
    if (!is.na(headName) && headName == "eval" && length(e) == 2L) {
      a <- e[[2L]]
      if ((dt && is.name(a)) || .isPkgFormalDefault(a)) {
        walk(a, dt)
        return(invisible())
      }
      p <- literalParamRead(a)
      if (!is.na(p)) {
        cls <- paramClasses[[p]]
        if (is.null(cls)) addReason("eval() of parameter ", p, ", whose declared class is unknown")
        else if (any(cls %in% .languageClasses))
          addReason("eval() of parameter ", p, ", declared ", paste(cls, collapse = "/"), ": its value is code")
        walk(a, dt) # the parameter read itself
        return(invisible())
      }
    }

    ## dynamic code
    if (!is.na(headName) && headName %in% .dynamicCallFns)
      addReason(headTxt, "()")
    if (!is.na(headName) && headName %in% .byNameFns && length(e) >= 2L) {
      a1 <- e[[2L]]
      nms <- names(e)
      if (!is.null(nms)) {
        key <- intersect(c("x", "what", "FUN", "f", "name", "list"), nms[-1L])
        if (length(key)) a1 <- e[[key[1L]]]
      }
      okLit <- is.character(a1) || (headName == "do.call" && is.name(a1)) ||
        (is.call(a1) && identical(a1[[1L]], quote(c)) &&
           all(vapply(as.list(a1)[-1L], is.character, logical(1))))
      ## a lookup in the simList's environment (its parent is emptyenv()) finds simList objects
      ##   only, never module functions or parameters; get(col) in a data.table finds a column
      envirArg <- if ("envir" %in% names(e)) e[["envir"]] else if (length(e) >= 3L &&
                                                                    !nzchar(names(e)[3L] %||% "")) e[[3L]]
      inSimEnv <- headName %in% c("get", "get0", "mget", "exists") && !is.null(envirArg) &&
        (.isFnCall(envirArg, "envir") || identical(envirArg, quote(sim@.xData)))
      inDT <- dt && headName %in% c("get", "mget")
      ## mget() without `inherits = TRUE` returns only the local variables of this frame
      inherits <- if ("inherits" %in% names(e)) e[["inherits"]] else FALSE
      localMget <- headName == "mget" && isFALSE(inherits) &&
        (is.null(envirArg) || .isFnCall(envirArg, "environment"))
      inSimEnv <- inSimEnv || localMget
      if (!okLit && !inSimEnv && !inDT) addReason(headTxt, "() with a non-literal name: ", dep(e))
    }

    ## the called function itself
    if (is.na(headName)) {
      isFnLiteral <- is.call(head) && (identical(head[[1L]], quote(`function`)) ||
                                         (identical(head[[1L]], quote(`(`)) && is.call(head[[2L]]) &&
                                            identical(head[[2L]][[1L]], quote(`function`))))
      if (!isFnLiteral) addReason("calls a function held in an object: ", headTxt, "()")
    } else if (is.name(head) && !headName %in% c(modFns, localFns)) {
      if (headName %in% fromObjects)
        addReason("calls `", headName, "`, a function taken from an object")
      else if (headName %in% formalsAll)
        calledFormals <<- c(calledFormals, headName) # checked at its call sites
    }
    if (is.name(head) && headName %in% modFns && !headName %in% locals)
      modCalls[[length(modCalls) + 1L]] <<- list(callee = headName, call = e, localFns = localFns)

    ## the module's whole parameter list as an argument
    isAssign <- (identical(head, quote(`<-`)) || identical(head, quote(`=`))) && is.name(e[[2L]])
    args <- as.list(e)[-1L]
    for (i in seq_along(args)) {
      if (!isWholeOwnList(e[[i + 1L]]) || (isAssign && i == 2L)) next
      if (!is.na(headName) && headName %in% modFns && !is.null(fnEnv)) {
        pl <- .paramListArgUses(get0(headName, envir = fnEnv, inherits = FALSE), e, i)
        if (is.null(pl)) addReason(dep(e[[i + 1L]]), " passed whole to ", headName, "()")
        else params <<- c(params, pl)
      } else {
        addReason(dep(e[[i + 1L]]), " passed whole to ", headTxt, "()")
      }
    }

    ## passing the simList on
    passesSim <- any(vapply(args, function(a) is.name(a) && as.character(a) %in% simNames, logical(1)))
    if (passesSim && !is.na(headName) && !headName %in% c(modFns, localFns, .simArgSafeFns)) {
      fw <- .simArgForwarders[[headName]]
      if (is.null(fw)) {
        addReason("passes sim to ", headTxt, "(), which may read parameters")
      } else {
        nms <- names(args) %||% rep("", length(args))
        f <- if (any(nms %in% fw[[2L]])) args[[which(nms %in% fw[[2L]])[1L]]] else {
          pos <- args[!nzchar(nms)]
          if (length(pos) >= fw[[1L]]) pos[[fw[[1L]]]] else NULL
        }
        known <- (is.name(f) && as.character(f) %in% c(modFns, localFns)) ||
          (is.character(f) && length(f) == 1L && f %in% modFns) ||
          (is.call(f) && identical(f[[1L]], quote(`function`)))
        if (!known)
          addReason("passes sim to ", headTxt, "() with ", dep(f), ", not a module function")
      }
    }

    ## recurse; the right side of `$` and `@` is a name, not code
    idx <- seq_along(e)[-1L]
    if (identical(head, quote(`$`)) || identical(head, quote(`@`))) idx <- 2L
    if (isAssign) idx <- setdiff(idx, 2L) # the name assigned to is not a use
    ## deparse(f): f's code is read (f is reached through findGlobals), not passed around
    if (!is.na(headName) && headName %in% .fnCodeReaders && length(e) >= 2L &&
        is.name(e[[2L]]) && as.character(e[[2L]]) %in% modFns) idx <- setdiff(idx, 2L)
    isBracket <- identical(head, quote(`[`))
    for (i in idx) {
      if (identical(e[[i]], quote(expr = ))) next
      a <- e[[i]]
      dtArg <- dt || (isBracket && i >= 3L)
      if (isWholeOwnList(a) && !is.name(a)) {
        ## counted above; still read the arguments inside, e.g. `sim`
        inner <- if (.isSelector(a)) a[[2L]] else a
        if (.isSelector(inner)) inner <- inner[[2L]]
        for (j in seq_along(inner)[-1L])
          if (!identical(inner[[j]], quote(expr = ))) walk(inner[[j]], dtArg)
      } else if (!(isWholeOwnList(a) && is.name(a))) {
        walk(a, dtArg)
      }
    }
    if (!is.name(head)) walk(head, dt)
    invisible()
  }
  walk(b)
  ## default values of formals are code too
  fmls <- formals(fn)
  for (i in seq_along(fmls)) if (!identical(fmls[[i]], quote(expr = ))) walk(fmls[[i]])

  list(globals = globals, params = unique(params), reasons = unique(reasons),
       calledFormals = unique(calledFormals), valueRefs = unique(valueRefs), modCalls = modCalls)
}

## A module function that calls one of its own arguments runs whatever its callers pass. That is
## known when every reachable call passes a function literal, a module function, a local function
## of the caller, or a constant (then the call finds a function by name as usual) -- and when the
## function is never used as a value, which would hide its callers (e.g. `lapply(x, f)`).
.calledFormalReasons <- function(calledFormals, valueRefs, modCalls, fnEnv, modFns) {
  reasons <- character()
  for (fnName in names(calledFormals)) {
    fn <- get0(fnName, envir = fnEnv, inherits = FALSE)
    for (g in calledFormals[[fnName]]) {
      what <- paste0(fnName, ": calls `", g, "`, a function passed in as an argument")
      if (fnName %in% valueRefs) {
        reasons <- c(reasons, paste0(what, ", and ", fnName, " is passed around as a value"))
        next
      }
      for (mc in modCalls[vapply(modCalls, function(x) identical(x$callee, fnName), logical(1))]) {
        m <- tryCatch(match.call(fn, mc$call), error = function(e) NULL)
        arg <- if (is.null(m)) NULL else if (g %in% names(m)) m[[g]] else formals(fn)[[g]]
        if (is.null(m) || !.isKnownFunctionArg(arg, c(modFns, mc$localFns))) {
          reasons <- c(reasons, paste0(what, " (", paste(deparse(mc$call)[1L], collapse = ""), ")"))
          break
        }
      }
    }
  }
  reasons
}

.isKnownFunctionArg <- function(arg, knownFns) {
  if (is.null(arg) || (is.atomic(arg) && !is.name(arg))) return(TRUE)
  if (is.name(arg)) return(as.character(arg) %in% knownFns)
  if (is.call(arg) && identical(arg[[1L]], quote(`function`))) return(TRUE)
  if (is.call(arg) && identical(arg[[1L]], quote(c)))
    return(all(vapply(as.list(arg)[-1L], function(a) is.atomic(a), logical(1))))
  FALSE
}

## `x$name` or `x[["name"]]` (or `x[[k]]`)
.isSelector <- function(e) is.call(e) && length(e) == 3L &&
  (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`[[`)))

## the name selected by `x$name` / `x[["name"]]`, or NA when it is not a literal;
## `x[[k]]` is literal when `k` is one of the string constants `consts` (see `.stringConstants()`)
.selectedName <- function(e, consts = NULL) {
  k <- e[[3L]]
  if (identical(e[[1L]], quote(`$`)) && (is.name(k) || is.character(k))) return(as.character(k))
  k <- .constOf(k, consts)
  if (identical(e[[1L]], quote(`[[`)) && is.character(k) && length(k) == 1L) return(k)
  NA_character_
}

## `k` itself, or the string it holds when it is a name among the string constants `consts`
.constOf <- function(k, consts)
  if (is.name(k) && as.character(k) %in% names(consts)) consts[[as.character(k)]] else k

## `formals(pkg::f)$x` / `formals(pkg::f)[["x"]]`: a package function's default, not module code
.isPkgFormalDefault <- function(a) {
  if (!.isSelector(a) || is.na(.selectedName(a))) return(FALSE)
  f <- a[[2L]]
  is.call(f) && identical(f[[1L]], quote(formals)) && length(f) == 2L && is.call(f[[2L]]) &&
    (identical(f[[2L]][[1L]], quote(`::`)) || identical(f[[2L]][[1L]], quote(`:::`)))
}

## Variables assigned exactly once in `expr` (by `<-`, `=`, `<<-` or `for`), to a string literal:
## a named character vector, name -> string. `exclude` are names that are also formals.
.stringConstants <- function(expr, exclude = character()) {
  n <- integer()
  val <- list()
  walk <- function(e) {
    if (!is.call(e)) return()
    h <- e[[1L]]
    isAssign <- (identical(h, quote(`<-`)) || identical(h, quote(`=`)) || identical(h, quote(`<<-`))) &&
      length(e) == 3L
    if ((isAssign || identical(h, quote(`for`))) && is.name(e[[2L]])) {
      nm <- as.character(e[[2L]])
      n[nm] <<- sum(n[nm], 1L, na.rm = TRUE)
      if (isAssign && is.character(e[[3L]]) && length(e[[3L]]) == 1L) val[[nm]] <<- e[[3L]]
    }
    for (i in seq_along(e)[-1L]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
  walk(expr)
  once <- setdiff(names(n)[n == 1L], exclude)
  unlist(val[intersect(once, names(val))])
}

## `f(...)` or `SpaDES.core::f(...)`
.isFnCall <- function(e, f) is.call(e) &&
  (identical(e[[1L]], as.name(f)) || identical(e[[1L]], call("::", quote(SpaDES.core), as.name(f))))

## The name of a called function (`f()` or `pkg::f()`), or NA for any other call head
.callHeadName <- function(head) {
  if (is.name(head)) return(as.character(head))
  if (is.call(head) && (identical(head[[1L]], quote(`::`)) || identical(head[[1L]], quote(`:::`))))
    return(as.character(head[[3L]]))
  NA_character_
}

## `currentModule(sim)` or `current(sim)$moduleName`
.isCurrentModuleExpr <- function(k) {
  .isFnCall(k, "currentModule") ||
    (.isSelector(k) && .isFnCall(k[[2L]], "current") && identical(.selectedName(k), "moduleName"))
}

## Which module a module key refers to: "own", "other" or "unknown". A symbol is a literal name
## only after `$`; inside `[[ ]]` it is a variable, known only when it holds currentModule(sim).
.moduleKey <- function(k, module, curModNames = character(), dollar = FALSE) {
  if (is.character(k) && length(k) == 1L) return(if (identical(k, module)) "own" else "other")
  if (dollar && is.name(k)) return(if (identical(as.character(k), module)) "own" else "other")
  if (.isCurrentModuleExpr(k)) return("own")
  if (!dollar && is.name(k) && as.character(k) %in% curModNames) return("own")
  "unknown"
}

## The module of `params(sim)$m` / `params(sim)[[m]]`
.selectorModule <- function(e, modKey) {
  if (identical(e[[1L]], quote(`$`))) modKey(e[[3L]], dollar = TRUE) else modKey(e[[3L]])
}

## The `param` argument of a P() call (NULL when absent), and the module it refers to
.pParam <- function(e) {
  a <- as.list(e)[-1L]
  nms <- names(a) %||% rep("", length(a))
  if ("param" %in% nms) return(a[["param"]])
  pos <- which(!nzchar(nms))
  if (length(pos) >= 2L) a[[pos[2L]]] else NULL
}
.pModule <- function(e, modKey) {
  a <- as.list(e)[-1L]
  nms <- names(a) %||% rep("", length(a))
  pos <- which(!nzchar(nms))
  m <- if ("module" %in% nms) a[["module"]] else if (length(pos) >= 3L) a[[pos[3L]]] else NULL
  if (is.null(m)) "own" else modKey(m)
}

## The module's whole parameter list: `Par`, `P(sim)`, `params(sim)[[currentModule(sim)]]`
.isWholeParamList <- function(e, module, curModNames = character()) {
  if (identical(e, quote(Par))) return(TRUE)
  modKey <- function(k, dollar = FALSE) .moduleKey(k, module, curModNames, dollar)
  if (.isFnCall(e, "P")) return(is.null(.pParam(e)) && .pModule(e, modKey) == "own")
  .isSelector(e) && .isFnCall(e[[2L]], "params") && .selectorModule(e, modKey) == "own"
}

## The parameters a module function `callee` reads from the whole parameter list passed as
## argument `i` of `call`: NULL unless that argument's formal is only ever used as `x$name` or
## `x[["name"]]` inside `callee`.
.paramListArgUses <- function(callee, call, i) {
  if (!is.function(callee)) return(NULL)
  argName <- names(call)[i + 1L]
  if (is.null(argName) || !nzchar(argName)) {
    mc <- tryCatch(match.call(callee, call, expand.dots = FALSE), error = function(e) NULL)
    if (is.null(mc)) return(NULL)
    hit <- vapply(as.list(mc)[-1L], function(a) identical(a, call[[i + 1L]]), logical(1))
    argName <- names(mc)[-1L][hit]
  } else {
    fmlNames <- names(formals(callee))
    argName <- fmlNames[pmatch(argName, fmlNames)]
  }
  if (length(argName) != 1L || is.na(argName) || argName == "..." ||
      !argName %in% names(formals(callee))) return(NULL)
  sym <- as.name(argName)
  out <- character()
  ok <- TRUE
  walk <- function(e) {
    if (!ok) return()
    if (identical(e, sym)) { ok <<- FALSE; return() }
    if (!is.call(e)) return()
    if (.isSelector(e) && identical(e[[2L]], sym)) {
      p <- .selectedName(e)
      if (is.na(p)) ok <<- FALSE else out <<- c(out, p)
      if (identical(e[[1L]], quote(`[[`))) walk(e[[3L]])
      return()
    }
    if (identical(e[[1L]], quote(`function`))) {
      if (argName %in% names(e[[2L]])) return() # shadowed
    }
    idx <- seq_along(e)[-1L]
    if (identical(e[[1L]], quote(`$`)) || identical(e[[1L]], quote(`@`))) idx <- 2L
    for (j in idx) if (!identical(e[[j]], quote(expr = ))) walk(e[[j]])
  }
  walk(body(callee))
  if (ok) unique(out) else NULL
}

## Names assigned (`<-`, `=`) anywhere in `expr` from a right side for which `test` is TRUE
.assignedNames <- function(expr, test) {
  out <- character()
  walk <- function(e) {
    if (is.call(e)) {
      if ((identical(e[[1L]], quote(`<-`)) || identical(e[[1L]], quote(`=`))) && length(e) == 3L &&
          is.name(e[[2L]]) && isTRUE(test(e[[3L]])))
        out <<- c(out, as.character(e[[2L]]))
      for (i in seq_along(e)[-1L]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
  }
  walk(expr)
  unique(out)
}

## The function itself and every `function(...)` literal in its body, as formals and body
.functionLiterals <- function(fn) {
  out <- list(list(formals = formals(fn), body = body(fn)))
  walk <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], quote(`function`))) {
        out[[length(out) + 1L]] <<- list(formals = e[[2L]], body = e[[3L]])
      }
      for (i in seq_along(e)[-1L]) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
    }
  }
  walk(body(fn))
  out
}

## Apply the scope to one cached event's key parts. `fns` (names in the module environment),
##   `modParamsFull` and `dependsSlots` hold what the full key would use.
## Returns the parts to use: fns, paramsInKey (logical over modParamsFull), dependsSlots, eventCode.
## A parameter `.paramsNotCachedOn()` leaves out stays out of a narrowed key too, even when the
## event's code reads it.
.narrowKeyParts <- function(sim, fnEnv, module, eventType, fns, modParamsFull, dependsSlots,
                            verbose = FALSE) {
  inKey <- !names(modParamsFull) %in% .paramsNotCachedOn(sim, module, eventType)
  out <- list(fns = fns, paramsInKey = inKey, dependsSlots = dependsSlots, eventCode = NULL)
  scope <- .cacheKeyScope(fnEnv, module, eventType, .paramClasses(sim, module))
  if (is.null(scope)) return(out)
  what <- paste0(module, "::", eventType, " cache key")
  if (!isTRUE(scope$narrowed)) {
    if (isTRUE(verbose))
      messageColoured(colour = "blue", what, " uses all parameters and functions: ",
                      paste(scope$reasons, collapse = "; "))
    return(out)
  }
  out$fns <- scope$objects
  out$paramsInKey <- inKey & names(modParamsFull) %in% scope$params
  out$dependsSlots <- setdiff(dependsSlots, c("version", "reqdPkgs"))
  out$eventCode <- scope$eventCode
  if (isTRUE(verbose))
    messageColoured(colour = "blue", what, " narrowed to parameters: ",
                    paste(names(modParamsFull)[out$paramsInKey], collapse = ", "),
                    "; functions: ", paste(scope$objects, collapse = ", "))
  out
}
