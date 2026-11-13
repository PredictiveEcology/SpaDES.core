## Evaluation of the expressions in a module's event code for event-flow
## documentation. Only expressions built from an allowlist of pure functions are
## evaluated, in a sandbox that knows the module's default parameters; anything
## else, and anything that reads simulation data, gives `.ef_unknown`.

## Sentinel for a value that a static read cannot know.
.ef_unknown <- structure(list(), class = "ef_unknown")
.ef_isUnknown <- function(x) inherits(x, "ef_unknown")

.ef_allowed <- c(
  "{", "(", "!", "&&", "||", "&", "|", "==", "!=", "<", ">", "<=", ">=",
  "+", "-", "*", "/", "^", "%%", "%/%", "%in%",
  "c", "list", "is.na", "is.null", "isTRUE", "isFALSE", "any", "all", "length",
  "nchar", "nzchar", "identical", "max", "min", "sum", "seq", "seq_len", "rep",
  "paste", "paste0", "names", "setNames", "$", "[[", "[", "if",
  "is.numeric", "is.character", "is.logical", "inherits", "as.numeric",
  "as.integer", "as.character", "unlist", "grepl", "tolower", "toupper",
  "ifelse", "abs", "round", "floor", "ceiling", "is.data.frame", "nrow", "NROW",
  "<-", "=",
  "P", "params", "start", "end", "time", "currentModule", "anyPlotting",
  ".first", ".last", ".normal", ".highest", ".lowest"
)

## Names of the functions called anywhere in `e`.
.ef_calledFns <- function(e) {
  out <- character()
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- x[[1]]
    if (is.name(h)) out <<- c(out, as.character(h)) else rec(h)
    for (j in seq_along(x)[-1]) {
      if (!.ef_isEmptyArg(x[[j]])) rec(x[[j]])
    }
  }
  rec(e)
  unique(out)
}

## `sim$x` and `sim[["x"]]` are simulation data, which a static read cannot know.
.ef_rewriteSimAccess <- function(e) {
  if (!is.call(e)) return(e)
  h <- e[[1]]
  if ((identical(h, quote(`$`)) || identical(h, quote(`[[`))) && identical(e[[2]], quote(sim))) {
    return(call(".simobj", as.character(e[[3]])))
  }
  for (j in seq_along(e)[-1]) {
    if (!.ef_isEmptyArg(e[[j]])) {
      r <- .ef_rewriteSimAccess(e[[j]])
      if (is.null(r)) e[j] <- list(NULL) else e[[j]] <- r
    }
  }
  e
}

## Evaluate `e` in `env` if it calls only allowed functions; otherwise, or if the
## evaluation fails or warns, return `.ef_unknown`.
.ef_safeEval <- function(expr, env) {
  if (!all(.ef_calledFns(expr) %in% .ef_allowed)) return(.ef_unknown)
  tryCatch(
    eval(.ef_rewriteSimAccess(expr), env),
    error = function(err) .ef_unknown,
    warning = function(w) .ef_unknown
  )
}

## The environment that expressions are evaluated in. `store` holds the module's
## current parameter values, the simulation start/end and the current time.
.ef_makeSandbox <- function(modName, store) {
  sb <- new.env(parent = baseenv())
  sb$sim <- structure(list(), class = "fakesim")
  sb$P <- function(sim, param, module) {
    if (missing(param)) store$pars else store$pars[[param]]
  }
  sb$params <- function(sim) stats::setNames(list(store$pars), modName)
  sb$`params<-` <- function(sim, value) {
    store$pars <- value[[modName]]
    nms <- names(store$defaults)
    changed <- vapply(nms, function(n) !identical(store$pars[[n]], store$defaults[[n]]), logical(1))
    store$rewritten <- union(store$rewritten, nms[changed])
    sim
  }
  sb$start <- function(sim, unit) store$START
  sb$end <- function(sim, unit) store$END
  sb$time <- function(sim, unit) store$NOW
  sb$currentModule <- function(sim) modName
  ## the real priority and plotting helpers, so the sandbox cannot drift from them
  sb$anyPlotting <- anyPlotting
  sb$.first <- .first
  sb$.last <- .last
  sb$.normal <- .normal
  sb$.highest <- .highest
  sb$.lowest <- .lowest
  sb$.simobj <- function(name) stop("simulation data")
  makeActiveBinding("Par", function() store$pars, sb)
  sb
}

## The state that expressions are evaluated against: an environment with the
## module's parameter values (`pars`, `defaults`, `rewritten`), the simulation
## start, end and current time, and the `sandbox`. `override` replaces defaults.
.ef_state <- function(mod, override = list()) {
  store <- new.env()
  store$START <- 0
  store$END <- Inf
  store$NOW <- 0
  store$rewritten <- character()
  store$pars <- list()
  sb <- .ef_makeSandbox(mod$name, store)
  for (p in names(mod$meta$params)) {
    d <- mod$meta$params[[p]]$expr
    v <- if (is.null(d)) NULL else .ef_safeEval(d, sb)
    if (is.null(v)) store$pars[p] <- list(NULL) else store$pars[[p]] <- v
  }
  store$moduleDefaults <- store$pars
  for (p in names(override)) {
    store$pars[[p]] <- override[[p]]
  }
  store$defaults <- store$pars
  store$sandbox <- sb
  store
}

## The walker state for one event branch of `mod`: `init` runs at the start, any
## other event is read as if mid-run (after start, before end).
.ef_stateFor <- function(mod, ev = "init", override = list(), store = .ef_state(mod, override),
                         out = NULL) {
  env <- new.env(parent = store$sandbox)
  env$eventType <- ev
  env$eventTime <- .ef_unknown
  env$debug <- FALSE
  store$NOW <- if (ev == "init") 0 else 1
  if (is.null(out)) {
    out <- new.env()
    out$recs <- list()
    out$modeLits <- list()
  }
  list(env = env, store = store, mod = mod, guards = list(), exec = TRUE, from = ev,
       out = out, prov = new.env(), gp = character())
}

## ---- parameters and times as symbols ---------------------------------------

## The parameter that `P(sim)$x`, `Par$x`, `params(sim)$mod$x` or `P(sim, "x")`
## names, or NULL.
.ef_paramRef <- function(expr) {
  if (!is.call(expr)) return(NULL)
  h <- expr[[1]]
  if (identical(h, quote(`$`)) || identical(h, quote(`[[`))) {
    obj <- expr[[2]]
    key <- expr[[3]]
    kn <- if (is.name(key)) as.character(key) else if (is.character(key)) key else NULL
    if (is.null(kn)) return(NULL)
    if (is.call(obj) && identical(obj[[1]], quote(P))) return(kn)
    if (identical(obj, quote(Par))) return(kn)
    if (is.call(obj) &&
        (identical(obj[[1]], quote(`$`)) || identical(obj[[1]], quote(`[[`))) &&
        is.call(obj[[2]]) && identical(obj[[2]][[1]], quote(params))) {
      return(kn)
    }
  }
  if (identical(h, quote(P)) && length(expr) >= 3 && is.character(expr[[3]])) return(expr[[3]])
  NULL
}

.ef_timeSyms <- c(start = ".START", end = ".END", time = ".NOW")

## Replace start/end/time(sim) by .START/.END/.NOW and parameters by their names.
## A parameter whose default is itself relative to the start or end
## (`start(sim) + 1`) is replaced by that default, unless the code changed it.
.ef_symbolize <- function(e, st, expand = TRUE) {
  pn <- .ef_paramRef(e)
  if (!is.null(pn)) {
    d <- st$mod$meta$params[[pn]]$expr
    if (expand && !is.null(d) && !(pn %in% st$store$rewritten)) {
      ds <- .ef_symbolize(d, st)$expr
      if (any(.ef_timeSyms %in% all.vars(ds))) return(list(expr = call("(", ds), params = pn))
    }
    return(list(expr = as.name(pn), params = pn))
  }
  if (is.call(e)) {
    h <- e[[1]]
    if (is.name(h) && as.character(h) %in% names(.ef_timeSyms) &&
        length(e) >= 2 && identical(e[[2]], quote(sim))) {
      return(list(expr = as.name(.ef_timeSyms[[as.character(h)]]), params = character()))
    }
    used <- character()
    for (j in seq_along(e)[-1]) {
      if (!.ef_isEmptyArg(e[[j]])) {
        r <- .ef_symbolize(e[[j]], st, expand)
        if (is.null(r$expr)) e[j] <- list(NULL) else e[[j]] <- r$expr
        used <- c(used, r$params)
      }
    }
    return(list(expr = e, params = unique(used)))
  }
  list(expr = e, params = character())
}

.ef_deparse <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")

## `.NOW + x` or `x + .NOW` -> `x`
.ef_dropNow <- function(e) {
  if (is.call(e) && identical(e[[1]], quote(`+`)) && length(e) == 3) {
    if (identical(e[[2]], quote(.NOW))) return(e[[3]])
    if (identical(e[[3]], quote(.NOW))) return(e[[2]])
  }
  e
}

## Classify the time of a scheduleEvent() call: "after" (relative to now, i.e.
## recurring), "end" or "start" (relative to the simulation end / start), or
## "at" (anything else). `value` is the time in an evaluation with start, end
## and now all 0, or NA when it cannot be known.
.ef_classifyTime <- function(expr, st) {
  ## a local variable or loop variable: report the parameters behind it
  if (is.name(expr) && exists(as.character(expr), envir = st$prov, inherits = FALSE)) {
    ps <- get(as.character(expr), envir = st$prov)
    lbl <- if (length(ps)) paste("times set by", paste(ps, collapse = ", ")) else "times set in the code"
    return(list(type = "at", value = NA_real_, params = character(), label = lbl))
  }
  s <- .ef_symbolize(expr, st)
  vars <- all.vars(s$expr)
  ev <- new.env(parent = st$env)
  ev$.START <- 0
  ev$.END <- 0
  ev$.NOW <- 0
  for (p in s$params) {
    assign(p, st$store$pars[[p]] %||% .ef_unknown, envir = ev)
  }
  val <- .ef_safeEval(s$expr, ev)
  val <- if (is.numeric(val) && length(val) == 1) val else NA_real_
  type <- if (".NOW" %in% vars) "after"
  else if (".END" %in% vars) "end"
  else if (".START" %in% vars) "start"
  else "at"
  list(
    type = type,
    value = val,
    params = s$params,
    label = .ef_deparse(if (type == "after") .ef_dropNow(s$expr) else s$expr)
  )
}

## ---- condition text -------------------------------------------------------

## Parameters named in `e`.
.ef_paramsIn <- function(expr) {
  out <- character()
  rec <- function(x) {
    pn <- .ef_paramRef(x)
    if (!is.null(pn)) {
      out <<- c(out, pn)
    } else if (is.call(x)) {
      for (j in seq_along(x)[-1]) {
        if (!.ef_isEmptyArg(x[[j]])) rec(x[[j]])
      }
    }
  }
  rec(expr)
  unique(out)
}

## Parameters named in `e`, and those behind the local variables it uses.
.ef_paramsBehind <- function(e, st) {
  locals <- intersect(all.vars(e), ls(st$prov, all.names = TRUE))
  unique(c(.ef_paramsIn(e), unlist(mget(locals, envir = st$prov))))
}

.ef_negateText <- function(txt) {
  if (startsWith(txt, "depends on")) return(txt)
  if (endsWith(txt, " is set")) return(sub(" is set$", " is not set", txt))
  if (endsWith(txt, " is not set")) return(sub(" is not set$", " is set", txt))
  paste0("!(", txt, ")")
}

## A condition as short text that reads like a setting.
.ef_condText <- function(expr, st) {
  ## a condition on a local variable is reported by the parameters behind it
  locals <- intersect(all.vars(expr), ls(st$prov, all.names = TRUE))
  if (length(locals)) {
    ps <- .ef_paramsBehind(expr, st)
    if (length(ps)) return(paste("depends on", paste(ps, collapse = ", ")))
  }
  txt <- .ef_deparse(do.call(
    substitute,
    list(
      .ef_symbolize(expr, st, expand = FALSE)$expr,
      list(.NOW = quote(time), .START = quote(start), .END = quote(end))
    )
  ))
  txt <- gsub("anyPlotting\\(\\.plots\\)", "plotting is on (.plots)", txt)
  txt <- sub("^!is\\.(na|null)\\(([.A-Za-z0-9_]+)\\)$", "\\2 is set", txt)
  txt <- sub("^is\\.(na|null)\\(([.A-Za-z0-9_]+)\\)$", "\\2 is not set", txt)
  if (nchar(txt) > 70) paste0(substr(txt, 1, 67), "...") else txt
}
