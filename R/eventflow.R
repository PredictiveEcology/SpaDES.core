## Event flow of a module, read from its code: its events, which events schedule
## which and when, and which of them run with the default parameters. The walker
## evaluates nothing itself; every value it uses comes from `.ef_safeEval()`.

## The object a left-hand side assigns into: "sim" with the key for `sim$x` or
## `sim[["x"]]`, otherwise the root name and no key.
.ef_lhsRoot <- function(lhs) {
  while (is.call(lhs) && length(lhs) >= 2) {
    h <- lhs[[1]]
    if ((identical(h, quote(`$`)) || identical(h, quote(`[[`))) &&
        identical(lhs[[2]], quote(sim))) {
      return(list(root = "sim", key = as.character(lhs[[3]])))
    }
    lhs <- lhs[[2]]
  }
  list(root = if (is.name(lhs)) as.character(lhs) else "", key = NULL)
}

## The text values a parameter is compared with anywhere in a condition:
## `param == "a"`, `"a" == param`, `param %in% c("a", "b")`.
.ef_literalComparisons <- function(expr) {
  out <- list()
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- x[[1]]
    if ((identical(h, quote(`==`)) || identical(h, quote(`%in%`))) && length(x) == 3) {
      for (k in list(c(2, 3), c(3, 2))) {
        pn <- .ef_paramRef(x[[k[1]]])
        lit <- x[[k[2]]]
        if (is.call(lit) && identical(lit[[1]], quote(c))) lit <- unlist(as.list(lit)[-1])
        if (!is.null(pn) && is.character(lit)) {
          out[[length(out) + 1L]] <<- list(param = pn, values = lit)
        }
      }
    }
    for (j in seq_along(x)[-1]) {
      if (!.ef_isEmptyArg(x[[j]])) rec(x[[j]])
    }
  }
  rec(expr)
  out
}

## Code under a condition: `value` is FALSE when the condition is false with the
## parameters read, NA when it cannot be known. Assignments under it are not
## evaluated.
.ef_addGuard <- function(st, text, value) {
  st$guards <- c(st$guards, list(list(text = text, value = value)))
  st$exec <- FALSE
  st
}

.ef_isSchedule <- function(e) is.call(e) && identical(e[[1]], quote(scheduleEvent))

.ef_noRecords <- function() {
  data.frame(from = character(), to = character(), toModule = character(),
             priority = numeric(), timeType = character(), timeValue = numeric(),
             timeLabel = character(), timeParams = character(), status = character(),
             cond = character(), guardText = character(), guardParams = character(),
             stringsAsFactors = FALSE)
}

## Record one scheduleEvent() call: what it schedules, when, at what priority,
## and whether it runs ("on"), runs under some settings ("cond") or not ("off")
## given the conditions around it.
.ef_recordSchedule <- function(cl, st) {
  ## arguments that do not match scheduleEvent() leave nothing to read
  m <- tryCatch(match.call(scheduleEvent, cl), error = function(e) NULL)
  if (is.null(m)) return(invisible())
  typ <- .ef_safeEval(m$eventType, st$env)
  ## a missing moduleName is the current module
  modn <- if (is.null(m$moduleName)) st$mod$name else .ef_safeEval(m$moduleName, st$env)
  pr <- if (is.null(m$eventPriority)) .normal() else .ef_safeEval(m$eventPriority, st$env)
  tm <- .ef_classifyTime(m$eventTime, st)
  vals <- vapply(st$guards, function(g) g$value, logical(1))
  gtxt <- vapply(st$guards, function(g) g$text, character(1))
  ## the innermost condition that switches it off, or that is unknown
  status <- if (any(vals %in% FALSE)) "off" else if (anyNA(vals)) "cond" else "on"
  cond <- switch(status,
    off = gtxt[max(which(vals %in% FALSE))],
    cond = gtxt[max(which(is.na(vals)))],
    NA_character_
  )
  ## scheduleEvent() skips an NA time, so a time parameter that defaults to NA is off
  pv <- if (is.list(st$store$pars) && identical(tm$params, tm$label)) st$store$pars[[tm$label]]
  if (status != "off" && tm$type == "at" && length(pv) == 1 && is.atomic(pv) && is.na(pv)) {
    status <- "off"
    cond <- paste(tm$label, "is set")
  }
  out <- st$out
  out$recs[[length(out$recs) + 1L]] <- data.frame(
    from = st$from,
    to = if (is.character(typ) && length(typ) == 1) typ else .ef_deparse(m$eventType),
    toModule = if (is.character(modn) && length(modn) == 1) modn else NA_character_,
    priority = if (is.numeric(pr) && length(pr) == 1) as.numeric(pr) else NA_real_,
    timeType = tm$type,
    timeValue = as.numeric(tm$value),
    timeLabel = tm$label,
    timeParams = paste(tm$params, collapse = ","),
    status = status,
    cond = cond,
    guardText = paste(gtxt, collapse = " & "),
    guardParams = paste(st$gp, collapse = ","),
    stringsAsFactors = FALSE
  )
  invisible()
}

## Walk the code of an event, recording each scheduleEvent() call.
##   `if`: a condition true with the parameters read walks its `then` branch as
##     it is and its `else` branch as off; false, the reverse; unknown, both as
##     conditional.
##   Assignments to local variables are evaluated only on code that runs with
##     the parameters read; each local remembers the parameters behind it.
##   `params(sim)$<module>$x <- value` updates the parameter's value.
##   Loops: the body is walked once and its assignments are not evaluated.
## Anything else is walked into, except function definitions.
.ef_walk <- function(e, st) {
  if (!is.call(e)) return(invisible())
  h <- if (is.name(e[[1]])) as.character(e[[1]]) else ""
  if (h == "{") {
    for (x in as.list(e)[-1]) .ef_walk(x, st)
  } else if (h == "if") {
    v <- .ef_safeEval(e[[2]], st$env)
    v <- if (is.logical(v) && length(v) == 1 && !is.na(v)) v else NA
    txt <- .ef_condText(e[[2]], st)
    ## the parameters behind every enclosing condition, and the text values a
    ## parameter is compared with, reveal run modes (mode == "single" / "multi")
    st$gp <- unique(c(st$gp, .ef_paramsBehind(e[[2]], st)))
    out <- st$out
    for (lc in .ef_literalComparisons(e[[2]])) {
      out$modeLits[[lc$param]] <- unique(c(out$modeLits[[lc$param]], lc$values))
    }
    if (isTRUE(v)) {
      .ef_walk(e[[3]], st)
      if (length(e) == 4) .ef_walk(e[[4]], .ef_addGuard(st, .ef_negateText(txt), FALSE))
    } else if (isFALSE(v)) {
      .ef_walk(e[[3]], .ef_addGuard(st, txt, FALSE))
      if (length(e) == 4) .ef_walk(e[[4]], st)
    } else {
      .ef_walk(e[[3]], .ef_addGuard(st, txt, NA))
      if (length(e) == 4) .ef_walk(e[[4]], .ef_addGuard(st, .ef_negateText(txt), NA))
    }
  } else if (h %in% c("<-", "=")) {
    if (.ef_isSchedule(e[[3]])) return(.ef_recordSchedule(e[[3]], st))
    if (!st$exec) return(invisible())
    lhs <- e[[2]]
    if (is.name(lhs)) {
      nm <- as.character(lhs)
      if (nm == "sim") return(invisible())
      if (.ef_isUnknown(.ef_safeEval(e, st$env))) assign(nm, .ef_unknown, envir = st$env)
      ## the parameters this local is computed from, so a condition on it can name them
      assign(nm, .ef_paramsBehind(e[[3]], st), envir = st$prov)
    } else {
      r <- .ef_lhsRoot(lhs)
      ## of assignments into sim, only parameter updates are evaluated
      if (r$root == "sim" && is.null(r$key) && "params" %in% .ef_calledFns(lhs)) {
        .ef_safeEval(e, st$env)
      }
    }
  } else if (h == "scheduleEvent") {
    .ef_recordSchedule(e, st)
  } else if (h %in% c("for", "while", "repeat")) {
    ## a `for` variable remembers the parameters behind the sequence it runs over
    if (h == "for") assign(as.character(e[[2]]), .ef_paramsBehind(e[[3]], st), envir = st$prov)
    st$exec <- FALSE
    .ef_walk(e[[length(e)]], st)
  } else if (h != "function") {
    for (j in seq_along(e)[-1]) {
      if (!.ef_isEmptyArg(e[[j]])) .ef_walk(e[[j]], st)
    }
  }
  invisible()
}

## The module's own functions that `e` calls, in the order they first appear.
.ef_moduleCalls <- function(e, mod) {
  prefix <- makeEventFn(mod$name, "")
  fns <- names(mod$fns) %||% character()
  skip <- fns == substr(prefix, 1L, nchar(prefix) - 1L) | startsWith(fns, prefix) |
    fns == ".inputObjects"
  intersect(.ef_calledFns(e), fns[!skip])
}

## Walk every event of a module, `init` first, with the parameter values in
## `override` replacing the defaults. Returns the flow, with the text values
## each parameter is compared with (`modeLits`) and the module's default
## parameter values (`defaults`), from which run modes are found.
.ef_walkModule <- function(mod, override = list()) {
  br <- mod$branches
  evs <- c(intersect("init", names(br)), setdiff(names(br), "init"))
  store <- .ef_state(mod, override)
  out <- NULL
  for (ev in evs) {
    st <- .ef_stateFor(mod, ev, store = store, out = out)
    out <- st$out
    for (s in mod$pre) .ef_walk(s, st)
    .ef_walk(br[[ev]], st)
  }
  recs <- if (length(out$recs)) do.call(rbind, out$recs) else .ef_noRecords()
  own <- is.na(recs$toModule) | recs$toModule == mod$name

  ## Event status: init runs; an event runs if something that runs schedules it.
  ## Status only rises (off < cond < on), so this settles within 2 passes per event.
  rank <- c(off = 0, cond = 1, on = 2)
  ## an unscheduled event never runs, so what it schedules is off
  eff <- function(s) ifelse(s == "unscheduled", "off", s)
  stat <- rep("off", length(evs))
  names(stat) <- evs
  stat[intersect("init", evs)] <- "on"
  for (pass in seq_len(2L * length(evs) + 1L)) {
    old <- stat
    for (ev in setdiff(evs, "init")) {
      r <- recs[own & recs$to %in% ev & recs$from != ev, , drop = FALSE]
      if (!nrow(r)) {
        stat[ev] <- "unscheduled"
        next
      }
      s <- pmin(rank[r$status], rank[eff(stat[r$from])])
      stat[ev] <- names(rank)[max(s) + 1]
    }
    if (identical(old, stat)) break
  }

  others <- setdiff(evs, "init")
  ## the records that schedule each event
  recsTo <- lapply(others, function(ev) {
    r <- recs[own & recs$to %in% ev, , drop = FALSE]
    ## a record from another event runs only as far as that event does
    r$effStatus <- ifelse(r$from == ev, r$status,
                          names(rank)[pmin(rank[r$status], rank[eff(stat[r$from])]) + 1])
    if (stat[[ev]] != "off") r <- r[r$effStatus != "off" | r$from == ev, , drop = FALSE]
    r
  })
  names(recsTo) <- others
  ## The condition an event runs under. A record whose scheduler lowers its status
  ## (an unconditional call from a conditional event) takes the scheduler's
  ## condition, or "<scheduler> runs" when the scheduler has none.
  condOf <- function(ev, seen = character()) {
    r <- recsTo[[ev]]
    if (is.null(r) || ev %in% seen) return(NA_character_)
    cs <- r$cond
    for (i in which(r$from != ev & rank[r$effStatus] < rank[r$status])) {
      cs[i] <- condOf(r$from[i], c(seen, ev))
      if (is.na(cs[i])) cs[i] <- paste(r$from[i], "runs")
    }
    cs <- cs[c(which(r$from != ev), which(r$from == ev & r$timeType == "after"))]
    c(cs[!is.na(cs)], NA_character_)[1]
  }
  events <- lapply(others, function(ev) {
    r <- recsTo[[ev]]
    self <- r[r$from == ev & r$timeType == "after", , drop = FALSE]
    first <- r[r$from != ev, , drop = FALSE]
    pr <- c(self$priority, first$priority)
    plotsave <- any(grepl("(^|,)\\.(plot|save)", c(r$timeParams, r$timeLabel))) ||
      any(grepl("\\.plot|\\.save|plotting is on", r$guardText))
    list(
      event = ev,
      status = stat[[ev]],
      priority = if (length(pr)) pr[1] else NA_real_,
      recurring = nrow(self) > 0,
      interval = if (nrow(self)) self$timeValue[1] else NA_real_,
      intervalLabel = if (nrow(self)) self$timeLabel[1] else NA_character_,
      repeatStatus = if (nrow(self)) self$status[1] else NA_character_,
      firsts = first,
      cond = condOf(ev),
      plotsave = plotsave
    )
  })
  names(events) <- others

  flow <- list(
    records = recs,
    events = events,
    status = stat,
    initCalls = .ef_moduleCalls(br[["init"]], mod),
    hasInputObjects = ".inputObjects" %in% names(mod$fns)
  )
  list(flow = flow, modeLits = out$modeLits %||% list(), defaults = store$moduleDefaults)
}

#' Read a module's event flow from its code
#'
#' Reads which events a module has, which of them schedule which, when, and at
#' what priority, from the `scheduleEvent()` calls in each event of
#' `doEvent.<module>` (or in each `doEvent.<module>.<event>` function). No module
#' code is run: an expression is evaluated only if it uses nothing but simple
#' functions without side effects and the module's parameters, which take their
#' default values. Anything else, such as the value of an object in the
#' `simList`, is unknown, and code that depends on it may or may not run.
#'
#' `init` runs. Any other event runs if code that runs schedules it, so each
#' event is `"on"`, `"cond"` (runs only under some settings, or depending on
#' data), `"off"` (does not run with the default parameters), or
#' `"unscheduled"` (no code schedules it).
#'
#' A parameter that the conditions around `scheduleEvent()` calls compare with
#' two or more text values, such as `P(sim)$mode == "multi"`, is a run mode. The
#' module is then also read once with each of those values. Only the first such
#' parameter is used.
#'
#' @param module Character string. The module name.
#' @param path Character string. The directory that contains the module's
#'   directory.
#'
#' @return An object of class `moduleEventFlow`, a list with elements:
#' \describe{
#'   \item{`module`, `path`}{the arguments;}
#'   \item{`read`}{the module's code, parameters and metadata as read;}
#'   \item{`timeunit`}{the module's time unit (`NA` if it cannot be read);}
#'   \item{`modeParam`}{the name of the run-mode parameter, or `NULL`;}
#'   \item{`modeValues`}{its values, the default value first, or `NULL`;}
#'   \item{`modeDefault`}{its default value, or `NULL` (also when the default
#'     cannot be evaluated);}
#'   \item{`default`}{the flow with the default parameters;}
#'   \item{`modes`}{a list with the flow for each of `modeValues`, or `NULL`.}
#' }
#' A flow is a list with elements:
#' \describe{
#'   \item{`records`}{a `data.frame` with one row per `scheduleEvent()` call:
#'     the event it is in (`from`), the event and module it schedules (`to`,
#'     `toModule`), its `priority`, its time (`timeType`: `"after"` the current
#'     time, i.e., recurring, relative to the `"start"` or `"end"`, or `"at"`
#'     another time; `timeValue`, with the simulation starting at 0; `timeLabel`;
#'     and the parameters it uses, `timeParams`), its `status`, the condition
#'     that switches it off or makes it conditional (`cond`), and all the
#'     conditions around it and the parameters behind them (`guardText`,
#'     `guardParams`);}
#'   \item{`events`}{a named list with an entry for each event other than `init`:
#'     its name (`event`), `status` and `priority`; whether it reschedules
#'     itself (`recurring`), at what `interval`, labelled with the parameter or
#'     expression that sets it (`intervalLabel`), and the status of that
#'     rescheduling (`repeatStatus`);
#'     the records that schedule it from other events (`firsts`); the condition
#'     it runs under (`cond`; for an event scheduled from a conditional event,
#'     that event's condition); and whether it is a plotting or saving event
#'     (`plotsave`);}
#'   \item{`status`}{a named character vector, the status of every event;}
#'   \item{`initCalls`}{the module's functions that `init` calls, in order;}
#'   \item{`hasInputObjects`}{whether the module has an `.inputObjects` function.}
#' }
#'
#' @export
#' @examples
#' \donttest{
#' tmpdir <- file.path(tempdir(), "moduleEventFlowExample")
#' newModule("myModule", tmpdir, open = FALSE)
#' ef <- moduleEventFlow("myModule", tmpdir)
#' ef$default$status
#' unlink(tmpdir, recursive = TRUE)
#' }
moduleEventFlow <- function(module, path) {
  if (!is.character(module) || length(module) != 1 || is.na(module)) {
    stop("module must be a single module name")
  }
  mod <- .ef_readModule(module, path)
  w <- .ef_walkModule(mod)
  ## a run mode: a parameter compared with two or more text values in the
  ## conditions around scheduleEvent() calls
  gp <- unique(unlist(strsplit(w$flow$records$guardParams, ",", fixed = TRUE)))
  lits <- Filter(function(v) length(v) >= 2, w$modeLits[intersect(names(w$modeLits), gp)])
  modeParam <- modeValues <- modeDefault <- modes <- NULL
  if (length(lits)) {
    modeParam <- names(lits)[1]
    modeDefault <- w$defaults[[modeParam]]
    ## a default that cannot be evaluated is not reported
    if (.ef_isUnknown(modeDefault)) modeDefault <- NULL
    first <- if (is.character(modeDefault)) intersect(modeDefault, lits[[modeParam]])
    modeValues <- unique(c(first, lits[[modeParam]]))
    modes <- lapply(modeValues, function(v) {
      .ef_walkModule(mod, override = stats::setNames(list(v), modeParam))$flow
    })
    names(modes) <- modeValues
  }
  structure(
    list(module = module, path = path, read = mod, timeunit = mod$meta$timeunit,
         modeParam = modeParam, modeValues = modeValues, modeDefault = modeDefault,
         default = w$flow, modes = modes),
    class = "moduleEventFlow"
  )
}

## ---- timing as text --------------------------------------------------------

.ef_fmtNum <- function(x) format(x, trim = TRUE, drop0trailing = TRUE)

.ef_spanText <- function(v, tu) paste(.ef_fmtNum(v), if (v == 1) tu else paste0(tu, "s"))

.ef_isParamLabel <- function(lbl) grepl("^[.A-Za-z][.A-Za-z0-9_]*$", lbl)

## "every 10 years (successionTimestep)"
.ef_intervalText <- function(event, tu) {
  if (!isTRUE(event$recurring)) return(NA_character_)
  lbl <- event$intervalLabel
  if (is.na(event$interval)) return(paste("every", lbl))
  paste0("every ", if (event$interval == 1) tu else .ef_spanText(event$interval, tu),
         if (.ef_isParamLabel(lbl)) paste0(" (", lbl, ")"))
}

## When an event of a flow runs, as text: "every year (fireTimestep); first at
## start + 1 year (burnInitialTime)". A parameter that sets both the interval
## and the first time is named once.
.ef_timingText <- function(event, timeunit) {
  tu <- if (length(timeunit) == 1 && !is.na(timeunit)) timeunit else "time unit"
  parts <- character()
  if (isTRUE(event$recurring)) parts <- .ef_intervalText(event, tu)
  f <- event$firsts
  if (NROW(f)) {
    ft <- unique(vapply(seq_len(nrow(f)), function(i) {
      r <- f[i, ]
      p <- if (nzchar(r$timeParams) && !identical(r$timeParams, event$intervalLabel)) {
        paste0(" (", gsub(",", ", ", r$timeParams, fixed = TRUE), ")")
      } else {
        ""
      }
      switch(r$timeType,
        start = if (isTRUE(r$timeValue == 0)) {
          paste0("at start", p)
        } else {
          paste0("at start + ", if (is.na(r$timeValue)) "?" else .ef_spanText(r$timeValue, tu), p)
        },
        end = "at end",
        at = if (!is.na(r$timeValue)) {
          paste0("at time ", .ef_fmtNum(r$timeValue), p)
        } else {
          paste0("at ", r$timeLabel)
        },
        paste0("after ", r$from)
      )
    }, character(1)))
    if (isTRUE(event$recurring)) {
      firstOnly <- setdiff(ft, "at end")
      parts <- c(parts,
                 if (length(firstOnly)) paste("first", paste(firstOnly, collapse = " and ")),
                 if ("at end" %in% ft) "and at end")
    } else {
      parts <- c(parts, paste("once,", paste(ft, collapse = " and ")))
    }
  }
  if (identical(event$repeatStatus, "cond")) parts <- c(parts, "repeats only under some settings")
  if (identical(event$status, "cond") && length(event$cond) == 1 && !is.na(event$cond)) {
    parts <- c(parts, paste("only if", event$cond))
  }
  paste(parts, collapse = "; ")
}
