## URL access logging hookup for simInit / spades.
##
## SpaDES wires the simList's environment into `reproducible.urlLog` so any
## prepInputs() / preProcess() call inside simInit or spades is recorded
## against the sim. The records live at envir(sim)$._urlLog$records. The name
## uses the leading-dot-underscore (`._`) convention for volatile SpaDES
## bookkeeping, so it is hidden from default ls() AND ignored by
## all.equal.simList() and other `._`-aware machinery.
##
## Each event dispatch updates envir(sim)$._urlLog$extra with the current
## module + event so reproducible tags every recorded URL access with the
## right context.

## Install the option for the duration of a simInit / spades call. Returns
## a sentinel the caller must hand back to .restoreUrlLog() on exit.
##
## spades.urlLog = FALSE is a hard off-switch: because reproducible's urlLog
## is on by default, we must set reproducible.urlLog = FALSE for the duration
## (otherwise reproducible would still log at the package level). Otherwise we
## point reproducible.urlLog at the sim's hidden ._urlLog env.
.installUrlLog <- function(sim) {
  prev <- getOption("reproducible.urlLog")
  if (!isTRUE(getOption("spades.urlLog", TRUE))) {
    options(reproducible.urlLog = FALSE)
    return(list(prev = prev))
  }
  e <- envir(sim)
  if (is.null(e$._urlLog)) {
    e$._urlLog <- new.env(parent = emptyenv())
    e$._urlLog$records <- list()
    e$._urlLog$seen    <- character()
  }
  options(reproducible.urlLog = e$._urlLog)
  list(prev = prev, log = e$._urlLog)
}

## Restore the option. The module + event left in `extra` by the last dispatched event must not
## tag what reproducible records after the run -- an outer Cache() around spades() writes the
## urls it collected only when it exits -- so they are cleared.
.restoreUrlLog <- function(token) {
  if (is.null(token)) return(invisible())
  if (is.environment(token$log)) token$log$extra <- NULL
  options(reproducible.urlLog = token$prev)
  invisible()
}

## Set envir(sim)$._urlLog$extra to the current module + event so that the
## next URL access recorded via reproducible is tagged accordingly. No-op
## if logging is off or no ._urlLog is installed.
.updateUrlLogExtra <- function(sim) {
  log <- envir(sim)$._urlLog
  if (!is.environment(log)) return(invisible())
  cur <- sim@current
  if (!length(cur)) return(invisible())
  log$extra <- list(
    module = if (!is.null(cur$moduleName)) as.character(cur$moduleName) else NA_character_,
    event  = if (!is.null(cur$eventType))  as.character(cur$eventType)  else NA_character_
  )
  invisible()
}

#' The download ledger of a `simList`
#'
#' A method of [reproducible::urlLog()] for a `simList`. Every
#' [reproducible::prepInputs()] / [reproducible::preProcess()] URL access made during
#' [simInit()] and [spades()] is recorded in the `simList` (unless
#' `options(spades.urlLog = FALSE)`), cached or not, and tagged with the module and event
#' that made it. The ledger is kept by [saveSimList()], [loadSimList()], [Copy()], Cache
#' restores and cacheChaining jumps, and it never enters a cache key. It is the per-run
#' counterpart of the session-wide [reproducible::prepInputsLog()].
#'
#' @param x A `simList`.
#' @param which Character vector of columns to return, in the order given; the same
#'   names as [reproducible::urlLog()]: `"cacheId"`, `"function"`, `"module"`, `"event"`,
#'   `"url"`, `"caller"`, `"firstSeen"`, `"lastSeen"`, `"hitCount"`, `"createdDate"`.
#'   For a `simList`, `"function"` and `"caller"` are both the function that accessed the
#'   url (`prepInputs` or `preProcess`), `"firstSeen"`, `"lastSeen"` and `"createdDate"` are
#'   the time of the access, and `"hitCount"` is `NA`.
#' @param ... Not used.
#'
#' @return A `data.table` with the columns in `which`, one row per access, most recent
#'   first; a zero-row `data.table` with the same columns when nothing was recorded.
#'
#' @seealso [reproducible::urlLog()], [reproducible::prepInputsLog()]
#' @exportS3Method reproducible::urlLog
#' @rdname urlLog
urlLog.simList <- function(x, which = c("function", "module", "url"), ...) {
  allWhich <- c("cacheId", "function", "module", "event", "url", "caller",
                "firstSeen", "lastSeen", "hitCount", "createdDate")
  which <- match.arg(which, allWhich, several.ok = TRUE)
  recs <- envir(x)$._urlLog$records
  chr <- function(nm) vapply(recs, function(r) {
    v <- r[[nm]]
    if (is.null(v) || !length(v)) NA_character_ else paste(as.character(v), collapse = "; ")
  }, character(1))
  tm <- as.POSIXct(chr("time"), format = "%Y-%m-%dT%H:%M:%OS", tz = "")
  out <- data.table::data.table(
    cacheId = chr("cacheId"), `function` = chr("fn"), module = chr("module"),
    event = chr("event"), url = chr("url"), caller = chr("fn"),
    firstSeen = tm, lastSeen = tm, hitCount = rep(NA_integer_, length(recs)),
    createdDate = tm)
  out <- out[order(out$lastSeen, decreasing = TRUE, na.last = TRUE)]
  out[, which, with = FALSE]
}

## Replay the URL tags of one cache entry into the sim's ledger, as reproducible does on a
## Cache() hit, but with the module + event of the entry rather than the current event. A
## cacheChaining jump restores the entries of the events it skips with loadFromCache(),
## which does not replay URL tags, and never dispatches those events, so nothing else
## records their downloads. reproducible exports no replay function: the record and the
## (fn, url, cacheId) de-duplication are reproducible's own (.urlLogRecord(),
## .writeSessionRecord()), so a url already in the ledger is not added twice.
.replayUrlLog <- function(sim, cachePath, cacheId, module, event) {
  log <- envir(sim)$._urlLog
  if (!is.environment(log)) return(invisible())
  sc <- tryCatch(reproducible::showCache(cachePath, cacheId = cacheId, verbose = -2),
                 error = function(e) NULL)
  if (is.null(sc) || NROW(sc) == 0L) return(invisible())
  urls <- reproducible::extractFromCache(sc, "reproducible.url")
  if (!length(urls)) return(invisible())
  fn <- reproducible::extractFromCache(sc, "reproducible.urlFn", ifNot = "prepInputs")[1L]
  mkRecord <- utils::getFromNamespace(".urlLogRecord", "reproducible")
  write <- utils::getFromNamespace(".writeSessionRecord", "reproducible")
  prevOpt <- options(reproducible.urlLog = log)
  prevExtra <- log$extra
  on.exit({
    options(prevOpt)
    log$extra <- prevExtra
  }, add = TRUE)
  log$extra <- list(module = module, event = event)
  for (u in urls)
    write(mkRecord(fn = fn, url = u, cacheId = cacheId))
  invisible()
}

## Add the records of `from` (a ._urlLog environment) that `into` does not have yet. `seen`
## holds one key per record, in record order (reproducible writes them together), so the
## keys are reused instead of rebuilt.
.mergeUrlLog <- function(into, from) {
  if (!is.environment(into) || !is.environment(from) || identical(into, from)) return(invisible())
  if (length(from$seen) != length(from$records)) return(invisible())
  for (i in seq_along(from$records)) {
    if (from$seen[i] %in% into$seen) next
    into$seen <- c(into$seen, from$seen[i])
    into$records[[length(into$records) + 1L]] <- from$records[[i]]
  }
  invisible()
}
