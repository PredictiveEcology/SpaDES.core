utils::globalVariables(c(".stopBeforeTxt", ".stopAfterTxt", "._stoppedAtTxt"))

## `spades(events = )` has always been a whitelist: name the events to run. That
## cannot express "run whatever the modules schedule, but do not cross this line",
## because in a discrete event simulation the schedule is emergent -- the caller
## cannot enumerate ahead of time everything that ought to run before the line.
## Naming the barrier itself is the only form that does not need the rest, so
## `events` also accepts two reserved entries carrying one.
##
## These are parsed ONCE, in `spades()`, into a small list that is then passed down
## with the event loop. Every `doEvent()` pays only `length(eventsBeforeAfter)` for
## the feature when it is unused, which is the common case and must stay free.
.stopBeforeTxt <- ".stopBefore"
.stopAfterTxt <- ".stopAfter"

## Where a barrier stop is recorded on the simList: dot-prefixed, like the other
## internal state in .xData (._rmo, ._simInitContext, ...), so it is never visible
## as a user object.
._stoppedAtTxt <- "._stoppedAt"

#' Parse `spades(events = )` once into a barrier object and a whitelist
#'
#' @param events The `events` argument of [spades()].
#' @return A list of two elements: `eventsBeforeAfter`, either `NULL` (no barrier
#'   given, so nothing to test per event) or a list with non-`NULL` `before` and/or
#'   `after` specifications; and `events`, the whitelist with the reserved entries
#'   removed, or `NULL` if nothing else was named.
#' @keywords internal
#' @rdname eventStops
.parseEventStops <- function(events) {
  if (is.null(events) || !length(events)) return(list(eventsBeforeAfter = NULL, events = events))
  nms <- names(events)
  ## a bare character vector is a whitelist, exactly as before
  if (is.null(nms)) return(list(eventsBeforeAfter = NULL, events = events))
  ba <- events[intersect(nms, c(.stopBeforeTxt, .stopAfterTxt))]
  if (!length(ba)) return(list(eventsBeforeAfter = NULL, events = events))
  keep <- setdiff(nms, c(.stopBeforeTxt, .stopAfterTxt))
  list(eventsBeforeAfter = list(before = events[[.stopBeforeTxt]], after = events[[.stopAfterTxt]]),
       events = if (length(keep)) events[keep] else NULL)
}

#' Does an event match an event specification?
#'
#' @param event One element of the event queue, i.e. a list with `moduleName` and
#'   `eventType`.
#' @param spec A named list of event types per module, as `events` itself takes, or
#'   a bare character vector meaning those event types in any module.
#' @return logical(1). `FALSE` for an empty or `NULL` `spec`, so a specification
#'   that names nothing stops nothing.
#' @keywords internal
#' @rdname eventStops
.matchesEventSpec <- function(event, spec) {
  if (is.null(spec) || !length(spec) || !length(event)) return(FALSE)
  et <- event[["eventType"]]
  if (is.null(et)) return(FALSE)
  wanted <- if (is.list(spec)) spec[[event[["moduleName"]]]] else spec
  if (is.null(wanted) || !length(wanted)) return(FALSE)
  isTRUE(et %in% wanted)
}

#' Why a `spades()` call returned
#'
#' @description
#' A call stopped by an `events` barrier (see the `events` argument of [spades()])
#' is otherwise hard to tell from one that ran to completion, and for a pass whose
#' whole purpose is to stop before a particular event that ambiguity is the worst
#' possible one. It matters in two directions:
#'
#' * `.stopBefore` leaves the clock and the queue untouched, so calling [spades()]
#'   again with the same barrier stops immediately and returns an equivalent
#'   object. A driver looping "until finished" needs to know that, or it spins.
#' * `.stopAfter` moves the clock past the end time, so the `simList` reports
#'   itself finished while events may still be queued.
#'
#' So rather than have callers parse a message, the stop is recorded on the
#' `simList` and read back with this.
#'
#' @param sim A `simList`.
#' @return `NULL` if the call was not stopped by a barrier; otherwise a list with
#'   `side` (`"before"` or `"after"`), `moduleName`, `eventType`, and `time`: the
#'   barrier event's own scheduled time, in the `simList`'s time units, so it is
#'   comparable with [time()] and [end()]. Note it is not `time(sim)` on the returned
#'   object: reaching a barrier ends the run, which leaves the clock at the end of the
#'   simulation exactly as a completed call would.
#'
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("SpaDES.tools", quietly = TRUE) &&
#'       packageVersion("SpaDES.tools") > "3.0.0") {
#'   opts <- options("spades.moduleCodeChecks" = FALSE, "spades.useRequire" = FALSE)
#'
#'   mkSim <- function() {
#'     simInit(
#'       times = list(start = 0.0, end = 3.0, timeunit = "year"),
#'       params = list(
#'         randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
#'         fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
#'       ),
#'       modules = list("randomLandscapes", "fireSpread"),
#'       paths = list(modulePath = getSampleModules(tempdir()))
#'     )
#'   }
#'
#'   ## a call that ran to completion has nothing to report
#'   stoppedAt(spades(mkSim(), debug = FALSE, .plots = NA))
#'
#'   ## one stopped at a barrier reports which side, which event, and when
#'   out <- spades(mkSim(), debug = FALSE, .plots = NA,
#'                 events = list(.stopBefore = list(fireSpread = "burn")))
#'   stoppedAt(out)
#'
#'   options(opts) # reset options
#' }
#' }
stoppedAt <- function(sim) {
  if (!inherits(sim, "simList")) stop("stoppedAt() needs a simList")
  get0(._stoppedAtTxt, envir = sim@.xData, inherits = FALSE)
}

## Recorded on the simList, using the same dot-prefixed convention as the other
## internal state there (._rmo, ._simInitContext, ...), so it is not visible as a
## user object.
.recordStop <- function(sim, event, side) {
  ## The queue row's moduleName carries the module's full path as its name, and on the
  ## `.stopAfter` side that name survives into the record -- so `stoppedAt()` printed a
  ## named vector with a /tmp path in it. Strip names: this is a value a caller compares
  ## and prints, not a queue row.
  ## The barrier event's OWN scheduled time, in the simList's timeunit.
  ## Not sim@simtimes[["current"]]: that is the internal seconds count, so 2 years
  ## reads as 63115200 and no caller can compare it with end(sim). And not time(sim)
  ## either: on the `before` side the barrier event has been selected but not started,
  ## so the clock still reads the PREVIOUS event's time -- a stop before a `burn` due
  ## at year 1 would report year 0.
  evTime <- convertTimeunit(event[["eventTime"]], sim@simtimes[["timeunit"]], sim@.xData)
  assign(._stoppedAtTxt,
         list(side = side,
              moduleName = unname(as.character(event[["moduleName"]])),
              eventType = unname(as.character(event[["eventType"]])),
              time = evTime),
         envir = sim@.xData)
  sim
}

## Cleared at the start of every spades() call, so a marker can never be read as
## belonging to a later call that ran to completion.
.clearStop <- function(sim) {
  if (exists(._stoppedAtTxt, envir = sim@.xData, inherits = FALSE))
    rm(list = ._stoppedAtTxt, envir = sim@.xData)
  sim
}

## Ending a run mid-stream, by the same route the framework already uses when the
## queue is empty: push the clock past the end time so the `while` in `spades()`
## exits. The event queue is deliberately left alone, so a `.stopBefore` run
## returns a simList whose next event is the one it refused -- which is what makes
## the stopped simulation resumable rather than truncated.
.endRunNow <- function(sim) {
  st <- slot(sim, "simtimes")
  st[["current"]] <- sim@simtimes[["end"]] + 1
  slot(sim, "simtimes", check = FALSE) <- st
  sim
}
