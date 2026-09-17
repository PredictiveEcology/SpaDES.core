## When a cached event is recovered, `.prepareOutput()` merges the queue the cached run had with the
## queue the live run has, so that events the cached event scheduled are not lost. The merge must not
## duplicate an event that is already in the live queue.
##
## It did. `R/cache.R` stamped a sort key onto each event -- `order <- 2` on the cached events,
## `order <- 1` on the live ones -- BEFORE calling `unique()`, and stripped the field again only AFTER
## sorting. An event present in both queues therefore was not identical at the moment `unique()` ran,
## survived as a duplicate, and re-emerged with the field removed: two byte-identical events.
##
## The consequence compounds, because the event queue is part of an event's cacheId. A warm hit on
## event N returns a queue with one duplicated event, so event N+1 digests a different `sim.events`,
## gets a different cacheId, and misses -- which is how a warm cache turns itself cold.
##
## Measured on a FireSense cache (2026-09-16): of the cache entries that were identical in every
## digest component except the event queue, 17 of 17 differed by exactly one event, and in every case
## that event was a duplicate of one already queued -- never a genuinely new event. Affected modules
## included burnSummaries, Biomass_summary, Biomass_speciesData, Biomass_speciesParameters,
## NRV_summary, fireSense_dataPrepFit, fireSense_dataPrepPredict, fireSense_IgnitionFit and
## fireSense_IgnitionPredict, i.e. it was not specific to any one module.
##
## The merge is exercised directly here rather than through a full cached `spades()` run: the failure
## is entirely in the list arithmetic, and a unit test pins it without a simList fixture.

ev <- function(time, type, mod, priority = 5) {
  list(eventTime = time, eventType = type, moduleName = mod, eventPriority = priority)
}

test_that("an event in BOTH queues appears once, not twice", {
  shared <- ev(10, "spreadFitPrepare", "fireSense_SpreadFit")
  onlyCached <- ev(11, "somethingElse", "fireSense_SpreadFit")

  out <- .mergeEventQueues(cached = list(shared, onlyCached), live = list(shared))

  expect_length(out, 2L)
  nShared <- sum(vapply(out, function(e) identical(e$eventType, "spreadFitPrepare"), logical(1)))
  expect_identical(nShared, 1L)
})

test_that("the merged queue carries no leftover sort key", {
  ## `order` is an implementation detail of the sort. If it survives into the returned events it
  ## becomes part of the next event's digest, which is the very problem being fixed.
  shared <- ev(10, "spreadFitPrepare", "fireSense_SpreadFit")
  out <- .mergeEventQueues(cached = list(shared), live = list(shared))
  expect_true(all(vapply(out, function(e) is.null(e[["order"]]), logical(1))))
})

test_that("events the cached run scheduled are kept", {
  ## The reason the merge exists at all: an event scheduled by the cached event must survive.
  live <- list(ev(10, "init", "modA"))
  cached <- list(ev(10, "init", "modA"), ev(12, "scheduledByInit", "modA"))
  out <- .mergeEventQueues(cached = cached, live = live)
  expect_length(out, 2L)
  expect_true(any(vapply(out, function(e) identical(e$eventType, "scheduledByInit"), logical(1))))
})

test_that("the result is ordered by eventTime then eventPriority", {
  live <- list(ev(20, "late", "modA", priority = 5))
  cached <- list(ev(20, "late", "modA", priority = 5),
                 ev(5,  "early", "modA", priority = 5),
                 ev(5,  "earlyHighPriority", "modA", priority = 1))
  out <- .mergeEventQueues(cached = cached, live = live)
  expect_identical(vapply(out, function(e) e$eventType, character(1)),
                   c("earlyHighPriority", "early", "late"))
})

test_that("empty queues are handled", {
  one <- ev(1, "init", "modA")
  expect_length(.mergeEventQueues(cached = list(), live = list(one)), 1L)
  expect_length(.mergeEventQueues(cached = list(one), live = list()), 1L)
  expect_length(.mergeEventQueues(cached = list(), live = list()), 0L)
})
