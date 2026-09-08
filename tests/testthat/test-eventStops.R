## `events` as a barrier rather than a whitelist. A whitelist cannot express "run
## whatever the modules schedule, but do not cross this line", because the schedule
## of a discrete event simulation is emergent: the caller cannot enumerate ahead of
## time everything that ought to run before the line. These name the line instead.

test_that(".parseEventStops returns nothing to test when no barrier was given", {
  ## the point of returning NULL: the event loop's guard is length(NULL) == 0, so
  ## the feature costs nothing when unused, which is the common case
  expect_null(.parseEventStops(NULL)$eventsBeforeAfter)
  expect_null(.parseEventStops(list())$eventsBeforeAfter)
  expect_null(.parseEventStops(c("init", "burn"))$eventsBeforeAfter)
  expect_equal(.parseEventStops(c("init", "burn"))$events, c("init", "burn"))
  expect_null(.parseEventStops(list(fireSpread = "burn"))$eventsBeforeAfter)
  expect_equal(.parseEventStops(list(fireSpread = "burn"))$events, list(fireSpread = "burn"))
})

test_that(".parseEventStops splits barriers out and leaves the whitelist behind", {
  s <- .parseEventStops(list(.stopBefore = list(fireSpread = "burn")))
  expect_equal(s$eventsBeforeAfter$before, list(fireSpread = "burn"))
  expect_null(s$eventsBeforeAfter$after)
  expect_null(s$events)   # nothing else named => every event runs

  s <- .parseEventStops(list(.stopAfter = list(fireSpread = "burn"),
                             randomLandscapes = "init"))
  expect_equal(s$eventsBeforeAfter$after, list(fireSpread = "burn"))
  expect_null(s$eventsBeforeAfter$before)
  expect_equal(s$events, list(randomLandscapes = "init"))
  ## the reserved names must not survive into the whitelist, or they would be
  ## read as module names
  expect_false(any(c(".stopBefore", ".stopAfter") %in% names(s$events)))

  s <- .parseEventStops(list(.stopBefore = list(a = "x"), .stopAfter = list(b = "y")))
  expect_equal(s$eventsBeforeAfter$before, list(a = "x"))
  expect_equal(s$eventsBeforeAfter$after, list(b = "y"))
})

test_that(".matchesEventSpec matches by module and event, and nothing on an empty spec", {
  ev <- list(moduleName = "fireSpread", eventType = "burn")
  expect_true(.matchesEventSpec(ev, list(fireSpread = "burn")))
  expect_true(.matchesEventSpec(ev, list(fireSpread = c("init", "burn"))))
  expect_true(.matchesEventSpec(ev, "burn"))                      # any module
  expect_false(.matchesEventSpec(ev, list(randomLandscapes = "burn")))  # other module
  expect_false(.matchesEventSpec(ev, list(fireSpread = "init")))   # other event
  expect_false(.matchesEventSpec(ev, NULL))
  expect_false(.matchesEventSpec(ev, list()))
  expect_false(.matchesEventSpec(ev, character(0)))
  expect_false(.matchesEventSpec(list(), list(fireSpread = "burn")))
})

test_that("a barrier stops the run, on the correct side, without a whitelist", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  times <- list(start = 0.0, end = 3, timeunit = "year")
  params <- list(
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .seed = list("init" = 321)),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "fireSpread")
  paths <- list(modulePath = getSampleModules(tmpdir))

  ## reference: everything runs
  full <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA)
  expect_true("burn" %in% completed(full)$eventType)
  expect_gt(sum(completed(full)$eventType == "burn"), 1L)  # it recurs

  ## stop BEFORE the first burn: init events still run, no burn ever does,
  ## and the refused event is still queued so the sim could be resumed
  before <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA,
           events = list(.stopBefore = list(fireSpread = "burn")))
  expect_true("init" %in% completed(before)$eventType)
  expect_true("randomLandscapes" %in% completed(before)$moduleName)
  expect_false("burn" %in% completed(before)$eventType)
  expect_true(any(events(before)$eventType == "burn"))

  ## stop AFTER the first burn: exactly one burn ran
  after <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA,
           events = list(.stopAfter = list(fireSpread = "burn")))
  expect_equal(sum(completed(after)$eventType == "burn"), 1L)
  expect_lt(NROW(completed(after)), NROW(completed(full)))

  ## a barrier naming something that never happens changes nothing
  neverMod <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA,
           events = list(.stopBefore = list(noSuchModule = "burn")))
  expect_equal(NROW(completed(neverMod)), NROW(completed(full)))

  neverEvent <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA,
           events = list(.stopBefore = list(fireSpread = "noSuchEvent")))
  expect_equal(NROW(completed(neverEvent)), NROW(completed(full)))
})

test_that("a barrier and a whitelist in one call both apply", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  times <- list(start = 0.0, end = 3, timeunit = "year")
  params <- list(
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .seed = list("init" = 321)),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "fireSpread")
  paths <- list(modulePath = getSampleModules(tmpdir))

  out <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plots = NA,
           events = list(.stopAfter = list(fireSpread = "burn"),
                         fireSpread = c("init", "burn")))
  ## whitelist: randomLandscapes ran only .inputObjects; barrier: one burn only
  expect_equal(sum(completed(out)$eventType == "burn"), 1L)
  expect_false("stats" %in% completed(out)$eventType)
})

test_that("stoppedAt() distinguishes a barrier stop from a completed run", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  times <- list(start = 0.0, end = 3, timeunit = "year")
  params <- list(
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .seed = list("init" = 321)),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "fireSpread")
  paths <- list(modulePath = getSampleModules(tmpdir))
  mk <- function() simInit(times, params, modules, objects = list(), paths)

  ## a run that finished has nothing to report
  full <- mk() |> spades(debug = FALSE, .plots = NA)
  expect_null(stoppedAt(full))

  ## stopped before: the marker names the side, module, event and time, which is
  ## what a driver needs -- a `.stopBefore` run otherwise looks like any other
  before <- mk() |> spades(debug = FALSE, .plots = NA,
                           events = list(.stopBefore = list(fireSpread = "burn")))
  st <- stoppedAt(before)
  expect_type(st, "list")
  expect_equal(st$side, "before")
  expect_equal(st$moduleName, "fireSpread")
  expect_equal(st$eventType, "burn")
  expect_true(is.numeric(st$time))

  ## stopped after: this is the case that most needs a marker, because the simList
  ## reports itself finished while events remain queued
  after <- mk() |> spades(debug = FALSE, .plots = NA,
                          events = list(.stopAfter = list(fireSpread = "burn")))
  expect_equal(stoppedAt(after)$side, "after")
  expect_gte(time(after), end(after))
  expect_true(NROW(events(after)) > 0)

  ## re-entering with the same barrier stops again rather than progressing, so the
  ## marker is how a driver avoids looping forever
  again <- spades(before, debug = FALSE, .plots = NA,
                  events = list(.stopBefore = list(fireSpread = "burn")))
  expect_equal(stoppedAt(again)$side, "before")
  expect_false("burn" %in% completed(again)$eventType)

  ## and a marker never survives into a call that ran to the end
  resumed <- spades(before, debug = FALSE, .plots = NA)
  expect_null(stoppedAt(resumed))
  expect_true("burn" %in% completed(resumed)$eventType)

  ## it is internal state, not a user object
  expect_false("._stoppedAt" %in% ls(before@.xData))
  expect_false("._stoppedAt" %in% objects(before))
})
