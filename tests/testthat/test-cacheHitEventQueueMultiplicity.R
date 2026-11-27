## On a plain (non-chaining) event cache hit, `.prepareOutput()` (R/cache.R) rebuilt the event queue
## by merging the queue stored in the entry with the live one and calling `unique()` on the union
## (`.mergeEventQueues()`). That collapsed every pair of byte-identical events in the LIVE queue,
## including a duplicate the run had really scheduled (a module calling scheduleEvent() twice with the
## same time, priority and type: fireSense_spreadFit init, burnSummaries init, NRV_summary init). The
## queue is part of every later event's cacheId, so after the hit every later event missed and was
## re-saved with the collapsed queue; the next identical run collapsed one step further, and a fully
## cached run was reached only after one run per such hit. The doubled event also ran once instead
## of twice.
##
## A hit now replays the entry's own `eventQueueDelta` (what the event added to and removed from the
## queue when it ran) onto the live queue, exactly as a cacheChaining jump does (.chainReadDelta(),
## .chainApplyDelta()). An entry saved without the tag falls back to `.mergeEventQueues()`, which now
## removes only as many copies of a cached event as the live queue already holds.

## `dup`: init schedules `work` TWICE at start(sim), then `after`, which schedules `later` at start + 1
## (so the queue at the end of the run is not empty). `obs`: a cached init that runs while both `work`
## events are still queued -- the hit whose merge collapsed them.
mkDupMods <- function(mp) {
  d <- file.path(mp, "dup"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "dup", description = "", keywords = "",
  authors = person("A", "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(dup = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", c("init", "work", "after"), NA, NA, "")),
  inputObjects = bindrows(expectsInput("x", "numeric", "")),
  outputObjects = bindrows(createsOutput("x", "numeric", ""), createsOutput("n", "numeric", ""))))

doEvent.dup <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      sim$n <- 0
      sim <- scheduleEvent(sim, start(sim), "dup", "work")
      sim <- scheduleEvent(sim, start(sim), "dup", "work")
      sim <- scheduleEvent(sim, start(sim), "dup", "after", .last())
    },
    work = { sim$n <- sim$n + 1 },
    after = {
      sim$x <- sim$n * 10
      sim <- scheduleEvent(sim, start(sim) + 1, "dup", "later")
    })
  sim
}

.inputObjects <- function(sim) { if (!suppliedElsewhere("x", sim)) sim$x <- 0; sim }
', file.path(d, "dup.R"))

  d <- file.path(mp, "obs"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "obs", description = "", keywords = "",
  authors = person("A", "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(obs = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", "init", NA, NA, "")),
  inputObjects = bindrows(expectsInput("x", "numeric", "")),
  outputObjects = bindrows(createsOutput("y", "numeric", ""))))

doEvent.obs <- function(sim, eventTime, eventType) { switch(eventType, init = { sim$y <- 1 }); sim }

.inputObjects <- function(sim) sim
', file.path(d, "obs.R"))
}

## `once`: init schedules `X` at start(sim) with the lowest priority, then `mid` (cached) before it;
## `mid` schedules `Y`. When `mid` is a hit, `X` is in the live queue AND in the queue stored in
## `mid`'s entry, but `mid` did not add it: the case #457 fixed, a hit must not duplicate `X`.
mkOnceMod <- function(mp) {
  d <- file.path(mp, "once"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "once", description = "", keywords = "",
  authors = person("A", "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(once = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", "mid", NA, NA, "")),
  inputObjects = bindrows(expectsInput("x", "numeric", "")),
  outputObjects = bindrows(createsOutput("nX", "numeric", ""), createsOutput("nY", "numeric", ""))))

doEvent.once <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      sim$nX <- 0; sim$nY <- 0
      sim <- scheduleEvent(sim, start(sim), "once", "X", .last())
      sim <- scheduleEvent(sim, start(sim), "once", "mid")
    },
    mid = { sim <- scheduleEvent(sim, start(sim), "once", "Y", .last()) },
    X = { sim$nX <- sim$nX + 1 },
    Y = { sim$nY <- sim$nY + 1 })
  sim
}

.inputObjects <- function(sim) { if (!suppliedElsewhere("x", sim)) sim$x <- 0; sim }
', file.path(d, "once.R"))
}

runMods <- function(mp, cp, modules) {
  simInitAndSpades(times = list(start = 0, end = 0), modules = as.list(modules),
                   paths = list(modulePath = mp, cachePath = cp, outputPath = file.path(dirname(cp), "out")))
}

cacheIds <- function(cp) unique(reproducible::showCache(cp, verbose = -1)$cacheId)

## completed() rows are compared without eventPriority: a cacheChaining jump records the landing
## event's priority on every event it skipped (appendCompleted() in R/simulation-spades.R)
evFrame <- function(x) as.data.frame(x)[, c("eventTime", "moduleName", "eventType")]
nType <- function(sim, type) sum(completed(sim)$eventType == type)

for (chaining in c(FALSE, TRUE)) for (reachable in c(FALSE, TRUE)) {
  test_that(sprintf("a rerun keeps a duplicate the run scheduled [cacheChaining %s, cacheKeyReachable %s]",
                    chaining, reachable), {
    skip_on_cran()
    testInit(opts = list(spades.cacheChaining = chaining, spades.cacheKeyReachable = reachable,
                         reproducible.useMemoise = FALSE))
    mp <- file.path(tmpdir, "mods"); mkDupMods(mp)
    cp <- file.path(tmpdir, "cc")

    s1 <- runMods(mp, cp, c("dup", "obs"))
    expect_identical(nType(s1, "work"), 2L)
    expect_equal(s1$n, 2)
    ids1 <- cacheIds(cp)

    ## the identical rerun: every event is a hit, nothing is re-saved
    s2 <- runMods(mp, cp, c("dup", "obs"))
    expect_identical(cacheIds(cp), ids1)
    ## and the run is the same run: `work` twice, the same completed and remaining events
    expect_identical(nType(s2, "work"), 2L)
    expect_equal(s2$n, 2)
    expect_identical(evFrame(completed(s2)), evFrame(completed(s1)))
    expect_identical(evFrame(events(s2)), evFrame(events(s1)))
    expect_identical(s2@events, s1@events)
  })
}

test_that("a hit does not duplicate an event that was already queued (the #457 case)", {
  skip_on_cran()
  testInit(opts = list(reproducible.useMemoise = FALSE))
  mp <- file.path(tmpdir, "mods"); mkOnceMod(mp)
  cp <- file.path(tmpdir, "cc")

  s1 <- runMods(mp, cp, "once")
  expect_identical(c(nType(s1, "X"), nType(s1, "Y")), c(1L, 1L))
  ## `mid`'s entry records that it added only `Y`; `X` was already queued and is not in its delta
  sc <- reproducible::showCache(cp, verbose = -1)
  midId <- unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == "doEvent.once::mid"])
  expect_length(midId, 1L)
  delta <- sc$tagValue[sc$cacheId == midId & sc$tagKey == "eventQueueDelta"]
  expect_match(delta, "\\+\t[^\n]*\tonce\tY")
  expect_no_match(delta, "\\+\t[^\n]*\tonce\tX")

  s2 <- runMods(mp, cp, "once") # `mid` is a hit while `X` is in the live queue
  expect_identical(c(nType(s2, "X"), nType(s2, "Y")), c(1L, 1L))
  expect_identical(evFrame(completed(s2)), evFrame(completed(s1)))
})

test_that("an entry without a recorded queue delta merges by multiplicity", {
  skip_on_cran()
  testInit(opts = list(reproducible.useMemoise = FALSE))
  mp <- file.path(tmpdir, "mods"); mkDupMods(mp)
  cp <- file.path(tmpdir, "cc")

  s1 <- runMods(mp, cp, c("dup", "obs"))
  ids1 <- cacheIds(cp)
  ## entries saved before the tag existed have no delta to replay: `.chainReadDelta()` reads an
  ##   absent tag and one without its format marker alike, so overwrite every entry's tag
  for (id in ids1)
    reproducible::.updateTagsRepo(cacheId = id, cachePath = cp, tagKey = "eventQueueDelta", tagValue = "none")

  s2 <- runMods(mp, cp, c("dup", "obs"))
  expect_identical(cacheIds(cp), ids1)
  expect_identical(nType(s2, "work"), 2L)
  expect_identical(evFrame(completed(s2)), evFrame(completed(s1)))
  expect_identical(s2@events, s1@events)
})
