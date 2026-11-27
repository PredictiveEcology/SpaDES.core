## A parameter that only labels a run (a run name, a replicate, a file-name suffix) was part of every
## event's cache key, so a new label recomputed data preparation that was already cached (fireSense
## phase 1, 2026-10-19). `.useCacheArgs[[event]]$omitParams` leaves such parameters out of the key,
## and a hit must keep the current label, not the cached one.

.writeOmitParamsModule <- function(modulePath, omit) {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  omitArgs <- if (is.null(omit)) "list()" else
    paste0("list(.inputObjects = list(omitParams = ", deparse(omit), "),
                 init = list(omitParams = ", deparse(omit), "))")
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, 0, 10, "changes the result"),
    defineParameter("runLabel", "character", "a", NA, NA, "only labels the run"),
    defineParameter("hosts", "character", "h", NA, NA, "never changes a result", cacheKey = FALSE),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA, "c"),
    defineParameter(".useCacheArgs", "list", ', omitArgs, ', NA, NA, "x")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("x", "numeric", "x"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim$x <- sim$x + P(sim)$alpha })
  invisible(sim)
}
.inputObjects <- function(sim) {
  sim$x <- 1
  sim
}
')
}

.runOmitParamsModule <- function(modulePath, cachePath, params) {
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1), params = list(m = params),
                                paths = list(modulePath = modulePath, cachePath = cachePath)))
  afterSimInit <- params(s)$m
  s <- suppressMessages(spades(s, debug = FALSE))
  list(afterSimInit = afterSimInit, afterSpades = params(s)$m, x = s$x)
}

.nEntries <- function(cachePath, fn) {
  sc <- reproducible::showCache(cachePath, verbose = -1)
  length(unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == fn]))
}

test_that("omitParams leaves a label parameter out of the event and .inputObjects cache keys", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))

  .writeOmitParamsModule(tmpdir, omit = "runLabel")
  .runOmitParamsModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  expect_identical(.nEntries(tmpCache, "doEvent.m::init"), 1L)
  expect_identical(.nEntries(tmpCache, ".inputObjects_m"), 1L)

  ## a new label: both cached steps hit, and the run keeps its own label
  res <- .runOmitParamsModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nEntries(tmpCache, "doEvent.m::init"), 1L)
  expect_identical(.nEntries(tmpCache, ".inputObjects_m"), 1L)
  expect_identical(res$afterSimInit$runLabel, "rep2")
  expect_identical(res$afterSpades$runLabel, "rep2")
  expect_identical(res$afterSpades$.useCacheArgs$init$omitParams, "runLabel")
  expect_identical(res$x, 2)

  ## a parameter that is not omitted is still part of the key
  res <- .runOmitParamsModule(tmpdir, tmpCache, list(runLabel = "rep3", alpha = 5))
  expect_identical(.nEntries(tmpCache, "doEvent.m::init"), 2L)
  expect_identical(res$x, 6)
  expect_identical(res$afterSpades$runLabel, "rep3")
})

test_that("without omitParams a label parameter is part of the cache key, as before", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))

  .writeOmitParamsModule(tmpdir, omit = NULL)
  .runOmitParamsModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  .runOmitParamsModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nEntries(tmpCache, "doEvent.m::init"), 2L)
  expect_identical(.nEntries(tmpCache, ".inputObjects_m"), 2L)
})

test_that("omitParams is not passed to Cache()", {
  params <- list(.useCacheArgs = list(init = list(omitParams = c("a", "zz"), userTags = "t")), a = 1, b = 2)
  expect_identical(.extraCacheArgsFor(params, "init"), list(userTags = "t"))
  expect_identical(.extraCacheArgsFor(params, "other"), list())
})

test_that("one helper gives an event's not-keyed params: the fixed list, cacheKey = FALSE, and its omitParams", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE))

  .writeOmitParamsModule(tmpdir, omit = "runLabel")
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  ## `init` has omitParams; `hosts` is cacheKey = FALSE for every event
  expect_setequal(.paramsNotCachedOn(s, "m", "init"), c(".useCache", ".useCacheArgs", "hosts", "runLabel"))
  expect_setequal(.paramsNotCachedOn(s, "m", "other"), c(".useCache", ".useCacheArgs", "hosts"))
})
