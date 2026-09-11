## `spades.cacheChaining` lets an event skip straight to the cacheId recorded after the
## previous event, when that previous event and a digest of the module's functions and
## params (`digestNonObjects`) match. A supplied cacheId makes Cache() skip digesting, so
## nothing in `.useCacheArgs` -- e.g. a `.cacheExtra` that exists precisely to change the
## key -- could break the chain: the event kept returning the entry recorded before the
## `.cacheExtra` value changed. Seen in fireSense fits: fireSense_ELFs put a digest of
## `LandR::sppEquivalencies_CA` in `init`'s `.cacheExtra`, and a chained `.inputObjects`
## still sent `init` to the stale entry.

.writeChainExtraModule <- function(modulePath, extra) {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA, "c"),
    defineParameter(".useCacheArgs", "list", list(init = list(.cacheExtra = "', extra, '")), NA, NA, "x")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("x", "numeric", "x"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = {
    ## a side effect a cache hit does not replay: one line per time init really runs
    cat("ran\\n", file = getOption("spades.test.initRuns"), append = TRUE)
    sim$x <- sim$x + 1
  })
  invisible(sim)
}
.inputObjects <- function(sim) {
  sim$x <- 1
  sim
}
')
}

test_that("cacheChaining does not reuse a chained cacheId when .useCacheArgs$.cacheExtra changed", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
                       spades.cacheChaining = TRUE))
  runs <- file.path(tmpdir, "initRuns.txt")
  withr::local_options(spades.test.initRuns = runs)
  nRuns <- function() if (file.exists(runs)) length(readLines(runs)) else 0L
  run <- function() suppressMessages(
    simInitAndSpades(modules = "m", times = list(start = 0, end = 1),
                     paths = list(modulePath = tmpdir, cachePath = tmpCache)))

  .writeChainExtraModule(tmpdir, "tableV1")
  run()                                   # cold: .inputObjects and init computed, chain recorded
  expect_identical(nRuns(), 1L)
  run()                                   # warm, same .cacheExtra: init comes from the cache
  expect_identical(nRuns(), 1L)

  .writeChainExtraModule(tmpdir, "tableV2")
  run()                                   # .cacheExtra changed: init must run again
  expect_identical(nRuns(), 2L)
  run()                                   # and the new entry is reused
  expect_identical(nRuns(), 2L)
})
