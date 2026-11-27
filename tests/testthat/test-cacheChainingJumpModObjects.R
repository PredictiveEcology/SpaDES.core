## A jump walks the recorded chain before the skipped events are restored, rebuilding each
## event's `digestNonObjects` from the live simList (.chainNonObjects()). That digest took in
## the NAMES of the module's `mod` objects -- as NA-named NULL placeholders, since `mod` objects
## do not live in the function environment the names were looked up in. A skipped event of the
## same module that creates a `mod` object therefore changed the digest of every later event of
## that module: the walk stopped there, and the run paid a second jump (setup, tag reads, input
## digests) for the rest. Seen in FireSense: fireSense_dataPrepFit prepIgnitionFitData sets
## `mod$allYears`, so NRV_summary init's jump stopped at it and prepEscapeFitData started another.

## jA init (cached) is the anchor, a plain hit. jM has three cached events; `init` creates a
## `mod` object the other two read.
mkModObjFixture <- function(mp) {
  mkJumpMod(mp, "jA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
            'bindrows(createsOutput("a", "numeric", ""))', "sim$a <- 1")
  d <- file.path(mp, "jM")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "jM", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(jM = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(expectsInput("a", "numeric", "")),
  outputObjects = bindrows(createsOutput("m1", "numeric", ""), createsOutput("m2", "numeric", ""),
                           createsOutput("m3", "numeric", ""))))

doEvent.jM <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = { mod$x <- 1; sim$m1 <- sim$a + 1; sim <- scheduleEvent(sim, time(sim), "jM", "second") },
    second = { sim$m2 <- sim$m1 + mod$x; sim <- scheduleEvent(sim, time(sim), "jM", "third") },
    third = { sim$m3 <- sim$m2 + mod$x })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', file.path(d, "jM.R"))
}

runModObj <- function(mp, cp) {
  withr::local_options(spades.cacheChaining = TRUE)
  simInitAndSpades(times = list(start = 1, end = 1),
                   params = list(jA = list(.useCache = "init"),
                                 jM = list(.useCache = c("init", "second", "third"))),
                   modules = list("jA", "jM"), paths = list(modulePath = mp, cachePath = cp))
}

jumpTest("a skipped event that creates a mod object does not stop the jump at its module", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkModObjFixture(mp)
  cp <- file.path(tmpdir, "on")

  cold <- jumpMessages(s1 <- runModObj(mp, cp))
  expect_length(cold$jumps, 0L)
  expect_equal(c(s1$m1, s1$m2, s1$m3), c(2, 3, 4))
  ## jA init is the first cached event of the run (nothing to chain from): its entry still records
  ## its queue delta and the mod objects it changed, as every new entry does
  sc <- reproducible::showCache(cp, verbose = -1)
  jaId <- unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == "doEvent.jA::init"])
  expect_length(jaId, 1L)
  expect_true(any(sc$cacheId == jaId & sc$tagKey == "eventQueueDelta"))
  jmId <- unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == "doEvent.jM::init"])
  expect_equal(sc$tagValue[sc$cacheId == jmId & sc$tagKey == "eventModObjectsChanged"], "modChg1\tx")

  warm <- jumpMessages(s2 <- runModObj(mp, cp))
  ## jA init is a plain hit; jM init chains from it and the walk reaches second and third:
  ## one jump of three events, not jM init alone ("Using cacheChaining ...") and then a jump of two
  expect_length(warm$jumps, 1L)
  expect_match(warm$jumps, "restored 3 cached events in one step")
  expect_length(warm$chains, 0L)
  expect_equal(c(s2$m1, s2$m2, s2$m3), c(2, 3, 4))
  ## the mod object the skipped init made is in the live sim after the jump
  expect_equal(s2@.xData[[".modObjs"]][["jM"]][["x"]], 1)
})
