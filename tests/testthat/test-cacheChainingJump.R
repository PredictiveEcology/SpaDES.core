## cacheChaining follows a recorded chain one link at a time. With several consecutive cached
## events it can follow the chain to its end and recover the final state in one step
## (R/cacheChainingJump.R). The oracle is the same as in test-cacheChaining.R: a jump may only
## ever be faster, never change an answer. Liveness -- that the jump really engages -- is
## asserted through its message, because a jump that never engages still gives right answers.

## Four modules, each with a cached `init` that writes its own output from the previous module's,
## then schedules a `grow` event (so the queue after the chain is not trivial). modB also reads
## `ext`, an object the user supplies at simInit: the "external input" the jump must re-check.
mkJumpMod <- function(mp, name, inObjs, outObjs, initBody, useCache = c(".inputObjects", "init")) {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = { %s; sim <- scheduleEvent(sim, time(sim) + 1, "%s", "grow") },
    grow = { sim$grown_%s <- time(sim) })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', name, name, inObjs, outObjs, name, initBody, name, name), file.path(d, paste0(name, ".R")))
  invisible(name)
}

jumpFixture <- function(mp) {
  mkJumpMod(mp, "jA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
            'bindrows(createsOutput("a", "numeric", ""), createsOutput("shared", "numeric", ""))',
            "sim$a <- 1; sim$shared <- 100")
  mkJumpMod(mp, "jB", 'bindrows(expectsInput("a", "numeric", ""), expectsInput("ext", "numeric", ""))',
            'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- sim$a + sim$ext")
  mkJumpMod(mp, "jC", 'bindrows(expectsInput("b", "numeric", ""))',
            'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b * 2")
  mkJumpMod(mp, "jD", 'bindrows(expectsInput("cc", "numeric", ""), expectsInput("shared", "numeric", ""))',
            'bindrows(createsOutput("d", "numeric", ""), createsOutput("shared", "numeric", ""))',
            "sim$d <- sim$cc + 1; sim$shared <- 200")
}

jumpParams <- function(...) {
  p <- list(jA = list(.useCache = "init"), jB = list(.useCache = "init"),
            jC = list(.useCache = "init"), jD = list(.useCache = "init"))
  modifyList(p, list(...))
}

## `spades.debug = TRUE` + `reproducible.verbose = 1` so the chaining messages are emitted
## (as in test-cacheChaining.R's liveness test); `debug` is therefore not passed here.
jumpOpts <- list(reproducible.useMemoise = FALSE, spades.debug = TRUE, reproducible.verbose = 1)

runJump <- function(mp, cp, params, ext = 5, chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  simInitAndSpades(times = list(start = 1, end = 2), params = params, objects = list(ext = ext),
                   modules = list("jA", "jB", "jC", "jD"),
                   paths = list(modulePath = mp, cachePath = cp))
}

grownOf <- function(sim) unname(unlist(mget(paste0("grown_", c("jA", "jB", "jC", "jD")), envir = sim@.xData)))

jumpMessages <- function(expr) {
  m <- capture_messages(expr)
  list(jumps = grep("skipping ahead over", m, value = TRUE),
       chains = grep("Using cacheChaining", m, value = TRUE))
}

stateOf <- function(sim) {
  list(objs = mget(c("a", "b", "cc", "d", "shared", "grown_jA", "grown_jB", "grown_jC", "grown_jD"),
                   envir = sim@.xData, ifnotfound = list(NULL)),
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]),
       time = time(sim))
}

test_that("a run of cached events is recovered in one jump, with the same result", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)

  ## reference: caching without chaining, cold then warm
  cpOff <- file.path(tmpdir, "off")
  runJump(mp, cpOff, jumpParams(), chaining = FALSE)
  ref <- runJump(mp, cpOff, jumpParams(), chaining = FALSE)

  cp <- file.path(tmpdir, "on")
  cold <- jumpMessages(s1 <- runJump(mp, cp, jumpParams()))
  expect_length(cold$jumps, 0L) # nothing recorded yet

  warm <- jumpMessages(s2 <- runJump(mp, cp, jumpParams()))
  ## jA init is a genuine hit (first cached event of the run, nothing to chain from);
  ## jB init chains, and from there the walk reaches jC and jD in one step.
  expect_length(warm$jumps, 1L)
  expect_match(warm$jumps, "over 2 cached events to jD init")

  ## every answer identical to caching without chaining
  expect_equal(stateOf(s2)$objs, stateOf(ref)$objs)
  ## the skipped events are still reported as completed, in order
  expect_equal(stateOf(s2)$completed, stateOf(ref)$completed)
  ## objects from skipped entries: b from jB, cc from jC; `shared` is jD's (200), not jA's (100)
  expect_equal(s2$b, 6); expect_equal(s2$cc, 12); expect_equal(s2$d, 13); expect_equal(s2$shared, 200)
  ## the queue after the jump was the recorded one: every module's `grow` still ran
  expect_equal(grownOf(s2), rep(2, 4))
})

test_that("a novel object supplied at simInit stops the jump at the module that reads it", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")

  runJump(mp, cp, jumpParams(), ext = 5)
  runJump(mp, cp, jumpParams(), ext = 5) # records + uses the full chain

  ## `ext` is read by jB only. jA init still hits; jB's chain link is recorded, but its external
  ## input differs, so nothing may be skipped past jB -- it has to compute with the new value.
  got <- jumpMessages(s <- runJump(mp, cp, jumpParams(), ext = 50))
  expect_length(got$jumps, 0L)
  expect_equal(s$b, 51); expect_equal(s$cc, 102); expect_equal(s$d, 103)

  ## and now that chain is recorded too: the same call jumps, to the same answer
  again <- jumpMessages(s3 <- runJump(mp, cp, jumpParams(), ext = 50))
  expect_length(again$jumps, 1L)
  expect_equal(stateOf(s3)$objs, stateOf(s)$objs)
})

test_that("an uncached event in the middle ends the jump there", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")
  params <- jumpParams(jC = list(.useCache = FALSE)) # jC init always runs

  runJump(mp, cp, params)
  got <- jumpMessages(s <- runJump(mp, cp, params))
  ## jB can chain from jA but there is no cached jC to walk on to; jD's chain starts after jC.
  expect_length(got$jumps, 0L)
  expect_equal(s$d, 13)
  expect_equal(s$shared, 200)
})

test_that("a deleted entry in the chain shortens the jump instead of breaking the run", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")

  runJump(mp, cp, jumpParams())
  runJump(mp, cp, jumpParams())
  sc <- showCache(cp, verbose = -2)
  idD <- unique(sc[tagKey == "function" & grepl("^doEvent.jD", tagValue)]$cacheId)
  clearCache(cp, cacheId = idD, ask = FALSE)

  got <- jumpMessages(s <- runJump(mp, cp, jumpParams()))
  expect_length(got$jumps, 1L)
  expect_match(got$jumps, "over 1 cached event to jC init")
  expect_equal(s$d, 13) # jD ran and its result is right
  expect_equal(s$shared, 200)
})
