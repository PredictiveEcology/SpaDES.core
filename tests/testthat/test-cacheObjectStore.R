## options(spades.cacheObjectStore = TRUE): an event's (or `.inputObjects`') cache entry keeps only
## the objects its event changed, each in its own content-addressed file under
## `<cachePath>/objects/`, and a hit reads an object only when it is used (R/cacheObjectStore.R).
## The oracle is the uncached run: the store may only ever save space and time, never change an
## answer. Every test runs with memoise off and on (jumpTest(), helper-cacheChaining.R).

storeOpts <- function(jumpOpts, store = TRUE) c(jumpOpts, list(spades.cacheObjectStore = store))

objStoreFiles <- function(cp) dir(file.path(cp, .objStoreDir))

entryId <- function(cp, fn) {
  sc <- reproducible::showCache(cp, verbose = -2)
  unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == fn])
}

entryRefs <- function(cp, id) {
  sc <- reproducible::showCache(cp, verbose = -2)
  sc$tagValue[sc$cacheId == id & sc$tagKey == .objStoreTagKey]
}

## os1: a cached `init` that makes two objects and a `mod` object, then a cached `step` that
##   changes one small object only. `unused` is never read by any event.
mkObjStoreMod <- function(mp) {
  d <- file.path(mp, "os1")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "os1", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(os1 = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list("data.table"),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(),
  outputObjects = bindrows(createsOutput("big", "numeric", ""), createsOutput("dt", "data.table", ""),
                           createsOutput("unused", "numeric", ""), createsOutput("small", "numeric", ""),
                           createsOutput("fromBig", "numeric", ""))))

doEvent.os1 <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      sim$big <- sin(seq_len(2e4))
      sim$dt <- data.table::data.table(a = 1:3, b = letters[1:3])
      sim$unused <- cos(seq_len(2e4))
      mod$m <- 1:10
      sim <- scheduleEvent(sim, time(sim) + 1, "os1", "step")
    },
    step = { sim$small <- time(sim) + sum(mod$m) },
    useBig = { sim$fromBig <- sum(sim$big) })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', file.path(d, "os1.R"))
}

runOS <- function(mp, cp, useCache = c("init", "step")) {
  simInitAndSpades(times = list(start = 1, end = 2), params = list(os1 = list(.useCache = useCache)),
                   modules = list("os1"), paths = list(modulePath = mp, cachePath = cp))
}

osState <- function(sim) {
  list(objs = mget(c("big", "dt", "unused", "small"), envir = sim@.xData),
       mod = sim@.xData$.modObjs$os1$m,
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]))
}

jumpTest("object store: a hit restores what an uncached run makes; an entry keeps only what its event changed", {
  skip_on_cran()
  testInit(c("terra", "data.table"), opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkObjStoreMod(mp)

  ref <- runOS(mp, file.path(tmpdir, "noCache"), useCache = NA)
  cp <- file.path(tmpdir, "cache")
  s1 <- runOS(mp, cp)
  s2 <- runOS(mp, cp)
  expect_equal(osState(s1), osState(ref))
  expect_equal(osState(s2), osState(ref))

  ## init changed big, dt, unused and mod$m. The step entry holds small and, as `changed` counts
  ##   every createsOutput outside the event's key, big, dt and unused too: the same files as init's.
  initId <- entryId(cp, "doEvent.os1::init")
  stepId <- entryId(cp, "doEvent.os1::step")
  expect_length(entryRefs(cp, initId), 4L)
  expect_length(entryRefs(cp, stepId), 4L)
  expect_length(objStoreFiles(cp), 5L)
  expect_setequal(objStoreFiles(cp), union(entryRefs(cp, initId), entryRefs(cp, stepId)))
  ## the entry holds references, not the objects
  e <- reproducible::loadFromCache(cp, stepId, verbose = -2)
  expect_true(all(vapply(c("big", "dt", "unused", "small"), .objStoreIsPending, logical(1),
                         envir = e@.xData)))
  expect_equal(e$small, ref$small)
})

jumpTest("object store: off (the default), no object files are written", {
  skip_on_cran()
  testInit(c("terra", "data.table"), opts = storeOpts(jumpOpts, store = FALSE))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkObjStoreMod(mp)
  cp <- file.path(tmpdir, "cache")
  s1 <- runOS(mp, cp)
  expect_length(objStoreFiles(cp), 0L)
  expect_length(entryRefs(cp, entryId(cp, "doEvent.os1::init")), 0L)
})

jumpTest("object store: two entries with an identical changed object share one file", {
  skip_on_cran()
  testInit("terra", opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkJumpMod(mp, "dA", "bindrows()", 'bindrows(createsOutput("pa", "numeric", ""))',
            "sim$pa <- sin(seq_len(1e4))")
  mkJumpMod(mp, "dB", "bindrows()", 'bindrows(createsOutput("pb", "numeric", ""))',
            "sim$pb <- sin(seq_len(1e4))")
  cp <- file.path(tmpdir, "cache")
  s1 <- simInitAndSpades(times = list(start = 1, end = 1),
                         params = list(dA = list(.useCache = "init"), dB = list(.useCache = "init")),
                         modules = list("dA", "dB"), paths = list(modulePath = mp, cachePath = cp))
  refA <- entryRefs(cp, entryId(cp, "doEvent.dA::init"))
  refB <- entryRefs(cp, entryId(cp, "doEvent.dB::init"))
  expect_length(refA, 1L)
  expect_identical(refA, refB)
  expect_identical(objStoreFiles(cp), refA)
  expect_identical(s1$pa, s1$pb)
})

jumpTest("object store: a hit reads an object only when it is used", {
  skip_on_cran()
  testInit(c("terra", "data.table"), opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkObjStoreMod(mp)
  cp <- file.path(tmpdir, "cache")
  ref <- runOS(mp, cp)

  ## take away the file of `unused`: a run that hits the entry but never reads `unused` still works
  initId <- entryId(cp, "doEvent.os1::init")
  e <- reproducible::loadFromCache(cp, initId, verbose = -2)
  h <- .objStoreHandle("unused", e@.xData)
  expect_true(is.environment(h))
  unlink(file.path(cp, .objStoreDir, h$key))

  s2 <- runOS(mp, cp)
  expect_true(.objStoreIsPending("unused", s2@.xData))
  expect_equal(mget(c("big", "dt", "small"), envir = s2@.xData), mget(c("big", "dt", "small"), envir = ref@.xData))
  expect_equal(s2@.xData$.modObjs$os1$m, ref@.xData$.modObjs$os1$m)
  expect_error(s2$unused, "unused")
})

## jA anchors the chain (a plain hit); jB (which also makes a `mod` object) and jC are skipped by
##   one jump.
mkStoreJumpFixture <- function(mp) {
  mkJumpMod(mp, "jA", "bindrows()", 'bindrows(createsOutput("a", "numeric", ""))',
            "sim$a <- sin(seq_len(1e4))")
  mkJumpMod(mp, "jB", 'bindrows(expectsInput("a", "numeric", ""))',
            'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- sim$a + 1; mod$x <- 5")
  mkJumpMod(mp, "jC", 'bindrows(expectsInput("b", "numeric", ""))',
            'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b * 2")
}

runStoreJump <- function(mp, cp, modules = list("jA", "jB", "jC"), chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  p <- list(jA = list(.useCache = "init"), jB = list(.useCache = "init"), jC = list(.useCache = "init"))
  simInitAndSpades(times = list(start = 1, end = 2), params = p[unlist(modules)], modules = modules,
                   paths = list(modulePath = mp, cachePath = cp))
}

storeJumpState <- function(sim) {
  list(objs = mget(c("a", "b", "cc", "grown_jA", "grown_jB", "grown_jC"), envir = sim@.xData),
       modX = sim@.xData$.modObjs$jB$x,
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]))
}

jumpTest("object store: a cacheChaining jump over entries in the new format equals the uncached run", {
  skip_on_cran()
  testInit("terra", opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkStoreJumpFixture(mp)
  withr::local_options(spades.cacheObjectStore = FALSE)
  ref <- runStoreJump(mp, file.path(tmpdir, "ref"), chaining = FALSE)
  withr::local_options(spades.cacheObjectStore = TRUE)

  cp <- file.path(tmpdir, "cache")
  s1 <- runStoreJump(mp, cp)
  warm <- jumpMessages(s2 <- runStoreJump(mp, cp))
  expect_length(warm$jumps, 1L)
  expect_match(warm$jumps, "restored 2 cached events in one step")
  expect_equal(storeJumpState(s1), storeJumpState(ref))
  expect_equal(storeJumpState(s2), storeJumpState(ref))
})

jumpTest("object store: old-format and new-format entries mix, in hits and in a jump", {
  skip_on_cran()
  testInit("terra", opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkStoreJumpFixture(mp)
  withr::local_options(spades.cacheObjectStore = FALSE)
  ref <- runStoreJump(mp, file.path(tmpdir, "ref"), chaining = FALSE)

  cp <- file.path(tmpdir, "cache")
  runStoreJump(mp, cp) # old format
  expect_length(objStoreFiles(cp), 0L)
  oldIds <- c(entryId(cp, "doEvent.jA::init"), entryId(cp, "doEvent.jB::init"))
  reproducible::clearCache(cp, cacheId = entryId(cp, "doEvent.jC::init"), ask = FALSE, verbose = -2)

  withr::local_options(spades.cacheObjectStore = TRUE)
  s2 <- runStoreJump(mp, cp) # jA and jB from their old entries; jC runs and is saved in the new format
  expect_identical(c(entryId(cp, "doEvent.jA::init"), entryId(cp, "doEvent.jB::init")), oldIds)
  expect_length(entryRefs(cp, oldIds[2]), 0L)
  expect_length(entryRefs(cp, entryId(cp, "doEvent.jC::init")), 1L)
  warm <- jumpMessages(s3 <- runStoreJump(mp, cp)) # jumps over old jB and new jC
  expect_length(warm$jumps, 1L)
  expect_match(warm$jumps, "restored 2 cached events in one step")
  expect_equal(storeJumpState(s2), storeJumpState(ref))
  expect_equal(storeJumpState(s3), storeJumpState(ref))
})

jumpTest("object store: a file-backed SpatRaster stays with its entry and comes back", {
  skip_on_cran()
  testInit("terra", opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  op <- file.path(tmpdir, "out"); dir.create(op, showWarnings = FALSE)
  mkJumpMod(mp, "fr", "bindrows()",
            'bindrows(createsOutput("r", "SpatRaster", ""), createsOutput("v", "numeric", ""))',
            paste0('sim$r <- terra::writeRaster(terra::rast(nrows = 5, ncols = 5, vals = 7), ',
                   'file.path(outputPath(sim), "fr.tif"), overwrite = TRUE); sim$v <- 1:5'))
  cp <- file.path(tmpdir, "cache")
  run <- function() simInitAndSpades(times = list(start = 1, end = 1),
                                     params = list(fr = list(.useCache = "init")), modules = "fr",
                                     paths = list(modulePath = mp, cachePath = cp, outputPath = op))
  s1 <- run()
  s2 <- run()
  ## only `v` is in the object store; the raster is file-backed and stays a part of the entry
  expect_length(entryRefs(cp, entryId(cp, "doEvent.fr::init")), 1L)
  expect_length(objStoreFiles(cp), 1L)
  expect_true(all(terra::values(s2$r) == 7))
  expect_true(all(nzchar(terra::sources(s2$r))))
  expect_identical(s2$v, 1:5)
})

jumpTest("object store: clearCacheObjectStore() deletes only the files no entry refers to", {
  skip_on_cran()
  testInit(c("terra", "data.table"), opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkObjStoreMod(mp)
  cp <- file.path(tmpdir, "cache")
  ref <- runOS(mp, cp)
  stepId <- entryId(cp, "doEvent.os1::step")
  stepOnly <- setdiff(entryRefs(cp, stepId), entryRefs(cp, entryId(cp, "doEvent.os1::init")))
  expect_length(stepOnly, 1L) # `small`; the others are init's files too
  before <- objStoreFiles(cp)

  ## nothing is orphaned yet; a young orphan is kept (another process may be saving its entry)
  expect_length(clearCacheObjectStore(cp, minAge = 0), 0L)
  reproducible::clearCache(cp, cacheId = stepId, ask = FALSE, verbose = -2)
  expect_identical(objStoreFiles(cp), before) # clearCache() leaves the object files
  expect_length(clearCacheObjectStore(cp), 0L) # default minAge: just written, so kept
  gone <- clearCacheObjectStore(cp, minAge = 0)
  expect_identical(basename(gone), stepOnly)
  expect_setequal(objStoreFiles(cp), setdiff(before, stepOnly))

  s2 <- runOS(mp, cp) # init hits; step runs again
  expect_equal(osState(s2), osState(ref))
})

jumpTest("object store: a digest and saveSimList() read the objects a hit left unread", {
  skip_on_cran()
  testInit(c("terra", "data.table"), opts = storeOpts(jumpOpts))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkObjStoreMod(mp)
  cp <- file.path(tmpdir, "cache")
  s1 <- runOS(mp, cp)
  s2 <- runOS(mp, cp)
  expect_true(.objStoreIsPending("unused", s2@.xData))
  expect_identical(.robustDigest(s2, .objects = c("big", "dt", "unused")),
                   .robustDigest(s1, .objects = c("big", "dt", "unused")))

  s3 <- runOS(mp, cp)
  f <- file.path(tmpdir, "s3.qs2")
  saveSimList(s3, f, files = FALSE)
  unlink(cp, recursive = TRUE) # the saved simList does not need the cache
  s4 <- loadSimList(f)
  expect_equal(mget(c("big", "dt", "unused", "small"), envir = s4@.xData),
               mget(c("big", "dt", "unused", "small"), envir = s1@.xData))
})
