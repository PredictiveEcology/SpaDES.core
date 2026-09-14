## `spades.cacheChaining` substitutes a narrower digest (module source + params)
##   for the full one (which includes the objects) whenever the previous event
##   came out of Cache. The oracle for every test here is therefore the same:
##   chaining is only ever allowed to be *faster*, never to change an answer.
##   So each test runs the identical simulation with the option off and on, and
##   compares.
##
##   That makes the first test a *safety* check: it would also pass if chaining
##   never engaged at all, since the results match either way. Liveness -- that
##   chaining actually fires, and keeps firing for every recorded chain -- is what
##   the third test covers.
##
##   Compare cold-with-cold and warm-with-warm, never cold with warm: recovering
##   an event from the cache does not consume the RNG the way recomputing it
##   does, so a warm run legitimately differs from a cold one in the *un*cached
##   stochastic events. That is true with chaining off as well, and is not what
##   these tests are about.

runChainTest <- function(cachePath, chaining, times, params, modules, modulePath, seed) {
  withr::local_options(spades.cacheChaining = chaining)
  set.seed(seed)
  simInitAndSpades(times = times, params = params, modules = modules,
                   paths = list(modulePath = modulePath, cachePath = cachePath))
}

## Compare only what is deterministic and cheap: the burn statistic accumulated
##   by fireSpread, and the sequence of events actually run.
chainDigestOf <- function(sim) {
  list(npixelsburned = sim$npixelsburned,
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]))
}

test_that("cacheChaining returns the same results as caching without it", {
  skip_on_cran()
  testInit(sampleModReqdPkgs, opts = list())
  modulePath <- getSampleModules(tmpdir)

  times <- list(start = 1.0, end = 2.0)
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects"), nx = 20, ny = 20),
    fireSpread = list(.useCache = c("init", ".inputObjects")),
    caribouMovement = list(.useCache = c("init", ".inputObjects"))
  )

  ## chaining off: populate a cache, then hit it
  cpOff <- file.path(tmpdir, "cacheOff")
  noChain1 <- runChainTest(cpOff, FALSE, times, params, modules, modulePath, 42)
  noChain2 <- runChainTest(cpOff, FALSE, times, params, modules, modulePath, 42)

  ## chaining on: first run records the chain tags, second run can use them
  cpOn <- file.path(tmpdir, "cacheOn")
  chain1 <- runChainTest(cpOn, TRUE, times, params, modules, modulePath, 42)
  chain2 <- runChainTest(cpOn, TRUE, times, params, modules, modulePath, 42)

  expect_equal(chainDigestOf(chain1), chainDigestOf(noChain1)) # cold: cannot chain
  expect_equal(chainDigestOf(chain2), chainDigestOf(noChain2)) # warm: chains
})

## The chain assumes `attr(sim, "tags")` -- the previous cached event's cacheId --
##   still describes `sim`. A module whose `.inputObjects` is NOT cached runs
##   outside Cache and mutates `sim` without touching that tag, so the tag goes
##   stale and the NEXT module chains off a state that no longer exists.
##
## Fixture: modA caches `.inputObjects`; modB's is uncached and writes a fresh
##   random number each run; modC caches and copies modB's value. Note modA must
##   declare an input that is never supplied: `.runModuleInputObjects()` skips the
##   whole phase when `all(inputObjects %in% .userSuppliedObjNames)`, and
##   `all(logical(0))` is TRUE, so a module declaring no inputs never caches
##   `.inputObjects` and cannot seed the chain.
mkChainMod <- function(mp, name, inObjs, outObjs, inputObjectsBody, extraParam = "") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")%s),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim <- sim }); return(invisible(sim))
}

.inputObjects <- function(sim) { %s; return(invisible(sim)) }
', name, name, extraParam, inObjs, outObjs, name, inputObjectsBody), file.path(d, paste0(name, ".R")))
}

test_that("cacheChaining does not chain across an uncached .inputObjects", {
  skip_on_cran()
  testInit("terra", opts = list())

  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkChainMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
             'bindrows(createsOutput("a", "numeric", ""))', "sim$a <- 1")
  mkChainMod(mp, "modB", 'bindrows(expectsInput("a", "numeric", ""))',
             'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- runif(1)")
  mkChainMod(mp, "modC", 'bindrows(expectsInput("b", "numeric", ""))',
             'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b")

  args <- list(times = list(start = 1, end = 1),
               params = list(modA = list(.useCache = ".inputObjects"),
                             modB = list(.useCache = FALSE),  # runs outside Cache
                             modC = list(.useCache = ".inputObjects")),
               modules = list("modA", "modB", "modC"),
               paths = list(modulePath = mp, cachePath = file.path(tmpdir, "cc")))

  withr::local_options(spades.cacheChaining = TRUE)
  set.seed(1); s1 <- do.call(simInit, args)
  ## a different seed => modB writes a different value, so a stale chain hit in
  ##   modC shows up as modC still holding run 1's value
  set.seed(2); s2 <- do.call(simInit, args)

  expect_false(isTRUE(all.equal(s1$b, s2$b)))  # guard: the fixture really varies
  expect_equal(s1$cc, s1$b)
  expect_equal(s2$cc, s2$b)  # fails if modC chained off modA's stale cacheId
})

## Two chains get recorded against ONE cache entry whenever the same upstream
##   state is followed by the same event with different parameters -- i.e. any
##   time the same project is re-run with a downstream parameter changed. Each is
##   stored as six `cacheChaining_<column>_<postCacheId>` tags on that entry, and
##   both have to stay findable.
##
## Asserting on engagement rather than on results is deliberate: a chain that was
##   recorded but cannot be found is a *missed* hit, so the answer is still
##   correct and no result-based oracle can see it. The two branches are
##   structurally identical, so the invariant is that they chain equally --
##   which does not depend on the absolute number of hits.
test_that("cacheChaining finds every chain recorded against one entry", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))

  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkChainMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
             'bindrows(createsOutput("a", "numeric", ""))', "sim$a <- 1")
  mkChainMod(mp, "modB", 'bindrows(expectsInput("a", "numeric", ""))',
             'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- 10")
  mkChainMod(mp, "modC", 'bindrows(expectsInput("b", "numeric", ""))',
             'bindrows(createsOutput("cc", "numeric", ""))',
             "sim$cc <- sim$b + P(sim)$p",
             extraParam = ', defineParameter("p", "numeric", 1, NA, NA, "")')

  cp <- file.path(tmpdir, "cc")
  runIt <- function(p) do.call(simInit, list(
    times = list(start = 1, end = 1),
    params = list(modA = list(.useCache = ".inputObjects"),
                  modB = list(.useCache = ".inputObjects"),
                  modC = list(.useCache = ".inputObjects", p = p)),
    modules = list("modA", "modB", "modC"),
    paths = list(modulePath = mp, cachePath = cp)))

  withr::local_options(spades.cacheChaining = TRUE)
  invisible(runIt(1)); invisible(runIt(2))  # records one chain each on modB's entry

  ## both branches are now recorded; re-running each must chain the same amount
  nChained <- function(p)
    length(grep("Using cacheChaining", capture_messages(invisible(runIt(p)))))
  n1 <- nChained(1)
  n2 <- nChained(2)

  expect_gt(n1, 0)          # liveness: chaining really engages
  expect_equal(n2, n1)      # the second recorded chain is not lost
})

test_that("the chain is recorded even when spades.cacheChaining is off", {
  skip_on_cran()
  testInit(sampleModReqdPkgs, opts = list())
  modulePath <- getSampleModules(tmpdir)

  times <- list(start = 1.0, end = 2.0)
  modules <- list("randomLandscapes", "fireSpread")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects"), nx = 20, ny = 20),
    fireSpread = list(.useCache = c("init", ".inputObjects"))
  )

  ## Recording must not depend on the option. A pass run with chaining off used to
  ## write no tags at all, so a later pass -- the one that would benefit -- had no
  ## chain to follow, and the user had to have known to enable it in advance.
  cp <- file.path(tmpdir, "recordedWhileOff")
  runChainTest(cp, FALSE, times, params, modules, modulePath, 42)

  tags <- showCache(cp, verbose = -1)
  chainTags <- tags[startsWith(tags[["tagKey"]], "cacheChaining")]
  expect_gt(NROW(chainTags), 0L)

  ## And the chain written while off is usable by a later run with it on.
  expect_no_error(runChainTest(cp, TRUE, times, params, modules, modulePath, 42))
})


## A link recorded more than once under one postCacheId -- first by an earlier version whose digest
## differs -- used to stop chaining for good: the reader flattened its recordings into one row (repeated
## names renamed `digestNonObjects.1`, ...) and compared only the oldest, and the writer appended another
## copy on every run. The writer now replaces a link's tags and the reader takes its newest recording.
test_that(".chainSuccessors() reads a link's newest recording, by time", {
  link <- function(digest, when) data.table::data.table(
    cacheId = "prev",
    tagKey = paste0("cacheChaining_", c("prevCache", "digestNonObjects", "module", "event",
                                        "lastEventDetails", "postCacheId"), "_post1"),
    tagValue = c("cacheId:prev", digest, "modC", ".inputObjects", "modB .inputObjects", "post1"),
    createdDate = when)
  ## stored newest first: "newest" has to come from createdDate, not from storage order
  sc <- rbind(link("current", "2026-09-14 11:41:17.604587"), link("stale", "2026-09-14 00:38:03.152215"))
  succ <- .chainSuccessors(sc)
  expect_equal(nrow(succ), 1L)
  expect_equal(succ$digestNonObjects, "current")
  expect_false(any(grepl("[.][0-9]+$", names(succ))))
})

chainLinkTags <- function(cp, successor) {
  sc <- data.table::as.data.table(reproducible::showCache(cp, verbose = -2))
  mod <- sc[startsWith(tagKey, "cacheChaining_module_") & tagValue == successor]
  post <- unique(sub("^cacheChaining_module_", "", mod$tagKey))
  prevId <- unique(mod$cacheId)
  stopifnot(length(post) == 1L, length(prevId) == 1L)
  list(prevId = prevId, post = post,
       tags = sc[cacheId == prevId & startsWith(tagKey, "cacheChaining_") & endsWith(tagKey, paste0("_", post))])
}

test_that("a link recorded twice, stale first, still chains", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkChainMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
             'bindrows(createsOutput("a", "numeric", ""))', "sim$a <- 1")
  mkChainMod(mp, "modB", 'bindrows(expectsInput("a", "numeric", ""))',
             'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- 10")
  mkChainMod(mp, "modC", 'bindrows(expectsInput("b", "numeric", ""))',
             'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b + 1")
  runIt <- function(cp) do.call(simInit, list(
    times = list(start = 1, end = 1),
    params = list(modA = list(.useCache = ".inputObjects"), modB = list(.useCache = ".inputObjects"),
                  modC = list(.useCache = ".inputObjects")),
    modules = list("modA", "modB", "modC"), paths = list(modulePath = mp, cachePath = cp)))
  nChained <- function(cp) length(grep("Using cacheChaining", capture_messages(invisible(runIt(cp)))))
  withr::local_options(spades.cacheChaining = TRUE)

  cpClean <- file.path(tmpdir, "clean"); invisible(runIt(cpClean))
  nClean <- nChained(cpClean)
  expect_gt(nClean, 0)

  ## the state found in a real cache: the modB -> modC link recorded first with a stale digest, then
  ##   again with the current one. The newest recording must be the one read.
  cpDup <- file.path(tmpdir, "dup"); invisible(runIt(cpDup))
  lk <- chainLinkTags(cpDup, "modC")
  reproducible::.updateTagsRepo(lk$prevId, cpDup, paste0("cacheChaining_digestNonObjects_", lk$post), "stale")
  reproducible::.addTagsRepo(lk$prevId, cpDup, tagKey = lk$tags$tagKey, tagValue = lk$tags$tagValue)
  expect_equal(nrow(chainLinkTags(cpDup, "modC")$tags), 12L)
  expect_equal(nChained(cpDup), nClean)

})

## The writer, tested directly: re-running the workflow cannot guarantee the re-recorded link lands on
## the same successor entry, so call cacheChainingPost() twice for one link under one postCacheId.
test_that("cacheChainingPost() replaces a link recorded again instead of appending a second copy", {
  testInit(opts = list())
  cp <- file.path(tmpdir, "cc"); dir.create(cp, showWarnings = FALSE)
  withr::local_options(reproducible.cachePath = cp)
  out <- reproducible::Cache(sum, 1, 2, cachePath = cp)
  prevCache <- attr(out, "tags")
  prevId <- gsub("cacheId:", "", prevCache)
  sim <- simInit(paths = list(cachePath = cp))
  attr(sim, "tags") <- "cacheId:POSTCACHEIDONE"
  record <- function(digest) {
    df <- data.table::data.table(prevCache = prevCache, digestNonObjects = digest, module = "modX",
                                 event = "init", lastEventDetails = "modW init")
    cacheChainingPost(sim, cacheIt = TRUE, prevCache = prevCache, cacheIdOfSkip = NULL, df = df,
                      moduleName = "modX", eventType = "init")
  }
  record("stale")
  record("current")
  sc <- data.table::as.data.table(reproducible::showCache(cp, verbose = -2))
  link <- sc[cacheId == prevId & startsWith(tagKey, "cacheChaining_") & endsWith(tagKey, "_POSTCACHEIDONE")]
  expect_equal(nrow(link), 6L)
  expect_equal(link[tagKey == "cacheChaining_digestNonObjects_POSTCACHEIDONE"]$tagValue, "current")
})
