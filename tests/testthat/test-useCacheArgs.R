## Helper: capture messages from a quiet spades() run, then filter out known
## baseline channels that are not gated by `debug` (Require/pak chatter, the
## simInit setDTthreads notice, and the spades saveSimOnExit notice). Anything
## remaining is a real surprise. The actual unknown content is embedded in
## the failure info so a CI or interactive run shows exactly what leaked
## through (different installs of reproducible, cli, etc. can occasionally
## emit a baseline message we hadn't anticipated).
.expectNoUnknownMessages <- function(expr) {
  knownPatterns <- c(
    "skipping new package dependency",
    "No packages to install/update",
    "Using setDTthreads",
    "simList saved in",
    "savedSimEnv\\(\\)\\$\\.sim",
    "deleted at next spades",
    ## reproducible / cli baseline noise sometimes seen in interactive
    ## sessions when `reproducible.verbose = 0` does not gate every line:
    "Cache cleared",
    "Cleared cache",
    "Caching",
    "Loading cached",
    "Setting cache path",
    "memoised copy",
    "cached copy",
    ## reproducible::useDBI() emits this at verboseLevel=0 (i.e., even when
    ## reproducible.verbose=0) when DBI/RSQLite isn't available or disabled.
    "Using DBI backend",
    "Using non-DBI backend"
  )
  mess <- testthat::capture_messages(utils::capture.output(force(expr)))
  unknown <- mess[!Reduce("|", lapply(knownPatterns, function(p) grepl(p, mess)))]
  testthat::expect_equal(
    unknown, character(0),
    info = paste0("spades(debug=FALSE) emitted unexpected messages:\n",
                  paste0("  ", trimws(unknown), collapse = "\n"))
  )
}

test_that(".useCacheArgs splices a fixed cacheId into the per-event Cache call", {
  skip_on_cran()

  testInit(opts = list(reproducible.useMemoise = FALSE,
                       reproducible.verbose     = 0,
                       spades.saveSimOnExit     = FALSE))

  modName <- "testUseCacheArgs"
  suppressMessages(newModule(modName, path = tmpdir, open = FALSE))

  fixedKey <- "testUseCacheArgs_initEvent_v1"

  args <- list(
    modules = list(modName),
    paths   = list(modulePath = tmpdir, cachePath = tmpCache),
    params  = stats::setNames(
      list(list(.useCache     = "init",
                .useCacheArgs = list(init = list(cacheId = fixedKey)))),
      modName
    )
  )

  try(reproducible::clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  mySim <- suppressMessages(do.call(simInit, args))

  .expectNoUnknownMessages(spades(mySim, debug = FALSE, .plotInitialTime = NA))

  cache <- reproducible::showCache(tmpCache, cacheId = fixedKey, verbose = -1)
  expect_true(NROW(cache) > 0,
              info = "Expected a cache entry under the developer-supplied cacheId")
})

test_that(".useCacheArgs absent falls through to default per-event Cache args", {
  skip_on_cran()

  testInit(opts = list(reproducible.useMemoise = FALSE,
                       reproducible.verbose     = 0,
                       spades.saveSimOnExit     = FALSE))

  modName <- "testUseCacheArgsFallthrough"
  suppressMessages(newModule(modName, path = tmpdir, open = FALSE))

  args <- list(
    modules = list(modName),
    paths   = list(modulePath = tmpdir, cachePath = tmpCache),
    params  = stats::setNames(
      list(list(.useCache = "init")),  # no .useCacheArgs at all
      modName
    )
  )

  try(reproducible::clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  mySim <- suppressMessages(do.call(simInit, args))

  .expectNoUnknownMessages(spades(mySim, debug = FALSE, .plotInitialTime = NA))

  cache <- reproducible::showCache(tmpCache,
                                   userTags = "eventType:init",
                                   verbose  = -1)
  expect_true(NROW(cache) > 0,
              info = "Default per-event Cache call should still record an init entry")
})

test_that(".useCacheArgs splices arbitrary Cache args (userTags) per event", {
  skip_on_cran()

  testInit(opts = list(reproducible.useMemoise = FALSE,
                       reproducible.verbose     = 0,
                       spades.saveSimOnExit     = FALSE))

  modName <- "testUseCacheArgsTags"
  suppressMessages(newModule(modName, path = tmpdir, open = FALSE))

  customTag <- "myCustomVersionTag:abc123"

  args <- list(
    modules = list(modName),
    paths   = list(modulePath = tmpdir, cachePath = tmpCache),
    params  = stats::setNames(
      list(list(.useCache     = "init",
                .useCacheArgs = list(init = list(userTags = customTag)))),
      modName
    )
  )

  try(reproducible::clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  mySim <- suppressMessages(do.call(simInit, args))

  .expectNoUnknownMessages(spades(mySim, debug = FALSE, .plotInitialTime = NA))

  cache <- reproducible::showCache(tmpCache, userTags = customTag, verbose = -1)
  expect_true(NROW(cache) > 0,
              info = "Caller-supplied userTags should override the default per-event userTags")
})

test_that(".useCacheArgs evaluates quoted entries at the splice site", {
  skip_on_cran()

  ## A `quote(...)` value in .useCacheArgs is evaluated in the local frame of
  ## .runEvent at splice time, so module authors can write e.g.
  ## `.cacheExtra = quote(sim$.someState)` and have it resolved per-event.
  testInit(opts = list(reproducible.useMemoise = FALSE,
                       reproducible.verbose     = 0,
                       spades.saveSimOnExit     = FALSE))

  modName <- "testUseCacheArgsQuoted"
  suppressMessages(newModule(modName, path = tmpdir, open = FALSE))

  args <- list(
    modules = list(modName),
    paths   = list(modulePath = tmpdir, cachePath = tmpCache),
    params  = stats::setNames(
      list(list(
        .useCache     = "init",
        ## Quoted call: should be evaluated at splice time and resolve to the
        ## literal string before being passed to Cache().
        .useCacheArgs = list(init = list(cacheId = quote(paste0("quoted_", "v1"))))
      )),
      modName
    )
  )

  try(reproducible::clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  mySim <- suppressMessages(do.call(simInit, args))
  .expectNoUnknownMessages(spades(mySim, debug = FALSE, .plotInitialTime = NA))

  cache <- reproducible::showCache(tmpCache, cacheId = "quoted_v1", verbose = -1)
  expect_true(NROW(cache) > 0,
              info = "Quoted cacheId in .useCacheArgs should be eval'd to a string before reaching Cache()")
})

test_that(".useCacheArgs is excluded from the per-module cache digest", {
  ## The grep("useCache", .knownDotParams) at simulation-spades.R:2367 should
  ## auto-include .useCacheArgs in paramsDontCacheOn now that .useCacheArgs is
  ## in .knownDotParams (helpers.R).
  paramsDontCacheOn <- grep("useCache",
                            SpaDES.core:::.knownDotParams, value = TRUE)
  expect_true(".useCacheArgs" %in% paramsDontCacheOn)
  expect_true(".useCache"     %in% paramsDontCacheOn)
})
