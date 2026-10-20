## A module parameter that changes only how an event does its work (the hosts or cores a
## cluster may use) and not its result was still part of every cache key of the module, so
## adding a host to fireSense_spreadFit's `cores` re-ran every cached spreadFit event.
## `defineParameter(..., cacheKey = FALSE)` leaves the parameter out of the module's event,
## `.inputObjects` and cacheChaining keys; after a hit it keeps its current value, not the
## cached one. A parameter defined without `cacheKey` is keyed on as before.

.writeCacheKeyModule <- function(modulePath, hosts, alpha, useCache, cacheKey = ", cacheKey = FALSE") {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", ', deparse(alpha), ', 0, 10, "a"),
    defineParameter("hosts", "character", ', deparse(hosts), ', NA, NA, "h"', cacheKey, '),
    defineParameter(".useCache", "character", ', deparse(useCache), ', NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("seen", "list", "s"),
                                                      createsOutput("seenIO", "list", "s"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim$seen <- list(hosts = P(sim)$hosts, alpha = P(sim)$alpha) })
  invisible(sim)
}
.inputObjects <- function(sim) {
  sim$seenIO <- list(hosts = P(sim)$hosts, alpha = P(sim)$alpha)
  sim
}
')
}

.runCacheKeyModule <- function(modulePath, cachePath, ...) {
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1),
                                paths = list(modulePath = modulePath, cachePath = cachePath), ...))
  suppressMessages(spades(s, debug = FALSE))
}

cacheKeyOpts <- list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                     reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE)

test_that("defineParameter(cacheKey = FALSE) is kept with the parameter and defaults to TRUE", {
  testInit()

  p <- defineParameter("hosts", "character", "a", NA, NA, "h", cacheKey = FALSE)
  expect_false(p$cacheKey)
  expect_true(defineParameter("alpha", "numeric", 1, NA, NA, "a")$cacheKey)
  expect_true(all(c("paramName", "cacheKey") %in% names(defineParameter())))
  expect_error(defineParameter("hosts", "character", "a", NA, NA, "h", cacheKey = NA), "cacheKey")
  expect_error(defineParameter("hosts", "character", "a", NA, NA, "h", cacheKey = "no"), "cacheKey")
})

test_that("a cacheKey = FALSE parameter is left out of an event's cache key and keeps its value after a hit", {
  skip_on_cran()
  testInit(opts = cacheKeyOpts)

  .writeCacheKeyModule(tmpdir, hosts = "a", alpha = 1, useCache = "init")
  s1 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s1$seen, list(hosts = "a", alpha = 1))

  ## only `hosts` changes: a hit, so the event's result is the cached one, and the
  ## module's current `hosts` is kept
  .writeCacheKeyModule(tmpdir, hosts = "b", alpha = 1, useCache = "init")
  s2 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s2$seen, list(hosts = "a", alpha = 1))
  expect_identical(params(s2)$m$hosts, "b")

  ## a parameter defined without cacheKey still changes the key: a miss
  .writeCacheKeyModule(tmpdir, hosts = "b", alpha = 2, useCache = "init")
  s3 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s3$seen, list(hosts = "b", alpha = 2))
})

test_that("a cacheKey = FALSE parameter is left out of the .inputObjects cache key", {
  skip_on_cran()
  testInit(opts = cacheKeyOpts)

  .writeCacheKeyModule(tmpdir, hosts = "a", alpha = 1, useCache = ".inputObjects")
  s1 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s1$seenIO, list(hosts = "a", alpha = 1))

  .writeCacheKeyModule(tmpdir, hosts = "b", alpha = 1, useCache = ".inputObjects")
  s2 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s2$seenIO, list(hosts = "a", alpha = 1))
  expect_identical(params(s2)$m$hosts, "b")

  .writeCacheKeyModule(tmpdir, hosts = "b", alpha = 2, useCache = ".inputObjects")
  s3 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s3$seenIO, list(hosts = "b", alpha = 2))
})

test_that("a cacheKey = FALSE parameter set at simInit is left out of the key too", {
  skip_on_cran()
  testInit(opts = cacheKeyOpts)

  ## the parameter's value comes from `params`, not the module's default, as `cores` does in runs
  .writeCacheKeyModule(tmpdir, hosts = "a", alpha = 1, useCache = "init")
  s1 <- .runCacheKeyModule(tmpdir, tmpCache, params = list(m = list(hosts = c("x", "y"))))
  expect_identical(s1$seen, list(hosts = c("x", "y"), alpha = 1))

  s2 <- .runCacheKeyModule(tmpdir, tmpCache, params = list(m = list(hosts = "z")))
  expect_identical(s2$seen, list(hosts = c("x", "y"), alpha = 1))
  expect_identical(params(s2)$m$hosts, "z")
})

test_that("a module that does not use cacheKey is keyed on the same parameters as before", {
  skip_on_cran()
  testInit(opts = cacheKeyOpts)

  .writeCacheKeyModule(tmpdir, hosts = "a", alpha = 1, useCache = "init", cacheKey = "")
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  ## every parameter is keyed on, except the fixed list that was already left out
  expect_true(all(s@depends@dependencies$m@parameters$cacheKey))
  expect_identical(.paramsNotCachedOn(s, "m"),
                   intersect(names(params(s)$m), paramsDontCacheOn))

  ## and a changed `hosts` is a miss, as it was
  s1 <- suppressMessages(spades(s, debug = FALSE))
  .writeCacheKeyModule(tmpdir, hosts = "b", alpha = 1, useCache = "init", cacheKey = "")
  s2 <- .runCacheKeyModule(tmpdir, tmpCache)
  expect_identical(s2$seen, list(hosts = "b", alpha = 1))
})
