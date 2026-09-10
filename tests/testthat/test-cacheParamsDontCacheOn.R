## The params that are not cached on (`.useCache`, `.useCacheArgs`, `.useCloud`) are left
## out of the cacheId, so adding or removing one of them in a module's metadata still hits
## the old cache entry. A hit replaces the module's params with the cached list, and the
## current values of those params were then put back with a logical index built on the
## *current* names -- i.e. by position. When the cached list has a different set of params
## the values shift: a module that gained `.useCloud` before `.useCacheArgs` got
## `.useCacheArgs = TRUE`, and the next cached event failed with "subscript out of bounds";
## the reverse gave `.useCloud` the `.useCacheArgs` list. Seen in fireSense fits when
## fireSense_ELFs gained `.useCloud`.

.writeDontCacheOnModule <- function(modulePath, useCache, withUseCloud, tag) {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, 0, 10, "a"),
    defineParameter(".useCache", "character", ', deparse(useCache), ', NA, NA, "c"),',
      if (withUseCloud) '
    defineParameter(".useCloud", "logical", TRUE, NA, NA, "u"),', '
    defineParameter(".useCacheArgs", "list", list(init = list(userTags = "', tag, '")), NA, NA, "x")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("x", "numeric", "x"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim$x <- sim$x + 1 })
  invisible(sim)
}
.inputObjects <- function(sim) {
  sim$x <- 1
  sim
}
')
}

.runDontCacheOnModule <- function(modulePath, cachePath) {
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1),
                                paths = list(modulePath = modulePath, cachePath = cachePath)))
  afterSimInit <- params(s)$m
  s <- suppressMessages(spades(s, debug = FALSE))
  list(afterSimInit = afterSimInit, afterSpades = params(s)$m)
}

test_that("a cache hit on .inputObjects restores not-cached-on params by name, not position", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))

  ## first run: no .useCloud; populates the .inputObjects and init cache entries
  .writeDontCacheOnModule(tmpdir, c(".inputObjects", "init"), withUseCloud = FALSE, tag = "tagA")
  .runDontCacheOnModule(tmpdir, tmpCache)

  ## the module gains .useCloud ahead of .useCacheArgs: both cache entries still hit
  .writeDontCacheOnModule(tmpdir, c(".inputObjects", "init"), withUseCloud = TRUE, tag = "tagB")
  res <- .runDontCacheOnModule(tmpdir, tmpCache)
  for (p in res) {
    expect_identical(p$.useCacheArgs, list(init = list(userTags = "tagB")))
    expect_identical(p$.useCloud, TRUE)
    expect_identical(p$.useCache, c(".inputObjects", "init"))
  }

  ## and the reverse: the module loses .useCloud again
  .writeDontCacheOnModule(tmpdir, c(".inputObjects", "init"), withUseCloud = FALSE, tag = "tagC")
  res <- .runDontCacheOnModule(tmpdir, tmpCache)
  for (p in res) {
    expect_identical(p$.useCacheArgs, list(init = list(userTags = "tagC")))
    expect_identical(p$.useCache, c(".inputObjects", "init"))
  }
})

test_that("a cache hit on an event restores not-cached-on params by name, not position", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))

  ## only `init` is cached, so this exercises the event path (spades), not simInit's
  .writeDontCacheOnModule(tmpdir, "init", withUseCloud = FALSE, tag = "tagA")
  .runDontCacheOnModule(tmpdir, tmpCache)

  .writeDontCacheOnModule(tmpdir, "init", withUseCloud = TRUE, tag = "tagB")
  p <- .runDontCacheOnModule(tmpdir, tmpCache)$afterSpades
  expect_identical(p$.useCacheArgs, list(init = list(userTags = "tagB")))
  expect_identical(p$.useCloud, TRUE)
  expect_identical(p$.useCache, "init")
})
