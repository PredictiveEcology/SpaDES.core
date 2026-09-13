## options(spades.useCache) chooses, without touching module code, whether the modules' `.useCache`
## events, the `Cache()` calls inside module code, both or neither are cached. Before this option
## the only lever was `reproducible.useCache`, which every one of those calls took, so it was all or
## nothing. On a fireSense fitting campaign the module-internal calls wrote 74 GB in 2 hours, 57 GB
## of it never read back, while the event caches (19 GB) are what let a failed job resume.
##
## The module's init event calls Cache(innerFn) twice with the same argument; innerFn appends a
## line to a file, so the file counts real executions: 1 per init run when inner caching is on,
## 2 when it is off. Whether init itself is cached shows in whether a second spades() run adds lines.

.writeUseCacheModule <- function(modulePath, counterFile) {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("y", "numeric", "y"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = {
    a <- reproducible::Cache(innerFn, sim$x, counterFile = "', counterFile, '")
    b <- reproducible::Cache(innerFn, sim$x, counterFile = "', counterFile, '")
    sim$y <- a + b
  })
  invisible(sim)
}
innerFn <- function(x, counterFile) {
  cat("ran\\n", file = counterFile, append = TRUE)
  x + 1
}
.inputObjects <- function(sim) {
  sim$x <- 1
  sim
}
')
}

.innerRuns <- function(counterFile) if (file.exists(counterFile)) length(readLines(counterFile)) else 0L

.runUseCacheModule <- function(modulePath, cachePath) {
  s <- suppressMessages(simInit(modules = "m", times = list(start = 0, end = 1),
                                paths = list(modulePath = modulePath, cachePath = cachePath)))
  suppressMessages(spades(s, debug = FALSE))
}

test_that("spades.useCache = 'all' caches events and module-internal calls (the previous behaviour)", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
                       spades.useCache = "all"))
  counterFile <- file.path(tmpdir, "runs.txt")
  .writeUseCacheModule(tmpdir, counterFile)

  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 1L) # second inner call hit the first
  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 1L) # init itself was cached
  expect_true(any(grepl("^doEvent.m", showCache(tmpCache, verbose = -2)[tagKey == "function"]$tagValue)))
})

test_that("spades.useCache = 'eventsOnly' caches events but not module-internal calls", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
                       spades.useCache = "eventsOnly"))
  counterFile <- file.path(tmpdir, "runs.txt")
  .writeUseCacheModule(tmpdir, counterFile)

  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 2L) # both inner calls ran
  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 2L) # init was cached: no more runs
  fns <- showCache(tmpCache, verbose = -2)[tagKey == "function"]$tagValue
  expect_true(any(grepl("^doEvent.m", fns)))
  expect_true(any(grepl("^.inputObjects_m", fns)))
  expect_false(any(grepl("innerFn", fns)))
  ## the option is restored after the run
  expect_true(getOption("reproducible.useCache", TRUE))
})

test_that("spades.useCache = 'off' caches nothing", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
                       spades.useCache = "off"))
  counterFile <- file.path(tmpdir, "runs.txt")
  .writeUseCacheModule(tmpdir, counterFile)

  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 2L)
  .runUseCacheModule(tmpdir, tmpCache)
  expect_identical(.innerRuns(counterFile), 4L) # init ran again
  expect_identical(nrow(showCache(tmpCache, verbose = -2)), 0L)
})

test_that("the numbers 2, 1, 0 mean 'all', 'eventsOnly', 'off'", {
  expect_identical(.spadesUseCache(2), .spadesUseCache("all"))
  expect_identical(.spadesUseCache(1), .spadesUseCache("eventsOnly"))
  expect_identical(.spadesUseCache(0), .spadesUseCache("off"))
  expect_error(.spadesUseCache(3), "0, 1 or 2")
  expect_error(.spadesUseCache("sometimes"))
})

test_that("with 'eventsOnly', a simInit nested inside a cached event still caches its .inputObjects", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
                       spades.useCache = "eventsOnly"))
  counterFile <- file.path(tmpdir, "inner.txt")
  ## module n's .inputObjects counts its runs; module outer runs a simInit of n inside its init,
  ## wrapped in a module-internal Cache() as fireSense_dataPrepFit does
  for (nm in c("n", "outer")) dir.create(file.path(tmpdir, nm), showWarnings = FALSE, recursive = TRUE)
  cat(file = file.path(tmpdir, "n", "n.R"), sep = "", '
defineModule(sim, list(name = "n", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(n = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("z", "numeric", "z"))))
doEvent.n <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
.inputObjects <- function(sim) {
  cat("ran\\n", file = "', counterFile, '", append = TRUE)
  sim$z <- 2
  sim
}
')
  cat(file = file.path(tmpdir, "outer", "outer.R"), sep = "", '
defineModule(sim, list(name = "outer", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(outer = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", "init", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("z", "numeric", "z"))))
doEvent.outer <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = {
    inner <- reproducible::Cache(simInit, modules = "n", times = list(start = 0, end = 0),
                                 paths = list(modulePath = "', tmpdir, '", cachePath = "', tmpCache, '"))
    sim$z <- inner$z
  })
  invisible(sim)
}
')
  run <- function() {
    s <- suppressMessages(simInit(modules = "outer", times = list(start = 0, end = 1),
                                  paths = list(modulePath = tmpdir, cachePath = tmpCache)))
    suppressMessages(spades(s, debug = FALSE))
  }
  run()
  expect_identical(.innerRuns(counterFile), 1L)
  fns <- showCache(tmpCache, verbose = -2)[tagKey == "function"]$tagValue
  expect_true(any(grepl("^.inputObjects_n", fns)))   # nested event cached
  expect_false(any(grepl("^simInit", fns)))           # the module-internal Cache(simInit) was skipped
  ## a second, uncached run of outer's init re-enters the nested simInit: n's .inputObjects hits.
  ## Clear outer's entry by cacheId: nested entries inherit the outer call's tags, so clearing by
  ## userTags would remove n's entry as well.
  outerId <- unique(showCache(tmpCache, verbose = -2)[tagKey == "function" & grepl("^doEvent.outer", tagValue)]$cacheId)
  clearCache(tmpCache, cacheId = outerId, ask = FALSE)
  run()
  expect_identical(.innerRuns(counterFile), 1L)
})
