## `.neverCache` (a module's character vector of event names, possibly ".inputObjects") wins
## over every form of `.useCache`, because those events run for their side effects.

test_that(".eventIsCached: .useCache alone", {
  f <- function(u, ev = "init") .eventIsCached(if (is.null(u)) list() else list(.useCache = u), ev)
  expect_identical(f(TRUE), list(cache = TRUE, notOlderThan = NULL))
  expect_identical(f(FALSE)$cache, FALSE)
  expect_identical(f(NULL)$cache, FALSE)
  expect_identical(f("init")$cache, TRUE)
  expect_identical(f("other")$cache, FALSE)
  expect_identical(f(".inputObjects", ".inputObjects")$cache, TRUE)
  tm <- Sys.time()
  expect_identical(f(tm), list(cache = TRUE, notOlderThan = tm))
})

test_that(".eventIsCached: .neverCache wins over every form of .useCache", {
  tm <- Sys.time()
  for (u in list(TRUE, "init", tm)) {
    res <- .eventIsCached(list(.useCache = u, .neverCache = "init"), "init")
    expect_identical(res, list(cache = FALSE, notOlderThan = NULL))
  }
  ## only the listed events
  expect_identical(.eventIsCached(list(.useCache = TRUE, .neverCache = "init"), "other")$cache, TRUE)
  expect_identical(.eventIsCached(list(.useCache = TRUE, .neverCache = ".inputObjects"),
                                  ".inputObjects")$cache, FALSE)
  expect_identical(.eventIsCached(list(.neverCache = "init"), "init")$cache, FALSE)
})

test_that(".eventIsCached: the message is once per module and event", {
  .pkgEnv$neverCacheMsgd <- NULL
  p <- list(.useCache = TRUE, .neverCache = "init")
  expect_message(.eventIsCached(p, "init", "m", verbose = 1), "not cached.*`.neverCache`")
  expect_no_message(.eventIsCached(p, "init", "m", verbose = 1))
  expect_message(.eventIsCached(p, "init", "m2", verbose = 1), "m2")
  .pkgEnv$neverCacheMsgd <- NULL
  expect_no_message(.eventIsCached(p, "init", "m", verbose = -1))
  .pkgEnv$neverCacheMsgd <- NULL
})

.writeNeverCacheModule <- function(modulePath, counterFile, neverCache, extraParams = "") {
  modDir <- file.path(modulePath, "m")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "c"),
    defineParameter(".neverCache", "character", ', deparse(neverCache), ', NA, NA, "n")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("y", "numeric", "y"))))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      cat("init\\n", file = "', counterFile, '", append = TRUE)
      sim$y <- sim$x + 1
      sim <- scheduleEvent(sim, time(sim), "m", "second")
    },
    second = {
      cat("second\\n", file = "', counterFile, '", append = TRUE)
      sim$y <- sim$y + 1
    })
  invisible(sim)
}
.inputObjects <- function(sim) {
  cat("inputObjects\\n", file = "', counterFile, '", append = TRUE)
  sim$x <- 1
  sim
}
')
}

.neverCacheRuns <- function(counterFile) if (file.exists(counterFile)) readLines(counterFile) else character()

.runNeverCacheModule <- function(modulePath, cachePath, params = list(), debug = FALSE) {
  withr::local_options(spades.debug = debug)
  s <- simInit(modules = "m", times = list(start = 0, end = 1), params = params,
               paths = list(modulePath = modulePath, cachePath = cachePath))
  spades(s, debug = debug)
}

test_that(".neverCache = 'init' reruns init every time while other events stay cached", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))
  counterFile <- file.path(tmpdir, "runs.txt")
  .writeNeverCacheModule(tmpdir, counterFile, "init")
  msgs <- capture_messages(.runNeverCacheModule(tmpdir, tmpCache, debug = TRUE))
  expect_identical(sum(grepl("m event init is not cached", msgs)), 1L)
  runs1 <- .neverCacheRuns(counterFile)
  expect_identical(sort(runs1), sort(c("inputObjects", "init", "second")))
  .runNeverCacheModule(tmpdir, tmpCache)
  runs2 <- .neverCacheRuns(counterFile)[-seq_along(runs1)]
  expect_identical(runs2, "init") # .inputObjects and second hit the cache
})

test_that(".neverCache = '.inputObjects' keeps .inputObjects uncached; .globals reaches it", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE))
  counterFile <- file.path(tmpdir, "runs.txt")
  .writeNeverCacheModule(tmpdir, counterFile, character())
  ## declared-only: .globals sets it in a module that declares it
  msgs <- capture_messages(.runNeverCacheModule(tmpdir, tmpCache,
                             params = list(.globals = list(.neverCache = ".inputObjects")),
                             debug = TRUE))
  expect_identical(sum(grepl("m event .inputObjects is not cached", msgs)), 1L)
  .runNeverCacheModule(tmpdir, tmpCache,
                       params = list(.globals = list(.neverCache = ".inputObjects")))
  runs <- .neverCacheRuns(counterFile)
  expect_identical(sum(runs == "inputObjects"), 2L)
  expect_identical(sum(runs == "init"), 1L)
})
