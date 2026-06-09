test_that("event cacheId ignores absolute module paths (modules + event moduleName)", {
  skip_on_cran()
  ## A module's install path is carried as the *name* of its `sim@modules` entry
  ## and as the *name* of each event's `moduleName`. Those paths differ across
  ## machines/OSs and must NOT enter the cacheId, else an identical `init`/event
  ## run on Linux vs Windows gets a different cacheId and cannot share a (cloud)
  ## cache. `.inputObjects` is unaffected (basename + no event queue); this guards
  ## the event path.
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE))
  modDir <- file.path(tmpdir, "m"); dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", "init", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) { switch(eventType, init = {}); invisible(sim) }
')
  s <- suppressMessages(simInit(modules = "m", paths = list(modulePath = tmpdir),
                                times = list(start = 0, end = 1)))
  ## digest the simList the way the event path does (events digested, module = "m")
  co <- list(events = "init", current = FALSE, completed = FALSE, simtimes = FALSE,
             modules = "m")
  d1 <- reproducible::CacheDigest(s, classOptions = co)$outputHash

  ## Re-point the module path everywhere it is carried as a *name* (emulates a
  ## different machine/OS install location). cacheId must not move.
  newPath <- "/some/other/abs/path/to/m"
  nm <- names(s@modules); nm[nzchar(nm)] <- newPath; names(s@modules) <- nm
  s@events <- lapply(s@events, function(e) {
    if (!is.null(e[["moduleName"]]) && !is.null(names(e[["moduleName"]])))
      names(e[["moduleName"]]) <- newPath
    e
  })
  d2 <- reproducible::CacheDigest(s, classOptions = co)$outputHash
  expect_identical(d1, d2)
})
