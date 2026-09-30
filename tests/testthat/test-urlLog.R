## Per-run download ledger: envir(sim)$._urlLog (R/urlLog.R) must be readable via
## urlLog(), and must persist with the run -- saveSimList/loadSimList, Copy, and a
## Cache restore -- while never entering a cache key.

ledger <- function(sim) urlLog(sim, which = c("cacheId", "function", "module", "event", "url"))

.urlLogToyModule <- function(tmpdir, srcFile, m = "urlMod", useCache = "init") {
  dir.create(file.path(tmpdir, m), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    sprintf('defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),', m),
    sprintf('  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),', m),
    '  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list("reproducible"),',
    '  parameters = rbind(), inputObjects = bindrows(), outputObjects = bindrows(createsOutput("got", "character", ""))))',
    sprintf('doEvent.%s <- function(sim, eventTime, eventType) {', m),
    '  if (eventType == "init") {',
    sprintf('    sim$got <- reproducible::prepInputs(url = "file://%s", targetFile = "%s",', srcFile, basename(srcFile)),
    '      destinationPath = file.path(outputPath(sim), "dl"), fun = NA, overwrite = TRUE)',
    '  }',
    '  invisible(sim)',
    '}'), file.path(tmpdir, m, paste0(m, ".R")))
  m
}

.urlLogSim <- function(tmpdir, cp, useCache = FALSE) {
  src <- file.path(tmpdir, "src.txt")
  writeLines("hello", src)
  m <- .urlLogToyModule(tmpdir, src)
  sim <- simInit(modules = m, times = list(start = 0, end = 0),
                 params = list(urlMod = list(.useCache = useCache)),
                 paths = list(modulePath = tmpdir, cachePath = cp, outputPath = file.path(tmpdir, "out")))
  spades(sim)
}

test_that("urlLog(sim) returns the records with module and event; zero-row table when none", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  empty <- simInit(times = list(start = 0, end = 1))
  expect_s3_class(urlLog(empty), "data.table")
  expect_identical(NROW(urlLog(empty)), 0L)
  expect_identical(names(urlLog(empty)), c("function", "module", "url"))

  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "cache"))
  ul <- urlLog(sim, which = c("function", "module", "event", "url", "caller", "cacheId", "lastSeen"))
  expect_s3_class(ul, "data.table")
  expect_identical(NROW(ul), 1L)
  expect_identical(ul[["function"]], "prepInputs")
  expect_identical(ul$module, "urlMod")
  expect_identical(ul$event, "init")
  expect_match(ul$url, "src.txt$")
})

test_that("the ledger survives saveSimList()/loadSimList() and Copy()", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "cache"))
  before <- ledger(sim)
  expect_identical(NROW(before), 1L)

  expect_identical(NROW(ledger(Copy(sim))), 1L)

  f <- file.path(tmpdir, "sim.qs2")
  saveSimList(sim, f)
  sim2 <- loadSimList(f)
  expect_identical(ledger(sim2)$url, before$url)
  expect_identical(ledger(sim2)$module, "urlMod")
})

test_that("a Cache restore of the init event keeps the ledger, merged and not duplicated", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  cp <- file.path(tmpdir, "cache")
  sim1 <- .urlLogSim(tmpdir, cp, useCache = "init")
  expect_identical(NROW(ledger(sim1)), 1L)
  sim2 <- .urlLogSim(tmpdir, cp, useCache = "init") # init restored from Cache
  ul <- ledger(sim2)
  expect_identical(NROW(ul), 1L)
  expect_identical(ul$module, "urlMod")
})

test_that("the ledger never changes a cache key", {
  testInit(opts = list(reproducible.useMemoise = FALSE))
  ids <- lapply(c(TRUE, FALSE), function(flag) {
    withr::local_options(spades.urlLog = flag)
    cp <- file.path(tmpdir, paste0("cache", flag))
    sim <- .urlLogSim(tmpdir, cp, useCache = "init")
    sc <- showCache(cp, verbose = -2)
    sort(unique(sc$cacheId[sc$tagKey == "function" & grepl("doEvent", sc$tagValue)]))
  })
  expect_length(ids[[1]], 1L)
  expect_identical(ids[[1]], ids[[2]])

  ## and a populated ledger digests identically to an empty one
  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "c3"))
  d1 <- .robustDigest(sim)
  rm("._urlLog", envir = envir(sim))
  expect_identical(.robustDigest(sim), d1)
})

test_that("a Cache restore keeps the live run's earlier records and adds the restored entry's", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  cp <- file.path(tmpdir, "cache")
  mp <- tmpdir
  srcA <- file.path(tmpdir, "srcA.txt"); writeLines("a", srcA)
  srcB <- file.path(tmpdir, "srcB.txt"); writeLines("b", srcB)
  .urlLogToyModule(mp, srcA, m = "urlA")
  .urlLogToyModule(mp, srcB, m = "urlB")
  run <- function() {
    sim <- simInit(modules = c("urlA", "urlB"), times = list(start = 0, end = 0),
                   params = list(urlA = list(.useCache = FALSE), urlB = list(.useCache = "init")),
                   paths = list(modulePath = mp, cachePath = cp, outputPath = file.path(tmpdir, "out")))
    spades(sim)
  }
  s1 <- run()
  expect_setequal(basename(ledger(s1)$url), c("srcA.txt", "srcB.txt"))
  s2 <- run() # urlB init restored from Cache; urlA init runs live
  ul <- ledger(s2)
  expect_identical(NROW(ul), 2L)
  expect_identical(sort(ul$module), c("urlA", "urlB"))
  expect_identical(ul$event, c("init", "init"))
  ## records made after the restore land in the sim's own ledger
  expect_true(identical(getOption("reproducible.urlLog"), NULL) ||
              !identical(getOption("reproducible.urlLog"), envir(s2)$._urlLog))
})

test_that("init events run during simInit (allowInitDuringSimInit) do not replace the ledger", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE,
                       spades.allowInitDuringSimInit = TRUE))
  mp <- tmpdir
  mk <- function(nm, inp, out, initBody) {
    dl <- function(tag) {
      f <- file.path(tmpdir, paste0("src", nm, tag, ".txt")); writeLines(tag, f)
      sprintf('reproducible::prepInputs(url = "file://%s", targetFile = "%s", destinationPath = "%s", fun = NA)',
              f, basename(f), file.path(tmpdir, "dl", nm, tag))
    }
    dir.create(file.path(mp, nm), showWarnings = FALSE)
    writeLines(c(
      sprintf('defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"), childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list("reproducible"), parameters = rbind(), inputObjects = bindrows(%s), outputObjects = bindrows(%s)))', nm, nm, inp, out),
      sprintf('doEvent.%s <- function(sim, eventTime, eventType) { if (eventType == "init") { junk <- %s; %s }; invisible(sim) }', nm, dl("init"), initBody),
      sprintf('.inputObjects <- function(sim) { junk <- %s; sim }', dl("io"))),
      file.path(mp, nm, paste0(nm, ".R")))
  }
  ## P needs an object nobody makes (so it can run first, during simInit); Q needs P's output
  mk("P", 'expectsInput("ext", "numeric", "")', 'createsOutput("X", "numeric", "")', "sim$X <- 1")
  mk("Q", 'expectsInput("X", "numeric", "")', 'createsOutput("Y", "numeric", "")', "sim$Y <- sim$X")
  sim <- simInit(modules = c("P", "Q"), times = list(start = 0, end = 0),
                 paths = list(modulePath = mp, cachePath = file.path(tmpdir, "cache"),
                              outputPath = file.path(tmpdir, "out")))
  ## P's init ran inside simInit() (resolveDepsRunInitIfPoss()): its records came from a second sim
  expect_true("P" %in% completed(sim)$moduleName)
  sim <- spades(sim)
  ul <- ledger(sim)
  key <- paste(ul$module, ul$event, basename(ul$url))
  expect_setequal(key, c("P .inputObjects srcPio.txt", "Q .inputObjects srcQio.txt",
                         "P init srcPinit.txt", "Q init srcQinit.txt"))
  expect_false(anyDuplicated(key) > 0)
})

test_that("the ledger survives every save format saveSimList() supports", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "cache"))
  want <- ledger(sim)
  expect_identical(NROW(want), 1L)
  cases <- list(qs2 = list(file = "s.qs2"), rds = list(file = "s.rds"),
                lazy = list(file = "sl.qs2", lazy = TRUE),
                filesFalse = list(file = "sf.qs2", files = FALSE))
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    args <- c(list(sim = sim, filename = file.path(tmpdir, cs$file)), cs[setdiff(names(cs), "file")])
    do.call(saveSimList, args)
    back <- loadSimList(file.path(tmpdir, cs$file))
    expect_identical(ledger(back)$url, want$url, label = nm)
    expect_identical(ledger(back)$module, "urlMod", label = nm)
  }
})

test_that("Copy() keeps the ledger unless objects are not copied, and the copy is independent", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "cache"))
  for (o in c(1, 2, TRUE)) {
    cp <- Copy(sim, objects = o)
    expect_identical(NROW(ledger(cp)), 1L, label = paste("objects =", o))
  }
  expect_identical(NROW(ledger(Copy(sim, queues = FALSE))), 1L)
  cp <- Copy(sim)
  cp$._urlLog$records <- list()
  expect_identical(NROW(ledger(sim)), 1L) # the copy's ledger is its own
})

test_that("a whole spades() Cache hit merges the entry's ledger into the live one, without duplicates", {
  testInit(opts = list(reproducible.useMemoise = FALSE, spades.urlLog = TRUE))
  cp <- file.path(tmpdir, "cache")
  s1 <- .urlLogSim(tmpdir, cp, useCache = FALSE) # uncached events; only the spades() call is cached below
  mk <- function() simInit(modules = "urlMod", times = list(start = 0, end = 0),
                           params = list(urlMod = list(.useCache = FALSE)),
                           paths = list(modulePath = tmpdir, cachePath = cp, outputPath = file.path(tmpdir, "out")))
  a <- spades(mk(), cache = TRUE)
  expect_identical(NROW(ledger(a)), 1L)
  b <- spades(mk(), cache = TRUE) # restored from Cache
  expect_identical(ledger(b)$url, ledger(a)$url)
  expect_identical(NROW(ledger(b)), 1L)
  ## the live ledger's own earlier records are kept: pre-load one, then restore
  live <- mk()
  live$._urlLog$records <- list(list(time = "2026-01-01T00:00:00.000", fn = "prepInputs",
                                     url = "https://example.com/earlier.tif", cacheId = NA_character_,
                                     module = "other", event = "init"))
  live$._urlLog$seen <- "k-earlier"
  d <- spades(live, cache = TRUE)
  expect_setequal(basename(ledger(d)$url), c("earlier.tif", basename(ledger(a)$url)))
})

test_that("an event's changed-object list never carries the ledger from a cache entry", {
  testInit(opts = list(reproducible.useMemoise = FALSE))
  sim <- .urlLogSim(tmpdir, file.path(tmpdir, "cache"))
  deps <- sim@depends@dependencies
  changed <- list(`._urlLog` = 1, got = 1, urlMod = 1)
  out <- SpaDES.core:::lsObjectsChanged(c("._urlLog", "got"), changed,
                                        hasCurrModule = match("urlMod", names(deps)),
                                        currModules = "urlMod", deps = deps)
  expect_false("._urlLog" %in% out)
  expect_true("got" %in% out)
})
