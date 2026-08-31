## memory.R: the ps-based memory sampler and the memoryUse() summary.
##
## The sampler normally runs inside a future::callr subprocess, so the loop is
## also driven directly here -- coverage in a subprocess is invisible to covr,
## and more importantly the loop's exit conditions deserve a test of their own.

## ---- small helpers -----------------------------------------------------

test_that("isWindows agrees with .Platform", {
  testInit()

  expect_identical(SpaDES.core:::isWindows(), identical(.Platform$OS.type, "windows"))
})

test_that("stopFilename derives the sentinel name from the output file", {
  testInit()

  expect_identical(SpaDES.core:::stopFilename("/a/b/memoryUse.csv"),
                   "/a/b/memoryUsedone.csv")
})

test_that("outputFilename embeds the pid", {
  testInit()

  f <- SpaDES.core:::outputFilename(4242)

  expect_type(f, "character")
  expect_match(basename(f), "memAvail_4242\\.txt$")
})

test_that(".rssField tolerates ps's right-aligned columns", {
  testInit()

  ## `ps` pads rss into a fixed-width column, so a value narrower than the
  ## column leaves the line starting with spaces. Splitting on " +" without
  ## trimming first returned "" and every reading parsed to NA -- which is why
  ## memoryUse() silently produced NA on macOS, and for any small process.
  expect_identical(SpaDES.core:::.rssField("    0       2"), "0")
  expect_identical(SpaDES.core:::.rssField(" 3884 3597561"), "3884")
  expect_identical(SpaDES.core:::.rssField("62064 3597570"), "62064")
})

## ---- memoryUseThisSession ----------------------------------------------

test_that("memoryUseThisSession reports this session's memory as an object_size", {
  skip_on_cran()
  testInit()

  m <- SpaDES.core:::memoryUseThisSession()

  expect_s3_class(m, "object_size")
  expect_true(is.numeric(unclass(m)))
  expect_gt(as.numeric(m), 0)
})

test_that("memoryUseThisSession defaults to the current pid", {
  skip_on_cran()
  testInit()

  expect_equal(as.numeric(SpaDES.core:::memoryUseThisSession()),
               as.numeric(SpaDES.core:::memoryUseThisSession(Sys.getpid())),
               tolerance = 0.5)
})

## ---- ongoingMemoryThisPid ----------------------------------------------

test_that("ongoingMemoryThisPid does nothing when the interval is 0", {
  testInit()

  f <- withr::local_tempfile(fileext = ".csv")

  msgs <- capture_messages(
    out <- SpaDES.core:::ongoingMemoryThisPid(seconds = 1, interval = 0, outputFile = f)
  )

  expect_match(paste(msgs, collapse = ""), "interval is 0")
  expect_identical(out, f)
  expect_false(file.exists(f))
})

test_that("ongoingMemoryThisPid samples memory into the output file", {
  skip_on_cran()
  testInit()

  ## the loop sleeps by getOption("spades.memoryUseInterval"), not by `interval`
  withr::local_options(list(spades.memoryUseInterval = 0.1))
  f <- withr::local_tempfile(fileext = ".csv")

  out <- SpaDES.core:::ongoingMemoryThisPid(seconds = 0.3, interval = 0.1, outputFile = f)

  expect_identical(out, f)
  expect_true(file.exists(f))

  dt <- data.table::fread(f)
  expect_true(NROW(dt) >= 1)
  expect_setequal(names(dt), c("memory", "time"))
  expect_true(all(dt$memory > 0))
})

test_that("ongoingMemoryThisPid stops when the sentinel file appears", {
  skip_on_cran()
  testInit()

  withr::local_options(list(spades.memoryUseInterval = 0.1))
  f <- withr::local_tempfile(fileext = ".csv")
  ## the sentinel is already there, so the loop must not run a single pass
  writeLines("x", SpaDES.core:::stopFilename(f))
  withr::defer(unlink(SpaDES.core:::stopFilename(f)))

  out <- SpaDES.core:::ongoingMemoryThisPid(seconds = 100, interval = 0.1, outputFile = f)

  expect_identical(out, f)
  expect_false(file.exists(f))
})

## ---- memoryUse ---------------------------------------------------------

test_that("memoryUse says so when the sim carries no memory data", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  defineEvent(sim, "tick", moduleName = "mA", code = { sim })
  sim <- scheduleEvent(sim, 0, "mA", "tick")
  out <- suppressMessages(spades(sim))

  msgs <- capture_messages(res <- memoryUse(out))

  expect_null(res)
  expect_match(paste(msgs, collapse = ""), "no data in the sim")
})

test_that("memoryUse summarises the maximum memory per module and event", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  defineEvent(sim, "tick", moduleName = "mA", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "mA", "tick")
  })
  sim <- scheduleEvent(sim, 0, "mA", "tick")
  out <- suppressMessages(spades(sim))

  ## stand in for the sampler's output: one reading per completed event
  cmp <- completed(out)
  out@.xData$.memoryUse <- list(obj = data.table::data.table(
    memory = seq(100, by = 10, length.out = NROW(cmp)),
    time = cmp[[SpaDES.core:::._txtClockTime]]
  ))

  a <- memoryUse(out)

  expect_s3_class(a, "data.table")
  expect_setequal(names(a), c("moduleName", "eventType", "maxMemory"))
  expect_true("mA" %in% a$moduleName)
  ## one row per module/event combination that ran
  expect_identical(NROW(a), NROW(unique(cmp[, c("moduleName", "eventType")])))
})

test_that("memoryUse with max = FALSE keeps one row per event time", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  defineEvent(sim, "tick", moduleName = "mA", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "mA", "tick")
  })
  sim <- scheduleEvent(sim, 0, "mA", "tick")
  out <- suppressMessages(spades(sim))

  cmp <- completed(out)
  out@.xData$.memoryUse <- list(obj = data.table::data.table(
    memory = seq(100, by = 10, length.out = NROW(cmp)),
    time = cmp[[SpaDES.core:::._txtClockTime]]
  ))

  a <- memoryUse(out, max = FALSE)

  expect_setequal(names(a), c("moduleName", "eventType", "eventTime", "maxMemory"))
  expect_identical(NROW(a), NROW(cmp))
})

## ---- setup / teardown around spades() ----------------------------------

test_that("memoryUseSetup insists on a non-sequential future plan", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  testInit()

  originalPlan <- future::plan()
  withr::defer(future::plan(originalPlan))
  future::plan("sequential")

  ## note: future::plan(x) returns the *previous* plan, so the current one has
  ## to be fetched separately for `originalFuturePlan` to really be sequential
  withr::local_options(list(spades.futurePlan = NULL))

  expect_error(
    SpaDES.core:::memoryUseSetup(simInit(), originalFuturePlan = future::plan()),
    "you must set a future::plan"
  )
})

test_that("spades() records memory use when an interval is set", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  testInit()

  originalPlan <- future::plan()
  withr::defer(future::plan(originalPlan))
  future::plan(future.callr::callr)

  withr::local_options(list(spades.memoryUseInterval = 0.2,
                            spades.futurePlan = "callr"))

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"),
                 paths = list(outputPath = tmpdir))
  defineEvent(sim, "tick", moduleName = "mA", code = {
    Sys.sleep(0.3)
    sim <- scheduleEvent(sim, time(sim) + 1, "mA", "tick")
  })
  sim <- scheduleEvent(sim, 0, "mA", "tick")

  out <- suppressMessages(spades(sim))

  ## the sampler's readings were folded back into the simList ...
  expect_true(is.data.frame(out@.xData$.memoryUse$obj))
  expect_true(NROW(out@.xData$.memoryUse$obj) > 0)
  ## ... and the csv it wrote was cleaned up
  expect_false(file.exists(out@.xData$.memoryUse$filename))

  a <- memoryUse(out)
  expect_s3_class(a, "data.table")
  expect_true("mA" %in% a$moduleName)
  expect_true(all(a$maxMemory > 0))
})
