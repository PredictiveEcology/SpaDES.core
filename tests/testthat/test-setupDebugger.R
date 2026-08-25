## setupDebugger() is internal; these drive it directly rather than through spades(),
## which only ever calls it with a list (see `useLoggingPkg <- is.list(debug)`).

setupDebugger <- SpaDES.core:::setupDebugger

test_that("setupDebugger passes FALSE straight through", {
  testInit()

  expect_false(setupDebugger(debug = FALSE))
})

test_that("setupDebugger with no argument returns the spades.debug option untouched", {
  testInit()

  withr::local_options(list(spades.debug = 42))
  expect_identical(setupDebugger(), 42)
})

test_that("setupDebugger returns the list's `debug` element when one is given", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  expect_identical(setupDebugger(debug = list(debug = 2)), 2)
})

test_that("setupDebugger defaults to debug level 1 when the list has no `debug` element", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  expect_identical(setupDebugger(debug = list(console = list(level = "INFO"))), 1)
})

test_that("setupDebugger rejects an unnamed list", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  expect_error(setupDebugger(debug = list(1, 2)), "named list")
})

test_that("setupDebugger rejects a `console` element that is not a list", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  expect_error(setupDebugger(debug = list(console = "DEBUG")),
               "not a list")
})

test_that("setupDebugger adds a console handler for a non-INFO level", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  logging::logReset()
  setupDebugger(debug = list(console = list(level = "DEBUG")))

  expect_true(any(grepl("writeToConsole", names(logging::getLogger()[["handlers"]]))))
})

test_that("setupDebugger leaves the console alone at INFO level", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  logging::logReset()
  setupDebugger(debug = list(console = list(level = "INFO")))

  expect_false(any(grepl("writeToConsole", names(logging::getLogger()[["handlers"]]))))
})

test_that("setupDebugger adds a file handler and writes a banner to the log file", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  logging::logReset()
  logFile <- tempfile(fileext = ".txt")

  setupDebugger(debug = list(file = list(file = logFile, level = "INFO")))

  expect_true(any(grepl("writeToFile", names(logging::getLogger()[["handlers"]]))))
  expect_true(file.exists(logFile))
  expect_match(paste(readLines(logFile), collapse = "\n"), "#####")
})

test_that("setupDebugger's file handler defaults its level when none is supplied", {
  testInit()
  skip_if_not_installed("logging")
  withr::defer(logging::logReset())

  logging::logReset()
  logFile <- tempfile(fileext = ".txt")

  expect_identical(setupDebugger(debug = list(file = list(file = logFile))), 1)
  expect_true(file.exists(logFile))
})
