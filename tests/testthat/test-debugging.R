test_that("writeEventInfo writes the current event to a file", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim <- scheduleEvent(sim, 0, "someModule", "someEvent")

  f <- tempfile(fileext = ".txt")
  writeEventInfo(sim, file = f)

  expect_true(file.exists(f))
  expect_true(nzchar(paste(readLines(f), collapse = "")))
})

test_that("writeEventInfo appends when asked and truncates when not", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim <- scheduleEvent(sim, 0, "someModule", "someEvent")

  f <- tempfile(fileext = ".txt")
  writeEventInfo(sim, file = f, append = FALSE)
  oneRun <- length(readLines(f))

  writeEventInfo(sim, file = f, append = TRUE)
  expect_gt(length(readLines(f)), oneRun)

  writeEventInfo(sim, file = f, append = FALSE)
  expect_identical(length(readLines(f)), oneRun)
})

test_that("writeRNGInfo writes the RNG stream state", {
  testInit()

  set.seed(123)  # ensure .Random.seed exists
  f <- tempfile(fileext = ".txt")
  writeRNGInfo(file = f)

  expect_true(file.exists(f))
  txt <- paste(readLines(f), collapse = "\n")
  expect_match(txt, "Start of new RNG stream")
  ## all 10 seed elements are written, comma-separated, ending in a period
  expect_length(strsplit(sub("^.*stream: ", "", sub("\\.$", "", txt)), ", ")[[1]], 10L)
})

test_that("writeRNGInfo appends when asked", {
  testInit()

  set.seed(123)
  f <- tempfile(fileext = ".txt")
  writeRNGInfo(file = f, append = FALSE)
  oneRun <- length(readLines(f))

  writeRNGInfo(file = f, append = TRUE)
  expect_gt(length(readLines(f)), oneRun)
})
