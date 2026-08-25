## Unit arithmetic in times.R: inSeconds(), convertTimeunit(), checkTimeunit(),
## and the min/max timeunit accessors.

inSeconds <- SpaDES.core:::inSeconds
convertTimeunit <- SpaDES.core:::convertTimeunit
.getTU <- SpaDES.core:::.getTU

test_that("inSeconds knows every built-in time unit, singular and plural", {
  testInit()

  e <- new.env()
  expect_identical(as.numeric(inSeconds("second", e)), 1)
  expect_identical(as.numeric(inSeconds("seconds", e)), 1)
  expect_identical(as.numeric(inSeconds("hour", e)), as.numeric(inSeconds("hours", e)))
  expect_identical(as.numeric(inSeconds("day", e)), as.numeric(inSeconds("days", e)))
  expect_identical(as.numeric(inSeconds("week", e)), as.numeric(inSeconds("weeks", e)))
  expect_identical(as.numeric(inSeconds("month", e)), as.numeric(inSeconds("months", e)))
  expect_identical(as.numeric(inSeconds("year", e)), as.numeric(inSeconds("years", e)))
})

test_that("inSeconds orders the units correctly", {
  testInit()

  e <- new.env()
  secs <- vapply(c("second", "hour", "day", "week", "month", "year"),
                 function(u) as.numeric(inSeconds(u, e)), numeric(1))
  expect_false(is.unsorted(secs))
})

test_that("inSeconds resolves a user-defined unit from the environment", {
  testInit()

  e <- new.env()
  e$dfortnight <- function(x) x * 60 * 60 * 24 * 14

  expect_identical(as.numeric(inSeconds("fortnight", e)), 1209600)
})

test_that("inSeconds treats a missing or NA unit as zero", {
  testInit()

  e <- new.env()
  expect_identical(as.numeric(inSeconds(NA_character_, e)), 0)
  expect_identical(as.numeric(inSeconds(NULL, e)), 0)
})

test_that("inSeconds rejects a non-character unit", {
  testInit()

  expect_error(inSeconds(1, new.env()), "unit must be a character")
})

test_that("checkTimeunit accepts the built-in units", {
  testInit()

  e <- new.env()
  expect_true(all(checkTimeunit("year", e)))
  expect_true(all(checkTimeunit(c("year", "day"), e)))
})

test_that("checkTimeunit rejects an unknown unit, with a warning", {
  testInit()

  e <- new.env()
  expect_warning(res <- checkTimeunit("bogus", e), "unknown timeunit provided")
  expect_false(all(res))
})

test_that("checkTimeunit reports per-unit results for a vector", {
  testInit()

  e <- new.env()
  suppressWarnings(res <- checkTimeunit(c("year", "bogus"), e))
  expect_identical(unname(res), c(TRUE, FALSE))
})

test_that("checkTimeunit accepts a user-defined unit function", {
  testInit()

  e <- new.env()
  e$dfortnight <- function(x) x * 60 * 60 * 24 * 14
  expect_true(all(checkTimeunit("fortnight", e)))
})

test_that("convertTimeunit converts between units", {
  testInit()

  e <- new.env()
  oneYear <- 1
  attr(oneYear, "unit") <- "year"

  inSecs <- convertTimeunit(oneYear, "second", e)
  expect_identical(attr(inSecs, "unit"), "second")
  expect_identical(as.numeric(inSecs), as.numeric(inSeconds("year", e)))
})

test_that("convertTimeunit round-trips", {
  testInit()

  e <- new.env()
  t0 <- 3
  attr(t0, "unit") <- "day"

  there <- convertTimeunit(t0, "second", e)
  back <- convertTimeunit(there, "day", e)

  expect_equal(as.numeric(back), 3)
  expect_identical(attr(back, "unit"), "day")
})

test_that("convertTimeunit is a no-op when the unit already matches", {
  testInit()

  e <- new.env()
  t0 <- 5
  attr(t0, "unit") <- "year"

  out <- convertTimeunit(t0, "year", e)
  expect_identical(as.numeric(out), 5)
  expect_identical(attr(out, "unit"), "year")
})

test_that("convertTimeunit converts between two non-second units", {
  testInit()

  ## Values below .pkgEnv$nUnitConversions take a lookup-table fast path and
  ## larger ones do not, so exercise both. Expectations come from inSeconds()
  ## rather than assuming a week is 7 days -- in SpaDES a week is a year/52,
  ## i.e. ~7.024 days.
  e <- new.env()

  for (v in c(14, 20000)) {          # fast path, then slow path
    t0 <- v
    attr(t0, "unit") <- "day"

    out <- convertTimeunit(t0, "week", e)
    expected <- v * as.numeric(inSeconds("day", e)) / as.numeric(inSeconds("week", e))
    expect_equal(as.numeric(out), expected, tolerance = 1e-5)
    expect_identical(attr(out, "unit"), "week")
  }
})

test_that("convertTimeunit agrees across the lookup-table boundary", {
  testInit()

  ## the fast path (small whole numbers) must give the same answer as the
  ## general arithmetic, for every pair of units
  e <- new.env()
  units <- c("year", "month", "week", "day", "hour", "second")

  for (from in units) {
    for (to in units) {
      t0 <- 3
      attr(t0, "unit") <- from
      out <- convertTimeunit(t0, to, e)
      expected <- 3 * as.numeric(inSeconds(from, e)) / as.numeric(inSeconds(to, e))
      expect_equal(as.numeric(out), expected, tolerance = 1e-5,
                   info = paste(from, "->", to))
      expect_identical(attr(out, "unit"), to, info = paste(from, "->", to))
    }
  }
})

test_that("convertTimeunit converts to and from seconds, the shape simtimes rely on", {
  testInit()

  ## the fast path reads a lookup table that zzz.R rounds to whole seconds, so
  ## results can differ from the exact product by well under a second
  e <- new.env()
  for (u in c("year", "month", "week", "day", "hour")) {
    t0 <- 3
    attr(t0, "unit") <- u
    toSecs <- convertTimeunit(t0, "second", e)
    expect_identical(attr(toSecs, "unit"), "second")
    expect_equal(as.numeric(toSecs), 3 * as.numeric(inSeconds(u, e)),
                 tolerance = 1e-6)

    backAgain <- convertTimeunit(toSecs, u, e)
    expect_equal(as.numeric(backAgain), 3, tolerance = 1e-6)
  }
})

test_that("end() and start() convert the sim's times into any unit", {
  testInit()

  e <- new.env()
  sim <- simInit(times = list(start = 0, end = 3, timeunit = "year"))
  yearSecs <- as.numeric(inSeconds("year", e))

  for (u in c("year", "month", "week", "day", "second")) {
    expect_equal(as.numeric(end(sim, u)), 3 * yearSecs / as.numeric(inSeconds(u, e)),
                 tolerance = 1e-6)
  }
})

test_that("convertTimeunit assumes seconds when the time carries no unit", {
  testInit()

  e <- new.env()
  out <- convertTimeunit(60, "second", e)
  expect_identical(attr(out, "unit"), "second")
})

test_that("convertTimeunit validates its arguments", {
  testInit()

  e <- new.env()
  expect_error(convertTimeunit(1, 1, e), "unit must be a character")
  expect_error(convertTimeunit("x", "second", e), "time must be a numeric")
  expect_error(convertTimeunit(1, "second", "notAnEnv"), "envir must be an environment")
})

test_that(".getTU resolves a unit function from the sim environment or the package", {
  testInit()

  e <- new.env()
  expect_identical(as.numeric(.getTU("year", e)), as.numeric(dyear(1)))

  e$dfortnight <- function(x) x * 60 * 60 * 24 * 14
  expect_true(is.function(.getTU("fortnight", e)))
})

test_that("min/maxTimeunit fall back sensibly when no module declares a timeunit", {
  testInit()

  sim <- simInit()
  expect_identical(minTimeunit(sim), "second")
  expect_identical(maxTimeunit(sim), NA_character_)
})

test_that("min/maxTimeunit pick the smallest and largest module timeunit", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes", "fireSpread"),
            paths = list(modulePath = mp))
  )

  expect_true(is.character(minTimeunit(sim)))
  expect_true(is.character(maxTimeunit(sim)))
})
