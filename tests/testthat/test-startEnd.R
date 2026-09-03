## startEnd() and resolveSimYears(): the single, unvarying way a summary module
## decides which two years it is summarising.

simFor <- function() {
  simInit(times = list(start = 1991, end = 2020, timeunit = "year"))
}

test_that("startEnd returns the sim's start and end as a named length-2 numeric", {
  skip_on_cran()
  testInit()

  se <- startEnd(simFor())

  expect_length(se, 2)
  expect_identical(names(se), c("start", "end"))
  expect_equal(unname(se), c(1991, 2020), ignore_attr = TRUE)
})

test_that("resolveSimYears keeps the parameter when it is supplied", {
  skip_on_cran()
  testInit()

  expect_equal(resolveSimYears(c(2011, 2100), simFor()), c(2011, 2100))
})

test_that("resolveSimYears falls back to the sim clock when the parameter is all NA", {
  skip_on_cran()
  testInit()

  sim <- simFor()

  expect_equal(unname(resolveSimYears(c(NA, NA), sim)), c(1991, 2020), ignore_attr = TRUE)
  expect_equal(resolveSimYears(c(NA_real_, NA_real_), sim), startEnd(sim))
})

test_that("resolveSimYears keeps a partially specified parameter as-is", {
  skip_on_cran()
  testInit()

  ## only *all* NA means "use the sim clock"
  expect_equal(resolveSimYears(c(2011, NA), simFor()), c(2011, NA))
})
