## defineParameter() edge cases: the no-argument form used to build an empty
## parameter table, and the min/max coercion that happens when neither is given.

test_that("defineParameter with no arguments returns an empty parameter table", {
  testInit()

  p <- defineParameter()

  expect_s3_class(p, "data.frame")
  expect_identical(NROW(p), 0L)
  expect_identical(names(p),
                   c("paramName", "paramClass", "default", "min", "max", "paramDesc"))
})

test_that("defineParameter fills min and max with a typed NA when neither is given", {
  testInit()

  p <- defineParameter("alpha", "numeric", 1, desc = "a")

  expect_true(is.na(p$min[[1]]))
  expect_true(is.na(p$max[[1]]))
  ## the NA takes the class of the parameter, not the default logical NA
  expect_type(p$min[[1]], "double")
  expect_type(p$max[[1]], "double")
})

test_that("defineParameter types the NA for each of the atomic classes", {
  testInit()

  expect_type(defineParameter("a", "character", "x", desc = "")$min[[1]], "character")
  expect_type(defineParameter("a", "integer", 1L, desc = "")$min[[1]], "integer")
  expect_type(defineParameter("a", "logical", TRUE, desc = "")$min[[1]], "logical")
})

test_that("defineParameter picks the class the default actually matches", {
  testInit()

  ## more than one class is allowed; the NA follows whichever one `default` is
  p <- defineParameter("a", c("numeric", "character"), 1, desc = "")

  expect_type(p$min[[1]], "double")
})

test_that("defineParameter falls back to the first class when the default matches none", {
  testInit()

  p <- defineParameter("a", c("integer", "character"), 1.5, desc = "")

  expect_type(p$min[[1]], "integer")
})

test_that("defineParameter leaves min and max as plain NA for a non-atomic class", {
  testInit()

  p <- defineParameter("a", "list", list(1), desc = "")

  expect_true(is.na(p$min[[1]]))
  expect_type(p$min[[1]], "logical")
})

test_that("defineParameter keeps an explicitly supplied min and max", {
  testInit()

  p <- defineParameter("alpha", "numeric", 1, 0, 10, "a")

  expect_identical(p$min[[1]], 0)
  expect_identical(p$max[[1]], 10)
})
