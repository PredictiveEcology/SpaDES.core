## singularPlural()/isAre(): vendored from Require, so these pin the behaviour
## the two copies have to keep sharing -- in particular that `v` wins when both
## `l` and `v` are supplied, which in Require's original came about only because
## the `v` assignment happened to come second.

test_that("singularPlural picks by count v", {
  testInit()

  expect_identical(singularPlural(c("is", "are"), v = 1), "is")
  expect_identical(singularPlural(c("is", "are"), v = 2), "are")
})

test_that("singularPlural treats a zero count as singular", {
  testInit()

  ## only > 1 is plural, matching Require
  expect_identical(singularPlural(c("is", "are"), v = 0), "is")
})

test_that("singularPlural picks by the length of l", {
  testInit()

  expect_identical(singularPlural(c("is", "are"), l = 1), "is")
  expect_identical(singularPlural(c("is", "are"), l = c(1, 2)), "are")
  expect_identical(singularPlural(c("is", "are"), l = character(0)), "is")
})

test_that("singularPlural lets v win when both are given", {
  testInit()

  expect_identical(singularPlural(c("is", "are"), l = c(1, 2, 3), v = 1), "is")
  expect_identical(singularPlural(c("is", "are"), l = 1, v = 5), "are")
})

test_that("isAre is singularPlural over is/are", {
  testInit()

  expect_identical(isAre(v = 1), "is")
  expect_identical(isAre(v = 2), "are")
  expect_identical(isAre(l = c("a", "b")), "are")
})

test_that("the vendored copies agree with Require's", {
  testInit()
  skip_if_not_installed("Require")

  reqSP <- getFromNamespace("singularPlural", "Require")
  reqIA <- getFromNamespace("isAre", "Require")

  for (n in c(0, 1, 2, 5)) {
    expect_identical(singularPlural(c("is", "are"), v = n),
                     reqSP(c("is", "are"), v = n))
    expect_identical(isAre(v = n), reqIA(v = n))
  }
  expect_identical(singularPlural(c("is", "are"), l = c(1, 2)),
                   reqSP(c("is", "are"), l = c(1, 2)))
  ## both supplied: v takes precedence in each
  expect_identical(singularPlural(c("is", "are"), l = c(1, 2, 3), v = 1),
                   reqSP(c("is", "are"), l = c(1, 2, 3), v = 1))
})
