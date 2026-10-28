test_that(".chainRan treats a missing newCache as 'not newly computed'", {
  mk <- function(...) {
    sim <- new("simList")
    attr(sim, ".Cache") <- list(...)
    sim
  }
  noJump <- list(jump = NULL)
  # newCache absent: used to error (NULL %in% TRUE is logical(0))
  expect_false(.chainRan(mk(), TRUE, noJump))
  expect_null(.chainDeltaIfRan(mk(), TRUE, noJump, list(), 0))
  expect_null(.chainOutTextIfRan(mk(), TRUE, noJump, NULL))
  # unchanged cases
  expect_true(.chainRan(mk(newCache = TRUE), TRUE, noJump))
  expect_false(.chainRan(mk(newCache = FALSE), TRUE, noJump))
  expect_false(.chainRan(mk(newCache = TRUE), FALSE, noJump))
  expect_false(.chainRan(mk(newCache = TRUE), TRUE, list(jump = 1)))
})

test_that(".newlyCached and .loadedFromCache are never NA or length zero", {
  withNC <- function(v) structure(1, .Cache = list(newCache = v))
  expect_identical(c(.newlyCached(withNC(TRUE)), .loadedFromCache(withNC(TRUE))), c(TRUE, FALSE))
  expect_identical(c(.newlyCached(withNC(FALSE)), .loadedFromCache(withNC(FALSE))), c(FALSE, TRUE))
  for (x in list(withNC(NULL), structure(1, .Cache = list()), 1, NULL)) {
    expect_identical(.newlyCached(x), FALSE)
    expect_identical(.loadedFromCache(x), FALSE)
  }
})
