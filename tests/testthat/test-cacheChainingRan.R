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
