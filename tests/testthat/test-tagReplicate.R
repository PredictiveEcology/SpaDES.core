## tagReplicate(): a table read from one replicate's outputs takes that replicate, and rows naming another
## replicate stop.

test_that("tagReplicate sets the replicate column, filling missing values", {
  x <- tagReplicate(data.frame(rep = c(NA, 2L), size = 1:2), id = 2L)
  expect_identical(x$rep, c(2L, 2L))
  ## no replicate column yet
  expect_identical(tagReplicate(data.frame(size = 1:2), id = 3L)$rep, c(3L, 3L))
  ## another column name, and ids as text (e.g. "rep01")
  y <- tagReplicate(data.frame(replicate = c("rep01", NA)), id = "rep01", col = "replicate")
  expect_identical(y$replicate, c("rep01", "rep01"))
  ## a number and its text agree
  expect_identical(tagReplicate(data.frame(rep = "1"), id = 1L)$rep, 1L)
})

test_that("tagReplicate modifies a data.table in place", {
  dt <- data.table::data.table(rep = NA_integer_, size = 1L)
  tagReplicate(dt, id = 4L)
  expect_identical(dt$rep, 4L)
})

test_that("tagReplicate stops on rows naming another replicate, saying where they came from", {
  expect_error(tagReplicate(data.frame(rep = c(1L, 3L)), id = 1L, source = "out/rep01/fires.csv"),
               "out/rep01/fires.csv: rows for replicate 3 in the outputs of replicate 1")
})

test_that("tagReplicate keeps a table with no rows", {
  x <- tagReplicate(data.frame(rep = integer(0), size = integer(0)), id = 1L)
  expect_identical(nrow(x), 0L)
  expect_identical(x$rep, integer(0))
})
