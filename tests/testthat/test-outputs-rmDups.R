## rmDups(): drops duplicated rows from sim@outputs. The `arguments` column is
## an AsIs list, which data.table cannot uniquify on, so those columns are
## handled by a second pass -- that pass is what these tests drive.

outputsTable <- function(objectName, file, arguments = NULL) {
  n <- length(objectName)
  if (is.null(arguments)) arguments <- vector("list", n)
  data.table::data.table(
    objectName = objectName, saveTime = rep(1, n), file = file,
    fun = rep("saveRDS", n), package = rep("base", n),
    saved = rep(NA, n), arguments = I(arguments)
  )
}

test_that("rmDups removes an exactly duplicated output row", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  sim@outputs <- outputsTable(c("a", "a", "b"), c("f1", "f1", "f2"))

  out <- SpaDES.core:::rmDups(sim)

  expect_identical(NROW(out@outputs), 2L)
  expect_identical(out@outputs$objectName, c("a", "b"))
})

test_that("rmDups keeps rows that differ in the file they save to", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  sim@outputs <- outputsTable(c("a", "a"), c("f1", "f2"))

  out <- SpaDES.core:::rmDups(sim)

  expect_identical(NROW(out@outputs), 2L)
})

test_that("rmDups keeps rows whose AsIs arguments differ", {
  skip_on_cran()
  testInit()

  ## everything data.table can compare is identical, so only the `arguments`
  ## column distinguishes these two rows -- they must both survive
  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  sim@outputs <- outputsTable(c("a", "a"), c("f1", "f1"),
                              arguments = list(list(compress = TRUE),
                                               list(compress = FALSE)))

  out <- SpaDES.core:::rmDups(sim)

  expect_identical(NROW(out@outputs), 2L)
})

test_that("rmDups leaves a table with no duplicates untouched", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  tbl <- outputsTable(c("a", "b", "c"), c("f1", "f2", "f3"))
  sim@outputs <- tbl

  out <- SpaDES.core:::rmDups(sim)

  expect_identical(NROW(out@outputs), 3L)
  expect_identical(out@outputs$objectName, c("a", "b", "c"))
})
