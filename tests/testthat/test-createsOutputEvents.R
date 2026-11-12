test_that("createsOutput has `events` as a formal argument", {
  expect_true("events" %in% names(formals(createsOutput)))
  for (m in c("ANY", "character")) {
    meth <- getMethod("createsOutput", signature = c(objectName = m, objectClass = m, desc = m))
    expect_true("events" %in% names(formals(meth)), info = m)
  }
})

test_that("createsOutput(events = ) reaches the column through both methods", {
  chr <- createsOutput("o", "numeric", "d", events = c("init", "grow"))
  expect_identical(chr$events[[1]], c("init", "grow"))
  any <- createsOutput(1, 2, "d", events = "init")
  expect_identical(any$events[[1]], "init")
  expect_identical(any$objectName, "1")
  ## other named arguments still become columns
  both <- createsOutput("o", "numeric", "d", other = "z", events = "init")
  expect_identical(both$other[[1]], "z")
  expect_identical(both$events[[1]], "init")
})

test_that("positional extra desc text is pasted onto desc once", {
  expect_identical(createsOutput("o", "numeric", "a ", "b")$desc, "a b")
  expect_identical(createsOutput(1, 2, "a ", "b", events = "init")$desc, "a b")
})

test_that("events must be character", {
  expect_error(createsOutput("o", "numeric", "d", events = 1), "events")
  expect_error(createsOutput(1, 2, "d", events = 1), "events")
})

test_that("a call without events gives the same data.frame as before", {
  x <- createsOutput("o", "numeric", "d1 ", "d2", other = "z")
  expect_identical(colnames(x), c("objectName", "objectClass", "desc", "other"))
  expect_false("events" %in% colnames(createsOutput("o", "numeric", "d")))
  skip_if_not(file.exists(Sys.getenv("CREATESOUTPUT_BEFORE")))
  before <- readRDS(Sys.getenv("CREATESOUTPUT_BEFORE"))
  expect_identical(before$a, x)
  expect_identical(before$b, createsOutput(c("o", "p"), "numeric", "d"))
  expect_identical(before$c, createsOutput(1, 2, "d"))
})
