test_that("Copy does not work correctly", {
  testInit(smcc = FALSE, libraries = "data.table")

  s <- simInit()
  s$dt <- data.table(a = 1)
  s$dt1 <- data.table(a = 2)
  s$dt3 <- data.table(a = 3)
  s$dt4 <- data.frame(a = 4)

  # copy -- breaks connection between data.tables
  s1 <- Copy(s)
  s1$dt[, a := 5]
  expect_false(identical(s1$dt, s$dt))
  expect_true(identical(s1$dt1, s$dt1))

  # non-copy -- keeps the data.table correct
  s2 <- s
  s2$dt[, a := 6]
  expect_true(identical(s2$dt, s$dt))
  expect_true(identical(s2$dt1, s$dt1))
})
