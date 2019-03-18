test_that("Copy does not work correctly", {
  testInitOut <- testInit(smcc = FALSE, libraries = "data.table")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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

test_that("Fork cluster does not work correctly for data.table", {
  # THIS ISN"T CORRECTLY FINDING THE PROBLEM YET
  if (interactive() && .Platform$OS.type == "unix")  {
    testInitOut <- testInit(smcc = FALSE, libraries = c("data.table", "parallel"))
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    cl <- SpaDES.core:::.makeClusterRandom(5)
    on.exit(stopCluster(cl))

    dt <- data.table(a = LETTERS, b = sample(letters))

    # copy -- breaks connection between data.tables

    fun <- function(dt) {
      setkeyv(dt, sample(c("a", "b"), size = 1))
      dt
    }

    clusterExport(cl = cl, c("fun", "dt"), envir = environment())
    clusterEvalQ(cl, {
      require(data.table)
    })
    clusterEvalQ(cl, {
      fun(dt = dt)
    })
  }
})
