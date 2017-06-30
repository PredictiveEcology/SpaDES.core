test_that(".pkgEnv functions work", {
  test1 <- 1L:10L

  assign("test1", test1, envir = SpaDES.core:::.pkgEnv)
  expect_true(exists("test1", envir = SpaDES.core:::.pkgEnv))
  expect_equal(test1, get("test1", envir = SpaDES.core:::.pkgEnv))
})
