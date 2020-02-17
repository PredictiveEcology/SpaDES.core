test_that("misc tests", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE)

  expect_error(needInstall("httr2", minVersion = "1.4.1"), "is required")
  expect_error(needInstall("httr", minVersion = "100000.4.1"), "httr\\(>=")
  expect_silent(needInstall("reproducible"))
  expect_error(needInstall("httr2", minVersion = "1.4.1"), "install\\.packages")

})
