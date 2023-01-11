library(testthat)
aa <- options(spades.debug = FALSE)
options("spades.temp.debug" = aa)
if (FALSE) {
  ff <- list()
  runTestsWithTimings("ff")
}
test_check("SpaDES.core")
options(aa)
