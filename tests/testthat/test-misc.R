test_that("misc tests", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  expect_error(needInstall("httr2", minVersion = "1.4.1"), "is required")
  expect_error(needInstall("httr", minVersion = "100000.4.1"), "httr\\(>=")
  expect_silent(needInstall("reproducible"))
  expect_error(needInstall("httr2", minVersion = "1.4.1"), "install\\.packages")

})

test_that(".emptyEventList tests", {
  testInitOut <- testInit(smcc = FALSE, libraries = "data.table")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  a <- .emptyEventList()
  expect_true(is(a, "data.table"))
  b <- .emptyEventList(1, "a", "b", 5)
  b1 <- data.table(1, "a", "b", 5)
  expect_true(is(b, "data.table"))
  expect_true(NROW(b) == 1)
  expect_false(identical(.singleEventListDT, b))
  expect_null(key(b))
  expect_true(identical(unname(as.matrix(b)), unname(as.matrix(b1))))

})

test_that("modify search path", {
  testInitOut <- testInit(smcc = FALSE, "fastdigest")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  pkgToAttach <- "crayon"
  spPre <- search()
  a <- .modifySearchPath(pkgs = pkgToAttach, skipNamespacing = FALSE)
  spPost <- search()
  expect_true(identical(grep(pkgToAttach, spPost), 2L))
  detach("package:crayon")
  spPost2 <- search()
  expect_false(identical(grep(pkgToAttach, spPost2), 2L))

  # use removeOthers
  pkgToAttach <- c("crayon", "reproducible", "quickPlot")
  spPre <- search()
  a <- .modifySearchPath(pkgs = pkgToAttach, removeOthers = TRUE, skipNamespacing = FALSE)
  spPost <- search()
  expect_true(any(grep(pkgToAttach[1], spPost) == 2L))
  expect_true(identical(grep("fastdigest", spPost), integer()))
  detach("package:crayon")
  spPost2 <- search()
  expect_false(any(grep(pkgToAttach[1], spPost2) == 2L))


})

test_that("test all.equal.simList", {
  testInitOut <- testInit(smcc = FALSE, "fastdigest")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  s1 <- simInit()
  s2 <- simInit()
  s1 <- spades(s1)
  s2 <- spades(s2)
  a1 <- s1$._firstEventClockTime
  a2 <- s2$._firstEventClockTime
  expect_true(all.equal(s1, s2)) # the all.equal method used to remove these permanently
  b1 <- s1$._firstEventClockTime
  b2 <- s2$._firstEventClockTime
  expect_true(all.equal(a1, b1))
  expect_true(all.equal(a2, b2))

})
