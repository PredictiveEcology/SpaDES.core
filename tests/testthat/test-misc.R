test_that("misc tests", {
  testInit(smcc = FALSE, debug = FALSE)

  expect_error(needInstall("httr2", minVersion = "1.4.1"), "is required")
  expect_error(needInstall("httr", minVersion = "100000.4.1"), "httr\\(>=")
  expect_silent(needInstall("reproducible"))
  expect_error(needInstall("httr2", minVersion = "1.4.1"), "install\\.packages")
})

test_that(".emptyEventList tests", {
  testInit(smcc = FALSE, libraries = "data.table")
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
  testInit(smcc = FALSE)

  pkgToAttach <- "cli"
  spPre <- search()
  a <- .modifySearchPath(pkgs = pkgToAttach, skipNamespacing = FALSE)
  spPost <- search()
  expect_true(identical(grep(pkgToAttach, spPost), 2L))
  detach("package:cli")
  spPost2 <- search()
  expect_false(identical(grep(pkgToAttach, spPost2), 2L))

  # use removeOthers
  pkgToAttach <- c("cli", "reproducible", "quickPlot")
  spPre <- search()
  a <- .modifySearchPath(pkgs = pkgToAttach, removeOthers = TRUE, skipNamespacing = FALSE)
  spPost <- search()
  expect_true(any(grep(pkgToAttach[1], spPost) == 2L))
  expect_true(identical(grep("digest", spPost), integer()))
  detach("package:cli")
  spPost2 <- search()
  expect_false(any(grep(pkgToAttach[1], spPost2) == 2L))
})

test_that("test all.equal.simList", {
  testInit(smcc = FALSE, "digest")

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

test_that("FilterRecursive works", {
  oldFns <- list(
    scfmDriverPars = list(
      `1` = list(
        pSpread = character(0),
        p0 = character(0),
        naiveP0 = character(0),
        pIgnition = character(0),
        maxBurnCells = character(0),
        uniroot.Res = list(
          root = character(0),
          f.root = character(0),
          iter = character(0),
          init.it = character(0),
          estim.prec = character(0)
        )
      ),
      `2` = list(
        pSpread = character(0),
        p0 = character(0),
        naiveP0 = character(0),
        pIgnition = character(0),
        maxBurnCells = character(0),
        uniroot.Res = list(
          root = character(0),
          f.root = character(0), iter = character(0), init.it = character(0),
          estim.prec = character(0)
        )
      ),
      `3` = list(
        pSpread = character(0),
        p0 = character(0), naiveP0 = character(0), pIgnition = character(0),
        maxBurnCells = character(0),
        uniroot.Res = list(
          root = character(0),
          f.root = character(0), iter = character(0), init.it = character(0),
          estim.prec = character(0)
        )
      ),
      `4` = list(
        pSpread = character(0),
        p0 = character(0), naiveP0 = character(0), pIgnition = character(0),
        maxBurnCells = character(0),
        uniroot.Res = list(
          root = character(0),
          f.root = character(0), iter = character(0), init.it = character(0),
          estim.prec = character(0)
        )
      )
    ),
    scfmRegimePars = list(
      `1` = list(
        ignitionRate = character(0), pEscape = character(0), xBar = character(0),
        lxBar = character(0), xMax = character(0), emfs_ha = character(0),
        empiricalBurnRate = character(0)
      ),
      `2` = list(
        ignitionRate = character(0),
        pEscape = character(0), xBar = character(0), lxBar = character(0),
        xMax = character(0), emfs_ha = character(0), empiricalBurnRate = character(0)
      ),
      `3` = list(
        ignitionRate = character(0), pEscape = character(0),
        xBar = character(0), lxBar = character(0), xMax = character(0),
        emfs_ha = character(0), empiricalBurnRate = character(0)
      ),
      `4` = list(
        ignitionRate = character(0), pEscape = character(0),
        xBar = character(0), lxBar = character(0), xMax = character(0),
        emfs_ha = character(0), empiricalBurnRate = character(0)
      )
    )
  )

  expect_equivalent(FilterRecursive(length, oldFns), list())
})
