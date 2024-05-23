test_that("local mod object", {
  testInit(smcc = FALSE, debug = FALSE, verbose = TRUE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)
  testFilePath <- file.path(tmpdir, "test", "test.R")
  test2FilePath <- file.path(tmpdir, "test2", "test2.R")

  ## 2018-09-18: Changed to use "seconds" -- better comparison with simple loop
  cat(file = testFilePath, testCode, fill = TRUE)
  cat(file = test2FilePath, test2Code, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"))

  expect_true(mySim$.mods$test2$.objects$y == "This module")
  out2 <- spades(Copy(mySim))
  out3 <- Cache(spades, Copy(mySim)) ## TODO: failure due to NULL current module
  mess <- capture_messages({
    out4 <- Cache(spades, Copy(mySim)) # should get cached
  })
  out <- spades(mySim)

  ## Test the Par stuff
  expect_true(identical(out2$testPar1, params(out2)$test$testParA))
  expect_true(identical(out2$testPar2, params(out2)$test2$testParB))
  expect_true(identical(out$testPar1, params(out)$test$testParA))
  expect_true(identical(out$testPar2, params(out)$test2$testParB))
  expect_true(identical(out3$testPar1, params(out3)$test$testParA))
  expect_true(identical(out3$testPar2, params(out3)$test2$testParB))
  expect_true(identical(out4$testPar1, params(out4)$test$testParA))
  expect_true(identical(out4$testPar2, params(out4)$test2$testParB))

  ## Test the results
  expect_true(out$.mods$test$.objects$a == 2) # object that results from addition
  expect_true(out$.mods$test2$.objects$a == 1) # object that results from addition
  expect_true(out$.mods$test2$.objects$b == 2) # object that results from addition -- didn't collide with sim$test$a
  expect_true(out$.mods$test$.objects$x == "sdf") # correct module, i.e., x is in test, and is sdf
  expect_true(is.null(out$.mods$test2$.objects$x)) # wrong module, i.e., x is in test
  expect_true(!is.null(mySim$.mods$test$.objects$x)) # .inputObjects is run
  expect_true(!is.null(mySim$.mods$test2$.objects$y)) # .inputObjects is run
  expect_true(out$.mods$test2$.objects$y == "This module is test2") # paste0 from .inputObjects & event1 event

  ## Post Copy(mySim)
  expect_true(out2$.mods$test$.objects$a == 2)
  expect_true(out2$.mods$test2$.objects$a == 1)
  expect_true(out2$.mods$test2$.objects$b == 2)
  expect_true(is.null(out2$.mods$test2$.objects$x))
  expect_true(!is.null(out2$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out2$.mods$test2$.objects$y == "This module is test2")

  ## Cache -- using the first time through
  expect_true(out3$.mods$test$.objects$a == 2)
  expect_true(out3$.mods$test2$.objects$a == 1)
  expect_true(out3$.mods$test2$.objects$b == 2)
  expect_true(is.null(out3$.mods$test2$.objects$x))
  expect_true(!is.null(out3$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out3$.mods$test2$.objects$y == "This module is test2")

  ## Cached copy
  expect_true(any(grepl("Loaded! Cached", mess)))
  expect_true(out4$.mods$test$.objects$a == 2) ## TODO: fails (gets NULL)
  expect_true(out4$.mods$test2$.objects$a == 1) ## TODO: fails (gets NULL)
  expect_true(out4$.mods$test2$.objects$b == 2) ## TODO: fails (gets NULL)
  expect_true(is.null(out4$.mods$test2$.objects$x))
  expect_true(!is.null(out4$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out4$.mods$test2$.objects$y == "This module is test2") ## TODO: fails (gets 'This module')

  ## Test P replace method
  mySim3 <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                   params = list(.globals = list(testParB = 543)))

  ## Need "Copy" in this sequence because the event queue is actually an environment :)
  ## so the LHS will have the updated event queue, but the parameters will be at initial conditions
  expect_true(P(mySim3)$test2$testParB == 1197) # .globals + .inputObjects
  mySim4 <- spades(Copy(mySim3), events = "init")
  expect_true(P(mySim4)$test2$testParB == 1953) ## .globals + .inputObjects + init
  mySim4 <- spades(Copy(mySim3))
  expect_true(P(mySim4)$test2$testParB == 1953 + 800) # # .globals + .inputObjects + init + event1 ran

  mySim5 <- Cache(spades, Copy(mySim3)) # should get cached -- event1 runs 1x
  expect_true(P(mySim5)$test2$testParB == 1953 + 800)

  end(mySim5) <- 1
  mySim6 <- Cache(spades, Copy(mySim5)) # doesn't change because only test1 is scheduled
  expect_true(P(mySim6)$test2$testParB == 1953 + 800)

  end(mySim6) <- 2
  mySim7 <- Cache(spades, Copy(mySim6)) # should get cached
  expect_true(P(mySim7)$test2$testParB == 1953 + 800 * 2)

  warns <- capture_warnings({
    mySim3 <- simInit(times = list(start = 0, end = 0),
                      paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                      params = list(.globals = list(testParB = 321321)))
  })
  expect_true(grepl("P has changed", warns))

  ## test different ways of setting parameters
  expect_true(identical(P(mySim7, module = "test2", "testParA"), 42))
  expect_true(identical(P(mySim7, module = "test2", "testParF"), 77))
  expect_true(identical(P(mySim7, module = "test2", "testParG"), 79))
  expect_true(identical(P(mySim7, module = "test2", "testParH"), 48))

  ## Test common parameters i.e., globals
  ## Set one to NULL
  vals <- list("sdfd", "default", NULL, "ffff")
  lens <- list(0,0,0,1)
  out <- Map(len = lens, val = vals, function(len, val) {
    mySim11 <- simInit(times = list(start = 0, end = 0),
                       paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                       params = list(test = list(testCommonPar = vals[[1]]),
                                     test2 = list(testCommonPar = val)))
    mess <- capture_messages(spades(mySim11, debug = FALSE))
    expect_true(sum(grepl("multiple values", mess)) == len)
    expect_true(sum(grepl("There was a warning", mess)) == len)
    expect_true(sum(grepl("There was an error", mess)) == len)
  })

  ## Test restartSpades # The removal of the completed ... it shouldn't, but it did previously
  if (interactive()) {
    withr::local_options(spades.recoveryMode = TRUE)

    mySim8 <- simInit(times = list(start = 0, end = 0),
                      paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                      params = list(test2 = list(testRestartSpades = 1)))
    ss <- try(spades(mySim8, debug = FALSE), silent = TRUE)

    sim <- savedSimEnv()$.sim ## TODO: retrieves NULL
    expect_true(is(sim, "simList"))

    err <- capture_error({
      sim2 <- restartSpades(sim, debug = FALSE)
    }) # is missing completed events

    sim <- savedSimEnv()$.sim
    err <- capture_error({
      sim3 <- restartSpades(sim, debug = FALSE)
    }) # is missing completed events

    sim <- savedSimEnv()$.sim
    sim@params$test2$testRestartSpades <- NULL
    sim3 <- restartSpades(sim, debug = FALSE)
    expect_true(NROW(completed(sim3)) == 7)
  }
})

test_that("convertToPackage testing", {
  skip_if_not_installed("pkgload")
  skip_on_cran()

  testInit(c("roxygen2", "ggplot2"), smcc = FALSE, debug = FALSE,
           opts = list(reproducible.useMemoise = FALSE,
                       spades.moduleDocument = TRUE))

  testName1 <- paste0("test.", .rndstr(len = 2))
  testName2 <- paste0("test2.", .rndstr(len = 1))
  mainModFile1 <- paste0(testName1, ".R")
  mainModFile2 <- paste0(testName2, ".R")
  try(pkgload::unload(testName1), silent = TRUE)
  try(pkgload::unload(testName2), silent = TRUE)

  on.exit({
    try(pkgload::unload(testName1), silent = TRUE)
    try(pkgload::unload(testName2), silent = TRUE)
  }, add = TRUE)

  newModule(testName1, tmpdir, open = FALSE)
  newModule(testName2, tmpdir, open = FALSE)
  testFilePath <- file.path(tmpdir, testName1, mainModFile1)
  test2FilePath <- file.path(tmpdir, testName2, mainModFile2)

  # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop

  testCodeMod <- gsub("test", testName1, testCode)
  test2CodeMod <- gsub("test2", testName2, test2Code)

  cat(file = testFilePath, testCodeMod, fill = TRUE)

  cat(file = test2FilePath, test2CodeMod, fill = TRUE)
  # Test converting these to packages
  cat(file = testFilePath,'
      #\' @title Init
      #\' @rdname Init
      #\' @name Init
      #\' @param sim A simList
      Init <- function(sim) {
        sim$aaaa <- Run(1)
        return(sim)
      }

      #\' @title Run
      #\' @name Run
      #\' @param a An object
      Run <- function(a) {
        return(a + 1)
      }
      ', fill = TRUE, append = TRUE)

  cat(file = test2FilePath,'
      Init <- function(sim) {
        # Need to keep comments
        sim$cccc <- try(Run(1), silent = TRUE)
        return(sim)
      }

      Run <- function(a) {
        return(a + 2)
      }
      ', fill = TRUE, append = TRUE)

  for (tt in c(testName1, testName2)) {
    expect_false(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    expect_false(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
  }

  convertToPackage(module = testName1, path = tmpdir, buildDocuments = FALSE)
  convertToPackage(module = testName2, path = tmpdir, buildDocuments = FALSE)

  for (tt in c(testName1, testName2)) {
    expect_true(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    expect_true(!file.exists(file.path(tmpdir, tt, "NAMESPACE")))
    expect_true(dir.exists(file.path(tmpdir, tt, "R")))
    ## list.files(file.path(tmpdir, tt, "R"))
    expect_true(file.exists(file.path(tmpdir, tt, "R", "Init.R"))) ## TODO: file does't exist
    expect_true(file.exists(file.path(tmpdir, tt, "R", "Run.R"))) ## TODO: file does't exist
  }

  mySim9 <- simInit(times = list(start = 0, end = 1),
                    paths = list(modulePath = tmpdir), modules = c(testName1, testName2))

  # doesn't document, unless it is first time
  for (tt in c(testName1, testName2)) {
    expect_true(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    expect_true(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
    expect_true(dir.exists(file.path(tmpdir, tt, "R")))
  }
  working <- spades(mySim9, debug = FALSE)

  # if (requireNamespace("roxygen2")) {
    # document -- this exports all functions!! Danger for testing later
    # out <- lapply(c(testName1, testName2), function(tt) {
    #   roxygen2::roxygenise(file.path(tmpdir, tt))
    # })

    # Will run document() so will have the NAMESPACE and
    for (tt in c(testName1, testName2)) {
      expect_true(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
      expect_true(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
      expect_true(sum(grepl("export.+doEvent", readLines(file.path(tmpdir, tt, "NAMESPACE")))) == 1)
    }

    # check that inheritance is correct -- Run is in the namespace, Init also... doEvent calls Init calls Run
    expect_true(is(working, "simList"))
    expect_true(working$aaaa == 2)
    expect_true(is(working$cccc, "try-error"))
    bbb <- get("Run", asNamespace(testName2))(2)
    fnTxt <- readLines(file.path(dirname(test2FilePath), "R", "Init.R"))
    expect_true(sum(grepl("Need to keep comments", fnTxt)) == 1)
    expect_true(bbb == 4)
    pkgload::unload(testName1)
    pkgload::unload(testName2)
  #}
})
