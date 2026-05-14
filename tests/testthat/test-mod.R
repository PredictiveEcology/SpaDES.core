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

  expect_true(mySim[[dotObjs]]$test2$y == "This module")
  # expect_true(mySim$.mods$test2$.objects$y == "This module")
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
  expect_true(out[[dotObjs]]$test$a == 2) # object that results from addition
  expect_true(out[[dotObjs]]$test2$a == 1) # object that results from addition
  expect_true(out[[dotObjs]]$test2$b == 2) # object that results from addition -- didn't collide with sim$test$a
  expect_true(out[[dotObjs]]$test$x == "sdf") # correct module, i.e., x is in test, and is sdf
  expect_true(is.null(out[[dotObjs]]$test2$x)) # wrong module, i.e., x is in test
  expect_true(!is.null(mySim[[dotObjs]]$test$x)) # .inputObjects is run
  expect_true(!is.null(mySim[[dotObjs]]$test2$y)) # .inputObjects is run
  expect_true(out[[dotObjs]]$test2$y == "This module is test2") # paste0 from .inputObjects & event1 event

  ## Post Copy(mySim)
  expect_true(out2[[dotObjs]]$test$a == 2)
  expect_true(out2[[dotObjs]]$test2$a == 1)
  expect_true(out2[[dotObjs]]$test2$b == 2)
  expect_true(is.null(out2[[dotObjs]]$test2$x))
  expect_true(!is.null(out2[[dotObjs]]$test$x)) # was made in .inputObjects, copies fine
  expect_true(out2[[dotObjs]]$test2$y == "This module is test2")

  ## Cache -- using the first time through
  expect_true(out3[[dotObjs]]$test$a == 2)
  expect_true(out3[[dotObjs]]$test2$a == 1)
  expect_true(out3[[dotObjs]]$test2$b == 2)
  expect_true(is.null(out3[[dotObjs]]$test2$x))
  expect_true(!is.null(out3[[dotObjs]]$test$x)) # was made in .inputObjects, copies fine
  expect_true(out3[[dotObjs]]$test2$y == "This module is test2")

  ## Cached copy
  expect_true(any(grepl("Loaded! Cached", mess)))
  expect_true(out4[[dotObjs]]$test$a == 2) ## TODO: fails (gets NULL)
  expect_true(out4[[dotObjs]]$test2$a == 1) ## TODO: fails (gets NULL)
  expect_true(out4[[dotObjs]]$test2$b == 2) ## TODO: fails (gets NULL)
  expect_true(is.null(out4[[dotObjs]]$test2$x))
  expect_true(!is.null(out4[[dotObjs]]$test$x)) # was made in .inputObjects, copies fine
  expect_true(out4[[dotObjs]]$test2$y == "This module is test2") ## TODO: fails (gets 'This module')

  ## Test P replace method
  mySim3 <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                   params = list(.globals = list(testParB = 543)))

  ## Need "Copy" in this sequence because the event queue is actually an environment :)
  ## so the LHS will have the updated event queue, but the parameters will be at initial conditions

  # quick test for SpaDES.core::P utilization
  expect_true(SpaDES.core::P(mySim3, "testParB", "test2") == 1197)

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

})

test_that("spades.recoveryMode + restartSpades round-trip", {
  testInit(smcc = FALSE, debug = FALSE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache,
                       spades.recoveryMode = TRUE,
                       spades.saveSimOnExit = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "test", "test.R"), testCode, fill = TRUE)
  cat(file = file.path(tmpdir, "test2", "test2.R"), test2Code, fill = TRUE)

  ## test2's init throws stop("testing restartSpades") when testRestartSpades is set
  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir),
                   modules = c("test", "test2"),
                   params = list(test2 = list(testRestartSpades = 1)))

  preCompleted <- NROW(completed(mySim))

  ## spades() must fail on test2's init event
  err <- try(spades(mySim, debug = FALSE), silent = TRUE)
  expect_s3_class(err, "try-error")
  expect_match(attr(err, "condition")$message, "testing restartSpades")

  ## After the failure, recoveryMode must have stashed the sim in savedSimEnv()
  saved <- savedSimEnv()$.sim
  expect_s4_class(saved, "simList")
  expect_true(!is.null(saved$.recoverableObjs))
  ## spades.recoveryMode = TRUE retains exactly one event of state
  expect_equal(length(saved$.recoverableObjs), 1L)
  ## test2's init never finished, so the failing event must still be in queue
  expect_true(any(events(saved)$moduleName == "test2"))
  ## ... and it must not be in completed
  expect_false(any(completed(saved)$moduleName == "test2" &
                     completed(saved)$eventType == "init"))

  ## restartSpades against a saved sim whose offending parameter is still set
  ## should re-throw the same module-level error
  err2 <- try(restartSpades(saved, debug = FALSE), silent = TRUE)
  expect_s3_class(err2, "try-error")
  expect_match(attr(err2, "condition")$message, "testing restartSpades")

  ## clear the offending param; restartSpades must now succeed and resume the queue
  fixed <- savedSimEnv()$.sim
  fixed@params$test2$testRestartSpades <- NULL
  resumed <- restartSpades(fixed, debug = FALSE)
  expect_s4_class(resumed, "simList")
  ## After resume, completed should include the .inputObjects rows, all core
  ## module init events except restartR, both module init events, plus
  ## test2's event1 (scheduled by test2's init at start time).
  expect_equal(NROW(completed(resumed)),
               length(modules(resumed)) +                                # .inputObjects
                 length(setdiff(unlist(.coreModules()), "restartR")) +   # core init
                 length(modules(resumed)) +                              # module init
                 1L)                                                     # test2's event1
})

test_that("spades.recoveryMode = 2L retains two events of state", {
  testInit(smcc = FALSE, debug = FALSE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache,
                       spades.recoveryMode = 2L,
                       spades.saveSimOnExit = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "test", "test.R"), testCode, fill = TRUE)
  cat(file = file.path(tmpdir, "test2", "test2.R"), test2Code, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir),
                   modules = c("test", "test2"),
                   params = list(test2 = list(testRestartSpades = 1)))

  err <- try(spades(mySim, debug = FALSE), silent = TRUE)
  expect_s3_class(err, "try-error")

  saved <- savedSimEnv()$.sim
  expect_s4_class(saved, "simList")
  ## with recoverMode = 2L, exactly two events of state must be retained
  expect_equal(length(saved$.recoverableObjs), 2L)
  ## entries are ordered most-recent-first: test2 (failing init), then test (prior init)
  expect_equal(names(saved$.recoverableObjs), c("test2", "test"))
  ## each entry is a list (per-event captured object snapshot)
  expect_true(all(vapply(saved$.recoverableObjs, is.list, logical(1))))

  ## restartSpades must mention "the last 2 events" in its diagnostic message,
  ## confirming the recoveryMode value flowed through to the recovery infrastructure
  fixed <- savedSimEnv()$.sim
  fixed@params$test2$testRestartSpades <- NULL
  mess <- capture_messages({
    resumed <- restartSpades(fixed, numEvents = 2L, debug = FALSE)
  })
  expect_s4_class(resumed, "simList")
  expect_true(any(grepl("Reversing event.*test2", mess)))
  expect_true(any(grepl("Reversing event.*test\\b", mess)))  # the prior init also rewound
  ## After a full 2-event rewind+replay, both module inits run exactly once again,
  ## producing the same total-completed count as the 1-event case:
  ## 2 inputObjects + 4 core inits + 2 module inits + test2's event1 = 9
  expect_equal(NROW(completed(resumed)),
               length(modules(resumed)) +
                 length(setdiff(unlist(.coreModules()), "restartR")) +
                 length(modules(resumed)) +
                 1L)
})

test_that("spades.recoveryMode = FALSE does not populate .recoverableObjs", {
  testInit(smcc = FALSE, debug = FALSE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache,
                       spades.recoveryMode = FALSE,
                       spades.saveSimOnExit = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "test", "test.R"), testCode, fill = TRUE)
  cat(file = file.path(tmpdir, "test2", "test2.R"), test2Code, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir),
                   modules = c("test", "test2"),
                   params = list(test2 = list(testRestartSpades = 1)))

  err <- try(spades(mySim, debug = FALSE), silent = TRUE)
  expect_s3_class(err, "try-error")

  ## With recoveryMode off, no recoverable state should have been captured
  saved <- savedSimEnv()$.sim
  expect_s4_class(saved, "simList")  # still stashed by saveSimOnExit
  expect_null(saved$.recoverableObjs)
})

test_that("convertToPackage testing", {
  skip_on_cran()

  testInit(c("ggplot2", "pkgload", "roxygen2"), smcc = FALSE, debug = FALSE,
           opts = list(reproducible.useMemoise = FALSE,
                       spades.moduleDocument = TRUE))

  testName1 <- paste0("test.", .rndstr(len = 2))
  testName2 <- paste0("test2.", .rndstr(len = 1))
  mainModFile1 <- paste0(testName1, ".R")
  mainModFile2 <- paste0(testName2, ".R")
  # try(pkgload::unload(testName1), silent = TRUE)
  # try(pkgload::unload(testName2), silent = TRUE)

  # on.exit({
  #   try(pkgload::unload(testName1), silent = TRUE)
  #   try(pkgload::unload(testName2), silent = TRUE)
  # }, add = TRUE)

  newModule(testName1, tmpdir, open = FALSE)
  newModule(testName2, tmpdir, open = FALSE)
  testFilePath <- file.path(tmpdir, testName1, mainModFile1)
  test2FilePath <- file.path(tmpdir, testName2, mainModFile2)

  ## Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop

  testCodeMod <- gsub("test", testName1, testCode)
  test2CodeMod <- gsub("test2", testName2, test2Code)

  cat(file = testFilePath, testCodeMod, fill = TRUE)
  cat(file = test2FilePath, test2CodeMod, fill = TRUE)

  ## Test converting these to packages
  cat(file = testFilePath, '
      #\' @title Init
      #\' @rdname Init
      #\' @name Init
      #\' @param sim A simList
      #\' @export
      Init <- function(sim) {
        sim$aaaa <- Run1(1)
        return(sim)
      }

      #\' @title Run
      #\' @name Run
      #\' @param a An object
      Run1 <- function(a) {
        return(a + 1)
      }
      ', fill = TRUE, append = TRUE)

  cat(file = test2FilePath,'
      Init <- function(sim) {
        # Need to keep comments
        sim$cccc <- try(Run1(1), silent = TRUE)
        return(sim)
      }

      Run2 <- function(a) {
        return(a + 2)
      }
      ', fill = TRUE, append = TRUE)

  for (tt in c(testName1, testName2)) {
    expect_false(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    expect_false(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
  }

  convertToPackage(module = testName1, path = tmpdir, buildDocuments = FALSE) ## TODO: roxygen2 not above function
  convertToPackage(module = testName2, path = tmpdir, buildDocuments = FALSE)

  for (tt in c(testName1, testName2)) {
    expect_true(file.exists(file.path(tmpdir, tt, ".Rbuildignore")))
    expect_true(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    expect_true(!file.exists(file.path(tmpdir, tt, "NAMESPACE")))
    expect_true(dir.exists(file.path(tmpdir, tt, "R")))
    ## list.files(file.path(tmpdir, tt, "R"))
    # expect_true(file.exists(filenameForMainFunctions(tt, tmpdir)))
  }

  mySim9 <- simInit(times = list(start = 0, end = 1),
                    paths = list(modulePath = tmpdir), modules = c(testName1, testName2))

  # doesn't document, unless it is first time
  for (tt in c(testName1, testName2)) {
    expect_true(file.exists(file.path(tmpdir, tt, "DESCRIPTION")))
    # expect_true(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
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
    # expect_true(file.exists(file.path(tmpdir, tt, "NAMESPACE")))
    # expect_true(sum(grepl("export.+doEvent", readLines(file.path(tmpdir, tt, "NAMESPACE")))) == 1)
  }

  # check that inheritance is correct -- Run is in the namespace, Init also... doEvent calls Init calls Run
  expect_true(is(working, "simList"))
  expect_true(working$aaaa == 2)
  expect_true(is(working$cccc, "try-error"))
  # bbb <- get("Run2", asNamespace(testName2))(2)
  bbb <- get("Run2", working$.mods[[testName2]])(2)

  packageFoldername <- file.path(tmpdir, testName2)
  fnTxt <- readLines(file.path(packageFoldername, paste0(testName2, ".R")))
  expect_true(sum(grepl("Need to keep comments", fnTxt)) == 1)
  expect_true(bbb == 4)

  # check documentation
  packageFoldername <- file.path(tmpdir, testName1)
  expect_false(dir.exists(file.path(packageFoldername, "man")))
  documentModule(packageFoldername)
  expect_true(dir.exists(file.path(packageFoldername, "man")))
  pkgload::load_all(packageFoldername)
  on.exit({
    try(pkgload::unload(.moduleNameNoUnderscore(basename(packageFoldername))), silent = TRUE)
  })
  fn <- get("Init", envir = asNamespace(.moduleNameNoUnderscore(basename(packageFoldername))))
  expect_is(fn, "function")
  pkgload::unload(.moduleNameNoUnderscore(basename(packageFoldername)))
  fn <- try(get("Init", envir = asNamespace(.moduleNameNoUnderscore(basename(packageFoldername)))), silent = TRUE)
  expect_true(is(fn, "try-error"))

  # pkgload::unload(testName1)
  # pkgload::unload(testName2)
  #}
})
