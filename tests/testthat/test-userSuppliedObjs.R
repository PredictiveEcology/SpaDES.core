test_that("test userSuppliedObj", {
  testInitOut <- testInit("raster", smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  m <- "test"
  newModule(m, tmpdir, open = FALSE)
  fileName <- file.path(m, paste0(m, ".R"))

  xxx <- readLines(fileName)
  lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  lineWithDotInputObjects <- grep(xxx, pattern = "\\.inputObjects")
  xxx1 <- xxx

  cat(xxx1[1:(lineWithInputObjects-1)], "
        expectsInput('ei1', 'numeric', '', ''),
        expectsInput('ei2', 'numeric', '', ''),
      ",
      xxx1[(lineWithInputObjects+1):lineWithDotInputObjects], "
      sim$ei1 <- 4
      sim$ei1 <- sim$ei3
      ",
      xxx1[(lineWithDotInputObjects+1):length(xxx1)],
      sep = "\n", fill = FALSE, file = fileName)

  ei1 <- 10
  mm2 <- capture_output(mm1 <- capture_messages(mySim <- simInit(modules = m, paths = list(modulePath = tmpdir),
                                                                 params = list(test = list(.useCache = ".inputObjects")),
                                                                 objects = list(ei1 = ei1))))
  if (nzchar(mm2)) {
    mm1 <- c(mm2, mm1)
  }
  mm1 <- cleanMessage(mm1)

  fullMessage <- c("Using or creating cached copy of inputObjects for test",
                   "test: module code: ei2 is declared in inputObjects, but no default\\(s\\) is provided in inputObjects",
                   "test: module code: ei1, ei2 are declared in inputObjects, but are not used in the module",
                   "test: inputObjects: ei3 is used from sim inside inputObjects, but is not declared in inputObjects"
  )

  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))

  # Run again, but with changed ei11 -- so should be no cache
  ei1 <- 11
  mm2 <- capture_output(mm1 <- capture_messages(mySim <- simInit(modules = m, paths = list(modulePath = tmpdir),
                                                                 params = list(test = list(.useCache = ".inputObjects")),
                                                                 objects = list(ei1 = ei1))))
  if (nzchar(mm2)) {
    mm1 <- c(mm2, mm1)
  }
  mm1 <- cleanMessage(mm1)

  fullMessage <- c("Using or creating cached copy of inputObjects for test",
                   "test: module code: ei2 is declared in inputObjects, but no default\\(s\\) is provided in inputObjects",
                   "test: module code: ei1, ei2 are declared in inputObjects, but are not used in the module",
                   "test: inputObjects: ei3 is used from sim inside inputObjects, but is not declared in inputObjects"
  )

  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))

  # Run 3rd time, should use cache
  mm2 <- capture_output(mm1 <- capture_messages(mySim <- simInit(modules = m, paths = list(modulePath = tmpdir),
                                           params = list(test = list(.useCache = ".inputObjects")),
                                           objects = list(ei1 = ei1))))
  if (nzchar(mm2)) {
    mm1 <- c(mm2, mm1)
  }
  mm1 <- cleanMessage(mm1)

  fullMessage <- c("  Using cached copy of inputObjects event in test module",
                   "Using or creating cached copy of inputObjects for test",
                   "test: module code: ei2 is declared in inputObjects, but no default\\(s\\) is provided in inputObjects",
                   "test: module code: ei1, ei2 are declared in inputObjects, but are not used in the module",
                   "test: inputObjects: ei3 is used from sim inside inputObjects, but is not declared in inputObjects"
  )

  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))

})
