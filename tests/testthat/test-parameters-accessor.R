## parameters(): the module *parameter definition* tables, in both the
## data.frame and the nested-list shape. params() is the current values; this is
## the declared metadata, and its two branches were untested.

twoModuleSim <- function(tmpdir) {
  suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape",
                                  burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))
}

test_that("parameters(asDF = TRUE) stacks every module's definitions into one frame", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  p <- parameters(twoModuleSim(tmpdir), asDF = TRUE)

  expect_s3_class(p, "data.frame")
  expect_identical(names(p),
                   c("paramName", "paramClass", "default", "min", "max", "paramDesc"))
  expect_gt(NROW(p), 0L)
  ## parameters from both modules are present
  expect_true(all(c("nx", "ny") %in% p$paramName))
})

test_that("parameters() defaults to a list of one entry per module", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  p <- parameters(twoModuleSim(tmpdir))

  expect_type(p, "list")
  expect_setequal(names(p), c("randomLandscapes", "fireSpread"))
  ## each module's entry is itself named by parameter
  expect_true(all(c("nx", "ny") %in% names(p[["randomLandscapes"]])))
})

test_that("parameters() and parameters(asDF = TRUE) describe the same parameters", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  sim <- twoModuleSim(tmpdir)

  asList <- parameters(sim)
  asDF <- parameters(sim, asDF = TRUE)

  expect_setequal(unlist(lapply(asList, names), use.names = FALSE), asDF$paramName)
})

test_that("parameters() is empty for a sim with no modules", {
  skip_on_cran()
  testInit()

  expect_null(parameters(simInit()))
})

test_that("inputs() converts loadTime into the sim's timeunit", {
  skip_on_cran()
  testInit()

  f <- file.path(tmpdir, "in.rds")
  saveRDS(1:3, f)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 2, timeunit = "year"),
    inputs = data.frame(file = f, objectName = "io", loadTime = 1)))

  ii <- inputs(sim)

  expect_s3_class(ii, "data.frame")
  expect_identical(NROW(ii), 1L)
  expect_identical(as.numeric(ii$loadTime), 1)
  expect_identical(ii$objectName, "io")
})

## ---- newObjectsCreated --------------------------------------------------

test_that("newObjectsCreated returns an empty table when nothing was recorded", {
  skip_on_cran()
  testInit()

  d <- newObjectsCreated(simInit(times = list(start = 0, end = 2, timeunit = "year")))

  expect_s3_class(d, "data.table")
  expect_identical(NROW(d), 0L)
  expect_true(all(c("newObjects", "eventTime", "moduleName", "eventType",
                    "eventPriority") %in% names(d)))
})

test_that("newObjectsCreated returns and prints what was recorded", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 2, timeunit = "year"))
  sim$._objectsCreated <- list(
    data.table::data.table(newObjects = "b", eventTime = 1, moduleName = "m",
                           eventType = "e", eventPriority = 1),
    data.table::data.table(newObjects = "a", eventTime = 2, moduleName = "m",
                           eventType = "e", eventPriority = 1))

  printed <- capture.output(d <- newObjectsCreated(sim))

  expect_identical(NROW(d), 2L)
  ## the rows come back ordered by object name
  expect_identical(d$newObjects, c("a", "b"))
  expect_gt(length(printed), 0L)
})

## ---- inputArgs<- --------------------------------------------------------

inputSim <- function(tmpdir) {
  f <- file.path(tmpdir, "ia.rds")
  saveRDS(1, f)
  suppressMessages(simInit(times = list(start = 0, end = 2, timeunit = "year"),
                           inputs = data.frame(file = f, objectName = "io")))
}

test_that("inputArgs<- accepts a list", {
  skip_on_cran()
  testInit()

  sim <- inputSim(tmpdir)
  inputArgs(sim) <- list(list(a = 1))

  expect_type(inputArgs(sim), "list")
  expect_identical(inputArgs(sim)[[1]], list(a = 1))
})

test_that("inputArgs<- NULL blanks one entry per input row", {
  skip_on_cran()
  testInit()

  sim <- inputSim(tmpdir)
  inputArgs(sim) <- list(list(a = 1))
  inputArgs(sim) <- NULL

  expect_length(inputArgs(sim), NROW(inputs(sim)))
  expect_null(inputArgs(sim)[[1]])
})

test_that("inputArgs<- rejects anything that is not a list", {
  skip_on_cran()
  testInit()

  sim <- inputSim(tmpdir)

  expect_error({inputArgs(sim) <- "nope"},
               "must be a list of named elements")
})
