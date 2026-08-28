## checkObject() and checkParams().

## the core parameter names simInit passes to checkParams()
coreParamsList <- function() {
  append(list(".savePath", ".saveObjects", ".seed"),
         list(".saveInterval", ".saveInitialTime", ".plotInterval", ".plotInitialTime"))
}

test_that("checkObject accepts an object that exists in the sim", {
  testInit()

  sim <- simInit()
  sim$existingObj <- 1:5

  expect_true(checkObject(sim, object = sim$existingObj))
})

test_that("checkObject accepts an object referenced by name", {
  testInit()

  sim <- simInit()
  sim$existingObj <- 1:5

  expect_true(checkObject(sim, name = "existingObj"))
})

test_that("checkObject rejects an object that is not there, and says so", {
  testInit()

  sim <- simInit()

  expect_false(suppressMessages(checkObject(sim, name = "notThere")))

  ## the explanatory message is verbose-gated
  withr::local_options(spades.debug = TRUE)
  expect_message(checkObject(sim, name = "notThere"), "does not exist")
})

test_that("checkObject accepts a layer that is present", {
  testInit()

  sim <- simInit()
  sim$aList <- list(alpha = 1, beta = 2)

  expect_true(checkObject(sim, name = "aList", layer = "alpha"))
})

test_that("checkObject rejects a layer that is absent, and names it", {
  testInit()

  sim <- simInit()
  sim$aList <- list(alpha = 1, beta = 2)

  expect_false(suppressMessages(checkObject(sim, name = "aList", layer = "zeta")))

  withr::local_options(spades.debug = TRUE)
  expect_message(checkObject(sim, name = "aList", layer = "zeta"),
                 "zeta is not a layer")
})

test_that("checkObject demands a simList", {
  testInit()

  expect_error(checkObject(name = "anything"), "Must provide a simList object")
})

test_that("checkParams passes a module whose parameters are all used", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )

  expect_true(suppressMessages(checkParams(sim, coreParamsList())))
})

test_that("checkParams flags a user parameter the module never uses", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp),
            params = list(randomLandscapes = list(bogusUnusedParam = 1)))
  )

  expect_false(suppressMessages(checkParams(sim, coreParamsList())))

  withr::local_options(spades.debug = TRUE)
  expect_message(checkParams(sim, coreParamsList()),
                 "bogusUnusedParam is not used in module randomLandscapes")
})

test_that("checkParams flags a global parameter no module uses", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp),
            params = list(.globals = list(bogusGlobal = "x")))
  )

  expect_false(suppressMessages(checkParams(sim, coreParamsList())))

  withr::local_options(spades.debug = TRUE)
  expect_message(checkParams(sim, coreParamsList()),
                 "Global parameter\\(s\\) not used in any module: bogusGlobal")
})

test_that("checkParams returns NA when there are no user modules to check", {
  testInit()

  ## nothing was checked, so neither TRUE ("all found") nor FALSE ("something
  ## missing") is honest -- see the @return docs
  sim <- simInit()
  expect_true(is.na(suppressMessages(checkParams(sim, coreParamsList()))))
})
