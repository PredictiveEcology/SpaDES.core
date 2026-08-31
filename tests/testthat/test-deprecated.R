test_that("deprecated functions warn and point at their new package", {
  testInit()

  ## the `experiment` family moved to SpaDES.project; `POM` to SpaDES.experiment
  for (fn in list(experiment, experiment2, simInitAndExperiment)) {
    expect_warning(fn(), "SpaDES\\.project")
  }
  expect_warning(POM(), "SpaDES\\.experiment")
})

test_that("the deprecation message names the function and how to install it", {
  testInit()

  w <- tryCatch(experiment(), warning = function(w) conditionMessage(w))
  expect_match(w, "^experiment has been moved to SpaDES\\.project\\.")
  expect_match(w, "install_github('PredictiveEcology/SpaDES.project@development')", fixed = TRUE)
})

test_that("loadPackages is deprecated in favour of Require", {
  testInit()

  expect_warning(SpaDES.core:::loadPackages(), "Require")
})

test_that(".messageDeprecatedFn builds both the branch and non-branch install hints", {
  testInit()

  msg <- SpaDES.core:::.messageDeprecatedFn("someFn", newPackage = "SomePkg")
  expect_match(msg, "someFn has been moved to SomePkg")
  expect_match(msg, "PredictiveEcology/SomePkg@development", fixed = TRUE)
  expect_match(msg, "PredictiveEcology/SomePkg'", fixed = TRUE)
  expect_match(msg, " or ", fixed = TRUE)
})

## `.plotInitialTime` as a spades() argument ---------------------------------
## The *module parameter* of the same name is unaffected; only the argument went.

test_that("spades() no longer takes .plotInitialTime as an argument", {
  testInit()

  expect_false(".plotInitialTime" %in% formalArgs(spades))
  expect_false(".plotInitialTime" %in% formalArgs(simInitAndSpades))
})

test_that("passing .plotInitialTime to spades() warns and is ignored", {
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tempdir())
  mkSim <- function() {
    suppressMessages(simInit(
      times = list(start = 0, end = 1), modules = list("randomLandscapes"),
      paths = list(modulePath = mp),
      params = list(randomLandscapes = list(.plotInitialTime = NA))))
  }

  expect_warning(suppressMessages(spades(mkSim(), .plotInitialTime = NA, debug = FALSE)),
                 "argument to spades\\(\\) is deprecated")

  ## it points at the replacement
  w <- tryCatch(suppressMessages(spades(mkSim(), .plotInitialTime = NA, debug = FALSE)),
                warning = conditionMessage)
  expect_match(w, "Use `.plots` instead", fixed = TRUE)

  ## and the run still completes
  expect_s4_class(
    suppressWarnings(suppressMessages(spades(mkSim(), .plotInitialTime = NA, debug = FALSE))),
    "simList")
})

test_that(".plots still turns plotting off, and leaves the module parameter alone", {
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tempdir())
  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1), modules = list("randomLandscapes"),
    paths = list(modulePath = mp),
    params = list(randomLandscapes = list(.plotInitialTime = NA))))

  out <- expect_no_warning(suppressMessages(spades(sim, .plots = NA, debug = FALSE)))
  expect_s4_class(out, "simList")
  expect_true(is.na(params(out)$randomLandscapes$.plotInitialTime))
})
