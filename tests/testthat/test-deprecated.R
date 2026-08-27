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
