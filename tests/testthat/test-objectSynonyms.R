test_that("test objectSynonyms", {
  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.useRequire = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  sim <- simInit()

  sim$age <- 1:10;
  #sim <- objectSynonyms(sim, c("age" = "ageMap"))
  sim <- objectSynonyms(sim, list(c("age", "ageMap", "age2")))

  expect_true(identical(sim$ageMap, sim$age))
  sim$age <- 4
  expect_true(identical(sim$ageMap, sim$age))
  sim$ageMap <- 2:5
  sim$ageMap[3] <- 11
  expect_true(identical(sim$ageMap, sim$age))

  # Check maintenance
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape")
  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  sim <- simInit(times, params, modules, objects = list(), paths)
  sim <- objectSynonyms(sim, list(c("caribou", "caribouObj")))

  simOut <- spades(sim, .plotInitialTime = NA)
  expect_true(identical(simOut$caribou, simOut$caribouObj))
  simOut$caribou$x1[2] <- 20
  expect_true(identical(simOut$caribou, simOut$caribouObj))
  expect_true(bindingIsActive("caribouObj", simOut))
  expect_false(bindingIsActive("caribou", simOut))

})
