test_that("testing memoryUse", {
  if (!interactive()) skip("This memoryUse is still very experimental")
  testInitOut <- testInit(c("raster", "future.callr", "future"), opts = list("spades.moduleCodeChecks" = FALSE,
                                      "spades.memoryUse" = 1,
                                      "spades.futurePlan" = "callr"))
  oldPlan <- future::plan()
  on.exit({
    testOnExit(testInitOut)
    if (!identical(future::plan(), oldPlan)) {
      future::plan(oldPlan)
    }
  }, add = TRUE)

  system.time(future::plan(future.callr::callr))

  #set.seed(42)

  times <- list(start = 0.0, end = 30, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
  #set.seed(1234)
  mySim2 <- simInit(times = times, params = params,
                    modules = modules, objects = list(), paths = paths)
  mySim3 <- spades(mySim2, debug = FALSE)
  suppressWarnings(memUse <- memoryUse(mySim3))
  expect_true(is(memUse, "data.table"))
  expect_true(is.numeric(memUse$maxMemory))
  expect_true(sum(!is.na(memUse$maxMemory))>0)
  suppressWarnings(memUse <- memoryUse(mySim3, max = FALSE))

})
