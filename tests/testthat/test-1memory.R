test_that("testing memoryUse", {
  # Needs to run first or else memory use test fails
  skip_on_cran()

  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  skip_if_not_installed("NLMR")

  rm(list = ls())
  testInitOut <- testInit(c("terra", "future.callr", "future"),
                          opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.memoryUseInterval = 0.2,
                                      spades.futurePlan = "callr"))
  oldPlan <- future::plan()
  on.exit({
    if (!identical(future::plan(), oldPlan)) {
      future::plan(oldPlan)
    }
  }, add = TRUE)

  system.time(future::plan(future.callr::callr))

  #set.seed(42)

  for (i in 1:10) gc()
  times <- list(start = 0.0, end = if (isWindows()) 60 else 30, timeunit = "year")
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

  if (!identical(Sys.getenv("USING_COVR"), "true")) {

    mySim3 <- spades(mySim2, debug = TRUE)
    suppressWarnings({
      memUse <- memoryUse(mySim3)
    })
    expect_true(is(memUse, "data.table"))
    expect_true(is.numeric(memUse$maxMemory))
    expect_true(sum(!is.na(memUse$maxMemory)) > 0)
    suppressWarnings({
      memUse <- memoryUse(mySim3, max = FALSE)
    })
    if (length(unique(memUse$maxMemory)) == 1)
      browser()
    expect_true(length(unique(memUse$maxMemory)) > 1) # i.e., the join had to result in multiple values
  }
})
