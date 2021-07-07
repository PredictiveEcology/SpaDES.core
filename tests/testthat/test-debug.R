test_that("testing debug options", {
  skip_if_not_installed("RandomFields")
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)


  times <- list(start = 0.0, end = 1, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .seed = list("init" = 321)),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", #"caribouMovement",
                  "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  s <- simInit(times = times, params = params, modules = modules, paths = paths)
  if (interactive()) {
    spades(s, debug = "init") # should open both init events in both modules
    spades(s, debug = c("fireSpread", "init")) # should only open init event in fireSpread Module
    spades(s, debug = c("fireSpread")) # should only 3 events in fireSpread Module
    spades(s, debug = c("stats")) # should only 1 events in fireSpread Module
    runName <- "lala"
    spades(s, debug = quote(runName)) # should only 1 events in fireSpread Module
  }

})
