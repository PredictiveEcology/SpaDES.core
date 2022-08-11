if (interactive()) library(testthat)

test_that("simulation runs with simInit and spades", {
  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  set.seed(42)

  sysFiles <- system.file("sampleModules", package = "SpaDES.core")
  modules <- list("randomLandscapes", "caribouMovement")
  files <- dir(sysFiles, recursive = TRUE, full.names = TRUE)
  rootPth1 <- file.path(tmpdir, modules[[1]]) %>%
    checkPath(., create = TRUE)
  rootPth2 <- file.path(tmpCache, modules[[2]]) %>%
    checkPath(., create = TRUE)

  file.copy(grep(modules[[1]], files, value = TRUE),
            file.path(rootPth1, paste0(modules[[1]], ".R")))
  file.copy(grep(modules[[2]], files, value = TRUE), rootPth2)

  times <- list(start = 0.0, end = 0.0, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  paths <- list(modulePath = c(tmpdir, tmpCache))

  newModule("test", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "test", "test.R"),'
      defineModule(sim, list(
      name = "test",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "year",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
      ),
      inputObjects = bindrows(
      ),
      outputObjects = bindrows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      sim$dp <- dataPath(sim)
      sim$cachePath <- cachePath(sim)
      sim$optionsCachePath <- getOption("reproducible.cachePath")
      sim <- scheduleEvent(sim, 2L, "test", "event0")
      },
      event1 = {
      #sim <- scheduleEvent(sim, sim@simtimes$current+1, "test", "event0")
      })
      return(invisible(sim))
      }
      ', fill = TRUE)
  rootPth3 <- file.path(tmpdir, "test")
  modules <- append(modules, "test")

  # Do a single run of simInit for this whole test
  mySim <- simInit(times, params, modules = modules, objects = list(), paths)
  mySim <- spades(mySim, debug = FALSE)
  mySim$age <- 2
  end(mySim) <- 1
  mySim <- scheduleConditionalEvent(mySim, "sim$age > 1", "test", "event1", maxEventTime = 5)
  mySim <- scheduleConditionalEvent(mySim, "sim$age > 2", "test", "event2", maxEventTime = 5)
  expect_true(length(mySim$._conditionalEvents) == 2)
  mySimOut <- spades(mySim, debug = FALSE)
  expect_true(NROW(completed(mySimOut)[eventType == "event1"]) == 1)
  expect_true(length(mySimOut$._conditionalEvents) == 1)
  end(mySimOut) <- 2
  mySim$age <- 3
  mySimOut2 <- spades(mySimOut, debug = FALSE)
  expect_true(!exists("._conditionalEvents", envir = mySimOut))
  expect_true(NROW(completed(mySimOut2)[eventType == "event2"]) == 1)
})
