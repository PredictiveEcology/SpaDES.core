test_that("test objectSynonyms", {
  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE,
                                      spades.useRequire = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  sim <- simInit()

  sim$age <- 1:10;
  sim <- objectSynonyms(sim, list(c("age", "ageMap", "age2")))

  expect_true(identical(sim$ageMap, sim$age))
  sim$age <- 4
  expect_true(identical(sim$ageMap, sim$age))
  sim$ageMap <- 2:5
  sim$ageMap[3] <- 11
  expect_true(identical(sim$ageMap, sim$age))

  # Check maintenance
  set.seed(123)
  times <- list(start = 1, end = 2)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    caribouMovement = list(N = 3),
    randomLandscapes = list(inRAM = TRUE, nx = 15, ny = 15)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  sim <- simInit(times, params, modules, objects = list(), paths)
  sim <- objectSynonyms(sim, list(c("caribou", "caribouObj")))

  simOut <- spades(sim, .plotInitialTime = NA, debug = FALSE)
  expect_true(identical(simOut$caribou, simOut$caribouObj))
  simOut$caribou$x1[2] <- 20
  expect_true(identical(simOut$caribou, simOut$caribouObj))
  expect_true(bindingIsActive("caribouObj", simOut))
  expect_false(bindingIsActive("caribou", simOut))

  # Test more complex "merge"
  sim <- simInit()
  os <- list(c("age", "ageMap"), c("vegMap", "veg"), c("studyArea", "studyArea2"))
  os2 <- list(c("ageMap", "timeSinceFire", "tsf"),
              c("systime", "systime2"),
              c("vegMap", "veg"))
  sim <- objectSynonyms(sim, os)
  expect_true(length(sim$objectSynonyms) == length(os))
  sim <- objectSynonyms(sim, os2)

  expect_true(length(sim$objectSynonyms) == 4)
  expect_true(identical(sim$objectSynonyms[[2]], unique(c(os[[1]], os2[[1]]))))

  e <- new.env(parent = emptyenv())
  e <- objectSynonyms(e, list(c("age", "ageMap")))
  expect_true(is.null(e$age))
  rm("age", envir = e)
  expect_true(is.null(e$age))
  expect_warning(e$ageMap)

  # Test simInit for .inputObjects
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
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
        defineParameter(".useCache", "character", ".inputObjects", NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("age", "numeric", ""),
        expectsInput("age2", "numeric", "") # need a dummy one that isn not supplied in simInit below
      ),
      outputObjects = bindrows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      #sim$dp <- dataPath(sim)
      sim <- scheduleEvent(sim, sim@simtimes$current+1, "test", "event1")
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes$current+1, "test", "event1")
      })
      return(invisible(sim))
      }

      .inputObjects <- function(sim) {
        if (suppliedElsewhere("ageMap", sim)) {
                sim$worked <- TRUE
        }
        sim
      }
      ', fill = TRUE)
  modules <- "test"
  sim <- simInit(times, params, modules = modules,
                 objects = list(age = 1, vegMap = 2, studyArea = 3, objectSynonyms = os),
                 paths = list(modulePath = tmpdir))
  expect_equal(sim$age, sim$ageMap)
  expect_equal(sim$veg, sim$vegMap)
  expect_equal(sim$studyArea, sim$studyArea2)

  sim <- Cache(simInitAndSpades, times, params, modules = modules,
               objects = list(objectSynonyms = os),
               paths = list(modulePath = tmpdir))
  expect_equal(sim$age, sim$ageMap)
  expect_equal(sim$veg, sim$vegMap)
  expect_equal(sim$studyArea, sim$studyArea2)
  sim <- Cache(simInitAndSpades, times, params, modules = modules,
               objects = list(objectSynonyms = os),
               paths = list(modulePath = tmpdir))
  expect_equal(sim$age, sim$ageMap)
  expect_equal(sim$veg, sim$vegMap)
  expect_equal(sim$studyArea, sim$studyArea2)
  expect_true(isTRUE(sim$worked))
})
