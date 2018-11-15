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
  set.seed(123)
  times <- list(start = 1, end = 1)
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
  expect_true(length(sim$objectSynonyms)==length(os))
  sim <- objectSynonyms(sim, os2)

  expect_true(length(sim$objectSynonyms)==4)
  expect_true(identical(sim$objectSynonyms[[2]], unique(c(os[[1]], os2[[1]]))))

  e <- new.env()
  e <- objectSynonyms(e, list(c("age", "ageMap")))
  expect_true(is.null(e$age))
  rm("age", envir = e)
  expect_true(is.null(e$age))
  expect_warning(e$ageMap)


})
