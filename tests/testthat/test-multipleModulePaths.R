if (interactive()) library(testthat)

test_that("simulation runs with simInit and spades", {
  skip_if_not_installed("NLMR")

  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE))

  set.seed(42)

  sysFiles <- system.file("sampleModules", package = "SpaDES.core")
  modules <- list("randomLandscapes", "caribouMovement")
  files <- dir(sysFiles, recursive = TRUE, full.names = TRUE)
  rootPth1 <- file.path(tmpdir, modules[[1]]) %>%
    checkPath(., create = TRUE)
  rootPth2 <- file.path(tmpCache, modules[[2]]) %>%
    checkPath(., create = TRUE)

  file.copy(grep(modules[[1]], files, value = TRUE), file.path(rootPth1, paste0(modules[[1]], ".R")))
  file.copy(grep(modules[[2]], files, value = TRUE), rootPth2)

  times <- list(start = 0.0, end = 2, timeunit = "year")
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
    authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    childModules = character(0),
    version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
    spatialExtent = terra::ext(rep(0, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.md", "test.Rmd"),
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
      sim <- scheduleEvent(sim, 2L, "test", "event1")
    },
    event1 = {
    #sim <- scheduleEvent(sim, sim@simtimes$current+1, "test", "event1")
    })
    return(invisible(sim))
    }
    ', fill = TRUE)
  rootPth3 <- file.path(tmpdir, "test")
  modules <- append(modules, "test")

  paths$cachePath <- tmpCache
  lapply(paths, checkPath, create = TRUE)
  expect_false(identical(normPath(paths$cachePath), normPath(.paths()$cachePath)))

  # Do a single run of simInit for this whole test
  mySim <- simInit(times, params, modules = modules, objects = list(), paths) %>%
    spades(debug = FALSE)

  expect_true(all(modules(mySim) %in% unlist(modules)))
  expect_true(all(unlist(modules) %in% completed(mySim)$moduleName))
  expect_true(all(normPath(names(modules(mySim))) %in%
                    normPath(c(rootPth1, rootPth2, rootPth3))))

  # Test dataPath(sim)
  expect_true(identical(normPath(mySim$dp), normPath(file.path(rootPth3, "data"))))

  # Test new modules arg for modulePath
  expect_true(identical(sort(normPath(modulePath(mySim, unlist(modules)[2:3]))),
                        sort(normPath(dirname(c(rootPth2, rootPth3))))))
  expect_true(identical(sort(normPath(modulePath(mySim, unlist(modules)[1:2]))),
                        sort(normPath(dirname(c(rootPth1, rootPth2))))))
  expect_true(identical(sort(normPath(modulePath(mySim, unlist(modules)[1]))),
                        sort(normPath(dirname(c(rootPth1))))))

  # Here it is different than just modulePath(mySim) because user is asking for
  #  3 modulePaths explicitly, rather than just the modulePaths ... i.e., there are
  #  2 unique modulePaths here, but there are 3 modules
  expect_true(identical(sort(normPath(modulePath(mySim, unlist(modules)[1:3]))),
                        sort(normPath(dirname(c(rootPth1, rootPth2, rootPth3))))))

  # Test the set up paths in the simList correctly
  # First check that the interactive .paths() is still the original way
  expect_false(identical(normPath(mySim@paths$cachePath), normPath(.paths()$cachePath)))
  # Then check that internally, the options("spades.xxxPath") was set correctly
  expect_true(identical(normPath(mySim@paths$cachePath), mySim$cachePath))
  expect_true(identical(normPath(mySim@paths$cachePath), mySim$cachePath))
  expect_true(identical(normPath(mySim@paths$cachePath), mySim$optionsCachePath))

  # Test for integer values in scheduleEvent
  expect_true(completed(mySim)[moduleName == "test" & eventType == 'event1', eventTime == 2])
})
