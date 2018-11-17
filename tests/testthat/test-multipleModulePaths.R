test_that("simulation runs with simInit and spades", {
  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  set.seed(42)

  sysFiles <- system.file("sampleModules", package = "SpaDES.core")
  modules <- list("randomLandscapes", "caribouMovement")
  files <- dir(sysFiles, recursive = TRUE, full.names = TRUE)
  rootPth1 <- file.path(tmpdir, modules[[1]])
  checkPath(rootPth1, create = TRUE)
  rootPth2 <- file.path(tmpCache, modules[[2]])
  checkPath(rootPth2, create = TRUE)

  file.copy(grep(modules[[1]], files, value = TRUE), file.path(rootPth1, paste0(modules[[1]], ".R")))
  file.copy(grep(modules[[2]], files, value = TRUE), file.path(rootPth2, paste0(modules[[2]], ".R")))

  times <- list(start = 0.0, end = 2, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  paths <- list(modulePath = c(tmpdir, tmpCache))

  mySim <- simInit(times, params, modules, objects = list(), paths) %>%
    spades(debug = FALSE)

  expect_true(all(modules(mySim) %in% unlist(modules)))
  expect_true(all(unlist(modules) %in% completed(mySim)$moduleName))
  expect_true(all(normPath(names(modules(mySim))) %in% normPath(c(rootPth1, rootPth2))))

})
