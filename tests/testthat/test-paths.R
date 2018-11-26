test_that("paths file does not work correctly", {
  testInitOut <- testInit(setPaths = FALSE)

  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  #setPaths()

  times <- list(start = 0.0, end = 10)
  params <- list(.globals = list(burnStats = "npixelsburned", stackName = "landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")

  #tmpdir <- file.path(tempdir(), "test-paths") %>% checkPath(create = TRUE)

  #on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  # test for mixture of named and unnamed
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = paths[[2]], inputPath = getPaths()[["inputPath"]],
                    modulePath = paths$modulePath,
                    outputPath = getPaths()[["outputPath"]]),
                    normPath)
              )

  # test for non consecutive order, but named
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                outputPath = tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = getPaths()[["cachePath"]], inputPath = getPaths()[["inputPath"]],
                    modulePath = paths$modulePath, outputPath = path.expand(paths$outputPath)), normPath))

  # test for all unnamed
  paths <- list(tmpdir,
                tmpdir,
                system.file("sampleModules", package = "SpaDES.core"),
                tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = paths[[1]],
                    inputPath = paths[[2]],
                    modulePath = paths[[3]],
                    outputPath = paths[[4]]), normPath))


  # test for all named, non consecutive, using accessors
  paths <- list(cachePath = tmpdir,
                modulePath = system.file("sampleModules", package = "SpaDES.core"),
                outputPath = tmpdir,
                inputPath = tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = cachePath(mySim),
                    inputPath = inputPath(mySim),
                    modulePath = modulePath(mySim),
                    outputPath = outputPath(mySim)), normPath))

  # missing paths
  oldPaths <- getPaths()
  do.call(setPaths, paths)
  on.exit({do.call(setPaths, paths)}, add = TRUE)
  mySim1 <- simInit(times, params, modules, objects = list())
  expect_equal(lapply(paths(mySim), normPath),
               lapply(paths(mySim1), normPath))

  # setting paths via setPaths works for changing the paths in the simInit & therefore simList
  setPaths(cachePath = oldPaths$cachePath)
  mySim2 <- simInit(times, params, modules, objects = list())
  expect_false(identical(lapply(paths(mySim), normPath),
               lapply(paths(mySim2), normPath)))

  inputPath(mySim) <- tmpdir
  expect_equal(inputPath(mySim), tmpdir)

  outputPath(mySim) <- tmpdir
  expect_equal(outputPath(mySim), tmpdir)

  modulePath(mySim) <- tmpdir
  expect_equal(modulePath(mySim), tmpdir)

  cachePath(mySim) <- tmpdir
  expect_equal(cachePath(mySim), tmpdir)


})
