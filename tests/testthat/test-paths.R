test_that("paths file does not work correctly", {
  testInit(sampleModReqdPkgs)

  times <- list(start = 0.0, end = 10)
  params <- list(.globals = list(burnStats = "npixelsburned", stackName = "landscape"))
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")

  # test for mixture of named and unnamed
  paths <- list(modulePath = getSampleModules(tmpdir), tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(
                 cachePath = paths[[2]],
                 inputPath = getPaths()[["inputPath"]],
                 modulePath = paths$modulePath,
                 outputPath = getPaths()[["outputPath"]],
                 rasterPath = getPaths()[["rasterPath"]],
                 scratchPath = getPaths()[["scratchPath"]],
                 terraPath = getPaths()[["terraPath"]]
                ), normPath)
              )

  # test for non consecutive order, but named
  paths <- list(modulePath = getSampleModules(tmpdir),
                outputPath = tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = getPaths()[["cachePath"]],
                           inputPath = getPaths()[["inputPath"]],
                           modulePath = paths$modulePath,
                           outputPath = path.expand(paths$outputPath),
                           rasterPath = getPaths()[["rasterPath"]],
                           scratchPath = getPaths()[["scratchPath"]],
                           terraPath = getPaths()[["terraPath"]]
               ), normPath))

  # test for all unnamed
  paths <- list(tmpdir,
                tmpdir,
                getSampleModules(tmpdir),
                tmpdir,
                tmpdir,
                tmpdir,
                tmpdir)
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = paths[[1]],
                           inputPath = paths[[2]],
                           modulePath = paths[[3]],
                           outputPath = paths[[4]],
                           rasterPath = paths[[5]],
                           scratchPath = paths[[6]],
                           terraPath = paths[[7]]
               ), normPath))

  # test for all named, non consecutive, using accessors
  paths <- list(
    cachePath = tmpdir,
    modulePath = getSampleModules(tmpdir),
    outputPath = tmpdir,
    inputPath = tmpdir,
    rasterPath = file.path(tmpdir, "raster"), ## subdir of scratchPath
    scratchPath = tmpdir,
    terraPath = file.path(tmpdir, "terra") ## subdir of scratchPath
  )
  mySim <- simInit(times, params, modules, objects = list(), paths)
  expect_equal(lapply(paths(mySim), normPath),
               lapply(list(cachePath = cachePath(mySim),
                           inputPath = inputPath(mySim),
                           modulePath = modulePath(mySim),
                           outputPath = outputPath(mySim),
                           rasterPath = rasterPath(mySim),
                           scratchPath = scratchPath(mySim),
                           terraPath = terraPath(mySim)
               ), normPath))

  # missing paths
  oldPaths <- getPaths()
  do.call(setPaths, paths)
  on.exit(do.call(setPaths, oldPaths), add = TRUE)
  mySim1 <- simInit(times, params, modules, objects = list())
  expect_equal(lapply(paths(mySim), normPath), lapply(paths(mySim1), normPath))

  # setting paths via setPaths works for changing the paths in the simInit & therefore simList
  setPaths(cachePath = oldPaths$cachePath)
  mySim2 <- simInit(times, params, modules, objects = list())
  expect_false(identical(lapply(paths(mySim), normPath), lapply(paths(mySim2), normPath)))

  cachePath(mySim) <- tmpdir
  expect_equal(cachePath(mySim), tmpdir)

  modulePath(mySim) <- tmpdir
  expect_equal(modulePath(mySim), tmpdir)

  inputPath(mySim) <- tmpdir
  expect_equal(inputPath(mySim), tmpdir)

  outputPath(mySim) <- tmpdir
  expect_equal(outputPath(mySim), tmpdir)

  rasterPath(mySim) <- tmpdir
  expect_equal(rasterPath(mySim), tmpdir)

  scratchPath(mySim) <- tmpdir
  expect_equal(scratchPath(mySim), tmpdir)

  terraPath(mySim) <- tmpdir
  expect_equal(terraPath(mySim), tmpdir)
})

test_that("absolutizePaths can handle multiple paths", {
  test_paths <- list(
    cachePath = "/mnt/scratch/achubaty/BC_HRV/cache",
    inputPath = "mnt/projects/HRV/BC_HRV/inputs",
    modulePath = c("/home/achubaty/GitHub/BC_HRV/modules",
                   "/home/achubaty/GitHub/BC_HRV/modules/scfm/modules"),
    outputPath = "/mnt/projects/HRV/BC_HRV/outputs/NRD_Quesnel_scfm_hrv_FRT_res125/rep01",
    rasterPath = "/mnt/scratch/achubaty/BC_HRV/raster",
    scratchPath = "/mnt/scratch/achubaty/BC_HRV",
    terraPath = "/mnt/scratch/achubaty/BC_HRV/terra"
  )

  expect_no_error({
    out <- absolutizePaths(paths = test_paths,
                           projectPath = "/home/achubaty/GitHub/BC_HRV",
                           tempdir = "/mnt/scratch/achubaty/tmp/RtmpJ0cOm1")
  })

  expect_equivalent(sapply(out, length), c(1, 1, 2, 1, 1, 1, 1))
})
