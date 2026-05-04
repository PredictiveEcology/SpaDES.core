## Round-trip tests for the lazy saveSimList / loadSimList path
## (tools::makeLazyLoadDB on save, lazyLoad() on load).
## A minimal `new("simList")` keeps the test fast and isolates the
## lazy mechanism from simInit/spades plumbing.

test_that("lazy saveSimList/loadSimList round-trip restores user objects via promises", {
  skip_if_not_installed("rlang")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["a"]] <- 1:5
  sim@.xData[["b"]] <- letters[1:3]
  sim@.xData[["c"]] <- list(x = 10, y = "z")

  filename <- file.path(td, "sim.rds")
  lazyBase <- file.path(td, "sim_xData")

  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)

  expect_true(file.exists(filename))
  expect_true(file.exists(paste0(lazyBase, ".rdx")))
  expect_true(file.exists(paste0(lazyBase, ".rdb")))

  loaded <- loadSimList(filename, projectPath = td)

  loadedEnv <- loaded@.xData
  userNms <- c("a", "b", "c")
  expect_true(all(userNms %in% ls(loadedEnv)))
  expect_true(all(rlang::env_binding_are_lazy(loadedEnv, userNms)))

  expect_equal(loaded$a, 1:5)
  expect_equal(loaded$b, letters[1:3])
  expect_equal(loaded$c, list(x = 10, y = "z"))

  expect_false(rlang::env_binding_are_lazy(loadedEnv, "a"))
})

test_that("lazy round-trip materializes a file-backed terra SpatRaster on access", {
  skip_if_not_installed("rlang")
  skip_if_not_installed("terra")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  ## Tiny on-disk raster — kept in place via files = FALSE on save.
  rastFile <- file.path(td, "tiny.tif")
  rastVals <- matrix(1:9, nrow = 3)
  terra::writeRaster(terra::rast(rastVals), rastFile, overwrite = TRUE)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["plain"]] <- 42L
  sim@.xData[["r"]]     <- terra::rast(rastFile)

  filename <- file.path(td, "sim.rds")
  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)
  expect_true(file.exists(paste0(file.path(td, "sim_xData"), ".rdx")))
  expect_true(file.exists(rastFile))  # files = FALSE -> backing file untouched

  loaded <- loadSimList(filename, projectPath = td)

  expect_true(all(rlang::env_binding_are_lazy(loaded@.xData, c("plain", "r"))))

  ## Force the raster — promise should resolve to a SpatRaster pointing at
  ## the original on-disk file, with values intact.
  rOut <- loaded$r
  expect_s4_class(rOut, "SpatRaster")
  expect_equal(terra::values(rOut, mat = FALSE),
               terra::values(terra::rast(rastFile), mat = FALSE))
  expect_false(rlang::env_binding_are_lazy(loaded@.xData, "r"))
  expect_true(rlang::env_binding_are_lazy(loaded@.xData, "plain"))
})

test_that("saveSimList does not mutate the caller's simList (Path-wrap leak)", {
  ## Regression: .wrapResiliently used to rebind sim@.xData[[nm]] to Path
  ## objects in place. .xData is an environment, so the caller's live sim
  ## ended up with Path-wrapped rasters after every saveSimList call —
  ## visibly broke the checkpoint module (which calls saveSimList mid-run)
  ## and any code that touched the sim afterwards.
  skip_if_not_installed("terra")

  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  rastFile <- file.path(td, "tiny.tif")
  terra::writeRaster(terra::rast(matrix(1:9, 3)), rastFile, overwrite = TRUE)

  sim <- new("simList")
  paths(sim) <- simPaths
  sim@.xData[["r"]] <- terra::rast(rastFile)
  sim@.xData[["x"]] <- 42L
  sim@.xData[["nested"]] <- new.env(parent = emptyenv())
  sim@.xData[["nested"]]$inner <- "untouched"

  saveSimList(sim, filename = file.path(td, "sim.rds"),
              files = FALSE, projectPath = td)

  expect_s4_class(sim$r, "SpatRaster")
  expect_false(inherits(sim$r, "Path"))
  expect_identical(sim$x, 42L)
  expect_identical(sim$nested$inner, "untouched")
})

test_that("lazy saveSimList omits the .rdx/.rdb when there are no user objects", {
  td <- normPath(withr::local_tempdir())
  simPaths <- list(cachePath = td, inputPath = td, outputPath = td,
                   modulePath = td, scratchPath = td, terraPath = td)

  sim <- new("simList")
  paths(sim) <- simPaths

  filename <- file.path(td, "empty.rds")
  lazyBase <- file.path(td, "empty_xData")

  saveSimList(sim, filename = filename, files = FALSE, lazy = TRUE,
              projectPath = td)

  expect_true(file.exists(filename))
  expect_false(file.exists(paste0(lazyBase, ".rdx")))
  expect_false(file.exists(paste0(lazyBase, ".rdb")))

  loaded <- loadSimList(filename, projectPath = td)
  expect_s4_class(loaded, "simList")
})
