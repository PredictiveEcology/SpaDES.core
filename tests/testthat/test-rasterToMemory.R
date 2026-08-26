## rasterToMemory() / rasterCreate(): pulling file-backed rasters into RAM.

test_that("rasterToMemory brings a file-backed SpatRaster into memory", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  f <- file.path(tmpdir, "r.tif")
  terra::writeRaster(terra::rast(nrows = 5, ncols = 5, vals = 1:25), f)
  r <- terra::rast(f)
  expect_true(any(nchar(Filenames(r)) > 0))

  m <- rasterToMemory(r)

  expect_s4_class(m, "SpatRaster")
  expect_identical(nchar(Filenames(m)), 0L)
  expect_identical(as.numeric(terra::values(m)), as.numeric(1:25))
})

test_that("rasterToMemory leaves an in-memory raster alone", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  r <- terra::rast(nrows = 2, ncols = 2, vals = 1:4)

  m <- rasterToMemory(r)

  expect_s4_class(m, "SpatRaster")
  expect_identical(as.numeric(terra::values(m)), as.numeric(1:4))
})

test_that("rasterToMemory passes a non-raster, non-character object through", {
  skip_on_cran()
  testInit()

  expect_identical(rasterToMemory(1:3), 1:3)
  expect_identical(rasterToMemory(list(a = 1)[["a"]]), 1)
})

test_that("rasterToMemory reads a character path as a raster file", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  f <- file.path(tmpdir, "byName.tif")
  terra::writeRaster(terra::rast(nrows = 3, ncols = 3, vals = 1:9), f)

  m <- rasterToMemory(f)

  expect_s4_class(m, "SpatRaster")
  expect_identical(nchar(Filenames(m)), 0L)
  expect_identical(as.numeric(terra::values(m)), as.numeric(1:9))
})

test_that("rasterToMemory maps over a list", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  f <- file.path(tmpdir, "inList.tif")
  terra::writeRaster(terra::rast(nrows = 2, ncols = 2, vals = 1:4), f)

  out <- rasterToMemory(list(r = terra::rast(f), n = 42))

  expect_type(out, "list")
  expect_identical(nchar(Filenames(out$r)), 0L)
  expect_identical(out$n, 42)
})

test_that("rasterToMemory on a simList converts every raster it holds", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  f <- file.path(tmpdir, "r2.tif")
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, vals = 1:16), f)

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim$ras <- terra::rast(f)
  sim$notARaster <- 42

  out <- rasterToMemory(sim)

  expect_s4_class(out, "simList")
  expect_identical(nchar(Filenames(out$ras)), 0L)
  expect_identical(out$notARaster, 42)
})

test_that("rasterCreate passes a non-raster through unchanged", {
  testInit()

  expect_identical(rasterCreate(1:3), 1:3)
  expect_identical(rasterCreate("a"), "a")
})

test_that("rasterCreate returns an empty raster with the same geometry", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()

  ## terra::rast(x) deliberately keeps only the geometry -- rasterToMemory()
  ## assigns the values immediately afterwards, so this is the contract
  r <- terra::rast(nrows = 3, ncols = 3, vals = 1:9)

  out <- rasterCreate(r)

  expect_s4_class(out, "SpatRaster")
  expect_identical(dim(out), dim(r))
  expect_identical(as.vector(terra::ext(out)), as.vector(terra::ext(r)))
  expect_false(terra::hasValues(out))
})

## ---- the raster (as opposed to terra) branches ---------------------------

test_that("rasterToMemory brings a file-backed RasterLayer into memory", {
  skip_on_cran()
  skip_if_not_installed("raster")
  testInit()

  f <- file.path(tmpdir, "rl.tif")
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, vals = 1:16), f)
  r <- raster::raster(f)
  expect_false(raster::inMemory(r))

  m <- rasterToMemory(r)

  expect_s4_class(m, "RasterLayer")
  expect_true(raster::inMemory(m))
})

test_that("rasterToMemory keeps a RasterStack a RasterStack", {
  skip_on_cran()
  skip_if_not_installed("raster")
  testInit()

  f <- file.path(tmpdir, "rs.tif")
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, vals = 1:16), f)

  m <- rasterToMemory(raster::stack(f))

  expect_s4_class(m, "RasterStack")
})

test_that("rasterCreate rebuilds each Raster* subclass as itself", {
  skip_on_cran()
  skip_if_not_installed("raster")
  testInit()

  f <- file.path(tmpdir, "rc.tif")
  terra::writeRaster(terra::rast(nrows = 4, ncols = 4, vals = 1:16), f)

  expect_s4_class(rasterCreate(raster::raster(f)), "RasterLayer")
  expect_s4_class(rasterCreate(raster::stack(f)), "RasterStack")

  fb <- file.path(tmpdir, "rb.tif")
  terra::writeRaster(c(terra::rast(f), terra::rast(f)), fb)
  expect_s4_class(rasterCreate(raster::brick(fb)), "RasterBrick")
})
