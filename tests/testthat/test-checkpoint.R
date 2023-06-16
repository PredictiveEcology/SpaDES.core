test_that("test checkpointing", {
  skip_on_cran()

  testInitOut <- testInit("NLMR", smcc = FALSE, opts = list(spades.recoveryMode = FALSE))

  file <- file.path("chkpnt.qs")
  ## save checkpoints; no load/restore
  set.seed(1234)
  times <- list(start = 0, end = 2, timeunit = "second")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    checkpoint = list(interval = 1, file = file),
    randomLandscapes = list(.plotInitialTime = NA),
    caribouMovement = list(.plotInitialTime = NA, torus = TRUE)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES.core"),
    outputPath = tmpdir
  )
  simA <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  simA <- suppressWarnings(spades(simA))

  ## save checkpoints; with load/restore
  set.seed(1234)
  times <- list(start = 0, end = 2, timeunit = "second")
  simB <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  end(simB) <- 1
  simB <- suppressWarnings(spades(simB))
  rm(simB)

  simB <- checkpointLoad(file = file.path(paths$outputPath, file))
  end(simB) <- 2
  simB <- spades(simB)

  rm("._startClockTime", envir = envir(simB))
  rm("._startClockTime", envir = envir(simA))
  rm("._timestamp", envir = envir(simB))
  rm("._timestamp", envir = envir(simA))

  ## both versions above should yield identical results
  expect_equal(simA, simB)
})

test_that("test checkpointing with disk-backed raster", {
  skip_on_cran()

  testInitOut <- testInit("NLMR", smcc = FALSE, opts = list(spades.recoveryMode = FALSE))

  file <- file.path("chkpnt.qs")

  ## save checkpoints; no load/restore
  set.seed(1234)
  times <- list(start = 0, end = 2, timeunit = "second")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    checkpoint = list(interval = 1, file = file),
    randomLandscapes = list(.plotInitialTime = NA_integer_),
    caribouMovement = list(.plotInitialTime = NA_integer_, torus = TRUE)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES.core"),
    outputPath = tmpdir
  )
  simA <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  simA$ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1)
  tmpRasFilename <- tempfile("tmpRas", fileext = ".grd") %T>%
    file.create() %>%
    normPath()
  if (file.exists(tmpRasFilename)) unlink(tmpRasFilename)
  simA$ras <- writeRaster(simA$ras, filename = tmpRasFilename)
  simA <- spades(simA)

  ## save checkpoints; with load/restore
  set.seed(1234)
  simB <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  simB$ras <- terra::rast(terra::ext(0,10,0,10), vals = 1)
  expect_error(simB$ras <- writeRaster(simA$ras, filename = tmpRasFilename))

  # Eliot uncommented this next line Sept 17, 2019 b/c writeRaster next line newly failed
  # filenames of source and target should be different
  tmpRasFilename <- tempfile("tmpRas", fileext = ".grd")
  if (file.exists(tmpRasFilename)) unlink(tmpRasFilename)
  simA$ras[] <- getValues(simA$ras)
  simB$ras <- writeRaster(simA$ras, filename = tmpRasFilename)
  end(simB) <- 1
  simB <- spades(simB)
  rm(simB)

  simB <- checkpointLoad(file = file.path(paths$outputPath, file))
  end(simB) <- 2
  simB <- spades(simB)

  # Because of no file overwriting, each checkpoint event saves the files with different names
  #  This means that the file backed names will be slightly different, but the raster content and
  #  names are the same -- next line will move from disk to memory
  simA$ras[] <- simA$ras[]
  simB$ras[] <- simB$ras[]
  # Because they did have different file-backed file names, their "names" attribute is different
  names(simA$ras) <- names(simB$ras) <- "tmp"

  ## both versions above should yield identical results
  expect_true(all.equal(simA, simB, check.environment = FALSE))
})
