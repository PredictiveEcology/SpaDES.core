test_that("test checkpointing", {
  skip_on_cran()

  testInit(sampleModReqdPkgs, opts = list(spades.recoveryMode = FALSE))

  file <- file.path("chkpnt.qs2")
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
  paths <- list(modulePath = getSampleModules(tmpdir), outputPath = tmpdir)
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
  # rm("._timestamp", envir = envir(simB))
  # rm("._timestamp", envir = envir(simA))

  ## both versions above should yield identical results
  expect_equivalent(simA, simB)
})

test_that("test checkpointing with disk-backed raster", {
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_not_installed("magrittr") ## using tee pipe (`%T>%`)

  testInit(c(sampleModReqdPkgs, "magrittr"), opts = list(spades.recoveryMode = FALSE))

  file <- file.path("chkpnt.qs2")

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
    modulePath = getSampleModules(tmpdir),
    outputPath = tmpdir,
    inputPath = file.path(tmpdir, "inputs")
  )
  simA <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  simA$ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1)

  # Can't create a temporary file because it is not inside projectPath
  tmpRasFilename <- tempfile("tmpRas", fileext = ".grd") |>
    basename() |>
    (function(x) file.path(inputPath(simA), x))() %T>%
    file.create() |>
    normPath()
  if (file.exists(tmpRasFilename)) {
    unlink(tmpRasFilename)
  }
  simA$ras <- writeRaster(simA$ras, filename = tmpRasFilename)
  simA <- spades(simA)

  ## save checkpoints; with load/restore
  set.seed(1234)
  simB <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  simB$ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = 1)
  expect_error(simB$ras <- writeRaster(simA$ras, filename = tmpRasFilename))

  # Eliot uncommented this next line Sept 17, 2019 b/c writeRaster next line newly failed
  # filenames of source and target should be different
  tmpRasFilename <- tempfile("tmpRas", fileext = ".grd") |>
    basename() |>
    (function(x) file.path(inputPath(simA), x))() %T>%
    file.create() |>
    normPath()

  # tmpRasFilename <- tempfile("tmpRas", fileext = ".grd")
  if (file.exists(tmpRasFilename)) {
    unlink(tmpRasFilename)
  }
  simA$ras[] <- simA$ras[]
  simB$ras <- writeRaster(simA$ras, filename = tmpRasFilename)
  end(simB) <- 1
  simB <- spades(simB)
  fns <- Filenames(simB)
  rm(simB)

  simB <- checkpointLoad(file = file.path(paths$outputPath, file))
  end(simB) <- 2
  simB <- spades(simB)

  # Because of no file overwriting, each checkpoint event saves the files with different names
  #  This means that the file backed names will be slightly different, but the raster content and
  #  names are the same -- next line will move from disk to memory
  simA$ras[] <- simA$ras[]
  simB$ras[] <- simB$ras[]
  # Because they did have different file-backed file names, their "varnames" attribute is different
  varnames(simA$ras) <- varnames(simB$ras) <- "tmp"

  # ignore unimportant attributes for tests
  attr(simA$ras, "nParentDirs") <- attr(simB$ras, "nParentDirs") <- NULL
  attr(simA$ras, "tags") <- attr(simB$ras, "tags") <- NULL

  ## both versions above should yield identical results
  expect_true(compareGeom(simA$ras, simB$ras, stopOnError = FALSE))
  expect_equal(simA$ras, simB$ras, ignore_attr = "cpp")
  expect_equivalent(simA, simB)
})
