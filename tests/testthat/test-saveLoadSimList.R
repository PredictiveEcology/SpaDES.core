## saveSimList() / loadSimList() round trips, file formats, and the deprecated
## argument handling.

simToSave <- function(end = 2) {
  sim <- simInit(times = list(start = 0, end = end, timeunit = "year"))
  sim$anInteger <- 1:5
  sim$aCharacter <- "hello"
  sim$aList <- list(a = 1, b = "two")
  sim <- scheduleEvent(sim, 1, "someModule", "someEvent")
  sim
}

expectSameSim <- function(out, orig) {
  expect_s4_class(out, "simList")
  expect_identical(out$anInteger, orig$anInteger)
  expect_identical(out$aCharacter, orig$aCharacter)
  expect_identical(out$aList, orig$aList)
  expect_identical(as.numeric(end(out)), as.numeric(end(orig)))
  expect_identical(timeunit(out), timeunit(orig))
}

test_that("saveSimList writes an .rds and loadSimList reads it back", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  f <- file.path(tmpdir, "sim.rds")

  suppressMessages(saveSimList(sim, f))
  expect_true(file.exists(f))

  out <- suppressMessages(loadSimList(f))
  expectSameSim(out, sim)
})

test_that("saveSimList writes a .qs2 and loadSimList reads it back", {
  skip_on_cran()
  skip_if_not_installed("qs2")
  testInit()

  sim <- simToSave()
  f <- file.path(tmpdir, "sim.qs2")

  suppressMessages(saveSimList(sim, f))
  expect_true(file.exists(f))

  out <- suppressMessages(loadSimList(f))
  expectSameSim(out, sim)
})

test_that("a round trip preserves the event queue", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))
  out <- suppressMessages(loadSimList(f))

  expect_true("someModule" %in% events(out)$moduleName)
  expect_identical(NROW(events(out)), NROW(events(sim)))
})

test_that("saveSimList rejects an unsupported file extension", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  expect_error(saveSimList(sim, file.path(tmpdir, "sim.txt")))
})

test_that("loadSimList rejects an unsupported file extension", {
  skip_on_cran()
  testInit()

  expect_error(loadSimList(file.path(tmpdir, "sim.txt")))
})

test_that("saveSimList accepts the sim by name from an environment", {
  skip_on_cran()
  testInit()

  e <- new.env()
  e$mySim <- simToSave()
  f <- file.path(tmpdir, "sim.rds")

  suppressMessages(saveSimList("mySim", f, envir = e))
  expect_true(file.exists(f))

  out <- suppressMessages(loadSimList(f))
  expectSameSim(out, e$mySim)
})

test_that("saveSimList with files = FALSE still round-trips the objects", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  f <- file.path(tmpdir, "sim.rds")

  suppressMessages(saveSimList(sim, f, files = FALSE))
  out <- suppressMessages(loadSimList(f))

  expectSameSim(out, sim)
})

test_that("the deprecated fileBackend argument warns", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  expect_warning(saveSimList(sim, file.path(tmpdir, "sim.rds"), fileBackend = 0),
                 "fileBackend argument is deprecated")
})

test_that("the deprecated filebackedDir argument warns", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  expect_warning(saveSimList(sim, file.path(tmpdir, "sim.rds"), filebackedDir = "x"),
                 "filebackedDir is deprecated")
})

test_that("the misnamed fileBackedDir argument is normalised, not an error", {
  skip_on_cran()
  testInit()

  ## this used to fail with "object 'filebackedDir' not found": the cleanup
  ## block referred to a name that is not a formal of saveSimList()
  sim <- simToSave()
  expect_warning(saveSimList(sim, file.path(tmpdir, "sim.rds"), fileBackedDir = "x"),
                 "filebackedDir is deprecated")
})

test_that("loadSimList can override the sim's paths", {
  skip_on_cran()
  testInit()

  sim <- simToSave()
  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))

  newOut <- checkPath(file.path(tmpdir, "reloadedOutputs"), create = TRUE)
  out <- suppressMessages(loadSimList(f, paths = list(outputPath = newOut)))

  expect_s4_class(out, "simList")
  expect_true(any(grepl("reloadedOutputs", unlist(paths(out)))))
})

test_that("a sim with modules round-trips its metadata", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  ## NOTE: this deliberately stops short of running the reloaded sim.
  ## saveSimList()/loadSimList() does not currently restore a module's
  ## functions -- .mods$<module> comes back holding only `mod` and `Par`, so
  ## spades(loadSimList(f)) fails with "object 'doEvent.<module>' not found".
  ## saveSimList() does bundle the module source files, so the intent looks to
  ## be that load re-parses them; that is not implemented. Asserting either
  ## runnability (fails) or the stripped state (cements the gap) would be
  ## wrong, so this asserts only the metadata that genuinely survives.
  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )
  f <- file.path(tmpdir, "withModules.rds")
  suppressMessages(saveSimList(sim, f))

  out <- suppressMessages(loadSimList(f))

  expect_s4_class(out, "simList")
  expect_true("randomLandscapes" %in% unlist(modules(out)))
  expect_identical(as.numeric(end(out)), as.numeric(end(sim)))
  expect_true(NROW(events(out)) > 0)
})

test_that("a file-backed raster survives a save/load round trip", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit("terra")

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  ## a raster written to disk, so .dealWithRasterBackends has work to do
  tf <- file.path(tmpdir, "aRaster.tif")
  r <- terra::rast(nrows = 10, ncols = 10, vals = seq_len(100))
  terra::writeRaster(r, tf, overwrite = TRUE)
  sim$aRaster <- terra::rast(tf)

  f <- file.path(tmpdir, "withRaster.rds")
  suppressMessages(saveSimList(sim, f))

  out <- suppressMessages(loadSimList(f))

  expect_s4_class(out, "simList")
  expect_true(inherits(out$aRaster, "SpatRaster"))
  expect_identical(terra::ncell(out$aRaster), terra::ncell(r))
  expect_equal(terra::values(out$aRaster), terra::values(r))
})

## ---- the two ways to use saveSimList() ---------------------------------
##
## 1. PORTABLE: save everything -- objects, metadata and the files behind
##    file-backed objects -- so the simulation can be moved elsewhere.
##    `projectPath` is the root everything is stored relative to.
##
## 2. IN PLACE (metadata only): save the information but not the file bundle,
##    on the assumption the user still has the original paths. Reload and use
##    `outputs(sim)` / `outputPath(sim)` to find what the run wrote. This is
##    the practical choice when a run holds far too many objects to bundle.

test_that("mode 1 (portable): saveSimList bundles file-backed objects into an archive", {
  skip_on_cran()
  skip_if_not_installed("terra")
  skip_if_not_installed("archive")
  testInit("terra")

  proj <- checkPath(file.path(tmpdir, "projPortable"), create = TRUE)
  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  tf <- file.path(proj, "r.tif")
  suppressWarnings(terra::writeRaster(
    terra::rast(nrows = 20, ncols = 20, vals = seq_len(400)), tf, overwrite = TRUE))
  sim$r <- terra::rast(tf)

  ## passing projectPath must not error -- it used to, for any sim holding a
  ## file-backed object, because the archiver was not run from projectPath
  expect_no_error(
    suppressMessages(saveSimList(sim, file.path(proj, "s.rds"), projectPath = proj))
  )

  made <- dir(proj, pattern = "^s\\.", full.names = TRUE)
  expect_length(made, 1L)

  ## both the simList and the raster's backing file are in the archive
  entries <- archive::archive(made)$path
  expect_true(any(grepl("s\\.rds$", entries)))
  expect_true(any(grepl("r\\.tif$", entries)))
})

test_that("mode 1 (portable): saving does not leave the working directory moved", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit("terra")

  proj <- checkPath(file.path(tmpdir, "projWd"), create = TRUE)
  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  tf <- file.path(proj, "r.tif")
  suppressWarnings(terra::writeRaster(
    terra::rast(nrows = 10, ncols = 10, vals = seq_len(100)), tf, overwrite = TRUE))
  sim$r <- terra::rast(tf)

  before <- getwd()
  suppressMessages(saveSimList(sim, file.path(proj, "s.rds"), projectPath = proj))
  expect_identical(getwd(), before)
})

test_that("mode 2 (in place): a metadata save reloads the outputs manifest", {
  skip_on_cran()
  testInit()

  proj <- checkPath(file.path(tmpdir, "projInPlace"), create = TRUE)
  outp <- checkPath(file.path(proj, "outputs"), create = TRUE)

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"),
                 paths = list(outputPath = outp))
  sim$bigObject <- 1:10
  outputs(sim) <- data.frame(objectName = "bigObject", saveTime = 0,
                             stringsAsFactors = FALSE)
  ran <- suppressMessages(spades(sim))

  f <- file.path(proj, "meta.rds")
  suppressMessages(saveSimList(ran, f, projectPath = proj, files = FALSE))

  back <- suppressMessages(loadSimList(f, projectPath = proj))

  ## the point of this mode: the manifest of what the run wrote comes back,
  ## pointing at files that are still on disk where the run left them
  expect_identical(NROW(outputs(back)), 1L)
  expect_identical(outputs(back)$objectName, "bigObject")
  expect_true(all(file.exists(outputs(back)$file)))
  expect_true(any(grepl("outputs", outputPath(back))))

  ## and those files are loadable by the user, which is the whole workflow
  reloaded <- readRDS(outputs(back)$file[1])
  expect_identical(reloaded, 1:10)
})

test_that("mode 2 (in place): a metadata save is much smaller than a bundled one", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit("terra")

  proj <- checkPath(file.path(tmpdir, "projSize"), create = TRUE)
  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  tf <- file.path(proj, "big.tif")
  suppressWarnings(terra::writeRaster(
    terra::rast(nrows = 300, ncols = 300, vals = runif(90000)), tf, overwrite = TRUE))
  sim$r <- terra::rast(tf)

  bundled <- file.path(proj, "bundled.rds")
  suppressMessages(saveSimList(sim, bundled, projectPath = proj))
  bundledFile <- dir(proj, pattern = "^bundled\\.", full.names = TRUE)[1]

  metaOnly <- file.path(proj, "metaOnly.rds")
  suppressMessages(saveSimList(sim, metaOnly, projectPath = proj, files = FALSE))

  expect_true(file.exists(bundledFile))
  expect_true(file.exists(metaOnly))
  expect_lt(file.size(metaOnly), file.size(bundledFile))
})

test_that("mode 1 (portable): a sim moved to a new location keeps its file-backed objects", {
  skip_on_cran()
  skip_if_not_installed("terra")
  skip_if_not_installed("archive")
  testInit("terra")

  ## The wrap machinery re-roots a file-backed object by recording WHICH named
  ## path it sits under (relToWhere) plus its name relative to that path. So a
  ## portable save requires the backing file to live under one of the sim's
  ## paths -- here outputPath. A file outside every named path gets an empty
  ## relToWhere and cannot be re-rooted; see the note in the test below.
  projA <- checkPath(file.path(tmpdir, "projA"), create = TRUE)
  outp <- checkPath(file.path(projA, "outputs"), create = TRUE)

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"),
                 paths = list(outputPath = outp,
                              cachePath = file.path(projA, "cache")))
  tf <- file.path(outp, "r.tif")
  suppressWarnings(terra::writeRaster(
    terra::rast(nrows = 20, ncols = 20, vals = seq_len(400)), tf, overwrite = TRUE))
  sim$r <- terra::rast(tf)

  suppressMessages(saveSimList(sim, file.path(projA, "s.rds"), projectPath = projA))
  made <- dir(projA, pattern = "^s\\.", full.names = TRUE)[1]

  ## the archive keeps the file under its named-path directory
  expect_true(any(grepl("outputs/r\\.tif$", archive::archive(made)$path)))

  ## move the archive somewhere else and destroy the original entirely
  projB <- checkPath(file.path(tmpdir, "projB"), create = TRUE)
  file.copy(made, file.path(projB, basename(made)))
  unlink(projA, recursive = TRUE)

  out <- suppressMessages(loadSimList(file.path(projB, basename(made)),
                                      projectPath = projB))

  expect_s4_class(out, "simList")
  expect_true(inherits(out$r, "SpatRaster"))
  expect_equal(as.numeric(terra::values(out$r)), as.numeric(seq_len(400)))
})

## Module code does not survive saveSimList(): Copy(objects = 2) -- the path
## .wrap.simList() takes -- rebuilds each .mods[[module]] env and copies only
## .modObjs back into it. loadSimList() re-parses the source to restore it.
## See #388.

simWithModule <- function(tmpdir, end = 1) {
  mp <- getSampleModules(tmpdir)
  simInit(times = list(start = 0, end = end, timeunit = "year"),
          modules = list("randomLandscapes"),
          paths = list(modulePath = mp, outputPath = tmpdir,
                       cachePath = tmpdir, inputPath = tmpdir))
}

test_that("loadSimList restores module functions so the simList can be run", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simWithModule(tmpdir))
  expect_true("doEvent.randomLandscapes" %in% ls(sim$.mods$randomLandscapes))

  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))
  out <- suppressMessages(loadSimList(f))

  expect_true("doEvent.randomLandscapes" %in% ls(out$.mods$randomLandscapes))
  expect_true(is.function(out$.mods$randomLandscapes$doEvent.randomLandscapes))
  ## the module's own helpers come back too, not just the dispatcher
  expect_true(all(c("Init", "makeNLM") %in% ls(out$.mods$randomLandscapes)))

  ## the point of the exercise: a reloaded simList runs
  expect_s4_class(suppressMessages(spades(out, .plotInitialTime = NA)), "simList")
})

test_that("loadSimList's re-parse leaves `mod` state alone", {
  skip_on_cran()
  testInit()

  sim <- suppressMessages(simWithModule(tmpdir))
  ## .modObjs is what the `mod` active binding reads through; re-parsing must
  ## not call newEnvsByModule(), which would replace it with an empty env.
  sim$.modObjs$randomLandscapes$carriedOver <- "keep me"

  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))
  out <- suppressMessages(loadSimList(f))

  expect_identical(out$.modObjs$randomLandscapes$carriedOver, "keep me")
})

test_that("loadSimList's re-parse does not overwrite end-state metadata", {
  skip_on_cran()
  testInit()

  sim <- suppressMessages(simWithModule(tmpdir))
  ## end-state that differs from what the module's defineModule() declares
  end(sim) <- 7
  sim <- scheduleEvent(sim, 3, "randomLandscapes", "someLaterEvent")
  nEvents <- NROW(sim@events)

  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))
  out <- suppressMessages(loadSimList(f))

  expect_identical(as.numeric(end(out)), 7)
  expect_identical(NROW(out@events), nEvents)
  expect_identical(unlist(modules(out), use.names = FALSE), "randomLandscapes")
})

test_that("loadSimList warns, rather than errors, when module source is gone", {
  skip_on_cran()
  testInit()

  sim <- suppressMessages(simWithModule(tmpdir))
  f <- file.path(tmpdir, "sim.rds")
  suppressMessages(saveSimList(sim, f))

  ## the metadata-only use case: the module path no longer exists
  unlink(file.path(modulePath(sim), "randomLandscapes"), recursive = TRUE)

  expect_warning(out <- suppressMessages(loadSimList(f)), "Could not find source code")
  expect_s4_class(out, "simList")
  ## loaded and inspectable, just not runnable
  expect_identical(unlist(modules(out), use.names = FALSE), "randomLandscapes")
  expect_false("doEvent.randomLandscapes" %in% ls(out$.mods$randomLandscapes))
})
