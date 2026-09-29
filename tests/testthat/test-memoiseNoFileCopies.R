## With `reproducible.useMemoise = TRUE`, caching a SpaDES event (`.useCache = "init"`) memoised
## the UNWRAPPED simList (`makeMemoisable.simList()`, R/cache.R:1308, does `Copy(sim)`), and
## reproducible's `Copy()` of a file-backed SpatRaster wrote a copy beside the original file as
## "<name>_1.tif". Two processes sharing an output folder then overwrote each other's copy
## (FireSense, 2026-09-28: a 1.6 GB climate raster corrupted). reproducible's fix (branch
## fix/cache-file-copies) makes the memoised entry the wrapped object, so `Copy()` is never called
## on a live simList; this only exercises that fix through SpaDES.core's own caching path.

rwModuleCode <- '
defineModule(sim, list(
  name = "rw", description = "writes a file-backed raster into outputPath in a cached init",
  keywords = "", authors = person("A", "B"), childModules = character(0),
  version = list(rw = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year",
  citation = list(), documentation = list(), reqdPkgs = list("terra"),
  parameters = rbind(defineParameter(".useCache", "character", "init", NA, NA, "cache the init event")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("r", "SpatRaster", ""))
))
doEvent.rw <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      sim$r <- terra::writeRaster(terra::rast(nrows = 5, ncols = 5, vals = 7),
                                  file.path(outputPath(sim), "climate_4.2.2.tif"), overwrite = TRUE)
      sim$dt <- data.table::data.table(a = 1:3)
      sim <- scheduleEvent(sim, start(sim) + 1, "rw", "check")
    },
    check = { stopifnot(all(terra::values(sim$r) == 7)) }
  )
  invisible(sim)
}
'

writeRwModule <- function(mp) {
  dir.create(file.path(mp, "rw"), recursive = TRUE, showWarnings = FALSE)
  writeLines(rwModuleCode, file.path(mp, "rw", "rw.R"))
}

## Loads THIS SpaDES.core source tree in a child Rscript, the way reproducible's own
## childProcessPreamble() (tests/testthat/helper-childProcess.R) loads reproducible from source.
spadesCoreChildPreamble <- function() {
  pkgPath <- normalizePath(getNamespaceInfo("SpaDES.core", "path"), mustWork = FALSE)
  fromSource <- !file.exists(file.path(pkgPath, "R", "SpaDES.core.rdb")) &&
    file.exists(file.path(pkgPath, "DESCRIPTION"))
  c(sprintf('.libPaths(%s)', paste0("c(", paste0('"', .libPaths(), '"', collapse = ", "), ")")),
    if (fromSource) sprintf('library(pkgload); load_all("%s", quiet = TRUE)', pkgPath) else
      'suppressMessages(library(SpaDES.core))')
}

test_that("a cached init with a file-backed raster leaves no file copies once memoised", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit(c("terra", "data.table"),
           opts = list(reproducible.useMemoise = TRUE, reproducible.showSimilar = FALSE))
  mp <- file.path(tmpdir, "modules")
  op <- file.path(tmpdir, "out")
  dir.create(op, recursive = TRUE, showWarnings = FALSE)
  writeRwModule(mp)
  paths <- list(modulePath = mp, outputPath = op, cachePath = tmpCache)

  run <- function() simInitAndSpades(times = list(start = 1, end = 2), modules = "rw",
                                     paths = paths, debug = FALSE)
  run() # a miss: cached and memoised
  sim2 <- run() # a memoise hit

  expect_identical(dir(op, all.files = TRUE, no.. = TRUE), "climate_4.2.2.tif")
  expect_identical(normPath(terra::sources(sim2$r)), normPath(file.path(op, "climate_4.2.2.tif")))
  expect_true(all(terra::values(sim2$r) == 7))
})

test_that("two processes running the same cached init do not corrupt the shared raster", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("terra")
  testInit(c("terra", "data.table"),
           opts = list(reproducible.useMemoise = TRUE, reproducible.showSimilar = FALSE))
  mp <- file.path(tmpdir, "modules")
  op <- file.path(tmpdir, "out")
  dir.create(op, recursive = TRUE, showWarnings = FALSE)
  writeRwModule(mp)

  script <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    spadesCoreChildPreamble(),
    'options(reproducible.useMemoise = TRUE, reproducible.showSimilar = FALSE, reproducible.verbose = -1,',
    '        spades.useRequire = FALSE, spades.moduleCodeChecks = FALSE)',
    sprintf('paths <- list(modulePath = "%s", outputPath = "%s", cachePath = "%s")', mp, op, tmpCache),
    'ok <- TRUE',
    'for (i in 1:3) {',
    '  sim <- simInitAndSpades(times = list(start = 1, end = 2), modules = "rw", paths = paths, debug = FALSE)',
    '  ok <- ok && all(terra::values(sim$r) == 7)',
    '}',
    'cat(if (ok) "ALLGOOD" else "BAD", "\\n")'), script)

  logs <- file.path(withr::local_tempdir("logs"), c("p1.log", "p2.log"))
  system(sprintf("Rscript %s > %s 2>&1 & Rscript %s > %s 2>&1; wait", script, logs[1], script, logs[2]))
  for (lg in logs) expect_true(any(grepl("ALLGOOD", readLines(lg))), info = paste(readLines(lg), collapse = "\n"))

  ## nothing but the one raster: no "_1" copy, no temporary file
  expect_identical(dir(op, all.files = TRUE, no.. = TRUE), "climate_4.2.2.tif")
  expect_equal(terra::values(terra::rast(file.path(op, "climate_4.2.2.tif")))[1], 7)
})
