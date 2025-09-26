test_that("test spades.futureEvents", {
  skip_on_cran() ## these are longer tests (~2m)

  needPkgs <- c(sampleModReqdPkgs, "future", "ggplot2", "future.callr")
  capture_warnings(skip_if_not_installed(needPkgs))

  # skip_on_os("windows")
  testInit(needPkgs, opts = list(reproducible.useMemoise = FALSE,
                                 spades.futureEvents = TRUE))
  # tmpdir <- tempdir2(.rndstr())
  modPath <- getSampleModules(tmpdir)
  origFiles <- dir(modPath, full.names = TRUE, recursive = TRUE)
  tmpFiles <- file.path(tmpdir, dir(modPath, recursive = TRUE))
  checkPath(unique(dirname(tmpFiles)), create = TRUE)
  expect_true(all(file.copy(origFiles, tmpFiles)))
  modPath <- tmpdir

  newModule("test", path = modPath, open = FALSE)
  mods1 <- c("caribouMovement", "fireSpread", "test")
  for (mod in mods1) {
    f1 <- file.path(modPath, mod, paste0(mod, ".R"))
    ll <- readLines(f1)
    lin <- grep("Burn\\(sim\\)|Move\\(sim\\)|plotFun\\(sim\\)", ll)
    newModCode <- c(
      ll[seq(lin - 1)], "    # Sys.sleep(0.2)", # comment out because not needed any more
      if (mod == "test") "sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, 'test', 'plot')",
      ll[lin:length(ll)]
    )
    lin <- grep("expectsInput", newModCode)[1]
    newModCode <- c(
      newModCode[seq(lin - 1)],
      if (mod == "test") "    expectsInput(objectName = 'caribou', objectClass = 'SpatRaster', desc = NA, sourceURL = NA),",
      if (mod == "test") "    expectsInput(objectName = 'landscape', objectClass = 'SpatRaster', desc = NA, sourceURL = NA),",
      newModCode[(lin):length(newModCode)]
    )

    writeLines(newModCode, con = f1)
  }

  if (isWindows()) {
    oldPlan <- future::plan(future.callr::callr, workers = 3)
  } else {
    oldPlan <- future::plan(future::multisession, workers = 3)
  }
  # oldPlan <- future::plan("sequential", workers = 3)
  on.exit(future::plan(oldPlan), add = TRUE)

  mods <- c("caribouMovement", "randomLandscapes", "fireSpread", "test")
  ## Example of changing parameter values
  withr::local_options(
    list(spades.saveFileExtensions = data.frame(exts = ".grd", fun = "writeRaster", package = "terra"))
  )
  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init", .showSimilar = TRUE)
    ),
    modules = mods,
    paths = list(modulePath = modPath,
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = rep(c("landscape", "caribou"), 2),
                         saveTime = rep(c(1, 2), each = 2),
                         fun = rep(c("writeRaster", "writeVector"), 2),
                         package = "terra",
                         stringsAsFactors = FALSE)
  )

  mySim@params$test$.plotInitialTime <- 0
  mySim@params$test$.plotInterval <- 1

  withr::local_options(list(spades.futureEvents = TRUE))
  set.seed(1)
  simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE) |>
    suppressWarnings() ## TODO: Error in cur$moduleName : $ operator is invalid for atomic vectors

  withr::local_options(list(spades.futureEvents = FALSE))
  fls <- outputs(simsTRUE)$file
  expect_true(all(file.exists(fls)))
  unlink(fls)
  set.seed(1)
  simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE) |>
    suppressWarnings()
  expect_true(isTRUE(all.equal(completed(simsFALSE)[, 1:4], completed(simsTRUE)[, 1:4])))
  fls <- outputs(simsFALSE)$file
  expect_true(all(file.exists(fls)))
  unlink(fls)

  mySim@depends@dependencies$caribouMovement@timeunit <- "year"
  options("spades.futureEvents" = TRUE)
  set.seed(1)
  simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE) |>
    suppressWarnings()
  fls <- outputs(simsTRUE)$file
  expect_true(all(file.exists(fls)))
  unlink(fls)
  options("spades.futureEvents" = FALSE)
  set.seed(1)
  simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE) |>
    suppressWarnings()
  fls <- outputs(simsFALSE)$file
  expect_true(all(file.exists(fls)))
  unlink(fls)
  expect_true(isTRUE(all.equal(completed(simsFALSE)[, 1:4], completed(simsTRUE)[, 1:4])))
})
