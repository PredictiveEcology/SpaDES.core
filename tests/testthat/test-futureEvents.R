test_that("test spades.futureEvents", {
  skip_on_cran() ## these are longer tests (~2m)
  # skip_on_os("windows")
  testInit(c(sampleModReqdPkgs, "future", "ggplot2"),
           opts = list("reproducible.useMemoise" = FALSE,
                       "spades.futureEvents" = TRUE))
  # tmpdir <- tempdir2(.rndstr())
  modPath <- system.file("sampleModules", package = "SpaDES.core")
  origFiles <- dir(modPath, full.names = TRUE, recursive = TRUE)
  tmpFiles <- file.path(tmpdir, dir(modPath, recursive = TRUE))
  checkPath(unique(dirname(tmpFiles)), create = TRUE)
  file.copy(origFiles, tmpFiles)
  modPath <- tmpdir


  newModule("test", path = modPath, open = FALSE)
  mods1 <- c("caribouMovement", "fireSpread", "test")
  for (mod in mods1) {
    f1 <- file.path(modPath, mod, paste0(mod, ".R"))
    ll <- readLines(f1)
    lin <- grep("Burn\\(sim\\)|Move\\(sim\\)|plotFun\\(sim\\)", ll)
    newModCode <- c(
      ll[seq(lin - 1)], "    system.time(for (i in 1:1e6) rnorm(10))",
      if (mod == "test") "sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, 'test', 'plot')",
      ll[lin:length(ll)]
    )
    lin <- grep("expectsInput", newModCode)[1]
    newModCode <- c(
      newModCode[seq(lin - 1)],
      if (mod == "test") "    expectsInput(objectName = 'caribou', objectClass = 'raster', desc = NA, sourceURL = NA),",
      newModCode[(lin):length(newModCode)]
    )
    writeLines(newModCode, con = f1)
  }

  if (isWindows()) {
    future::plan(future.callr::callr, workers = 3)
  } else {
    future::plan(future::multisession, workers = 3)
  }
  #future::plan("sequential", workers = 3)

  mods <- c("caribouMovement", "randomLandscapes", "fireSpread", "test")
  # Example of changing parameter values
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
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  mySim@params$test$.plotInitialTime <- 0
  mySim@params$test$.plotInterval <- 1

  options("spades.futureEvents" = TRUE)
  set.seed(1)
  simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
  options("spades.futureEvents" = FALSE)
  set.seed(1)
  simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
  # expect_true(isTRUE(all.equal(completed(simsFALSE), completed(simsTRUE))))
  s2 <- completed(simsFALSE)[, 1:3]
  data.table::setorderv(s2, c("eventTime", "moduleName", "eventType"))
  s1 <- completed(simsTRUE)[, 1:3]
  data.table::setorderv(s1, c("eventTime", "moduleName", "eventType"))

  mySim@depends@dependencies$caribouMovement@timeunit <- "year"
  options("spades.futureEvents" = TRUE)
  set.seed(1)
  simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
  options("spades.futureEvents" = FALSE)
  set.seed(1)
  simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)

  s2 <- completed(simsFALSE)[, 1:3]
  data.table::setorderv(s2, c("eventTime", "moduleName", "eventType"))
  s1 <- completed(simsTRUE)[, 1:3]
  data.table::setorderv(s1, c("eventTime", "moduleName", "eventType"))
  expect_true(isTRUE(all.equal(s2, s1)))

})
