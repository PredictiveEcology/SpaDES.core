test_that("test cache", {
  library(igraph)
  library(reproducible)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit({
    detach("package:reproducible")
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  sims <- experiment(mySim, replicates = 2, cache = TRUE)
  out <- showCache(sims[[1]])
  expect_true(NROW(out[tagValue == "spades"]) == 2) # 2 cached copies
  expect_true(NROW(unique(out$artifact)) == 2) # 2 cached copies
  expect_output(print(out), "cacheId")
  expect_output(print(out), "simList")
  expect_true(NROW(out[tagKey != "otherFunctions"]) == 16) #
  expect_message(sims <- Cache(experiment, mySim, replicates = 2, cache = TRUE),
                 "loading cached result from previous spades call")

  out2 <- showCache(sims[[1]])

  # 2 original times, 2 cached times per spades, 1 experiment time
  expect_true(NROW(out2[tagKey == "accessed"]) == 5)

  # 2 cached copies of spades, 1 experiment
  expect_true(NROW(unique(out2$artifact)) == 3)

  clearCache(sims[[1]])
  out <- showCache(sims[[1]])
  expect_true(NROW(out) == 0)
})

test_that("test event-level cache", {
  library(igraph)
  library(reproducible)
  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)

  on.exit({
    detach("package:reproducible")
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)
  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  set.seed(1123)
  expect_true(!"Using cached copy of init event in randomLandscapes module" %in%
                capture_output(sims <- spades(Copy(mySim), notOlderThan = Sys.time())))
  #sims <- spades(Copy(mySim), notOlderThan = Sys.time()) ## TODO: fix this test
  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires

  mess1 <- capture_output(sims <- spades(Copy(mySim)))
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))

  clearCache(sims)
})

test_that("test module-level cache", {
  library(igraph)
  library(reproducible)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit({
    detach("package:reproducible")
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  expect_true(file.create(tmpfile))
  tmpfile <- normPath(tmpfile)
  try(clearCache(tmpdir), silent = TRUE)

  # Example of changing parameter values
  times <- list(start = 0.0, end = 1.0, timeunit = "year")
  mySim <- simInit(
    times = times,
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = times$start, .useCache = TRUE)
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  set.seed(1123)
  pdf(tmpfile)
  expect_true(!("Using cached copy of init event in randomLandscapes module" %in%
                  capture_output(sims <- spades(Copy(mySim), notOlderThan = Sys.time()))))
  #sims <- spades(Copy(mySim), notOlderThan = Sys.time())
  dev.off()

  expect_true(file.info(tmpfile)$size > 20000)
  unlink(tmpfile)

  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires

  # The cached version will be identical for both events (init and plot),
  # but will not actually complete the plot, because plotting isn't cacheable
  pdf(tmpfile)
  mess1 <- capture_output(sims <- spades(Copy(mySim)))
  dev.off()

  expect_true(file.info(tmpfile)$size < 10000)
  unlink(tmpfile)

  expect_true(any(grepl(pattern = "Using cached copy of randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))

  clearCache(sims)
})


test_that("test .prepareOutput", {
  library(igraph)
  library(reproducible)
  library(raster)

  tmpdir <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  on.exit({
    detach("package:reproducible")
    detach("package:igraph")
    detach("package:raster")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  try(clearCache(tmpdir), silent = TRUE)

  times <- list(start = 0.0, end = 1, timeunit = "year")
  mapPath <- system.file("maps", package = "quickPlot")
  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[-3],
    stringsAsFactors = FALSE
  )
  layers <- lapply(filelist$files, rasterToMemory)
  landscape <- raster::stack(layers)

  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA)
    ),
    modules = list("fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    objects = c("landscape")
  )

  simCached1 <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time())
  simCached2 <- spades(Copy(mySim), cache = TRUE)

  if (interactive()) {
    cat(file = "~/tmp/out.txt", names(params(mySim)$.progress), append = FALSE)
    cat(file = "~/tmp/out.txt", "\n##############################\n", append = TRUE)
    cat(file = "~/tmp/out.txt", names(params(simCached1)$.progress), append = TRUE)
    cat(file = "~/tmp/out.txt", "\n##############################\n", append = TRUE)
    cat(file = "~/tmp/out.txt", names(params(simCached2)$.progress), append = TRUE)
    cat(file = "~/tmp/out.txt", "\n##############################\n", append = TRUE)
    cat(file = "~/tmp/out.txt", all.equal(simCached1, simCached2), append = TRUE)
  }
  expect_true(isTRUE(all.equal(simCached1, simCached2)))

  clearCache(tmpdir)
})

test_that("test .robustDigest for simLists", {
  library(igraph)
  library(reproducible)

  tmpdir <- tempdir()
  tmpCache <- file.path(tempdir(), "testCache") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    setwd(cwd)
    detach("package:reproducible")
    detach("package:igraph")
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  modName <- "test"
  newModule(modName, path = tmpdir)
  fileName <- file.path(modName, paste0(modName,".R"))
  newCode <- "\"hi\"" # this will be added below in 2 different spots

  args = list(modules = list("test"),
              paths = list(modulePath = tmpdir, cachePath = tmpCache),
              params = list(test = list(.useCache = ".inputObjects")))

  try(clearCache(x = tmpCache), silent = TRUE)

  expect_message(do.call(simInit, args),
                 regexp = "Using or creating cached copy",
                 all = TRUE)
  expect_message(do.call(simInit, args),
                 regexp = "Using or creating cached copy|loading cached result",
                 all = TRUE)


  # make change to .inputObjects code -- should rerun .inputObjects
  xxx <- readLines(fileName)
  startOfFunctionLine <- grep(xxx, pattern = "^.inputObjects")
  editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
  editBelowLine <- editBelowLines[editBelowLines > startOfFunctionLine]
  xxx[editBelowLine + 1] <- newCode
  cat(xxx, file = fileName, sep = "\n")

  expect_message(do.call(simInit, args),
                 regexp = "Using or creating cached copy",
                 all = TRUE)
  expect_message(do.call(simInit, args),
                 regexp = "Using or creating cached copy|loading cached result",
                 all = TRUE)

  # make change elsewhere (i.e., not .inputObjects code) -- should NOT rerun .inputObjects
  xxx <- readLines(fileName)
  startOfFunctionLine <- grep(xxx, pattern = "^.inputObjects")
  editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
  editBelowLine <- editBelowLines[editBelowLines < startOfFunctionLine][1]
  xxx[editBelowLine + 1] <- newCode
  cat(xxx, file = fileName, sep = "\n")

  expect_message(do.call(simInit, args),
                 regexp = "Using or creating cached copy|loading cached result",
                 all = TRUE)


  # In some other location, test during spades call
  newModule(modName, path = tmpdir)
  try(clearCache(x = tmpCache), silent = TRUE)
  args$params <- list(test = list(.useCache = c(".inputObjects", "init")))
  bbb <- do.call(simInit, args)
  expect_silent(spades(bbb))
  expect_output(spades(bbb),
                 regexp = "Using cached copy of init",
                 all = TRUE)

  # make a change in Init function
  xxx <- readLines(fileName)
  startOfFunctionLine <- grep(xxx, pattern = "^Init")
  editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
  editBelowLine <- editBelowLines[editBelowLines > startOfFunctionLine][1]
  xxx[editBelowLine + 1] <- newCode
  cat(xxx, file = fileName, sep = "\n")

  bbb <- do.call(simInit, args)
  expect_true(any(grepl(format(bbb$test$Init), pattern = newCode)))

  # should NOT use Cached copy, so no message
  expect_silent(spades(bbb))
  expect_output(spades(bbb),
                regexp = "Using cached copy of init",
                all = TRUE)


})
