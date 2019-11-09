test_that("test event-level cache", {
  testInitOut <- testInit(smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init", .showSimilar = TRUE)
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
                capture_output({
                  sims <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = FALSE)
                }))
  #sims <- spades(Copy(mySim), notOlderThan = Sys.time()) ## TODO: fix this test
  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires
  mess1 <- capture_output({
    sims <- spades(Copy(mySim), debug = FALSE)
  })
  expect_true(any(grepl(pattern = "Using cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))
})

test_that("test module-level cache", {
  testInitOut <- testInit("raster", smcc = FALSE, debug = FALSE, ask = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  tmpfile <- tempfile(fileext = ".pdf")
  tmpfile1 <- tempfile(fileext = ".pdf")
  expect_true(file.create(tmpfile))
  tmpfile <- normPath(tmpfile)

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
                  capture_output({
                    sims <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = FALSE)
                  })))
  dev.off()

  expect_true(file.info(tmpfile)$size > 20000)
  unlink(tmpfile, force = TRUE)

  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires

  # The cached version will be identical for both events (init and plot),
  # but will not actually complete the plot, because plotting isn't cacheable
  pdf(tmpfile1)
  mess1 <- capture_output({
    sims <- spades(Copy(mySim), debug = FALSE)
  })
  dev.off()

  if (!identical(Sys.info()[["sysname"]], "Windows") || interactive()) ## TODO: TEMPORARY to avoid random CRAN fail
    expect_true(file.info(tmpfile1)$size < 10000)

  unlink(tmpfile1)

  expect_true(any(grepl(pattern = "Using cached copy of randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))
})

test_that("test .prepareOutput", {
  testInitOut <- testInit("raster", smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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

  simCached1 <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time(), debug = FALSE)
  simCached2 <- spades(Copy(mySim), cache = TRUE, debug = FALSE)

  if (interactive()) {
    tmpDir <- "~/tmp"
    testFile <- file.path(tmpDir, "test-cache-out.txt")
    if (!dir.exists(tmpDir)) dir.create(tmpDir, recursive = TRUE)
    cat(file = testFile, names(params(mySim)$.progress), append = FALSE)
    cat(file = testFile, "\n##############################\n", append = TRUE)
    cat(file = testFile, names(params(simCached1)$.progress), append = TRUE)
    cat(file = testFile, "\n##############################\n", append = TRUE)
    cat(file = testFile, names(params(simCached2)$.progress), append = TRUE)
    cat(file = testFile, "\n##############################\n", append = TRUE)
    cat(file = testFile, all.equal(simCached1, simCached2), append = TRUE)
  }
  expect_true(all.equal(simCached1, simCached2))
})

test_that("test .robustDigest for simLists", {
  testInitOut <- testInit("igraph", smcc = TRUE, opts = list(spades.recoveryMode = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  modName <- "test"
  newModule(modName, path = tmpdir, open = FALSE)
  fileName <- file.path(modName, paste0(modName,".R"))
  newCode <- "\"hi\"" # this will be added below in 2 different spots

  args <- list(modules = list("test"),
               paths = list(modulePath = tmpdir, cachePath = tmpCache),
               params = list(test = list(.useCache = ".inputObjects")))

  try(clearCache(x = tmpCache, ask = FALSE), silent = TRUE)

  mess1 <- capture_messages(do.call(simInit, args))
  msgGrep <- "Running .input|module code|Setting|Paths|using dataPath|There is no similar item in the cacheRepo"
  expect_true(all(grepl(msgGrep, mess1)))

  msgGrep <- "Running .input|Using cached copy|module code|Setting|Paths"
  a <- capture.output(expect_message(do.call(simInit, args), regexp = msgGrep))

  # make change to .inputObjects code -- should rerun .inputObjects
  xxx <- readLines(fileName)
  startOfFunctionLine <- grep(xxx, pattern = "^.inputObjects")
  editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
  editBelowLine <- editBelowLines[editBelowLines > startOfFunctionLine]
  xxx[editBelowLine + 1] <- newCode
  cat(xxx, file = fileName, sep = "\n")

  msgGrep <- "Running .input|module code|Setting|Paths|using dataPath|There is no similar item in the cacheRepo"
  expect_message(do.call(simInit, args), regexp = msgGrep, all = TRUE)

  # make change elsewhere (i.e., not .inputObjects code) -- should NOT rerun .inputObjects
  xxx <- readLines(fileName)
  startOfFunctionLine <- grep(xxx, pattern = "^.inputObjects")
  editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
  editBelowLine <- editBelowLines[editBelowLines < startOfFunctionLine][1]
  xxx[editBelowLine + 1] <- newCode
  cat(xxx, file = fileName, sep = "\n")

  msgGrep <- "Running .input|loading cached result|module code"
  expect_message(do.call(simInit, args), regexp = msgGrep)

  # In some other location, test during spades call
  newModule(modName, path = tmpdir, open = FALSE)
  try(clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  args$params <- list(test = list(.useCache = c(".inputObjects", "init")))
  bbb <- do.call(simInit, args)
  opts <- options(spades.saveSimOnExit = FALSE)
  expect_silent(spades(bbb, debug = FALSE))
  options(opts)
  expect_output(spades(bbb), regexp = "Using cached copy of init", all = TRUE)

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
  opts <- options(spades.saveSimOnExit = FALSE)
  expect_silent(spades(bbb, debug = FALSE))
  options(opts)
  expect_output(spades(bbb), regexp = "Using cached copy of init", all = TRUE)
})

test_that("test .checkCacheRepo with function as reproducible.cachePath", {
  testInitOut <- testInit("igraph", smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  #tmpCache <- file.path(tmpdir, "testCache") %>% checkPath(create = TRUE)

  awesomeCacheFun <- function() tmpCache ;
  options("reproducible.cachePath" = awesomeCacheFun)

  # uses .getOptions
  aa <- .checkCacheRepo(list(1), create = TRUE)
  expect_equal(aa, tmpCache)

  # accepts character string
  aa <- .checkCacheRepo(tmpCache, create = TRUE)
  expect_equal(aa, tmpCache)

  # uses .getPaths during simInit
  mySim <- simInit()
  aa <- .checkCacheRepo(list(mySim))
  expect_equal(aa, tmpCache)

  justAPath <- tmpCache ;
  options("reproducible.cachePath" = justAPath)

  # uses .getOptions
  aa <- .checkCacheRepo(list(1), create = TRUE)
  expect_equal(aa, tmpCache)

  # accepts character string
  aa <- .checkCacheRepo(tmpCache, create = TRUE)
  expect_equal(aa, tmpCache)

  # uses .getPaths during simInit
  mySim <- simInit()
  aa <- .checkCacheRepo(list(mySim))
  expect_equal(aa, tmpCache)
})

test_that("test objSize", {
  testInitOut <- testInit(smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- simInit(objects = list(d = 1:10, b = 2:20))
  os <- objSize(a)
  expect_true(length(os) == 5) # 4 objects, the environment, the rest
})

test_that("Cache of sim objects via .Cache attr -- using preDigest and postDigest", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE, opts = list(spades.recoveryMode = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  Cache(rnorm, 1)

  m1 <- "test"
  m <- c(m1)
  newModule(m1, tmpdir, open = FALSE)
  fileNames <- dir(tmpdir, recursive = TRUE, pattern = "test.R$")
  xxx <- lapply(fileNames, readLines)
  set.seed(113)

  lineWithInit <- grep(xxx[[1]], pattern = "^Init")
  lineWithDotUseCache <- grep(xxx[[1]], pattern = "\\.useCache")
  lineWithInputObjects <- grep(xxx[[1]], pattern = " expectsInput")
  lineWithOutputObjects <- grep(xxx[[1]], pattern = " createsOutput")
  lineWithDotInputObjects <- grep(xxx[[1]], pattern = "\\.inputObjects")[1]

  xxx1 <- list()
  xxx1[[1]] <- xxx[[1]]

  cat(xxx1[[1]][1:(lineWithInputObjects - 1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei2', 'numeric', '', ''),
      expectsInput('ei3', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx1[[1]][(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co2', 'numeric', ''),
      createsOutput('co3', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx1[[1]][(lineWithOutputObjects + 1):lineWithInit], "
      sim$co1 <- 1
      sim$co2 <- 1
      sim$co3 <- 1
      sim$test$hi <- 1
      mod$hello <- 2
      ",
      xxx1[[1]][(lineWithInit + 1):lineWithDotInputObjects], "
      aaa <- 1
      ",
      xxx1[[1]][(lineWithDotInputObjects + 1):length(xxx1[[1]])],
      sep = "\n", fill = FALSE, file = fileNames[1])

  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                   params = list(test = list(.useCache = "init")))
  mySim$co4 <- 5
  mySim$co5 <- 6
  mySim2 <- spades(Copy(mySim))
  expect_true(mySim2$co1 == 1)
  expect_true(mySim2$co2 == 1)
  expect_true(mySim2$co3 == 1)
  expect_true(mySim2$co4 == 5)
  expect_true(mySim2$co5 == 6)

  # Test mod
  expect_true(mySim2$test$.objects$hello == 2)

  mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                   objects = list(co4 = 3, co3 = 2, co1 = 4), params =
                     list(test = list(.useCache = "init")))

  expect_true(mySim$co3 == 2) # will be changed by init
  expect_true(mySim$co1 == 4)# will be changed by init
  expect_true(is.null(mySim$test$hi)) # will be changed by init
  mySim2 <- spades(Copy(mySim))
  expect_true(mySim2$test$hi == 1) # was affected
  expect_true(mySim2$co1 == 1) # was affected
  expect_true(mySim2$co2 == 1)# was affected
  expect_true(mySim2$co3 == 1) # was affected
  expect_false(mySim2$co4 == 5) # wasn't affected by init event
  expect_true(mySim2$co4 == 3) # wasn't affect by init event
  expect_true(is.null(mySim2$co5)) # wan't affected, and isn't there

  # Try again, hi should be there
  expect_true(is.null(mySim$test$hi)) # is not in the
  mess1 <- capture_output({
    mySim2 <- spades(Copy(mySim))
  })
  expect_true(mySim2$test$hi == 1) # recovered in Cache
  # Test mod
  expect_true(mySim2$test$.objects$hello == 2) # recovered in Cache
  expect_true(grepl("Using cached copy", mess1))
})


test_that("test showSimilar", {
  testInitOut <- testInit(smcc = FALSE, "raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Example of changing parameter values
  params <- list(
    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
    # Turn off interactive plotting
    fireSpread = list(.plotInitialTime = NA),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, .useCache = "init", .showSimilar = TRUE)
  )

  mySim <- simInit(
    times = list(start = 0.0, end = 1.0, timeunit = "year"),
    param = params,
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  out1 <- spades(Copy(mySim))#, showSimilar = TRUE)
  params(mySim)$randomLandscapes$nx <- 101
  mess <- capture_messages(out2 <- spades(Copy(mySim)))#, showSimilar = TRUE)
  mySim$a <- 1
  out1 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  mySim$a <- 2
  out1 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
})
