test_that("test event-level cache & memory leaks", {
  skip_on_cran()
  skip_if_not_installed("NLMR")

  testInitOut <- testInit(smcc = FALSE,
                          opts = list("reproducible.useMemoise" = FALSE))
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
    testOnExit(testInitOut)
  }, add = TRUE)

  modPath <- system.file("sampleModules", package = "SpaDES.core")

  mods <- c("caribouMovement", "randomLandscapes", "fireSpread")
  pkgs <- reqdPkgs(module = mods, modulePath = modPath)
  expect_true(length(pkgs) == 3)
  expect_true(all(names(pkgs) == mods))
  expect_true(all(c("raster", "sp", "SpaDES.tools", "RColorBrewer") %in% unlist(pkgs)))

  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 3.0, timeunit = "year"),
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

  set.seed(1123)
  # ._robustDigest_2 <<- ._addChangedAttr_5  <<- ._addTagsToOutput_2 <<- ._Cache_11 <<- ._Cache_13 <<- 1
  #._addTagsToOutput_2 <<- 1
  expect_true(!"loaded cached copy of init event in randomLandscapes module" %in%
                capture_messages({
                  sims <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = FALSE)
                }))
  #sims <- spades(Copy(mySim), notOlderThan = Sys.time()) ## TODO: fix this test
  landscapeMaps1 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap1 <- sims$landscape$Fires
  #._doEvent_3 <<- ._prepareOutput_5 <<- 1
  mess1 <- capture_messages({
    sims <- spades(Copy(mySim), debug = FALSE)
  })
  expect_true(any(grepl(pattern = "loaded cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))

  # Test for memory leak
  # Noting that there was a bug in `objSize` in reproducible that would
  #   get this part wrong
  # Take a function from the package -- shouldn't trigger memory leak stuff
  sims$crazyFunction2 <- SpaDES.core:::bindrows
  end(sims) <- end(sims) + 0.1

  mess <- capture.output({
    warnsFunction <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFunction) == 0)

  sims$crazyFunction3 <- sims$.mods$caribouMovement$Move
  end(sims) <- end(sims) + 0.1
  # simsOut <- spades(sims, debug = FALSE)

  mess <- capture.output({
    warnsFunction <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFunction) == 0)

  # Take a leaky function -- should trigger memory leak stuff
  fn <- function() { rnorm(1)}
  sims$crazyFunction <- fn
  end(sims) <- end(sims) + 0.1

  # simsOut <- spades(sims, debug = FALSE)
  mess <- capture.output({
    warnsFunction <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFunction) > 0)
  expect_true(grepl("function", warnsFunction))
  expect_true(grepl("crazyFunction", warnsFunction))
  expect_true(!grepl("crazyFormula", warnsFunction))
  expect_true(!grepl("formula", warnsFunction))

  sims$crazyFormula <- formula(hi ~ test)
  end(sims) <- end(sims) + 0.1
  mess <- capture.output({
    warnsFormula <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFormula) > 0)
  expect_true(grepl("formula", warnsFormula))
  expect_true(grepl("crazyFormula", warnsFormula))
  expect_true(!grepl("crazyFunction", warnsFormula))
  expect_true(!grepl("function", warnsFormula))

  sims$.mods$caribouMovement$.objects$crazyFunction <- function() { rnorm(1)}
  end(sims) <- end(sims) + 0.1
  mess <- capture.output({
    warnsFunction <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFunction) > 0)
  expect_true(grepl("function", warnsFunction))
  expect_true(grepl("crazyFunction", warnsFunction))
  expect_true(!grepl("crazyFormula", warnsFunction))
  expect_true(grepl("mod", warnsFunction))
  expect_true(!grepl("formula", warnsFunction))

  sims$.mods$caribouMovement$.objects$crazyFormula <-  formula(hi ~ test)
  end(sims) <- end(sims) + 0.1
  mess <- capture.output({
    warnsFormula <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  expect_true(length(warnsFormula) > 0)
  expect_true(grepl("formula", warnsFormula))
  expect_true(grepl("mod", warnsFormula))
  expect_true(grepl("crazyFormula", warnsFormula))
  expect_true(!grepl("crazyFunction", warnsFormula))
  expect_true(!grepl("function", warnsFormula))
})

test_that("test module-level cache", {
  skip_if_not_installed("NLMR")
  testInitOut <- testInit("raster", smcc = FALSE, debug = FALSE, ask = FALSE,
                          opts = list("reproducible.useMemoise" = FALSE))

  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
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
  expect_true(!("loaded cached copy of init event in randomLandscapes module" %in%
                  capture_messages({
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
  mess1 <- capture_messages({
    sims <- spades(Copy(mySim), debug = FALSE)
  })
  dev.off()

  if (!identical(Sys.info()[["sysname"]], "Windows") || interactive()) ## TODO: TEMPORARY to avoid random CRAN fail
    expect_true(file.info(tmpfile1)$size < 10000)

  unlink(tmpfile1)

  expect_true(any(grepl(pattern = "loaded cached copy of randomLandscapes module", mess1)))
  landscapeMaps2 <- raster::dropLayer(sims$landscape, "Fires")
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1, fireMap2))))
})

test_that("test .prepareOutput", {
  skip_if_not_installed("SpaDES.tools")

  testInitOut <- testInit("raster", smcc = FALSE)
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
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
  if (requireNamespace("ggplot2")) {
    testInitOut <- testInit(c("igraph", "raster"), smcc = TRUE,
                            opts = list(spades.recoveryMode = FALSE,
                                        "reproducible.useMemoise" = FALSE))
    opts <- options("reproducible.cachePath" = tmpdir)
    on.exit({
      options(opts)
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
    msgGrep <- paste("Running .input", "module code", "so not checking minimum package", "ggplot2",
                     "Setting", "Paths", "using dataPath", "Using setDTthreads",
                     "There is no similar item in the cachePath", sep = "|")
    expect_true(all(grepl(msgGrep, mess1)))

    msgGrep <- "Running .input|loaded cached copy|module code|Setting|Paths"
    #a <- capture.output(
    expect_message(do.call(simInit, args), regexp = msgGrep)
    #)

    # make change to .inputObjects code -- should rerun .inputObjects
    xxx <- readLines(fileName)
    startOfFunctionLine <- grep(xxx, pattern = "^.inputObjects")
    editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
    editBelowLine <- editBelowLines[editBelowLines > startOfFunctionLine]
    xxx[editBelowLine + 1] <- newCode
    cat(xxx, file = fileName, sep = "\n")

    msgGrep <- paste("Running .input", "module code", "so not checking minimum package",
                     "Setting", "Paths", "using dataPath", "Using setDTthreads",
                     "There is no similar item in the cachePath", sep = "|")
    mess1 <- capture_messages(do.call(simInit, args))
    expect_true(all(grepl(msgGrep, mess1)))

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
    expect_silent({
      aaMess <- capture_messages(spades(bbb, debug = FALSE))
    })
    options(opts)
    expect_message(spades(bbb), regexp = "loaded cached copy of init", all = FALSE)

    # make a change in Init function
    xxx <- readLines(fileName)
    startOfFunctionLine <- grep(xxx, pattern = "^Init")
    editBelowLines <- grep(xxx, pattern = "EDIT BELOW")
    editBelowLine <- editBelowLines[editBelowLines > startOfFunctionLine][1]
    xxx[editBelowLine + 1] <- newCode
    cat(xxx, file = fileName, sep = "\n")

    bbb <- do.call(simInit, args)
    expect_true(any(grepl(format(bbb@.xData$.mods$test$Init), pattern = newCode)))

    # should NOT use Cached copy, so no message
    opts <- options(spades.saveSimOnExit = FALSE)
    aaa <- capture_messages(spades(bbb, debug = FALSE))
    aa <- sum(grepl("loaded cached", aaa))
    expect_true(aa == 0) # seems to vary stochastically; either is OK
    options(opts)
    expect_message(spades(bbb), regexp = "loaded cached copy of init", all = FALSE)
  }
})

test_that("test .checkCacheRepo with function as reproducible.cachePath", {
  testInitOut <- testInit("igraph", smcc = TRUE)
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
    testOnExit(testInitOut)
  }, add = TRUE)
  #tmpCache <- file.path(tmpdir, "testCache") %>% checkPath(create = TRUE)

  awesomeCacheFun <- function() tmpCache ;
  options("reproducible.cachePath" = awesomeCacheFun)

  # uses .getOptions
  aa <- .checkCacheRepo(list(1), create = TRUE)
  expect_equal(normPath(aa), normPath(tmpCache))

  # accepts character string
  aa <- .checkCacheRepo(tmpCache, create = TRUE)
  expect_equal(normPath(aa), normPath(tmpCache))

  # uses .getPaths during simInit
  mySim <- simInit()
  aa <- .checkCacheRepo(list(mySim))
  expect_equal(normPath(aa), normPath(tmpCache))

  justAPath <- tmpCache ;
  options("reproducible.cachePath" = justAPath)

  # uses .getOptions
  aa <- .checkCacheRepo(list(1), create = TRUE)
  expect_equal(normPath(aa), normPath(tmpCache))

  # accepts character string
  aa <- .checkCacheRepo(tmpCache, create = TRUE)
  expect_equal(normPath(aa), normPath(tmpCache))

  # uses .getPaths during simInit
  mySim <- simInit()
  aa <- .checkCacheRepo(list(mySim))
  expect_equal(normPath(aa), normPath(tmpCache))

})

test_that("test objSize", {
  testInitOut <- testInit(smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- simInit(objects = list(d = 1:10, b = 2:20))
  os <- objSize(a)
  expect_true(length(os) == 1)
})

test_that("Cache sim objs via .Cache attr", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE, opts = list(spades.recoveryMode = FALSE,
                                                                   "reproducible.useMemoise" = FALSE))
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
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
      # sim$.mods$test$hi <- 1
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
  expect_true(mySim2$.mods$test$.objects$hello == 2)

  mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                   objects = list(co4 = 3, co3 = 2, co1 = 4), params =
                     list(test = list(.useCache = "init")))

  expect_true(mySim$co3 == 2) # will be changed by init
  expect_true(mySim$co1 == 4)# will be changed by init
  # expect_true(is.null(mySim$.mods$test$hi)) # will be changed by init
  mySim2 <- spades(Copy(mySim))
  # expect_true(mySim2$.mods$test$hi == 1) # was affected
  expect_true(mySim2$co1 == 1) # was affected
  expect_true(mySim2$co2 == 1)# was affected
  expect_true(mySim2$co3 == 1) # was affected
  expect_false(mySim2$co4 == 5) # wasn't affected by init event
  expect_true(mySim2$co4 == 3) # wasn't affect by init event
  expect_true(is.null(mySim2$co5)) # wan't affected, and isn't there

  # # Try again, hi should be there
  # expect_true(is.null(mySim$.mods$test$hi)) # is not in the
  # ._prepareOutput_5 <<- ._addChangedAttr_5  <<- ._addTagsToOutput_2 <<-  1
  mess1 <- capture_messages({
    mySim2 <- spades(Copy(mySim))
  })
  # expect_true(mySim2$.mods$test$hi == 1) # recovered in Cache
  # Test mod
  expect_true(mySim2$.mods$test$.objects$hello == 2) # recovered in Cache
  expect_true(any(grepl("loaded cached copy", mess1)))


  # Capture failed Cache, when a function is changed, that is not the .inputObjects,
  #   Cache should return the .inputObjects cached copy, but not the cached copy of the
  #   functions
  mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                   objects = list(co4 = 3, co3 = 2, co1 = 4), params =
                     list(test = list(.useCache = c(".inputObjects", "init"))))

  cat(append = TRUE, sep = "\n", fill = FALSE, file = fileNames[1],
  "newFun <- function(sim) return(invisible(sim))")
  mess10 <- capture_messages({
    mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                   objects = list(co4 = 3, co3 = 2, co1 = 4), params =
                     list(test = list(.useCache = c(".inputObjects", "init"))))
  })
  expect_true(sum(grepl("loaded cached copy of .inputObjects", mess10)) == 1)
  expect_true(exists("newFun", envir = mySim$.mods$test))



  # Test 2 in the "capture failed Cache"...
  # This should not recover the cache because it has a new .inputObjects function
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
      # sim$.mods$test$hi <- 1
      mod$hello <- 2
      ",
      xxx1[[1]][(lineWithInit + 1):lineWithDotInputObjects], "
      aaa <- 2
      ",
      xxx1[[1]][(lineWithDotInputObjects + 1):length(xxx1[[1]])],
      sep = "\n", fill = FALSE, file = fileNames[1])
  mess11 <- capture_messages({
    mySim <- simInit(paths = list(modulePath = tmpdir), modules = as.list(m[1]),
                     objects = list(co4 = 3, co3 = 2, co1 = 4), params =
                       list(test = list(.useCache = c(".inputObjects", "init"))))
  })
  expect_true(sum(grepl("loaded cached copy of .inputObjects", mess11)) == 0)
  expect_true(sum(grepl("Running .inputObjects", mess11)) == 1)
  expect_true(!exists("newFun", envir = mySim$.mods$test))
  expect_true(sum(grepl("aaa <- 2", format(mySim$.mods$test$.inputObjects))) == 1)
})

test_that("test showSimilar", {
  skip_if_not_installed("NLMR")

  testInitOut <- testInit(smcc = FALSE, "raster")
  opts <- options("reproducible.cachePath" = tmpdir)
  on.exit({
    options(opts)
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
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  out1 <- spades(Copy(mySim))#, showSimilar = TRUE)
  params(mySim)$randomLandscapes$nx <- 101
  mess <- capture_messages({
    out2 <- spades(Copy(mySim))#, showSimilar = TRUE)
  })
  mySim$a <- 1
  mess <- capture_messages({
    out1 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_true(any(grepl("Cache of.*differs", mess))) ## TODO: no longer true; confirm why.
  mySim$a <- 2
  mess <- capture_messages({
    out1 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_true(any(grepl("Cache of.*differs", mess))) ## TODO: no longer true; confirm why.
  mess <- capture_messages({
    out1 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_false(any(grepl("Cache of.*differs", mess)))
})
