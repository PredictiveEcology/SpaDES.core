test_that("test event-level cache & memory leaks", {
  skip_on_cran()

  testInit(sampleModReqdPkgs,
           opts = list(reproducible.useMemoise = FALSE,
                       spades.memoryUseInterval = NULL))
  opts <- options("reproducible.cachePath" = tmpdir)

  modPath <- getSampleModules(tmpdir)

  mods <- c("caribouMovement", "randomLandscapes", "fireSpread")
  pkgs <- reqdPkgs(module = mods, modulePath = modPath)
  expect_true(length(pkgs) == 3)
  expect_true(all(names(pkgs) == mods))
  expect_true(all(c("terra", "SpaDES.tools", "RColorBrewer") %in%
                    Require::extractPkgName(unlist(unname(pkgs)))))

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
  landscapeMaps1 <- sims$landscape[[-which(names(sims$landscape) %in% "Fires")]]
  fireMap1 <- sims$landscape$Fires
  #._doEvent_3 <<- ._prepareOutput_5 <<- 1
  bbbb <<- 1
  mess1 <- capture_messages({
    sims <- spades(Copy(mySim), debug = FALSE)
  })
  expect_true(any(grepl(pattern = "loaded cached copy of init event in randomLandscapes module", mess1)))
  landscapeMaps2 <- sims$landscape[[-which(names(sims$landscape) %in% "Fires")]]
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equivalent(landscapeMaps1, landscapeMaps2)
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1[], fireMap2[]))))

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
  opts <- options("spades.memoryLeakAllowed" = 150)
  on.exit(opts, add = TRUE)

  mess <- capture.output({
    warnsFunction <- capture_warnings({
      simsOut <- spades(sims, debug = FALSE)
    })
  })
  os1 <- as.numeric(gsub(".+object.size = ([0-9]+).+", "\\1", warnsFunction))
  os2 <- as.numeric(gsub(".+objSize = ([0-9]+).+", "\\1", warnsFunction))

  # On covr::package_coverage -- this shows a HUGE difference ... about 130x. I don't know exactly why,
  #   but I feel like it is due to capturing of each call, which is unique to covr
  #   So this should be skipped on covr
  if (!identical(Sys.getenv("USING_COVR"), "true"))
    expect_identical(length(grep("causing a memory leak", warnsFunction)), 0L)

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

  # os1 <- as.numeric(gsub(".+object.size = ([0-9]+).+", "\\1", warnsFunction))
  # os2 <- as.numeric(gsub(".+objSize = ([0-9]+).+", "\\1", warnsFunction))

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
  testInit(sampleModReqdPkgs, opts = list("reproducible.useMemoise" = FALSE))

  opts <- options("reproducible.cachePath" = tmpdir)
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
    paths = list(modulePath = getSampleModules(tmpdir),
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

  landscapeMaps1 <- sims$landscape[[-which(names(sims$landscape) %in% "Fires")]]
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
  landscapeMaps2 <- sims$landscape[[-which(names(sims$landscape) %in% "Fires")]]
  fireMap2 <- sims$landscape$Fires

  # Test that cached part comes up identical in both (all maps but Fires),
  #   but non-cached part are different (Fires should be different because stochastic)
  expect_equal(landscapeMaps1[], landscapeMaps2[]) ## TODO: #236
  expect_false(isTRUE(suppressWarnings(all.equal(fireMap1[], fireMap2[]))))
})

test_that("test .prepareOutput", {
  skip_on_cran() # too long

  testInit(sampleModReqdPkgs)
  opts <- options("reproducible.cachePath" = tmpdir)

  times <- list(start = 0.0, end = 1, timeunit = "year")
  mapPath <- getMapPath(tmpdir)

  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[-3],
    stringsAsFactors = FALSE
  )
  layers1 <- unname(sapply(filelist$files, rasterToMemory))
  newFns <- file.path(tmpdir, basename(filelist$files))
  layers <- linkOrCopy(filelist$files, newFns)
  landscape <- terra::rast(unlist(newFns))

  mySim <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA)
    ),
    modules = list("fireSpread", "caribouMovement"),
    paths = list(modulePath = getSampleModules(tmpdir),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    objects = c("landscape")
  )

  # simCached3 <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time(), debug = FALSE) # not sure why this causes caching to occur
  simCached1 <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time(), debug = FALSE) # not sure why
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

  # The Filebacking changed during `.wrap`
  simCached1$landscape[] <- simCached1$landscape[]
  simCached2$landscape[] <- simCached2$landscape[]
  simCached1$habitatQuality[] <- simCached1$habitatQuality[]
  simCached2$habitatQuality[] <- simCached2$habitatQuality[]
  expect_equivalent(simCached1$habitatQuality, simCached2$habitatQuality)
})

test_that("test .robustDigest for simLists", {
  testInit(c("terra", "ggplot2"), smcc = TRUE,
                          opts = list(spades.recoveryMode = FALSE,
                                      reproducible.verbose = 1,
                                      "reproducible.useMemoise" = FALSE,
                                      reproducible.showSimilar = TRUE))
  # opts <- options("reproducible.cachePath" = tmpdir)

  modName <- "test"
  newModule(modName, path = tmpdir, open = FALSE)
  fileName <- file.path(modName, paste0(modName,".R"))
  newCode <- "\"hi\"" # this will be added below in 2 different spots

  args <- list(modules = list("test"),
               paths = list(modulePath = tmpdir, cachePath = tmpCache),
               params = list(test = list(.useCache = ".inputObjects")))

  try(clearCache(x = tmpCache, ask = FALSE), silent = TRUE)
  mess1 <- capture_messages(do.call(simInit, args))
  msgGrep11 <- paste("Running .input", "module code", "so not checking minimum package", "ggplot2",
                   "Setting", "Paths", "using dataPath", "Using setDTthreads",
                   "with user supplied tags",
                   "There is no similar item in the cachePath", "Saving", "Done", "Elpsed time for", sep = "|")
  expect_true(all(grepl(msgGrep11, mess1)))

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

  mess1 <- capture_messages(do.call(simInit, args))
  expect_true(all(grepl(msgGrep11, mess1)))

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

})

test_that("test .checkCacheRepo with function as reproducible.cachePath", {
  testInit(smcc = TRUE)

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
  testInit(smcc = FALSE)

  a <- simInit(objects = list(d = 1:10, b = 2:20))
  os <- objSize(a)
  expect_true(length(os) == 1)
})

test_that("Cache sim objs via .Cache attr", {
  testInit("ggplot2",
                          smcc = FALSE, debug = FALSE,
                          opts = list(spades.recoveryMode = FALSE,
                                      "reproducible.useMemoise" = FALSE))
  withr::local_options(list(reproducible.cachePath = tmpdir))

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

  try(clearCache(ask = FALSE), silent = TRUE)
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
  # expect_true(is.null(mySim$.mods$test$hi)) # hi was removed from module
  mySim2 <- spades(Copy(mySim))
  # expect_true(mySim2$.mods$test$hi == 1) # hi was removed from module
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

  testInit(sampleModReqdPkgs, verbose = TRUE)
  opts <- options("reproducible.cachePath" = tmpdir)

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
    paths = list(modulePath = getSampleModules(tmpdir),
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
    out3 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_false(any(grepl("Cache of.*differs", mess))) ## Now it is function-specific -- no previous spades call
  mySim$a <- 2
  mess <- capture_messages({
    out4 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_true(any(grepl("Cache of.*differs", mess)))
  mess <- capture_messages({
    out5 <- Cache(spades, Copy(mySim), showSimilar = TRUE)
  })
  expect_false(any(grepl("Cache of.*differs", mess)))
})


test_that("test multipart cache file", {

  testInit(sampleModReqdPkgs, verbose = TRUE)
  opts <- options("reproducible.cachePath" = tmpdir)

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
    paths = list(modulePath = getSampleModules(tmpdir),
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
  )

  out1 <- Cache(spades(Copy(mySim)))
  end(out1) <- 2
  out2 <- Cache(spades(Copy(out1)))

})

test_that("multifile cache saving", {
  skip_on_cran()
  testInit("terra",
           tmpFileExt = c(".tif", ".tif"),
           opts = list(reproducible.useMemoise = FALSE)
  )

  nOT <- Sys.time()

  randomPolyToDisk2 <- function(tmpfiles) {
    r <- terra::rast(ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    r2 <- terra::rast(ext(0, 10, 0, 10), vals = sample(1:30, size = 100, replace = TRUE))
    terra::writeRaster(r, tmpfiles[1], overwrite = TRUE)
    terra::writeRaster(r, tmpfiles[2], overwrite = TRUE)
    r <- c(terra::rast(tmpfiles[1]), terra::rast(tmpfiles[2]))
    r
  }
  s <- simInit()
  s$ras <- randomPolyToDisk2(tmpfile)
  s2 <- Cache(spades(s))
  expect_true(identical(Filenames(s2), Filenames(s)))
})
