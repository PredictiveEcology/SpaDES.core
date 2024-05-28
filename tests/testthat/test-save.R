test_that("saving files (and memoryUse)", {
  skip_on_cran()
  # skip_on_os("windows") ## TODO: memoryUse() hanging on windows
  skip_on_covr() ## issue with memoryUseSetup

  testInit(smcc = FALSE, opts = list("spades.memoryUseInterval" = 0.1),
           c(sampleModReqdPkgs, "future", "future.callr"))

  origPlan <- future::plan()
  if (is(origPlan, "sequential")) {
    pl <- suppressWarnings(future::plan("multisession", workers = 2)) ## suppressed for checks in Rstudio
  }

  on.exit({
    future::plan(origPlan)
  }, add = TRUE)

  times <- list(start = 0, end = 6, "month")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(
      .plotInitialTime = NA, torus = TRUE, .saveObjects = "caribou",
      .saveInitialTime = 1, .saveInterval = 1
    ),
    randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
  )

  outputs <- data.frame(
    expand.grid(objectName = c("caribou", "landscape"),
                saveTime = 1:1,
                stringsAsFactors = FALSE)
  )

  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = getSampleModules(tmpdir),
    outputPath = tmpdir
  )

  mySim <- simInit(times = times, params = parameters, modules = modules,
                   paths = paths, outputs = outputs)

  mess <- capture_messages({
    mySim <- spades(mySim)
  })

  cc <- ongoingMemoryThisPid(0.2, interval = 0.1)
  expect_true(file.exists(cc))
  ff <- fread(cc)
  expect_true(NROW(ff) > 0)

  options("spades.memoryUseInterval" = 0)
  outputFile <- mySim$.memoryUse$filename
  expect_false(file.exists(outputFile))
  obj <- mySim$.memoryUse$obj
  expect_true(NROW(obj) > 0)
  aa <- memoryUse(mySim)
  expect_true(NROW(aa) > 0)

  a <- memoryUseThisSession()
  expect_true(is.numeric(a))

  # test spades-level mechanism
  expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
  expect_false(file.exists(file.path(tmpdir, "landscape_month2.rds")))

  # test module-level mechanism
  expect_true(file.exists(file.path(tmpdir, "caribou_month3.rds")))
  expect_true(file.exists(file.path(tmpdir, "caribou_month5.rds")))

  outputs <- data.frame(
    expand.grid(objectName = c("caribou", "landscape")),
    stringsAsFactors = FALSE
  )
  times <- list(start = 0, end = 1, "month")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
  )
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   paths = paths, outputs = outputs)

  mySim <- spades(mySim)

  # test that if no save times are stated, then it is at end time
  expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
  expect_true(file.exists(file.path(tmpdir, "landscape_month1.rds")))
  rm(mySim)

  # test when filename has a dot
  tmpdir <- paste0(tmpdir, ".sdfd.lkjlll")

  outputs <- data.frame(
    expand.grid(objectName = c("caribou", "landscape")),
    stringsAsFactors = FALSE
  )
  paths$outputPath <- tmpdir

  times <- list(start = 0, end = 1, "month")
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
  )
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   paths = paths, outputs = outputs)

  mySim <- spades(mySim)

  # test that if no save times are stated, then it is at end time
  expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
  expect_true(file.exists(file.path(tmpdir, "landscape_month1.rds")))
  rm(mySim)
})

test_that("saving csv files works correctly", {
  testInit(smcc = FALSE)

   tempObj <- 1:10
   tempObj2 <- paste("val", 1:10)
   df1 <- data.frame(col1 = tempObj, col2 = tempObj2)
   sim <- simInit(objects = c("tempObj", "tempObj2", "df1"),
                  paths = list(outputPath = tmpdir))
   outs <- data.frame(
        objectName = c(rep("tempObj", 2), rep("tempObj2", 3), "df1"),
        saveTime = c(c(1, 4), c(2, 6, 7), end(sim)),
        eventPriority = c(2:7),
        fun = c(rep("saveRDS", 5), "write.csv"),
        package = c(rep("base", 5), "utils"),
        stringsAsFactors = FALSE)
   outputs(sim) <- outs
   # since write.csv has a default of adding a column, x, with rownames, must add additional
   #   argument for 6th row in data.frame (corresponding to the write.csv function)
   sim2 <- Copy(sim)
   outputArgs(sim2)[[6]] <- list(row.names = FALSE)
   sim2 <- spades(sim2)
   outputs(sim2)
   completes <- completed(sim2)
   expect_true("eventPriority" %in% colnames(completes))
   expect_true(all(completes[eventType != "init"]$eventPriority == outs[order(outs$saveTime),]$eventPriority))

   # read one back in just to test it all worked as planned
   newObj <- read.csv(dir(tmpdir, pattern = "year10.csv", full.name = TRUE))
   expect_true(identical(df1, newObj))

   # Confirm that arguments are actually being passed in by changing row.names to TRUE
   sim2 <- Copy(sim)
   outputArgs(sim2)[[6]] <- list(row.names = TRUE)
   sim2 <- spades(sim2)
   outputs(sim2)
   # read one back in just to test it all worked as planned
   newObj <- read.csv(dir(tmpdir, pattern = "year10.csv", full.name = TRUE))
   expect_false(identical(df1, newObj))
})

test_that("saveSimList works correctly", {
  skip_if_not_installed("archive")

  testInit(sampleModReqdPkgs,
           tmpFileExt = c("grd", "qs", "qs", "tif", "", "", "grd", "rds"),
           opts = list(reproducible.verbose = 0))
  unlink(tmpfile[5])
  unlink(tmpfile[6])
  mapPath <- getMapPath(tmpdir)
  modules <- getSampleModules(tmpdir)

  modulePath <- checkPath(file.path(tmpdir, "modules"), create = TRUE)
  inputPath <- checkPath(file.path(tmpdir, "inputs"), create = TRUE)
  outputPath <- checkPath(file.path(tmpdir, "outputs"), create = TRUE)
  cachePath <- checkPath(file.path(tmpdir, "cache"), create = TRUE)

  linkOrCopy(dir(mapPath, full.names = TRUE), file.path(modulePath, dir(mapPath)))
  linkOrCopy(dir(modules, recursive = TRUE, full.names = TRUE),
             file.path(modulePath, dir(modules, recursive = TRUE)))

  times <- list(start = 0, end = 1)
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA_integer_),
    randomLandscapes = list(.plotInitialTime = NA_integer_, nx = 20, ny = 20)
  )

  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    cachePath = cachePath,
    modulePath = modulePath,
    inputPath = inputPath,
    outputPath = outputPath
  )

  mySim <- simInit(
    times = times,
    params = parameters,
    modules = modules,
    paths = paths,
    outputs = data.frame(objectName = "landscape", saveTime = times$end)
  ) |>
    spades()
  mySim$landscape[] <- round(mySim$landscape[], 3) # after saving, these come back different, unless rounded
  mySim$landscape <- writeRaster(mySim$landscape, filename = tmpfile[1], overwrite = TRUE, datatype = "FLT4S")
  mySim$habitatQuality <- writeRaster(mySim$landscape, filename = tmpfile[7], overwrite = TRUE)

  ## test using qs
  # keeps file-backings
  saveSimList(mySim, filename = tmpfile[2])

  sim <- loadSimList(file = tmpfile[2], projectPath = tmpCache)
  expect_true(all(basename(Filenames(sim)) %in% basename(Filenames(mySim))))

  expect_true(is.numeric(sim$landscape$DEM[]))

  ## test using rds
  saveSimList(mySim, filename = tmpfile[8])
  fnsOrig <- Filenames(mySim)
  pthsOrig <- paths(mySim)
  rm(mySim)
  unlink(fnsOrig)
  sim <- loadSimList(file = tmpfile[8], projectPath = tmpCache, paths = pthsOrig)
  expect_true(is.numeric(sim$landscape$DEM[]))
  expect_true(all(Filenames(sim) %in% fnsOrig))
  sim$landscape[] <- sim$landscape[]
  sim$habitatQuality[] <- sim$habitatQuality[]

  ## Now put it back to disk for subsequent test
  # sim$landscape[] <- sim$landscape[]
  # sim$habitatQuality[] <- sim$habitatQuality[]
  unlink(c(tmpfile[1], paste0(tools::file_path_sans_ext(tmpfile[1]), ".gri"))) ## needed because of hardlink shenanigans
  unlink(c(tmpfile[7], paste0(tools::file_path_sans_ext(tmpfile[7]), ".gri"))) ## needed because of hardlink shenanigans
  sim$landscape <- writeRaster(sim$landscape, filename = tmpfile[1])
  sim$habitatQuality <- writeRaster(sim$habitatQuality, filename = tmpfile[7])
  ## grd format doesn't get minmax right especially with terra -- can't fix it with terra

  ## The terra pointers with grd files make comparisons wrong
  # mySim$habitatQuality <- rasterToMemory(mySim$habitatQuality)
  # mySim$landscape <- rasterToMemory(mySim$landscape)
  # sim$habitatQuality <- rasterToMemory(sim$habitatQuality)
  # sim$landscape <- rasterToMemory(sim$landscape)
  #
  # expect_true(all.equal(mySim, sim, check.environment = FALSE))

  ## Now try to keep filename intact
  # mySim$landscape <- writeRaster(mySim$landscape, filename = tmpfile[1], overwrite = TRUE)
  # mySim$habitatQuality <- writeRaster(mySim$landscape, filename = tmpfile[7], overwrite = TRUE)

  ## loses the raster landscape
  saveSimList(sim, filename = tmpfile[3])
  simLoaded <- loadSimList(file = tmpfile[3])
  expect_equivalent(
    gsub("\\_[[:digit:]]{1,2}$", "", checkPath(Filenames(simLoaded$landscape, allowMultiple = FALSE))),
    tmpfile[1])
  expect_true(bindingIsActive("mod", simLoaded@.xData$.mods$caribouMovement)) ## TODO: fails?

  mySim <- simLoaded
  # Now keep as file-backed, but change name
  # aaaa <<- 1
  saveSimList(mySim, filename = tmpfile[3])

  sim <- loadSimList(file = tmpfile[3])
  fns <- Filenames(sim, allowMultiple = FALSE)
  # different filenames
  expect_false(identical(gsub("\\_.", "", checkPath(fns)), tmpfile[c(7, 1)]))
  #  but same basenames
  expect_true(identical(basename(gsub("\\_.", "", checkPath(fns))), basename(tmpfile[c(7, 1)])))

  # delete all grd/gri files
  file.remove(dir(dirname(tmpfile[c(1)]), pattern = ".gr.$", full.names = TRUE))
  # rm(mySim)

  assign("a", 1, envir = mySim@.xData$.mods$caribouMovement$.objects)
  assign("a", 2, envir = sim@.xData$.mods$caribouMovement$.objects)

  expect_true(bindingIsActive("mod", sim@.xData$.mods$caribouMovement)) ## TODO: fails?
  # test file-backed raster is gone
  expect_error(mySim$landscape$DEM[])
})

test_that("saveSimList with file backed objs", {
  skip_if_not_installed("archive")

  testInit(sampleModReqdPkgs,
           tmpFileExt = c("zip", "grd", "tif", "tif", "tif", "grd", "qs"))
  mapPath <- getMapPath(tmpdir)
  modules <- getSampleModules(tmpdir)

  modulePath <- checkPath(file.path(tmpdir, "modules"), create = TRUE)
  inputPath <- checkPath(file.path(tmpdir, "inputs"), create = TRUE)
  outputPath <- file.path(tmpdir, "outputs") ## don't create; will symlink on Unix-alike

  tmpfile[-1] <- file.path(outputPath, basename(tmpfile[-1]))

  if (identical(tolower(.Platform$OS.type), "windows")) {
    ## don't use symlink on Windows
    checkPath(outputPath, create = TRUE)
    expect_true(dir.exists(outputPath))
  } else {
    ## use symlink if not Windows
    linkedDir <- checkPath(file.path(tempdir(), "SpaDES.core_tests", .rndstr(1)), create = TRUE)
    file.symlink(linkedDir, outputPath)
    expect_true(dir.exists(outputPath))
    expect_identical(Sys.readlink(outputPath), linkedDir)
  }

  expect_true(
    all(linkOrCopy(dir(mapPath, full.names = TRUE), file.path(modulePath, dir(mapPath))))
  )
  expect_true(
    all(linkOrCopy(dir(modules, recursive = TRUE, full.names = TRUE),
                   file.path(modulePath, dir(modules, recursive = TRUE))))
  )

  times <- list(start = 0, end = 5)
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA_integer_),
    randomLandscapes = list(.plotInitialTime = NA_integer_, nx = 20, ny = 20)
  )

  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = modulePath,
    inputPath = inputPath,
    outputPath = outputPath
  )

  mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths,
                   outputs = data.frame(objectName = "landscape",
                                        saveTime = seq(times$start, times$end)))
  mySim <- spades(mySim, debug = FALSE)

  Map(nam = names(mySim$landscape), i = seq(nlyr(mySim$landscape)), function(nam, i) {
    coltab(mySim$landscape[[nam]]) <- NULL ## can't use colour table with FLT4S (#261)
    mySim$landscape[[nam]] <- writeRaster(mySim$landscape[[nam]], tmpfile[i + 1], datatype = "FLT4S")
  })

  ## with file backed
  if (identical(tolower(.Platform$OS.type), "windows")) {
    ## don't use symlink on Windows
    saveSimList(mySim, filename = tmpfile[1], verbose = FALSE)
  } else {
    saveSimList(mySim, filename = tmpfile[1], verbose = FALSE, symlinks = list(outputPath = "outputs"))
  }

  newTmpdir <- normPath(withr::local_tempdir(file.path("newTmp")))
  withr::local_dir(newTmpdir)
  mySimOut <- loadSimList(tmpfile[1])

  ## all files except .grd.aux.xml files are copied
  expect_true(all(grep("[.]grd[.]aux[.]xml$", dir(outputPath), invert = TRUE, value = TRUE) %in%
                    dir(outputPath(mySimOut))))
  expect_true(all(grep("[.]grd[.]aux[.]xml$", dir(outputPath(mySim)), invert = TRUE, value = TRUE) %in%
                    dir(outputPath(mySimOut))))

  # convert to matrix for all.equal --> the two objects have different paths now
  mySim$landscape <- mySim$landscape[]
  mySimOut$landscape <- mySimOut$landscape[]
  expect_equivalent(mySim, mySimOut)

  if (exists("linkedDir"))
    unlink(linkedDir, recursive = TRUE)
})

test_that("restart does not work correctly", {
  skip("restartR not possible in automated tests")
  skip_if_not_installed("NLMR")

  # Must be run manually
  setwd("~/GitHub/SpaDES.core")
  #devtools::install(update.dependencies = FALSE, dependencies = FALSE) # need to install latest so that at restart it has everything
  testInit(sampleModReqdPkgs, tmpFileExt = c("grd", "Rdata", "Rdata"),
           opts = list("spades.restartRInterval" = 0, "spades.restartR.clearFiles" = FALSE))

  tmpdir <- "~"
  testNum = 3
  if (testNum == 1) {
    interval = 1
    mapPath <- getMapPath(tmpdir)

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = getSampleModules(tmpdir),
      inputPath = mapPath,
      outputPath = tmpdir
    )

    options("spades.restartRInterval" = interval)
    times <- list(start = 0, end = 3)
    mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths,
                     outputs = data.frame(objectName = "landscape", saveTime = times$end))
    mySim <- spades(mySim, debug = 1)
  } else if (testNum == 2) {
    options("spades.restartRInterval" = 10)
    mySim <- mySim <- simInit(
      times = list(start = 0.0, end = 30.0, timeunit = "year"),
      params = list(
        .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
        # Turn off interactive plotting
        fireSpread = list(.plotInitialTime = NA),
        caribouMovement = list(.plotInitialTime = NA),
        randomLandscapes = list(.plotInitialTime = NA)
      ),
      modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
      paths = list(modulePath = getSampleModules(tmpdir),
                   outputPath = file.path("~", "outputs"),
                   cachePath = tmpdir),
      # Save final state of landscape and caribou
      outputs = data.frame(expand.grid(objectName = c("landscape", "caribou"),
                                       stringsAsFactors = FALSE,
                                       saveTime = 1:30))
    )
    mySim <- spades(mySim, debug = 1)
  } else if (testNum == 3) {
    interval = 1
    mapPath <- getMapPath(tmpdir)

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = getSampleModules(tmpdir),
      inputPath = mapPath,
      outputPath = tmpdir
    )

    options("spades.restartRInterval" = interval)
    times <- list(start = 0, end = 3)
    mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths,
                     outputs = data.frame(objectName = "landscape", saveTime = times$end))
    mySim$tesRas <- raster(extent(0,10,0,10), vals = 1, res = 1)
    tmpFilename <- "~/tmpRas.tif"
    mySim$tesRas <- writeRaster(mySim$tesRas, tmpFilename, overwrite = TRUE)
    mySim <- spades(mySim, debug = 1)
    sim$tesRas + 1
    file.exists(tmpFilename)
  }

  unlink(unique(dirname(outputs(sim)$file)), recursive = TRUE, force = TRUE)
  options("spades.restartRInterval" = 0)
})

test_that("restart with logging", {
  skip("restartR with logging not possible in automated tests")

  # Must be run manually
  setwd("~/GitHub/SpaDES.core")
  library(SpaDES.core)
  #devtools::install(update.dependencies = FALSE, dependencies = FALSE) # need to install latest so that at restart it has everything
  testInit(sampleModReqdPkgs, opts = list("spades.restartRInterval" = 0,
                                          "spades.restartR.clearFiles" = FALSE))

  tmpdir <- "~"
  testNum = 3
    interval = 1
    mapPath <- getMapPath(tmpdir)

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = getSampleModules(tmpdir),
      inputPath = mapPath,
      outputPath = tmpdir
    )

    options("spades.restartRInterval" = interval)
    times <- list(start = 0, end = 3)
    mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths,
                     outputs = data.frame(objectName = "landscape", saveTime = times$end))
    mySim$tesRas <- raster(extent(0,10,0,10), vals = 1, res = 1)
    tmpFilename <- "~/tmpRas.tif"
    mySim$tesRas <- writeRaster(mySim$tesRas, tmpFilename, overwrite = TRUE)
    mySim <- spades(mySim, debug = list("file" = list("file" = "log.txt")))
    sim$tesRas + 1
    file.exists(tmpFilename)

  unlink(unique(dirname(outputs(sim)$file)), recursive = TRUE, force = TRUE)
  options("spades.restartRInterval" = 0)
})

test_that("registerOutputs", {
  testInit()
  sim <- simInit()
  # This would normally be a save call, e.g., `writeRaster`
  tf <- reproducible::tempfile2(fileext = ".tif")
  tf2 <- reproducible::tempfile2(fileext = ".tif")
  sim <- registerOutputs(sim, filename = c(tf, tf2))
  odf <- outputs(sim)
  expect_true(all(odf$file %in% c(tf, tf2)))


  newModule("test", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "test", "test.R"),'
      defineModule(sim, list(
      name = "test",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
      spatialExtent = terra::ext(rep(0, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "year",
      citation = list("citation.bib"),
      documentation = list("README.md", "test.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
        defineParameter(".useCache", "character", ".inputObjects", NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("age", "numeric", ""),
        expectsInput("ageMap", "numeric", ""),
        expectsInput("worked", "numeric", ""),
        expectsInput("age2", "numeric", "") # need a dummy one that isn not supplied in simInit below
      ),
      outputObjects = bindrows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {

      tf <- tempfile2(.rndstr(), fileext = ".rds")
      tf2 <- tempfile2(.rndstr(), fileext = ".rds")
      tf3 <- tempfile2(.rndstr(), fileext = ".rds")

      sim <- saveRDS(sim$age, file = tf) |> registerOutputs(filename = tf)
      sim <- registerOutputs(filename = saveRDS(sim$age, file = tf2))
      sim <- saveRDS(sim$age, file = tf3) |> registerOutputs()


      #sim$dp <- dataPath(sim)
      sim <- scheduleEvent(sim, time(sim)+1, "test", "event1")
      },
      event1 = {
      tf4 <- tempfile2(.rndstr(), fileext = ".qs")
      sim <- qs::qsave(sim$age, file = tf4) |> registerOutputs()

      sim <- scheduleEvent(sim, time(sim)+1, "test", "event1")
      })
      return(invisible(sim))
      }

      .inputObjects <- function(sim) {
        if (suppliedElsewhere("ageMap", sim)) {
                sim$worked <- TRUE
        }
        sim
      }
      ', fill = TRUE)
  modules <- "test"
  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"),
                 modules = modules,
                 objects = list(age = 1, vegMap = 2, studyArea = 3),
                 paths = list(modulePath = tmpdir))
  sim <- spades(sim)
  expect_equal(NROW(outputs(sim)$file), 4)
  expect_true(all(file.exists(outputs(sim)$file)))
})
