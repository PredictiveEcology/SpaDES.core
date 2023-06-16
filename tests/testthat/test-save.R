# test_that("saving files (and memoryUse)", {
#   skip_on_cran()
# #   skip_on_os("windows") ## TODO: memoryUse() hanging on windows
#   skip_on_covr() ## issue with memoryUseSetup
#
#   testInitOut <- testInit(smcc = FALSE, opts = list("spades.memoryUseInterval" = 0.1),
#                           libraries = c("NLMR", "data.table", "future", "future.callr"))
#
#   origPlan <- future::plan()
#   if (is(origPlan, "sequential")) {
#     pl <- suppressWarnings(future::plan("multisession", workers = 2)) ## suppressed for checks in Rstudio
#   }
#
#   on.exit({
#     future::plan(origPlan)
#   }, add = TRUE)
#
#   times <- list(start = 0, end = 6, "month")
#   parameters <- list(
#     .globals = list(stackName = "landscape"),
#     caribouMovement = list(
#       .plotInitialTime = NA, torus = TRUE, .saveObjects = "caribou",
#       .saveInitialTime = 1, .saveInterval = 1
#     ),
#     randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
#   )
#
#   outputs <- data.frame(
#     expand.grid(objectName = c("caribou", "landscape"),
#                 saveTime = 1:1,
#                 stringsAsFactors = FALSE)
#   )
#
#   modules <- list("randomLandscapes", "caribouMovement")
#   paths <- list(
#     modulePath = system.file("sampleModules", package = "SpaDES.core"),
#     outputPath = tmpdir
#   )
#
#   mySim <- simInit(times = times, params = parameters, modules = modules,
#                    paths = paths, outputs = outputs)
#
#   mess <- capture_messages({
#     mySim <- spades(mySim)
#   })
#
#   cc <- ongoingMemoryThisPid(0.2, interval = 0.1)
#   expect_true(file.exists(cc))
#   ff <- fread(cc)
#   expect_true(NROW(ff) > 0)
#
#   options("spades.memoryUseInterval" = 0)
#   outputFile <- mySim$.memoryUse$filename
#   expect_false(file.exists(outputFile))
#   obj <- mySim$.memoryUse$obj
#   expect_true(NROW(obj) > 0)
#   aa <- memoryUse(mySim)
#   expect_true(NROW(aa) > 0)
#
#   a <- memoryUseThisSession()
#   expect_true(is.numeric(a))
#
#   # test spades-level mechanism
#   expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
#   expect_false(file.exists(file.path(tmpdir, "landscape_month2.rds")))
#
#   # test module-level mechanism
#   expect_true(file.exists(file.path(tmpdir, "caribou_month3.rds")))
#   expect_true(file.exists(file.path(tmpdir, "caribou_month5.rds")))
#
#   outputs <- data.frame(
#     expand.grid(objectName = c("caribou", "landscape")),
#     stringsAsFactors = FALSE
#   )
#   times <- list(start = 0, end = 1, "month")
#   parameters <- list(
#     .globals = list(stackName = "landscape"),
#     caribouMovement = list(.plotInitialTime = NA),
#     randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
#   )
#   mySim <- simInit(times = times, params = parameters, modules = modules,
#                    paths = paths, outputs = outputs)
#
#   mySim <- spades(mySim)
#
#   # test that if no save times are stated, then it is at end time
#   expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
#   expect_true(file.exists(file.path(tmpdir, "landscape_month1.rds")))
#   rm(mySim)
#
#   # test when filename has a dot
#   tmpdir <- paste0(tmpdir, ".sdfd.lkjlll")
#
#   outputs <- data.frame(
#     expand.grid(objectName = c("caribou", "landscape")),
#     stringsAsFactors = FALSE
#   )
#   paths$outputPath <- tmpdir
#
#   times <- list(start = 0, end = 1, "month")
#   parameters <- list(
#     .globals = list(stackName = "landscape"),
#     caribouMovement = list(.plotInitialTime = NA),
#     randomLandscapes = list(.plotInitialTime = NA, nx = 5, ny = 5)
#   )
#   mySim <- simInit(times = times, params = parameters, modules = modules,
#                    paths = paths, outputs = outputs)
#
#   mySim <- spades(mySim)
#
#   # test that if no save times are stated, then it is at end time
#   expect_true(file.exists(file.path(tmpdir, "caribou_month1.rds")))
#   expect_true(file.exists(file.path(tmpdir, "landscape_month1.rds")))
#   rm(mySim)
# })
#
# test_that("saving csv files does not work correctly", {
#   testInitOut <- testInit(smcc = FALSE)
#
#    tempObj <- 1:10
#    tempObj2 <- paste("val", 1:10)
#    df1 <- data.frame(col1 = tempObj, col2 = tempObj2)
#    sim <- simInit(objects = c("tempObj", "tempObj2", "df1"),
#                   paths = list(outputPath = tmpdir))
#    outs <- data.frame(
#         objectName = c(rep("tempObj", 2), rep("tempObj2", 3), "df1"),
#         saveTime = c(c(1, 4), c(2, 6, 7), end(sim)),
#         eventPriority = c(2:7),
#         fun = c(rep("saveRDS", 5), "write.csv"),
#         package = c(rep("base", 5), "utils"),
#         stringsAsFactors = FALSE)
#    outputs(sim) <- outs
#    # since write.csv has a default of adding a column, x, with rownames, must add additional
#    #   argument for 6th row in data.frame (corresponding to the write.csv function)
#    sim2 <- Copy(sim)
#    outputArgs(sim2)[[6]] <- list(row.names = FALSE)
#    sim2 <- spades(sim2)
#    outputs(sim2)
#    completes <- completed(sim2)
#    expect_true("eventPriority" %in% colnames(completes))
#    expect_true(all(completes[eventType != "init"]$eventPriority == outs[order(outs$saveTime),]$eventPriority))
#
#    # read one back in just to test it all worked as planned
#    newObj <- read.csv(dir(tmpdir, pattern = "year10.csv", full.name = TRUE))
#    expect_true(identical(df1, newObj))
#
#    # Confirm that arguments are actually being passed in by changing row.names to TRUE
#    sim2 <- Copy(sim)
#    outputArgs(sim2)[[6]] <- list(row.names = TRUE)
#    sim2 <- spades(sim2)
#    outputs(sim2)
#    # read one back in just to test it all worked as planned
#    newObj <- read.csv(dir(tmpdir, pattern = "year10.csv", full.name = TRUE))
#    expect_false(identical(df1, newObj))
# })

test_that("saveSimList does not work correctly", {

  testInitOut <- testInit(smcc = FALSE, libraries = c("NLMR", "terra"),
                          tmpFileExt = c("grd", "qs", "qs", "tif", "", "", "grd"))
  unlink(tmpfile[5])
  unlink(tmpfile[6])
  mapPath <- system.file("maps", package = "quickPlot")

  times <- list(start = 0, end = 1)
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plotInitialTime = NA_integer_),
    randomLandscapes = list(.plotInitialTime = NA_integer_, nx = 20, ny = 20)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES.core"),
    inputPath = mapPath,
    outputPath = tmpdir
  )

  mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths,
                   outputs = data.frame(objectName = "landscape", saveTime = times$end))
  mySim <- spades(mySim)
  mySim$landscape[] <- round(mySim$landscape[], 3) # after saving, these come back different, unless rounded
  mySim$landscape <- writeRaster(mySim$landscape, filename = tmpfile[1], overwrite = TRUE, datatype = "FLT4S")
  mySim$habitatQuality <- writeRaster(mySim$landscape, filename = tmpfile[7], overwrite = TRUE)

  ## test using qs
  # removes the file-backing, loading it into R as an inMemory object
  saveSimList(mySim, filename = tmpfile[2], fileBackend = 2)
  sim <- loadSimList(file = tmpfile[2], paths = paths(mySim))

  # on the saved/loaded one, it is there because it is not file-backed
  expect_true(is.numeric(sim$landscape$DEM[]))

  ## test using rds
  # removes the file-backing, loading it into R as an inMemory object
  fn <- paste0(tools::file_path_sans_ext(tmpfile[2]), ".rds")
  saveSimList(mySim, filename = fn, fileBackend = 2)
  sim <- loadSimList(file = fn, paths = paths(mySim))
  # on the saved/loaded one, it is there because it is not file-backed
  expect_true(is.numeric(sim$landscape$DEM[]))
  unlink(fn)

  # Now put it back to disk for subsequent test
  sim$landscape[] <- sim$landscape[]
  sim$habitatQuality[] <- sim$habitatQuality[]
  unlink(c(tmpfile[1], paste0(tools::file_path_sans_ext(tmpfile[1]), ".gri"))) ## needed because of hardlink shenanigans
  unlink(c(tmpfile[7], paste0(tools::file_path_sans_ext(tmpfile[7]), ".gri"))) ## needed because of hardlink shenanigans
  sim$landscape <- writeRaster(sim$landscape, filename = tmpfile[1])
  sim$habitatQuality <- writeRaster(sim$habitatQuality, filename = tmpfile[7])
  # grd format doesn't get minmax right especially with terra -- can't fix it with terra

  # The terra pointers with grd files make comparisons wrong
  mySim$habitatQuality <- rasterToMemory(mySim$habitatQuality)
  mySim$landscape <- rasterToMemory(mySim$landscape)
  sim$habitatQuality <- rasterToMemory(sim$habitatQuality)
  sim$landscape <- rasterToMemory(sim$landscape)

  expect_true(all.equal(mySim, sim, check.environment = FALSE))

  # Now try to keep filename intact
  mySim$landscape <- writeRaster(mySim$landscape, filename = tmpfile[1], overwrite = TRUE)
  mySim$habitatQuality <- writeRaster(mySim$landscape, filename = tmpfile[7], overwrite = TRUE)


  # loses the raster landscape
  saveSimList(mySim, filename = tmpfile[3], fileBackend = 0, filebackedDir = NULL)

  sim <- loadSimList(file = tmpfile[3])
  expect_true(identical(gsub("\\_.", "", checkPath(Filenames(sim$landscape, allowMultiple = FALSE))),
                        tmpfile[1]))
  expect_true(bindingIsActive("mod", sim@.xData$.mods$caribouMovement))

  # Now keep as file-backed, but change name
  # aaaa <<- 1
  saveSimList(mySim, filename = tmpfile[3], fileBackend = 1, filebackedDir = tmpCache)

  sim <- loadSimList(file = tmpfile[3])
  expect_false(identical(Filenames(sim$landscape, allowMultiple = FALSE), tmpfile[1]))

  file.remove(dir(dirname(tmpfile[1]), pattern = ".gr.$", full.names = TRUE))
  # rm(mySim)

  assign("a", 1, envir = mySim@.xData$.mods$caribouMovement$.objects)
  assign("a", 2, envir = sim@.xData$.mods$caribouMovement$.objects)

  expect_true(bindingIsActive("mod", sim@.xData$.mods$caribouMovement))
  # test file-backed raster is gone
  expect_error(mySim$landscape$DEM[])

  #### zipSimList test
  skip_if_not(nzchar(Sys.which("zip")))
  tmpZip <- file.path(tmpdir, paste0(rndstr(1, 6), ".zip"))
  checkPath(dirname(tmpZip), create = TRUE)
  landscape2 <- suppressMessages(Copy(sim$landscape, filebackedDir = "hello", fileBackend = 1))
  landscape3 <- suppressMessages(Copy(sim$landscape, filebackedDir = "hi", fileBackend = 1))
  landscape3 <- suppressWarnings(writeRaster(landscape3, filename = tmpfile[[4]], overwrite = TRUE))
  landscape3 <- suppressMessages(Copy(landscape3, filebackedDir = "hello", fileBackend = 1, overwrite = TRUE))
  habitatQuality <- suppressMessages(Copy(sim$habitatQuality, filename = tmpfile[[7]], filebackedDir = "hello", fileBackend = 1, overwrite = TRUE))
  sim$ListOfRasters <- list(landscape2, landscape3, habitatQuality)
  suppressMessages(zipSimList(sim, zipfile = tmpZip, filename = "test.qs"))

  unlink(Filenames(sim))
  files <- dir()
  unlink(grep(".*\\.zip$", files, value = TRUE, invert = TRUE), force = TRUE, recursive = TRUE)
  unlink(paths(mySim)$rasterPath, recursive = T, force = TRUE)

  pths <- paths(mySim)
  pths$cachePath <- tmpfile[5]
  pths$outputPath <- tmpfile[6]

  # TODO HERE -- load both reproducible and SpaDES.core
  out <- unzipSimList(tmpZip, paths = pths)

  origFns <- Filenames(sim)
  # Files all exist
  expect_true(all(file.exists(Filenames(out))))
  # They are in their sub-directories (2 * dirname), in the pths NOT the original paths
  expect_true(any(normPath(unname(unlist(pths))) %in% normPath(dirname(dirname(Filenames(out))))))
  expect_false(any(normPath(unname(origFns)) %in% unname(normPath(Filenames(out)))))
  # capture the subdirectories
  expect_true(all(basename(dirname(origFns)) %in% basename(dirname(Filenames(out)))))

  # None of the original files exist
  expect_true(!all(file.exists(origFns)))
})

test_that("restart does not work correctly", {
  skip("restartR not possible in automated tests")
  skip_if_not_installed("NLMR")

  # Must be run manually
  setwd("~/GitHub/SpaDES.core")
  #devtools::install(update.dependencies = FALSE, dependencies = FALSE) # need to install latest so that at restart it has everything
  testInitOut <- testInit(libraries = c("NLMR", "terra"), tmpFileExt = c("grd", "Rdata", "Rdata"),
                          opts = list("spades.restartRInterval" = 1, "spades.moduleCodeChecks" = FALSE))
  options("spades.restartRInterval" = 0, "spades.saveSimList.fileBackend" = 0,
          "spades.restartR.clearFiles" = FALSE)

  tmpdir <- "~"
  testNum = 3
  if (testNum == 1) {
    interval = 1
    mapPath <- system.file("maps", package = "quickPlot")

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = system.file("sampleModules", package = "SpaDES.core"),
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
      paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
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
    mapPath <- system.file("maps", package = "quickPlot")

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = system.file("sampleModules", package = "SpaDES.core"),
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
  skip_if_not_installed("NLMR")

  # Must be run manually
  setwd("~/GitHub/SpaDES.core")
  library(SpaDES.core)
  #devtools::install(update.dependencies = FALSE, dependencies = FALSE) # need to install latest so that at restart it has everything
  options("spades.restartRInterval" = 0, "spades.saveSimList.fileBackend" = 0,
          "spades.restartR.clearFiles" = FALSE)

  tmpdir <- "~"
  testNum = 3
    interval = 1
    mapPath <- system.file("maps", package = "quickPlot")

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = system.file("sampleModules", package = "SpaDES.core"),
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
