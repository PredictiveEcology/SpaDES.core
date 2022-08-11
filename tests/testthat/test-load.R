test_that("test-load.R: loading inputs does not work correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  mapPath <- system.file("maps", package = "quickPlot")

  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    stringsAsFactors = FALSE
  )

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

  mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)
  mySim <- spades(mySim)
  expect_true(all(c("DEM", "forestAge") %in% names(mySim$landscape)))

  # test overall inputs setReplaceMethod
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "fun", "package", "loadTime")], inputs)
  expect_equal(fileName(inputs(mySim)$file), inputs(mySim)$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  # test fill in objectName and function and package
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "loadTime")], inputs)
  expect_equal(fileName(inputs(mySim)$file), inputs(mySim)$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  # test override default object name
  inputs  <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    objectName = c("rasDEM", "rasForestAge"),
    stringsAsFactors = FALSE
  )
  inputs(mySim) <- inputs
  expect_equal(inputs(mySim)[, c("file", "objectName")], inputs)
  expect_equal(inputs(mySim)$objectName, inputs$objectName)
  expect_equal(inputs(mySim)$loaded, rep(NA, NROW(inputs(mySim))))

  rm(mySim)

  # use loadFiles directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    sim1 <- loadFiles(
      filelist = filelist,
      paths = list(
        modulePath = system.file("sampleModules", package = "SpaDES.core"),
        inputPath = mapPath,
        outputPath = file.path(tmpdir, rndstr()))
    )
    expect_true(all(c("DEM", "forestAge") %in% ls(sim1)))
    rm(sim1)

    # load at future time, i.e., nothing gets loaded
    inputs <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      loadTime = 3,
      stringsAsFactors = FALSE
    )
    mySim <- simInit(times = times, params = parameters, modules = modules,
                     paths = paths, inputs = inputs)
    expect_true(!any(c("DEM", "forestAge") %in% ls(mySim)))
    rm(mySim)

    # load some at future time, i.e., only one gets loaded
    inputs <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      loadTime = c(0, 3),
      stringsAsFactors = FALSE
    )
    mySim <- simInit(times = times, params = parameters, modules = modules,
                     paths = paths, inputs = inputs)

    expect_true(c("DEM") %in% ls(mySim))
    expect_true(!any(c("forestAge") %in% ls(mySim)))
    rm(mySim)
  }
})

test_that("test-load.R: passing arguments to filelist in simInit does not work correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
  #  at time = 10 and 20 (via "intervals").
  # Also, pass the single argument as a list to all functions...
  #  specifically, when add "native = TRUE" as an argument to the raster function
  mapPath <- system.file("maps", package = "quickPlot")
  files <- dir(file.path(mapPath), full.names = TRUE, pattern =  "tif")[1:4]
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

  inputs <- data.frame(
    files = files,
    functions = rep("raster::raster", 4),
    objectName = rep(NA, 4),
    loadTime = c(0, 1, 1, 3),
    intervals = c(NA, 1, 2, NA),
    args = I(rep(list("native" = TRUE), 4)),
    stringsAsFactors = FALSE
  )
  times <- list(start = 0, end = 1, timeunit = "seconds")

  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    sim2 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, inputs = inputs)
    expect_true(c("DEM") %in% ls(sim2))

    # Test that arguments got passed in correctly
    expect_equal(inputs(sim2)$arguments, I(rep(list(native = TRUE), 4)))
    expect_true(!any(c("forestCover", "forestAge", "habitatQuality") %in% ls(sim2)))

    sim2 <- spades(sim2)
    expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
    expect_true(!any(c("habitatQuality") %in% ls(sim2)))

    rm(forestAge, envir = envir(sim2))
    expect_true(!("forestAge" %in% ls(sim2)))

    end(sim2) <- 2
    sim2 <- spades(sim2)
    expect_true(all(c("forestAge") %in% names(sim2$landscape)))

    end(sim2) <- 3
    expect_message(spades(sim2), "habitatQuality read from")
    expect_message(spades(sim2), "forestCover")
    expect_message(spades(sim2), "forestAge")
    expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
    rm(sim2)

    # test without package specified
    dt <- data.table::data.table(a = 3, b = 2)
    tmpFile <- tempfile()
    write.table(dt, file = tmpFile, sep = "\t", col.names = TRUE, row.names = FALSE)
    inputs <- data.frame(
      files = tmpFile,
      functions = "data.table::fread",
      stringsAsFactors = FALSE
    )
    sim2 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, inputs = inputs)
    expect_equal(sim2@.xData[[basename(tmpFile)]], dt)

    inputs <- data.frame(
      files = tmpFile,
      functions = "fread",
      stringsAsFactors = FALSE
    )
    require(data.table)
    mess <- capture_messages(simInit(times = times, params = parameters, modules = modules,
                                     paths = paths, inputs = inputs))
    expect_true(any(grepl(paste(basename(tmpFile)), mess)))
  }
})

test_that("test-load.R: passing objects to simInit does not work correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  mapPath <- mapPath <- system.file("maps", package = "quickPlot")

  # test object passing directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, raster)
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

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

    # Pass as a named list
    objects <- list(DEM = DEM, forestAge = forestAge)
    sim3 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects  =  objects)
    expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
    rm(sim3)

    # pass as character vector
    objects <- c("DEM", "forestAge")
    sim4 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects)
    expect_true(all(c("DEM", "forestAge") %in% ls(sim4)))
    rm(sim4)

    # pass both inputs and objects
    objects <- c("DEM")
    sim5 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects, inputs = filelist[-1, ])
    expect_true(all(c("DEM", "forestAge") %in% ls(sim5)))
    rm(sim5)

    # pass both inputs (at non-start time) and objects
    # test object passing directly
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[2],
      functions = "raster",
      package = "raster",
      loadTime = 1,
      stringsAsFactors = FALSE
    )
    objects <- c("DEM")
    sim6 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, objects = objects, inputs = filelist)
    expect_true(all(c("DEM") %in% ls(sim6)))
    expect_false(all(c("forestAge") %in% ls(sim6)))
    sim6End <- spades(sim6) # run for 1 time, loading the file
    expect_true(all(c("forestAge") %in% ls(sim6End)))
    rm(sim6)
  }
})

test_that("test-load.R: passing nearly empty file to simInit does not work correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  mapPath <- system.file("maps", package = "quickPlot")

  # test object passing directly
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = "raster",
      package = "raster",
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, raster)
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

    times <- list(start = 0, end = 1)

    sim3 <- simInit(inputs = filelist)

    expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
    rm(sim3)
  }
})

test_that("test-load.R: more tests", {
  skip_on_cran()

  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  sim <- simInit()
  test <- 1:10
  tmpFile <- file.path(tmpdir, "test.rds")
  save(test, file = tmpFile)

  # Test for data.frame being kept as data.frame even with only one column
  expect_silent(inputs(sim) <- data.frame(file = tmpFile))

  bb <- simInit(inputs = data.frame(files = tmpFile, fun = "load"))
  expect_identical(bb$test, test)

  bb <- simInit(inputs = data.frame(files = tmpFile, fun = "load", package = "base"))
  expect_identical(bb$test, test)

  # Test for incremental loading via intervals
  if (require(rgdal, quietly = TRUE)) {
    on.exit(detach("package:rgdal"), add = TRUE)

    files <- dir(system.file("maps", package = "quickPlot"),
                full.names = TRUE, pattern = "tif")
    arguments <- I(rep(list(native = TRUE), length(files)))
    filelist <- data.frame(
       files = files,
       functions = "raster::raster",
       objectName = NA,
       arguments = arguments,
       loadTime = 0,
       intervals = c(rep(NA, length(files) - 1), 10)
    )
    expect_message({
      sim2 <- loadFiles(filelist = filelist)
    }, "DEM read from")
    expect_message({
      sim2 <- loadFiles(filelist = filelist)
    }, "forestAge read from")
    end(sim2) <- 20
    expect_message({
      sim3 <- spades(sim2)
    }, "time 10")
    expect_message({
      sim3 <- spades(sim2)
    }, "time 20")
  }
})

test_that("test-load.R: interval loading of objects from .GlobalEnv", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  times <- 0:10
  test1 <- "hi"
  newModule("test", path = tempdir(), open = FALSE)

  a <- simInit(inputs = data.frame(obj = "test1", loadTimes = times), times = list(start = 0, end = 2),
               modules = list("test"), paths = list(modulePath = tempdir()))
  expect_equal(test1, a$test1)
  a <- spades(a)
  expect_equal(test1, a$test1)
  test1 <- "lo"
  end(a) <- 3
  a <- spades(a)
  expect_equal(test1, a$test1)

  # Renamed using arguments
  times <- 0:10
  test1 <- "hi"
  newModule("test", path = tempdir(), open = FALSE)

  a1 <- 1
  a2 <- 2
  args <- lapply(1:2, function(x) {
    list(x = paste0("a", x),
         envir = environment())
  })
  names(args) <- rep("x", length(args))
  inputs <- data.frame(objectName = "a", loadTime = 1:2, fun = "get", package = "base", arguments = I(args))
  a <- simInit(inputs = inputs, times = list(start = 0, end = 1))
  a <- spades(a)
  expect_identical(a1, a$a)

  end(a) <- 3
  a <- spades(a)
  expect_identical(a2, a$a)

  # same, but without package specified
  inputs <- data.frame(objectName = "a", loadTime = 1:2, fun = "get", arguments = I(args))
  a <- simInit(inputs = inputs, times = list(start = 0, end = 1))
  a <- spades(a)
  expect_identical(a1, a$a)

  end(a) <- 3
  a <- spades(a)
  expect_identical(a2, a$a)
})

test_that("Filenames for simList", {
  testInitOut <- testInit(c("raster"), tmpFileExt = c(".tif", ".grd", ".tif", ".tif", ".grd"),
                          opts = list("reproducible.ask" = FALSE))

  on.exit({
    testOnExit(testInitOut)
    options(opts)
    rm(s)
  }, add = TRUE)

  s <- simInit()
  s$r <- raster(extent(0, 10, 0, 10), vals = 1, res = 1)
  s$r2 <- raster(extent(0, 10, 0, 10), vals = 1, res = 1)
  s$r <- suppressWarnings(writeRaster(s$r, filename = tmpfile[1], overwrite = TRUE))
  s$r2 <- suppressWarnings(writeRaster(s$r2, filename = tmpfile[3], overwrite = TRUE))
  s$s <- stack(s$r, s$r2)
  a <- s$s
  a[] <- a[]
  s$b <- writeRaster(a, filename = tmpfile[5], overwrite = TRUE)

  Fns <- Filenames(s)

  fnsGrd <- normPath(c(filename(s$b), gsub("grd$", "gri", filename(s$b))))
  expect_true(identical(c(Fns[["b1"]], Fns[["b2"]]), fnsGrd))
  expect_true(identical(Fns[["r"]], normPath(filename(s$r))))
  expect_true(identical(Fns[["r2"]], normPath(filename(s$r2))))
  expect_true(identical(c(Fns[["s1"]], Fns[["s2"]]),
              sapply(seq_len(nlayers(s$s)), function(rInd) normPath(filename(s$s[[rInd]])))))
})
