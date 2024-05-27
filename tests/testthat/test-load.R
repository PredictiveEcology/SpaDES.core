test_that("loading inputs does not work correctly", {
  testInit(c(sampleModReqdPkgs, "quickPlot"))

  mapPath <- getMapPath(tmpdir)

  functions <- c("raster", "rast")
  packages <- c("raster", "terra")
  for (i in seq(functions)) {
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = functions[i],
      package = packages[i],
      stringsAsFactors = FALSE
    )

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plots = ""),
      randomLandscapes = list(.plots = "", nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = getSampleModules(tmpdir),
      inputPath = mapPath,
      outputPath = tmpdir
    )

    mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)
    mySim <- spades(mySim)
    expect_true(all(c("DEM", "forestAge") %in% names(mySim$landscape)))

    # test overall inputs setReplaceMethod
    inputs  <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = functions[i],
      package = packages[i],
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
  }

  # use loadFiles directly
    sim1 <- loadFiles(
      filelist = filelist,
      paths = list(
        modulePath = getSampleModules(tmpdir),
        inputPath = mapPath,
        outputPath = file.path(tmpdir, rndstr()))
    )
    expect_true(all(c("DEM", "forestAge") %in% ls(sim1)))
    rm(sim1)

    # load at future time, i.e., nothing gets loaded
    for (i in seq(packages)) {
      inputs <- data.frame(
        files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
        functions = functions[i],
        package = packages[i],
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
        functions = functions[i],
        package = packages[i],
        loadTime = c(0, 3),
        stringsAsFactors = FALSE
      )
      mySim <- simInit(times = times, params = parameters, modules = modules,
                       paths = paths, inputs = inputs)

      expect_true(c("DEM") %in% ls(mySim))
      expect_true(!any(c("forestAge") %in% ls(mySim)))
      rm(mySim)
    }
  # }
})

test_that("passing arguments to filelist in simInit does not work correctly", {
  skip_on_cran()

  testInit(c(sampleModReqdPkgs, "quickPlot", "data.table"))

  # Second, more sophisticated. All maps loaded at time = 0, and the last one is reloaded
  #  at time = 10 and 20 (via "intervals").
  # Also, pass the single argument as a list to all functions...
  #  specifically, when add "native = TRUE" as an argument to the raster function
  mapPath <- getMapPath(tmpdir)
  files <- dir(file.path(mapPath), full.names = TRUE, pattern =  "tif")[1:4]
  parameters <- list(
    .globals = list(stackName = "landscape"),
    caribouMovement = list(.plots = ""),
    randomLandscapes = list(.plots = "", nx = 20, ny = 20)
  )
  modules <- list("randomLandscapes", "caribouMovement")
  paths <- list(
    modulePath = getSampleModules(tmpdir),
    inputPath = mapPath,
    outputPath = tmpdir
  )

  fns <- c("raster::raster", "terra::rast")
  args <- list(I(rep(list("native" = TRUE), 4)), NULL)
  for (i in seq(fns)) {
    inputs <- data.frame(
      files = files,
      functions = rep(fns[i], 4),
      objectName = rep(NA, 4),
      loadTime = c(0, 1, 1, 3),
      intervals = c(NA, 1, 2, NA),
      stringsAsFactors = FALSE
    )
    if (length(args[[i]]))
      inputs$args = args[[i]]

    times <- list(start = 0, end = 1, timeunit = "seconds")

    sim2 <- simInit(times = times, params = parameters, modules = modules,
                    paths = paths, inputs = inputs)
    expect_true(c("DEM") %in% ls(sim2))

    # Test that arguments got passed in correctly
    if (length(args[[i]]))
      expect_equal(inputs(sim2)$arguments, I(rep(list(native = TRUE), 4)))
    expect_true(!any(c("forestCover", "forestAge", "habitatQuality") %in% ls(sim2)))

    sim2 <- spades(sim2)
    expect_true(all(c("DEM", "forestAge", "forestCover") %in% ls(sim2)))
    expect_true(!any(c("percentPine") %in% ls(sim2)))

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
  }
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
  mess <- capture_messages(simInit(times = times, params = parameters, modules = modules,
                                   paths = paths, inputs = inputs))
  expect_true(any(grepl(paste(basename(tmpFile)), mess)))
  # }
})

test_that("passing objects to simInit does not work correctly", {
  testInit(c(sampleModReqdPkgs, "quickPlot"))

  mapPath <- mapPath <- getMapPath(tmpdir)

  # test object passing directly
  functions <- c("raster", "rast")
  packages <- c("raster", "terra")
  if (!requireNamespace("raster", quietly = TRUE)) {
    functions <- functions[2]
    packages <- packages[2]
  }

  for (i in seq(functions)) {
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = functions[i],
      package = packages[i],
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, getFromNamespace(functions[i], ns = packages[i]))
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

    times <- list(start = 0, end = 1)
    parameters <- list(
      .globals = list(stackName = "landscape"),
      caribouMovement = list(.plots = ""),
      randomLandscapes = list(.plots = "", nx = 20, ny = 20)
    )
    modules <- list("randomLandscapes", "caribouMovement")
    paths <- list(
      modulePath = getSampleModules(tmpdir),
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
      functions = functions[i],
      package = packages[i],
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

test_that("passing nearly empty file to simInit does not work correctly", {
  testInit(c("terra", "quickPlot"))

  mapPath <- mapPath <- getMapPath(tmpdir)

  # test object passing directly
  functions <- c("raster", "rast")
  packages <- c("raster", "terra")
  if (!requireNamespace("raster", quietly = TRUE)) {
    functions <- functions[2]
    packages <- packages[2]
  }

  for (i in seq(functions)) {

    mapPath <- getMapPath(tmpdir)

    # test object passing directly
    filelist <- data.frame(
      files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
      functions = functions[i],
      package = packages[i],
      stringsAsFactors = FALSE
    )
    layers <- lapply(filelist$files, getFromNamespace(functions[i], ns = packages[i]))
    DEM <- layers[[1]]
    forestAge <- layers[[2]]

    times <- list(start = 0, end = 1)

    sim3 <- simInit(inputs = filelist)

    expect_true(all(c("DEM", "forestAge") %in% ls(sim3)))
    rm(sim3)
  }
})

test_that("more tests", {
  skip_on_cran()

  testInit("raster")

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

    files <- dir(getMapPath(tmpdir), full.names = TRUE)
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
  # }
})

test_that("interval loading of objects from .GlobalEnv", {
  testInit("ggplot2")

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
  testInit(c("terra"), tmpFileExt = c(".tif", ".grd", ".tif", ".tif", ".grd"),
                          opts = list("reproducible.ask" = FALSE))

  mapPath <- getMapPath(tmpdir)

  s <- simInit()
  packages <- c("raster", "terra")
  functions <- cbind(c("raster", "extent", "stack", "nlayers"),
                     c("rast", "ext", "rast", "nlyr"))
  if (!requireNamespace("raster", quietly = TRUE)) {
    functions <- functions[, 2,drop = FALSE]
    packages <- packages[2]
  }
  for (i in seq(packages)) {
    read <- getFromNamespace(functions[1, i], ns = packages[i])
    ext <- getFromNamespace(functions[2, i], ns = packages[i])
    if (packages[i] %in% "raster")
      stk <- getFromNamespace(functions[3, i], ns = packages[i])
    else
      stk <- c
    nlyr <- getFromNamespace(functions[4, i], ns = packages[i])

    s$r <- read(ext(0, 10, 0, 10), vals = 1, res = 1)
    s$r2 <- read(ext(0, 10, 0, 10), vals = 1, res = 1)
    s$r <- suppressWarnings(writeRaster(s$r, filename = tmpfile[1], overwrite = TRUE))
    s$r2 <- suppressWarnings(writeRaster(s$r2, filename = tmpfile[3], overwrite = TRUE))
    s$s <- stk(s$r, s$r2)
    a <- s$s
    a[] <- a[]
    a <- stk(a)
    s$b <- writeRaster(a, filename = tmpfile[5], overwrite = TRUE)
    s$b <- stk(s$b)
    Fns <- Filenames(s)

    fnsGrd <- normPath(c(Filenames(s$b)))#, gsub("grd$", "gri", Filenames(s$b))))
    expect_true(identical(c(Fns[["b1"]], Fns[["b2"]]), fnsGrd))
    expect_true(identical(Fns[["r"]], normPath(Filenames(s$r))))
    expect_true(identical(Fns[["r2"]], normPath(Filenames(s$r2))))
    expect_true(identical(c(Fns[["s1"]], Fns[["s2"]]),
                          sapply(seq_len(nlyr(s$s)), function(rInd)
                            normPath(Filenames(s$s[[rInd]])))))
  }
})
