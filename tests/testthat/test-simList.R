test_that("simList object initializes correctly", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  defaults <- .coreModules() %>% unname()
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape")
  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  mySim <- simInit(times, params, modules, objects = list(), paths)

  # Test metadata accessors
  expect_true(length(inputObjects(mySim)) == 3)
  expect_true(NROW(inputObjects(mySim, "fireSpread")) == 2)
  expect_true(is.data.frame(inputObjects(mySim, "fireSpread")))

  expect_true(length(outputObjects(mySim)) == 3)
  expect_true(NROW(outputObjects(mySim, "fireSpread")) == 2)
  expect_true(is.data.frame(outputObjects(mySim, "fireSpread")))

  expect_true(length(documentation(mySim)) == 3)
  expect_true(NROW(documentation(mySim, "fireSpread")) == 0)

  expect_true(length(citation(mySim)) == 3)
  expect_true(NROW(citation(mySim, module = "fireSpread")) == 0)

  expect_true(length(reqdPkgs(mySim)) == 3)
  expect_true(NROW(reqdPkgs(mySim, "fireSpread")) == 4)


  #
  expect_is(mySim, "simList")

  w <- getOption("width")
  options(width = 100L)
  out <- utils::capture.output(show(mySim))

  # data.table v1.11.0 no longer prints "NULL" data.table.
  # See bug fix 8 in https://github.com/Rdatatable/data.table/blob/master/NEWS.md
  nline <- if (out[60] == "NULL") 75 else 73

  expect_equal(length(out), nline)
  options(width = w); rm(w)

  ### SLOT .xData
  expect_is(envir(mySim), "environment")
  expect_is(objs(mySim), "list")
  expect_equal(sort(names(objs(mySim, all.names = TRUE))),
               sort(names(as(mySim, "simList_"))))
  expect_equivalent(mySim, as(as(mySim, "simList_"), "simList"))
  expect_equal(ls(mySim), objects(mySim))
  expect_equal(ls(mySim), sort(names(objs(mySim))))
  expect_equivalent(ls.str(mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(pos = mySim), ls.str(objs(mySim)))
  expect_equivalent(ls.str(envir = mySim@.xData), ls.str(objs(mySim)))

  mySim$test1 <- TRUE
  mySim[["test2"]] <- TRUE

  # load
  expect_equal(inputs(mySim), .fileTableIn())

  objs(mySim) <- list(test3 = TRUE)

  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])
  expect_true(objs(mySim)$test3)
  expect_error(objs(mySim) <- "test4", "must provide a named list.")

  oldEnv <- envir(mySim)
  envir(mySim) <- new.env(parent = .GlobalEnv)

  expect_true(is.null(mySim$test1))
  expect_true(is.null(mySim[["test2"]]))
  expect_true(is.null(objs(mySim)$test3[[1]]))

  envir(mySim) <- oldEnv
  expect_true(mySim$test1)
  expect_true(mySim[["test2"]])
  rm(oldEnv)

  ### SLOT modules
  expect_is(modules(mySim), "list")
  compList <- as.list(c(defaults, modules))
  attr(compList, "modulesGraph") <- data.frame(from = character(0), to = character(),
                                               stringsAsFactors = FALSE)
  expect_equivalent(modules(mySim, hidden = TRUE), compList)
  expect_equal(unname(modules(mySim)), as.list(modules))

  ### SLOT params
  expect_is(params(mySim), "list")

  # globals
  outputPath(mySim) <- file.path(tempdir(), "outputs")
  expect_identical(outputPath(mySim), file.path(tempdir(), "outputs"))

  # checkpoint
  expect_true(is.null(checkpointFile(mySim)))
  checkpointFile(mySim) <- file.path(outputPath(mySim), "checkpoint.RData")
  expect_identical(checkpointFile(mySim),
                   file.path(outputPath(mySim), "checkpoint.RData"))

  expect_true(is.na(checkpointInterval(mySim)))
  checkpointInterval(mySim) <- 10
  expect_identical(checkpointInterval(mySim), 10)

  # progress
  expect_true(is.na(progressType(mySim)))
  progressType(mySim) <- "text"
  expect_identical(progressType(mySim), "text")

  expect_true(is.na(progressInterval(mySim)))
  progressInterval(mySim) <- 10
  expect_identical(progressInterval(mySim), 10)

  # load
  expect_error(inputs(mySim) <- "something", "inputs must be a list")

  # need tests for inputs
  # See test-load.R

  ### SLOT events
  expect_is(events(mySim), "data.table")
  expect_equal(nrow(events(mySim)), length(modules(mySim, hidden = TRUE)))

  ### SLOT current
  expect_is(current(mySim), "data.table")
  expect_equal(nrow(current(mySim)), 0)

  ### SLOT completed
  expect_is(completed(mySim), "data.table")
  expect_equal(nrow(completed(mySim)), 0)

  ### SLOT depends
  expect_is(depends(mySim), ".simDeps")
  expect_is(depends(mySim)@dependencies, "list")
  expect_is(depends(mySim)@dependencies[[3]], ".moduleDeps")
  expect_equal(depends(mySim)@dependencies[[3]]@name, modules[[3]])
  # not going to go though each level...object validity checking does types

  ### SLOT simtimes
  expect_equivalent(
    times(mySim),
    list(
      current = 1.0,
      start = 1.0,
      end = convertTimeunit(as.numeric(dmonth(10)), "month"),
      timeunit = "month")
  )

  expect_equivalent(end(mySim),  10)
  expect_equivalent(start(mySim), 1)
  expect_equivalent(time(mySim),  1)

  # Test explicit unit passing to end and start -- was a bug introduced in converting end & start to S3
  expect_equivalent(end(mySim, "seconds"), as.numeric(dmonth(10)))
  expect_equivalent(start(mySim, "seconds"), as.numeric(dmonth(1)))

  expect_equivalent(end(mySim, "year"), as.numeric(dmonth(1)/dyear(1)*10))
  expect_equivalent(start(mySim, "year"), as.numeric(dmonth(1)/dyear(1)))

  expect_equivalent(end(mySim)   <- 20, 20.0)
  expect_equivalent(start(mySim) <- 10, 10.0)
  expect_equivalent(time(mySim)  <- 10, 10.0)

  expect_equal(timeunit(mySim), attr(end(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(start(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(time(mySim), "unit"))

  expect_equal("second", attr(mySim@simtimes$start, "unit"))
  expect_equal("second", attr(mySim@simtimes$end, "unit"))
  expect_equal("second", attr(mySim@simtimes$current, "unit"))

  ### required packages
  pkgs <- c("grid", "methods", "raster", "RColorBrewer", "sp",
            "SpaDES.tools", "SpaDES.core", "stats")
  expect_equal(sort(packages(mySim)), sort(pkgs))

  reqdPkgs <- lapply(modules, function(m) {
    mfile <- file.path(system.file("sampleModules", package = "SpaDES.core"), m, paste0(m, ".R"))
    packages(filename = mfile)
  }) %>%
    unlist() %>%
    unique() %>%
    sort()
  expect_equal(sort(reqdPkgs), sort(pkgs))

  mdir <- getOption("spades.modulePath")
  options(spades.modulePath = system.file("sampleModules", package = "SpaDES.core"))
  on.exit(options(spades.modulePath = mdir), add = TRUE)
  reqdPkgs <- lapply(modules, function(m) packages(module = m)) %>%
    unlist() %>%
    unique() %>%
    sort()
  expect_equal(sort(reqdPkgs), sort(pkgs))

  rm(mySim)
})

test_that("simList test all signatures", {
  testInitOut <- testInit(opts = list(spades.moduleCodeChecks = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  # times
  times <- list(start = 0.0, end = 10)

  # modules
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")

  # paths
  mapPath <- system.file("maps", package = "quickPlot")
  paths <- list(
    modulePath = system.file("sampleModules", package = "SpaDES.core"),
    inputPath = mapPath,
    outputPath = tempdir()
  )

  # inputs
  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "raster",
    package = "raster",
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )

  if (require(rgdal)) {
    on.exit(detach("package:rgdal"), add = TRUE)

    # objects
    layers <- lapply(filelist$files, rasterToMemory)
    DEM <- layers[[1]]
    forestAge <- layers[[2]]
    objects <- list(DEM = "DEM", forestAge = "forestAge")
    objectsChar <- c("DEM", "forestAge")

    # outputs
    outputs <- data.frame(
      expand.grid(objectName = c("caribou", "landscape"),
                  saveTime = 1:2,
                  stringsAsFactors = FALSE)
    )

    # parameters
    parameters <- list(
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
    )

    # loadOrder
    loadOrder <- c("randomLandscapes", "caribouMovement", "fireSpread")

    # test all argument combinations to simInit
    N <- 256L
    successes <- logical(N)
    argsTested <- vector("list", length = N)
    #setPaths(inputPath = NULL, outputPath = NULL, modulePath = NULL, cachePath = NULL)
    for (i in 1L:N) {
      li <- list(
        {if (i %% 2 ^ 1 == 0) times = times},                   # nolint
        {if (ceiling(i / 2) %% 2 == 0) params = parameters},    # nolint
        {if (ceiling(i / 4) %% 2 == 0) modules = modules},      # nolint
        {if (ceiling(i / 8) %% 2 == 0) objects = objects},      # nolint
        {if (ceiling(i / 16) %% 2 == 0) paths = paths},         # nolint
        {if (ceiling(i / 32) %% 2 == 0) inputs = filelist},     # nolint
        {if (ceiling(i / 64) %% 2 == 0) outputs = outputs},     # nolint
        {if (ceiling(i / 128) %% 2 == 0) loadOrder = loadOrder} # nolint
      )
      argNames <- c("times", "params", "modules", "objects", "paths", "inputs",
                    "outputs", "loadOrder")
      names(li) <- argNames
      li <- li[!sapply(li, is.null)]
      successes[i] <- tryCatch(
        is(do.call(simInit, args = li), "simList"),
        error = function(e) { FALSE },
        warning = function(w) { FALSE }
      )
      argsTested[[i]] <- names(li)
    }

    # needs paths and params; many defaults are fine
    expect_equal(sum(successes, na.rm = TRUE), 256)
  }
})

test_that("simList object initializes correctly", {
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  ## test with outputs
  ras <- raster::raster(nrows = 10, ncols = 10, xmn = -5, xmx = 5, ymn = -5, ymx = 5)
  abundRasters <- list(SpaDES.tools::gaussMap(ras, scale = 100, var = 0.01))

  tmpdir <- tempdir()
  newModule(name = "test", path = file.path(tmpdir, "modules"), open = FALSE)
  obj <- list(abundRasters = abundRasters)#, tempRasters = tempRasters)
  paths <- list(modulePath = file.path(tmpdir, "modules"))

  ## If start is set to 1.0, there is a warning message and spades doesn't seem to run
  aa <- (capture_warnings(mySim <- simInit(times = list(start = 1.0, end = 2.0),
                                           modules = list("test"), paths = paths,
                                           objects = obj)))
  expect_length(aa, 0)
})

test_that("childModule bug test -- created infinite loop of 'Duplicated...'", {
  ## Test resulting from bug found by Greg Paradis April 7, 2019
  testInitOut <- testInit("raster")
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  setPaths(modulePath = tmpdir)
  childModName <- "child_module"
  newModule(childModName, tmpdir, type = "child")
  newModule("parent_module", tmpdir, type = "parent", children = c("child_module"))
  paths <- getPaths()
  modules <- list("parent_module")
  times <- list(start = 1, end = 10)
  expect_is(mySim <- simInit(paths = paths, modules = modules, times = times), "simList")
  ## test some child related stuff
  expect_true(all(modules(mySim) %in% childModName))
  expect_true(dirname(names(modules(mySim))) %in% modulePath(mySim))
})
