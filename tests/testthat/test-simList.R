test_that("simList object initializes correctly (1)", {
  testInit(sampleModReqdPkgs)

  defaults <- .coreModules() |> unname()
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape")
  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = getSampleModules(tmpdir))

  mySim <- simInit(times, params, modules, objects = list(), paths)

  ## test metadata accessors
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

  expect_is(mySim, "simList")

  w <- getOption("width")
  options(width = 100L)
  out <- utils::capture.output(show(mySim))

  expect_equal(length(out), 82)
  options(width = w)
  rm(w)

  ## test path accessors
  expect_identical(normPath(figurePath(mySim)), normPath(file.path(outputPath(mySim), "figures")))
  expect_identical(normPath(logPath(mySim)), normPath(file.path(outputPath(mySim), "log")))

  ### SLOT .xData
  expect_is(envir(mySim), "environment")
  expect_is(objs(mySim), "list")
  expect_equal(sort(names(objs(mySim, all.names = TRUE))), sort(names(as(mySim, "simList_"))))
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
  compList <- setdiff(as.list(c(defaults, modules)), "restartR")
  attr(compList, "modulesGraph") <- data.frame(from = character(0), to = character(),
                                               stringsAsFactors = FALSE)
  expect_equivalent(unname(modules(mySim, hidden = TRUE)), compList)
  expect_equal(unname(modules(mySim)), as.list(modules))

  ### SLOT params
  expect_is(params(mySim), "list")

  # globals
  outputPath(mySim) <- file.path(tempdir(), "outputs")
  expect_identical(outputPath(mySim), file.path(tempdir(), "outputs"))

  # checkpoint
  expect_true(is.null(checkpointFile(mySim)))
  checkpointFile(mySim) <- file.path(outputPath(mySim), "checkpoint.RData")
  expect_identical(checkpointFile(mySim), file.path(outputPath(mySim), "checkpoint.RData"))

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
  expect_equal(nrow(completed(mySim)), 3) ## .inputObjects for each module have run

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

  ## explicitly test unit passing to end and start;
  ## was a bug introduced in converting end & start to S3
  expect_equivalent(end(mySim, "seconds"), as.numeric(dmonth(10)))
  expect_equivalent(start(mySim, "seconds"), as.numeric(dmonth(1)))

  expect_equivalent(end(mySim, "year"), as.numeric(dmonth(1) / dyear(1) * 10))
  expect_equivalent(start(mySim, "year"), as.numeric(dmonth(1) / dyear(1)))

  expect_equivalent(end(mySim)   <- 20, 20.0)
  expect_equivalent(start(mySim) <- 10, 10.0)
  expect_equivalent(time(mySim), 1.0)

  expect_equal(timeunit(mySim), attr(end(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(start(mySim), "unit"))
  expect_equal(timeunit(mySim), attr(time(mySim), "unit"))

  expect_equal("second", attr(mySim@simtimes$start, "unit"))
  expect_equal("second", attr(mySim@simtimes$end, "unit"))
  expect_equal("second", attr(mySim@simtimes$current, "unit"))

  ### required packages
  pkgs <- c("grid", "methods", "NLMR", "terra", "reproducible", "RColorBrewer", "sf",
            "SpaDES.core", "SpaDES.tools", "stats")
  expect_equal(sort(packages(mySim, clean = TRUE)), sort(pkgs))

  reqdPkgs <- lapply(modules, function(m) {
    mfile <- file.path(getSampleModules(tmpdir), m, paste0(m, ".R"))
    packages(filename = mfile)
  }) |>
    unlist() |>
    sort() |>
    .cleanPkgs() |>
    unique()
  expect_equal(sort(reqdPkgs), sort(pkgs))

  mdir <- getOption("spades.modulePath")
  options(spades.modulePath = getSampleModules(tmpdir))
  on.exit(options(spades.modulePath = mdir), add = TRUE)
  reqdPkgs <- lapply(modules, function(m) packages(module = m)) |>
    unlist() |>
    sort() |>
    .cleanPkgs() |>
    unique()
  expect_equal(sort(reqdPkgs), sort(pkgs))

  rm(mySim)
})

test_that("simList object initializes correctly (2)", {
  testInit(c("terra", "ggplot2"), smcc = FALSE)
  ## test with outputs
  abundRasters <- list(terra::rast(system.file("extdata", "abundRaster.tif",
                                               package = "SpaDES.core")))

  tmpdir <- tempdir()
  newModule(name = "test", path = file.path(tmpdir, "modules"), open = FALSE)
  obj <- list(abundRasters = abundRasters)#, tempRasters = tempRasters)
  paths <- list(modulePath = file.path(tmpdir, "modules"))

  ## If start is set to 1.0, there is a warning message and spades doesn't seem to run
  aa <- (capture_warnings({
    mySim <- simInit(times = list(start = 1.0, end = 2.0),
                     modules = list("test"), paths = paths,
                     objects = obj)
  }))
  expect_equivalent(grep("was built under R version", aa, invert = TRUE, value = TRUE), character())
})

test_that("simList test all signatures", {
  skip_on_cran()

  testInit(sampleModReqdPkgs, opts = list(spades.useRequire = FALSE))

  on.exit({
    if (!curPathIsPkgPath) {
      setPaths(modulePath = origDir)
    }
  }, add = TRUE)

  ## times
  times <- list(start = 0.0, end = 10)

  ## modules
  modules <- list("randomLandscapes")#, "caribouMovement", "fireSpread")
  curPathIsPkgPath <- (identical(
    normPath(getSampleModules(tmpdir)),
    normPath(getwd())))
  if (!curPathIsPkgPath) { # on a R CMD check, these paths are same, so don't copy
    modPaths <- dir(getSampleModules(tmpdir),
                    recursive = TRUE, full.names = TRUE)
    modPaths <- grep(paste(modules, collapse = "|"), modPaths, value = TRUE)
    file.copy(dirname(modPaths), recursive = TRUE, to = ".")
    origDir <- getOption("spades.modulePath", getwd())
    setPaths(modulePath = getwd())
  }

  ## paths
  mapPath <- getMapPath(tmpdir)
  paths <- list(
    modulePath = getSampleModules(tmpdir),
    inputPath = mapPath,
    outputPath = tempdir()
  )

  ## inputs
  filelist <- data.frame(
    files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
    functions = "rast",
    package = "terra",
    loadTime = c(0, 3),
    stringsAsFactors = FALSE
  )

  ## objects
  layers <- lapply(filelist$files, rasterToMemory)
  DEM <- layers[[1]]
  forestAge <- layers[[2]]
  objects <- list(DEM = "DEM", forestAge = "forestAge")
  objectsChar <- c("DEM", "forestAge")

  ## outputs
  outputs <- data.frame(
    expand.grid(objectName = c("caribou", "landscape"),
                saveTime = 1:2,
                stringsAsFactors = FALSE)
  )

  ## parameters
  parameters <- list(
    caribouMovement = list(.plotInitialTime = NA),
    randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20)
  )

  # loadOrder
  loadOrder <- c("randomLandscapes")#, "caribouMovement", "fireSpread")

  ## test all argument combinations to simInit
  N <- 256L
  successes <- logical(N)
  argsTested <- vector("list", length = N)
  ## setPaths(inputPath = NULL, outputPath = NULL, modulePath = NULL, cachePath = NULL)
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
    messes <- capture_messages({
      successes[i] <- tryCatch({
          is(suppressMessages(do.call(simInit, args = li)), "simList")
        }, error = function(e) { FALSE }, warning = function(w) { TRUE }
      )
    })
    argsTested[[i]] <- names(li)
  }

  ## needs paths and params; many defaults are fine
  expect_equal(sum(successes, na.rm = TRUE), 256)
  if (FALSE) {
    dt <- data.table(lapply(argsTested, paste, collapse = "_"), successes)
  }
  #  }
})

test_that("childModule bug test -- created infinite loop of 'Duplicated...'", {
  skip_on_cran()
  ## Test resulting from bug found by Greg Paradis April 7, 2019
  testInit(c("terra", "ggplot2"), smcc = FALSE)
  setPaths(modulePath = tmpdir)
  childModName <- "child_module"
  newModule(childModName, tmpdir, type = "child", open = FALSE)
  newModule("parent_module", tmpdir, type = "parent", children = c("child_module"), open = FALSE)
  paths <- getPaths()
  modules <- list("parent_module")
  times <- list(start = 1, end = 10)
  expect_is({mySim <- simInit(paths = paths, modules = modules, times = times)}, "simList")
  ## test some child related stuff
  expect_true(all(modules(mySim) %in% childModName))
  expect_true(dirname(names(modules(mySim))) %in% modulePath(mySim))
})

test_that("test that module directory exists, but not files", {
  skip_on_cran()

  ## Test resulting from bug found by Eliot McIntire April 28, 2019
  testInit("terra", smcc = FALSE)
  setPaths(modulePath = tmpdir)
  childModName <- "child_module"
  parentModName <- "parent_module"
  newModule(childModName, tmpdir, type = "child", open = FALSE)
  newModule(parentModName, tmpdir, type = "parent", children = childModName, open = FALSE)
  paths <- getPaths()
  modules <- list(parentModName)
  times <- list(start = 1, end = 10)
  mainChildModuleFile <- file.path(paths$modulePath, childModName, paste0(childModName, ".R"))
  mainParentModuleFile <- file.path(paths$modulePath, parentModName, paste0(parentModName, ".R"))
  expect_true(file.exists(mainChildModuleFile))

  file.remove(mainChildModuleFile)
  a <- capture_messages({
    expect_error(simInit(paths = paths, modules = modules, times = times), "does not exist")
  })

  unlink(dirname(mainChildModuleFile), recursive = TRUE)
  a <- capture_messages({
    expect_error(simInit(paths = paths, modules = modules, times = times), "does not exist")
  })

  file.remove(mainParentModuleFile)
  a <- capture_messages({
    expect_error(simInit(paths = paths, modules = modules, times = times), "doesn't exist")
  })

  unlink(dirname(mainParentModuleFile), recursive = TRUE)
  a <- capture_messages({
    expect_error(simInit(paths = paths, modules = modules, times = times), "doesn't exist in")
  })

  unlink(paths$modulePath, recursive = TRUE)
})

test_that("inputObjects on module arg not sim", {
  testInit(sampleModReqdPkgs)

  defaults <- .coreModules() |> unname()
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape")
  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = getSampleModules(tmpdir))
  io <- inputObjects(module = modules, path = paths$modulePath)
  oo <- outputObjects(module = modules, path = paths$modulePath)
  sim1 <- simInit(modules = modules, paths = paths)
  io1 <- inputObjects(sim1)
  oo1 <- outputObjects(sim1)
  expect_true(is.data.frame(io[[modules[[1]]]]))
  expect_true(is.data.frame(oo[[modules[[1]]]]))
  expect_true(length(io) == length(modules))
  expect_true(length(oo) == length(modules))

  out <- lapply(modules, function(m) {
    expect_true(all.equal(setDF(io[[m]][, -1]), io1[[m]][, -1]))
  })
})

test_that("test sped-up Caching of sequentially cached events", {
  testInit(sampleModReqdPkgs, opts = list(spades.allowSequentialCaching = TRUE))
  withr::local_options(list(reproducible.cachePath = tmpdir))

  defaults <- .coreModules() |> unname()
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects"),
                            nx = 100, ny = 100),
    fireSpread = list(.useCache = c("init", ".inputObjects")),
    caribouMovement = list(.useCache = c("init", ".inputObjects"))

  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = getSampleModules(tmpdir))

  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  et <- elapsedTime(mySimOut)
  mins <- "mins"
  et2 <- elapsedTime(mySimOut, units = mins)
  expect_is(et, "data.table")
  expect_identical(units(et2$elapsedTime), mins)
  expect_identical(colnames(et), c("moduleName", "eventType", "elapsedTime"))
  expect_false(any(grepl("Skipped digest", mess)))

  ## Rerun with Cached copies being recovered
  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  expect_true(sum(grepl("Skipped digest", mess)) == 2)

  ## If they are not sequential, shouldn't do it
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects")),
    caribouMovement = list(.useCache = c("init", ".inputObjects"))
  )
  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  expect_false(any(grepl("Skipped digest", mess)))

  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  expect_false(any(grepl("Skipped digest", mess)))

  ## If they are different sequential, should do it
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects")),
    fireSpread = list(.useCache = c("init", ".inputObjects"))
    # caribouMovement = list(.useCache = c("init", ".inputObjects"))
  )
  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  expect_false(sum(grepl("Skipped digest", mess)) == 1)

  mySim <- simInit(times, params, modules, objects = list(), paths)
  mess <- capture_messages({
    mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
  })
  expect_true(sum(grepl("Skipped digest", mess)) == 1)
})

test_that("test sped-up Caching of sequentially cached events", {
  testInit(sampleModReqdPkgs, opts = list(spades.allowSequentialCaching = TRUE))
  withr::local_options(list(reproducible.cachePath = tmpdir))

  defaults <- .coreModules() |> unname()
  times <- list(start = 1.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.useCache = c("init", ".inputObjects"),
                            nx = 100, ny = 100),
    fireSpread = list(.useCache = c("init", ".inputObjects")),
    caribouMovement = list(.useCache = c("init", ".inputObjects"))

  )
  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
  paths <- list(modulePath = getSampleModules(tmpdir))

  for (i in 2:3) {
    fn <- file.path(paths$modulePath, modules[i], paste0(modules[i], ".R"))
    xxx1 <- c(readLines(fn),
              ".inputObjects <- function(sim) {",
              "  a = asPath(file.path(inputPath(sim), \"test\")) ",
              paste0("  if (!suppliedElsewhere(", params$.globals$stackName, "))"),
              paste0("  sim[['", params$.globals$stackName, "']] <- sim[['", params$.globals$stackName, "']]"),
              "sim",
              "}")

    cat(xxx1, file = fn, sep = "\n")
  }

  ## Run first time to create the caches
  for (i in 1:2) {
    mess1 <- capture_messages({
      mySim <- simInit(times, params, modules, objects = list(), paths, debug = 1)
    })
    mess <- capture_messages({
      mySimOut <- spades(mySim, debug = 1, .plotInitialTime = NA)
    })
    if (i == 1) {
      expect_equal(sum(grepl("Skipped digest", mess)), 0)
      expect_equal(sum(grepl("Skipped digest", mess1)), 0)
    } else {
      expect_equal(sum(grepl("Skipped digest", mess)), 3)
      expect_equal(sum(grepl("Skipped digest", mess1)), 1) # there is no .inputObjects for randomLandscapes
    }
  }
})
