test_that("simulation runs with simInit and spades with set.seed; events arg", {
  skip_on_cran() # too long
  testInit(sampleModReqdPkgs)

  set.seed(42)

  times <- list(start = 0.0, end = 1, timeunit = "year")
  params <- list(
  #   .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .seed = list("init" = 321)),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", #"caribouMovement",
                  "fireSpread")
  paths <- list(modulePath = getSampleModules(tmpdir))

  set.seed(123)
  mySim <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA)
  set.seed(123)
  mySim2 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA)

  ## simtime
  expect_equivalent(time(mySim), 1.0)
  expect_equivalent(start(mySim), 0.0)
  expect_equivalent(end(mySim), 1.0)
  expect_true(all.equal(mySim2, mySim))

  # Test events argument in spades call
  expect_true(!all("init" == completed(mySim)$eventType))
  expect_true(max(events(mySim)$eventTime) > end(mySim)) # didn't schedule next event
  expect_true(all("stats" %in% completed(mySim)$eventType))

  set.seed(123)
  mySimEvent <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = "init")
  expect_true(all(c(".inputObjects", "init") %in% completed(mySimEvent)$eventType))
  expect_true(max(events(mySimEvent)$eventTime) <= end(mySimEvent)) # didn't schedule next event


  eventTypes <- c("init", "burn")
  mySimEvent2 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes)
  expect_true(all(eventTypes %in% completed(mySimEvent2)$eventType))

  eventTypes <- c()
  mySimEvent3 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes)
  expect_true(all(eventTypes %in% completed(mySimEvent3)$eventType))
  expect_true(identical(completed(mySimEvent3)[, 1:4], completed(mySim)[, 1:4]))

  eventTypes <- c("nothing")
  mySimEvent4 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes)
  expect_true(NROW(completed(mySimEvent4)) == length(modules)) # only .inputObjects completed
  expect_true(all(c(".inputObjects", "init") %in% completed(mySimEvent)$eventType))

  eventTypes <- list(randomLandscapes = c("init"),
                     fireSpread = c("init", "burn")
  )
  mySimEvent5 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes)
  expect_true(all(unique(unlist(eventTypes)) %in% completed(mySimEvent5)$eventType))
  expect_true(!all("stats" %in% completed(mySimEvent5)$eventType))

  # Typo or "run events that aren't there"
  eventTypes <- list(randomLandscapes = c("init"),
                     fireSpread = c("initial", "burn")
  )
  mySimEvent6 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes)
  expect_true(all("randomLandscapes" %in% completed(mySimEvent6)$moduleName))
  expect_true(sum("fireSpread" %in% completed(mySimEvent6)$moduleName) == 1) # only .inputObjects; didn't run any fireSpread events b/c misspelled
  expect_true(all("fireSpread" %in% events(mySimEvent6)$moduleName)) # didn't run any fireSpread events b/c misspelled

  mySimEvent7 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes, cache = TRUE)
  compped <- completed(mySimEvent7)
  compped <- compped[!compped$eventType %in% ".inputObjects"]
  expect_true(all("randomLandscapes" %in% compped$moduleName))
  expect_true(!all("fireSpread" %in% compped$moduleName)) # didn't run any fireSpread events b/c misspelled
  expect_true(all("fireSpread" %in% events(mySimEvent7)$moduleName)) # didn't run any fireSpread events b/c misspelled

  mySimEvent8 <- simInit(times, params, modules, objects = list(), paths) |>
    spades(debug = FALSE, .plotInitialTime = NA, events = eventTypes, cache = TRUE)
  compped <- completed(mySimEvent8)
  compped <- compped[!compped$eventType %in% ".inputObjects"]
  expect_true(all("randomLandscapes" %in% compped$moduleName))
  expect_true(!all("fireSpread" %in% compped$moduleName)) # didn't run any fireSpread events b/c misspelled
  expect_true(all("fireSpread" %in% events(mySimEvent8)$moduleName)) # didn't run any fireSpread events b/c misspelled

  mySimEvent9 <- simInitAndSpades(times, params, modules, objects = list(), paths,
                        debug = FALSE, .plotInitialTime = NA, events = "init")
  compped <- completed(mySimEvent9)
  compped <- compped[!compped$eventType %in% ".inputObjects"]
  expect_true(all("init" == compped$eventType))
  expect_true(max(events(mySimEvent9)$eventTime) <= end(mySimEvent9)) # didn't schedule next event

  # Test times
  #  Set end time to WAY after the init events
  mySimEvent10 <- simInitAndSpades(times = list(start = 0, end = 10), params, modules, objects = list(), paths,
                                  debug = FALSE, .plotInitialTime = NA, events = "init")
  compped <- completed(mySimEvent10)
  compped <- compped[!compped$eventType %in% ".inputObjects"]
  expect_true(time(mySimEvent10) == end(mySimEvent10)) # it is at 10, the end
  expect_true(all("init" == compped$eventType))
  expect_true(max(compped$eventTime) == start(mySimEvent10)) # didn't go past start time because init are all at start
  simOut <- spades(mySimEvent10)
  expect_true(time(simOut) == end(simOut)) # it is at 10, the end
  expect_true(!all("init" == completed(simOut)$eventType))
  expect_true(max(completed(simOut)$eventTime) == end(simOut)) # got to end time

  mySimEvent11 <- simInit(times = list(start = 2000, end = 2010), params, modules,
                          objects = list(), paths,
                          # events = "init",
                          outputs = data.frame(objectName = "landscape", saveTime = 2000:2010,
                                               eventPriority = 1))
  mess <- capture_messages({
    mySimEvent11Out <- spades(Copy(mySimEvent11), event = list(randomLandscapes = "init"))
  })
  expect_true(any(grepl("not specified", mess)))
  expect_true(all(file.exists(outputs(mySimEvent11Out)$file[outputs(mySimEvent11Out)$saved])))

  # Now with data.table
  mySimEvent12 <- simInit(times = list(start = 2000, end = 2010), params, modules,
                          objects = list(), paths,
                          # events = "init",
                          outputs = data.table(objectName = "landscape", saveTime = 2000:2010,
                                               eventPriority = 1))
  mess <- capture_messages({
    mySimEvent12Out <- spades(Copy(mySimEvent12), event = list(randomLandscapes = "init"))
  })
  expect_true(any(grepl("not specified", mess)))

  expect_true(all(file.exists(outputs(mySimEvent12Out)$file[outputs(mySimEvent12Out)$saved])))
})

# test_that("spades calls - diff't signatures", {
#   testInit(sampleModReqdPkgs, verbose = TRUE)
#
#   a <- simInit()
#   a1 <- Copy(a)
#   opts <- options(spades.saveSimOnExit = FALSE)
#   expect_message(spades(a, debug = TRUE), "eventTime")
#   expect_silent(expect_message(spades(a, debug = FALSE), "DTthreads"))
#   expect_silent(expect_message(spades(a, debug = FALSE, .plotInitialTime = NA), "DTthreads"))
#   expect_silent(expect_message(spades(a, debug = FALSE, .saveInitialTime = NA), "DTthreads"))
#   opts <- options(opts)
#   expect_message(spades(a, debug = TRUE, .plotInitialTime = NA), "eventTime")
#   expect_message(spades(a, debug = TRUE, .saveInitialTime = NA), "eventTime")
#   expect_equivalent(capture_output(spades(a, debug = "current", .plotInitialTime = NA)),
#                     capture_output(spades(a, debug = TRUE, .plotInitialTime = NA)))
#
#   if (requireNamespace("logging", quietly = TRUE)) {
#     expect_message(spades(Copy(a), debug = list(debug = list("current", "events")), .plotInitialTime = NA),
#         "eventTime *moduleName *eventType *eventPriority")
#   } else {
#     expect_message(spades(Copy(a), debug = list(debug = list("current", "events")), .plotInitialTime = NA),
#                    "eventTime *moduleName *eventType *eventPriority")
#   }
#   expect_message(spades(a, debug = c("current", "events"), .plotInitialTime = NA), "moduleName")
#   expect_message(spades(a, debug = "simList", .plotInitialTime = NA), "Completed Events")
#
#   if (interactive()) {
#     # warnings occur on Rstudio-server related to can't use display 0:, when using devtools::test() interactively
#     suppressWarnings(expect_output(spades(a, progress = "text", debug = TRUE), "10%"))
#     suppressWarnings(expect_output(spades(a, progress = "text", debug = TRUE), "20%"))
#     suppressWarnings(expect_output(spades(a, progress = "text"), "..........| 100%"))
#   }
#   opts <- options(spades.saveSimOnExit = FALSE)
#   expect_silent(expect_message(spades(a, debug = FALSE, progress = FALSE), "DTthreads"))
#   expect_silent(expect_message(spades(a, debug = FALSE, progress = "rr"), "DTthreads"))
#   opts <- options(opts)
#
#   paths(a)$cachePath <- file.path(tempdir(), "cache") |> checkPath(create = TRUE)
#   a <- Copy(a1)
#   expect_message(spades(a, cache = TRUE, debug = TRUE, notOlderThan = Sys.time()), "eventTime")
#   expect_true(all(basename2(c(CacheDBFile(paths(a)$cachePath), CacheStorageDir(paths(a)$cachePath))) %in%
#                     dir(paths(a)$cachePath)))
#   file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))
#
#   # test for system time ... in this case, the first time through loop is slow
#   #   because of writing cache to disk, not because of spades being slow.
#   #   simList is empty.
#
#   set.seed(42)
#
#   times <- list(start = 0.0, end = 0, timeunit = "year")
#   params <- list(
#     .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
#     randomLandscapes = list(nx = 20, ny = 20)
#   )
#   modules <- list("randomLandscapes", "fireSpread")
#   paths <- list(modulePath = getSampleModules(tmpdir))
#
#   for (i in 1:2) {
#     a <- simInit(times, params, modules, paths = paths)
#     paths(a)$cachePath <- file.path(tempdir(), "cache") |> checkPath(create = TRUE)
#     assign(paste0("st", i), system.time(spades(a, cache = TRUE, .plotInitialTime = NA)))
#   }
#   params1 <- list(
#     .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
#     randomLandscapes = c(nx = 20, ny = 20)
#   )
#   expect_error({ a <- simInit(times, params1, modules, paths = paths) })
#   expect_error({ a <- simInit(list(3, "a", "s"), params, modules, paths = paths) })
#   err <- capture_error({
#     a <- simInit(list(3, "years", start = 1), params, modules, paths = paths)
#   })
#   expect_true(is.null(err))
#
#   #expect_gt(st1[1], st2[1]) ## no longer true on R >= 3.5.1 ??
#   file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))
# })
#
# test_that("simInit with R subfolder scripts", {
#   skip_if_not_installed("NLMR")
#
#   testInit()
#
#   newModule("child1", ".", open = FALSE)
#   cat(file = file.path("child1", "R", "script.R"),
#       "a <- function(poiuoiu) {
#       poiuoiu + 1
#   }", sep = "\n")
#   mySim <- simInit(modules = "child1", paths = list(modulePath = tmpdir))
#   expect_true(sum(grepl(unlist(lapply(ls(mySim@.xData$.mods, all.names = TRUE), function(x) {
#     if (is.environment(mySim@.xData$.mods[[x]])) ls(envir = mySim@.xData$.mods[[x]], all.names = TRUE)
#   })), pattern = "^a$")) == 1)
#   expect_true(mySim@.xData$.mods$child1$a(2) == 3) # Fns
# })
#
# test_that("simulation runs with simInit with duplicate modules named", {
#   testInit(sampleModReqdPkgs)
#
#   set.seed(42)
#
#   times <- list(start = 0.0, end = 10, timeunit = "year")
#   params <- list(
#     randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
#     caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE)
#   )
#   modules <- list("randomLandscapes", "randomLandscapes", "caribouMovement")
#   paths <- list(modulePath = getSampleModules(tmpdir))
#
#   expect_true(any(grepl(capture_messages({
#     mySim <- simInit(times, params, modules, objects = list(), paths)
#   }), pattern = "Duplicate module")))
#   expect_true(length(modules(mySim)) != length(modules))
#   expect_true(length(modules(mySim)) == length(unique(modules)))
# })
#
# test_that("simulation runs with simInit with duplicate modules named", {
#   skip("benchmarking DES")
#
#   testInit()
#
#   newModule("test", tmpdir, open = FALSE)
#   newModule("test2", tmpdir, open = FALSE)
#
#   sim <- simInit()
#
#   # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop
#   cat(file = file.path(tmpdir, "test", "test.R"), '
#       defineModule(sim, list(
#       name = "test",
#       description = "insert module description here",
#       keywords = c("insert key words here"),
#       authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
#       childModules = character(0),
#       version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
#       spatialExtent = terra::ext(rep(0, 4)),
#       timeframe = as.POSIXlt(c(NA, NA)),
#       timeunit = "second",
#       citation = list("citation.bib"),
#       documentation = list("README.md", "test.Rmd"),
#       reqdPkgs = list(),
#       parameters = rbind(
#       ),
#       inputObjects = bindrows(
#       ),
#       outputObjects = bindrows(
#       )
#       ))
#
#       doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
#       switch(
#       eventType,
#       init = {
#       sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
#       },
#       event1 = {
#       sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
#       })
#       return(invisible(sim))
#       }
#       ', fill = TRUE)
#
#   cat(file = file.path(tmpdir, "test2", "test2.R"), '
#       defineModule(sim, list(
#       name = "test2",
#       description = "insert module description here",
#       keywords = c("insert key words here"),
#       authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
#       childModules = character(0),
#       version = list(SpaDES.core = "0.1.0", test2 = "0.0.1"),
#       spatialExtent = terra::ext(rep(0, 4)),
#       timeframe = as.POSIXlt(c(NA, NA)),
#       timeunit = "second",
#       citation = list("citation.bib"),
#       documentation = list("README.md", "test2.Rmd"),
#       reqdPkgs = list(),
#       parameters = rbind(
#       ),
#       inputObjects = bindrows(
#       ),
#       outputObjects = bindrows(
#       )
#       ))
#
#       doEvent.test2 = function(sim, eventTime, eventType, debug = FALSE) {
#       switch(
#       eventType,
#       init = {
#       sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
#       },
#       event1 = {
#       sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
#       })
#       return(invisible(sim))
#       }
#       ', fill = TRUE)
#
#   N <- 5000
#
#   moduleDir <- file.path(tmpdir)
#   inputDir <- file.path(moduleDir, "inputs") |> checkPath(create = TRUE)
#   outputDir <- file.path(moduleDir, "outputs")
#   cacheDir <- file.path(outputDir, "cache")
#   times <- list(start = 0, end = N)
#   parameters <- list(
#   )
#   modules <- list("test")
#   objects <- list()
#   paths <- list(
#     cachePath = cacheDir,
#     modulePath = moduleDir,
#     inputPath = inputDir,
#     outputPath = outputDir
#   )
#
#   #options("spades.nCompleted" = 500)
#   mySim <- simInit(times = times, params = parameters, modules = modules,
#                    objects = objects, paths = paths)
#
#   nTimes <- 20
#
#   #######################
#   # Tested on laptop
#   #######################
#   # laptop was 10.2 seconds -- currently 4.2 seconds or so --> June 29, 2018 is 1.06 seconds
#   # laptop New with "seconds" -- Sept 21, 2018 is 0.492 seconds --> 98 microseconds/event
#   # laptop New with "seconds" -- Nov 26, 2018 is 0.458 seconds --> 92 microseconds/event!
#   # Windows Desktop -- slower -- Nov 26, 2018 0.730 Seconds --> 148 microseconds/event!
#   # Linux Server -- slower -- Nov 26, 2018 0.795 Seconds --> 159 microseconds/event!
#   # BorealCloud Server -- slower -- Nov 26, 2018 0.972 Seconds --> 194 microseconds/event!
#   # laptop -- May 25, 2019 0.603 Seconds --> 120 microseconds/event!
#   # laptop with new completed as environment -- May 25, 2019 0.357 Seconds --> 71 microseconds/event!
#   options("spades.keepCompleted" = TRUE)
#   # microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)})
#   #
#   # # Turn off completed list
#   # #  Changed to use "seconds" -- better comparison with simple loop
#   # # Old times using "year"  -- June 29, 2018 is 0.775 seconds, Sept 19, 2018 0.809 seconds
#   # #                         -- This is 161 microseconds per event
#   # # New times using "second" -- Sept 19, 2018 0.244 Seconds --> 49 microseconds/event
#   # # New times using "second" -- Nov 26, 2018 0.192 Seconds --> 38 microseconds/event!
#   # # Windows Desktop -- slower -- Nov 26, 2018 0.348 Seconds --> 70 microseconds/event!
#   # # Linux Server -- slower -- Nov 26, 2018 0.461 Seconds --> 92 microseconds/event!
#   # # BorealCloud Server -- slower -- Nov 26, 2018 0.282 Seconds --> 56 microseconds/event!
#   # # With many new "exists"
#   # # laptop -- May 25, 2019 0.264 Seconds --> 53 microseconds/event!
#   # options("spades.keepCompleted" = FALSE)
#   # (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
#   # #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})
#   #
#   # a <- 0
#   # a3 <- microbenchmark::microbenchmark(
#   #   for (i in 1:N) {
#   #     a <- a + 1
#   #   }
#   # )
#   #
#   # summary(a2)[, "median"]/summary(a3)[, "median"]
#   #
#   # ########################################
#   # # With 2 modules, therefore sorting
#   # ########################################
#   # modules <- list("test", "test2")
#   # mySim <- simInit(times = times, params = parameters, modules = modules,
#   #                  objects = objects, paths = paths)
#   #
#   # nTimes <- 10
#   # # Turn off completed list
#   # # New times using "second" -- Nov 26, 2018 0.443 Seconds --> 59 microseconds/event, even with sorting
#   # options("spades.keepCompleted" = FALSE)
#   # (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
#   # #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})
#   #
#   # # New times using "second" -- Nov 26, 2018 0.443 Seconds --> 130 microseconds/event, even with sorting
#   # options("spades.keepCompleted" = TRUE)
#   # (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
# })
#
# test_that("conflicting function types", {
#   testInit(sampleModReqdPkgs, smcc = TRUE)
#
#   m <- "child4"
#   newModule(m, tmpdir, open = FALSE)
#   fileName <- file.path(m, paste0(m, ".R")) # child4/child4.R"
#   xxx <- readLines(fileName)
#   lineWithInit <- grep(xxx, pattern = "^Init")
#
#   xxx1 <- gsub(xxx, pattern = 'plotFun', replacement = 'Plot') # nolint
#   cat(xxx1, file = fileName, sep = "\n")
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m), "Plot is defined")
#
#   # do functions like raster::levels
#   cat(xxx[1:lineWithInit], "
#       library(raster)
#       poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
#       poiuoiu <- poiuoiu
#       poiuoiu <- scale(poiuoiu)
#       poiuoiu <- ratify(poiuoiu)
#       rat <- raster::levels(poiuoiu)[[1]]
#
#       levels(poiuoiu) <- rat
#       ",
#       xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
#
#   fullMessage <- c("the following function\\(s\\) is used that", "raster::scale", "scale")
#   expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
#   nonMessage <- c("raster::levels", "levels")
#   expect_false(all(unlist(lapply(nonMessage, function(x) any(grepl(mm, pattern = x))))))
#
#   cat(xxx[1:lineWithInit], "
#       library(raster)
#       poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
#       poiuoiu <- scale(poiuoiu)
#       ",
#       xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m), "raster::scale")
#
#   cat(xxx[1:lineWithInit], "
#       library(raster)
#       poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
#       poiuoiu <- raster::scale(poiuoiu)
#       sim$poiuoiu <- poiuoiu
#       ",
#       xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m), "poiuoiu is assigned")
#
#   cat(xxx[1:(lineWithInit - 1)], "
#       a <- function(x) {
#       b <- b + 1
#       }
#       ",
#       xxx[(lineWithInit):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m), "a: parameter")
#
#   xxx1 <- gsub(xxx, pattern = "\\.plotInitialTime", replacement = "value")
#   xxx1 <- gsub(xxx1, pattern = "NA, NA, NA", replacement = "'hi', NA, NA")
#
#   cat(xxx1[1:lineWithInit], "
#       a <- sim$b
#       d <- sim$d
#       f <- sim[['f']]
#       f <- sim[[P(sim)$value]]
#       poiuoiu <- sim@.xData$d1
#       qwerqwer <- sim@.xData[['test']]
#       sim$g <- f
#       sim@.xData$g1 <- f
#       return(list(a, d, f, sim))
#       ",
#       xxx1[(lineWithInit + 1):length(xxx1)], sep = "\n", fill = FALSE, file = fileName)
#
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
#
#   fullMessage <- c(# "defineParameter: 'value' is not of specified type 'numeric'",
#                    "defineParameter: 'plotInterval' is not of specified type 'numeric'",
#                    "defineParameter: 'saveInitialTime' is not of specified type 'numeric'",
#                    "defineParameter: 'saveInterval' is not of specified type 'numeric'",
#                    "child4: module code: Init: local variable.*qwerqwer.*assigned but may not be used",
#                    "Running .inputObjects for child4",
#                    "child4: module code: Init: local variable.*poiuoiu.*assigned but may not be used",
#                    "child4: outputObjects: g, g1 are assigned to sim inside Init, but are not declared in metadata outputObjects",
#                    "child4: inputObjects: b, d, f, d1, test are used from sim inside Init, but are not declared in metadata inputObjects"
#   )
#
#   mm <- cleanMessage(mm)
#   expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
#
#   cat(xxx[1:lineWithInit], "
#       sim$child4 <- 1
#       ",
#       xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_error(simInit(paths = list(modulePath = tmpdir), modules = m),
#                c(paste0(m, ": You have created an object")))
#
#   # declared in metadata inputObjects
#   lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
#   cat(xxx[1:(lineWithInputObjects - 1)], "
#       expectsInput('a', 'numeric', '', '')
#       ",
#       xxx[(lineWithInputObjects + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
#                  c(paste0(m, ": module code: a is declared in metadata inputObjects")))
#
#   # declared in metadata outputObjects
#   lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
#   cat(xxx[1:(lineWithOutputObjects - 1)], "
#       createsOutput('b', 'numeric', '')
#       ",
#       xxx[(lineWithInputObjects + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
#                  c(paste0(m, ": module code: b is declared in metadata outputObjects")))
#
#   cat(xxx[1:(lineWithInputObjects - 1)], "
#       expectsInput('a', 'numeric', '', '')
#       ",
#       xxx[(lineWithInputObjects + 1):(lineWithOutputObjects - 1)],
#       "
#       createsOutput('b', 'numeric', '')
#       ",
#       xxx[(lineWithInputObjects + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
#   expect_true(all(grepl(mm,
#                         pattern = c(paste0(m, ": module code: b is declared in metadata outputObjects|",
#                                            "so not checking minimum package|",
#                                            m, ": module code: a is declared in metadata inputObjects|",
#                                            "Running .inputObjects|",
#                                            "Setting:|Paths set to:|",
#                                            "Using setDTthreads|",
#                                            m, ": using dataPath|", "Elapsed")))))
#
#   # assign to sim for functions like scheduleEvent
#   lineWithScheduleEvent <- grep(xxx, pattern = "scheduleEvent")[1]
#   xxx1 <- xxx
#   xxx1[lineWithScheduleEvent] <- sub(xxx[lineWithScheduleEvent], pattern = "sim <- scheduleEvent",
#                                      replacement = "scheduleEvent")
#   cat(xxx1, sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
#                  c(paste0(m, ": module code: scheduleEvent inside doEvent.child4 must")))
#
#   # Return sim in doEvent
#   patt <- "return\\(invisible\\(sim\\)\\)"
#   lineWithReturnSim <- grep(xxx, pattern = patt)[1]
#   xxx1 <- xxx
#   xxx1[lineWithReturnSim] <- sub(xxx[lineWithReturnSim], pattern = patt,
#                                  replacement = "return(invisible())")
#   cat(xxx1, sep = "\n", fill = FALSE, file = fileName)
#
#   expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
#                  c(paste0(m, ": module code: doEvent.", m, " must return")))
#
#   lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
#   lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
#   lineWithDotInputObjects <- grep(xxx, pattern = "\\.inputObjects")[1]
#   cat(xxx[1:(lineWithInputObjects - 1)], "
#       expectsInput('ei1', 'numeric', desc = 'This is a test with    spaces
#                     and EOL', ''),
#       expectsInput('ei2', 'numeric', '', ''),
#       expectsInput('ei3', 'numeric', '', ''),
#       expectsInput('ei4', 'numeric', '', 'test.com')
#       ",
#       xxx[(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
#       createsOutput('co1', 'numeric', ''),
#       createsOutput('co2', 'numeric', desc = 'This is a test with    spaces
#                     and EOL on the      createsOutputs'),
#       createsOutput('co3', 'numeric', ''),
#       createsOutput('co4', 'numeric', '')
#       ",
#       xxx[(lineWithOutputObjects + 1):lineWithInit], "
#       a <- sim$b
#       sim$g <- f
#       holy(sim$co4) <- f
#       moly(sim$aaa) <- f
#       fff <- sim$ei2
#       fff <- sim$co3
#       sim$co1 <- 123
#       xx <- c(1,2)
#       xx[sim$ei4] <- NA
#       ",
#       xxx[(lineWithInit + 1):lineWithDotInputObjects], "
#       a <- sim$b
#       url1 <- extractURL('ei4')
#       if (!identical(url1, 'test.com'))
#         stop('extractURL without sim or module fails')
#       url1 <- extractURL('ei4', sim = sim)
#       if (!identical(url1, 'test.com'))
#         stop('extractURL without module fails')",
# paste0("      url1 <- extractURL('ei4', sim = sim, module = \"", m, "\")") ,"
#       if (!identical(url1, 'test.com'))
#         stop('extractURL fails')
#       sim$g <- 1
#       sim$ei1 <- 4
#       fff <- sim$ei1
#       fff <- sim$co3
#       sim$co1 <- 123
#       aaa <- sim$.userSuppliedObjNames # in the ignoreObjects
#       ",
#       xxx[(lineWithDotInputObjects + 1):length(xxx)],
#       sep = "\n", fill = FALSE, file = fileName)
#
#   fullMessage <- c(
#     "Running .inputObjects for child4",
#     "child4: module code: co2, co3 are declared in metadata outputObjects, but are not assigned in the module",
#     "child4: module code: ei2, ei3, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
#     "child4: module code: ei3 is declared in metadata inputObjects, but is not used in the module",
#     "child4: module code: .inputObjects: local variable.*a.*assigned but may not be used",
#     "child4: module code: .inputObjects: local variable.*fff.*assigned but may not be used",
#     "child4: module code: Init: local variable.*a.*assigned but may not be used",
#     "child4: module code: Init: local variable.*fff.*assigned but may not be used",
#     "child4: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in metadata outputObjects",
#     "child4: inputObjects: g, co1 are assigned to sim inside .inputObjects, but are not declared in metadata inputObjects",
#     "child4: inputObjects: b, aaa are used from sim inside Init, but are not declared in metadata inputObjects",
#     "child4: inputObjects: b, co3 are used from sim inside .inputObjects, but are not declared in metadata inputObjects"
#   )
#
#   # Test moduleMetadata without `sim` and where there is a `sim` in the module metadata,
#   #   so needs to load it. A non-error is good enough for now.
#   md1 <- moduleMetadata(module = m, path = tmpdir) # no sim in metadata
#   md2 <- moduleMetadata(path = getSampleModules(tmpdir),
#                         module = "randomLandscapes")
#
#
#   mm <- capture_messages({
#     mySim <- simInit(paths = list(modulePath = tmpdir), modules = m)
#   })
#   mm <- cleanMessage(mm)
#   expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
#
#   x1 <- moduleMetadata(mySim)
#   sns <- slotNames(mySim@depends@dependencies[[m]])
#   names(sns) <- sns
#   x2 <- lapply(sns, function(sn) {
#     slot(mySim@depends@dependencies[[m]], sn)
#   })
#
#   # Now extra spaces are removed automatically on load ########################
#
#   # When there are more than a certain number of characters, a hidden \n gets inserted
#   #   Our metadata in tests is close to that, and some push past. No point diagnosing further. Accept 1 "TRUE"
#   expect_true(sum(unlist(lapply(x2, function(v) grepl("  |\n", v)))) <= 1)
#   x2 <- rmExtraSpacesEOLList(x2)
#   expect_true(sum(unlist(lapply(x1, function(v) grepl("  |\n", v)))) <= 1)
#   expect_true(sum(unlist(lapply(x2, function(v) grepl("  |\n", v)))) <= 1)
#
#   x1 <- moduleParams(m, dirname(dirname(fileName)))
#   expect_false(any(unlist(lapply(x1, function(v) grepl("  |\n", v)))))
#   x1 <- moduleInputs(m, dirname(dirname(fileName)))
#   expect_false(any(unlist(lapply(x1, function(v) grepl("  |\n", v)))))
#   x1 <- moduleOutputs(m, dirname(dirname(fileName)))
#   expect_false(any(unlist(lapply(x1, function(v) grepl("  |\n", v)))))
# })
#
# test_that("scheduleEvent with NA logical in a non-standard parameter", {
#   testInit("ggplot2", smcc = TRUE)
#   m <- "test"
#   newModule(m, tmpdir, open = FALSE)
#   fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
#   xxx <- readLines(fileName)
#   #lineWithInit <- grep(xxx, pattern = "^Init")
#
#   xxx1 <- gsub(xxx, pattern = '.plotInitialTime', replacement = '.plotInitialTim') # nolint
#   xxx2a <- grep(".plotInitialTim\\>", xxx1, value = TRUE)[1]
#   xxx2b <- gsub(",$", grep("time interval between plot", xxx1, value = TRUE), replacement = "")
#   xxx3 <- parse(text = paste(xxx2a, xxx2b))
#   # show that it is logical
#   sim <- simInit(times = list(start = 0, end = 2))
#   expect_true(is.numeric(eval(xxx3)$default[[1]]))
#
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
#   expect_true(all(unlist(lapply(c("Running .inputObjects", "module code appears clean"),
#                                 function(x) any(grepl(mm, pattern = x))))))
# })
#
# test_that("messaging with multiple modules", {
#   testInit("ggplot2", smcc = TRUE)
#
#   m1 <- "test"
#   m2 <- "test2"
#   m3 <- "test3"
#   m4 <- "test4"
#   m <- c(m1, m2, m3, m4)
#   newModule(m1, tmpdir, open = FALSE)
#   newModule(m2, tmpdir, open = FALSE)
#   newModule(m3, tmpdir, open = FALSE)
#   newModule(m4, tmpdir, open = FALSE)
#   #lapply(m, newModule, tmpdir, open = FALSE)
#   fileNames <- file.path(tmpdir, m, paste0(m, ".R"))
#   xxx <- lapply(fileNames, readLines)
#   set.seed(113)
#
#   lineWithInit <- grep(xxx[[1]], pattern = "^Init")
#   lineWithInputObjects <- grep(xxx[[1]], pattern = " expectsInput")
#   lineWithOutputObjects <- grep(xxx[[1]], pattern = " createsOutput")
#   lineWithDotInputObjects <- grep(xxx[[1]], pattern = "\\.inputObjects")[1]
#
#   xxx1 <- list()
#   #lapply(seq(m), function(yy) sample(c("character", "numeric", "logical"), size = 3, replace = TRUE))
#   xxx1[[1]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
#                     "\\.plotInitialTime\", \"character\", 1", xxx[[1]])
#   xxx1[[1]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
#                     "\\.saveInitialTime\", \"character\", FALSE", xxx1[[1]])
#   xxx1[[1]] <- gsub("\\.saveInterval\", \"numeric\", NA",
#                     "\\testtime\", \"logical\", NA_real_", xxx1[[1]])
#
#   xxx1[[2]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
#                     "\\.plotInitialTime\", \"character\", TRUE", xxx[[2]])
#   xxx1[[2]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
#                     "\\.saveInitialTime\", \"character\", 'c'", xxx1[[2]])
#   xxx1[[2]] <- gsub("\\.saveInterval\", \"numeric\", NA",
#                     "\\testtime\", \"character\", NA_real_", xxx1[[2]])
#
#   xxx1[[3]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
#                     "\\.plotInitialTime\", \"character\", 1", xxx[[3]])
#   xxx1[[3]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
#                     "\\hello\", \"character\", 1", xxx1[[3]])
#   xxx1[[3]] <- gsub("\\.saveInterval\", \"numeric\", NA",
#                     "\\testtime\", \"logical\", NA_real_", xxx1[[3]])
#   xxx1[[4]] <- xxx[[4]] # clean one
#
#   cat(xxx1[[1]][1:(lineWithInputObjects - 1)], "
#       expectsInput('ei1', 'numeric', '', ''),
#       expectsInput('ei2', 'numeric', '', ''),
#       expectsInput('ei3', 'numeric', '', ''),
#       expectsInput('ei4', 'numeric', '', '')
#       ",
#       xxx1[[1]][(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
#       createsOutput('co1', 'numeric', ''),
#       createsOutput('co2', 'numeric', ''),
#       createsOutput('co3', 'numeric', ''),
#       createsOutput('co4', 'numeric', '')
#       ",
#       xxx1[[1]][(lineWithInputObjects + 1):lineWithInit], "
#       a <- sim$b
#       sim$g <- f
#       holy(sim$co4) <- f
#       moly(sim$aaa) <- f
#       fff <- sim$ei2
#       fff <- sim$co3
#       sim$co1 <- 123
#       xx <- c(1,2)
#       xx[sim$ei4] <- NA
#       ",
#       xxx1[[1]][(lineWithInit + 1):lineWithDotInputObjects], "
#       a <- sim$b
#       sim$g <- 1
#       sim$ei1 <- 4
#       fff <- sim$ei1
#       fff <- sim$co3
#       sim$co1 <- 123
#       ",
#       xxx1[[1]][(lineWithDotInputObjects + 1):length(xxx1[[1]])],
#       sep = "\n", fill = FALSE, file = fileNames[1])
#
#
#   cat(xxx1[[2]][1:(lineWithInputObjects - 1)], "
#       expectsInput('ei1', 'numeric', '', ''),
#       expectsInput('ei4', 'numeric', '', '')
#       ",
#       xxx1[[2]][(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
#       createsOutput('co1', 'numeric', ''),
#       createsOutput('co4', 'numeric', '')
#       ",
#       xxx1[[2]][(lineWithInputObjects + 1):lineWithInit], "
#       a <- sim$b
#       xx <- c(1,2)
#       xx[sim$ei4] <- NA
#       ",
#       xxx1[[2]][(lineWithInit + 1):lineWithDotInputObjects], "
#       a <- sim$b
#       sim$co1 <- 123
#       ",
#       xxx1[[2]][(lineWithDotInputObjects + 1):length(xxx1[[2]])],
#       sep = "\n", fill = FALSE, file = fileNames[2])
#
#   fullMessage <- c(
#     # "defineParameter: 'plotInitialTime' is not of specified type 'character'",
#     "defineParameter: 'saveInitialTime' is not of specified type 'character'",
#     "Running .inputObjects for test",
#     "test: module code: co2, co3 are declared in metadata outputObjects, but are not assigned in the module",
#     "test: module code: ei2, ei3, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
#     "test: module code: ei3 is declared in metadata inputObjects, but is not used in the module",
#     "test: module code: .inputObjects: local variable.*a.*assigned but may not be used",
#     "test: module code: .inputObjects: local variable.*fff.*assigned but may not be used",
#     "test: module code: Init: local variable.*a.*assigned but may not be used",
#     "test: module code: Init: local variable.*fff.*assigned but may not be used",
#     "test: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in metadata outputObjects",
#     "test: inputObjects: g, co1 are assigned to sim inside .inputObjects, but are not declared in metadata inputObjects",
#     "test: inputObjects: b, aaa are used from sim inside Init, but are not declared in metadata inputObjects",
#     "test: inputObjects: b, co3 are used from sim inside .inputObjects, but are not declared in metadata inputObjects",
#     # "defineParameter: 'plotInitialTime' is not of specified type 'character'",
#     "Running .inputObjects for test2",
#     "test2: module code: co1, co4 are declared in metadata outputObjects, but are not assigned in the module",
#     "test2: module code: ei1, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
#     "test2: module code: ei1 is declared in metadata inputObjects, but is not used in the module",
#     "test2: module code: .inputObjects: local variable.*a.*assigned but may not be used",
#     "test2: module code: Init: local variable.*a.*assigned but may not be used",
#     "test2: inputObjects: co1 is assigned to sim inside .inputObjects, but is not declared in metadata inputObjects",
#     "test2: inputObjects: b is used from sim inside Init, but is not declared in metadata inputObjects",
#     "test2: inputObjects: b is used from sim inside .inputObjects, but is not declared in metadata inputObjects",
#     # "defineParameter: 'plotInitialTime' is not of specified type 'character'",
#     "defineParameter: 'hello' is not of specified type 'character'",
#     "Running .inputObjects for test3",
#     "test3: module code appears clean",
#     "Running .inputObjects for test4",
#     "test4: module code appears clean"
#   )
#
#   for (y in 3:4) {
#     cat(xxx1[[y]], sep = "\n", fill = FALSE, file = fileNames[y])
#   }
#   withr::local_options(spades.allowInitDuringSimInit = FALSE)
#   mm1 <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = as.list(m)))
#   mm1 <- cleanMessage(mm1)
#   expect_true(all(unlist(lapply(fullMessage,
#                                 function(x) any(grepl(mm1, pattern = x))))))
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = as.list(m)))
#   mm <- cleanMessage(mm)
# })
#
# test_that("Module code checking -- pipe with matrix product with backtick & data.table", {
#   testInit("ggplot2", smcc = TRUE)
#
#   m <- "child4"
#   newModule(m, tmpdir, open = FALSE)
#   fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
#   xxx <- readLines(fileName)
#   lineWithInit <- grep(xxx, pattern = "^Init")
#   xxx1 <- xxx
#   cat(xxx[1:lineWithInit], "
#       checksums1 <- structure(list(result = c('OK', 'OK'),
#       expectedFile = c('Land_Cover_2010_TIFF.zip','NA_LandCover_2010_25haMMU.tif'),
#       actualFile = c('Land_Cover_2010_TIFF.zip', 'NA_LandCover_2010_25haMMU.tif'),
#       checksum.x = c('f4f647d11f5ce109', '6b74878f59de5ea9'),
#       checksum.y = c('f4f647d11f5ce109', '6b74878f59de5ea9'),
#       algorithm.x = c('xxhash64', 'xxhash64'),
#       algorithm.y = c('xxhash64', 'xxhash64'),
#       renamed = c(NA, NA),
#       module = c('simplifyLCCVeg',  'simplifyLCCVeg')),
#       .Names = c('result', 'expectedFile', 'actualFile',
#       'checksum.x', 'checksum.y', 'algorithm.x', 'algorithm.y', 'renamed',
#       'module'),
#       row.names = c(NA, -2L),
#       class = c('grouped_df', 'tbl_df', 'tbl', 'data.frame'),
#       vars = 'expectedFile',
#       indices = list(0L, 1L),
#       group_sizes = c(1L, 1L),
#       biggest_group_size = 1L,
#       labels = structure(list(expectedFile = c('Land_Cover_2010_TIFF.zip', 'NA_LandCover_2010_25haMMU.tif')),
#       .Names = 'expectedFile',
#       row.names = c(NA, -2L),
#       class = 'data.frame', vars = 'expectedFile'))
#
#       result1 <- checksums1[checksums1$expectedFile == 'NA_LandCover_2010_25haMMU.tif',]$result
#
#       sim$bvcx <- matrix(1:2) %>% `%*%` (2:3)
#       sim$bvcx2 <- matrix(1:2) %>% \"%*%\" (2:3)
#       sim$b <- matrix(1:2) %>% t()
#
#       sim$a <- 1
#       ",
#       xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)
#
#   mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
#   mm <- cleanMessage(mm)
#
#   fullMessage1 <- c(
#     "Running .inputObjects for child4",
#     "child4: module code: Init: local variable.*result1.*assigned but may not be used ",
#     "child4: outputObjects: bvcx, bvcx2, b, a are assigned to sim inside Init, but are not declared in metadata outputObjects")
#   fullMessageNonInteractive <- c(
#     "Running .inputObjects for child4",
#     "child4: module code: Init", cantCodeCheckMessage, "'sim\\$bvcx <- matrix.*", #possibly at .*147",
#     "child4: module code: Init", cantCodeCheckMessage, "'sim\\$bvcx2 <- matrix.*", #possibly at .*148",
#     "child4: module code: Init: local variable.*result1.*assigned but may not be used",
#     "child4: outputObjects: b, a are assigned to sim inside Init, but are not declared in metadata outputObjects"
#   )
#   test1 <- all(unlist(lapply(fullMessage1, function(x) any(grepl(mm, pattern = x)))))
#   test2 <- all(unlist(lapply(fullMessageNonInteractive, function(x) any(grepl(mm, pattern = x)))))
#   # if (grepl( "emcintir", Sys.info()["user"])) {
#   #   tmpFilename = "c:/Eliot/tmp/test1.txt"
#   #
#   #   cat("################### test1\n", file = tmpFilename, append = FALSE)
#   #   cat(paste(collapse = " ", lapply(fullMessage1, function(x) any(grepl(mm, pattern = x)))), file = tmpFilename, append = TRUE)
#   #   cat("\n################### test2\n", file = tmpFilename, append = TRUE)
#   #   cat(paste(collapse = " ", lapply(fullMessageNonInteractive, function(x) any(grepl(mm, pattern = x)))), file = tmpFilename, append = TRUE)
#   #   cat("\n################### fullMessage1\n", file = tmpFilename, append = TRUE)
#   #   cat(paste(collapse = "\n", fullMessage1), file = tmpFilename, append = TRUE)
#   #   cat("\n################### fullMessageNonInteractive\n", file = tmpFilename, append = TRUE)
#   #   cat(paste(collapse = "\n", fullMessageNonInteractive), file = tmpFilename, append = TRUE)
#   #   cat("\n###################  mm\n", file = tmpFilename, append = TRUE)
#   #   cat(paste(collapse = "\n", mm), file = tmpFilename, append = TRUE)
#   # }
#   expect_true(test1 || test2)
# })
#
# test_that("simInitAndSpades", {
#
#   testInit(sampleModReqdPkgs)
#
#   set.seed(42)
#
#   times <- list(start = 0.0, end = 0, timeunit = "year")
#   params <- list(
#     .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
#     randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
#     caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
#     fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
#   )
#   modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
#   paths <- list(modulePath = getSampleModules(tmpdir))
#   set.seed(123)
#   mySim <- simInitAndSpades(times = times, params = params,
#                             modules = modules, objects = list(), paths = paths, debug = FALSE)
#
#   set.seed(123)
#   mySim2 <- simInit(times = times, params = params,
#                     modules = modules, objects = list(), paths = paths) |>
#     spades(debug = FALSE)
#
#   expect_true(all.equal(mySim, mySim2))
#
# })
#
# test_that("scheduleEvent with invalid values for eventTime", {
#   testInit()
#   s <- simInit(times = list(start = 1, end = 10))
#   expect_error({
#     s <- scheduleEvent(s, eventTime = -1, eventType = "test1", moduleName = "test")
#   })
#   expect_warning({
#     s <- scheduleEvent(s, eventTime = numeric(), eventType = "test1", moduleName = "test")
#   })
#   expect_error({
#     s <- scheduleEvent(s, eventTime = 0, eventType = "test1", moduleName = "test")
#   })
# })
#
# test_that("debug using logging", {
#
#   testInit(c(sampleModReqdPkgs, "logging"), tmpFileExt = "log")
#
#   set.seed(42)
#
#   times <- list(start = 0.0, end = 1, timeunit = "year")
#   params <- list(
#     .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
#     randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA, .useCache = "init"),
#     caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
#     fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
#   )
#   modules <- list("randomLandscapes")
#   paths <- list(modulePath = getSampleModules(tmpdir))
#
#   set.seed(123)
#   mySim <- simInit(times, params, modules, objects = list(), paths) #|>
#   logging::logReset()
#   unlink(tmpfile)
#   expect_false(file.exists(tmpfile))
#   mess1 <- capture_messages({
#     mess2 <- capture.output(type = "output", {
#       mySim2 <- spades(Copy(mySim),
#                        debug = list("console" = list(level = 10), debug = 1),
#                        .plotInitialTime = NA)
#     })
#   })
#   expect_false(any(grepl("total elpsd", mess1))) # using new mechanism console
#   expect_true(any(grepl("total elpsd", mess2)))
#   expect_true(any(grepl(Sys.Date(), mess2))) # the loginfo does have date
#   expect_false(any(grepl(Sys.Date(), mess1))) # original debug has date added
#
#   logging::logReset()
#   mess1 <- capture_messages({
#     mess2 <- capture.output(type = "output", {
#       mySim2 <- spades(Copy(mySim),
#                        debug = list("console" = list(level = 5),
#                                     "file" = list(file = tmpfile),
#                                     debug = 1),
#                        .plotInitialTime = NA)
#     })
#   })
#
#   expect_true(file.exists(tmpfile))
#   log1 <- readLines(tmpfile)
#   expect_true(any(grepl("total elpsd", log1)))
#   expect_true(any(grepl(Sys.Date(), log1)))
#   expect_false(any(grepl("total elpsd", mess1)))  # messages not produced with debug as list
#   unlink(tmpfile)
#
#   logging::logReset()
#   mess1 <- capture_messages({
#     mess2 <- capture.output(type = "output", {
#       mySim2 <- spades(Copy(mySim), debug = 1, .plotInitialTime = NA)
#     })
#   })
#   expect_false(file.exists(tmpfile))
#   expect_true(length(mess2) == 0)
#   expect_true(any(grepl("total elpsd", mess1)))
#   expect_true(any(grepl(format(Sys.Date(), "%h%d"), mess1))) # the straight messages don't have date
#
#   # Test whether suppressMessages works
#   mess1 <- capture_messages({
#     mess2 <- capture.output(type = "output", {
#       suppressMessages({
#         mySim2 <- spades(Copy(mySim),
#                          debug = list("console" = list(level = "INFO"), debug = 1),
#                          .plotInitialTime = NA)
#         })
#     })
#   })
#   expect_true(length(mess1) == 0)
#
#   # Test whether suppressMessages works
#   mess1 <- capture_messages({
#     mess2 <- capture.output(type = "output", {
#       suppressMessages({
#         mySim2 <- spades(Copy(mySim), debug = 1, .plotInitialTime = NA)
#       })
#     })
#   })
#   expect_true(length(mess1) == 0)
# })
