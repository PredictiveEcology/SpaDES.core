test_that("timeunit works correctly", {
  times <- list(start = 0.0, end = 10)
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  mySim <- simInit(times, params, modules, objects = list(), paths = paths)

  expect_equal(maxTimeunit(sim = mySim), "year")

  x1 <- list(
    name = "testModule",
    description = "this is a test.",
    keywords = c("test"),
    authors = c(person(c("Alex", "M"), "Chubaty",
                       email = "alexander.chubaty@canada.ca",
                       role = c("aut", "cre"))),
    version = list(testModule = "0.0.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = NA_character_,
    citation = list(),
    reqdPkgs = list("grid", "raster", "sp"),
    parameters = rbind(
      defineParameter("dummyVal", "numeric", 1.0, NA, NA, "vague description")
    ),
    inputObjects = dplyr::bind_rows(
      expectsInput(objectName = "testInput", objectClass = "list", sourceURL = "", desc = NA_character_)
    ),
    outputObjects = dplyr::bind_rows(
      createsOutput(objectName = "testOutput", objectClass = "list", desc = NA_character_)
    )
  )

  # Test for numerics, or character strings that are not recognized
  expect_message(timeunit(mySim) <- 1, "^unknown timeunit provided:")
  expect_message(timeunit(mySim) <- "LeapYear", "^unknown timeunit provided:")

  # Test for user defined timeunits, in .GlobalEnv
  expect_message(timeunit(mySim) <- "fortnight", "^unknown timeunit provided:")
  assign("dfortnight", function(x) lubridate::duration(dday(14)),
         envir = .GlobalEnv)
  expect_match(timeunit(mySim) <- "fortnight", "")
  expect_match(timeunit(mySim), "fortnight")
  expect_equivalent(as.numeric(dfortnight(1)), 1209600)
  rm(dfortnight, envir = .GlobalEnv)

  # check for new unit being put into simList
  assign("dfortnight", function(x) lubridate::duration(dday(14)),
         envir = envir(mySim))
  expect_match(timeunit(mySim) <- "fortnight", "")
  expect_match(timeunit(mySim), "fortnight")
  expect_equivalent(as.numeric(mySim$dfortnight(1)), 1209600)
  rm(dfortnight, envir = envir(mySim))

  # test that NA_real_ gets coerced to NA_character_
  timeunit(mySim) <- NA_real_
  expect_identical(timeunit(mySim), NA_character_)

  # check that the minTimeunit captures one of the timestepUnits in the loaded modules
  expect_true(
    any(match(minTimeunit(mySim),
              sapply(depends(mySim)@dependencies, function(x) {
                x@timeunit
              })
             )
       )
  )

  # check that minTimeunit finds the smallest timeunit of the modules loaded
  whNotNA <- sapply(depends(mySim)@dependencies,
                    function(x) !is.na(x@timeunit))
  expect_equivalent(as.numeric(eval(parse(
    text = paste0("d", minTimeunit(mySim), "(1)")))),
    min(sapply(depends(mySim)@dependencies[whNotNA],
               function(x) {
                 eval(parse(text = paste0("d", x@timeunit, "(1)")))
               }
    )))
  expect_equal(as.numeric(inSeconds(NA_character_)), 0)
  expect_equal(as.numeric(inSeconds(NULL)), 0)

  exampleTime <- 1:10
  attributes(exampleTime)$unit <- NA_character_
  expect_equal(as.numeric(convertTimeunit(exampleTime, "seconds")),
               rep(0, length(exampleTime)))

  exampleTime <- 1:10
  attributes(exampleTime)$unit <- "hour"
  expect_equal(as.numeric(convertTimeunit(exampleTime)), 1:10 * 3600)

  mySim <- simInit()
  expect_equal(maxTimeunit(mySim), NA_character_)

  expect_equal(c("year", "month", "week", "day", "hour", "second"),
                 spadesTimes())

  expect_equal(as.numeric(dNA()), 0)
  expect_equal(as.numeric(dhour(1)), 60 * 60)
  expect_equal(as.numeric(dday(1)), 60 * 60 * 24)
  expect_equal(as.numeric(dweeks(1)), 60 * 60 * 24 * 365.25 / 52)
  expect_equal(as.numeric(dweek(1)), 60 * 60 * 24 * 365.25 / 52)
})

test_that("timeunits with child and parent modules work correctly", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_timeunits") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  #suppressMessages({
    newModule("grandpar1", tmpdir, type = "parent", children = c("child1", "child2", "par1"), open = FALSE)
    newModule("par1", tmpdir, type = "parent", children = c("child4", "child3"), open = FALSE)
    newModule("child1", tmpdir, open = FALSE)
    newModule("child2", tmpdir, open = FALSE)
    newModule("child3", tmpdir, open = FALSE)
    newModule("child4", tmpdir, open = FALSE)
    newModule("child5", tmpdir, open = FALSE)
    #})

  fileName <- "child2/child2.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "day"') # nolint
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "child3/child3.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "week"') # nolint
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "child5/child5.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "second"') # nolint
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "par1/par1.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "month"') # nolint
  cat(xxx1, file = fileName, sep = "\n")

  mySim <- simInit(modules = list("grandpar1", "par1"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "month")

  # If only listing the one module and it is a parent, then use it regardless of whether
  #  it is shortest or longest
  mySim <- simInit(modules = list("grandpar1"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "year")

  fileName <- "grandpar1/grandpar1.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "hour"')
  cat(xxx1, file = fileName, sep = "\n")

  # If only listing the one module and it is a parent, then use it regardless of whether
  #  it is shortest or longest
  mySim <- simInit(modules = list("grandpar1"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "hour")

  mySim <- simInit(modules = list("grandpar1", "child5"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "second")

  suppressMessages(
    newModule("grandpar1", tmpdir, type = "parent", children = c("child1", "child2", "par1"), open = FALSE)
  )
  fileName <- "grandpar1/grandpar1.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = "timeunit = NA") # nolint
  cat(xxx1, file = fileName, sep = "\n")

  # If parent has NA for timeunit, then take smallest of children
  mySim <- simInit(modules = list("grandpar1"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "day")

  suppressMessages(
    newModule("grandpar2", tmpdir, type = "parent", children = c("child1", "child6", "par1"), open = FALSE)
  )
  fileName <- "grandpar2/grandpar2.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = "timeunit = NA") # nolint
  cat(xxx1, file = fileName, sep = "\n")


  ## child6 is used for further module testing
  suppressMessages(newModule("child6", tmpdir, open = FALSE))
  fileName <- file.path(tmpdir, "child6/child6.R")
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = "timeunit = NA") # nolint
  cat(xxx1, file = fileName, sep = "\n")

  # If parent has NA for timeunit, then take smallest of children
  mySim <- simInit(modules = list("grandpar2"), paths = list(modulePath = tmpdir))
  expect_equal(timeunit(mySim), "month") # because par1 is month, grandpar1 is NA


  ### Tese dataPath and currentModule function, which is namespaced
  xxx <- readLines(fileName)
  modName <- basename(dirname(fileName))
  initLine <- grep(xxx, pattern = "Init\\(sim\\)")
  xxx1 <- c(xxx[seq(initLine)], "  sim$dp <- dataPath(sim)",
            "  sim$cm <- currentModule(sim)", xxx[seq(length(xxx)-initLine)+initLine],
            "  cm1 <- currentModule(sim)", "  dp1 <- dataPath(sim)")
  cat(xxx1, file = fileName, sep = "\n")
  mySim <- simInit(modules = list(modName), paths = list(modulePath = tmpdir))
  expect_true(mySim[[modName]]$cm1 == file.path(modName))
  expect_true(mySim[[modName]]$dp1 == file.path(dirname(fileName), "data"))

  mySimOut <- spades(mySim)
  expect_true(mySimOut$dp == file.path(dirname(fileName), "data"))
  expect_true(mySimOut$cm == file.path(modName))


  ######
  theFile <- file.path(tmpdir, "test")
  write.table(x = data.frame(1), file = theFile)

  xxx <- readLines(fileName)
  modName <- basename(dirname(fileName))
  lineOfInterest1 <- tail(grep(xxx, pattern = "expectsInput"),1)
  xxx1 <- c(xxx[seq(lineOfInterest1-1)], "  expectsInput(\"b\", \"character\", \"temp thing\"),",
            xxx[seq(length(xxx)-lineOfInterest1)+lineOfInterest1])
  cat(xxx1, file = fileName, sep = "\n")

  lineOfInterest <- grep(xxx1, pattern = ".inputObjects <- ")
  xxx1 <- c(xxx1[seq(lineOfInterest - 1)], "  .inputObjects <- function(sim, a = asPath(file.path(inputPath(sim), \"test\"))) {",
            "  sim$b <- a",
            xxx1[seq(length(xxx1)-lineOfInterest)+lineOfInterest])
  cat(xxx1, file = fileName, sep = "\n")

  cacheDir <- file.path(tmpdir, "cache")
  try(clearCache(cacheDir), silent = TRUE)
  expect_silent(expect_message(mySim <- simInit(modules = list(modName),
                                  paths = list(modulePath = tmpdir, inputPath = tmpdir, cachePath = cacheDir),
                                  params = list("child6"= list(.useCache = ".inputObjects"))),
                               "Using or creating cached|child6 -- outputObjects: b, dp, cm are assigned")
  )

  # pulls cached value
  mm1 <- capture_messages(
    mySim <- simInit(modules = list(modName),
                     paths = list(modulePath = tmpdir, inputPath = tmpdir, cachePath = cacheDir),
                     params = list("child6"= list(.useCache = ".inputObjects")))
  )
  mm1 <- cleanMessage(mm1)
  fullMessage <- c("Using or creating cached copy of inputObjects for child6",
    "child6: module code: b is declared in inputObjects, but is not used in the module",
    "child6: outputObjects: dp, cm are assigned to sim inside doEventchild6, but are not declared in outputObjects"
  )
  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))

  expect_true(identical(mySim$b, asPath(theFile)))

  # Change the file that is in the arguments to .inputObjects
  write.table(x = data.frame(sample(1e6,1)), file = theFile)
  # Cache should force a rerun -- i.e., not cached value
  mm1 <- capture_messages(mySim <- simInit(modules = list(modName),
                                           paths = list(modulePath = tmpdir, inputPath = tmpdir, cachePath = cacheDir),
                                           params = list("child6"= list(.useCache = ".inputObjects"))))

  mm1 <- cleanMessage(mm1)
  fullMessage <- c("Using or creating cached copy of inputObjects for child6",
                   "child6: module code: b is declared in inputObjects, but is not used in the module",
                   "child6: outputObjects: dp, cm are assigned to sim inside doEventchild6, but are not declared in outputObjects"
  )
  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))

  # pulls cached value
  mm1 <- capture_messages(mm2 <- capture_output(mySim <- simInit(modules = list(modName),
                                           paths = list(modulePath = tmpdir, inputPath = tmpdir, cachePath = cacheDir),
                                           params = list("child6"= list(.useCache = ".inputObjects")))
  ))
  mm1 <- cleanMessage(mm1)
  fullMessage <- c("Using or creating cached copy of inputObjects for child6",
                   "child6: module code: b is declared in inputObjects, but is not used in the module",
                   "child6: outputObjects: dp, cm are assigned to sim inside doEventchild6, but are not declared in outputObjects"
  )
  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))
  expect_true(grepl("Using cached", mm2))


})
