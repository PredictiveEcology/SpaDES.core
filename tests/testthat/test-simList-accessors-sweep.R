## A sweep over the simList accessors and their replacement counterparts.
## Deliberately table-driven: these are many small methods, each with only a
## line or two of body, and covering them one test_that() at a time would be
## mostly boilerplate.

test_that("path accessors round-trip through their replacement methods", {
  testInit()

  sim <- simInit()

  pathAccessors <- c("cachePath", "outputPath", "rasterPath", "scratchPath",
                     "terraPath", "modulePath", "inputPath")

  for (a in pathAccessors) {
    getter <- get(a)
    setter <- get(paste0(a, "<-"))

    orig <- getter(sim)
    expect_true(is.character(orig), info = a)

    newVal <- file.path(tempdir(), paste0("swept-", a))
    sim <- setter(sim, value = newVal)

    expect_true(any(grepl(paste0("swept-", a), getter(sim))), info = a)
  }
})

test_that("paths() returns all the individual paths", {
  testInit()

  sim <- simInit()
  p <- paths(sim)

  expect_true(is.list(p))
  expect_true(all(c("modulePath", "inputPath", "outputPath", "cachePath") %in% names(p)))
  expect_identical(normPath(p$outputPath), normPath(outputPath(sim)))
})

test_that("paths<- sets the paths as a group", {
  testInit()

  sim <- simInit()
  newOut <- file.path(tempdir(), "sweptGroupOutputs")
  paths(sim) <- list(outputPath = newOut)

  expect_true(any(grepl("sweptGroupOutputs", outputPath(sim))))
})

test_that("checkpoint and progress scalars round-trip", {
  testInit()

  sim <- simInit()

  checkpointFile(sim) <- "chk.rds"
  expect_identical(basename(checkpointFile(sim)), "chk.rds")

  checkpointInterval(sim) <- 5
  expect_identical(as.numeric(checkpointInterval(sim)), 5)

  progressInterval(sim) <- 2
  expect_identical(as.numeric(progressInterval(sim)), 2)

  progressType(sim) <- "text"
  expect_identical(progressType(sim), "text")
})

test_that("globals and its G alias are the same accessor", {
  testInit()

  sim <- simInit()

  globals(sim) <- list(aGlobal = 42)
  expect_identical(globals(sim)$aGlobal, 42)
  expect_identical(G(sim)$aGlobal, 42)

  G(sim) <- list(aGlobal = 43)
  expect_identical(globals(sim)$aGlobal, 43)
})

test_that("objs and objs<- read and write the sim environment", {
  testInit()

  sim <- simInit()
  objs(sim) <- list(anObject = 1:3)

  expect_true("anObject" %in% names(objs(sim)))
  expect_identical(objs(sim)$anObject, 1:3)
  expect_identical(sim$anObject, 1:3)
})

test_that("envir and envir<- expose the sim environment", {
  testInit()

  sim <- simInit()
  expect_true(is.environment(envir(sim)))

  e <- new.env()
  e$fromNewEnv <- "yes"
  envir(sim) <- e
  expect_identical(sim$fromNewEnv, "yes")
})

test_that("times and its component accessors round-trip", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 10, timeunit = "year"))

  tt <- times(sim)
  expect_true(all(c("current", "start", "end", "timeunit") %in% names(tt)))

  end(sim) <- 20
  expect_identical(as.numeric(end(sim)), 20)

  start(sim) <- 2
  expect_identical(as.numeric(start(sim)), 2)

  time(sim) <- 3
  expect_identical(as.numeric(time(sim)), 3)

  timeunit(sim) <- "day"
  expect_identical(timeunit(sim), "day")
})

test_that("times<- sets all three times at once", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 10, timeunit = "year"))
  times(sim) <- list(current = 1, start = 1, end = 5, timeunit = "year")

  expect_identical(as.numeric(start(sim)), 1)
  expect_identical(as.numeric(end(sim)), 5)
})

test_that("events, current and completed expose the event queues", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim <- scheduleEvent(sim, 0, "someModule", "someEvent")

  expect_true(is.data.frame(events(sim)) || data.table::is.data.table(events(sim)))
  expect_true("someModule" %in% events(sim)$moduleName)

  expect_true(NROW(completed(sim)) == 0)

  cur <- current(sim)
  expect_true(NROW(cur) == 0 || is.data.frame(cur))
})

test_that("current<- sets the current event", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim <- scheduleEvent(sim, 0, "someModule", "someEvent")

  ## events() is priority-sorted, so pick our row explicitly rather than row 1
  ours <- events(sim)[events(sim)$moduleName == "someModule", ][1, ]
  current(sim) <- ours
  expect_true("someModule" %in% current(sim)$moduleName)
})

test_that("params and parameters expose module parameters", {
  testInit()

  sim <- simInit()

  params(sim) <- list(myMod = list(aParam = 7))
  expect_identical(params(sim)$myMod$aParam, 7)

  ## parameters() reports declared module metadata; with no modules loaded
  ## there is none, so it is NULL
  expect_null(parameters(sim))
})

test_that("P and P<- read and write a module's parameters", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  ## P.simList resolves the module directory, so this needs a real module
  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )

  allP <- P(sim, module = "randomLandscapes")
  expect_true("nx" %in% names(allP))
  expect_identical(P(sim, param = "nx", module = "randomLandscapes"), allP$nx)

  P(sim, param = "nx", module = "randomLandscapes") <- 42
  expect_identical(P(sim, param = "nx", module = "randomLandscapes"), 42)
})

test_that("inputs and outputs round-trip", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  expect_true(is.data.frame(inputs(sim)))
  expect_true(is.data.frame(outputs(sim)))

  f <- tempfile(fileext = ".rds")
  outputs(sim) <- data.frame(objectName = "anObject", file = f,
                             saveTime = 0, stringsAsFactors = FALSE)

  expect_true("anObject" %in% outputs(sim)$objectName)
})

test_that("inputArgs reports the arguments used to build the sim", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  expect_true(is.list(inputArgs(sim)) || is.null(inputArgs(sim)))
})

test_that("modules and depends expose the loaded modules", {
  testInit()

  sim <- simInit()
  expect_true(is.list(modules(sim)) || is.character(modules(sim)))
  expect_s4_class(depends(sim), ".simDeps")
})

test_that("conditionalEvents lists events scheduled on a condition", {
  testInit()

  sim <- simInit(times = list(start = 0, end = 5, timeunit = "year"))

  ## no conditional events yet
  expect_true(NROW(conditionalEvents(sim)) == 0)

  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) > 2),
                                  moduleName = "someModule",
                                  eventType = "conditionalEvent")

  expect_true(NROW(conditionalEvents(sim)) >= 1)
  expect_true("someModule" %in% conditionalEvents(sim)$moduleName)
})

test_that("elapsedTime summarises event timings after a run", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )
  out <- suppressMessages(spades(sim))

  byEvent <- elapsedTime(out)
  expect_true(NROW(byEvent) > 0)

  byModule <- elapsedTime(out, byEvent = FALSE)
  expect_true(NROW(byModule) > 0)
  expect_true(NROW(byModule) <= NROW(byEvent))

  inSecs <- elapsedTime(out, units = "secs")
  expect_true(NROW(inSecs) > 0)
})

test_that("moduleObjects reports a module's declared inputs and outputs", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)

  mo <- moduleObjects(module = "randomLandscapes", path = mp)

  expect_true(is.data.frame(mo))
  expect_true(NROW(mo) > 0)
  expect_true(all(c("objectName", "module", "type", "objectClass") %in% names(mo)))
  expect_setequal(unique(mo$type), c("input", "output"))
})

test_that("moduleObjects works from a simList too", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )

  mo <- moduleObjects(sim, module = "randomLandscapes")
  expect_true(is.data.frame(mo))
  expect_true(NROW(mo) > 0)
})

test_that("metadata accessors read a module's declared metadata", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )

  expect_true(is.list(reqdPkgs(sim)) || is.character(reqdPkgs(sim)))
  expect_true(is.list(documentation(sim)) || is.character(documentation(sim)) ||
                is.null(documentation(sim)))
  expect_true(is.list(citation(sim)) || is.character(citation(sim)) ||
                inherits(citation(sim), "citation"))
  expect_true(is.character(packages(sim)) || is.list(packages(sim)) ||
                is.null(packages(sim)))
})

test_that("reqdPkgs can be read straight from a module on disk", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  rp <- reqdPkgs(module = "randomLandscapes", modulePath = mp)

  expect_true(is.list(rp) || is.character(rp))
})
