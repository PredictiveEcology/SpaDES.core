## simInit(alreadyRun = ): a new module receives a finished run's saved outputs at their save
## times, without that run's modules running. The run is a real run of `makesX`
## (x = time, saved every 2 years); `fetch` copies files from a "remote" folder and records
## what it was asked for.

alreadyRunForTest <- function(tmpdir) {
  mp <- file.path(tmpdir, "modules")
  writeTestModule(mp, "makesX",
    'outputObjects = bindrows(createsOutput("x", "numeric", ""), createsOutput("y", "numeric", ""))',
    paste('if (eventType == "init") sim <- scheduleEvent(sim, start(sim), "makesX", "make")',
          'if (eventType == "make") { sim$x <- time(sim); sim$y <- -time(sim)',
          '  sim <- scheduleEvent(sim, time(sim) + 1, "makesX", "make") }',
          'invisible(sim)', sep = "\n"))
  writeTestModule(mp, "usesX",
    'inputObjects = bindrows(expectsInput("x", "numeric", ""))',
    paste('if (eventType == "init") sim <- scheduleEvent(sim, start(sim), "usesX", "see")',
          'if (eventType == "see") { sim$seen <- c(sim$seen, setNames(sim$x, time(sim)))',
          '  sim <- scheduleEvent(sim, time(sim) + 1, "usesX", "see") }',
          'invisible(sim)', sep = "\n"))
  writeTestModule(mp, "usesXY",
    'inputObjects = bindrows(expectsInput("x", "numeric", ""), expectsInput("y", "numeric", ""))')
  arch <- simInit(times = list(start = 0, end = 8), modules = "makesX",
                  paths = list(modulePath = mp, outputPath = file.path(tmpdir, "archOut")),
                  outputs = data.frame(objectName = "x", saveTime = seq(0, 8, by = 2)))
  arch <- spades(arch)
  ## archived runs register outputs that modules wrote themselves: no objectName
  o <- outputs(arch)
  o$objectName <- NA_character_
  outputs(arch) <- o
  remote <- file.path(tmpdir, "remote")
  dir.create(remote)
  file.rename(o$file, file.path(remote, basename(o$file)))
  asked <- new.env()
  fetch <- function(files) {
    asked$files <- c(asked$files, basename(files))
    dir.create(unique(dirname(files)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(remote, basename(files)), files)
    files
  }
  list(alreadyRun = list(sim = arch, fetch = fetch, run = "makesXrun"), asked = asked, modulePath = mp)
}

test_that("alreadyRun = NULL changes nothing", {
  testInit()
  a <- alreadyRunForTest(tmpdir)
  sim <- simInit(times = list(start = 3, end = 7), modules = "usesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = NULL)
  expect_false(exists("alreadyRun", envir = envir(sim), inherits = FALSE))
  expect_null(sim$._alreadyRun)
  expect_equal(NROW(inputs(sim)), 0)
})

test_that("a new module gets the run's saves: the latest at or before start, then each in the window", {
  testInit()
  a <- alreadyRunForTest(tmpdir)
  sim <- simInit(times = list(start = 3, end = 7), modules = "usesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun)
  ## the save at 2 loads at start; 4 and 6 at their save times; 0 and 8 are not fetched
  expect_equal(inputs(sim)$objectName, rep("x", 3))
  expect_equal(as.numeric(inputs(sim)$loadTime), c(3, 4, 6))
  expect_equal(sort(a$asked$files), c("x_year2.rds", "x_year4.rds", "x_year6.rds"))
  expect_false("makesX" %in% unlist(modules(sim)))
  expect_equal(sim$._alreadyRun$run, "makesXrun")
  expect_equal(sim$._alreadyRun$modules, "makesX")
  sim <- spades(sim)
  expect_equal(sim$seen, c(`3` = 2, `4` = 4, `5` = 4, `6` = 6, `7` = 6))
  expect_false("makesX" %in% completed(sim)$moduleName)
})

test_that("an object the run's modules created but did not save stops, naming it", {
  testInit()
  a <- alreadyRunForTest(tmpdir)
  expect_error(simInit(times = list(start = 3, end = 7), modules = "usesXY",
                       paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun),
               "y.*must be saved")
  ## a user-supplied object is not taken from the run
  sim <- simInit(times = list(start = 3, end = 7), modules = "usesXY",
                 paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun,
                 objects = list(y = 1))
  expect_equal(unique(inputs(sim)$objectName[!is.na(inputs(sim)$file)]), "x")
  ## nothing to take from the run
  sim <- simInit(times = list(start = 3, end = 7), modules = "usesXY",
                 paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun,
                 objects = list(x = 1, y = 1))
  expect_true(all(is.na(inputs(sim)$file)))
  expect_equal(sim$._alreadyRun$modules, "makesX")
})
