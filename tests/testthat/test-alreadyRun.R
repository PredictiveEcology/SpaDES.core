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

test_that("an object the run did not save comes from its simList (end-of-run value); in neither, it stops", {
  testInit()
  a <- alreadyRunForTest(tmpdir)
  ## y was never saved as an output, but the run's simList has it (-8, its value at end 8)
  sim <- simInit(times = list(start = 3, end = 7), modules = "usesXY",
                 paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun)
  expect_equal(as.numeric(sim$y), -8)
  expect_true("y" %in% sim$.userSuppliedObjNames)
  expect_equal(sim$._alreadyRun$objects, "y")
  expect_equal(unique(inputs(sim)$objectName), "x")
  expect_false("y" %in% a$asked$files)
  ## not in the simList either: stops, naming it
  rm("y", envir = envir(a$alreadyRun$sim))
  expect_error(simInit(times = list(start = 3, end = 7), modules = "usesXY",
                       paths = list(modulePath = a$modulePath), alreadyRun = a$alreadyRun),
               "create y.*neither saved it.*nor kept it in its simList")
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

## simInit(alreadyRun = <several runs>): a module that summarises replicates (as NRV_summary and
## burnSummaries in mode "multi") gets every run's saved outputs as `outputsDF` and the objects that
## are the same in all runs. Three real runs of `makesXrep`, each in a bare-number replicate folder
## (runs/<r>, as pathBuild() writes them); run 2's files are "remote", behind `fetch`.
multiRunsForTest <- function(tmpdir, reps = 1:3) {
  mp <- file.path(tmpdir, "modules")
  writeTestModule(mp, "makesXrep",
    paste('inputObjects = bindrows(expectsInput("repId", "numeric", "")),',
          'outputObjects = bindrows(createsOutput("x", "numeric", ""), createsOutput("w", "character", ""),',
          '  createsOutput("z", "numeric", ""))'),
    paste('if (eventType == "init") { sim$w <- "same"; sim$z <- sim$repId',
          '  sim <- scheduleEvent(sim, start(sim), "makesXrep", "make") }',
          'if (eventType == "make") { sim$x <- time(sim) * sim$repId',
          '  sim <- scheduleEvent(sim, time(sim) + 1, "makesXrep", "make") }',
          'invisible(sim)', sep = "\n"))
  ## the summary: the last saved x of each replicate, read from the replicate's files
  writeTestModule(mp, "summarisesX",
    paste('inputObjects = bindrows(expectsInput("x", "numeric", ""), expectsInput("w", "character", ""),',
          '  expectsInput("z", "numeric", ""), expectsInput("sa", "character", ""),',
          '  expectsInput("outputsDF", "data.table", ""))'),
    paste('if (eventType == "init") {',
          '  files <- sim$outputsDF$file',
          '  dirs <- dirnamesFromSet(files, 1:9, leafOnly = FALSE)',
          '  sim$lastX <- vapply(dirs, function(d) {',
          '    f <- sort(grep(paste0("^", d, "/x_year"), files, value = TRUE))',
          '    readRDS(f[length(f)]) }, numeric(1), USE.NAMES = FALSE)',
          '  sim$wSeen <- sim$w }',
          'invisible(sim)', sep = "\n"))
  runs <- lapply(reps, function(r) {
    ## sa: given to each run by the user (as a study area is), no module creates it
    s <- simInit(times = list(start = 0, end = 8), modules = "makesXrep", objects = list(repId = r, sa = "A"),
                 paths = list(modulePath = mp, outputPath = file.path(tmpdir, "runs", r)),
                 outputs = data.frame(objectName = "x", saveTime = seq(0, 8, by = 2)))
    list(sim = spades(s))
  })
  ## run 2 is remote: its files are elsewhere until fetched
  o <- outputs(runs[[2]]$sim)
  remote <- file.path(tmpdir, "remote")
  dir.create(remote)
  file.rename(o$file, file.path(remote, basename(o$file)))
  asked <- new.env()
  runs[[2]]$fetch <- function(files) {
    asked$files <- c(asked$files, basename(files))
    file.copy(file.path(remote, basename(files)), files)
    files
  }
  list(runs = runs, asked = asked, modulePath = mp)
}

test_that("several runs: outputsDF spans them, objects equal in all are taken once, nothing is replayed", {
  testInit(debug = 1)
  a <- multiRunsForTest(tmpdir)
  mess <- capture_messages(
    sim <- simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                   paths = list(modulePath = a$modulePath, outputPath = file.path(tmpdir, "summary")),
                   alreadyRun = a$runs))
  expect_true(any(grepl("From the 3 runs of `alreadyRun`: w, sa, outputsDF; not taken.*x, z", mess)))
  ## no load events
  expect_equal(NROW(inputs(sim)), 0)
  ## every run's saved outputs, with the run's name; the remote run's files were fetched
  expect_equal(sort(unique(sim$outputsDF$run)), c("1", "2", "3"))
  expect_equal(NROW(sim$outputsDF), 15)
  expect_true(all(file.exists(sim$outputsDF$file)))
  expect_equal(sort(a$asked$files), sort(paste0("x_year", seq(0, 8, by = 2), ".rds")))
  ## w is "same" in every run; x (end-of-run 8, 16, 24) and z (1, 2, 3) differ
  expect_equal(sim$w, "same")
  expect_equal(sim$sa, "A") # in every run's simList, though no module of the runs created it
  expect_false(exists("x", envir = envir(sim), inherits = FALSE))
  expect_false(exists("z", envir = envir(sim), inherits = FALSE))
  expect_true(all(c("w", "outputsDF") %in% sim$.userSuppliedObjNames))
  expect_equal(sim$._alreadyRun$objects, c("w", "sa"))
  expect_true(sim$._alreadyRun$outputsDF)
  expect_equal(names(sim$._alreadyRun$runs), c("1", "2", "3"))
  expect_equal(sim$._alreadyRun$runs[["1"]]$modules, "makesXrep")
  expect_false("makesXrep" %in% unlist(modules(sim)))
  sim <- spades(sim)
  expect_equal(sim$lastX, c(8, 16, 24))
  expect_equal(sim$wSeen, "same")
  expect_false("makesXrep" %in% completed(sim)$moduleName)
})

test_that("several runs: names, user-supplied objects and a list of one run", {
  testInit()
  a <- multiRunsForTest(tmpdir)
  runs <- setNames(a$runs, c("a", "b", "c"))
  ## user-supplied objects, outputsDF among them, are not replaced
  odf <- data.table::data.table(file = character(0))
  sim <- simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = runs,
                 objects = list(w = "mine", outputsDF = odf))
  expect_equal(sim$w, "mine")
  expect_equal(NROW(sim$outputsDF), 0)
  expect_false(sim$._alreadyRun$outputsDF)
  expect_equal(names(sim$._alreadyRun$runs), c("a", "b", "c"))
  sim <- simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = runs)
  expect_equal(sort(unique(sim$outputsDF$run)), c("a", "b", "c"))
  ## a list of one run is a summary over one replicate, not a replay: z and x are taken
  sim <- simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = a$runs[3])
  expect_equal(NROW(inputs(sim)), 0)
  expect_equal(unique(sim$outputsDF$run), "1")
  expect_equal(sim$z, 3)
  sim <- spades(sim)
  expect_equal(sim$lastX, 24)
  ## NULL in one run (absent, or a lazy object that failed to load): not taken
  assign("w", NULL, envir = envir(runs$a$sim))
  sim <- simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                 paths = list(modulePath = a$modulePath), alreadyRun = runs)
  expect_false(exists("w", envir = envir(sim), inherits = FALSE))
  expect_equal(sim$sa, "A")
  ## neither a run nor a list of runs
  expect_error(simInit(times = list(start = 0, end = 1), modules = "summarisesX",
                       paths = list(modulePath = a$modulePath), alreadyRun = list(a$runs[[1]]$sim)),
               "must be one run")
})
