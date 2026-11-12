## saveSimList() keeps each module's source text in sim$.mods[[module]]$._source, so a loaded
## simList runs where its modules are not on modulePath. simInit() records the modules it was
## given and the parent -> children map in sim$._modulesGiven. Neither reaches Cache.

## m1 uses a function from its R/ folder; `sim$y` shows which copy of the source ran
sourceModules <- function(mp, value = 42) {
  writeTestModule(mp, "m1", io = 'outputObjects = bindrows(createsOutput("y", "numeric", ""))',
                  events = 'if (eventType == "init") sim$y <- helperVal()\ninvisible(sim)')
  dir.create(file.path(mp, "m1", "R"), showWarnings = FALSE)
  writeLines(sprintf("helperVal <- function() %s", value), file.path(mp, "m1", "R", "helper.R"))
}

test_that("a saved simList carries each module's source text, equal to its files", {
  testInit()
  mp <- file.path(tmpdir, "modules")
  sourceModules(mp)
  sim <- simInit(times = list(start = 0, end = 1), modules = "m1", paths = list(modulePath = mp))
  f <- file.path(tmpdir, "sim.rds")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)

  src <- readRDS(f)@.xData$.mods$m1$._source
  rel <- c("m1.R", "R/helper.R")
  files <- file.path(mp, "m1", rel)
  expect_setequal(names(src$files), rel)
  expect_identical(src$files[rel], setNames(vapply(files, function(x) readChar(x, file.size(x), useBytes = TRUE),
                                                   character(1)), rel))
  expect_identical(src$version, "0.0.1")
  expect_identical(src$path, normPath(file.path(mp, "m1")))
  expect_null(src$git) # not a git checkout
  expect_identical(moduleSource(loadSimList(f, projectPath = tmpdir))$m1, src)
  ## saving does not change the caller's sim
  expect_null(sim$.mods$m1$._source)

  ## a git checkout: its commit and branch, read from .git
  sha <- strrep("a1", 20)
  dir.create(file.path(mp, "m1", ".git", "refs", "heads"), recursive = TRUE)
  writeLines("ref: refs/heads/dev", file.path(mp, "m1", ".git", "HEAD"))
  writeLines(sha, file.path(mp, "m1", ".git", "refs", "heads", "dev"))
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  expect_identical(readRDS(f)@.xData$.mods$m1$._source$git, list(sha = sha, ref = "dev"))
  ## a copied submodule: its .git file names a git dir that is not there
  unlink(file.path(mp, "m1", ".git"), recursive = TRUE)
  writeLines("gitdir: ../../.git/modules/m1", file.path(mp, "m1", ".git"))
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  expect_null(readRDS(f)@.xData$.mods$m1$._source$git)
})

test_that("a loaded simList whose modules are not on modulePath runs from the stored source", {
  testInit()
  mp <- file.path(tmpdir, "modules")
  sourceModules(mp)
  sim <- simInit(times = list(start = 0, end = 1), modules = "m1", paths = list(modulePath = mp))
  f <- file.path(tmpdir, "sim.rds")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  unlink(mp, recursive = TRUE)

  expect_message(loaded <- loadSimList(f, projectPath = tmpdir, verbose = 1),
                 "m1.*stored in the simList")
  loaded <- spades(loaded)
  expect_identical(loaded$y, 42)

  ## saved again, still without the module on modulePath: the stored text is kept
  f2 <- file.path(tmpdir, "sim2.rds")
  saveSimList(loaded, f2, projectPath = tmpdir, files = FALSE)
  expect_identical(readRDS(f2)@.xData$.mods$m1$._source, moduleSource(loaded)$m1)
})

test_that("the module on modulePath is used before the stored source", {
  testInit()
  mp <- file.path(tmpdir, "modules")
  sourceModules(mp)
  sim <- simInit(times = list(start = 0, end = 1), modules = "m1", paths = list(modulePath = mp))
  f <- file.path(tmpdir, "sim.rds")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  sourceModules(mp, value = 7) # the user's copy changed after the save

  loaded <- spades(loadSimList(f, projectPath = tmpdir))
  expect_identical(loaded$y, 7)
  expect_match(moduleSource(loaded)$m1$files[["R/helper.R"]], "42") # still inspectable
})

test_that("the stored source is not in Cache entries and does not change cache keys", {
  testInit()
  mp <- file.path(tmpdir, "modules")
  sourceModules(mp)
  sim <- simInit(times = list(start = 0, end = 1), modules = "m1", paths = list(modulePath = mp))
  f <- file.path(tmpdir, "sim.rds")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  loaded <- loadSimList(f, projectPath = tmpdir)
  expect_false(is.null(moduleSource(loaded)$m1))

  without <- loadSimList(f, projectPath = tmpdir)
  rm("._source", envir = without$.mods$m1)
  expect_identical(.robustDigest(loaded), .robustDigest(without))

  out <- Cache(spades, loaded, cachePath = tmpCache)
  entry <- loadFromCache(tmpCache, cacheId = gsub("cacheId:", "", attr(out, "tags")))
  expect_null(entry$.mods$m1$._source)
  expect_false(any(vapply(as.list(entry$.mods$m1, all.names = TRUE), is.function, logical(1))))
})

test_that("simInit records the modules it was given and each parent's children; a save keeps them", {
  testInit()
  mp <- file.path(tmpdir, "modules")
  writeTestModule(mp, "c1")
  writeTestModule(mp, "c2")
  writeTestModule(mp, "par", children = c("c1", "c2"))
  given <- c("Acct/par@dev" = "par")
  sim <- simInit(times = list(start = 0, end = 1), modules = given, paths = list(modulePath = mp))
  expect_identical(sim$._modulesGiven$modules, given)
  expect_identical(sim$._modulesGiven$parents, list(par = c("c1", "c2")))

  f <- file.path(tmpdir, "sim.rds")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  shell <- loadSimList(f, projectPath = tmpdir, parse = FALSE)
  expect_identical(shell$._modulesGiven, sim$._modulesGiven)
  expect_setequal(names(moduleSource(shell)), c("c1", "c2")) # a parent has no code to run
})
