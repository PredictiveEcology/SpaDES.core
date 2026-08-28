# simInit()/spades() advertise the simulation's project paths to reproducible via
# options(reproducible.fileBackedAnchors = paths(sim)), so that file-backed objects
# (e.g. terra SpatRaster) cached during a run are stored relative to a named,
# machine-independent anchor and can be restored on another machine/user (e.g. a
# shared cloud cache). An explicit user setting must always win, and the option
# must be restored after the run.

## minimal module that records the option value during .inputObjects and/or init.
## `dummy` is an unprovided expected input so that `.inputObjects` actually runs
## (simInit skips `.inputObjects` when all expected inputs are already supplied).
mkAnchMod <- function(tmpdir, name, dotIO = "sim", initBody = "sim") {
  newModule(name, tmpdir, open = FALSE)
  code <- sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, "")),
  inputObjects = bindrows(expectsInput("dummy", "ANY", "")), outputObjects = bindrows()))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") { %s }
  sim
}
.inputObjects <- function(sim) { %s; sim }
', name, name, name, initBody, dotIO)
  cat(code, file = file.path(tmpdir, name, paste0(name, ".R")), fill = TRUE)
}

test_that(".useFileBackedAnchors sets the option only when it is unset", {
  withr::local_options(reproducible.fileBackedAnchors = NULL)
  p <- list(cachePath = "/c", inputPath = "/i")

  expect_true(isTRUE(SpaDES.core:::.useFileBackedAnchors(p)))
  expect_identical(getOption("reproducible.fileBackedAnchors"), p)

  # already set -> no-op, returns FALSE, value preserved
  keep <- list(inputPath = "/keep")
  options(reproducible.fileBackedAnchors = keep)
  expect_false(isTRUE(SpaDES.core:::.useFileBackedAnchors(p)))
  expect_identical(getOption("reproducible.fileBackedAnchors"), keep)

  # unnamed / empty paths -> no-op
  options(reproducible.fileBackedAnchors = NULL)
  expect_false(isTRUE(SpaDES.core:::.useFileBackedAnchors(list())))
  expect_null(getOption("reproducible.fileBackedAnchors"))
})

test_that("simInit() and spades() set fileBackedAnchors from paths() and restore after", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache,
                       reproducible.fileBackedAnchors = NULL)

  mkAnchMod(tmpdir, "anchMod",
            dotIO = 'sim$ioAnchors <- getOption("reproducible.fileBackedAnchors")',
            initBody = 'sim$initAnchors <- getOption("reproducible.fileBackedAnchors")')

  sim <- simInit(modules = "anchMod",
                 paths = list(modulePath = tmpdir, cachePath = tmpCache),
                 times = list(start = 0, end = 1))

  # during .inputObjects (run inside simInit) the anchors were the sim's paths
  expect_false(is.null(sim$ioAnchors))
  expect_identical(sim$ioAnchors, paths(sim))
  # ...and the option is restored once simInit() returns
  expect_null(getOption("reproducible.fileBackedAnchors"))

  out <- spades(sim, debug = FALSE)

  # during the init event (run inside spades) the anchors were again the sim's paths
  expect_false(is.null(out$initAnchors))
  expect_identical(out$initAnchors, paths(sim))
  # ...and restored after spades() too
  expect_null(getOption("reproducible.fileBackedAnchors"))
})

test_that("an explicit reproducible.fileBackedAnchors is not overridden", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  userAnchors <- list(inputPath = "/my/custom/inputs", cachePath = tmpCache)
  withr::local_options(reproducible.cachePath = tmpCache,
                       reproducible.fileBackedAnchors = userAnchors)

  mkAnchMod(tmpdir, "anchMod2",
            dotIO = 'sim$ioAnchors <- getOption("reproducible.fileBackedAnchors")')

  sim <- simInit(modules = "anchMod2",
                 paths = list(modulePath = tmpdir, cachePath = tmpCache),
                 times = list(start = 0, end = 1))

  # the user's explicit anchors were used during the run, and left untouched after
  expect_identical(sim$ioAnchors, userAnchors)
  expect_identical(getOption("reproducible.fileBackedAnchors"), userAnchors)
})
