## Tests for the SpaDES.core <-> reproducible urlLog wiring.
##
## Strategy: two tiny modules invoke reproducible:::.logUrlAccess from inside
## .inputObjects and from inside their init / event1 events. We assert that
## envir(sim)$._urlLog$records accumulates correctly and that each record
## carries the right module + event labels via the sink$extra mechanism.

modAcode <- '
defineModule(sim, list(
  name = "modA",
  description = "url-log test module A",
  keywords = "url-log",
  authors = person("Test", "User", email = "test@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", modA = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "second",
  citation = list("citation.bib"),
  documentation = list("README.md", "modA.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(defineParameter("p", "numeric", 1, NA, NA, "")),
  inputObjects = bindrows(
    expectsInput("modAobj", "ANY", "force .inputObjects to run")
  ),
  outputObjects = bindrows()
))

doEvent.modA = function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      reproducible:::.logUrlAccess("prepInputs",
                                   "https://example.com/modA-init.tif",
                                   destinationPath = ".")
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "modA", "step",
                           .skipChecks = TRUE)
    },
    step = {
      reproducible:::.logUrlAccess("prepInputs",
                                   "https://example.com/modA-step.tif",
                                   destinationPath = ".")
    }
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  reproducible:::.logUrlAccess("prepInputs",
                               "https://example.com/modA-inputObjects.tif",
                               destinationPath = ".")
  return(sim)
}
'

modBcode <- '
defineModule(sim, list(
  name = "modB",
  description = "url-log test module B",
  keywords = "url-log",
  authors = person("Test", "User", email = "test@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", modB = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "second",
  citation = list("citation.bib"),
  documentation = list("README.md", "modB.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(defineParameter("p", "numeric", 1, NA, NA, "")),
  inputObjects = bindrows(
    expectsInput("modBobj", "ANY", "force .inputObjects to run")
  ),
  outputObjects = bindrows()
))

doEvent.modB = function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      reproducible:::.logUrlAccess("prepInputs",
                                   "https://example.com/modB-init.tif",
                                   destinationPath = ".")
    }
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  reproducible:::.logUrlAccess("prepInputs",
                               "https://example.com/modB-inputObjects.tif",
                               destinationPath = ".")
  return(sim)
}
'

test_that("urlLog: simInit + spades populate envir(sim)$._urlLog with module+event labels", {
  testInit("terra", smcc = FALSE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  skip_if_not_installed("reproducible", "3.1.1.9012")

  newModule("modA", tmpdir, open = FALSE)
  newModule("modB", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "modA", "modA.R"), modAcode, fill = TRUE)
  cat(file = file.path(tmpdir, "modB", "modB.R"), modBcode, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 2),
                   paths = list(modulePath = tmpdir),
                   modules = c("modA", "modB"))

  ## ._urlLog env was created on envir(sim) and is a "dot" object.
  expect_true("._urlLog" %in% ls(envir(mySim), all.names = TRUE))
  expect_false("._urlLog" %in% ls(envir(mySim)))   # hidden from default ls()
  expect_true(is.environment(envir(mySim)$._urlLog))

  ## simInit phase: .inputObjects of each module ran -> 2 records.
  recs <- envir(mySim)$._urlLog$records
  expect_length(recs, 2L)
  iomods <- vapply(recs, function(r) r$module %||% NA_character_, character(1))
  ioevts <- vapply(recs, function(r) r$event  %||% NA_character_, character(1))
  expect_setequal(iomods, c("modA", "modB"))
  expect_true(all(ioevts == ".inputObjects"))

  ## Now run spades. modA: init + step events, modB: init only.
  out <- spades(mySim)
  recs <- envir(out)$._urlLog$records
  ## 2 (.inputObjects) + 2 (init) + 1 (step) = 5 records total
  expect_length(recs, 5L)
  mods <- vapply(recs, function(r) r$module %||% NA_character_, character(1))
  evts <- vapply(recs, function(r) r$event  %||% NA_character_, character(1))
  expect_setequal(unique(mods), c("modA", "modB"))
  expect_true(all(evts %in% c(".inputObjects", "init", "step")))

  ## Spot-check one specific tuple
  has_modA_step <- any(mods == "modA" & evts == "step" &
                       vapply(recs, function(r) r$url, character(1)) ==
                       "https://example.com/modA-step.tif")
  expect_true(has_modA_step)
})

test_that("urlLog: option spades.urlLog = FALSE disables the wiring", {
  testInit("terra", smcc = FALSE,
           opts = list(reproducible.useMemoise = FALSE,
                       spades.urlLog = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  skip_if_not_installed("reproducible", "3.1.1.9012")

  newModule("modA", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "modA", "modA.R"), modAcode, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = "modA")
  expect_null(envir(mySim)$._urlLog)
})

test_that("urlLog: caller-supplied reproducible.urlLog env is respected (not clobbered)", {
  testInit("terra", smcc = FALSE,
           opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  skip_if_not_installed("reproducible", "3.1.1.9012")

  ## A user-owned env that simInit/spades should overwrite for the duration
  ## of the call; the prior option value is restored on exit. This documents
  ## the chosen behavior: the sim's ._urlLog always wins inside simInit/spades.
  userEnv <- new.env(parent = emptyenv())
  userEnv$records <- list()
  userEnv$seen    <- character()
  withr::local_options(reproducible.urlLog = userEnv)

  newModule("modA", tmpdir, open = FALSE)
  cat(file = file.path(tmpdir, "modA", "modA.R"), modAcode, fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = "modA")

  ## sim's ._urlLog was populated by simInit (not userEnv).
  expect_true(is.environment(envir(mySim)$._urlLog))
  expect_true(length(envir(mySim)$._urlLog$records) >= 1L)
  expect_length(userEnv$records, 0L)

  ## And the user's option value is back after simInit returned.
  expect_identical(getOption("reproducible.urlLog"), userEnv)
})
