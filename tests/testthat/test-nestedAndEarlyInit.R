## simInitAndSpades() set options(spades.loadReqdPkgs = FALSE) for its spades() call, so the option was
## still FALSE while the events ran: a simInit() inside an event (Biomass_borealDataPrep runs
## Biomass_core that way) did not load its modules' packages. Biomass_core's `%>>%` (fpCompare) was then
## "not found" unless another module of the outer simulation happened to list fpCompare (2026-11-02).

mkNestMod <- function(mp, name, reqdPkgs = character(), initBody = "NULL") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = %s,
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(), outputObjects = bindrows()))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim$initResult_%s <- { %s } })
  return(invisible(sim))
}
', name, name, deparse1(as.list(reqdPkgs)), name, name, initBody),
    file.path(d, paste0(name, ".R")))
  invisible(name)
}

test_that("a simInit() inside an event of simInitAndSpades() loads its own modules' packages", {
  skip_if_not_installed("tools")
  skip_if("package:tools" %in% search(), "tools is already attached")
  testInit()
  withr::defer(if ("package:tools" %in% search()) detach("package:tools"))
  ## `inner` uses a function of its reqdPkgs `tools`; `outer` runs `inner` as a nested simulation
  mkNestMod(tmpdir, "inner", "tools", initBody = 'toTitleCase("hello world")')
  mkNestMod(tmpdir, "outer", initBody = sprintf(
    'simInitAndSpades(modules = "inner", times = list(start = 0, end = 0), paths = list(modulePath = "%s"))$initResult_inner',
    tmpdir))
  sim <- simInitAndSpades(modules = "outer", times = list(start = 0, end = 0),
                          paths = list(modulePath = tmpdir))
  expect_identical(sim$initResult_outer, "Hello World")
  ## the option is the user's, untouched
  expect_true(getOption("spades.loadReqdPkgs", TRUE))
})

test_that("spades.loadReqdPkgs = FALSE set by the user still skips loading, nested or not", {
  skip_if_not_installed("tools")
  skip_if("package:tools" %in% search(), "tools is already attached")
  testInit(opts = list(spades.loadReqdPkgs = FALSE))
  mkNestMod(tmpdir, "inner", "tools", initBody = '"package:tools" %in% search()')
  sim <- simInitAndSpades(modules = "inner", times = list(start = 0, end = 0),
                          paths = list(modulePath = tmpdir))
  expect_false(sim$initResult_inner)
})

## A module whose init runs during simInit() (its outputs are needed by later .inputObjects) used to
## lose any object with a name starting with "." that it set: only objs() (which leaves those out) was
## copied back. fireSense_ELFs sets `.ELFind` that way; the fit modules then found none (2026-11-02).
test_that("an object with a '.' name set by an init run during simInit() reaches the simList", {
  testInit(opts = list(spades.allowInitDuringSimInit = TRUE))
  for (m in c("first", "second")) dir.create(file.path(tmpdir, m), showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "first", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(first = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(expectsInput(".runId", "character", "")),
  outputObjects = bindrows(createsOutput("area", "numeric", ""))))
doEvent.first <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") { sim$.runId <- "abc"; sim$area <- 1 }
  invisible(sim)
}', file.path(tmpdir, "first", "first.R"))
  writeLines('
defineModule(sim, list(name = "second", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(second = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(expectsInput("area", "numeric", ""), expectsInput(".runId", "character", "")),
  outputObjects = bindrows(createsOutput("seenRunId", "character", ""))))
doEvent.second <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
.inputObjects <- function(sim) { sim$seenRunId <- if (is.null(sim$.runId)) "none" else sim$.runId; sim }
', file.path(tmpdir, "second", "second.R"))
  sim <- simInit(modules = list("first", "second"), times = list(start = 0, end = 0),
                 paths = list(modulePath = tmpdir))
  expect_identical(sim$.runId, "abc")
  expect_identical(sim$seenRunId, "abc")     # .inputObjects of `second` ran after `first`'s init
})
