## `where` must be read by value. From match.call() it was the unevaluated call: `where = c("sim", "user")`
## deparsed to "c", "sim", "user", and "c" matched "cyclic", so the future-init check ran although the caller
## left it out. A module then saw an object as supplied by another module's future init event and skipped its
## own default (fireSense_dataPrepFit / canClimateData, 2026-09-24).

test_that("where = c('sim', 'user') leaves out another module's future init event", {
  testInit()
  for (m in c("makesX", "usesX")) {
    dir.create(file.path(tmpdir, m), recursive = TRUE, showWarnings = FALSE)
    io <- if (m == "makesX") 'outputObjects = bindrows(createsOutput("x", "numeric", ""))' else
      'inputObjects = bindrows(expectsInput("x", "numeric", ""))'
    writeLines(c(
      sprintf('defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),', m),
      sprintf('  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),', m),
      '  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(), parameters = rbind(),',
      sprintf('  %s))', io),
      sprintf('doEvent.%s <- function(sim, eventTime, eventType) invisible(sim)', m)),
      file.path(tmpdir, m, paste0(m, ".R")))
  }
  sim <- simInit(modules = c("makesX", "usesX"), paths = list(modulePath = tmpdir))
  ## the future init of makesX creates x
  expect_true(suppliedElsewhere("x", sim))
  expect_true(suppliedElsewhere("x", sim, where = "initEvent"))
  ## ... but neither "sim" nor "user" has it, however `where` is written
  expect_false(suppliedElsewhere("x", sim, where = c("sim", "user")))
  w <- c("sim", "user")
  expect_false(suppliedElsewhere("x", sim, where = w))
  expect_false(suppliedElsewhere("x", sim, where = "user"))
  expect_identical(unname(suppliedElsewhere("x", sim, where = c("sim", "user"), returnWhere = TRUE)["inFutureInit"]), FALSE)
})

test_that("an unknown `where` still stops", {
  testInit()
  sim <- simInit()
  expect_error(suppliedElsewhere("x", sim, where = "nowhere"), "where must be")
})
