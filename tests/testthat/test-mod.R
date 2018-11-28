test_that("local mod object", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)

  sim <- simInit()

  # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop
  cat(file = file.path(tmpdir, "test", "test.R"),'
      defineModule(sim, list(
      name = "test",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
      ),
      inputObjects = bind_rows(
      ),
      outputObjects = bind_rows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      mod$a <- 2

      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      ', fill = TRUE)

  cat(file = file.path(tmpdir, "test2", "test2.R"),'
      defineModule(sim, list(
      name = "test2",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test2 = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test2.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
      ),
      inputObjects = bind_rows(
      ),
      outputObjects = bind_rows(
      )
      ))

      doEvent.test2 = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      mod$a <- 1
      sim <- scheduleEvent(sim, start(sim), "test2", "event1", .skipChecks = TRUE)
      },
      event1 = {
      mod$b <- mod$a + 1
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      ', fill = TRUE)
  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"))
  out <- spades(mySim)
  expect_true(out$test$a == 2)
  expect_true(out$test2$a == 1)
  expect_true(out$test2$b == 2)

})
