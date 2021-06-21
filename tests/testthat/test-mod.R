test_that("local mod object", {
  testInitOut <- testInit(smcc = FALSE, debug = FALSE,
                          opts = list("reproducible.useMemoise" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)

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
        defineParameter("testParA", "numeric", 1, NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("sdf", "sdf", "sdfd")
      ),
      outputObjects = bindrows(
        createsOutput("testPar1", "numeric", "")
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      mod$a <- 2
      sim$testPar1 <- Par$testParA

      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }

      .inputObjects <- function(sim) {
        mod$x <- "sdf"
        return(sim)

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
        defineParameter("testParB", "numeric", 2, NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("sdf", "sdf", "sdfd")
      ),
      outputObjects = bindrows(
        createsOutput("testPar2", "numeric", "")
      )
      ))

      doEvent.test2 = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      if (isTRUE(P(sim)$testParB >= 1100)) {
         P(sim, "testParB") <-  P(sim)$testParB + 756
      }
      mod$a <- 1
      sim$testPar2 <- Par$testParB
      sim <- scheduleEvent(sim, start(sim), "test2", "event1", .skipChecks = TRUE)
      },
      event1 = {
      if (isTRUE(P(sim)$testParB >= 1100)) {
         P(sim, "testParB") <-  P(sim)$testParB + 800
      }
      mod$b <- mod$a + 1
      mod$y <- paste0(mod$y, " is test2")
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      .inputObjects <- function(sim) {
      if (isTRUE(P(sim)$testParB >= 543)) {
         P(sim, "testParB") <-  P(sim)$testParB + 654
      }
      mod$y <- "This module"
        return(sim)
      }
      ', fill = TRUE)

  mySim <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"))

  expect_true(mySim$.mods$test2$.objects$y == "This module")
  out2 <- spades(Copy(mySim))
  out3 <- Cache(spades, Copy(mySim))
  mess <- capture_messages({
    out4 <- Cache(spades, Copy(mySim)) # should get cached
  })
  out <- spades(mySim)

  # Test the Par stuff
  expect_true(identical(out2$testPar1, params(out2)$test$testParA))
  expect_true(identical(out2$testPar2, params(out2)$test2$testParB))
  expect_true(identical(out$testPar1, params(out)$test$testParA))
  expect_true(identical(out$testPar2, params(out)$test2$testParB))
  expect_true(identical(out3$testPar1, params(out3)$test$testParA))
  expect_true(identical(out3$testPar2, params(out3)$test2$testParB))
  expect_true(identical(out4$testPar1, params(out4)$test$testParA))
  expect_true(identical(out4$testPar2, params(out4)$test2$testParB))

  # Test the results
  expect_true(out$.mods$test$.objects$a == 2) # object that results from addition
  expect_true(out$.mods$test2$.objects$a == 1) # object that results from addition
  expect_true(out$.mods$test2$.objects$b == 2) # object that results from addition -- didn't collide with sim$test$a
  expect_true(out$.mods$test$.objects$x == "sdf") # correct module, i.e., x is in test, and is sdf
  expect_true(is.null(out$.mods$test2$.objects$x)) # wrong module, i.e., x is in test
  expect_true(!is.null(mySim$.mods$test$.objects$x)) # .inputObjects is run
  expect_true(!is.null(mySim$.mods$test2$.objects$y)) # .inputObjects is run
  expect_true(out$.mods$test2$.objects$y == "This module is test2") # paste0 from .inputObjects & event1 event

  # Post Copy(mySim)
  expect_true(out2$.mods$test$.objects$a == 2)
  expect_true(out2$.mods$test2$.objects$a == 1)
  expect_true(out2$.mods$test2$.objects$b == 2)
  expect_true(is.null(out2$.mods$test2$.objects$x))
  expect_true(!is.null(out2$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out2$.mods$test2$.objects$y == "This module is test2")

  # Cache -- using the first time through
  expect_true(out3$.mods$test$.objects$a == 2)
  expect_true(out3$.mods$test2$.objects$a == 1)
  expect_true(out3$.mods$test2$.objects$b == 2)
  expect_true(is.null(out3$.mods$test2$.objects$x))
  expect_true(!is.null(out3$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out3$.mods$test2$.objects$y == "This module is test2")

  # Cached copy
  expect_true(any(grepl("loaded cached", mess)))
  expect_true(out4$.mods$test$.objects$a == 2)
  expect_true(out4$.mods$test2$.objects$a == 1)
  expect_true(out4$.mods$test2$.objects$b == 2)
  expect_true(is.null(out4$.mods$test2$.objects$x))
  expect_true(!is.null(out4$.mods$test$.objects$x)) # was made in .inputObjects, copies fine
  expect_true(out4$.mods$test2$.objects$y == "This module is test2")

  # Test P replace method
  mySim3 <- simInit(times = list(start = 0, end = 0),
                   paths = list(modulePath = tmpdir), modules = c("test", "test2"),
                   params = list(.globals = list(testParB = 543)))

  # Need "Copy" in this sequence because the event queue is actually an environment :)
  #   so the LHS will have the updated event queue, but the parameters will be at initial conditions
  expect_true(P(mySim3)$test2$testParB == 1197) # .globals + .inputObjects
  mySim4 <- spades(Copy(mySim3), events = "init")
  expect_true(P(mySim4)$test2$testParB == 1953) ## .globals + .inputObjects + init
  mySim4 <- spades(Copy(mySim3))
  expect_true(P(mySim4)$test2$testParB == 1953 + 800) # # .globals + .inputObjects + init + event1 ran

  mySim5 <- Cache(spades, Copy(mySim3)) # should get cached -- event1 runs 1x
  expect_true(P(mySim5)$test2$testParB == 1953 + 800)

  end(mySim5) <- 1
  mySim6 <- Cache(spades, Copy(mySim5)) # doesn't change because only test1 is scheduled
  expect_true(P(mySim6)$test2$testParB == 1953 + 800)

  end(mySim6) <- 2
  mySim7 <- Cache(spades, Copy(mySim6)) # should get cached
  expect_true(P(mySim7)$test2$testParB == 1953 + 800 * 2)
  browser()


})
