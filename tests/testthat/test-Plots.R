test_that("Plots function 1", {
  if (require("ggplot2")) {
    testInitOut <- testInit()
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    newModule("test", tmpdir, open = FALSE)

    # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop
    outs <- list(c("png", "object", "raw"),
                 c("png", "object"),
                 c("png", "raw"),
                 c("raw"),
                 NULL)
    .plotInitialTimes <- c(NA_integer_, NA_integer_, 1L, 1L, NA_integer_)
    iii <- 0
    for (out in outs) {
      iii <- iii + 1
      .plotInitialTime <- .plotInitialTimes[iii]

      lll <- capture.output(dput(out))
      fn <- "testing"
      wdth <- 4.77
      fnForCat <- capture.output(dput(fn))
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
      timeunit = "year",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test.Rmd"),
      reqdPkgs = list("ggplot2"),
      parameters = rbind(
        defineParameter(".plotsToDisk", "character", ',lll,', NA, NA, "lala"),
        defineParameter(".plotInitialTime", "numeric", ',.plotInitialTime,', NA, NA, "lala")
      ),
      inputObjects = bindrows(
      ),
      outputObjects = bindrows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
        sim <- scheduleEvent(sim, time(sim) + 1, "test", "event1", .skipChecks = TRUE)
        sim$something <- data.frame(a = sample(1:10, replace = TRUE))
        Plots(data = sim$something, fn = fn1, filename = ',fnForCat,', bins = 10, fill = "red",
              ggsaveArgs = list(width = ',wdth,'))
      },
      event1 = {
      sim <- scheduleEvent(sim, time(sim) + 1, "test", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      fn1 <- function(d1, bins, ...) {
          ggplot(d1, aes(a)) +
          geom_histogram(bins = bins, ...) +
          labs(title = "hello")
        }



      ', fill = TRUE)
      sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                     times = list(start = 0, end = 10, timeunit = "year"))
      mess <- capture_messages(simOut <- spades(sim, debug = TRUE))
      files <- dir(file.path(outputPath(sim), "figures"), full.names = TRUE)
      expect_true(all(grepl(fn, files)))
      if (iii == 5) {
        expect_true(length(files) == 0L)
      }
      if (any(grepl("object", out)))
        expect_true(any(grepl("gg", files)))
      if (any(grepl("raw", out)))
        expect_true(any(grepl("qs", files) & !grepl("gg", files)))
      if (any(grepl("png", out))) {
        expect_true(any(grepl("png", files)))
        expect_true(sum(grepl(wdth, mess)) == 1)
      }
      if (any(grepl("pdf", out)))
        expect_true(any(grepl("pdf", files)))

      expect_true(NROW(outputs(simOut)) == length(out))
      unlink(files)

    }
  }

  if (interactive()) {
    something <- data.frame(a = sample(1:10, replace = TRUE))
    fn1 <- function(d1, bins, title = "hello", ...) {
      ggplot(d1, aes(a)) +
        geom_histogram(bins = bins, ...) +
        labs(title = title)
    }
    # Should add next 2 plots to same windows
    Plots(data = something, fn = fn1, bins = 10, fill = "red", types = "screen", title = "run1")
    Plots(data = something, fn = fn1, bins = 10, fill = "red", types = "screen", title = "run2")
    # Should clear plot windows and each of following will be a new plot on its own
    Plots(data = something, fn = fn1, bins = 10, fill = "red", types = "screen", title = "run3", usePlot = FALSE)
    Plots(data = something, fn = fn1, bins = 10, fill = "red", types = "screen", title = "run4", usePlot = FALSE)
    clearPlot()
    # avoid using `data` arg; just use all named args
    Plots(d1 = something, fn = fn1, bins = 10, fill = "red", types = "screen", title = "run4", usePlot = FALSE)
  }
})

test_that("testing .plotInitialTime & .plots", {
  if (interactive()) {
    testInitOut <- testInit()
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    times <- list(start = 0.0, end = 1, timeunit = "year")
    params <- list(
      .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
      randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
      caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
      fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
    )
    modules <- list("randomLandscapes", #"caribouMovement",
                    "fireSpread")
    paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

    mySim <- simInit(times, params, modules, objects = list(), paths)

    mySim@params$randomLandscapes$.plotInitialTime <- 0
    mySim@params$fireSpread$.plotInitialTime <- 0
    mySim@params$caribouMovement$.plotInitialTime <- 0

    # Makes plots
    spades(mySim)
    # Makes no plots
    spades(mySim, .plots = NA)
    spades(mySim, .plotInitialTime = NA)
  }
})

test_that("Plots function 2", {
  if (require("ggplot2")) {
    testInitOut <- testInit()
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)

    newModule("test", tmpdir, open = FALSE)

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
      timeunit = "year",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test.Rmd"),
      reqdPkgs = list("SpaDES.core (>= 3.0)", "SpaDES.core (>= 3.3)", "SpaDES.core (>= 1.0)"),
      parameters = rbind(
      ),
      inputObjects = bindrows(
      ),
      outputObjects = bindrows(
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      },
      event1 = {
      })
      return(invisible(sim))
      }
      fn1 <- function(d, bins, ...) {
          ggplot(d, aes(a)) +
          geom_histogram(bins = bins, ...)
        }
  ', fill = TRUE)
    expect_error(sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                                times = list(start = 0, end = 10, timeunit = "year")),
                 "needs a newer version of SpaDES.core")
  }
})


test_that("Plots function 3 - use as Plot", {
  if (interactive()) {
    testInitOut <- testInit("raster")
    on.exit({
      testOnExit(testInitOut)
    }, add = TRUE)
    ras <- raster(extent(0,10, 0, 10), vals = runif(100, 0, 1), res = 1)
    stk1 <- raster::stack(ras, ras)
    stk2 <- raster::stack(ras, ras)

    clearPlot()
    Plots(data = stk1, types = "screen")
    stk1[1:10] <- 0.5
    stk1 <- raster::stack(stk1)
    Plots(data = stk1, types = "screen") # should show both plots with top row at 0.5
    stk1[[1]][1:10] <- 0.25
    stk1 <- raster::stack(stk1)
    Plot(stk1) # should show first row on left plot only as lower -- 0.25

    Plots(data = stk2, types = "screen") # should add 2 plots, with original data, not updated
    stk2[[2]][1:10] <- 0.25
    stk2 <- raster::stack(stk2)
    Plots(data = stk2, types = "screen") # should add 2 plots, with original data, not updated
    stk2[[2]][11:20] <- 0.6
    stk2 <- raster::stack(stk2)
    Plot(stk2) # should show first row on left plot only as lower -- 0.25
  }

})

