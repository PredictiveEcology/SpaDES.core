test_that("Plots function 1", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  testInit()

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
    cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
    name = "test",
    description = "insert module description here",
    keywords = c("insert key words here"),
    authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    childModules = character(0),
    version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.md", "test.Rmd"),
    reqdPkgs = list("ggplot2"),
    parameters = rbind(
      defineParameter(".plotsToDisk", "character", ', lll, ', NA, NA, "lala"),
      defineParameter(".plotInitialTime", "numeric", ', .plotInitialTime, ', NA, NA, "lala")
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
      Plots(data = sim$something, fn = fn1, filename = ', fnForCat, ', bins = 10, fill = "red",
            ggsaveArgs = list(width = ', wdth, '))
    },
    event1 = {
    sim <- scheduleEvent(sim, time(sim) + 1, "test", "event1", .skipChecks = TRUE)
    })
    return(invisible(sim))
    }
    fn1 <- function(d1, bins, ...) {
        ggplot2::ggplot(d1, ggplot2::aes(a)) +
        ggplot2::geom_histogram(bins = bins, ...) +
        ggplot2::labs(title = "hello")
      }



    ', fill = TRUE)
    sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                   times = list(start = 0, end = 10, timeunit = "year"))
    mess <- capture_messages({
      simOut <- spades(sim, debug = TRUE)
    })
    files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)
    expect_true(all(grepl(fn, files)))
    if (iii == 5) {
      expect_true(length(files) == 0L)
    }
    if (any(grepl("object", out))) {
      expect_true(any(grepl("gg", files)))
    }
    if (any(grepl("raw", out))) {
      expect_true(any(grepl("qs", files) & !grepl("gg", files)))
    }
    if (any(grepl("png", out))) {
      expect_true(any(grepl("png", files)))
      expect_true(sum(grepl(wdth, mess)) == 1)
    }
    if (any(grepl("pdf", out)))
      expect_true(any(grepl("pdf", files)))

    expect_true(NROW(outputs(simOut)) == length(out))
    unlink(files)
  }

  if (interactive()) {
    something <- data.frame(a = sample(1:10, replace = TRUE))
    fn1 <- function(d1, bins, title = "hello", ...) {
      ggplot2::ggplot(d1, ggplot2::aes(a)) +
        ggplot2::geom_histogram(bins = bins, ...) +
        ggplot2::labs(title = title)
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
  testInit(sampleModReqdPkgs)

  if (interactive()) {

    times <- list(start = 0.0, end = 1, timeunit = "year")
    params <- list(
      .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
      randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
      # caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
      fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
    )
    modules <- list("randomLandscapes", #"caribouMovement",
                    "fireSpread")
    paths <- list(modulePath = getSampleModules(tmpdir))

    mySim <- simInit(times, params, modules, objects = list(), paths)

    mySim@params$randomLandscapes$.plotInitialTime <- 0
    mySim@params$fireSpread$.plotInitialTime <- 0

    # Makes plots
    expect_no_error(spades(mySim))
    .quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")
    expect_true(exists(paste0("Dev", dev.cur()), .quickPlotEnv))
    # Makes no plots
    clearPlot()
    spades(mySim, .plots = NA)
    expect_false(exists(paste0("Dev", dev.cur()), .quickPlotEnv))
    spades(mySim, .plotInitialTime = NA)
    expect_false(exists(paste0("Dev", dev.cur()), .quickPlotEnv))
  }
})

test_that("Plots function 2", {
  skip_if_not_installed("ggplot2")

  testInit()

  newModule("test", tmpdir, open = FALSE)

  # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop
  cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
    name = "test",
    description = "insert module description here",
    keywords = c("insert key words here"),
    authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    childModules = character(0),
    version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
    spatialExtent = terra::ext(rep(0, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.md", "test.Rmd"),
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
        ggplot2::ggplot(d, ggplot2::aes(a)) +
        ggplot2::geom_histogram(bins = bins, ...)
      }
', fill = TRUE)
  expect_error({
    sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                   times = list(start = 0, end = 10, timeunit = "year"))
    }, "needs a newer version of SpaDES.core"
  )
})

test_that("Plots function 3 - use as Plot", {
  skip_if_not(interactive())
  # if (interactive()) {
    testInit("terra", opts = list(spades.PlotsUsePlot = TRUE))
    packages <- c("raster", "terra")
    functions <- cbind(c("raster", "extent", "stack", "nlayers"),
                       c("rast", "ext", "rast", "nlyr"))
    if (!requireNamespace("raster", quietly = TRUE)) {
      functions <- functions[, 2, drop = FALSE]
      packages <- packages[2]
    }
    for (i in seq(packages)) {
      read <- getFromNamespace(functions[1, i], ns = packages[i])
      ext <- getFromNamespace(functions[2, i], ns = packages[i])
      if (packages[i] %in% "raster") {
        stk <- getFromNamespace(functions[3, i], ns = packages[i])
      } else {
        stk <- c
      }
      nlyr <- getFromNamespace(functions[4, i], ns = packages[i])


      ras <- read(ext(0, 10, 0, 10), vals = runif(100, 0, 1), res = 1)
      stk1 <- stk(ras, lyr2 = ras)
      stk2 <- stk(ras, lyr2 = ras)

      clearPlot()
      expect_no_error(Plots(data = stk1, types = "screen"))
      stk1[1:10] <- 0.5
      stk1 <- stk(stk1)
      expect_no_error(Plots(data = stk1, types = "screen")) # should show both plots with top row at 0.5
      stk1[[1]][1:10] <- 0.25
      stk1 <- stk(stk1)
      expect_no_error(Plot(stk1)) # should show first row on left plot only as lower -- 0.25

      expect_no_error(Plots(data = stk2, types = "screen")) # should add 2 plots, with original data, not updated
      stk2[[2]][1:10] <- 0.25
      stk2 <- stk(stk2)
      expect_no_error(Plots(data = stk2, types = "screen")) # should add 2 plots, with original data, not updated
      stk2[[2]][11:20] <- 0.6
      stk2 <- stk(stk2)
      expect_no_error(Plot(stk2)) # should show first row on left plot only as lower -- 0.25

      clearPlot()
      # should show plots as a using terra::plot
      expect_no_error(Plots(data = stk1, types = "screen", usePlot = FALSE, fn = terra::plot))
    }
  # }
})

test_that("Plots test .guessPkgFun", {
  testInit("raster")

  pkgFun <- sapply(baseClassesCanHandle, SpaDES.core:::.guessPkgFun)
  test <- sapply(pkgFun, function(x) {
    exists(sub(".*:", "", x), where = paste0("package:", sub(":.*", "", x)), mode = "function")
  })
  expect_true(all(test))
})
