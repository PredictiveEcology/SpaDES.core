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
  withr::local_options(reproducible.cacheSaveFormat = "qs2")
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
      expect_true(any(grepl("qs2", files) & !grepl("gg", files)))
    }
    if (any(grepl("png", out))) {
      expect_true(any(grepl("png", files)))
      expect_true(sum(grepl(wdth, mess)) == 1)
    }
    if (any(grepl("pdf", out))) {
      expect_true(any(grepl("pdf", files)))
    }

    expect_true(NROW(outputs(simOut)) == length(out))
    unlink(files)
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
  testthat::skip_on_ci()
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

  pkgFun <- sapply(baseClassesCanHandle, .guessPkgFun)
  test <- sapply(pkgFun, function(x) {
    exists(sub(".*:", "", x), where = paste0("package:", sub(":.*", "", x)), mode = "function")
  })
  expect_true(all(test))
})

test_that("Plots - base R fn (non-gg result)", {
  skip_on_cran()
  testInit()
  newModule("test", tmpdir, open = FALSE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")

  cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
      name = "test", description = NA, keywords = NA,
      authors = person("A", "B"), childModules = character(0),
      version = list(test = "0.0.1"),
      timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year",
      citation = list(), documentation = list(), reqdPkgs = list(),
      parameters = rbind(), inputObjects = bindrows(), outputObjects = bindrows()
    ))
    doEvent.test <- function(sim, eventTime, eventType, debug = FALSE) {
      switch(eventType,
        init = {
          sim$df <- data.frame(a = rnorm(50))
          Plots(data = sim$df, fn = fnHist, filename = "hist_test",
                types = c("png", "raw"), .plotInitialTime = NA)
        }
      )
      return(invisible(sim))
    }
    fnHist <- function(d, ...) hist(d$a, main = "test", ...)
  ', fill = TRUE)

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                 times = list(start = 0, end = 1, timeunit = "year"))
  suppressMessages(simOut <- spades(sim, debug = FALSE))
  files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)
  expect_true(any(grepl("hist_test", files) & endsWith(files, ".png")))
  expect_true(any(grepl("hist_test", files) & grepl("_data\\.qs2$", files)))
  expect_equal(NROW(outputs(simOut)), 2L)
})

test_that("Plots - terra SpatRaster and SpatVector", {
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit()
  newModule("test", tmpdir, open = FALSE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")

  cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
      name = "test", description = NA, keywords = NA,
      authors = person("A", "B"), childModules = character(0),
      version = list(test = "0.0.1"),
      timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year",
      citation = list(), documentation = list(), reqdPkgs = list("terra"),
      parameters = rbind(), inputObjects = bindrows(), outputObjects = bindrows()
    ))
    doEvent.test <- function(sim, eventTime, eventType, debug = FALSE) {
      switch(eventType,
        init = {
          # SpatRaster: raw saves as .tif
          sim$ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = runif(100), res = 1)
          Plots(data = sim$ras, filename = "ras_test",
                types = c("png", "raw"), .plotInitialTime = NA)
          # SpatVector: raw saves as .qs2 (not Raster/SpatRaster)
          sim$vect <- terra::vect(cbind(1:5, 1:5))
          Plots(data = sim$vect, filename = "vect_test",
                types = c("png", "raw"), .plotInitialTime = NA)
        }
      )
      return(invisible(sim))
    }
  ', fill = TRUE)

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                 times = list(start = 0, end = 1, timeunit = "year"))
  suppressMessages(simOut <- spades(sim, debug = FALSE))
  files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)

  expect_true(any(grepl("ras_test", files) & endsWith(files, ".png")))
  expect_true(any(grepl("ras_test", files) & endsWith(files, ".tif")))   # raw SpatRaster -> .tif

  expect_true(any(grepl("vect_test", files) & endsWith(files, ".png")))
  expect_true(any(grepl("vect_test", files) & grepl("_data\\.qs2$", files))) # raw SpatVector -> .qs2

  expect_equal(NROW(outputs(simOut)), 4L)  # 2 per Plots call
})

test_that("Plots - named ... args without data argument", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  testInit()
  newModule("test", tmpdir, open = FALSE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")

  cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
      name = "test", description = NA, keywords = NA,
      authors = person("A", "B"), childModules = character(0),
      version = list(test = "0.0.1"),
      timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year",
      citation = list(), documentation = list(), reqdPkgs = list("ggplot2"),
      parameters = rbind(), inputObjects = bindrows(), outputObjects = bindrows()
    ))
    doEvent.test <- function(sim, eventTime, eventType, debug = FALSE) {
      switch(eventType,
        init = {
          sim$df <- data.frame(a = rnorm(20))
          # data omitted -- sim$df passed via named arg d1 in ...
          Plots(d1 = sim$df, fn = fnDots, filename = "dots_test",
                types = "png", .plotInitialTime = NA)
        }
      )
      return(invisible(sim))
    }
    fnDots <- function(d1, ...) {
      ggplot2::ggplot(d1, ggplot2::aes(a)) + ggplot2::geom_histogram(bins = 5)
    }
  ', fill = TRUE)

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                 times = list(start = 0, end = 1, timeunit = "year"))
  suppressMessages(simOut <- spades(sim, debug = FALSE))
  files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)
  expect_true(any(grepl("dots_test", files) & endsWith(files, ".png")))
  expect_equal(NROW(outputs(simOut)), 1L)
})

test_that("Plots - ggplot object passed directly as data", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  testInit()
  newModule("test", tmpdir, open = FALSE)
  withr::local_options(reproducible.cacheSaveFormat = "qs2")

  cat(file = file.path(tmpdir, "test", "test.R"), '
    defineModule(sim, list(
      name = "test", description = NA, keywords = NA,
      authors = person("A", "B"), childModules = character(0),
      version = list(test = "0.0.1"),
      timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year",
      citation = list(), documentation = list(), reqdPkgs = list("ggplot2"),
      parameters = rbind(), inputObjects = bindrows(), outputObjects = bindrows()
    ))
    doEvent.test <- function(sim, eventTime, eventType, debug = FALSE) {
      switch(eventType,
        init = {
          sim$gg_obj <- ggplot2::ggplot(data.frame(a = rnorm(20)), ggplot2::aes(a)) +
            ggplot2::geom_histogram(bins = 5)
          # ggplot object passed directly -- fn not needed, ggsave path is used
          Plots(data = sim$gg_obj, filename = "ggobj_test",
                types = c("png", "object"), .plotInitialTime = NA)
        }
      )
      return(invisible(sim))
    }
  ', fill = TRUE)

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir),
                 times = list(start = 0, end = 1, timeunit = "year"))
  suppressMessages(simOut <- spades(sim, debug = FALSE))
  files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)
  expect_true(any(grepl("ggobj_test", files) & endsWith(files, ".png")))
  expect_true(any(grepl("ggobj_test", files) & grepl("_gg\\.qs2$", files)))
  expect_equal(NROW(outputs(simOut)), 2L)
})
