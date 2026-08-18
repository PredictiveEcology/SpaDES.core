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
    sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
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
    sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
                   times = list(start = 0, end = 10, timeunit = "year"))
    }, "needs a newer version of SpaDES.core"
  )
})

test_that("Plots function 3 - use as Plot", {
  skip_on_cran()
    testInit("terra", opts = list(spades.PlotsUsePlot = TRUE))
    # quickPlot::Plot (and Plots(types = "screen") routed through it via
    # spades.PlotsUsePlot = TRUE) opens an interactive screen device. On a
    # headless machine (CI, logged runs) there is none, so open an offscreen
    # png device first: quickPlot's dev() reuses an existing device rather than
    # calling dev.new(), so the screen path runs without a display. (Previously
    # this whole test was skipped on CI for want of a device.)
    grDevices::png(withr::local_tempfile(fileext = ".png"))
    withr::defer(grDevices::dev.off())
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

test_that("Plots - modern wrapper (usePlot = FALSE) screen path + dispatch", {
  # The non-deprecated Plots path: usePlot = FALSE (the default) does not route
  # through quickPlot::Plot; it dispatches to `fn` (terra::plot for spatial data,
  # base plot otherwise). This used to be exercised only via the screen-bound
  # test above; assert it here too, headless, so it runs on CI. See also
  # "Plots - terra SpatRaster and SpatVector" for the file-saving (types = "png")
  # branch of the same wrapper.
  skip_on_cran()
  skip_if_not_installed("terra")
  testInit("terra") # usePlot defaults to FALSE

  grDevices::png(withr::local_tempfile(fileext = ".png"))
  withr::defer(grDevices::dev.off())

  ras <- terra::rast(terra::ext(0, 10, 0, 10), vals = runif(100), res = 1)
  vec <- terra::vect(cbind(1:5, 1:5))

  # A custom fn lets us assert the wrapper actually dispatched to it on the
  # screen path (not just that it ran without error).
  callCount <- new.env(parent = emptyenv())
  callCount$n <- 0L
  myFn <- function(x, ...) {
    callCount$n <- callCount$n + 1L
    invisible()
  }
  expect_no_error(Plots(data = ras, types = "screen", fn = myFn))
  expect_identical(callCount$n, 1L)

  # Default dispatch: SpatRaster / SpatVector route to terra::plot without error.
  expect_no_error(Plots(data = ras, types = "screen"))
  expect_no_error(Plots(data = vec, types = "screen"))
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

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
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

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
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

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
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

  sim <- simInit(modules = "test", paths = list(modulePath = tmpdir, outputPath = file.path(tmpdir, "outputs")),
                 times = list(start = 0, end = 1, timeunit = "year"))
  suppressMessages(simOut <- spades(sim, debug = FALSE))
  files <- dir(figurePath(sim), full.names = TRUE, recursive = TRUE)
  expect_true(any(grepl("ggobj_test", files) & endsWith(files, ".png")))
  expect_true(any(grepl("ggobj_test", files) & grepl("_gg\\.qs2$", files)))
  expect_equal(NROW(outputs(simOut)), 2L)
})

test_that("canonicalize_ggplot digest distinguishes plot differences", {
  skip_if_not_installed("ggplot2")
  testInit("ggplot2")

  d <- data.frame(x = 1:12, y = (1:12)^2, g = rep(c("a", "b"), 6), h = rep(c("p", "q"), each = 6))
  d2 <- d
  d2$y <- d2$y * 2
  B <- function() ggplot(d, aes(x, y)) + geom_point()
  dig <- function(p) .robustDigest(canonicalize_ggplot(p))

  # each pair differs in exactly one way; the digest must see all of them, because a
  # collision here means Plots(useCache = TRUE) keeps a stale figure
  pairs <- list(
    "geom type"          = list(B(), ggplot(d, aes(x, y)) + geom_line()),
    "geom param"         = list(B() + geom_point(size = 3), B() + geom_point(size = 5)),
    "alpha"              = list(ggplot(d, aes(x, y)) + geom_point(alpha = 0.3),
                                ggplot(d, aes(x, y)) + geom_point(alpha = 0.9)),
    "constant colour"    = list(ggplot(d, aes(x, y)) + geom_point(colour = "red"),
                                ggplot(d, aes(x, y)) + geom_point(colour = "blue")),
    "stat param"         = list(ggplot(d, aes(x)) + geom_histogram(bins = 5),
                                ggplot(d, aes(x)) + geom_histogram(bins = 30)),
    "aes mapping"        = list(B(), ggplot(d, aes(x, y, colour = g)) + geom_point()),
    "plot data values"   = list(B(), ggplot(d2, aes(x, y)) + geom_point()),
    "layer-specific data" = list(B() + geom_line(data = d[1:3, ]), B() + geom_line(data = d[1:6, ])),
    "layer order"        = list(ggplot(d, aes(x, y)) + geom_point() + geom_line(),
                                ggplot(d, aes(x, y)) + geom_line() + geom_point()),
    "facet_wrap var"     = list(B() + facet_wrap("g"), B() + facet_wrap("h")),
    "facet_wrap ncol"    = list(B() + facet_wrap("g", ncol = 2), B() + facet_wrap("g", ncol = 3)),
    "facet_grid vars"    = list(B() + facet_grid(g ~ h), B() + facet_grid(h ~ g)),
    "scale transform"    = list(B() + scale_y_log10(), B() + scale_y_sqrt()),
    "scale limits"       = list(B() + scale_y_continuous(limits = c(0, 100)),
                                B() + scale_y_continuous(limits = c(0, 200))),
    "coord limits"       = list(B() + coord_cartesian(xlim = c(1, 3)), B() + coord_cartesian(xlim = c(1, 4))),
    "coord type"         = list(B() + coord_cartesian(), B() + coord_flip()),
    "labels"             = list(B() + labs(title = "A"), B() + labs(title = "B")),
    "theme preset"       = list(B() + theme_bw(), B() + theme_classic()),
    "theme element"      = list(B() + theme(legend.position = "right"), B() + theme(legend.position = "none")),
    "theme unit element" = list(B() + theme(plot.margin = grid::unit(rep(1, 4), "cm")),
                                B() + theme(plot.margin = grid::unit(rep(4, 4), "cm"))),
    "position adjustment" = list(ggplot(d, aes(g, y)) + geom_point(position = position_jitter(width = 0.1)),
                                 ggplot(d, aes(g, y)) + geom_point(position = position_jitter(width = 0.5)))
  )
  for (nm in names(pairs))
    expect_false(identical(dig(pairs[[nm]][[1]]), dig(pairs[[nm]][[2]])), info = nm)
})

test_that("canonicalize_ggplot digest is stable for identical plots", {
  skip_if_not_installed("ggplot2")
  testInit("ggplot2")

  d <- data.frame(x = 1:12, y = (1:12)^2, g = rep(c("a", "b"), 6), h = rep(c("p", "q"), each = 6))
  dig <- function(p) .robustDigest(canonicalize_ggplot(p))

  # the point of dropping the data and environments: rebuilding the same plot -- notably
  # from a different enclosing environment, as each module event does -- must not change it
  same <- list(
    "different enclosing envs" = list(local({zz <- runif(1e4); ggplot(d, aes(x, y)) + geom_point()}),
                                      local({ww <- "unused"; ggplot(d, aes(x, y)) + geom_point()})),
    "rebuilt twice"            = list(ggplot(d, aes(x, y)) + geom_point() + facet_grid(g ~ h) + scale_y_log10(),
                                      ggplot(d, aes(x, y)) + geom_point() + facet_grid(g ~ h) + scale_y_log10()),
    "equal but separate data"  = list(ggplot(d, aes(x, y)) + geom_point(),
                                      ggplot(data.frame(x = 1:12, y = (1:12)^2, g = rep(c("a", "b"), 6),
                                                        h = rep(c("p", "q"), each = 6)),
                                             aes(x, y)) + geom_point()),
    "pipe vs direct"           = list(d |> ggplot(aes(x, y)) + geom_point(),
                                      ggplot(d, aes(x, y)) + geom_point())
  )
  for (nm in names(same))
    expect_identical(dig(same[[nm]][[1]]), dig(same[[nm]][[2]]), info = nm)

  # a layer inheriting the plot data holds waiver(), not NULL, so it must resolve to the
  # plot data digest rather than to a constant
  md <- canonicalize_ggplot(ggplot(d, aes(x, y)) + geom_point())
  expect_identical(md$layers[[1]]$data_digest$source, "plot")
  expect_identical(md$layers[[1]]$data_digest$digest, md$data_digest)

  # scale transformation is read via get_transformation() on ggplot2 >= 3.5.0; the name
  # itself is ggplot2's to choose, so assert only that one was found (log10 vs sqrt is
  # covered by the pairs above)
  trans <- canonicalize_ggplot(ggplot(d, aes(x, y)) + geom_point() + scale_y_log10())$scales[[1]]$trans
  expect_true(is.character(trans) && nzchar(trans))
})
