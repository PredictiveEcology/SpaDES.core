## The progress bar module: newProgressBar()/setProgressBar() and the
## doEvent.progress() event handler.
##
## These touch .pkgEnv$.pb, which is package-global, so each test restores it.

newProgressBar <- SpaDES.core:::newProgressBar
setProgressBar <- SpaDES.core:::setProgressBar
doEvent.progress <- SpaDES.core:::doEvent.progress
pkgEnv <- SpaDES.core:::.pkgEnv

## newProgressBar()/setProgressBar() probe for tcltk; on a headless machine the
## first load warns "no DISPLAY variable so Tk is not available". Trigger it
## once here so it is not attributed to whichever test happens to run first.
suppressWarnings(requireNamespace("tcltk", quietly = TRUE))

## save/restore .pkgEnv$.pb around a test
localProgressBar <- function(envir = parent.frame()) {
  had <- exists(".pb", envir = pkgEnv)
  old <- if (had) get(".pb", envir = pkgEnv)
  withr::defer({
    if (had) {
      assign(".pb", old, envir = pkgEnv)
    } else if (exists(".pb", envir = pkgEnv)) {
      rm(".pb", envir = pkgEnv)
    }
  }, envir = envir)
}

## a sim with real module paths -- P(sim, module = ".progress") resolves them
progressSim <- function(tmpdir, type = "text", interval = 1, end = 2) {
  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = end, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )
  sim@params[[".progress"]] <- list(type = type, interval = interval)
  sim
}

test_that("newProgressBar creates a text progress bar", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir)
  invisible(capture.output(newProgressBar(sim)))

  expect_true(exists(".pb", envir = pkgEnv))
  expect_s3_class(get(".pb", envir = pkgEnv), "txtProgressBar")
})

test_that("newProgressBar replaces an existing bar", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir)
  invisible(capture.output(newProgressBar(sim)))
  first <- get(".pb", envir = pkgEnv)

  invisible(capture.output(newProgressBar(sim)))
  second <- get(".pb", envir = pkgEnv)

  expect_s3_class(second, "txtProgressBar")
  expect_false(identical(first, second))
})

test_that("setProgressBar advances the bar to the current sim time", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir)
  invisible(capture.output(newProgressBar(sim)))

  sim@simtimes[["current"]] <- end(sim, "second") / 2

  ## assert the bar advanced, not that the call was silent -- under covr this
  ## code path emits messages it does not emit in a plain run
  before <- utils::getTxtProgressBar(get(".pb", envir = pkgEnv))
  invisible(capture.output(suppressMessages(setProgressBar(sim))))
  after <- utils::getTxtProgressBar(get(".pb", envir = pkgEnv))

  expect_s3_class(get(".pb", envir = pkgEnv), "txtProgressBar")
  expect_gt(after, before)
})

test_that("the shiny progress bar is explicitly not implemented", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir, type = "shiny")

  expect_error(newProgressBar(sim), "shiny progress bar not yet implemented")

  ## setProgressBar needs an existing bar before it reaches the shiny branch
  sim2 <- progressSim(tmpdir, type = "text")
  invisible(capture.output(newProgressBar(sim2)))
  sim2@params[[".progress"]] <- list(type = "shiny", interval = 1)
  expect_error(setProgressBar(sim2), "shiny progress bar not yet implemented")
})

test_that("doEvent.progress init disables the bar when non-interactive", {
  skip_on_cran()
  skip_if(interactive(), "this asserts the non-interactive branch")
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir)
  nEventsBefore <- NROW(events(sim))

  out <- doEvent.progress(sim, eventTime = 0, eventType = "init")

  expect_s4_class(out, "simList")
  ## the non-interactive branch installs the all-NA template ...
  expect_true(all(is.na(unlist(P(out, module = ".progress")))))
  ## ... and therefore schedules no progress events
  expect_identical(NROW(events(out)), nEventsBefore)
})

test_that("doEvent.progress set schedules the next update one interval later", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir, interval = 1)
  invisible(capture.output(newProgressBar(sim)))

  out <- invisible(capture.output(
    sim2 <- doEvent.progress(sim, eventTime = 0, eventType = "set")
  ))

  scheduled <- events(sim2)[events(sim2)$moduleName == "progress", ]
  expect_true(NROW(scheduled) >= 1)
  expect_true("set" %in% scheduled$eventType)
})

test_that("doEvent.progress warns on an unknown event type", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)
  localProgressBar()

  sim <- progressSim(tmpdir)
  ## the warning message reads from current(sim), so give it a current event
  sim <- scheduleEvent(sim, 0, "progress", "bogusType")
  current(sim) <- events(sim)[events(sim)$moduleName == "progress", ][1, ]

  expect_warning(doEvent.progress(sim, eventTime = 0, eventType = "bogusType"),
                 "Undefined event type")
})
