test_that("objectDiagram(module =, path =) draws one figure per code unit; titles add height", {
  testInit(c("ggplot2", "ragg", "systemfonts"))
  p <- dirname(efFixtureFire(tmpdir))
  a <- objectDiagram(module = "fireMod", path = p, figDir = file.path(tmpdir, "a"))
  b <- objectDiagram(module = "fireMod", path = p, figDir = file.path(tmpdir, "b"), titles = FALSE)
  expect_identical(names(a), c(".inputObjects", "init", "Burn", "summarySingle", "summaryMulti"))
  expect_true(all(file.exists(a)))
  expect_true(all(attr(a, "heights") > attr(b, "heights")))
})

test_that("event names that sanitize alike get different files", {
  testInit(c("ggplot2", "ragg", "systemfonts"))
  efModule(tmpdir, "nameMod", outputs = 'createsOutput("o", "numeric", "")', code =
    'doEvent.nameMod <- function(sim, eventTime, eventType) {
       switch(eventType,
         init = { sim <- scheduleEvent(sim, start(sim), "nameMod", "a b")
                  sim <- scheduleEvent(sim, start(sim), "nameMod", "a_b") },
         "a b" = { sim$o <- 1 }, "a_b" = { sim$o <- 2 })
       invisible(sim)
     }')
  f <- objectDiagram(module = "nameMod", path = tmpdir, figDir = file.path(tmpdir, "n"))
  expect_false(anyDuplicated(basename(f)) > 0)
})

## the figure files that moduleEventFigures() output `out` refers to
efWrittenFigures <- function(out) {
  sub("^!\\[\\]\\((.*)\\)\\{width=100%\\}$", "\\1", grep("^!\\[\\]\\(", out, value = TRUE))
}

test_that("moduleEventFigures removes figures of events that no longer exist", {
  testInit(c("ggplot2", "ragg", "systemfonts", "knitr"))
  fd <- file.path(tmpdir, "figs"); dir.create(fd)
  stale <- file.path(fd, "fireMod_io_oldEvent.png"); file.create(stale)
  other <- file.path(fd, "fireMod2_io_x.png"); file.create(other)
  out <- capture.output(moduleEventFigures(moduleEventFlow("fireMod", dirname(efFixtureFire(tmpdir))), fd))
  expect_false(file.exists(stale))
  expect_true(file.exists(other))
  written <- efWrittenFigures(out)
  expect_length(written, 5)
  expect_true(all(file.exists(written)))
  expect_true(any(grepl("^#### Burn \\{-\\}$", out)))
  expect_true(any(grepl("fireMod_io_Burn.png", out, fixed = TRUE)))
})

test_that("moduleEventFigures keeps the figures it writes when figDir starts with ~", {
  skip_on_cran() # writes under the home directory
  testInit(c("ggplot2", "ragg", "systemfonts", "knitr"))
  skip_if_not(file.access(path.expand("~"), 2) == 0, "the home directory is not writable")
  sub <- basename(tempfile("SpaDES.core-eventFigures-"))
  withr::defer(unlink(file.path(path.expand("~"), sub), recursive = TRUE))
  fd <- file.path("~", sub)
  dir.create(fd)
  out <- capture.output(moduleEventFigures(moduleEventFlow("fireMod", dirname(efFixtureFire(tmpdir))), fd))
  written <- efWrittenFigures(out)
  expect_length(written, 5)
  expect_true(all(startsWith(written, "~")))
  expect_true(all(file.exists(written)))
})
