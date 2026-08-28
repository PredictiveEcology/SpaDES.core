## Diagram helpers in plotting-diagrams.R: ganttStatus(), .sim2gantt(),
## eventDiagram(), objectDiagram(), moduleDiagram() and moduleGraph().
##
## The Gantt-chart side only needs a completed event list, so those tests build a
## cheap sim with defineEvent() rather than loading the sample modules. The
## dependency-graph side needs real module metadata, so those use the sample
## modules but stop at simInit() -- none of them needs the sim to have been run.

ranSim <- function(end = 2) {
  sim <- simInit(times = list(start = 0, end = end, timeunit = "year"))
  defineEvent(sim, "init", moduleName = "mA", code = {
    sim <- scheduleEvent(sim, time(sim), "mA", "grow")
  })
  defineEvent(sim, "grow", moduleName = "mA", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "mA", "grow")
  })
  defineEvent(sim, "plot", moduleName = "mB", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "mB", "plot")
  })
  sim <- scheduleEvent(sim, 0, "mA", "init")
  sim <- scheduleEvent(sim, 0, "mB", "plot")
  suppressMessages(spades(sim))
}

## ---- ganttStatus -------------------------------------------------------

test_that("ganttStatus maps init, plot and everything else to mermaid statuses", {
  testInit()

  expect_identical(ganttStatus("init"), "done")
  expect_identical(ganttStatus("plot"), "crit")
  expect_identical(ganttStatus("grow"), "active")
})

test_that("ganttStatus is vectorised over the event types", {
  testInit()

  expect_identical(ganttStatus(c("init", "plot", "grow", "save")),
                   c("done", "crit", "active", "active"))
})

## ---- .sim2gantt --------------------------------------------------------

test_that(".sim2gantt returns one data.frame per module in the completed list", {
  skip_on_cran()
  testInit()

  out <- ranSim()
  ll <- SpaDES.core:::.sim2gantt(out, n = NROW(completed(out)),
                                 startDate = "2000-01-01", width = 1000)

  expect_type(ll, "list")
  expect_setequal(names(ll), unique(completed(out)$moduleName))
  expect_true(all(vapply(ll, is.data.frame, logical(1))))
  expect_true(all(vapply(ll, function(d)
    all(c("task", "status", "pos", "start", "end") %in% names(d)), logical(1))))
})

test_that(".sim2gantt dates the tasks from startDate and orders start before end", {
  skip_on_cran()
  testInit()

  out <- ranSim()
  ll <- SpaDES.core:::.sim2gantt(out, n = NROW(completed(out)),
                                 startDate = "2000-01-01", width = 1000)
  d <- ll[["mA"]]

  expect_s3_class(d$start, "Date")
  expect_s3_class(d$end, "Date")
  expect_true(all(d$start >= as.Date("2000-01-01")))
  expect_true(all(d$end > d$start))
  ## the status column is ganttStatus() applied to the tasks
  expect_identical(d$status, ganttStatus(d$task))
})

test_that(".sim2gantt honours n, keeping only the most recent events", {
  skip_on_cran()
  testInit()

  out <- ranSim(end = 3)
  ll <- SpaDES.core:::.sim2gantt(out, n = 2, startDate = "2000-01-01", width = 1000)

  expect_identical(sum(vapply(ll, NROW, integer(1))), 2L)
})

## ---- eventDiagram ------------------------------------------------------

test_that("eventDiagram builds a mermaid gantt chart from a completed sim", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  out <- ranSim()
  d <- eventDiagram(out, n = NROW(completed(out)), startDate = "2000-01-01")

  expect_s3_class(d, "htmlwidget")
  expect_match(d$x$diagram, "^gantt")
  expect_match(d$x$diagram, "title SpaDES event diagram")
  expect_match(d$x$diagram, "section  mA")
})

test_that("eventDiagram defaults n to the whole completed list", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  out <- ranSim()
  withN <- eventDiagram(out, n = NROW(completed(out)), startDate = "2000-01-01")
  noN <- eventDiagram(out, startDate = "2000-01-01")

  expect_identical(noN$x$diagram, withN$x$diagram)
})

test_that("eventDiagram defaults startDate to today", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  out <- ranSim()
  d <- eventDiagram(out)

  expect_s3_class(d, "htmlwidget")
  ## start(sim) is 0, so the origin is today's date
  expect_match(d$x$diagram, format(Sys.Date(), "%Y-%m-%d"))
})

test_that("eventDiagram passes width and height through to mermaid", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  out <- ranSim()
  d <- eventDiagram(out, n = NROW(completed(out)), startDate = "2000-01-01",
                    width = 500, height = 700)

  expect_identical(d$width, 500)
  expect_identical(d$height, 700)
})

test_that("eventDiagram drops the progress module from the chart", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  out <- ranSim()
  ## fake a progress event in the completed list
  cmp <- completed(out)
  cmp$moduleName[1] <- "progress"
  out@completed <- as.environment(list2env(setNames(
    lapply(seq_len(NROW(cmp)), function(i) as.list(cmp[i, ])), as.character(seq_len(NROW(cmp))))))

  d <- eventDiagram(out, n = NROW(cmp), startDate = "2000-01-01")

  expect_false(grepl("section  progress", d$x$diagram))
})

test_that("eventDiagram refuses a simulation that has not been run", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  expect_error(eventDiagram(sim, n = 0, startDate = "2000-01-01"),
               "hasn't been run")
})

## ---- objectDiagram -----------------------------------------------------

test_that("objectDiagram builds a mermaid sequence diagram of the object deps", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  d <- objectDiagram(sim)

  expect_s3_class(d, "htmlwidget")
  expect_match(d$x$diagram, "^sequenceDiagram")
  ## every edge of the dependency list appears as a mermaid message
  dt <- depsEdgeList(sim, FALSE)
  expect_match(d$x$diagram, paste(dt$from[1], "->>", dt$to[1], ":", dt$objName[1]),
               fixed = TRUE)
})

test_that("objectDiagram accepts height and width", {
  skip_on_cran()
  skip_if_not_installed("DiagrammeR")
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  d <- objectDiagram(sim, height = 3000, width = 2000)

  expect_identical(d$height, 3000)
  expect_identical(d$width, 2000)
})

## ---- moduleDiagram -----------------------------------------------------

test_that("moduleDiagram plots the module dependency graph", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  grDevices::png(withr::local_tempfile(fileext = ".png"))
  withr::defer(grDevices::dev.off())

  expect_no_error(suppressMessages(moduleDiagram(sim)))
})

test_that("moduleDiagram accepts a supplied title", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  grDevices::png(withr::local_tempfile(fileext = ".png"))
  withr::defer(grDevices::dev.off())

  expect_no_error(suppressMessages(moduleDiagram(sim, title = "custom")))
})

test_that("moduleDiagram's type method reaches base plot", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  grDevices::png(withr::local_tempfile(fileext = ".png"))
  withr::defer(grDevices::dev.off())

  expect_no_error(suppressMessages(moduleDiagram(sim, type = "plot", showParents = FALSE)))
})

## ---- moduleGraph -------------------------------------------------------

test_that("moduleGraph returns the graph and its communities", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  if (Sys.which("glpsol") == "") skip("GLPK not available")

  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = getSampleModules(tmpdir))))

  mg <- suppressMessages(moduleGraph(sim, plot = FALSE))

  ## NULL when igraph lacks GLPK support even though glpsol is on the PATH
  skip_if(is.null(mg), "igraph not compiled with GLPK support")

  expect_type(mg, "list")
  expect_setequal(names(mg), c("graph", "communities"))
  expect_s3_class(mg$graph, "igraph")
  expect_true(all(c("randomLandscapes", "fireSpread") %in% names(igraph::V(mg$graph))))
})

test_that("moduleGraph tells the user how to install GLPK when it is missing", {
  testInit()

  ## the sim is never touched: the missing-GLPK branch returns before it is used
  withr::local_envvar(c(PATH = ""))
  skip_if(Sys.which("glpsol") != "", "could not hide glpsol from Sys.which")
  skip_if(!Sys.info()[["sysname"]] %in% c("Darwin", "Linux"), "message is unix-only")

  msgs <- capture_messages(res <- moduleGraph(simInit(), plot = FALSE))

  expect_null(res)
  expect_match(paste(msgs, collapse = ""), "GLPK not found")
})
