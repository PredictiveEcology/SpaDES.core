test_that("module templates work", {
  testInit(c("knitr", "rmarkdown"), smcc = FALSE)

  expect_true(dir.exists(tmpdir))
  moduleName <- "myModule"

  newModule(moduleName, tmpdir, open = FALSE, unitTests = TRUE, useGitHub = TRUE)

  mpath <- file.path(tmpdir, moduleName)

  expect_true(file.exists(mpath))
  expect_true(file.exists(file.path(mpath, "citation.bib")))
  expect_true(file.exists(file.path(mpath, "LICENSE.md")))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".R"))))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".Rmd"))))
  expect_true(file.exists(file.path(mpath, "NEWS.md")))
  if (getRversion() > "4.1.3") {
    expect_true(
      utils::file_test("-h", file.path(mpath, "README.md")) |
        file.exists(file.path(mpath, "README.md"))
    )
  }

  ghaWorkflowFile <- file.path(mpath, ".github", "workflows", "render-module-rmd.yaml")
  expect_true(dir.exists(dirname(dirname(ghaWorkflowFile))))
  expect_true(dir.exists(dirname(ghaWorkflowFile)))
  expect_true(file.exists(ghaWorkflowFile))
  expect_false(
    sapply(readLines(ghaWorkflowFile), grepl, pattern = "[[name]]", fixed = TRUE) |> any()
  )

  expect_true(dir.exists(file.path(mpath, "data")))
  expect_true(file.exists(file.path(mpath, "data", "CHECKSUMS.txt")))
  expect_true(file.exists(file.path(mpath, "data", ".gitignore")))

  expect_true(dir.exists(file.path(mpath, "tests")))
  expect_true(dir.exists(file.path(mpath, "tests", "testthat")))
  expect_true(file.exists(file.path(mpath, "tests", "unitTests.R")))
  expect_true(file.exists(file.path(mpath, "tests", "testthat", "test-template.R")))

  utils::capture.output(
    zipModule(name = moduleName, path = tmpdir, version = "0.0.2", flags = "-q -r9X")
  )

  skip_if_not(nzchar(Sys.which("zip")))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, "_0.0.2.zip"))))

  ## Test that the .Rmd file actually can run with knitr
  expect_equal(
    knitr::knit(
      input = file.path(mpath, paste0(moduleName, ".Rmd")),
      output = file.path(mpath, paste0(moduleName, ".md")),
      quiet = TRUE
    ),
    file.path(mpath, paste0(moduleName, ".md"))
  )
  expect_true(file.exists(file.path(mpath, "README.md"))) ## file should exist now, post-knit

  ## Test that the dummy unit tests work
  # test_file(file.path(mpath, "tests", "testthat", "test-template.R")) # TODO: make it work
})

test_that("empty defineModule", {
  testInit()

  sim <- simInit()
  sim <- expect_warning(defineModule(sim, list()))
  b <- depends(sim)
  out <- lapply(names(moduleDefaults), function(modDef) {
    if (modDef != "version") {
      if (all(!(c("extent", "timeframe") %in% modDef))) {
        expect_identical(slot(b@dependencies[[1]], modDef), moduleDefaults[[modDef]])
      } else if (modDef == "extent") {
        expect_equivalent(
          slot(b@dependencies[[1]], "spatialExtent"),
          eval(moduleDefaults[[modDef]])
        )
      } else if (modDef == "timeframe") {
        expect_identical(slot(b@dependencies[[1]], "timeframe"), eval(moduleDefaults[[modDef]]))
      }
    }
  })
})

test_that("newModule with events and functions", {
  testInit("ggplot2")
  nm <- "test"
  unlink(dir(Require::tempdir2(), pattern = nm, full.names = TRUE), recursive = TRUE)
  newModule(
    nm,
    path = Require::tempdir2(),
    open = FALSE,
    events = list(
      init = {
        sim <- Init(sim)
        sim <- scheduleEvent(sim, start(sim) + 1, moduleName = "test", eventType = "next1")
        sim <- scheduleEvent(sim, start(sim) + 1, moduleName = "test", eventType = "plot")
      },
      plot = {
        sim$d <- 33
        plotFun(sim)
        func()
        sim <- scheduleEvent(sim, time(sim) + 1, moduleName = "test", eventType = "plot")
      },
      next1 = {
        sim$b <- 2
        sim$a <- sim$a + 1
        sim <- Init2(sim)
      }
    ),
    func = function(x) {
      message("hi")
    },
    Init = function(sim) {
      sim$dd <- "no way"
      sim$a <- 1
      return(sim)
    },
    Init2 = function(sim) {
      a <- 1
      sim$dd <- "no way 2"
      sim$b <- max(sim$b, a) + 1
      return(sim)
    }
  )

  pdfFile <- tempfile(fileext = ".pdf")
  pdf(pdfFile)
  mess <- capture_messages(
    out <- simInitAndSpades(
      modules = "test",
      times = list(start = 0, end = 2),
      paths = list(modulePath = Require::tempdir2())
    )
  )
  dev.off()
  expect_true(file.exists(pdfFile))
  expect_true(file.size(pdfFile) > 0)
  unlink(pdfFile)

  expect_is(out, "simList")
  expect_true(out$a == 2)
  expect_true(out$b == 3)
  yrsSimulated <- (end(out) - start(out))
  expect_true(sum(grepl("hi", mess)) == yrsSimulated)
  expect_true(NROW(completed(out)) == yrsSimulated +
                (NROW(.coreModules()) - 1) + length(c(".inputObjects", "next1", "init")))
  expect_true(NROW(events(out)) == 1)
  expect_true(NROW(completed(out)[eventType == "next1"]) == 1)
  expect_true(NROW(completed(out)[eventType == "plot"]) == yrsSimulated)
})

test_that("newModule without path specified as arg", {
  testInit("ggplot2")
  nm <- "test"
  setPaths(modulePath = file.path(Require::tempdir2(), "lolololo"))
  unlink(dir(getPaths()$modulePath, pattern = nm, full.names = TRUE), recursive = TRUE)
  expect_false(file.exists(file.path(getPaths()$modulePath, nm, paste0(nm, ".R"))))
  newModule(
    nm,
    open = FALSE,
    events = list(
      init = {
        sim <- Init(sim)
      }
    ),
    Init = function(sim) {
      sim$dd <- "no way"
      sim$a <- 1
      return(sim)
    }
  )
  expect_true(file.exists(file.path(getPaths()$modulePath, nm, paste0(nm, ".R"))))

  mess <- capture_messages(
    out <- simInitAndSpades(module = "test", times = list(start = 0, end = 2))
  )
  expect_is(out, "simList")
  expect_identical(out$dd, "no way")
  expect_true(sum(grepl("init", mess)) == 1)

  unlink(dir(getPaths()$modulePath, pattern = nm, full.names = TRUE), recursive = TRUE)
  expect_false(file.exists(file.path(getPaths()$modulePath, nm, paste0(nm, ".R"))))
  newModule(nm, open = FALSE)
  expect_true(file.exists(file.path(getPaths()$modulePath, nm, paste0(nm, ".R"))))
})
