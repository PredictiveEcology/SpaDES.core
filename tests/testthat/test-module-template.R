test_that("module templates work", {
  testInitOut <- testInit("knitr", smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  expect_true(file.exists(tmpdir))
  moduleName <- "myModule"

  newModule(moduleName, tmpdir, open = FALSE, unitTests = TRUE)

  mpath <- file.path(tmpdir, moduleName)

  expect_true(file.exists(mpath))
  expect_true(file.exists(file.path(mpath, "citation.bib")))
  expect_true(file.exists(file.path(mpath, "LICENSE")))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".R"))))
  expect_true(file.exists(file.path(mpath, paste0(moduleName, ".Rmd"))))
  expect_true(file.exists(file.path(mpath, "README.txt")))
  expect_true(dir.exists(file.path(mpath, "data")))
  expect_true(dir.exists(file.path(mpath, "tests")))
  expect_true(dir.exists(file.path(mpath, "tests", "testthat")))
  expect_true(file.exists(file.path(mpath, "tests", "unitTests.R")))
  expect_true(file.exists(file.path(mpath, "tests", "testthat", "test-template.R")))
  expect_true(file.exists(file.path(mpath, "data", "CHECKSUMS.txt")))

  utils::capture.output(
    zipModule(name = moduleName, path = tmpdir, version = "0.0.2", flags = "-q -r9X")
  )

  expect_true(file.exists(file.path(mpath, paste0(moduleName, "_0.0.2.zip"))))

  # Test that the .Rmd file actually can run with knitr
  expect_equal(knitr::knit(input = file.path(mpath, paste0(moduleName, ".Rmd")),
                           output = file.path(mpath, paste0(moduleName, ".md")),
                           quiet = TRUE),
               file.path(mpath, paste0(moduleName, ".md")))

  # Test that the dummy unit tests work
  #test_file(file.path(mpath, "tests", "testthat", "test-template.R"))
})

test_that("empty defineModule", {
  testInitOut <- testInit("knitr", smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  sim <- simInit()
  sim <- expect_warning(defineModule(sim, list()))
  b <- depends(sim)
  out <- lapply(names(moduleDefaults), function(modDef) {
    if (modDef != "version") {
      if (all(!(c("extent", "timeframe") %in% modDef))) {
        expect_identical(slot(b@dependencies[[1]], modDef), moduleDefaults[[modDef]])
      } else if (modDef == "extent") {
        expect_identical(slot(b@dependencies[[1]], "spatialExtent"), eval(moduleDefaults[[modDef]]))
      } else if (modDef == "timeframe") {
        expect_identical(slot(b@dependencies[[1]], "timeframe"), eval(moduleDefaults[[modDef]]))
      }
    }
  })
})
