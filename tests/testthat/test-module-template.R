test_that("module templates work", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("knitr")
  skip_if_not_installed("rmarkdown")

  testInitOut <- testInit(c("knitr", "rmarkdown"), smcc = FALSE)

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
  expect_true(
    utils::file_test("-h", file.path(mpath, "README.md")) | file.exists(file.path(mpath, "README.md"))
  )
  expect_true(dir.exists(file.path(mpath, ".github")))
  expect_true(dir.exists(file.path(mpath, ".github", "workflows")))
  expect_true(file.exists(file.path(mpath, ".github", "workflows", "render-module-rmd.yaml")))

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

  # Test that the .Rmd file actually can run with knitr
  expect_equal(knitr::knit(input = file.path(mpath, paste0(moduleName, ".Rmd")),
                           output = file.path(mpath, paste0(moduleName, ".md")),
                           quiet = TRUE),
               file.path(mpath, paste0(moduleName, ".md")))
  expect_true(file.exists(file.path(mpath, "README.md"))) ## file should exist now, post-knit

  # Test that the dummy unit tests work
  #test_file(file.path(mpath, "tests", "testthat", "test-template.R")) # TODO: make it work
})

test_that("empty defineModule", {
  testInitOut <- testInit()

  sim <- simInit()
  sim <- expect_warning(defineModule(sim, list()))
  b <- depends(sim)
  out <- lapply(names(moduleDefaults), function(modDef) {
    if (modDef != "version") {
      if (all(!(c("extent", "timeframe") %in% modDef))) {
        expect_identical(slot(b@dependencies[[1]], modDef), moduleDefaults[[modDef]])
      } else if (modDef == "extent") {
        expect_equivalent(slot(b@dependencies[[1]], "spatialExtent"), eval(moduleDefaults[[modDef]]))
      } else if (modDef == "timeframe") {
        expect_identical(slot(b@dependencies[[1]], "timeframe"), eval(moduleDefaults[[modDef]]))
      }
    }
  })
})
