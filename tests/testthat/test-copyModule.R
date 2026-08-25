## copyModule(), newModuleCode() and newModuleDocumentation().
##
## newModule()/zipModule() are already covered by test-module-template.R, so
## this file sticks to the parts that were not.
##
## NOTE: openModules() and .fileEdit() are deliberately not tested -- they exist
## to launch an editor (file.edit / RStudio), so exercising them would open
## windows rather than assert anything.

test_that("copyModule copies a module under a new name", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("origMod", path = tmpdir, open = FALSE))
  suppressMessages(copyModule("origMod", "copiedMod", path = tmpdir))

  newDir <- file.path(tmpdir, "copiedMod")
  expect_true(dir.exists(newDir))
  ## the code file is renamed to match the new module name
  expect_true(file.exists(file.path(newDir, "copiedMod.R")))
  expect_false(file.exists(file.path(newDir, "origMod.R")))
})

test_that("copyModule brings the documentation across, renamed", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("docSrc", path = tmpdir, open = FALSE))
  suppressMessages(copyModule("docSrc", "docDst", path = tmpdir))

  newDir <- file.path(tmpdir, "docDst")
  expect_true(file.exists(file.path(newDir, "docDst.Rmd")))
  expect_false(file.exists(file.path(newDir, "docSrc.Rmd")))
})

test_that("copyModule creates the standard module subdirectories", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("dirSrc", path = tmpdir, open = FALSE))
  suppressMessages(copyModule("dirSrc", "dirDst", path = tmpdir))

  newDir <- file.path(tmpdir, "dirDst")
  expect_true(dir.exists(file.path(newDir, "data")))
  expect_true(dir.exists(file.path(newDir, "tests")))
  expect_true(dir.exists(file.path(newDir, "tests", "testthat")))
})

test_that("copyModule copies files from the module's data directory", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("dataSrc", path = tmpdir, open = FALSE))
  writeLines("some data", file.path(tmpdir, "dataSrc", "data", "aDataFile.txt"))

  suppressMessages(copyModule("dataSrc", "dataDst", path = tmpdir))

  expect_true(file.exists(file.path(tmpdir, "dataDst", "data", "aDataFile.txt")))
})

test_that("copyModule copies files from tests/testthat", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("testSrc", path = tmpdir, open = FALSE))
  ttDir <- checkPath(file.path(tmpdir, "testSrc", "tests", "testthat"), create = TRUE)
  writeLines("## a module test", file.path(ttDir, "test-something.R"))

  suppressMessages(copyModule("testSrc", "testDst", path = tmpdir))

  expect_true(file.exists(file.path(tmpdir, "testDst", "tests", "testthat",
                                    "test-something.R")))
})

test_that("copyModule copies files sitting directly in tests/", {
  skip_on_cran()
  testInit()

  ## the "tests" branch used to match "test" (singular), so a file directly in
  ## tests/ -- e.g. the unitTests.R that newModule(unitTests = TRUE) writes --
  ## was silently skipped, while tests/testthat/* copied fine
  suppressMessages(newModule("looseSrc", path = tmpdir, open = FALSE))
  writeLines("## a loose test file",
             file.path(tmpdir, "looseSrc", "tests", "aLooseTest.R"))

  suppressMessages(copyModule("looseSrc", "looseDst", path = tmpdir))

  expect_true(file.exists(file.path(tmpdir, "looseDst", "tests", "aLooseTest.R")))
})

test_that("copyModule creates the target subdirectories regardless of the working directory", {
  skip_on_cran()
  testInit()

  ## The existence check used to test a bare relative name against the working
  ## directory rather than the directory actually being created. testInit()
  ## sets the working directory to tmpdir, so keep the modules somewhere else
  ## for the decoy below to be a genuine decoy.
  modPath <- checkPath(file.path(tmpdir, "mods"), create = TRUE)
  suppressMessages(newModule("wdSrc", path = modPath, open = FALSE))

  ## an unrelated directory of the same name in the WORKING directory must not
  ## be mistaken for the target, which lives under modPath
  decoy <- file.path(getwd(), "wdDst")
  dir.create(decoy, showWarnings = FALSE)
  withr::defer(unlink(decoy, recursive = TRUE))

  suppressMessages(copyModule("wdSrc", "wdDst", path = modPath))

  expect_true(dir.exists(file.path(modPath, "wdDst", "data")))
  expect_true(dir.exists(file.path(modPath, "wdDst", "tests", "testthat")))
  expect_true(file.exists(file.path(modPath, "wdDst", "wdDst.R")))
})

test_that("copyModule returns TRUE when every file copied", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("retSrc", path = tmpdir, open = FALSE))
  res <- suppressMessages(copyModule("retSrc", "retDst", path = tmpdir))

  expect_true(res)
})

test_that("a copied module's code still parses", {
  skip_on_cran()
  testInit()

  suppressMessages(newModule("parseSrc", path = tmpdir, open = FALSE))
  suppressMessages(copyModule("parseSrc", "parseDst", path = tmpdir))

  f <- file.path(tmpdir, "parseDst", "parseDst.R")
  expect_true(file.exists(f))
  expect_silent(parse(f))
})

test_that("newModuleCode writes the module's .R file", {
  skip_on_cran()
  testInit()

  checkPath(file.path(tmpdir, "codeOnly"), create = TRUE)
  suppressMessages(newModuleCode("codeOnly", path = tmpdir, open = FALSE))

  f <- file.path(tmpdir, "codeOnly", "codeOnly.R")
  expect_true(file.exists(f))

  src <- paste(readLines(f), collapse = "\n")
  expect_match(src, "defineModule")
  expect_match(src, "codeOnly")
  expect_silent(parse(f))
})

test_that("newModuleDocumentation writes the module's .Rmd", {
  skip_on_cran()
  testInit()

  checkPath(file.path(tmpdir, "docsOnly"), create = TRUE)
  suppressMessages(newModuleDocumentation("docsOnly", path = tmpdir, open = FALSE))

  expect_true(file.exists(file.path(tmpdir, "docsOnly", "docsOnly.Rmd")))
})
