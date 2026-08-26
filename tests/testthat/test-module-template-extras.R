## module-template.R helpers that the existing tests do not reach: the file
## opener, openModules()'s five methods, zipModule() and checkModulePath().
##
## openModules() ends by handing every file it found to .fileEdit(), which
## launches an editor. That one call is mocked out so the file-selection logic
## -- which is the part with the branches -- can be exercised headlessly.

## ---- openIsRequested ----------------------------------------------------

test_that("openIsRequested is TRUE for TRUE and for a matching suffix", {
  testInit()

  expect_true(SpaDES.core:::openIsRequested(TRUE, "r"))
  expect_true(SpaDES.core:::openIsRequested("R", "r"))
  expect_true(SpaDES.core:::openIsRequested("Rmd", "md"))
})

test_that("openIsRequested is FALSE for FALSE and for a non-matching suffix", {
  testInit()

  expect_false(SpaDES.core:::openIsRequested(FALSE, "r"))
  expect_false(SpaDES.core:::openIsRequested("Rmd", "r"))
})

## ---- .fileEdit ----------------------------------------------------------

test_that(".fileEdit shows the file and says so under RStudio", {
  testInit()

  f <- withr::local_tempfile(fileext = ".R")
  writeLines("1 + 1", f)
  withr::local_envvar(c(RSTUDIO = "1"))

  msgs <- capture_messages(SpaDES.core:::.fileEdit(f))

  expect_match(paste(msgs, collapse = ""), "Using RStudio")
  expect_match(paste(msgs, collapse = ""), "file.edit(", fixed = TRUE)
})

test_that(".fileEdit strips a leading ./ from the path under RStudio", {
  testInit()

  d <- withr::local_tempdir()
  writeLines("1 + 1", file.path(d, "a.R"))
  owd <- setwd(d); withr::defer(setwd(owd))
  withr::local_envvar(c(RSTUDIO = "1"))

  msgs <- capture_messages(SpaDES.core:::.fileEdit("./a.R"))

  expect_match(paste(msgs, collapse = ""), "file.edit('a.R')", fixed = TRUE)
})

## ---- openModules --------------------------------------------------------

## a module tree with the shape openModules() expects: <path>/<mod>/<mod>.R
modTree <- function(dir, mods = c("modA", "modB")) {
  for (m in mods) {
    dir.create(file.path(dir, m, "tests", "testthat"), recursive = TRUE,
               showWarnings = FALSE)
    writeLines("## module code", file.path(dir, m, paste0(m, ".R")))
    writeLines("## a test", file.path(dir, m, "tests", "testthat", paste0("test-", m, ".R")))
  }
  dir
}

test_that("openModules opens every module's R file when asked for 'all'", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())
  opened <- character(0)

  testthat::with_mocked_bindings(
    .fileEdit = function(file) { opened <<- c(opened, file); invisible(NULL) },
    .package = "SpaDES.core",
    expect_null(openModules(name = "all", path = d))
  )

  expect_length(opened, 2L)
  expect_true(all(grepl("mod[AB]/mod[AB]\\.R$", opened)))
})

test_that("openModules skips files under a module's tests directory", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())
  opened <- character(0)

  testthat::with_mocked_bindings(
    .fileEdit = function(file) { opened <<- c(opened, file); invisible(NULL) },
    .package = "SpaDES.core",
    openModules(name = "all", path = d)
  )

  expect_false(any(grepl("tests", opened)))
})

test_that("openModules opens just the named module", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())
  opened <- character(0)

  testthat::with_mocked_bindings(
    .fileEdit = function(file) { opened <<- c(opened, file); invisible(NULL) },
    .package = "SpaDES.core",
    openModules(name = "modA", path = d)
  )

  expect_length(opened, 1L)
  expect_match(opened, "modA/modA\\.R$")
})

test_that("openModules refuses a mix of file types", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())

  expect_error(openModules(name = c("a.R", "b.Rmd"), path = d),
               "Can only open one file type at a time")
})

test_that("openModules leaves the working directory where it found it", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())
  owd <- getwd()

  testthat::with_mocked_bindings(
    .fileEdit = function(file) invisible(NULL),
    .package = "SpaDES.core",
    openModules(name = "all", path = d)
  )

  expect_identical(getwd(), owd)
})

test_that("openModules defaults its path to the module path", {
  skip_on_cran()
  testInit()

  d <- modTree(withr::local_tempdir())
  withr::local_options(list(spades.modulePath = d))
  opened <- character(0)

  testthat::with_mocked_bindings(
    .fileEdit = function(file) { opened <<- c(opened, file); invisible(NULL) },
    .package = "SpaDES.core",
    {
      openModules(path = d)          # name missing
      openModules(name = "modA")     # path missing
    }
  )

  expect_true(length(opened) >= 3L)
})

test_that("openModules takes the modules and path from a simList", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(simInit(
    times = list(start = 0, end = 1, timeunit = "year"),
    params = list(.globals = list(stackName = "landscape", burnStats = "npixelsburned")),
    modules = list("randomLandscapes", "fireSpread"),
    paths = list(modulePath = mp)))

  opened <- character(0)
  testthat::with_mocked_bindings(
    .fileEdit = function(file) { opened <<- c(opened, file); invisible(NULL) },
    .package = "SpaDES.core",
    openModules(sim)
  )

  expect_true(any(grepl("randomLandscapes\\.R$", opened)))
  expect_true(any(grepl("fireSpread\\.R$", opened)))
})

## ---- zipModule ----------------------------------------------------------

test_that("zipModule bundles a module into a versioned zip inside the module dir", {
  skip_on_cran()
  testInit()

  d <- withr::local_tempdir()
  dir.create(file.path(d, "modA", "data"), recursive = TRUE)
  writeLines("## code", file.path(d, "modA", "modA.R"))
  writeLines("a", file.path(d, "modA", "data", "CHECKSUMS.txt"))
  writeLines("payload", file.path(d, "modA", "data", "big.tif"))

  suppressMessages(zipModule(name = "modA", path = d, version = "1.2.3", data = TRUE))

  zf <- file.path(d, "modA", "modA_1.2.3.zip")
  expect_true(file.exists(zf))
  ## the zip was moved into the module directory, not left beside it
  expect_false(file.exists(file.path(d, "modA_1.2.3.zip")))

  contents <- unzip(zf, list = TRUE)$Name
  expect_true(any(grepl("modA\\.R$", contents)))
  expect_true(any(grepl("big\\.tif$", contents)))
})

test_that("zipModule with data = FALSE keeps CHECKSUMS.txt but drops the data files", {
  skip_on_cran()
  testInit()

  d <- withr::local_tempdir()
  dir.create(file.path(d, "modA", "data"), recursive = TRUE)
  writeLines("## code", file.path(d, "modA", "modA.R"))
  writeLines("a", file.path(d, "modA", "data", "CHECKSUMS.txt"))
  writeLines("payload", file.path(d, "modA", "data", "big.tif"))

  suppressMessages(zipModule(name = "modA", path = d, version = "1.2.3", data = FALSE))

  contents <- unzip(file.path(d, "modA", "modA_1.2.3.zip"), list = TRUE)$Name
  expect_true(any(grepl("CHECKSUMS\\.txt$", contents)))
  expect_false(any(grepl("big\\.tif$", contents)))
})

test_that("zipModule leaves the working directory where it found it", {
  skip_on_cran()
  testInit()

  d <- withr::local_tempdir()
  dir.create(file.path(d, "modA"), recursive = TRUE)
  writeLines("## code", file.path(d, "modA", "modA.R"))
  owd <- getwd()

  suppressMessages(zipModule(name = "modA", path = d, version = "0.0.1", data = TRUE))

  expect_identical(getwd(), owd)
})

## ---- checkModulePath ----------------------------------------------------

test_that("checkModulePath returns '.' when the module path is still the default", {
  testInit()

  withr::local_options(list(spades.modulePath = spadesOptions()[["spades.modulePath"]]))

  expect_identical(SpaDES.core:::checkModulePath(), ".")
})

test_that("checkModulePath returns the module path once it has been set", {
  testInit()

  d <- withr::local_tempdir()
  withr::local_options(list(spades.modulePath = d))

  expect_identical(normPath(SpaDES.core:::checkModulePath()), normPath(d))
})
