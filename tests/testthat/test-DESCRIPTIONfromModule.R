## DESCRIPTIONfromModule() translates SpaDES module metadata into a DESCRIPTION,
## so tooling that only understands packages -- R CMD check, pak, renv, the
## r-lib/actions CI steps -- can resolve a module's dependencies.

mkModule <- function(name, reqdPkgs, dir = tempfile(), version = "1.2.3",
                     description = 'paste("Line one.", "Line two.")') {
  md <- file.path(dir, name)
  dir.create(md, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(
  name = "%s",
  description = %s,
  keywords = "",
  authors = person("A", "B", email = "a@b.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(%s = "%s"),
  reqdPkgs = list(%s),
  parameters = rbind(),
  inputObjects = data.frame(),
  outputObjects = data.frame()
))
', name, description, name, version,
   paste0('"', reqdPkgs, '"', collapse = ", ")),
   file.path(md, paste0(name, ".R")))
  dir
}

test_that("Imports, Remotes and version inequalities come from reqdPkgs", {
  d <- mkModule("modA", c("data.table", "PredictiveEcology/Require@development (>= 1.0.0)"))
  on.exit(unlink(d, recursive = TRUE))

  f <- DESCRIPTIONfromModule("modA", modulePath = d, write = FALSE, verbose = 0)
  dcf <- read.dcf(f)

  expect_match(dcf[, "Imports"], "data.table")
  expect_match(dcf[, "Imports"], "Require \\(>= 1\\.0\\.0\\)")
  ## SpaDES.core is implicit for every module and must be present even though it
  ## is not in reqdPkgs.
  expect_match(dcf[, "Imports"], "SpaDES.core")
  ## A GitHub spec has to reach Remotes or pak cannot install it.
  expect_match(dcf[, "Remotes"], "PredictiveEcology/Require@development")
  ## ... and must not carry the version inequality into Remotes.
  expect_false(grepl(">=", dcf[, "Remotes"]))
})

test_that("Version is the module's own entry, not the deparsed list() call", {
  ## Regression: metadata holds `version = list(<module> = "x.y.z")` unevaluated.
  ## Pasting the call emitted two Version: lines ("list", then the number).
  d <- mkModule("modB", "data.table", version = "9.8.7")
  on.exit(unlink(d, recursive = TRUE))

  f <- DESCRIPTIONfromModule("modB", modulePath = d, write = FALSE, verbose = 0)
  expect_equal(unname(read.dcf(f)[, "Version"]), "9.8.7")
  expect_length(grep("^Version:", readLines(f)), 1L)
})

test_that("Description is evaluated, not deparsed", {
  ## Regression: `description = paste("a", "b")` is a call; deparsing leaked the
  ## function name in, producing "Description: paste Line one. Line two."
  d <- mkModule("modC", "data.table")
  on.exit(unlink(d, recursive = TRUE))

  f <- DESCRIPTIONfromModule("modC", modulePath = d, write = FALSE, verbose = 0)
  desc <- unname(read.dcf(f)[, "Description"])
  expect_equal(desc, "Line one. Line two.")
  expect_false(startsWith(desc, "paste"))
})

test_that("each module gets its own metadata, not the last one's", {
  ## Regression: a single shared `d` meant every file got the LAST module's
  ## metadata when several modules were written at once.
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE))
  mkModule("modD", "data.table", dir = d, version = "1.0.0")
  mkModule("modE", "terra",      dir = d, version = "2.0.0")

  fs <- DESCRIPTIONfromModule(c("modD", "modE"), modulePath = d, verbose = 0)
  expect_length(fs, 2L)
  vers <- vapply(fs, function(f) unname(read.dcf(f)[, "Version"]), character(1))
  expect_equal(unname(vers), c("1.0.0", "2.0.0"))
  expect_match(read.dcf(fs[1])[, "Imports"], "data.table")
  expect_match(read.dcf(fs[2])[, "Imports"], "terra")
})

test_that("singleDESCRIPTION aggregates modules into one file", {
  d <- tempfile(); on.exit(unlink(d, recursive = TRUE))
  mkModule("modF", c("data.table", "terra"), dir = d)
  mkModule("modG", c("data.table", "sf"),    dir = d)
  proj <- file.path(d, "proj"); dir.create(proj, recursive = TRUE)

  f <- DESCRIPTIONfromModule(c("modF", "modG"), modulePath = d, projectPath = proj,
                             singleDESCRIPTION = TRUE, verbose = 0)
  expect_length(f, 1L)
  imports <- read.dcf(f)[, "Imports"]
  for (p in c("data.table", "terra", "sf")) expect_match(imports, p)
  ## data.table is required by both modules but must appear once.
  expect_equal(unname(lengths(regmatches(imports, gregexpr("\\bdata\\.table\\b", imports)))), 1L)
})

test_that("merge keeps hand-added entries in an existing DESCRIPTION", {
  d <- mkModule("modH", "data.table")
  on.exit(unlink(d, recursive = TRUE))
  dFile <- file.path(d, "modH", "DESCRIPTION")

  DESCRIPTIONfromModule("modH", modulePath = d, verbose = 0)
  ## Someone adds a package by hand ...
  dcf <- read.dcf(dFile)
  dcf[, "Imports"] <- paste(dcf[, "Imports"], "handAdded", sep = ",\n    ")
  write.dcf(dcf, dFile)

  ## ... regenerating must not silently drop it.
  DESCRIPTIONfromModule("modH", modulePath = d, verbose = 0)
  expect_match(read.dcf(dFile)[, "Imports"], "handAdded")

  ## and merge = FALSE is the way to discard it deliberately
  DESCRIPTIONfromModule("modH", modulePath = d, merge = FALSE, verbose = 0)
  expect_false(grepl("handAdded", read.dcf(dFile)[, "Imports"]))
})

test_that("field overrides win over metadata", {
  d <- mkModule("modI", "data.table")
  on.exit(unlink(d, recursive = TRUE))
  f <- DESCRIPTIONfromModule("modI", modulePath = d, write = FALSE, verbose = 0,
                             package = "myPkg", version = "0.0.1", title = "T")
  dcf <- read.dcf(f)
  expect_equal(unname(dcf[, "Package"]), "myPkg")
  expect_equal(unname(dcf[, "Version"]), "0.0.1")
  expect_equal(unname(dcf[, "Title"]), "T")
})

test_that("module names with underscores become valid package names", {
  d <- mkModule("mod_with_underscores", "data.table")
  on.exit(unlink(d, recursive = TRUE))
  f <- DESCRIPTIONfromModule("mod_with_underscores", modulePath = d,
                             write = FALSE, verbose = 0)
  expect_equal(unname(read.dcf(f)[, "Package"]), "mod.with.underscores")
})

test_that("convertToPackage still produces a DESCRIPTION and the @import stub", {
  ## convertToPackage() had no tests, and it now delegates its DESCRIPTION half
  ## to DESCRIPTIONfromModule() while keeping the NAMESPACE @import stub of its
  ## own. Pin both halves.
  skip_if_not_installed("pkgload")
  skip_if_not_installed("roxygen2")

  d <- mkModule("modZ", "data.table")
  on.exit(unlink(d, recursive = TRUE))
  md <- file.path(d, "modZ")
  cat("\ndoEvent.modZ <- function(sim, eventTime, eventType) sim\n",
      file = file.path(md, "modZ.R"), append = TRUE)

  expect_no_error(convertToPackage(module = "modZ", path = d, buildDocuments = FALSE))

  dcf <- read.dcf(file.path(md, "DESCRIPTION"))
  expect_equal(unname(dcf[, "Package"]), "modZ")
  expect_equal(unname(dcf[, "Version"]), "1.2.3")
  expect_equal(unname(dcf[, "Description"]), "Line one. Line two.")
  expect_match(dcf[, "Imports"], "data.table")
  expect_match(dcf[, "Imports"], "SpaDES.core")

  imports <- readLines(file.path(md, "R", "imports.R"))
  expect_true(any(grepl("@import SpaDES.core", imports)))
  expect_true(any(grepl("@import data.table", imports)))
})

test_that("convertToPackage(destinationPath=) leaves the module untouched", {
  ## convertToPackage() is irreversible and rewrites in place, which makes it
  ## unusable for "convert, then test" workflows. destinationPath builds the
  ## package rendition in a throwaway directory instead.
  skip_if_not_installed("pkgload")
  skip_if_not_installed("roxygen2")

  d <- file.path(tempdir(), paste0("ctpDest", .rndstr(len = 4)))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(d, recursive = TRUE)
  withr::local_options(spades.moduleDocument = FALSE)
  suppressMessages(newModule("modDest", d, open = FALSE, unitTests = FALSE))
  src <- file.path(d, "modDest")

  before <- sort(list.files(src, recursive = TRUE, all.files = TRUE, no.. = TRUE))

  dest <- file.path(d, "built")
  pkg <- suppressMessages(suppressWarnings(
    convertToPackage("modDest", path = d, buildDocuments = FALSE, destinationPath = dest)))

  ## the source module is byte-for-byte the same set of files
  expect_identical(sort(list.files(src, recursive = TRUE, all.files = TRUE, no.. = TRUE)), before)
  expect_false(file.exists(file.path(src, "DESCRIPTION")))

  ## and the package rendition landed under destinationPath, whose path is returned
  expect_identical(normalizePath(pkg), normalizePath(file.path(dest, "modDest")))
  expect_true(file.exists(file.path(pkg, "DESCRIPTION")))
  expect_true(file.exists(file.path(pkg, "R", "imports.R")))
})

test_that("convertToPackage() refuses more than one module at a time", {
  expect_error(convertToPackage(c("a", "b"), path = tempdir()), "one module at a time")
})
