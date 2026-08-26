## The small, pure helpers behind saveSimList()/loadSimList(): archive name
## handling, path relativisation, the deep environment cloner and the anchor
## list offered to reproducible's .wrap()/.unwrap().

## ---- archive name handling ---------------------------------------------

test_that("checkArchiveAlternative returns the filename when it exists", {
  testInit()

  f <- withr::local_tempfile(fileext = ".qs2")
  writeLines("x", f)

  expect_identical(SpaDES.core:::checkArchiveAlternative(f), f)
})

test_that("checkArchiveAlternative finds a sibling archive when the file is missing", {
  testInit()

  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "mySim.tar.gz"))

  out <- SpaDES.core:::checkArchiveAlternative(file.path(d, "mySim.qs2"))

  expect_identical(normPath(out), normPath(file.path(d, "mySim.tar.gz")))
})

test_that("checkArchiveAlternative leaves the name alone when no archive is there", {
  testInit()

  d <- withr::local_tempdir()
  missing <- file.path(d, "mySim.qs2")

  expect_identical(SpaDES.core:::checkArchiveAlternative(missing), missing)
})

test_that("archiveConvertFileExt swaps the extension", {
  testInit()

  expect_identical(SpaDES.core:::archiveConvertFileExt("/a/b/sim.zip", "tar.gz"),
                   "/a/b/sim.tar.gz")
  expect_identical(SpaDES.core:::archiveConvertFileExt("/a/b/sim.qs2", "zip"),
                   "/a/b/sim.zip")

  ## NOTE: not asserted here -- converting "sim.tar.gz" to "zip" currently gives
  ## "sim.tar.zip", because tools::file_ext() sees only the "gz". The gsub() is
  ## also unanchored, so a directory named e.g. "gz" would be rewritten too.
  ## Both look wrong, so neither is pinned down by a test until it is decided.
})

test_that("archiveConvertFileExt leaves a tar.gz name alone when converting to tar.gz", {
  testInit()

  ## gsub() on file_ext() would turn "sim.tar.gz" into "sim.tar.tar.gz"
  expect_identical(SpaDES.core:::archiveConvertFileExt("/a/b/sim.tar.gz", "tar.gz"),
                   "/a/b/sim.tar.gz")
})

test_that("checkSimListExts accepts the supported extensions and rejects others", {
  testInit()

  for (f in c("a.qs2", "a.rds", "a.zip", "a.tar", "a.tar.gz", "a.RDS"))
    expect_silent(SpaDES.core:::checkSimListExts(f))

  expect_error(SpaDES.core:::checkSimListExts("a.txt"))
})

test_that("archiveWrite and archiveExtract round-trip files relative to projectPath", {
  skip_on_cran()
  ## archiveWrite()/archiveExtract() only take the archive:: path when that
  ## package is available and we are not on Windows; otherwise they shell out to
  ## zip()/unzip(), which needs an external zip binary and writes a differently
  ## named file. Only the archive:: path is exercised here.
  skip_if_not_installed("archive")
  skip_on_os("windows")
  testInit()

  proj <- withr::local_tempdir()
  dir.create(file.path(proj, "outputs"))
  writeLines("hello", file.path(proj, "outputs", "a.txt"))
  writeLines("world", file.path(proj, "sim.qs2"))

  arch <- file.path(withr::local_tempdir(), "bundle.tar.gz")
  SpaDES.core:::archiveWrite(arch, c("sim.qs2", file.path("outputs", "a.txt")),
                             verbose = -1, projectPath = proj)

  arch <- SpaDES.core:::archiveConvertFileExt(arch, "tar.gz")
  expect_true(file.exists(arch))
  ## writing must not leave the session in projectPath
  expect_false(identical(normPath(getwd()), normPath(proj)))

  exdir <- withr::local_tempdir()
  owd <- setwd(exdir); withr::defer(setwd(owd))
  out <- SpaDES.core:::archiveExtract(arch, exdir = exdir)

  expect_true(any(grepl("a\\.txt$", out)))
  expect_true(any(grepl("sim\\.qs2$", out)))
})

## ---- deprecation text ---------------------------------------------------

test_that("warnDeprecFileBacked returns the right text for each deprecated arg", {
  testInit()

  expect_match(SpaDES.core:::warnDeprecFileBacked("fileBackedDir"), "use projectPath")
  expect_match(SpaDES.core:::warnDeprecFileBacked("fileBackend"), "file-backed objects are")
})

test_that("warnDeprecFileBacked errors on an unknown arg", {
  testInit()

  expect_error(SpaDES.core:::warnDeprecFileBacked("nonsense"),
               "No deprecation warning with that arg")
})

test_that("unzipSimList is deprecated in favour of loadSimList", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim$a <- 1
  f <- file.path(tmpdir, "sim.qs2")
  suppressMessages(saveSimList(sim, f))

  expect_warning(out <- suppressMessages(SpaDES.core:::unzipSimList(f)), "deprecated")
  expect_s4_class(out, "simList")
})

## ---- path relativisation ------------------------------------------------

test_that("relativizePaths makes the core paths relative to projectPath", {
  testInit()

  proj <- normPath(withr::local_tempdir())
  paths <- list(cachePath = file.path(proj, "cache"),
                inputPath = file.path(proj, "inputs"),
                modulePath = file.path(proj, "modules"),
                outputPath = file.path(proj, "outputs"),
                rasterPath = file.path(proj, "scratch", "raster"),
                scratchPath = file.path(proj, "scratch"),
                terraPath = file.path(proj, "scratch", "terra"))

  p <- SpaDES.core:::relativizePaths(paths, projectPath = proj)

  expect_identical(unname(p[["modulePath"]]), "modules")
  expect_identical(unname(p[["outputPath"]]), "outputs")
  ## the tmp paths are relative to scratchPath, not to projectPath
  expect_identical(unname(p[["rasterPath"]]), "raster")
  expect_identical(unname(p[["terraPath"]]), "terra")
})

test_that("relativizePaths infers projectPath from modulePath when not given", {
  testInit()

  proj <- normPath(withr::local_tempdir())
  paths <- list(cachePath = file.path(proj, "cache"),
                inputPath = file.path(proj, "inputs"),
                modulePath = file.path(proj, "modules"),
                outputPath = file.path(proj, "outputs"),
                rasterPath = file.path(proj, "scratch", "raster"),
                scratchPath = file.path(proj, "scratch"),
                terraPath = file.path(proj, "scratch", "terra"))

  p <- SpaDES.core:::relativizePaths(paths)

  expect_identical(unname(p[["modulePath"]]), "modules")
})

test_that("absolutizePaths undoes relativizePaths", {
  testInit()

  proj <- normPath(withr::local_tempdir())
  scratch <- file.path(proj, "scratch")
  paths <- list(cachePath = file.path(proj, "cache"),
                inputPath = file.path(proj, "inputs"),
                modulePath = file.path(proj, "modules"),
                outputPath = file.path(proj, "outputs"),
                rasterPath = file.path(scratch, "raster"),
                scratchPath = scratch,
                terraPath = file.path(scratch, "terra"))

  rel <- SpaDES.core:::relativizePaths(paths, projectPath = proj)
  back <- SpaDES.core:::absolutizePaths(rel, projectPath = proj, tempdir = scratch)

  for (n in names(paths))
    expect_identical(normPath(back[[n]]), normPath(paths[[n]]))
})

## ---- .cloneEnvDeep / .cloneSimEnvs --------------------------------------

test_that(".cloneEnvDeep passes non-environments straight through", {
  testInit()

  expect_identical(SpaDES.core:::.cloneEnvDeep(1:3), 1:3)
  expect_identical(SpaDES.core:::.cloneEnvDeep("a"), "a")
})

test_that(".cloneEnvDeep copies bindings into a new environment", {
  testInit()

  e <- new.env(parent = emptyenv())
  e$a <- 1
  e$b <- "two"

  out <- SpaDES.core:::.cloneEnvDeep(e)

  expect_false(identical(out, e))
  expect_setequal(ls(out), c("a", "b"))
  expect_identical(out$a, 1)
  ## a real copy: changing one must not change the other
  out$a <- 99
  expect_identical(e$a, 1)
})

test_that(".cloneEnvDeep recurses into nested environments", {
  testInit()

  e <- new.env(parent = emptyenv())
  e$inner <- new.env(parent = emptyenv())
  e$inner$x <- 1

  out <- SpaDES.core:::.cloneEnvDeep(e)

  expect_false(identical(out$inner, e$inner))
  expect_identical(out$inner$x, 1)
  out$inner$x <- 42
  expect_identical(e$inner$x, 1)
})

test_that(".cloneEnvDeep survives a cycle and preserves the sharing", {
  testInit()

  e <- new.env(parent = emptyenv())
  e$self <- e
  e$x <- 1

  out <- SpaDES.core:::.cloneEnvDeep(e)

  ## the cycle guard means the clone points at itself, not at the original
  expect_identical(out$self, out)
  expect_false(identical(out$self, e))
})

test_that(".cloneEnvDeep keeps active bindings active", {
  testInit()

  e <- new.env(parent = emptyenv())
  makeActiveBinding("live", function() 7, e)

  out <- SpaDES.core:::.cloneEnvDeep(e)

  expect_true(bindingIsActive("live", out))
  expect_identical(out$live, 7)
})

test_that(".cloneEnvDeep preserves attributes and the parent environment", {
  testInit()

  p <- new.env(parent = emptyenv())
  e <- new.env(parent = p)
  attr(e, "myAttr") <- "kept"

  out <- SpaDES.core:::.cloneEnvDeep(e)

  expect_identical(attr(out, "myAttr"), "kept")
  expect_identical(parent.env(out), p)
})

test_that(".cloneSimEnvs detaches the sim's environments from the original", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim$a <- 1

  out <- SpaDES.core:::.cloneSimEnvs(sim)

  expect_false(identical(out@.xData, sim@.xData))
  expect_identical(out@.envir, out@.xData)
  expect_identical(out$a, 1)

  out$a <- 99
  expect_identical(sim$a, 1)
})

## ---- .wrapAnchors --------------------------------------------------------

test_that(".wrapAnchors is just the sim's paths when no projectPath is given", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  expect_identical(SpaDES.core:::.wrapAnchors(sim), paths(sim))
})

test_that(".wrapAnchors adds projectPath when it differs from the working directory", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  proj <- withr::local_tempdir()

  anchors <- SpaDES.core:::.wrapAnchors(sim, projectPath = proj)

  expect_true("projectPath" %in% names(anchors))
  expect_identical(anchors[["projectPath"]], proj)
})

test_that(".wrapAnchors does not add projectPath when it is the working directory", {
  skip_on_cran()
  testInit()

  ## reproducible always appends its own `getwd` anchor; adding a second anchor
  ## for the same directory makes which one wins order-dependent
  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  anchors <- SpaDES.core:::.wrapAnchors(sim, projectPath = getwd())

  expect_false("projectPath" %in% names(anchors))
  expect_identical(anchors, paths(sim))
})

test_that(".wrapAnchors ignores an empty projectPath", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))

  expect_identical(SpaDES.core:::.wrapAnchors(sim, projectPath = ""), paths(sim))
})

## ---- recoverDataTableFromQs ---------------------------------------------

test_that("recoverDataTableFromQs is a no-op when nothing needs converting", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 1, timeunit = "year"))
  sim$a <- data.table::data.table(x = 1:3)

  out <- suppressMessages(SpaDES.core:::recoverDataTableFromQs(sim))

  expect_s4_class(out, "simList")
  expect_s3_class(out$a, "data.table")
})
