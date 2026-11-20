## Caching a whole simInit(), spades() or simInitAndSpades() with `cache =` (or `.useCache =`):
## the key includes the modules' code, so editing a module reruns it. See R/cacheWholeRun.R.

## A module whose init draws a number (so a rerun is visible) and stores it as `zz`
wrMod <- function(modulePath, name = "wr1", extra = "") {
  newModule(name, modulePath, open = FALSE)
  code <- sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, "")),
  inputObjects = bindrows(), outputObjects = bindrows(createsOutput("zz", "numeric", ""))))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") { sim$zz <- runif(1) %s }
  sim
}
', name, name, name, extra)
  cat(code, file = file.path(modulePath, name, paste0(name, ".R")))
}

wrArgs <- function(tmpdir, ...) {
  list(modules = "wr1", times = list(start = 0, end = 1),
       paths = list(modulePath = file.path(tmpdir, "modules"), outputPath = file.path(tmpdir, "outputs"),
                    inputPath = file.path(tmpdir, "inputs"), cachePath = file.path(tmpdir, "cache")),
       ...)
}

test_that("simInitAndSpades(cache = TRUE) reruns when module code changes; .useCache is the same", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))

  s1 <- simInitAndSpades2(wrArgs(tmpdir, cache = TRUE))
  s2 <- simInitAndSpades2(wrArgs(tmpdir, .useCache = TRUE))
  expect_true(.newlyCached(s1))
  expect_true(.loadedFromCache(s2))
  expect_identical(s1$zz, s2$zz)
  expect_false(exists(".useCache", envir = envir(s2), inherits = FALSE))

  ## an edit to the module's code is a new key, although `modules` is unchanged
  wrMod(file.path(tmpdir, "modules"), extra = "; sim$edited <- TRUE")
  s3 <- simInitAndSpades2(wrArgs(tmpdir, cache = TRUE))
  expect_true(.newlyCached(s3))
  expect_true(s3$edited)

  ## so is an edit to a non-R code file in the module; a file under data/ is not
  writeLines("x = 1", file.path(tmpdir, "modules", "wr1", "helper.py"))
  expect_true(.newlyCached(simInitAndSpades2(wrArgs(tmpdir, cache = TRUE))))
  writeLines("a,b", file.path(tmpdir, "modules", "wr1", "data", "d.csv"))
  expect_true(.loadedFromCache(simInitAndSpades2(wrArgs(tmpdir, cache = TRUE))))
})

test_that("paths are not in the whole-run cache key, as in a simList's; params are", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))
  withTmp <- function(d) {
    a <- wrArgs(tmpdir, cache = TRUE)
    a$paths[c("scratchPath", "terraPath", "rasterPath")] <-
      as.list(file.path(tmpdir, d, c("scratch", "terra", "raster")))
    a
  }

  expect_true(.newlyCached(simInitAndSpades2(withTmp("session1"))))
  expect_true(.loadedFromCache(simInitAndSpades2(withTmp("session2"))))
  a <- withTmp("session2")
  a$paths$outputPath <- file.path(tmpdir, "outputs2")
  expect_true(.loadedFromCache(simInitAndSpades2(a)))
  ## the key is the simList digest's: params, sorted, are in it
  a$params <- list(wr1 = list(.useCache = FALSE, .plots = NA))
  expect_true(.newlyCached(simInitAndSpades2(a)))
  a$params <- list(wr1 = list(.plots = NA, .useCache = FALSE))
  expect_true(.loadedFromCache(simInitAndSpades2(a)))
})

test_that("simInit(cache = TRUE) caches the whole simInit, keyed on module code", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))

  expect_true(.newlyCached(simInit2(wrArgs(tmpdir, cache = TRUE))))
  s2 <- simInit2(wrArgs(tmpdir, .useCache = "local"))
  expect_true(.loadedFromCache(s2))
  expect_false(exists(".useCache", envir = envir(s2), inherits = FALSE))
  expect_false(exists("cache", envir = envir(s2), inherits = FALSE))

  wrMod(file.path(tmpdir, "modules"), extra = "; sim$edited <- TRUE")
  expect_true(.newlyCached(simInit2(wrArgs(tmpdir, cache = TRUE))))

  ## not caching: neither name lands in the simList as an object
  s4 <- simInit2(wrArgs(tmpdir, .useCache = FALSE))
  expect_false(exists(".useCache", envir = envir(s4), inherits = FALSE))
})

test_that("spades() takes cache = \"local\" and .useCache", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))
  sim <- simInit2(wrArgs(tmpdir))

  s1 <- spades(Copy(sim), cache = "local")
  s2 <- spades(Copy(sim), .useCache = TRUE)
  expect_true(.newlyCached(s1))
  expect_true(.loadedFromCache(s2))
  expect_identical(s1$zz, s2$zz)
})

test_that("bad or conflicting cache values are errors", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))

  expect_error(simInitAndSpades2(wrArgs(tmpdir, cache = TRUE, .useCache = "portable")), "disagree")
  expect_error(simInit2(wrArgs(tmpdir, cache = "yes")), "must be TRUE, FALSE")
})

test_that("a local cache hit whose output files are gone warns and returns the simList", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)
  wrMod(file.path(tmpdir, "modules"))
  args <- wrArgs(tmpdir, cache = TRUE, outputs = data.frame(objectName = "zz", saveTime = 1))

  s1 <- simInitAndSpades2(args)
  outFile <- outputs(s1)$file[outputs(s1)$saved %in% TRUE]
  expect_true(all(file.exists(outFile)))
  unlink(outFile)
  expect_warning(s2 <- simInitAndSpades2(args), "no longer on disk")
  expect_true(.loadedFromCache(s2))
  expect_identical(s1$zz, s2$zz)
})

test_that("cache = \"portable\" brings the outputs to a copy of the project elsewhere", {
  skip_if_not_installed("archive")
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  proj1 <- file.path(tmpdir, "proj1")
  proj2 <- file.path(tmpdir, "proj2")
  dir.create(proj1)
  dir.create(proj2)
  withr::local_dir(proj1)
  withr::local_options(reproducible.cachePath = "cache")
  wrMod(file.path(proj1, "modules"))
  args <- list(modules = "wr1", times = list(start = 0, end = 1), cache = "portable",
               paths = list(modulePath = "modules", outputPath = "outputs", inputPath = "inputs",
                            cachePath = "cache"),
               outputs = data.frame(objectName = "zz", saveTime = 1))

  s1 <- simInitAndSpades2(args)
  expect_true(.newlyCached(s1))
  expect_length(dir(reproducible::CacheStorageDir("cache"), pattern = "_portable\\."), 1)

  ## the project (modules and cache, no outputs) moved to another folder
  file.copy(c("modules", "cache"), proj2, recursive = TRUE)
  setwd(proj2)
  s2 <- simInitAndSpades2(args)
  expect_true(.loadedFromCache(s2))
  expect_identical(s1$zz, s2$zz)
  expect_true(file.exists(file.path(proj2, "outputs", basename(outputs(s1)$file[1]))))

  ## the archive belongs to the entry
  clearCache("cache", ask = FALSE)
  expect_length(dir(reproducible::CacheStorageDir("cache"), pattern = "_portable\\."), 0)
})

test_that("saveSimList archives outputs when the simList has no file-backed object", {
  skip_if_not_installed("archive")
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_dir(tmpdir)
  wrMod(file.path(tmpdir, "modules"))
  sim <- simInitAndSpades2(wrArgs(tmpdir, outputs = data.frame(objectName = "zz", saveTime = 1)))
  outFile <- outputs(sim)$file[outputs(sim)$saved %in% TRUE]

  saveSimList(sim, file.path(tmpdir, "s.qs2"), projectPath = tmpdir)
  arch <- dir(tmpdir, pattern = "^s\\.(tar\\.gz|zip)$", full.names = TRUE)
  expect_length(arch, 1)
  expect_true(any(endsWith(archive::archive(arch)$path, basename(outFile))))
})

test_that("moduleCodeFiles follows childModules and lists non-R code files", {
  testInit(smcc = FALSE)
  mp <- file.path(tmpdir, "modules")
  wrMod(mp, "kid")
  newModule("parent", mp, type = "parent", children = "kid", open = FALSE)
  writeLines("x = 1", file.path(mp, "kid", "helper.py"))
  writeLines("x = 1", file.path(mp, "kid", "data", "ignored.py"))

  files <- moduleCodeFiles(list(modulePath = mp), "parent")
  expect_false(any(grepl("kid", files)))

  files <- moduleCodeFiles(list(modulePath = mp), "parent", childModules = TRUE, nonR = TRUE)
  expect_true(any(endsWith(files, file.path("kid", "kid.R"))))
  expect_true(any(endsWith(files, file.path("kid", "helper.py"))))
  expect_false(any(grepl("ignored.py", files, fixed = TRUE)))
})
