## A cached simList's entry must name its files, as a cached list's does. reproducible's .wrap.list() keeps
## the tags of what it wraps (a file-backed raster's origFilename, filenamesInCache, ...), and Cache() records
## them on the entry; .wrap.simList() wrapped the objects the same way but dropped those tags in list2env(),
## so showCache()/clearCache() could not see the files of, e.g., a cached .inputObjects (FireSense, 2026-09-24).

test_that("a cached .inputObjects with a file-backed raster tags its file, and clearCache removes it", {
  skip_if_not_installed("terra")
  testInit("terra")
  m <- "fbIO"
  dir.create(file.path(tmpdir, m), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    'defineModule(sim, list(name = "fbIO", description = "", keywords = "", authors = person("a", "b"),',
    '  childModules = character(0), version = list(fbIO = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),',
    '  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list("terra"), parameters = rbind(),',
    '  inputObjects = bindrows(expectsInput("rstLCC", "list", "")), outputObjects = bindrows()))',
    'doEvent.fbIO <- function(sim, eventTime, eventType) invisible(sim)',
    '.inputObjects <- function(sim) {',
    '  f <- file.path(outputPath(sim), "rstLCC1985_4.2.1.tif")',
    '  terra::writeRaster(terra::rast(nrows = 5, ncols = 5, vals = 1), f, overwrite = TRUE)',
    '  sim$rstLCC <- list(year1985 = terra::rast(f))',
    '  sim',
    '}'), file.path(tmpdir, m, paste0(m, ".R")))
  cp <- file.path(tmpdir, "cacheWST")
  sim <- simInit(modules = m, params = list(fbIO = list(.useCache = ".inputObjects")),
                 paths = list(modulePath = tmpdir, cachePath = cp, outputPath = file.path(tmpdir, "outWST")))
  sc <- showCache(cp, verbose = -2)
  id <- unique(sc$cacheId[sc$tagKey == "function" & grepl("inputObjects", sc$tagValue)])
  expect_length(id, 1L)
  entry <- sc[cacheId == id]
  expect_true("rstLCC1985_4.2.1.tif" %in% entry$tagValue[entry$tagKey == "origFilename"])
  expect_true(any(grepl("rstLCC1985_4.2.1.tif", entry$tagValue[entry$tagKey == "filenamesInCache"])))

  storage <- reproducible::CacheStorageDir(cp)
  expect_true(length(dir(storage, pattern = paste0("^", id, "_"))) > 0)
  clearCache(cp, cacheId = id, ask = FALSE, verbose = -2)
  expect_length(dir(storage, pattern = paste0("^", id)), 0L)
})
