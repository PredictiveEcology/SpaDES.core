## simInit() stores its own resume state on the live sim at
## sim@.xData[["._simInitContext"]] (R/simulation-simInit.R) while `.inputObjects` events
## run. .wrap.simList() (R/cache.R) used to copy that state, unwrapped, into a cached
## `.inputObjects` entry -- so a terra object passed to simInit() via `objects =` (or
## `inputs =`) was saved raw, not `.wrap()`ed. Reloading that entry after a serialization
## round trip (a fresh `loadFromCache()` read) then produced a SpatRaster/SpatVector with a
## dead external pointer: "external pointer is not valid" (2026-09-26).

test_that("a cached .inputObjects entry with a raster in `objects=` reloads without a dead pointer, and drops ._simInitContext", {
  skip_if_not_installed("terra")
  testInit("terra", opts = list(reproducible.useMemoise = FALSE))
  m <- "ctxIO"
  dir.create(file.path(tmpdir, m), recursive = TRUE, showWarnings = FALSE)
  ## "dummy" is a required input the user does NOT supply, so `.inputObjects` actually runs
  ## (and gets cached); "rstLCC" is an unrelated object passed via `objects=` that lands in
  ## simInit()'s ._simInitContext$objects regardless of which module needs it.
  writeLines(c(
    'defineModule(sim, list(name = "ctxIO", description = "", keywords = "", authors = person("a", "b"),',
    '  childModules = character(0), version = list(ctxIO = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),',
    '  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list("terra"), parameters = rbind(),',
    '  inputObjects = bindrows(expectsInput("dummy", "numeric", "")), outputObjects = bindrows()))',
    'doEvent.ctxIO <- function(sim, eventTime, eventType) invisible(sim)',
    '.inputObjects <- function(sim) { sim$dummy <- 1; sim }'), file.path(tmpdir, m, paste0(m, ".R")))

  cp <- file.path(tmpdir, "cacheCtx")
  sim <- simInit(modules = m, params = list(ctxIO = list(.useCache = ".inputObjects")),
                 objects = list(rstLCC = terra::rast(nrows = 5, ncols = 5, vals = 1)),
                 paths = list(modulePath = tmpdir, cachePath = cp, outputPath = file.path(tmpdir, "outCtx")))

  sc <- showCache(cp, verbose = -2)
  id <- unique(sc$cacheId[sc$tagKey == "function" & grepl("inputObjects", sc$tagValue)])
  expect_length(id, 1L)

  ## the symptom: reloading the entry (crossing a serialization boundary) must not carry
  ## simInit()'s transient resume state -- and so cannot come back with a dead terra pointer.
  loaded <- reproducible::loadFromCache(cp, cacheId = id, verbose = -2)
  expect_s4_class(loaded, "simList")
  expect_null(loaded@.xData[["._simInitContext"]])
  expect_false(inherits(tryCatch(Copy(loaded), error = function(e) e), "error"))
})

test_that(".unwrap.simList() drops a ._simInitContext present in its input", {
  testInit(opts = list(reproducible.useMemoise = FALSE))
  sim <- simInit(times = list(start = 0, end = 1))
  sim@.xData[["._simInitContext"]] <- list(objects = list(a = 1))
  sim@.xData[["._rmo"]] <- list(recoverableObjs = list())

  unwrapped <- SpaDES.core:::.unwrap.simList(sim, cachePath = tmpCache)
  expect_null(unwrapped@.xData[["._simInitContext"]])
  expect_null(unwrapped@.xData[["._rmo"]])
})
