## `outputs(sim)$arguments` is `AsIs` when the rows were added through `outputsAppend()`
## (simList-accessors.R:1465-1469 wraps the existing column with `I()` so `rbindlist()` will bind it)
## and a plain `list` when they were not -- e.g. outputs set once at `simInit()` and never appended to.
## That is a construction detail. It says nothing about what an event computes.
##
## But it lands in the cacheId. `.robustDigest()`'s simList method drops the outputs ROWS for a
## module-level call (cache.R:292-295, `object@outputs[0, ...]`) and then digests what survives
## (cache.R:311) -- which is the column structure: names, order, and CLASS. With the rows gone, the
## class of `arguments` is essentially all that is left to digest.
##
## Measured on a FireSense phase-2 cache (2026-09-16): across 2395 entries the `sim.outputs`
## component took exactly two values, `5d0cef838b366d87` (arguments = AsIs) and `fd0dc16cc74b5a3c`
## (arguments = list), reproduced byte-exactly from the stored simLists. The same event therefore
## got two cacheIds, in every module -- fireSense_SpreadFit, Biomass_regeneration and
## fireSense_IgnitionPredict all appeared under both -- and warm caches missed for no reason.
##
## The zero-row case below is the faithful reproduction, because that is the state the digest
## actually sees for a module-level event call.

test_that("the outputs digest ignores whether `arguments` is AsIs or a plain list", {
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE))
  modDir <- file.path(tmpdir, "m"); dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) { switch(eventType, init = {}); invisible(sim) }
')
  s <- suppressMessages(simInit(modules = "m", paths = list(modulePath = tmpdir),
                                times = list(start = 0, end = 1)))

  ## Build WITH rows and then subset to zero, which is exactly what cache.R:295 does
  ## (`object@outputs[0, ...]`). Constructing a 0-row data.frame directly fails on the AsIs list
  ## column ("arguments imply differing number of rows"), and subsetting is the faithful operation.
  mkOutputs <- function(n = 1L) {
    k <- max(n, 1L)
    d <- data.frame(objectName = rep("x", k), saveTime = as.numeric(seq_len(k)),
                    file = paste0("x", seq_len(k), ".rds"), fun = rep("saveRDS", k),
                    package = rep("base", k), exts = rep("rds", k), saved = rep(NA, k),
                    arguments = I(rep(list(NA), k)), stringsAsFactors = FALSE)
    if (n == 0L) d[0, , drop = FALSE] else d
  }

  ## the reproduction: zero rows, which is what a module-level event call digests
  sA <- s; sA@outputs <- mkOutputs(0L)
  sL <- s; sL@outputs <- mkOutputs(0L)
  sL@outputs[["arguments"]] <- unclass(sL@outputs[["arguments"]])  # AsIs -> plain list, same content
  expect_true(is(sA@outputs[["arguments"]], "AsIs"))               # premise: the two states differ
  expect_false(is(sL@outputs[["arguments"]], "AsIs"))
  expect_identical(reproducible::CacheDigest(sA)$outputHash,
                   reproducible::CacheDigest(sL)$outputHash)

  ## and with rows present, for the non-module-level call
  sA2 <- s; sA2@outputs <- mkOutputs(2L)
  sL2 <- s; sL2@outputs <- mkOutputs(2L)
  sL2@outputs[["arguments"]] <- unclass(sL2@outputs[["arguments"]])
  expect_identical(reproducible::CacheDigest(sA2)$outputHash,
                   reproducible::CacheDigest(sL2)$outputHash)
})

test_that("the outputs digest still responds to the outputs themselves", {
  ## Guard: the fix must normalise the `arguments` CLASS, not stop digesting outputs. If this ever
  ## fails, outputs have been dropped from the key altogether and real changes would be missed.
  skip_on_cran()
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE))
  modDir <- file.path(tmpdir, "m"); dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) { switch(eventType, init = {}); invisible(sim) }
')
  s <- suppressMessages(simInit(modules = "m", paths = list(modulePath = tmpdir),
                                times = list(start = 0, end = 1)))
  mk <- function(nm) data.frame(objectName = nm, saveTime = 1, file = "x.rds", fun = "saveRDS",
                                package = "base", exts = "rds", saved = NA,
                                arguments = I(list(NA)), stringsAsFactors = FALSE)
  s1 <- s; s1@outputs <- mk("x")
  s2 <- s; s2@outputs <- mk("aDifferentObject")
  expect_false(identical(reproducible::CacheDigest(s1)$outputHash,
                         reproducible::CacheDigest(s2)$outputHash))
})
