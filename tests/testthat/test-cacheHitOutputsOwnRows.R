## On an event cache hit, `.prepareOutput()` (R/cache.R) merged EVERY outputs(sim) row of the cached
## simList into the live run. The cached simList carries rows that were already in the run that
## created the entry (another module's `Plots()` rows, a caller's `outputs`), not only those the cached
## event added. A row saved at the earlier run's start then made the `save` module schedule an event
## before start(sim), and spades() stopped with "You have tried to schedule an event before
## start(sim)". It also leaked another run's file paths into this run's outputs(sim).
## A hit now restores only the rows the cached event added (`eventOutputsAdded` tag, written on every
## new entry; .chainOutAddedRows() in R/cacheChainingJump.R). spades.cacheChaining stays at its default.

## A module whose `.inputObjects` runs `body`. `modReg`'s is cached; `modForeign`'s is not.
mkInputObjectsMod <- function(mp, name, body, useCache = ".inputObjects") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", "%s", NA, NA, "")),
  inputObjects = bindrows(expectsInput("unsupplied0", "numeric", "")),
  outputObjects = bindrows(createsOutput("%sOut", "numeric", ""))))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim <- sim }); return(invisible(sim))
}

.inputObjects <- function(sim) {
  sim$%sOut <- 1
  %s
  return(invisible(sim))
}
', name, name, useCache, name, name, name, body), file.path(d, paste0(name, ".R")))
}

## `modForeign` registers a row (saved at start, like a `Plots()` row) before `modReg`'s cached
## `.inputObjects` runs; `modReg` registers its own row, saved at the end of the run.
mkMods <- function(mp) {
  mkInputObjectsMod(mp, "modForeign", useCache = "FALSE",
    body = 'sim@outputs <- outputsAppend(sim@outputs, saveTime = start(sim), file = file.path(outputPath(sim), "foreign.rds"))')
  mkInputObjectsMod(mp, "modReg",
    body = 'sim@outputs <- outputsAppend(sim@outputs, saveTime = end(sim), file = file.path(outputPath(sim), "byReg.rds"))')
}

runReg <- function(mp, cp, start, modules = "modReg") {
  simInit(times = list(start = start, end = start + 1), modules = as.list(modules),
          paths = list(modulePath = mp, cachePath = cp, outputPath = file.path(dirname(cp), "out")))
}

files <- function(sim) basename(as.character(outputs(sim)$file))

test_that("a cache hit does not restore outputs rows that the cached event did not add", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkMods(mp)
  cp <- file.path(tmpdir, "cc")

  s1 <- runReg(mp, cp, start = 2020, modules = c("modForeign", "modReg"))
  expect_true("foreign.rds" %in% files(s1))

  ## modReg's .inputObjects is the FIRST cached event of this run (no previous entry to chain from): its
  ##   entry records the one row it added all the same
  sc <- reproducible::showCache(cp, verbose = -2)
  expect_identical(sc$tagValue[sc$tagKey == "eventOutputsAdded"], "outAdd1\t1")

  s2 <- runReg(mp, cp, start = 2020)   # modReg's .inputObjects is a cache hit; modForeign is not in this run
  expect_false("foreign.rds" %in% files(s2))
  expect_true("byReg.rds" %in% files(s2))  # the row the cached event added is restored

  s3 <- runReg(mp, cp, start = 2021)   # the failing case: a later start than the run that made the entry
  expect_false("foreign.rds" %in% files(s3))
  expect_true("byReg.rds" %in% files(s3))
  expect_true(all(outputs(s3)$saveTime >= 2021))
  expect_no_error(spades(s3, debug = FALSE))
})

test_that("an entry saved without the outputs tag restores all its rows, as before the tag existed", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkMods(mp)
  cp <- file.path(tmpdir, "cc")

  runReg(mp, cp, start = 2020, modules = c("modForeign", "modReg"))
  sc <- reproducible::showCache(cp, verbose = -2)
  ids <- unique(sc$cacheId[sc$tagKey == "eventOutputsAdded"])
  expect_gt(length(ids), 0L)
  for (id in ids) reproducible::.updateTagsRepo(cacheId = id, cachePath = cp, tagKey = "eventOutputsAdded",
                                               tagValue = "oldEntry")
  s2 <- runReg(mp, cp, start = 2020)
  expect_true(all(c("foreign.rds", "byReg.rds") %in% files(s2)))
})
