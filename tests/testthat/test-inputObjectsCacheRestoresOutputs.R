## `.runModuleInputObjects()` (R/simulation-simInit.R) passed only the module's own declared
## `expectsInput` names as the `outputObjects` argument to the `Cache()` call wrapping
## `.inputObjects`. That argument decides which names survive a cache-hit reload
## (`.wrap.simList()`/`.unwrap.simList()`). An object a module sets in `.inputObjects` but
## declares only via `createsOutput` (not also `expectsInput`) was never part of that set, so
## it vanished from the simList on a cache HIT -- a cache MISS never showed this, since a miss
## returns the live, freshly computed sim rather than round-tripping through disk. This is
## independent of `spades.cacheChaining`, which stays off (its default) in this test.

mkOutputOnlyMod <- function(mp, name) {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, "")),
  inputObjects = bindrows(expectsInput("unsupplied0", "numeric", "")),
  outputObjects = bindrows(createsOutput("onlyOutput", "numeric", ""))))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim <- sim }); return(invisible(sim))
}

.inputObjects <- function(sim) { sim$onlyOutput <- 1; return(invisible(sim)) }
', name, name, name), file.path(d, paste0(name, ".R")))
}

test_that("a cache hit restores an .inputObjects output declared only via createsOutput", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))

  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkOutputOnlyMod(mp, "modOut")

  cp <- file.path(tmpdir, "cc")
  runIt <- function() simInit(times = list(start = 1, end = 1), modules = list("modOut"),
                              paths = list(modulePath = mp, cachePath = cp))

  ## spades.cacheChaining left at its default (off/FALSE) -- this is not a chaining defect
  s1 <- runIt()  # cache MISS: fresh compute
  expect_equal(s1$onlyOutput, 1)

  s2 <- runIt()  # cache HIT: reloaded from disk
  expect_equal(s2$onlyOutput, 1)  # fails on development: onlyOutput is NULL after the reload
})
