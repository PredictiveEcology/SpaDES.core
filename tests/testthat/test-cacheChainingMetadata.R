## The ordinary event cache key digests a module's metadata VALUES (version, reqdPkgs, ...), so
## changing them recomputes the event. The cacheChaining key must do the same: otherwise a chain
## recorded before the change keeps restoring the entry recorded before it.

mkMetaMod <- function(mp, name, inObjs, outObjs, body, version = "0.0.1", reqdPkgs = "list()") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "%s"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = %s,
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventType, ...) invisible(sim)

.inputObjects <- function(sim) { %s; return(invisible(sim)) }
', name, name, version, reqdPkgs, inObjs, outObjs, name, body), file.path(d, paste0(name, ".R")))
}

test_that("a change to a module's version or reqdPkgs breaks the chain", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  cp <- file.path(tmpdir, "cc")
  withr::local_options(spades.cacheChaining = TRUE)

  runIt <- function(counter, version = "0.0.1", reqdPkgs = "list()") {
    mkMetaMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
              'bindrows(createsOutput("x", "numeric", ""))', "sim$x <- 1")
    mkMetaMod(mp, "modB", 'bindrows(expectsInput("y", "numeric", ""))',
              'bindrows(createsOutput("y", "numeric", ""))', "sim$y <- getOption(\"metaTestCounter\")",
              version = version, reqdPkgs = reqdPkgs)
    options(metaTestCounter = counter)
    simInit(times = list(start = 1, end = 1),
            params = list(modA = list(.useCache = ".inputObjects"),
                          modB = list(.useCache = ".inputObjects")),
            modules = list("modA", "modB"),
            paths = list(modulePath = mp, cachePath = cp))
  }
  withr::defer(options(metaTestCounter = NULL))

  expect_equal(runIt(1)$y, 1)                  # records the modA -> modB chain
  expect_equal(runIt(2)$y, 1)                  # unchanged metadata: restored, not recomputed
  expect_equal(runIt(3, version = "0.0.2")$y, 3)                  # fails on development: 1
  expect_equal(runIt(4, version = "0.0.2")$y, 3)                  # new state is cached
  expect_equal(runIt(5, version = "0.0.2", reqdPkgs = 'list("stats")')$y, 5) # fails on development: 3
})
