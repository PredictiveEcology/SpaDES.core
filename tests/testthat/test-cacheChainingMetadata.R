## The ordinary event cache key digests a module's metadata VALUES (reqdPkgs, ...), so changing
## them recomputes the event. The cacheChaining key must do the same: otherwise a chain recorded
## before the change keeps restoring the entry recorded before it. The module `version` is NOT
## digested (function bodies already are), so a version-only change keeps every entry and chain.

mkMetaMod <- function(mp, name, inObjs, outObjs, body, version = "0.0.1", reqdPkgs = "list()",
                      initBody = "") {
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

doEvent.%s <- function(sim, eventType, ...) { if (eventType == "init") { %s }; invisible(sim) }

.inputObjects <- function(sim) { %s; return(invisible(sim)) }
', name, name, version, reqdPkgs, inObjs, outObjs, name, initBody, body), file.path(d, paste0(name, ".R")))
}

test_that("a change to a module's reqdPkgs, or a function body, breaks the chain; a version does not", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  cp <- file.path(tmpdir, "cc")
  withr::local_options(spades.cacheChaining = TRUE)

  runIt <- function(counter, version = "0.0.1", reqdPkgs = "list()", bodyB = "getOption(\"metaTestCounter\")") {
    mkMetaMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
              'bindrows(createsOutput("x", "numeric", ""))', "sim$x <- 1")
    mkMetaMod(mp, "modB", 'bindrows(expectsInput("y", "numeric", ""))',
              'bindrows(createsOutput("y", "numeric", ""))', paste("sim$y <-", bodyB),
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
  expect_equal(runIt(3, version = "0.0.2")$y, 1)  # version only: chain holds (fails on development: 3)
  expect_equal(runIt(4, version = "0.0.2", bodyB = "getOption(\"metaTestCounter\") + 0")$y, 4) # body change breaks it
  expect_equal(runIt(5, version = "0.0.2", bodyB = "getOption(\"metaTestCounter\") + 0")$y, 4)
  expect_equal(runIt(6, version = "0.0.2", reqdPkgs = 'list("stats")',
                     bodyB = "getOption(\"metaTestCounter\") + 0")$y, 6) # reqdPkgs change breaks it
})

test_that("a version-only change keeps .inputObjects and init cached; a body change does not", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  cp <- file.path(tmpdir, "cc")
  withr::defer(options(metaTestCounter = NULL))

  runIt <- function(counter, version = "0.0.1", bodyIO = "getOption(\"metaTestCounter\")",
                    bodyInit = "sim$z <- getOption(\"metaTestCounter\")") {
    mkMetaMod(mp, "modC", 'bindrows(expectsInput("y", "numeric", ""))',
              'bindrows(createsOutput("y", "numeric", ""), createsOutput("z", "numeric", ""))',
              paste("sim$y <-", bodyIO), version = version, initBody = bodyInit)
    options(metaTestCounter = counter)
    sim <- simInit(times = list(start = 1, end = 1),
                   params = list(modC = list(.useCache = c(".inputObjects", "init"))),
                   modules = list("modC"), paths = list(modulePath = mp, cachePath = cp))
    spades(sim)
  }

  s1 <- runIt(1)
  expect_equal(c(s1$y, s1$z), c(1, 1))
  s2 <- runIt(2, version = "0.0.2")                                    # (a) version only: both hit
  expect_equal(c(s2$y, s2$z), c(1, 1))                                 # fails on development: 2, 2
  expect_equal(as.character(s2@depends@dependencies$modC@version[[1]]), "0.0.2")  # (c) the CURRENT version
  s3 <- runIt(3, version = "0.0.3", bodyIO = "getOption(\"metaTestCounter\") + 0",
              bodyInit = "sim$z <- getOption(\"metaTestCounter\") + 0")  # (b) version AND bodies
  expect_equal(c(s3$y, s3$z), c(3, 3))
  expect_equal(as.character(s3@depends@dependencies$modC@version[[1]]), "0.0.3")
})
