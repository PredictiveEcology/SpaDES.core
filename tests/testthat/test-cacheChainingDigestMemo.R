## A chain hit checks the module's external inputs against the entry's preDigest tags
## (.chainExternalInputsMatch()), and a jump checks them again at every link. Nothing runs in between,
## so the digests are memoised for one hit + walk (`digests`, an environment made by
## cacheChainingSetup()): an object is digested once however many links read it. Without the memo a
## two-event jump digested a module's inputs twice and a long jump digested a shared input once per
## module, which was most of the 25-45 s before each jump in the FireSense runs.

memoMod <- function(mp) {
  d <- file.path(mp, "mA")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines('
defineModule(sim, list(name = "mA", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(mA = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(expectsInput("ext", "numeric", "")),
  outputObjects = bindrows(createsOutput("a", "numeric", ""))))

doEvent.mA <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim$a <- sim$ext + 1 })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', file.path(d, "mA.R"))
}

test_that("external-input digests are memoised across one chain hit and its walk", {
  skip_on_cran()
  testInit("terra", opts = list(reproducible.useMemoise = FALSE))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  memoMod(mp)
  cp <- file.path(tmpdir, "cache")
  sim <- simInitAndSpades(times = list(start = 1, end = 1), params = list(mA = list(.useCache = "init")),
                          objects = list(ext = 5), modules = list("mA"),
                          paths = list(modulePath = mp, cachePath = cp))
  sc <- showCache(cp, verbose = -1)
  id <- unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == "doEvent.mA::init"])
  expect_length(id, 1L)
  postTags <- .chainEntryTags(id, cp)
  expect_false(is.null(postTags))

  ## no memo: the live object is digested every call
  expect_true(.chainExternalInputsMatch(sim, "mA", postTags, produced = character()))
  sim$ext <- 6
  expect_false(.chainExternalInputsMatch(sim, "mA", postTags, produced = character()))

  ## a memo: the digest of `ext` is taken once and reused by the next call, which is what a walk does
  ##   when a later link reads the same object -- the object cannot have changed between the two
  sim$ext <- 5
  memo <- new.env(parent = emptyenv())
  expect_true(.chainExternalInputsMatch(sim, "mA", postTags, produced = character(), digests = memo))
  expect_true(exists("ext", envir = memo, inherits = FALSE))
  sim$ext <- 6
  expect_true(.chainExternalInputsMatch(sim, "mA", postTags, produced = character(), digests = memo))
  ## a fresh memo sees the change
  expect_false(.chainExternalInputsMatch(sim, "mA", postTags, produced = character(),
                                         digests = new.env(parent = emptyenv())))
  ## an input the chain produced is not digested, memo or not
  expect_true(.chainExternalInputsMatch(sim, "mA", postTags, produced = "ext",
                                        digests = new.env(parent = emptyenv())))
})
