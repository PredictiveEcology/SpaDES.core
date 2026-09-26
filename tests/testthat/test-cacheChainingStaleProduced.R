## cacheChainingPost() (R/simulation-spades.R) used to add EVERY cached event's outputs to
## `produced` -- the set of objects a later chain link may trust without digesting -- including
## events that were NOT themselves restored from the recorded chain (a fresh recompute, or an
## ordinary digest-based cache hit). That let a downstream module's chain hit skip re-digesting
## an object that had actually changed, and return a stale cached result.
##
## Fixture: modA writes `x` from a parameter (so it recomputes when the parameter changes);
## modB does not use `x`'s value (so its own cache entry, and cacheId, are unaffected by `x`);
## modC reads `x` and writes `z <- x * 2`. All three are cached at `.inputObjects`.

mkStaleMod <- function(mp, name, inObjs, outObjs, inputObjectsBody, extraParam = "") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")%s),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { sim <- sim }); return(invisible(sim))
}

.inputObjects <- function(sim) { %s; return(invisible(sim)) }
', name, name, extraParam, inObjs, outObjs, name, inputObjectsBody), file.path(d, paste0(name, ".R")))
}

test_that("a chain hit does not inherit a fresh recompute's outputs as 'produced'", {
  skip_on_cran()
  testInit("terra", opts = list(spades.debug = TRUE, reproducible.verbose = 1))

  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  ## modA needs an unsupplied input, or .inputObjects is skipped entirely and never cached
  ## (`.runModuleInputObjects()`: `all(inputObjects %in% .userSuppliedObjNames)` is TRUE
  ## on `all(logical(0))` when a module declares no inputs at all).
  mkStaleMod(mp, "modA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
             'bindrows(createsOutput("x", "numeric", ""))', "sim$x <- P(sim)$p",
             extraParam = ', defineParameter("p", "numeric", 1, NA, NA, "")')
  ## modB declares no inputs at all -- not even x -- so its own Cache() digest, and its
  ## cacheId, do not depend on x's value; module order (modA, modB, modC) alone puts it after
  ## modA's .inputObjects phase.
  mkStaleMod(mp, "modB", 'bindrows()',
             'bindrows(createsOutput("y", "numeric", ""))', "sim$y <- 10")
  ## modC also declares its own output `z` as an (unsupplied) input: `.inputObjects` caching
  ## only retains objects listed in the module's OWN `inputObjects` (see
  ## `.runModuleInputObjects()`, `moduleSpecificInputObjects`), so a `createsOutput`-only object
  ## would not round-trip through a reloaded cache entry regardless of this bug.
  mkStaleMod(mp, "modC",
             'bindrows(expectsInput("y", "numeric", ""), expectsInput("x", "numeric", ""), expectsInput("z", "numeric", ""))',
             'bindrows(createsOutput("z", "numeric", ""))', "sim$z <- sim$x * 2")

  cp <- file.path(tmpdir, "cc")
  runIt <- function(p) do.call(simInit, list(
    times = list(start = 1, end = 1),
    params = list(modA = list(.useCache = ".inputObjects", p = p),
                  modB = list(.useCache = ".inputObjects"),
                  modC = list(.useCache = ".inputObjects")),
    modules = list("modA", "modB", "modC"),
    paths = list(modulePath = mp, cachePath = cp)))

  withr::local_options(spades.cacheChaining = TRUE)

  ## run 1 (p = 1): records the chain, including the modB -> modC link
  s1 <- runIt(1)
  expect_equal(s1$x, 1); expect_equal(s1$z, 2)

  ## run 2 (p = 2): modA recomputes (its own digest includes `p`); modB does not read x, so its
  ## own cacheId (and its entry's tags, and the modB -> modC link they key on) are unchanged;
  ## modC's chain link to modB still matches, so modC is offered as a chain hit -- but `x` is
  ## not one of the objects a genuine chain hit produced this run, so it must still be digested.
  s2 <- runIt(2)
  expect_equal(s2$x, 2)
  expect_equal(s2$z, s2$x * 2) # fails on development: modC returns run 1's stale z (== 2)

  ## run 3 (p = 2 again): modA is now an ordinary cache hit on run 2's entry, so a genuine
  ## modA -> modB chain link (recorded after run 2) is available; chaining must still engage.
  msgs <- capture_messages(invisible(runIt(2)))
  expect_true(any(grepl("Using cacheChaining", msgs)))
})
