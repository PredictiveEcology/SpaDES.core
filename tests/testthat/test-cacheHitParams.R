## What a cache hit does to the module's parameters: the params in the entry's key are the live
## ones by construction, so a hit takes from the entry only those and the ones its event changed
## itself (`P(sim, "x") <- ...`); every other param keeps its live value. The oracle is the same
## run without caching.

hitParams <- 'defineParameter(".useCache", "character", NA, NA, NA, ""),
              defineParameter("k", "numeric", 1, NA, NA, ""),
              defineParameter("sfx", "character", "", NA, NA, "", cacheKey = FALSE),
              defineParameter("note", "character", "", NA, NA, "", cacheKey = FALSE),
              defineParameter("unread", "character", "", NA, NA, "")'

hitOutputs <- 'bindrows(createsOutput("seen", "character", ""))'

runHit <- function(mp, cp, params, mods = "hA") {
  withr::local_options(spades.cacheChaining = FALSE)
  simInitAndSpades(times = list(start = 1, end = 2), params = params, modules = as.list(mods),
                   paths = list(modulePath = mp, cachePath = cp))
}

## `sfx` is out of the key by `cacheKey = FALSE`; `unread` because, under spades.cacheKeyReachable,
## the event does not read it
test_that("an ordinary cache hit keeps the live value of a param that is not in the key", {
  skip_on_cran()
  testInit("terra", opts = c(jumpOpts, spades.cacheKeyReachable = TRUE))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkJumpMod(mp, "hA", "bindrows()", hitOutputs, "sim$seen <- P(sim)$sfx", params = hitParams)
  cp <- file.path(tmpdir, "cache")
  runHit(mp, cp, list(.globals = list(sfx = "old", unread = "old"), hA = list(.useCache = "init")))
  s2 <- runHit(mp, cp, list(.globals = list(sfx = "new", unread = "new"), hA = list(.useCache = "init")))
  expect_identical(s2$seen, "old") # a hit: the event did not run again
  expect_identical(params(s2)$hA[c("sfx", "unread")], list(sfx = "new", unread = "new"))
})

test_that("an event that changes its own params keeps the changes, run fresh or from the cache", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  ## `k` is in the key, `note` is not; the event sets both
  mkJumpMod(mp, "hA", "bindrows()", hitOutputs,
            'sim$seen <- P(sim)$sfx; P(sim, "k") <- 2; P(sim, "note") <- "set by init"',
            params = hitParams)
  ref <- runHit(mp, file.path(tmpdir, "none"), list(hA = list(.useCache = NA)))
  expect_identical(params(ref)$hA$k, 2)
  expect_identical(params(ref)$hA$note, "set by init")

  cp <- file.path(tmpdir, "cache")
  for (run in c("fresh", "hit")) {
    s <- runHit(mp, cp, list(.globals = list(sfx = run), hA = list(.useCache = "init")))
    expect_identical(params(s)$hA[c("k", "note")], params(ref)$hA[c("k", "note")], info = run)
    expect_identical(s$seen, "fresh", info = run) # the second run is a hit
    expect_identical(params(s)$hA$sfx, run)
  }
})
