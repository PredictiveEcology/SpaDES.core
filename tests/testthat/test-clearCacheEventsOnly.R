## clearCacheEventsOnly(): drop only the cache entries belonging to module
## events (`doEvent.*`) and `.inputObjects`, leaving every other cached call
## alone.
##
## NOTE: the entries are *read* from `x`, but the clearing itself calls
## reproducible::clearCache() without passing `x`, so it acts on
## getOption("reproducible.cachePath"). The clearing tests below therefore point
## that option at the same directory. See the note in the summary -- when the
## two differ, the function reports what it would remove from one cache and then
## removes from another.

cacheAnEvent <- function(path) {
  f <- function(x) x
  for (nm in c("doEvent.myMod.init", ".inputObjects", "somethingElse"))
    invisible(reproducible::Cache(f, nm, .functionName = nm,
                                  cachePath = path, verbose = -1))
  path
}

nCacheIds <- function(path) {
  length(unique(reproducible::showCache(path, verbose = -1)$cacheId))
}

test_that("clearCacheEventsOnly returns nothing for an empty cache", {
  skip_on_cran()
  testInit()

  d <- checkPath(file.path(tmpdir, "emptyCache"), create = TRUE)

  out <- suppressMessages(clearCacheEventsOnly(ask = FALSE, x = d, dryRun = TRUE,
                                               verbose = 0))

  expect_type(out, "list")
  expect_length(out, 0L)
})

test_that("clearCacheEventsOnly with dryRun reports the event entries and clears nothing", {
  skip_on_cran()
  testInit()

  d <- cacheAnEvent(checkPath(file.path(tmpdir, "cacheDry"), create = TRUE))
  expect_identical(nCacheIds(d), 3L)

  msgs <- capture_messages(
    clearCacheEventsOnly(ask = FALSE, x = d, dryRun = TRUE, verbose = 1)
  )
  msgs <- paste(msgs, collapse = "")

  expect_match(msgs, "dryRun = TRUE, no clearing")
  expect_match(msgs, "Would remove: doEvent.myMod.init", fixed = TRUE)
  expect_match(msgs, "Would remove: .inputObjects", fixed = TRUE)
  ## the unrelated cached call is not even mentioned
  expect_false(grepl("somethingElse", msgs, fixed = TRUE))
  ## and nothing was actually removed
  expect_identical(nCacheIds(d), 3L)
})

test_that("clearCacheEventsOnly removes the event entries and keeps the others", {
  skip_on_cran()
  testInit()

  d <- cacheAnEvent(checkPath(file.path(tmpdir, "cacheClear"), create = TRUE))
  expect_identical(nCacheIds(d), 3L)

  withr::local_options(list(reproducible.cachePath = d))
  suppressMessages(clearCacheEventsOnly(ask = FALSE, x = d, dryRun = FALSE, verbose = 0))

  ## only `somethingElse` survives
  expect_identical(nCacheIds(d), 1L)
  sc <- reproducible::showCache(d, verbose = -1)
  fns <- sc$tagValue[grepl("function", sc$tagKey)]
  expect_identical(unique(fns), "somethingElse")
})

test_that("clearCacheEventsOnly leaves a cache with no event entries alone", {
  skip_on_cran()
  testInit()

  d <- checkPath(file.path(tmpdir, "cacheNoEvents"), create = TRUE)
  f <- function(x) x
  invisible(reproducible::Cache(f, 1, .functionName = "somethingElse",
                                cachePath = d, verbose = -1))

  withr::local_options(list(reproducible.cachePath = d))
  out <- suppressMessages(clearCacheEventsOnly(ask = FALSE, x = d, dryRun = FALSE,
                                               verbose = 0))

  expect_length(out, 0L)
  expect_identical(nCacheIds(d), 1L)
})
