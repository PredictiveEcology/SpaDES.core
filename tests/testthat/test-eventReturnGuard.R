# Tests for the .checkEventReturn() guard.
#
# Why this exists: when a module event (init/.inputObjects/etc.) returns NULL instead
# of `sim`, reproducible::Cache() stores the NULL and -- on the next cache hit -- replays
# it as the literal character string "NULL". That non-simList value then propagates into
# `sim`, and the next slot access blows up with a confusing secondary error from the
# on.exit (e.g. "more elements supplied than there are to replace") that masks the real
# culprit module. The guard turns that into a clear, module-named error at the point of
# return so the user can act on it.

## `doEventBody` is the entire `doEvent.<name>` body (NOT just an init branch) so callers
## can make init explicitly return NULL. Default keeps the standard "do something on init,
## return sim" shape.
mkMinModule <- function(tmpdir, name,
                        doEventBody = 'if (eventType == "init") sim$ran <- TRUE; sim',
                        ioBody = "sim",
                        inObj = character(), outObj = character(),
                        params = 'rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, ""))') {
  newModule(name, tmpdir, open = FALSE)
  inRows  <- if (length(inObj))  paste0('expectsInput("', inObj,  '", "ANY", "")', collapse = ", ") else ""
  outRows <- if (length(outObj)) paste0('createsOutput("', outObj, '", "ANY", "")', collapse = ", ") else ""
  code <- sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = %s,
  inputObjects = bindrows(%s), outputObjects = bindrows(%s)))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  %s
}
.inputObjects <- function(sim) { %s }
', name, name, params, inRows, outRows, name, doEventBody, ioBody)
  cat(code, file = file.path(tmpdir, name, paste0(name, ".R")), fill = TRUE)
}

test_that(".checkEventReturn passes through a simList and errors otherwise", {
  ## passes through a real simList
  sim <- simInit(times = list(start = 0, end = 1))
  expect_silent(SpaDES.core:::.checkEventReturn(sim, "modX", "init", fromCache = FALSE))

  ## non-simList, non-cache: hint points at the module's event
  err <- tryCatch(
    SpaDES.core:::.checkEventReturn(NULL, "modX", "init", fromCache = FALSE),
    error = identity
  )
  expect_s3_class(err, "simpleError")
  msg <- conditionMessage(err)
  expect_match(msg, "modX", fixed = TRUE)
  expect_match(msg, "`init`", fixed = TRUE)
  expect_match(msg, "did not return a simList", fixed = TRUE)
  expect_match(msg, "return(sim)", fixed = TRUE)

  ## non-simList, from cache: hint calls out Cache's "NULL"-replay quirk
  errCached <- tryCatch(
    SpaDES.core:::.checkEventReturn("NULL", "modY", ".inputObjects", fromCache = TRUE),
    error = identity
  )
  msgCached <- conditionMessage(errCached)
  expect_match(msgCached, "modY", fixed = TRUE)
  expect_match(msgCached, "reproducible::Cache", fixed = TRUE)
  expect_match(msgCached, "string \"NULL\"", fixed = TRUE)
  expect_match(msgCached, "clear the cached entry", fixed = TRUE)
})

test_that(".inputObjects returning NULL produces a clear, named error", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## .inputObjects body returns NULL (no trailing `sim`); "dummy" is an unprovided input
  ## so .inputObjects actually runs.
  mkMinModule(tmpdir, "modNullIO", inObj = "dummy", ioBody = "NULL")

  err <- tryCatch(
    suppressMessages(suppressWarnings(
      simInit(modules = "modNullIO", paths = list(modulePath = tmpdir),
              times = list(start = 0, end = 1))
    )),
    error = identity
  )
  expect_s3_class(err, "simpleError")
  msg <- conditionMessage(err)
  expect_match(msg, "modNullIO", fixed = TRUE)
  expect_match(msg, ".inputObjects", fixed = TRUE)
  expect_match(msg, "did not return a simList", fixed = TRUE)
})

test_that("a NULL cached by .inputObjects is replayed as \"NULL\" and caught with a clear error", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## .inputObjects is cached AND returns NULL: first call writes NULL to the cache; second
  ## call hits the cache and reproducible returns the string "NULL" -- the exact scenario
  ## that previously surfaced as an opaque on.exit secondary error.
  mkMinModule(tmpdir, "modCachedNull", inObj = "dummy", ioBody = "NULL",
              params = 'rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, ""))')

  ## first run errors during the fresh .inputObjects call (non-cache path of the guard)
  expect_error(
    suppressMessages(suppressWarnings(
      simInit(modules = "modCachedNull", paths = list(modulePath = tmpdir),
              times = list(start = 0, end = 1))
    )),
    "did not return a simList"
  )

  ## second run hits the cached NULL ("NULL" character) and surfaces the cache-specific hint
  err <- tryCatch(
    suppressMessages(suppressWarnings(
      simInit(modules = "modCachedNull", paths = list(modulePath = tmpdir),
              times = list(start = 0, end = 1))
    )),
    error = identity
  )
  msg <- conditionMessage(err)
  expect_match(msg, "modCachedNull", fixed = TRUE)
  expect_match(msg, "reproducible::Cache", fixed = TRUE)
  expect_match(msg, "string \"NULL\"", fixed = TRUE)
})

test_that("a normal event returning NULL produces a clear, named error", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## init body returns NULL instead of sim (full doEvent body returns NULL)
  mkMinModule(tmpdir, "modNullInit",
              doEventBody = 'if (eventType == "init") return(NULL); sim')

  s <- simInit(modules = "modNullInit", paths = list(modulePath = tmpdir),
               times = list(start = 0, end = 1))
  err <- tryCatch(
    suppressMessages(suppressWarnings(spades(s))),
    error = identity
  )
  expect_s3_class(err, "simpleError")
  msg <- conditionMessage(err)
  expect_match(msg, "modNullInit", fixed = TRUE)
  expect_match(msg, "`init`", fixed = TRUE)
  expect_match(msg, "did not return a simList", fixed = TRUE)
})

test_that("a NULL cached for a normal event is replayed as \"NULL\" and caught with a clear error", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## init is cached AND returns NULL: first spades() writes NULL, second hits the cache
  mkMinModule(tmpdir, "modCachedInit",
              doEventBody = 'if (eventType == "init") return(NULL); sim',
              params = 'rbind(defineParameter(".useCache", "character", "init", NA, NA, ""))')

  s <- simInit(modules = "modCachedInit", paths = list(modulePath = tmpdir),
               times = list(start = 0, end = 1))
  expect_error(suppressMessages(suppressWarnings(spades(s))), "did not return a simList")

  s2 <- simInit(modules = "modCachedInit", paths = list(modulePath = tmpdir),
                times = list(start = 0, end = 1))
  err <- tryCatch(
    suppressMessages(suppressWarnings(spades(s2))),
    error = identity
  )
  msg <- conditionMessage(err)
  expect_match(msg, "modCachedInit", fixed = TRUE)
  expect_match(msg, "reproducible::Cache", fixed = TRUE)
  expect_match(msg, "string \"NULL\"", fixed = TRUE)
})
