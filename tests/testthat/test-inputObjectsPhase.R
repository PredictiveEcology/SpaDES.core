# Regression tests for the `.inputObjects` phase of simInit().
#
# As of SpaDES.core 3.1.2.9004, simInit() no longer runs `.inputObjects` in a bespoke
# loop: each module's `.inputObjects` is scheduled as a real event and drained through the
# same single-event step (.stepEvent -> doEvent) as the spades() event loop. These tests
# lock the behaviour that must stay identical to the pre-rework loop (object placement,
# caching, ordering, the post-simInit queue/completed invariants, RNG) plus the option
# gating. See also test-inputObjects-golden.R for the captured-from-base equivalence test.

## write a minimal module with the given `.inputObjects`/init bodies and metadata
mkMod <- function(tmpdir, name, dotIO = "sim", initBody = "sim",
                  inObj = character(), outObj = character(),
                  params = 'rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, ""))',
                  reqdPkgs = "list()") {
  newModule(name, tmpdir, open = FALSE)
  inRows  <- if (length(inObj))  paste0('expectsInput("', inObj,  '", "ANY", "")', collapse = ", ") else ""
  outRows <- if (length(outObj)) paste0('createsOutput("', outObj, '", "ANY", "")', collapse = ", ") else ""
  code <- sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = %s,
  parameters = %s,
  inputObjects = bindrows(%s), outputObjects = bindrows(%s)))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") { %s }
  sim
}
.inputObjects <- function(sim) { %s; sim }
', name, name, reqdPkgs, params, inRows, outRows, name, initBody, dotIO)
  cat(code, file = file.path(tmpdir, name, paste0(name, ".R")), fill = TRUE)
}

ioCompleted <- function(sim) {
  cc <- completed(sim)
  if (NROW(cc)) cc[cc$eventType == ".inputObjects", ]$moduleName else character()
}

test_that("user-supplied objects are visible to suppliedElsewhere() inside .inputObjects", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## direct: a user-supplied input object is placed before .inputObjects runs, so
  ##   suppliedElsewhere() sees it. ("dummy" is an unprovided input so .inputObjects runs.)
  mkMod(tmpdir, "modDirect", inObj = c("x", "dummy"),
        dotIO = 'sim$sawX <- suppliedElsewhere("x", sim)')
  sim <- simInit(modules = "modDirect", paths = list(modulePath = tmpdir),
                 objects = list(x = 99L), times = list(start = 0, end = 1))
  expect_true(isTRUE(sim$sawX))  # provided object visible inside .inputObjects
  expect_equal(sim$x, 99L)       # and not clobbered

  ## via objectSynonyms: user supplies `age`; the synonym `ageMap` must look supplied too.
  ##   `age` (declared + provided) is placed before .inputObjects; `ageMap` is unprovided
  ##   so .inputObjects runs and the synonym expansion makes `ageMap` look supplied.
  os <- list(c("age", "ageMap"))
  mkMod(tmpdir, "modSyn", inObj = c("age", "ageMap"),
        dotIO = 'if (suppliedElsewhere("ageMap", sim)) sim$worked <- TRUE')
  sim <- simInit(modules = "modSyn", paths = list(modulePath = tmpdir),
                 objects = list(age = 1, objectSynonyms = os), times = list(start = 0, end = 1))
  expect_true(isTRUE(sim$worked))
  expect_equal(sim$age, sim$ageMap)
})

test_that(".inputObjects results are cached (and reused) via .useCache", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## .inputObjects draws a random number; with caching on, a second simInit must return
  ##   the *same* value (a cache hit), which it could not if .inputObjects re-ran fresh.
  mkMod(tmpdir, "modCache", inObj = "val", dotIO = "sim$val <- runif(1)",
        params = 'rbind(defineParameter(".useCache", "character", ".inputObjects", NA, NA, ""))')

  sim1 <- simInit(modules = "modCache", paths = list(modulePath = tmpdir),
                  times = list(start = 0, end = 1))
  sim2 <- simInit(modules = "modCache", paths = list(modulePath = tmpdir),
                  times = list(start = 0, end = 1))
  expect_false(is.null(sim1$val))
  expect_identical(sim1$val, sim2$val)  # cache hit -> identical despite no set.seed

  ## without caching, a re-run produces a different draw (sanity check the above is real)
  mkMod(tmpdir, "modNoCache", inObj = "val", dotIO = "sim$val <- runif(1)")
  s1 <- simInit(modules = "modNoCache", paths = list(modulePath = tmpdir),
                times = list(start = 0, end = 1))
  s2 <- simInit(modules = "modNoCache", paths = list(modulePath = tmpdir),
                times = list(start = 0, end = 1))
  expect_false(identical(s1$val, s2$val))
})

test_that("after simInit the queue/completed reflect a drained .inputObjects phase", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  mkMod(tmpdir, "modA", dotIO = "sim$a <- 1L")
  mkMod(tmpdir, "modB", dotIO = "sim$b <- 1L")
  sim <- simInit(modules = c("modA", "modB"), paths = list(modulePath = tmpdir),
                 loadOrder = c("modA", "modB"), times = list(start = 0, end = 1))

  ## the whole .inputObjects phase drained: none remain in the queue
  expect_false(any(events(sim)$eventType == ".inputObjects"))
  ## each user module's .inputObjects ran exactly once, recorded in loadOrder
  expect_equal(ioCompleted(sim), c("modA", "modB"))
  ## init events scheduled: user modules at .first(), core modules at .first() - 1
  ev <- events(sim)
  expect_setequal(ev[ev$moduleName %in% c("modA", "modB"), ]$eventType, "init")
  expect_true(all(ev[ev$moduleName %in% c("modA", "modB"), ]$eventPriority == .first()))
  expect_true(all(ev[ev$moduleName %in% unlist(.coreModules()) & ev$eventType == "init", ]$eventPriority
                  == .first() - 1))
})

test_that(".inputObjects run in loadOrder (not alphabetical)", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## each .inputObjects appends its name to a shared object; with a reversed loadOrder the
  ##   recorded order must follow loadOrder, not the (alphabetical) module names. ("dummy"
  ##   is an unprovided input so each module's .inputObjects actually runs.)
  mkMod(tmpdir, "aaa", inObj = "dummy", dotIO = 'sim$ioOrder <- c(sim$ioOrder, "aaa")')
  mkMod(tmpdir, "bbb", inObj = "dummy", dotIO = 'sim$ioOrder <- c(sim$ioOrder, "bbb")')
  mkMod(tmpdir, "ccc", inObj = "dummy", dotIO = 'sim$ioOrder <- c(sim$ioOrder, "ccc")')
  sim <- simInit(modules = c("aaa", "bbb", "ccc"), paths = list(modulePath = tmpdir),
                 loadOrder = c("ccc", "aaa", "bbb"), times = list(start = 0, end = 1))
  expect_equal(sim$ioOrder, c("ccc", "aaa", "bbb"))
  expect_equal(ioCompleted(sim), c("ccc", "aaa", "bbb"))
})

test_that("spades.dotInputObjects = FALSE skips .inputObjects entirely", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache,
                       spades.dotInputObjects = FALSE)

  ## "dummy" unprovided input means .inputObjects WOULD run if the option were on
  mkMod(tmpdir, "modSkip", inObj = "dummy", dotIO = "sim$ran <- TRUE")
  sim <- simInit(modules = "modSkip", paths = list(modulePath = tmpdir),
                 times = list(start = 0, end = 1))
  expect_null(sim$ran)                       # .inputObjects did not run
  expect_length(ioCompleted(sim), 0L)        # nothing recorded for .inputObjects
  expect_true(any(events(sim)$moduleName == "modSkip"))  # init still scheduled
})

test_that(".inputObjects RNG is deterministic given a seed", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## caching off so .inputObjects actually re-runs; same seed -> identical draw
  mkMod(tmpdir, "modRng", inObj = "r", dotIO = "sim$r <- runif(3)")
  set.seed(123); s1 <- simInit(modules = "modRng", paths = list(modulePath = tmpdir),
                               times = list(start = 0, end = 1))
  set.seed(123); s2 <- simInit(modules = "modRng", paths = list(modulePath = tmpdir),
                               times = list(start = 0, end = 1))
  expect_equal(s1$r, s2$r)
  expect_length(s1$r, 3L)
})

test_that("allowInitDuringSimInit = TRUE still produces an equivalent simList", {
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache)

  ## a self-contained module; the result must not depend on the allowInit option
  ## ("x" is an unprovided input so .inputObjects runs and sets it)
  mkMod(tmpdir, "modAi", inObj = "x", dotIO = "sim$x <- 5L", initBody = "sim$y <- sim$x * 2L")

  withr::with_options(list(spades.allowInitDuringSimInit = FALSE), {
    simF <- simInit(modules = "modAi", paths = list(modulePath = tmpdir),
                    times = list(start = 0, end = 1))
  })
  withr::with_options(list(spades.allowInitDuringSimInit = TRUE), {
    simT <- simInit(modules = "modAi", paths = list(modulePath = tmpdir),
                    times = list(start = 0, end = 1))
  })
  expect_s4_class(simT, "simList")
  expect_equal(simF$x, simT$x)
  ## both must run cleanly through spades and reach the same init result
  outF <- spades(simF, debug = FALSE)
  outT <- spades(simT, debug = FALSE)
  expect_equal(outF$y, outT$y)
})

test_that("simInit(objects=) loads ALL user objects when allowInitDuringSimInit = TRUE", {
  ## Regression: with spades.allowInitDuringSimInit = TRUE, simInit used to load
  ## only objects declared as a module `inputObject`, silently dropping arbitrary
  ## user-supplied objects (and every object when no module declares them).
  testInit(smcc = FALSE, opts = list(reproducible.useMemoise = FALSE))

  withr::local_options(spades.allowInitDuringSimInit = TRUE)
  s <- suppressMessages(simInit(objects = list(a = 1, b = "x")))
  expect_identical(s[["a"]], 1)
  expect_identical(s[["b"]], "x")

  ## and unchanged when the option is FALSE
  withr::local_options(spades.allowInitDuringSimInit = FALSE)
  s2 <- suppressMessages(simInit(objects = list(a = 1, b = "x")))
  expect_identical(s2[["a"]], 1)
  expect_identical(s2[["b"]], "x")
})
