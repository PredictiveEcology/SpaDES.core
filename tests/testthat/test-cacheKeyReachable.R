## Every module parameter and every module function was part of every cached event's key, so a
## parameter that only labels a run (a run name, a replicate) or an edit to an unrelated function
## recomputed cached events (fireSense phase 1, 2026-10-19: tens of hours of reruns).
## options(spades.cacheKeyReachable = TRUE) keeps in an event's key only the parameters and module
## functions its code reaches, and falls back to the full key when it cannot tell.

.writeReachModule <- function(modulePath, initBody = "sim$x <- sim$x + P(sim)$alpha",
                              otherHelper = "b * 2", extra = "") {
  modDir <- file.path(modulePath, "reach")
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "reach.R"), sep = "", '
defineModule(sim, list(name = "reach", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(reach = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, 0, 10, "read by init"),
    defineParameter("beta", "numeric", 1, 0, 10, "read by the other event"),
    defineParameter("runLabel", "character", "a", NA, NA, "read by no event"),
    defineParameter(".useCache", "character", c(".inputObjects", "init"), NA, NA, "c")),
  inputObjects = bindrows(),
  outputObjects = bindrows(createsOutput("x", "numeric", "x"), createsOutput("y", "numeric", "y"))))
doEvent.reach <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      sim <- initFn(sim)
      sim <- scheduleEvent(sim, start(sim), "reach", "other")
    },
    other = { sim$y <- otherHelper(P(sim)$beta) })
  invisible(sim)
}
initFn <- function(sim) {
  ', initBody, '
  sim
}
otherHelper <- function(b) {
  ', otherHelper, '
}
.inputObjects <- function(sim) {
  sim$x <- 1
  sim
}
', extra)
}

.runReachModule <- function(modulePath, cachePath, params = list()) {
  s <- suppressMessages(simInit(modules = "reach", times = list(start = 0, end = 1),
                                params = list(reach = params),
                                paths = list(modulePath = modulePath, cachePath = cachePath)))
  s <- suppressMessages(spades(s, debug = FALSE))
  list(params = params(s)$reach, x = s$x, y = s$y)
}

.nReachEntries <- function(cachePath, fn) {
  sc <- reproducible::showCache(cachePath, verbose = -1)
  length(unique(sc$cacheId[sc$tagKey == "function" & sc$tagValue == fn]))
}

## not "m": a module whose name starts its own `mod` binding counts as not namespaced
.reachOpts <- function(on)
  list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
       reproducible.useMemoise = FALSE, spades.saveSimOnExit = FALSE,
       spades.cacheKeyReachable = on)

test_that("a parameter no event reads stays out of the key, and keeps its current value after a hit", {
  skip_on_cran()
  testInit(opts = .reachOpts(TRUE))
  .writeReachModule(tmpdir)
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  res <- .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 1L)
  expect_identical(.nReachEntries(tmpCache, ".inputObjects_reach"), 1L)
  expect_identical(res$params$runLabel, "rep2")
  expect_identical(res$x, 2)

  ## a parameter init reads is in its key
  res <- .runReachModule(tmpdir, tmpCache, list(runLabel = "rep3", alpha = 5))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
  expect_identical(res$x, 6)
  expect_identical(res$params$runLabel, "rep3")
  ## ... but not in .inputObjects', which does not read it
  expect_identical(.nReachEntries(tmpCache, ".inputObjects_reach"), 1L)
})

test_that("without the option, every parameter is in the key, as before", {
  skip_on_cran()
  testInit(opts = .reachOpts(FALSE))
  .writeReachModule(tmpdir)
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
  expect_identical(.nReachEntries(tmpCache, ".inputObjects_reach"), 2L)
})

test_that("editing a function an event does not reach leaves its key; editing one it reaches changes it", {
  skip_on_cran()
  testInit(opts = .reachOpts(TRUE))
  .writeReachModule(tmpdir)
  .runReachModule(tmpdir, tmpCache)
  .writeReachModule(tmpdir, otherHelper = "b * 3")
  res <- .runReachModule(tmpdir, tmpCache)
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 1L)
  expect_identical(res$y, 3)

  .writeReachModule(tmpdir, otherHelper = "b * 3", initBody = "sim$x <- sim$x + P(sim)$alpha + 10")
  res <- .runReachModule(tmpdir, tmpCache)
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
  expect_identical(res$x, 12)

  ## the module version is not in the key: the code is
  .writeReachModule(tmpdir, otherHelper = "b * 3", initBody = "sim$x <- sim$x + P(sim)$alpha + 10")
  modFile <- file.path(tmpdir, "reach", "reach.R")
  writeLines(sub('version = list(reach = "0.0.1")', 'version = list(reach = "0.0.2")', readLines(modFile),
                 fixed = TRUE), modFile)
  .runReachModule(tmpdir, tmpCache)
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
})

test_that("the analysis narrows to what the event reaches", {
  e <- new.env()
  e$doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) {
    switch(eventType,
           init = , start = { sim <- initFn(sim) },
           other = { sim$y <- otherFn(P(sim, "gamma")) })
    invisible(sim)
  }
  e$initFn <- function(sim) {
    p <- P(sim)
    sim$x <- Par$alpha + p$delta + params(sim)[[currentModule(sim)]]$eps + P(sim)[["zeta"]] +
      params(sim)$m$eta + linkOf(P(sim)) + CONST
    sim
  }
  e$linkOf <- function(pars) if (identical(pars$link, "a")) 1 else 0
  e$otherFn <- function(g) g
  e$CONST <- 2
  s <- .cacheKeyScopeAnalyse(e, "m", "init")
  expect_true(s$narrowed)
  expect_setequal(s$params, c(".seed", "alpha", "delta", "eps", "zeta", "eta", "link"))
  expect_setequal(s$objects, c("initFn", "linkOf", "CONST"))
  expect_false(grepl("otherFn", s$eventCode))
  ## fall-through: `init = ,` takes the `start` branch
  expect_identical(.cacheKeyScopeAnalyse(e, "m", "start")$params, s$params)

  s2 <- .cacheKeyScopeAnalyse(e, "m", "other")
  expect_setequal(s2$params, c(".seed", "gamma"))
  expect_identical(s2$objects, "otherFn")
})

## The scope of an `init` event whose body is `code`, with module functions `helpers` (as source)
.reachScope <- function(code, helpers = list(), paramClasses = NULL) {
  e <- new.env()
  e$doEvent.m <- eval(parse(text = paste0(
    "function(sim, eventTime, eventType, debug = FALSE) {
       switch(eventType, init = { ", code, " })
       invisible(sim)
     }")))
  for (h in names(helpers)) e[[h]] <- eval(parse(text = helpers[[h]]))
  .cacheKeyScopeAnalyse(e, "m", "init", paramClasses = paramClasses)
}

.reachFallsBack <- function(code, helpers = list(), paramClasses = NULL) {
  s <- .reachScope(code, helpers, paramClasses)
  expect_false(s$narrowed, label = code)
  expect_true(length(s$reasons) > 0, label = code)
}

test_that("code the analysis cannot resolve falls back to the full key", {
  fallsBack <- .reachFallsBack
  fallsBack("nm <- names(sim)[1]; sim$x <- P(sim)[[nm]]")       # non-literal P(sim)[[nm]]
  fallsBack("sim$x <- P(sim, nm)")                               # non-literal param
  fallsBack("sim$x <- P(sim)")                                   # P(sim) captured whole
  fallsBack("sim$x <- params(sim)")                              # params(sim) used whole
  fallsBack("sim$x <- params(sim)[[currentModule(sim)]]")        # module list used whole
  fallsBack("sim$x <- sim@params$m$a")                           # sim@params
  fallsBack("sim$x <- as.list(Par)")                             # Par captured whole
  fallsBack("sim$x <- helper(Par)", list(helper = "function(p) p"))  # ... passed on whole
  fallsBack("sim$x <- Par[[nm]]")                                # non-literal Par[[nm]]
  fallsBack("f <- get(fnName); sim <- f(sim)")                   # get() of a non-literal name
  fallsBack("sim$x <- mget(nms, inherits = TRUE)")
  fallsBack("f <- match.fun(fnName); sim$x <- f(1)")
  fallsBack("sim <- do.call(fnList[[1]], list(sim))")
  fallsBack("sim$x <- eval(parse(text = 'P(sim)$a'))")           # eval / parse
  fallsBack("sim <- sim$fun(sim)")                               # a function held in an object
  fallsBack("sim <- applyIt(sim$fun, sim)", list(applyIt = "function(f, sim) f(sim)"))
  fallsBack("sim <- someMod::someFn(sim)")                       # sim passed to a package
})

test_that("a local string constant names the parameter it selects", {
  s <- .reachScope("mec <- 'alpha'; if (identical(Par[[mec]], 1)) P(sim)[[mec]] <- 2; sim$x <- P(sim, mec)")
  expect_true(s$narrowed)
  expect_setequal(s$params, c(".seed", "alpha"))
  .reachFallsBack("mec <- 'alpha'; mec <- 'beta'; sim$x <- Par[[mec]]")  # assigned twice
  .reachFallsBack("mec <- paste0('al', 'pha'); sim$x <- Par[[mec]]")    # not a string literal
  .reachFallsBack("for (mec in 'alpha') sim$x <- Par[[mec]]")           # a loop variable
  .reachFallsBack("sim$x <- Par[[mec]]")                                 # never assigned here
})

test_that("a parameter selected through a local constant is in the key, not every parameter", {
  skip_on_cran()
  testInit(opts = .reachOpts(TRUE))
  .writeReachModule(tmpdir, initBody = 'k <- "alpha"; sim$x <- sim$x + P(sim)[[k]]')
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 1L)
  res <- .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2", alpha = 5))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
  expect_identical(res$x, 6)
})

test_that("eval() of a data.table column name, a package formal default or a value parameter is not dynamic code", {
  s <- .reachScope("DT <- data.table::data.table(a = 1); col <- 'b'; DT[a == 1, eval(col) := 1]; sim$x <- DT")
  expect_true(s$narrowed)
  s <- .reachScope("sim$x <- eval(formals(stats::rnorm)$mean) + eval(formals(stats::rnorm)[['sd']])")
  expect_true(s$narrowed)
  expect_identical(s$params, ".seed")
  s <- .reachScope("sim$x <- eval(P(sim)$alpha) + eval(Par$beta)",
                   paramClasses = list(alpha = "numeric", beta = c("numeric", "character")))
  expect_true(s$narrowed)
  expect_setequal(s$params, c(".seed", "alpha", "beta"))
  ## a parameter declared as code runs whatever it holds
  .reachFallsBack("sim$x <- eval(P(sim)$alpha)", paramClasses = list(alpha = c("numeric", "name", "call")))
  .reachFallsBack("sim$x <- eval(P(sim)$alpha)", paramClasses = list(alpha = "function"))
  .reachFallsBack("sim$x <- eval(P(sim)$alpha)")                        # class unknown
  .reachFallsBack("col <- 'b'; sim$x <- eval(col)")                     # a name outside DT[...]
  .reachFallsBack("DT[a == 1, eval(quote(b)) := 1]")                    # code inside DT[...]
  .reachFallsBack("sim$x <- eval(formals(helper)$a)", list(helper = "function(a = 1) a"))
})

test_that("eval() of a parameter declared as a value narrows the event's key", {
  skip_on_cran()
  testInit(opts = .reachOpts(TRUE))
  .writeReachModule(tmpdir, initBody = "sim$x <- sim$x + eval(P(sim)$alpha)")
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 1L)
  s <- suppressMessages(simInit(modules = "reach", times = list(start = 0, end = 1),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  expect_false("runLabel" %in% names(.chainNonObjects(s, "reach", "init")$params))
})

test_that("a module function whose code deparse()/body()/formals() read is reached, not passed around", {
  draw <- list(draw = "function(g) g(2)")
  s <- .reachScope("sim$x <- draw(function(z) z); sim$y <- deparse(draw); sim$z <- body(draw); sim$w <- formals(draw)", draw)
  expect_true(s$narrowed)
  expect_identical(s$objects, "draw")
  .reachFallsBack("sim$x <- draw(function(z) z); sim$y <- lapply(1, draw)", draw)
})

test_that("code that really is dynamic still falls back", {
  .reachFallsBack("for (nm in modules(sim)) sim$x <- P(sim, module = nm)$alpha")
  .reachFallsBack("sim$x <- eval(str2lang('1 + 1'))")
  .reachFallsBack("f <- getFromNamespace(paste0('rno', 'rm'), 'stats'); sim$x <- f(1)")
  .reachFallsBack("sim$x <- eval(quote(P(sim)$alpha))")
  .reachFallsBack("sim$x <- eval(sim$code)")
})

test_that("a falling-back event has exactly the key it had without the option", {
  skip_on_cran()
  testInit(opts = .reachOpts(FALSE))
  .writeReachModule(tmpdir, initBody = "sim$x <- sim$x + eval(quote(P(sim)$alpha))")
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  options(spades.cacheKeyReachable = TRUE)
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep1"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 1L)
  ## and a label change still misses, as before
  .runReachModule(tmpdir, tmpCache, list(runLabel = "rep2"))
  expect_identical(.nReachEntries(tmpCache, "doEvent.reach::init"), 2L)
})

test_that("the narrowing is reported with showSimilar", {
  skip_on_cran()
  testInit(verbose = 1, opts = c(.reachOpts(TRUE), list(reproducible.showSimilar = TRUE)))
  .writeReachModule(tmpdir)
  s <- suppressMessages(simInit(modules = "reach", times = list(start = 0, end = 1),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  msgs <- capture_messages(spades(s, debug = FALSE))
  expect_true(any(grepl("reach::init cache key narrowed to parameters: alpha; functions: initFn", msgs)))

  .writeReachModule(tmpdir, initBody = "sim$x <- sim$x + eval(quote(P(sim)$alpha))")
  s <- suppressMessages(simInit(modules = "reach", times = list(start = 0, end = 1),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  msgs <- capture_messages(spades(s, debug = FALSE))
  expect_true(any(grepl("reach::init cache key uses all parameters and functions: initFn: eval()",
                        msgs, fixed = TRUE)))
})

test_that("cache chaining rebuilds the same narrowed key", {
  skip_on_cran()
  testInit(opts = .reachOpts(TRUE))
  .writeReachModule(tmpdir)
  s <- suppressMessages(simInit(modules = "reach", times = list(start = 0, end = 1),
                                params = list(reach = list(runLabel = "rep1")),
                                paths = list(modulePath = tmpdir, cachePath = tmpCache)))
  no <- .chainNonObjects(s, "reach", "init")
  expect_false("runLabel" %in% names(no$params))
  expect_true("alpha" %in% names(no$params))
  expect_false("version" %in% no$depends)
  expect_true(is.character(no$eventCode))
  expect_false(any(grepl("doEvent.reach", names(no))))
  noIO <- .chainNonObjects(s, "reach", ".inputObjects")
  expect_false("runLabel" %in% names(noIO$params))
})
