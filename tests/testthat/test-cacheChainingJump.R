## cacheChaining follows a recorded chain one link at a time. With several consecutive cached
## events it can follow the chain to its end and recover the final state in one step
## (R/cacheChainingJump.R). The oracle is the same as in test-cacheChaining.R: a jump may only
## ever be faster, never change an answer. Liveness -- that the jump really engages -- is
## asserted through its message, because a jump that never engages still gives right answers.

## Four modules, each with a cached `init` that writes its own output from the previous module's,
## then schedules a `grow` event (so the queue after the chain is not trivial). modB also reads
## `ext`, an object the user supplies at simInit: the "external input" the jump must re-check.
mkJumpMod <- function(mp, name, inObjs, outObjs, initBody, useCache = c(".inputObjects", "init")) {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = { %s; sim <- scheduleEvent(sim, time(sim) + 1, "%s", "grow") },
    grow = { sim$grown_%s <- time(sim) })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', name, name, inObjs, outObjs, name, initBody, name, name), file.path(d, paste0(name, ".R")))
  invisible(name)
}

jumpFixture <- function(mp) {
  mkJumpMod(mp, "jA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
            'bindrows(createsOutput("a", "numeric", ""), createsOutput("shared", "numeric", ""))',
            "sim$a <- 1; sim$shared <- 100")
  mkJumpMod(mp, "jB", 'bindrows(expectsInput("a", "numeric", ""), expectsInput("ext", "numeric", ""))',
            'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- sim$a + sim$ext")
  mkJumpMod(mp, "jC", 'bindrows(expectsInput("b", "numeric", ""))',
            'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b * 2")
  mkJumpMod(mp, "jD", 'bindrows(expectsInput("cc", "numeric", ""), expectsInput("shared", "numeric", ""))',
            'bindrows(createsOutput("d", "numeric", ""), createsOutput("shared", "numeric", ""))',
            "sim$d <- sim$cc + 1; sim$shared <- 200")
}

jumpParams <- function(...) {
  p <- list(jA = list(.useCache = "init"), jB = list(.useCache = "init"),
            jC = list(.useCache = "init"), jD = list(.useCache = "init"))
  modifyList(p, list(...))
}

## `spades.debug = TRUE` + `reproducible.verbose = 1` so the chaining messages are emitted
## (as in test-cacheChaining.R's liveness test); `debug` is therefore not passed here.
jumpOpts <- list(reproducible.useMemoise = FALSE, spades.debug = TRUE, reproducible.verbose = 1)

## Every jump test runs with memoise off and on: with it on, the jump's loadFromCache() and the
## plain Cache() hits share one memoise environment, and that once broke a run.
jumpTest <- function(desc, code) {
  code <- substitute(code)
  for (memo in c(FALSE, TRUE)) {
    body <- bquote({
      jumpOpts <- modifyList(jumpOpts, list(reproducible.useMemoise = .(memo)))
      .(code)
    })
    eval(bquote(test_that(.(paste0(desc, " [memoise ", if (memo) "on" else "off", "]")), .(body))))
  }
}

runJump <- function(mp, cp, params, ext = 5, chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  simInitAndSpades(times = list(start = 1, end = 2), params = params, objects = list(ext = ext),
                   modules = list("jA", "jB", "jC", "jD"),
                   paths = list(modulePath = mp, cachePath = cp))
}

grownOf <- function(sim) unname(unlist(mget(paste0("grown_", c("jA", "jB", "jC", "jD")), envir = sim@.xData)))

jumpMessages <- function(expr) {
  m <- capture_messages(expr)
  list(jumps = restoredMsgs(m),
       chains = grep("Using cacheChaining", m, value = TRUE),
       raw = m)
}

stateOf <- function(sim) {
  list(objs = mget(c("a", "b", "cc", "d", "shared", "grown_jA", "grown_jB", "grown_jC", "grown_jD"),
                   envir = sim@.xData, ifnotfound = list(NULL)),
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]),
       time = time(sim))
}

jumpTest("a run of cached events is recovered in one jump, with the same result", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)

  ## reference: caching without chaining, cold then warm
  cpOff <- file.path(tmpdir, "off")
  runJump(mp, cpOff, jumpParams(), chaining = FALSE)
  ref <- runJump(mp, cpOff, jumpParams(), chaining = FALSE)

  cp <- file.path(tmpdir, "on")
  cold <- jumpMessages(s1 <- runJump(mp, cp, jumpParams()))
  expect_length(cold$jumps, 0L) # nothing recorded yet

  warm <- jumpMessages(s2 <- runJump(mp, cp, jumpParams()))
  ## jA init is a genuine hit (first cached event of the run, nothing to chain from);
  ## jB init chains, and from there the walk reaches jC and jD in one step.
  expect_length(warm$jumps, 1L)
  expect_match(warm$jumps, "restored 3 cached events in one step")

  ## the skipped events are listed, numbered in the order they would have run, with cache IDs
  listMsg <- grep("1\\.\\s*jB init", warm$raw, value = TRUE)
  expect_length(listMsg, 1L)
  expect_match(listMsg, "1\\.\\s+jB init\\s+\\([[:alnum:]]+\\)")
  expect_match(listMsg, "2\\.\\s+jC init\\s+\\([[:alnum:]]+\\)")
  if (length(listMsg))
    expect_lt(regexpr("1\\. jB init", listMsg)[[1]], regexpr("2\\. jC init", listMsg)[[1]])

  ## every answer identical to caching without chaining
  expect_equal(stateOf(s2)$objs, stateOf(ref)$objs)
  ## the skipped events are still reported as completed, in order
  expect_equal(stateOf(s2)$completed, stateOf(ref)$completed)
  ## objects from skipped entries: b from jB, cc from jC; `shared` is jD's (200), not jA's (100)
  expect_equal(s2$b, 6); expect_equal(s2$cc, 12); expect_equal(s2$d, 13); expect_equal(s2$shared, 200)
  ## the queue after the jump was the recorded one: every module's `grow` still ran
  expect_equal(grownOf(s2), rep(2, 4))
})

jumpTest("a jump announces its events once, labelled with the landing event", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")
  runJump(mp, cp, jumpParams())
  warm <- jumpMessages(runJump(mp, cp, jumpParams()))
  m <- gsub("\\s+", " ", cli::ansi_strip(warm$raw))

  ## one header, one numbered list: each restored event appears exactly once
  expect_length(grep("cacheChaining: restored 3 cached events in one step", m), 1L)
  for (ev in c("jB init", "jC init", "jD init"))
    expect_length(grep(paste0("\\d\\. ", ev, " \\("), m), 1L)
  expect_length(grep("continuing with the next scheduled event", m), 0L)
  ## the plain "Using cacheChaining ..." is for a chain of one event, not for a jump
  expect_length(grep("Using cacheChaining \\.\\.\\.", m), 0L)
  ## reproducible's cacheId override message repeats what was just said
  if (packageVersion("reproducible") >= "3.2.1.9051") # older versions do not know cacheIdAnnounced
    expect_length(grep("cacheId passed to override", m), 0L)
  ## the entry loaded is the landing event's (jD), not the one the call started from (jB)
  expect_length(grep("Loaded! (Cached|Memoised) result from previous doEvent.jB::init", m), 0L)
  expect_length(grep("Object to retrieve \\(fn: doEvent.jB::init", m), 0L)
  expect_length(grep("Loaded! (Cached|Memoised) result from previous doEvent.jD::init", m), 1L)
})

test_that("messageNewObjects prints no column header, however many rows", {
  for (n in c(3, 25)) {
    m <- capture_messages(messageNewObjects(setNames(vector("list", n), paste0("obj", seq_len(n))),
                                            verbose = 1))
    m <- unlist(strsplit(cli::ansi_strip(m), "\n"))
    expect_false(any(grepl("newObjects|<char>", m)), info = paste(n, "rows"))
    expect_equal(sum(grepl("^ *[0-9]+: +obj", m)), n, info = paste(n, "rows"))
  }
})

jumpTest("a novel object supplied at simInit stops the jump at the module that reads it", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")

  runJump(mp, cp, jumpParams(), ext = 5)
  runJump(mp, cp, jumpParams(), ext = 5) # records + uses the full chain

  ## `ext` is read by jB only. jA init still hits; jB's chain link is recorded, but its external
  ## input differs, so nothing may be skipped past jB -- it has to compute with the new value.
  got <- jumpMessages(s <- runJump(mp, cp, jumpParams(), ext = 50))
  expect_length(got$jumps, 0L)
  expect_equal(s$b, 51); expect_equal(s$cc, 102); expect_equal(s$d, 103)

  ## and now that chain is recorded too: the same call jumps, to the same answer
  again <- jumpMessages(s3 <- runJump(mp, cp, jumpParams(), ext = 50))
  expect_length(again$jumps, 1L)
  expect_equal(stateOf(s3)$objs, stateOf(s)$objs)
})

jumpTest("an uncached event in the middle ends the jump there", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")
  params <- jumpParams(jC = list(.useCache = FALSE)) # jC init always runs

  runJump(mp, cp, params)
  got <- jumpMessages(s <- runJump(mp, cp, params))
  ## jB can chain from jA but there is no cached jC to walk on to; jD's chain starts after jC.
  expect_length(got$jumps, 0L)
  expect_equal(s$d, 13)
  expect_equal(s$shared, 200)
})

jumpTest("a deleted entry in the chain shortens the jump instead of breaking the run", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")

  runJump(mp, cp, jumpParams())
  runJump(mp, cp, jumpParams())
  sc <- showCache(cp, verbose = -2)
  idD <- unique(sc[tagKey == "function" & grepl("^doEvent.jD", tagValue)]$cacheId)
  clearCache(cp, cacheId = idD, ask = FALSE)

  got <- jumpMessages(s <- runJump(mp, cp, jumpParams()))
  expect_length(got$jumps, 1L)
  expect_match(got$jumps, "restored 2 cached events in one step")
  expect_equal(s$d, 13) # jD ran and its result is right
  expect_equal(s$shared, 200)
})

## Per-event controls live in doEvent(): the .stopBefore/.stopAfter barriers, the `events` whitelist
## and end(sim) are tested on each event as it is dequeued, and spades.evalPostEvent runs after each.
## A skipped event never passes through there, so the walk has to stop at any event those act on.
## Oracle for every control: with a chain recorded end to end WITHOUT the control, chaining off and
## chaining on end in the same state -- objects, completed events with their times, clock, and
## stoppedAt().
runJumpCtl <- function(mp, cp, params, events = NULL, end = 2, chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  simInitAndSpades(times = list(start = 1, end = end), params = params, objects = list(ext = 5),
                   modules = list("jA", "jB", "jC", "jD"),
                   paths = list(modulePath = mp, cachePath = cp), events = events)
}

ctlStateOf <- function(sim) {
  comp <- as.data.frame(completed(sim))
  st <- stoppedAt(sim)
  list(objs = stateOf(sim)$objs,
       completed = comp[, c("moduleName", "eventType", "eventTime")],
       time = time(sim),
       stoppedAt = if (is.null(st)) NULL else c(st$moduleName, st$eventType))
}

controlOutcomes <- function(mp, root, params, events = NULL, end = 2) {
  lapply(c(off = FALSE, on = TRUE), function(chaining) {
    cp <- file.path(root, if (chaining) "on" else "off")
    runJumpCtl(mp, cp, params, chaining = chaining) # records the chain end to end
    runJumpCtl(mp, cp, params, chaining = chaining)
    m <- capture_messages(s <- runJumpCtl(mp, cp, params, events = events, end = end, chaining = chaining))
    list(state = ctlStateOf(s), jumps = restoredMsgs(m),
         hooks = sum(grepl("postEventHook", m)))
  })
}

jumpTest("a jump stops at .stopBefore and .stopAfter barriers, on the event it starts from or a later one", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  barriers <- list(
    stopBeforeLater = list(.stopBefore = list(jC = "init")), # a successor inside the recorded chain
    stopAfterFirst  = list(.stopAfter = list(jB = "init")),  # the event the jump would start from
    stopAfterLater  = list(.stopAfter = list(jD = "init"))
  )
  res <- lapply(names(barriers), function(nm)
    controlOutcomes(mp, file.path(tmpdir, nm), jumpParams(), events = barriers[[nm]]))
  names(res) <- names(barriers)
  for (nm in names(res)) expect_equal(res[[nm]]$on$state, res[[nm]]$off$state, info = nm)
  expect_equal(res$stopBeforeLater$on$state$stoppedAt, c("jC", "init"))
  ## liveness: with the barrier on jD the walk still skips jC -- and stops before jD
  expect_length(res$stopAfterLater$on$jumps, 1L)
  expect_match(res$stopAfterLater$on$jumps, "restored 2 cached events in one step")
})

jumpTest("a jump does not recover an event the `events` whitelist excludes", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  res <- controlOutcomes(mp, tmpdir, jumpParams(), events = list(jC = character(0)))
  expect_equal(res$on$state, res$off$state)
  expect_null(res$on$state$objs$cc)
})

jumpTest("a jump does not go past end(sim), and skipped events keep their own times", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  growCached <- lapply(jumpParams(), function(p) { p$.useCache <- c("init", "grow"); p })
  res <- controlOutcomes(mp, tmpdir, growCached, end = 1) # the chain was recorded to t = 2
  expect_equal(res$on$state, res$off$state)
  expect_equal(as.numeric(res$on$state$time), 1) # time(sim) carries a `unit` attribute
  ## liveness: the t = 1 events are still skipped, up to the last one before end(sim)
  expect_length(res$on$jumps, 1L)
  expect_match(res$on$jumps, "restored 3 cached events in one step")
})

jumpTest("no jump while spades.evalPostEvent is set: the hook sees every event", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  ## options(), not withr::local_options(): the latter leaves this quoted-call option unset
  op <- options(spades.evalPostEvent = quote(message("postEventHook")))
  withr::defer(options(op))
  res <- controlOutcomes(mp, tmpdir, jumpParams())
  expect_length(res$on$jumps, 0L)
  expect_equal(res$on$state, res$off$state)
  expect_gt(res$off$hooks, 0)
  expect_equal(res$on$hooks, res$off$hooks)
})

## With memoise on, a jump reads skipped entries with reproducible::loadFromCache(), which memoised
## them in a different form from the one a plain Cache() hit reads: after a jump, a plain hit on the
## same cacheId got a `list` instead of a simList.
test_that("with memoise on, plain Cache() hits after a jump in the same session", {
  skip_on_cran()
  testInit("terra", opts = modifyList(jumpOpts, list(reproducible.useMemoise = TRUE)))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")

  ref <- runJump(mp, cp, jumpParams())
  runJump(mp, cp, jumpParams()) # records the chain

  me <- reproducible:::memoiseEnv(cp)
  rm(list = ls(me), envir = me) # a new session
  j <- jumpMessages(runJump(mp, cp, jumpParams()))
  expect_length(j$jumps, 1L)
  s <- runJump(mp, cp, jumpParams(), chaining = FALSE)
  expect_equal(stateOf(s), stateOf(ref))
})

## Same four-module chain as jumpFixture(), but every module's cached work happens in
## `.inputObjects` instead of `init`. ioB reads `a`/`ext` and creates `b`; ioC reads `b` and creates
## `cc`; ioD reads `cc`/`shared` (merely passing `cc` through, never reassigning it) and creates
## `d`/`shared`. A jump starting at ioB's own entry walks forward and lands on ioD, skipping ioC.
mkIOJumpMod <- function(mp, name, inObjs, outObjs, ioBody) {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = {})
  return(invisible(sim))
}

.inputObjects <- function(sim) { %s; return(invisible(sim)) }
', name, name, inObjs, outObjs, name, ioBody), file.path(d, paste0(name, ".R")))
  invisible(name)
}

ioJumpFixture <- function(mp) {
  mkIOJumpMod(mp, "ioA", 'bindrows(expectsInput("unsupplied0", "numeric", ""))',
              'bindrows(createsOutput("a", "numeric", ""), createsOutput("shared", "numeric", ""))',
              "sim$a <- 1; sim$shared <- 100")
  mkIOJumpMod(mp, "ioB", 'bindrows(expectsInput("a", "numeric", ""), expectsInput("ext", "numeric", ""))',
              'bindrows(createsOutput("b", "numeric", ""))', "sim$b <- sim$a + sim$ext")
  mkIOJumpMod(mp, "ioC", 'bindrows(expectsInput("b", "numeric", ""))',
              'bindrows(createsOutput("cc", "numeric", ""))', "sim$cc <- sim$b * 2")
  mkIOJumpMod(mp, "ioD", 'bindrows(expectsInput("cc", "numeric", ""), expectsInput("shared", "numeric", ""))',
              'bindrows(createsOutput("d", "numeric", ""), createsOutput("shared", "numeric", ""))',
              "sim$d <- sim$cc + 1; sim$shared <- 200")
}

ioJumpParams <- function() {
  list(ioA = list(.useCache = ".inputObjects"), ioB = list(.useCache = ".inputObjects"),
       ioC = list(.useCache = ".inputObjects"), ioD = list(.useCache = ".inputObjects"))
}

runIOJump <- function(mp, cp, chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  simInit(times = list(start = 1, end = 2), params = ioJumpParams(), objects = list(ext = 5),
         modules = list("ioA", "ioB", "ioC", "ioD"),
         paths = list(modulePath = mp, cachePath = cp))
}

jumpTest("a jump landing on .inputObjects restores an expectsInput the landing module only reads", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "iomods"); dir.create(mp, showWarnings = FALSE)
  ioJumpFixture(mp)
  cp <- file.path(tmpdir, "io")

  runIOJump(mp, cp)                              # cold: writes every entry, records the chain
  got <- jumpMessages(sim <- runIOJump(mp, cp))   # warm: ioB's own hit jumps, landing on ioD
  expect_length(got$jumps, 1L)
  expect_match(got$jumps, "restored 3 cached events in one step")

  ## `cc` is ioC's own createsOutput and also ioD's expectsInput; ioD's `.inputObjects` only reads
  ## it (never reassigns it), so it is not among ioD's own cache entry's restorable objects and
  ## must come from ioC's own (skipped) entry instead.
  expect_true(exists("cc", envir = sim@.xData, inherits = FALSE))
  expect_equal(sim$cc, 12)
  expect_equal(sim$d, 13)
  expect_equal(sim$shared, 200)
})

## The queue after a jump is the LIVE queue, minus the events the jump restored, plus what those events
## scheduled -- never the queue stored in the entry the jump lands on. That queue belongs to the run that
## saved the entry, and an entry is shared by runs with different module sets. Each entry carries its own
## event's queue delta (the `eventQueueDelta` tag); the walk replays them onto the live queue and only
## skips an event that is the next one in it. Oracle: the same modules, chaining off, cold cache.
jumpKeys <- c("a", "b", "cc", "d", "shared", "x")

## `.inputObjects` phase modules, or init-phase modules, from a list of names; each extra module reads
## something a fixture module makes and creates `x`
mkExtraMod <- function(mp, name, phaseIO, expects = "a") {
  inObjs <- sprintf('bindrows(expectsInput("%s", "numeric", ""))', expects)
  outObjs <- 'bindrows(createsOutput("x", "numeric", ""))'
  body <- sprintf("sim$x <- sim$%s + 1000", expects)
  if (phaseIO) mkIOJumpMod(mp, name, inObjs, outObjs, body)
  else mkJumpMod(mp, name, inObjs, outObjs, body)
}

runMods <- function(mp, cp, mods, phaseIO, chaining = TRUE) {
  withr::local_options(spades.cacheChaining = chaining)
  params <- lapply(stats::setNames(mods, mods), function(m)
    list(.useCache = if (phaseIO) ".inputObjects" else "init"))
  run <- if (phaseIO) simInit else simInitAndSpades
  run(times = list(start = 1, end = 2), params = params, objects = list(ext = 5),
      modules = as.list(mods), paths = list(modulePath = mp, cachePath = cp))
}

modsState <- function(sim, mods) {
  list(objs = mget(c(jumpKeys, paste0("grown_", mods)), envir = sim@.xData, ifnotfound = list(NULL)),
       completed = as.data.frame(completed(sim)[, c("moduleName", "eventType")]))
}

## record the chain with `recMods` (cold, then warm: the warm run has the chain to follow), then run
## with `runModsNow`; the answer must be what the same modules give with chaining off
queueCase <- function(tmpdir, phaseIO, recMods, runModsNow, mkMods = NULL) {
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  if (phaseIO) ioJumpFixture(mp) else jumpFixture(mp)
  if (is.function(mkMods)) mkMods(mp)
  cp <- file.path(tmpdir, "on")
  runMods(mp, cp, recMods, phaseIO)
  runMods(mp, cp, recMods, phaseIO)
  got <- jumpMessages(s <- runMods(mp, cp, runModsNow, phaseIO))
  ref <- runMods(mp, file.path(tmpdir, "off"), runModsNow, phaseIO, chaining = FALSE)
  list(got = got, state = modsState(s, runModsNow), ref = modsState(ref, runModsNow))
}

phaseNames <- function(phaseIO) {
  list(ph = if (phaseIO) ".inputObjects" else "init",
       base = if (phaseIO) c("ioA", "ioB", "ioC", "ioD") else c("jA", "jB", "jC", "jD"),
       xm = if (phaseIO) "ioX" else "jX",
       renamed = if (phaseIO) "ioE" else "jE")
}

## the extra module comes last: its event is in the queue the landing entry stored
scenExtraLast <- function(tmpdir, phaseIO) {
  n <- phaseNames(phaseIO)
  queueCase(tmpdir, phaseIO, c(n$base, n$xm), n$base,
            mkMods = function(mp) mkExtraMod(mp, n$xm, phaseIO, expects = "d"))
}

## the extra module loads between the second and third: its event is in the live queue only
scenExtraMiddle <- function(tmpdir, phaseIO) {
  n <- phaseNames(phaseIO)
  queueCase(tmpdir, phaseIO, n$base, c(n$base[1:2], n$xm, n$base[3:4]),
            mkMods = function(mp) mkExtraMod(mp, n$xm, phaseIO, expects = "a"))
}

## the last module renamed, same code: the landing entry's queue names the old one
scenRenamed <- function(tmpdir, phaseIO) {
  n <- phaseNames(phaseIO)
  mk <- if (phaseIO) mkIOJumpMod else mkJumpMod
  queueCase(tmpdir, phaseIO, n$base, c(n$base[1:3], n$renamed),
            mkMods = function(mp)
              mk(mp, n$renamed, 'bindrows(expectsInput("cc", "numeric", ""), expectsInput("shared", "numeric", ""))',
                 'bindrows(createsOutput("d", "numeric", ""), createsOutput("shared", "numeric", ""))',
                 "sim$d <- sim$cc + 1; sim$shared <- 200"))
}

for (phaseIO in c(TRUE, FALSE)) {
  ph <- phaseNames(phaseIO)$ph
  eval(bquote({
    jumpTest(.(paste0("a chain recorded with an extra module still jumps without it (", ph, ")")), {
      skip_on_cran()
      testInit("terra", opts = jumpOpts)
      r <- scenExtraLast(tmpdir, .(phaseIO))
      n <- phaseNames(.(phaseIO))
      ## `.inputObjects` keys leave out the queue and the other modules, so the entries are shared and the
      ##   jump engages; an event's key includes the queue, so a different module set never hits there
      if (.(phaseIO)) {
        expect_length(r$got$jumps, 1L)
        expect_match(r$got$jumps, "restored 3 cached events in one step")
        expect_identical(restoredList(r$got$raw), c("ioB .inputObjects", "ioC .inputObjects", "ioD .inputObjects"))
      }
      expect_equal(r$state, r$ref)
      expect_null(r$state$objs$x)
    })

    jumpTest(.(paste0("a chain recorded without a module still runs that module's events when it is added (", ph, ")")), {
      skip_on_cran()
      testInit("terra", opts = jumpOpts)
      r <- scenExtraMiddle(tmpdir, .(phaseIO))
      n <- phaseNames(.(phaseIO))
      expect_equal(r$state, r$ref)
      expect_equal(r$state$objs$x, 1001)
      if (!.(phaseIO)) expect_equal(as.numeric(r$state$objs$grown_jX), 2)
      expect_true(paste(n$xm, n$ph) %in% paste(r$state$completed$moduleName, r$state$completed$eventType))
    })

    jumpTest(.(paste0("a chain recorded before a module was renamed does not queue the old name (", ph, ")")), {
      skip_on_cran()
      testInit("terra", opts = jumpOpts)
      r <- scenRenamed(tmpdir, .(phaseIO))
      expect_equal(r$state$objs[jumpKeys], r$ref$objs[jumpKeys])
      expect_equal(r$state$completed, r$ref$completed)
      expect_equal(r$state$objs$d, 13)
    })
  }))
}

## init phase: skipped events schedule events of their own module; each must be queued and run once
## (not lost, not twice) after the jump. jC schedules two identical `grow` events.
jumpTest("events a skipped event scheduled are queued after a jump, duplicates counted", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  mkJumpMod(mp, "jC", 'bindrows(expectsInput("b", "numeric", ""))', 'bindrows(createsOutput("cc", "numeric", ""))',
            'sim$cc <- sim$b * 2; sim <- scheduleEvent(sim, time(sim) + 1, "jC", "grow")')
  cp <- file.path(tmpdir, "on")
  mods <- c("jA", "jB", "jC", "jD")
  runMods(mp, cp, mods, FALSE)
  runMods(mp, cp, mods, FALSE)
  got <- jumpMessages(s <- runMods(mp, cp, mods, FALSE))
  ref <- runMods(mp, file.path(tmpdir, "off"), mods, FALSE, chaining = FALSE)
  expect_length(got$jumps, 1L)
  expect_match(got$jumps, "restored 3 cached events in one step")
  expect_identical(restoredList(got$raw), c("jB init", "jC init", "jD init"))
  expect_equal(modsState(s, mods), modsState(ref, mods))
  comp <- completed(s)
  expect_equal(sum(comp$moduleName == "jC" & comp$eventType == "grow"), 2L)
  expect_equal(sum(comp$moduleName == "jB" & comp$eventType == "grow"), 1L)
  expect_equal(NROW(events(s)), 0L)
})

## An entry saved before queue deltas were recorded has none: the walk cannot cross it, so that link
## is an ordinary single-event cache hit. Simulated by recording with the delta not written.
jumpTest("an entry without a queue delta stops the jump", {
  skip_on_cran()
  testInit("terra", opts = jumpOpts)
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  jumpFixture(mp)
  cp <- file.path(tmpdir, "on")
  mods <- c("jA", "jB", "jC", "jD")
  local({
    testthat::local_mocked_bindings(.chainDeltaIfRan = function(...) NULL)
    runMods(mp, cp, mods, FALSE)
    runMods(mp, cp, mods, FALSE)
  })
  got <- jumpMessages(s <- runMods(mp, cp, mods, FALSE))
  ref <- runMods(mp, file.path(tmpdir, "off"), mods, FALSE, chaining = FALSE)
  expect_length(got$jumps, 0L)
  expect_equal(modsState(s, mods), modsState(ref, mods))
})
