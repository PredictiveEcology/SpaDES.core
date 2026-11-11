## A cached event's entry kept, recorded as changed and, on a hit, restored every `createsOutput`
## of its module that is not also an `expectsInput`, whether or not the event wrote it. A later
## event's hit then put back an old copy of an object an earlier event had just remade (FireSense:
## fireSense_dataPrepFit's prepIgnitionFitData hit restored an old fireBufferedListDT over the one
## prepSpreadFitData had just rebuilt). `createsOutput(events = )` says which events write an
## output; an event's entry keeps, and its hit restores, only the outputs declared for it, plus
## those that declare no `events`.

## One module: event A writes `big` from parameter `v` (in A's key only, under
## spades.cacheKeyReachable); B writes `small`, C `tiny` (random, so a hit is visible); A makes
## `dt` and B changes it by reference. All three events are cached. `declare = FALSE`: no `events`.
.writeEvMod <- function(mp, declare = TRUE) {
  ev <- function(x) if (declare) sprintf(', events = %s', x) else ""
  d <- file.path(mp, "evMod")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "evMod", description = "", keywords = "", authors = person("A", "B"),
  childModules = character(0), version = list(evMod = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list("data.table"),
  parameters = rbind(
    defineParameter("v", "numeric", 1, NA, NA, "what A writes into big"),
    defineParameter(".useCache", "character", c("A", "B", "C"), NA, NA, "")),
  inputObjects = bindrows(),
  outputObjects = bindrows(
    createsOutput("big", "numeric", "written by A"%s),
    createsOutput("small", "numeric", "written by B"%s),
    createsOutput("tiny", "numeric", "written by C"%s),
    createsOutput("dt", "data.table", "made by A, changed by reference by B"%s))))

doEvent.evMod <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      sim <- scheduleEvent(sim, start(sim), "evMod", "A", eventPriority = 6)
      sim <- scheduleEvent(sim, start(sim), "evMod", "B", eventPriority = 7)
      sim <- scheduleEvent(sim, start(sim), "evMod", "C", eventPriority = 8)
    },
    A = { sim$big <- P(sim)$v; sim$dt <- data.table::data.table(y = 0) },
    B = { sim$small <- runif(1); data.table::set(sim$dt, j = "y", value = 10) },
    C = { sim$tiny <- runif(1) })
  invisible(sim)
}
', ev('"A"'), ev('"B"'), ev('"C"'), ev('c("A", "B")')), file.path(d, "evMod.R"))
}

.runEvMod <- function(mp, cp, v, debug = FALSE) {
  s <- simInit(times = list(start = 0, end = 1), modules = "evMod",
               params = list(evMod = list(v = v)),
               paths = list(modulePath = mp, cachePath = cp))
  spades(s, debug = debug)
}

.evOpts <- function(chain)
  list(spades.cacheKeyReachable = TRUE, spades.cacheChaining = chain,
       spades.moduleCodeChecks = FALSE, spades.loadReqdPkgs = FALSE)

for (chain in c(FALSE, TRUE)) {
  test_that(paste0("a cached event restores only the outputs declared for it (cacheChaining ",
                   chain, ")"), {
    skip_on_cran()
    testInit("data.table", opts = .evOpts(chain))
    mp <- file.path(tmpdir, "mods")
    cp <- file.path(tmpdir, "cache")
    .writeEvMod(mp, declare = TRUE)

    r1 <- .runEvMod(mp, cp, v = 1)
    expect_identical(r1$big, 1)
    r2 <- .runEvMod(mp, cp, v = 2) # A misses (v in its key); B and C hit
    expect_identical(r2$big, 2)    # was 1: B's and C's hits restored run 1's `big`
    ## B's and C's own outputs come from their entries (the run-1 random draws), so they hit
    expect_identical(r2$small, r1$small)
    expect_identical(r2$tiny, r1$tiny)
    ## B changed `dt` by reference; declared for B, so B's hit restores it
    expect_identical(r2$dt$y, 10)

    ## all cached; under cacheChaining a jump B -> C (its messages need debug and verbose)
    withr::with_options(list(reproducible.verbose = 1),
                        m <- capture_messages(r3 <- .runEvMod(mp, cp, v = 2, debug = TRUE)))
    if (chain) expect_identical(restoredList(m), c("evMod B", "evMod C"))
    expect_identical(r3$big, 2)
    expect_identical(r3$small, r1$small)
    expect_identical(r3$tiny, r1$tiny)
    expect_identical(r3$dt$y, 10)

    ## the entries record only their own outputs as changed
    sc <- reproducible::showCache(cp, verbose = -1)
    tg <- merge(sc[sc$tagKey == "eventType", c("cacheId", "tagValue")],
                sc[sc$tagKey == "eventObjectsChanged", c("cacheId", "tagValue")], by = "cacheId")
    chg <- strsplit(tg$tagValue.y[tg$tagValue.x == "B"], "\t", fixed = TRUE)
    for (x in chg) expect_false("big" %in% x)
  })
}

test_that("a module that declares no events keeps the old restore", {
  skip_on_cran()
  testInit("data.table", opts = .evOpts(FALSE))
  mp <- file.path(tmpdir, "mods")
  cp <- file.path(tmpdir, "cache")
  .writeEvMod(mp, declare = FALSE)

  r1 <- .runEvMod(mp, cp, v = 1)
  r2 <- .runEvMod(mp, cp, v = 2)
  expect_identical(r2$small, r1$small)
  ## unchanged behaviour: B's (or C's) entry counts `big` as changed and restores run 1's copy
  expect_identical(r2$big, 1)
  expect_setequal(.outputsOfEvent(r2@depends@dependencies$evMod, "B"), c("big", "small", "tiny", "dt"))
  expect_false(.txtOutputEvents %in% colnames(r2@depends@dependencies$evMod@outputObjects))
})

test_that("a hit on an entry saved before events were declared restores only the declared outputs", {
  ## `changed` of such an entry names every createsOutput; the restore takes those of its event only
  testInit("data.table", opts = .evOpts(FALSE))
  mp <- file.path(tmpdir, "mods")
  .writeEvMod(mp, declare = TRUE)
  s <- simInit(times = list(start = 0, end = 1), modules = "evMod", paths = list(modulePath = mp))
  deps <- s@depends@dependencies
  oldChanged <- list(big = "a", small = "b", tiny = "c", dt = "d")
  ls1 <- c("big", "small", "tiny", "dt")
  expect_identical(lsObjectsChanged(ls1, oldChanged, 1L, "evMod", deps, "B"), c("small", "dt"))
  expect_identical(lsObjectsChanged(ls1, oldChanged, 1L, "evMod", deps, "C"), "tiny")
  ## no event (a whole-simList call): all
  expect_identical(lsObjectsChanged(ls1, oldChanged, 1L, "evMod", deps), ls1)
  ## a cacheChaining jump: what a skipped entry may give, and what the landing entry restored
  expect_identical(.chainOutputs(s, "evMod", "B"), c("small", "dt"))
  expect_identical(.chainRestored(s, names(oldChanged), "evMod", "C"), "tiny")
})

## `.inputObjects`: modX's `.inputObjects` makes `X` from `v`; modY declares `X` as an output of its
## `init` and `yy` as one of its `.inputObjects`, which is cached. modY's `.inputObjects` hit put
## back run 1's `X`. modY's `.inputObjects` key does not include `outputObjects`, so an entry saved
## before modY declared `events` is hit after; it must not restore `X` either.
.writeIOMods <- function(mp, declare = TRUE) {
  ev <- function(x) if (declare) sprintf(', events = "%s"', x) else ""
  hdr <- 'defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("A", "B"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(%s), inputObjects = bindrows(), outputObjects = bindrows(%s)))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) invisible(sim)
.inputObjects <- function(sim) { %s; invisible(sim) }
'
  for (m in c("modX", "modY")) dir.create(file.path(mp, m), recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf(hdr, "modX", "modX", 'defineParameter("v", "numeric", 1, NA, NA, "")',
                     'createsOutput("X", "numeric", "")', "modX", "sim$X <- P(sim)$v"),
             file.path(mp, "modX", "modX.R"))
  writeLines(sprintf(hdr, "modY", "modY",
                     'defineParameter(".useCache", "character", ".inputObjects", NA, NA, "")',
                     paste0('createsOutput("X", "numeric", ""', ev("init"), '), ',
                            'createsOutput("yy", "numeric", ""', ev(".inputObjects"), ')'),
                     "modY", "sim$yy <- runif(1)"),
             file.path(mp, "modY", "modY.R"))
}

.runIOMods <- function(mp, cp, v)
  simInit(times = list(start = 0, end = 1), modules = c("modX", "modY"),
          params = list(modX = list(v = v)), paths = list(modulePath = mp, cachePath = cp))

for (declareFirst in c(TRUE, FALSE)) {
  test_that(paste0(".inputObjects restores only the outputs declared for it (run 1 declared: ",
                   declareFirst, ")"), {
    skip_on_cran()
    testInit(opts = .evOpts(FALSE))
    mp <- file.path(tmpdir, "mods")
    cp <- file.path(tmpdir, "cache")
    .writeIOMods(mp, declare = declareFirst)
    s1 <- .runIOMods(mp, cp, v = 1)
    expect_identical(s1$X, 1)
    .writeIOMods(mp, declare = TRUE)
    s2 <- .runIOMods(mp, cp, v = 2)
    expect_identical(s2$yy, s1$yy) # modY's .inputObjects hit, and restored its own output
    expect_identical(s2$X, 2)      # was 1: the hit restored run 1's `X`
  })
}

test_that("createsOutput keeps events when a non-character argument dispatches to its ANY method", {
  ## desc = NA (logical) reaches the "ANY" method, which dropped named `...`
  out <- createsOutput("x", "numeric", NA, events = c("A", "B"))
  expect_identical(unlist(out[[.txtOutputEvents]]), c("A", "B"))
  ## an unnamed extra piece of `desc` is still pasted into it, once
  out2 <- createsOutput("x", "numeric", NA, " more", events = "A")
  expect_identical(out2$desc, "NA more")
  expect_identical(unlist(out2[[.txtOutputEvents]]), "A")
})
