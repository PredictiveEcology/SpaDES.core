test_that("default flow of an if-chain module with a run mode", {
  testInit()
  ef <- moduleEventFlow("fireMod", dirname(efFixtureFire(tmpdir)))
  expect_s3_class(ef, "moduleEventFlow")
  ev <- ef$default$events
  expect_identical(ev$Burn[c("status", "priority", "recurring", "interval", "intervalLabel")],
                   list(status = "on", priority = 2.5, recurring = TRUE, interval = 1, intervalLabel = "fireTimestep"))
  expect_identical(ev$summaryMulti$status, "off")
  expect_identical(ev$summaryMulti$cond, 'mode == "multi"')
  expect_identical(ev$save[c("status", "cond")], list(status = "off", cond = ".saveInitialTime is set"))
  expect_identical(SpaDES.core:::.ef_timingText(ev$Burn, "year"),
                   "every year (fireTimestep); first at start + 1 year (burnInitialTime)")
  expect_identical(ef$modeParam, "mode")
  expect_identical(ef$modeValues, c("single", "multi"))
  expect_identical(ef$modes$multi$status[c("Burn", "summaryMulti")], c(Burn = "off", summaryMulti = "on"))
})
test_that("switch module: locals, data-dependent conditions, fall-through, unscheduled", {
  testInit()
  ef <- moduleEventFlow("vegMod", dirname(efFixtureVeg(tmpdir)))
  ev <- ef$default$events
  expect_identical(ev$summaryStart[c("status", "cond")], list(status = "off", cond = "depends on calcSummary"))
  expect_identical(ev$Dispersal[c("priority", "interval", "intervalLabel")],
                   list(priority = 5, interval = 10, intervalLabel = "successionTimestep"))
  expect_identical(ev$Growth$status, "cond")
  expect_identical(unname(ef$default$status[c("Grow2", "orphan")]), c("unscheduled", "unscheduled"))
  expect_null(ef$modeParam)
})
test_that("per-event doEvent functions and the newModule() template both read", {
  testInit(smcc = FALSE)
  expect_true(moduleEventFlow("evMod", dirname(efFixtureEvents(tmpdir)))$default$events$step$recurring)
  newModule("tmplMod", tmpdir, open = FALSE)
  ef <- moduleEventFlow("tmplMod", tmpdir)
  expect_setequal(names(ef$default$status), c("init", "plot", "save", "event1", "event2"))
})
test_that("module code is never run while reading", {
  testInit()
  f <- tempfile(); writeLines("x", f); d <- tempfile(); dir.create(d)
  efModule(tmpdir, "unsafeMod", code = sprintf(
    'doEvent.unsafeMod <- function(sim, eventTime, eventType) {
       switch(eventType, init = {
         x <- unlink("%s", recursive = TRUE)
         if (file.remove("%s")) sim <- scheduleEvent(sim, start(sim), "unsafeMod", "a")
       }, a = {})
       invisible(sim)
     }', d, f))
  ef <- moduleEventFlow("unsafeMod", tmpdir)
  expect_true(file.exists(f)); expect_true(dir.exists(d))
  expect_identical(ef$default$events$a$status, "cond")
})
test_that("a module with no events reads as having none", {
  testInit()
  efModule(tmpdir, "parentMod", code = "")
  expect_length(moduleEventFlow("parentMod", tmpdir)$default$events, 0)
})
test_that("an event reached only through a conditional event takes that event's condition", {
  testInit()
  efModule(tmpdir, "chainMod", code = c(
    "doEvent.chainMod <- function(sim, eventTime, eventType) {",
    "  switch(eventType,",
    "    init = {",
    "      if (nrow(sim$x) > 0) sim <- scheduleEvent(sim, start(sim), \"chainMod\", \"A\")",
    "    },",
    "    A = {",
    "      sim <- scheduleEvent(sim, time(sim) + 1, \"chainMod\", \"B\")",
    "    },",
    "    B = {}",
    "  )",
    "  invisible(sim)",
    "}"
  ))
  ev <- moduleEventFlow("chainMod", tmpdir)$default$events
  expect_identical(ev$A[c("status", "cond")], list(status = "cond", cond = "nrow(sim$x) > 0"))
  expect_identical(ev$B[c("status", "cond")], list(status = "cond", cond = "nrow(sim$x) > 0"))
  expect_false(grepl("NA", SpaDES.core:::.ef_timingText(ev$B, "year")))
})
test_that(".ef_timingText prints no missing condition and no leading separator", {
  testInit()
  ev <- list(status = "cond", recurring = FALSE, firsts = data.frame(),
             repeatStatus = NA_character_, cond = NA_character_)
  expect_identical(SpaDES.core:::.ef_timingText(ev, "year"), "")
  ev$cond <- "x is set"
  expect_identical(SpaDES.core:::.ef_timingText(ev, "year"), "only if x is set")
})
