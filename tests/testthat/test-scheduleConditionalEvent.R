## scheduleConditionalEvent() -- argument validation and the condition forms.

condSim <- function(end = 5) {
  simInit(times = list(start = 0, end = end, timeunit = "year"))
}

test_that("scheduleConditionalEvent demands a simList", {
  testInit()

  expect_error(
    scheduleConditionalEvent(list(), condition = quote(TRUE),
                             moduleName = "m", eventType = "e"),
    "sim must be a simList"
  )
})

test_that("scheduleConditionalEvent rejects a non-numeric minEventTime", {
  testInit()

  expect_error(
    scheduleConditionalEvent(condSim(), condition = quote(time(sim) > 1),
                             moduleName = "m", eventType = "e",
                             minEventTime = "soon"),
    "Invalid or missing minEventTime"
  )
})

test_that("scheduleConditionalEvent rejects a non-numeric maxEventTime", {
  testInit()

  expect_error(
    scheduleConditionalEvent(condSim(), condition = quote(time(sim) > 1),
                             moduleName = "m", eventType = "e",
                             maxEventTime = "later"),
    "Invalid or missing maxEventTime"
  )
})

test_that("scheduleConditionalEvent rejects non-character eventType and moduleName", {
  testInit()

  expect_error(
    scheduleConditionalEvent(condSim(), condition = quote(time(sim) > 1),
                             moduleName = "m", eventType = 99),
    "eventType must be a character"
  )
  expect_error(
    scheduleConditionalEvent(condSim(), condition = quote(time(sim) > 1),
                             moduleName = 99, eventType = "e"),
    "moduleName must be a character"
  )
})

test_that("scheduleConditionalEvent rejects a non-numeric eventPriority", {
  testInit()

  expect_error(
    scheduleConditionalEvent(condSim(), condition = quote(time(sim) > 1),
                             moduleName = "m", eventType = "e",
                             eventPriority = "high"),
    "eventPriority must be a numeric"
  )
})

test_that("scheduleConditionalEvent rejects a condition that is not a call, string or expression", {
  testInit()

  expect_error(
    scheduleConditionalEvent(condSim(), condition = 42,
                             moduleName = "m", eventType = "e"),
    "condition must be a character string or call or expression"
  )
})

test_that("scheduleConditionalEvent warns on an empty condition", {
  testInit()

  expect_warning(
    scheduleConditionalEvent(condSim(), condition = NULL,
                             moduleName = "m", eventType = "e"),
    "Invalid or missing condition"
  )
})

test_that("scheduleConditionalEvent accepts a call, a string and an expression", {
  testInit()

  forms <- list(quote(time(sim) > 1), "time(sim) > 1", expression(time(sim) > 1))

  for (cond in forms) {
    sim <- scheduleConditionalEvent(condSim(), condition = cond,
                                    moduleName = "m", eventType = "e")
    ce <- conditionalEvents(sim)
    expect_identical(NROW(ce), 1L)
    expect_identical(ce$moduleName, "m")
    expect_identical(ce$eventType, "e")
  }
})

test_that("scheduleConditionalEvent records min and max event times", {
  testInit()

  sim <- scheduleConditionalEvent(condSim(end = 10), condition = quote(time(sim) > 1),
                                  moduleName = "m", eventType = "e",
                                  minEventTime = 2, maxEventTime = 6)
  ce <- conditionalEvents(sim)

  expect_identical(as.numeric(ce$minEventTime), 2)
  expect_identical(as.numeric(ce$maxEventTime), 6)
})

test_that("scheduleConditionalEvent appends further events to the queue", {
  testInit()

  sim <- condSim(end = 10)
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) > 1),
                                  moduleName = "modA", eventType = "a",
                                  minEventTime = 1)
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) > 2),
                                  moduleName = "modB", eventType = "b",
                                  minEventTime = 2)

  ce <- conditionalEvents(sim)
  expect_identical(NROW(ce), 2L)
  expect_setequal(ce$moduleName, c("modA", "modB"))
})

test_that("scheduleConditionalEvent orders the queue by minEventTime", {
  testInit()

  ## add the later event first, so the queue must be re-sorted
  sim <- condSim(end = 10)
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) > 5),
                                  moduleName = "later", eventType = "b",
                                  minEventTime = 5)
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) > 1),
                                  moduleName = "earlier", eventType = "a",
                                  minEventTime = 1)

  ce <- conditionalEvents(sim)
  expect_identical(NROW(ce), 2L)
  expect_false(is.unsorted(as.numeric(ce$minEventTime)))
})

## ---- firing semantics, driven through spades() -------------------------
##
## NOTE: defineEvent() clears the event function's enclosing environment (its
## parent is the SpaDES.core namespace), so event code cannot see locals of a
## helper that built the sim. Anything the code needs must live on the sim, and
## the rest is written as literals.

test_that("a conditional event fires when minEventTime is not zero", {
  skip_on_cran()
  testInit()

  ## minEventTime/maxEventTime are stored in seconds; comparing them against
  ## time(sim), which is in the sim's timeunit, meant these never fired at all
  sim <- simInit(times = list(start = 0, end = 6, timeunit = "year"))
  sim$fired <- 0L
  defineEvent(sim, "condEv", moduleName = "cm", code = { sim$fired <- sim$fired + 1L })
  defineEvent(sim, "filler", moduleName = "cm", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "cm", "filler")
  })
  sim <- scheduleEvent(sim, 0, "cm", "filler")
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) >= 0),
                                  moduleName = "cm", eventType = "condEv",
                                  minEventTime = 2, maxEventTime = 5)

  out <- suppressMessages(spades(sim))

  expect_identical(out$fired, 1L)
  expect_false(exists("._conditionalEvents", envir = out, inherits = FALSE))
})

test_that("a conditional event does not fire past its maxEventTime", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 6, timeunit = "year"))
  sim$fired <- 0L
  defineEvent(sim, "condEv", moduleName = "cm", code = { sim$fired <- sim$fired + 1L })
  defineEvent(sim, "filler", moduleName = "cm", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "cm", "filler")
  })
  sim <- scheduleEvent(sim, 0, "cm", "filler")
  ## window opens after the sim has already ended
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) >= 0),
                                  moduleName = "cm", eventType = "condEv",
                                  minEventTime = 10, maxEventTime = 12)

  out <- suppressMessages(spades(sim))

  expect_identical(out$fired, 0L)
})

test_that("a conditional event never runs twice in a row", {
  skip_on_cran()
  testInit()

  sim <- simInit(times = list(start = 0, end = 3, timeunit = "year"))
  sim$fired <- 0L
  ## re-arms itself every run, which previously looped forever at one timestep
  defineEvent(sim, "condEv", moduleName = "cm", code = {
    sim$fired <- sim$fired + 1L
    if (sim$fired < 8L)
      sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) >= 0),
                                      moduleName = "cm", eventType = "condEv",
                                      minEventTime = 0, maxEventTime = 3)
  })
  defineEvent(sim, "filler", moduleName = "cm", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "cm", "filler")
  })
  sim <- scheduleEvent(sim, 0, "cm", "filler")
  sim <- scheduleConditionalEvent(sim, condition = quote(time(sim) >= 0),
                                  moduleName = "cm", eventType = "condEv",
                                  minEventTime = 0, maxEventTime = 3)

  out <- suppressMessages(spades(sim))
  cmp <- completed(out)
  ev <- paste0(cmp$moduleName, ".", cmp$eventType)

  expect_gt(out$fired, 0L)
  backToBack <- sum(ev[-1] == "cm.condEv" & ev[-length(ev)] == "cm.condEv")
  expect_identical(backToBack, 0L)
})

test_that("a conditional event jumps the queue and runs at the current time", {
  skip_on_cran()
  testInit()

  ## a conditional event does not wait its turn: once its condition is true it
  ## is scheduled at the current time, which puts it at the head of the queue
  ## ahead of work already scheduled for later
  sim <- simInit(times = list(start = 0, end = 5, timeunit = "year"))
  sim$firedAt <- numeric(0)
  defineEvent(sim, "condEv", moduleName = "cm", code = {
    sim$firedAt <- c(sim$firedAt, as.numeric(time(sim)))
  })
  defineEvent(sim, "later", moduleName = "cm", code = { sim })
  defineEvent(sim, "trigger", moduleName = "cm", code = { sim$ready <- TRUE })

  for (tt in c(2, 3, 4)) sim <- scheduleEvent(sim, tt, "cm", "later")
  sim <- scheduleEvent(sim, 1, "cm", "trigger")
  sim$ready <- FALSE
  sim <- scheduleConditionalEvent(sim, condition = quote(isTRUE(sim$ready)),
                                  moduleName = "cm", eventType = "condEv",
                                  minEventTime = 0, maxEventTime = 5)

  out <- suppressMessages(spades(sim))
  cmp <- completed(out)
  ev <- paste0(cmp$moduleName, ".", cmp$eventType)

  ## it ran at t = 1, the moment the condition became true ...
  expect_identical(out$firedAt, 1)
  ## ... i.e. immediately after the trigger and before the t = 2 event
  expect_identical(which(ev == "cm.condEv"), which(ev == "cm.trigger") + 1L)
})

test_that("a conditional event whose condition stays false never fires", {
  skip_on_cran()
  testInit()

  ## a false condition leaves the entry armed, re-checked after every event
  sim <- simInit(times = list(start = 0, end = 3, timeunit = "year"))
  sim$fired <- 0L
  defineEvent(sim, "condEv", moduleName = "cm", code = { sim$fired <- sim$fired + 1L })
  defineEvent(sim, "filler", moduleName = "cm", code = {
    sim <- scheduleEvent(sim, time(sim) + 1, "cm", "filler")
  })
  sim <- scheduleEvent(sim, 0, "cm", "filler")
  sim <- scheduleConditionalEvent(sim, condition = quote(isTRUE(sim$neverSet)),
                                  moduleName = "cm", eventType = "condEv",
                                  minEventTime = 0, maxEventTime = 3)

  out <- suppressMessages(spades(sim))

  expect_identical(out$fired, 0L)
  ## still armed, waiting for a condition that never came true
  expect_true(exists("._conditionalEvents", envir = out, inherits = FALSE))
  expect_length(out$._conditionalEvents, 1L)
})
