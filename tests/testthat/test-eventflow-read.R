test_that(".ef_readModule finds if-chain branches, wrapped functions and metadata", {
  testInit()
  m <- SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir)))
  expect_identical(names(m$branches), c("init", "Burn", "summarySingle", "summaryMulti", "save"))
  expect_true(all(c("Init", "Burn", ".inputObjects") %in% names(m$fns)))
  expect_identical(m$meta$inputs, c("flammableMap", "fireReturnInterval"))
  expect_identical(m$meta$timeunit, "year")
  expect_identical(deparse(m$meta$params$burnInitialTime$expr), 'start(sim, "year") + 1')
})

test_that(".ef_readModule handles switch fall-through and pre-dispatch statements", {
  testInit()
  m <- SpaDES.core:::.ef_readModule("vegMod", dirname(efFixtureVeg(tmpdir)))
  expect_identical(names(m$branches), c("init", "Dispersal", "Growth", "Grow2", "summaryStart", "orphan"))
  expect_identical(m$branches$Growth, m$branches$Grow2)
  expect_length(m$pre, 2)
  expect_false(anyNA(attr(m$branches$init, "lines")))
})

test_that(".ef_readModule reads per-event doEvent functions", {
  testInit()
  m <- SpaDES.core:::.ef_readModule("evMod", dirname(efFixtureEvents(tmpdir)))
  expect_identical(names(m$branches), c("init", "step"))
})

test_that(".ef_readModule signals a classed error for unparseable code", {
  testInit()
  d <- efModule(tmpdir, "badMod", code = "doEvent.badMod <- function(sim, eventTime, eventType) {")
  expect_error(SpaDES.core:::.ef_readModule("badMod", tmpdir), class = "moduleEventFlowError")
  expect_error(SpaDES.core:::.ef_readModule("noSuchMod", tmpdir), class = "moduleEventFlowError")
})

test_that(".ef_readModule errors when inputObjects or outputObjects cannot be read", {
  testInit()
  efModule(tmpdir, "badIn", inputs = 'expectsInput(noSuchFn("x"), "numeric", "d")', code = "")
  efModule(tmpdir, "badOut", outputs = 'createsOutput(noSuchFn("x"), "numeric", "d")', code = "")
  expect_error(SpaDES.core:::.ef_readModule("badIn", tmpdir), class = "moduleEventFlowError",
               regexp = "inputObjects")
  expect_error(SpaDES.core:::.ef_readModule("badOut", tmpdir), class = "moduleEventFlowError",
               regexp = "outputObjects")
})

test_that(".ef_readModule errors without a defineModule(), but reads a module with no events", {
  testInit()
  dir.create(file.path(tmpdir, "noDef"))
  writeLines("doEvent.noDef <- function(sim, eventTime, eventType) sim",
             file.path(tmpdir, "noDef", "noDef.R"))
  expect_error(SpaDES.core:::.ef_readModule("noDef", tmpdir), class = "moduleEventFlowError",
               regexp = "defineModule")
  efModule(tmpdir, "parentMod", code = "")
  m <- SpaDES.core:::.ef_readModule("parentMod", tmpdir)
  expect_length(m$branches, 0)
})
