test_that(".ef_readModule finds if-chain branches, wrapped functions and metadata", {
  testInit(efPkgs)
  m <- SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir)))
  expect_identical(names(m$branches), c("init", "Burn", "summarySingle", "summaryMulti", "save"))
  expect_true(all(c("Init", "Burn", ".inputObjects") %in% names(m$fns)))
  expect_identical(m$meta$inputs, c("flammableMap", "fireReturnInterval"))
  expect_identical(m$meta$timeunit, "year")
  expect_identical(deparse(m$meta$params$burnInitialTime$expr), 'start(sim, "year") + 1')
})

test_that(".ef_readModule handles switch fall-through and pre-dispatch statements", {
  testInit(efPkgs)
  m <- SpaDES.core:::.ef_readModule("vegMod", dirname(efFixtureVeg(tmpdir)))
  expect_identical(names(m$branches), c("init", "Dispersal", "Growth", "Grow2", "summaryStart", "orphan"))
  expect_identical(m$branches$Growth, m$branches$Grow2)
  expect_length(m$pre, 2)
  expect_false(anyNA(attr(m$branches$init, "lines")))
  ## a multi-line block spans its first to its last line
  m2 <- SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir)))
  ln <- attr(m2$branches$init, "lines")
  expect_gt(diff(ln), 0)
  sched <- grep("scheduleEvent", readLines(m2$files[1]))
  expect_true(all(sched[sched <= ln[2]] >= ln[1]))
  expect_gte(ln[2], max(sched[1:4]))
})

test_that(".ef_readModule reads per-event doEvent functions", {
  testInit(efPkgs)
  m <- SpaDES.core:::.ef_readModule("evMod", dirname(efFixtureEvents(tmpdir)))
  expect_identical(names(m$branches), c("init", "step"))
})

test_that(".ef_readModule signals a classed error for unparseable code", {
  testInit(efPkgs)
  d <- efModule(tmpdir, "badMod", code = "doEvent.badMod <- function(sim, eventTime, eventType) {")
  expect_error(SpaDES.core:::.ef_readModule("badMod", tmpdir), class = "moduleEventFlowError")
  expect_error(SpaDES.core:::.ef_readModule("noSuchMod", tmpdir), class = "moduleEventFlowError")
})

test_that(".ef_readModule skips object names that are not string constants", {
  testInit(efPkgs)
  efModule(tmpdir, "codeIn", code = "", inputs = paste(
    "expectsInput(noSuchFn(\"x\"), \"numeric\", \"d\"),",
    "expectsInput(\"y\", \"numeric\", \"d\")"
  ))
  efModule(tmpdir, "codeOut", outputs = 'createsOutput(noSuchFn("x"), "numeric", "d")', code = "")
  expect_identical(SpaDES.core:::.ef_readModule("codeIn", tmpdir)$meta$inputs, "y")
  expect_identical(SpaDES.core:::.ef_readModule("codeOut", tmpdir)$meta$outputs, character(0))
})

test_that(".ef_readModule runs no code to read parameter names or the time unit", {
  testInit(efPkgs)
  f <- file.path(tmpdir, "keep.txt")
  file.create(f)
  code <- paste0("{file.remove(", deparse(f), "); \"a\"}")
  efModule(tmpdir, "codeMod", timeunit = code, code = "",
           params = paste0("defineParameter(", code, ", \"numeric\", 1, NA, NA, \"d\")"),
           inputs = "expectsInput(\"x\", \"numeric\", \"d\")")
  m <- SpaDES.core:::.ef_readModule("codeMod", tmpdir)
  expect_true(file.exists(f))
  expect_identical(m$meta$timeunit, NA_character_)
  expect_length(m$meta$params, 0)
  expect_identical(m$meta$inputs, "x")
})

test_that(".ef_readModule never sources the module to read its object tables", {
  testInit(efPkgs)
  marker <- file.path(tmpdir, "ran.txt")
  ## evaluating outputObjects fails on `outClass`, and "P(sim)" in a desc once made
  ## .parseModulePartial() fall back to simInit(), which sources the module
  efModule(tmpdir, "srcMod",
           outputs = paste(
             "createsOutput(\"a\", outClass, \"from P(sim)$x\"),",
             "createsOutput(\"b\", \"numeric\", \"plain\")"
           ),
           code = c(paste0("writeLines(\"ran\", ", deparse(marker), ")"),
                    "outClass <- \"numeric\""))
  ## whether or not the read succeeds, the module's code must not have run
  m <- try(SpaDES.core:::.ef_readModule("srcMod", tmpdir), silent = TRUE)
  expect_false(file.exists(marker))
  expect_identical(m$meta$outputs, c("a", "b"))
})

test_that(".ef_readModule errors without a defineModule(), but reads a module with no events", {
  testInit(efPkgs)
  dir.create(file.path(tmpdir, "noDef"))
  writeLines("doEvent.noDef <- function(sim, eventTime, eventType) sim",
             file.path(tmpdir, "noDef", "noDef.R"))
  expect_error(SpaDES.core:::.ef_readModule("noDef", tmpdir), class = "moduleEventFlowError",
               regexp = "defineModule")
  efModule(tmpdir, "parentMod", code = "")
  m <- SpaDES.core:::.ef_readModule("parentMod", tmpdir)
  expect_length(m$branches, 0)
})

test_that(".ef_readModule drops the template's NA placeholder object names", {
  testInit(efPkgs)
  d <- efModule(
    tmpdir, "naMod",
    inputs = paste(
      "expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),",
      "expectsInput(\"x\", \"numeric\", \"an input\")"
    ),
    outputs = "createsOutput(objectName = NA, objectClass = NA, desc = NA)",
    code = c("doEvent.naMod <- function(sim, eventTime, eventType) {", "  sim", "}")
  )
  m <- SpaDES.core:::.ef_readModule("naMod", dirname(d))
  expect_identical(m$meta$inputs, "x")
  expect_identical(m$meta$outputs, character(0))
})
