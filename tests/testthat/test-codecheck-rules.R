## The individual `.ccr_*` rules. Each takes a `uses` data.frame (built here
## with the .cc_use() constructor) plus a `meta` list, and returns a findings
## data.frame -- so they can be driven directly, without parsing a module.

ccUse <- SpaDES.core:::.cc_use
ccNoUses <- SpaDES.core:::.cc_emptyUses

## ---- must_return_sim ----------------------------------------------------

test_that("must_return_sim flags a doEvent function that never returns sim", {
  testInit()

  env <- new.env()
  env$doEvent.mod.init <- function(sim) sim
  env$helper <- function() 1

  f <- SpaDES.core:::.ccr_must_return_sim(ccNoUses(), list(module = "mod", moduleEnv = env))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "must_return_sim")
  expect_identical(f$severity, "error")
  expect_identical(f$name, "doEvent.mod.init")
  expect_match(f$message, "must end with")
  ## only doEvent.* functions are subject to the rule
  expect_false("helper" %in% f$name)
})

test_that("must_return_sim is quiet when the doEvent function does return sim", {
  testInit()

  env <- new.env()
  env$doEvent.mod.init <- function(sim) sim

  f <- SpaDES.core:::.ccr_must_return_sim(ccUse("return_sim", fn = "doEvent.mod.init"),
                                          list(module = "mod", moduleEnv = env))

  expect_identical(NROW(f), 0L)
})

test_that("must_return_sim is quiet when there is no module environment", {
  testInit()

  expect_identical(
    NROW(SpaDES.core:::.ccr_must_return_sim(ccNoUses(),
                                            list(module = "mod", moduleEnv = NULL))),
    0L)
})

test_that("must_return_sim is quiet when the module defines no doEvent function", {
  testInit()

  env <- new.env()
  env$helper <- function() 1

  expect_identical(
    NROW(SpaDES.core:::.ccr_must_return_sim(ccNoUses(),
                                            list(module = "mod", moduleEnv = env))),
    0L)
})

## ---- must_assign_to_sim -------------------------------------------------

test_that("must_assign_to_sim flags a bare call that should have been assigned", {
  testInit()

  u <- ccUse("assign_to_sim", name = "scheduleEvent", fn = "doEvent.mod.init", line = 10L)

  f <- SpaDES.core:::.ccr_must_assign_to_sim(u, list(module = "mod"))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "must_assign_to_sim")
  expect_identical(f$severity, "error")
  expect_match(f$message, "must be assigned to sim")
  expect_match(f$suggestion, "sim <- scheduleEvent", fixed = TRUE)
  expect_identical(f$line, 10L)
})

test_that("must_assign_to_sim is quiet when there is nothing to flag", {
  testInit()

  expect_identical(NROW(SpaDES.core:::.ccr_must_assign_to_sim(ccNoUses(),
                                                              list(module = "mod"))), 0L)
})

## ---- module_named_object ------------------------------------------------

test_that("module_named_object flags sim$<moduleName> <- ...", {
  testInit()

  u <- ccUse("sim_assign", name = "mod", fn = "doEvent.mod.init", line = 3L)

  f <- SpaDES.core:::.ccr_module_named_object(u, list(module = "mod"))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "module_named_object")
  expect_identical(f$severity, "error")
  expect_match(f$message, "collides with module name")
})

test_that("module_named_object ignores assignments with any other name", {
  testInit()

  u <- ccUse("sim_assign", name = "somethingElse", fn = "doEvent.mod.init")

  expect_identical(NROW(SpaDES.core:::.ccr_module_named_object(u, list(module = "mod"))), 0L)
})

## ---- clashing_fn --------------------------------------------------------

test_that("clashing_fn flags a module function named Plot", {
  testInit()

  env <- new.env()
  env$Plot <- function() 1

  f <- SpaDES.core:::.ccr_clashing_fn(ccNoUses(), list(module = "mod", moduleEnv = env))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "clashing_module_fn")
  expect_identical(f$severity, "warning")
  expect_match(f$message, "quickPlot::Plot")
})

test_that("clashing_fn is quiet for an ordinary module function", {
  testInit()

  env <- new.env()
  env$myHelper <- function() 1

  expect_identical(NROW(SpaDES.core:::.ccr_clashing_fn(ccNoUses(),
                                                       list(module = "mod", moduleEnv = env))), 0L)
})

test_that("clashing_fn is quiet when there is no module environment", {
  testInit()

  expect_identical(NROW(SpaDES.core:::.ccr_clashing_fn(ccNoUses(),
                                                       list(module = "mod", moduleEnv = NULL))), 0L)
})

## ---- param_used_other_module --------------------------------------------

test_that("param_used_other_module notes a parameter read from an unknown module", {
  testInit()

  u <- ccUse("param", name = "alpha", module = "otherMod", fn = "doEvent.mod.init", line = 5L)

  f <- SpaDES.core:::.ccr_param_used_other_module(
    u, list(module = "mod", otherModuleParams = list()))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "param_used_other_module")
  expect_identical(f$severity, "note")
  expect_match(f$message, "looked up in module 'otherMod'")
})

test_that("param_used_other_module is quiet when the sibling declares the parameter", {
  testInit()

  u <- ccUse("param", name = "alpha", module = "otherMod", fn = "doEvent.mod.init")

  f <- SpaDES.core:::.ccr_param_used_other_module(
    u, list(module = "mod", otherModuleParams = list(otherMod = "alpha")))

  expect_identical(NROW(f), 0L)
})

test_that("param_used_other_module ignores parameters of the current module", {
  testInit()

  u <- ccUse("param", name = "alpha", module = "mod", fn = "doEvent.mod.init")

  expect_identical(
    NROW(SpaDES.core:::.ccr_param_used_other_module(u, list(module = "mod",
                                                            otherModuleParams = list()))),
    0L)
})

## ---- codetools ----------------------------------------------------------

test_that("codetools reports what checkUsageEnv finds", {
  testInit()
  skip_if_not_installed("codetools")

  env <- new.env()
  env$bad <- function() { y <- 1; invisible(NULL) }

  f <- SpaDES.core:::.ccr_codetools(ccNoUses(), list(module = "mod", moduleEnv = env))

  expect_identical(NROW(f), 1L)
  expect_identical(f$id, "codetools")
  expect_identical(f$severity, "note")
  expect_match(f$message, "local variable")
})

test_that("codetools is quiet for clean code", {
  testInit()
  skip_if_not_installed("codetools")

  env <- new.env()
  env$fine <- function(x) x + 1

  expect_identical(NROW(SpaDES.core:::.ccr_codetools(ccNoUses(),
                                                     list(module = "mod", moduleEnv = env))), 0L)
})

test_that("codetools is quiet when there is no module environment", {
  testInit()

  expect_identical(NROW(SpaDES.core:::.ccr_codetools(ccNoUses(),
                                                     list(module = "mod", moduleEnv = NULL))), 0L)
})

test_that("codetools drops the noisy doEvent parameter complaints", {
  testInit()
  skip_if_not_installed("codetools")

  ## doEvent functions take eventTime/eventType/priority that they rarely use;
  ## those complaints are filtered out rather than reported every time
  env <- new.env()
  env$doEvent.mod.init <- function(sim, eventTime, eventType, priority) sim

  f <- SpaDES.core:::.ccr_codetools(ccNoUses(), list(module = "mod", moduleEnv = env))

  expect_false(any(grepl("doEvent.*: parameter", f$message)))
})
