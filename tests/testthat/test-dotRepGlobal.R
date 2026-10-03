test_that(".rep is a known dot parameter: .globals sets it in every module, declared or not", {
  expect_true(".rep" %in% SpaDES.core:::.knownDotParams)
  p <- list(.globals = list(.rep = 3L), modA = list(.rep = 1L), modB = list(other = 1))
  out <- SpaDES.core:::updateParamsSlotFromGlobals(
    p, modDefaultParams = list(modA = c(".rep", "x"), modB = "other"), verbose = 0)
  expect_identical(out$modA$.rep, 3L)  # declared
  expect_identical(out$modB$.rep, 3L)  # not declared: set as a known dot parameter
  ## a value the user gave for the module wins over .globals
  out <- SpaDES.core:::updateParamsSlotFromGlobals(
    p, dontUseGlobals = list(modA = list(.rep = 7L)),
    modDefaultParams = list(modA = c(".rep", "x"), modB = "other"), verbose = 0)
  expect_identical(out$modA$.rep, 1L)
})
