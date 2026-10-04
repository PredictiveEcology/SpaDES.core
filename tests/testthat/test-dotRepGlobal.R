test_that(".globals sets universal dot parameters in every module, others only where declared", {
  upd <- SpaDES.core:::updateParamsSlotFromGlobals
  expect_setequal(SpaDES.core:::.knownDotParams,
                  c(".plots", ".seed", ".showSimilar", ".useCache", ".useCacheArgs"))
  g <- list(.useCache = TRUE, .plots = "png", .rep = 3L, .plotInterval = 2)
  p <- list(.globals = g, modA = list(.rep = 1L, .plotInterval = 1), modB = list(other = 1))
  defs <- list(modA = c(".rep", ".plotInterval", "x"), modB = "other")
  out <- upd(p, modDefaultParams = defs, verbose = 0)
  ## universal: reaches the module that does not declare it
  expect_true(out$modB$.useCache)
  expect_identical(out$modB$.plots, "png")
  ## declared only: reaches the declaring module, not the other
  expect_identical(out$modA$.rep, 3L)
  expect_identical(out$modA$.plotInterval, 2)
  expect_null(out$modB$.rep)
  expect_null(out$modB$.plotInterval)
  ## a value the user gave for the module wins over .globals (simInit passes params as dontUseGlobals)
  p$modA$.rep <- 1L
  p$modB$.plots <- "screen"
  out <- upd(p, dontUseGlobals = p, modDefaultParams = defs, verbose = 0)
  expect_identical(out$modA$.rep, 1L)
  expect_identical(out$modB$.plots, "screen")
  expect_true(out$modB$.useCache)
})
