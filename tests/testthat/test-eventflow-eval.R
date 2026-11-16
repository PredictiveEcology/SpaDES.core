test_that(".ef_safeEval never runs functions outside the allowlist", {
  testInit(efPkgs)
  f <- tempfile(); writeLines("x", f)
  st <- SpaDES.core:::.ef_state(SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir))))
  expect_s3_class(SpaDES.core:::.ef_safeEval(bquote(file.remove(.(f))), st$sandbox), "ef_unknown")
  expect_true(file.exists(f))
  expect_true(SpaDES.core:::.ef_safeEval(quote(P(sim)$mode == "single"), st$sandbox))
  expect_s3_class(SpaDES.core:::.ef_safeEval(quote(sim$burnMap > 1), st$sandbox), "ef_unknown")
})

test_that(".ef_safeEval cannot reach a function outside the allowlist by any route", {
  testInit(efPkgs)
  f <- tempfile(); writeLines("x", f)
  st <- SpaDES.core:::.ef_state(SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir))))
  bad <- list(
    bquote({ g <- file.remove; g(.(f)) }),
    bquote({ c <- file.remove; c(.(f)) }),
    bquote({ P <- file.remove; P(.(f)) }),
    bquote(list(g = file.remove)$g(.(f))),
    bquote(.BaseNamespaceEnv$file.remove(.(f))),
    bquote(.GlobalEnv$file.remove(.(f))),
    bquote(base::file.remove(.(f))),
    bquote((file.remove)(.(f))),
    quote(end(sim) <- 5)
  )
  for (e in bad) {
    expect_s3_class(SpaDES.core:::.ef_safeEval(e, new.env(parent = st$sandbox)), "ef_unknown")
    expect_true(file.exists(f))
  }
})

test_that("a parameter default cannot rebind the sandbox", {
  testInit(efPkgs)
  modDir <- efModule(tmpdir, "rebMod",
    params = "defineParameter(\"a\", \"numeric\", {P <- 1; 2}, NA, NA, \"x\")",
    code = "doEvent.rebMod <- function(sim, eventTime, eventType, priority) NULL")
  st <- SpaDES.core:::.ef_state(SpaDES.core:::.ef_readModule("rebMod", dirname(modDir)))
  expect_true(is.function(st$sandbox$P))
})

test_that("params(sim)<- rewrites are tracked", {
  testInit(efPkgs)
  st <- SpaDES.core:::.ef_state(SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir))))
  e <- new.env(parent = st$sandbox)
  SpaDES.core:::.ef_safeEval(quote(params(sim)$fireMod$fireTimestep <- 5L), e)
  expect_identical(st$pars$fireTimestep, 5L)
  expect_true("fireTimestep" %in% st$rewritten)
})

test_that(".ef_classifyTime classifies recurring, start-relative, end and NA times", {
  testInit(efPkgs)
  m <- SpaDES.core:::.ef_readModule("fireMod", dirname(efFixtureFire(tmpdir)))
  st <- SpaDES.core:::.ef_stateFor(m)  # list(env, store, mod, prov) at init
  tt <- SpaDES.core:::.ef_classifyTime(quote(time(sim) + P(sim)$fireTimestep), st)
  expect_identical(tt[c("type", "value", "label")], list(type = "after", value = 1, label = "fireTimestep"))
  tt <- SpaDES.core:::.ef_classifyTime(quote(P(sim)$burnInitialTime), st)
  expect_identical(tt[c("type", "value", "params")], list(type = "start", value = 1, params = "burnInitialTime"))
  expect_identical(SpaDES.core:::.ef_classifyTime(quote(end(sim)), st)$type, "end")
  expect_true(is.na(SpaDES.core:::.ef_classifyTime(quote(P(sim)$.saveInitialTime), st)$value))
})

test_that(".ef_condText reads like a setting, and names the parameters behind locals", {
  testInit(efPkgs)
  m <- SpaDES.core:::.ef_readModule("vegMod", dirname(efFixtureVeg(tmpdir)))
  st <- SpaDES.core:::.ef_stateFor(m)
  expect_identical(SpaDES.core:::.ef_condText(quote(!is.na(P(sim)$.saveInitialTime)), st), ".saveInitialTime is set")
  assign("summPriority", "calcSummary", envir = st$prov)
  expect_identical(SpaDES.core:::.ef_condText(quote(!is.null(summPriority$start)), st), "depends on calcSummary")
})
