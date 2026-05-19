## Tests for the v2 module code-checking engine.
##
## These tests run codeCheckModule() against fixture modules under
## tests/testthat/fixtures/codecheck/ and assert against the structured
## findings data.frame, NOT against captured message text.

fixtureDir <- function(name) {
  testthat::test_path("fixtures", "codecheck", name)
}

test_that("clean fixture produces no findings", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  f <- codeCheckModule(fixtureDir("clean"), print = FALSE)
  expect_s3_class(f, "data.frame")
  expect_equal(nrow(f), 0)
})

test_that("missingParam fixture flags declared-unused and used-undeclared params", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  f <- codeCheckModule(fixtureDir("missingParam"), print = FALSE)
  ids <- f$id

  ## gamma is declared but never used
  expect_true("param_declared_unused" %in% ids)
  expect_true("gamma" %in% f$name[f$id == "param_declared_unused"])

  ## delta and epsilon are used but not declared
  used <- f$name[f$id == "param_used_undeclared"]
  expect_true(all(c("delta", "epsilon") %in% used))

  ## the used-undeclared findings have line numbers
  bad <- f[f$id == "param_used_undeclared", , drop = FALSE]
  expect_true(all(!is.na(bad$line)))
  expect_true(all(bad$fn == "Init"))
})

test_that("unresolvedAccessor fixture flags sim[[expr]] and get(...)", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  f <- codeCheckModule(fixtureDir("unresolvedAccessor"), print = FALSE)
  expect_true("unresolved_accessor" %in% f$id)
  ## must be info severity, not warning
  expect_true(all(f$severity[f$id == "unresolved_accessor"] == "info"))
})

test_that("usedUndeclared fixture flags out_used_undeclared and in_used_undeclared", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  f <- codeCheckModule(fixtureDir("usedUndeclared"), print = FALSE)
  expect_true("out_used_undeclared" %in% f$id)
  expect_true("out2" %in% f$name[f$id == "out_used_undeclared"])
  expect_true("in_used_undeclared" %in% f$id)
  expect_true("in2" %in% f$name[f$id == "in_used_undeclared"])
  ## with line numbers
  loc <- f[f$id == "in_used_undeclared", , drop = FALSE]
  expect_true(all(!is.na(loc$line) & loc$line > 0))
})

test_that("collector recognizes all sim accessor forms", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  a <- sim$x1
  b <- sim[["x2"]]
  c <- get("x3", envir = envir(sim))
  d <- sim[[varname]]                  # unresolved
  sim$y1 <- 1
  sim[["y2"]] <- 2
  assign("y3", 3, envir = envir(sim))
  return(invisible(sim))
}
'
  uses <- .cc_collectModule(text = src, currentModule = "anon")
  ## resolved gets: x1, x2, x3
  resolvedGets <- uses$name[uses$kind == "sim_get" & uses$resolved]
  expect_setequal(resolvedGets, c("x1", "x2", "x3"))
  ## resolved assigns: y1, y2, y3
  resolvedAssigns <- uses$name[uses$kind == "sim_assign" & uses$resolved]
  expect_setequal(resolvedAssigns, c("y1", "y2", "y3"))
  ## one unresolved get for sim[[varname]]
  expect_equal(sum(uses$kind == "sim_get" & !uses$resolved), 1L)
})

test_that("collector recognizes all parameter accessor forms", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  a <- Par$alpha
  b <- P(sim)$beta
  c <- P(sim, module = "other")$gamma
  d <- params(sim)$thisMod$delta
  e <- params(sim)[["other"]][["epsilon"]]
  return(invisible(sim))
}
'
  uses <- .cc_collectModule(text = src, currentModule = "thisMod")
  pUses <- uses[uses$kind == "param", , drop = FALSE]
  expect_true(all(c("alpha", "beta", "gamma", "delta", "epsilon") %in% pUses$name))
  ## modules:
  modByName <- setNames(pUses$module, pUses$name)
  expect_equal(modByName[["alpha"]],   "thisMod")
  expect_equal(modByName[["beta"]],    "thisMod")
  expect_equal(modByName[["gamma"]],   "other")
  expect_equal(modByName[["delta"]],   "thisMod")
  expect_equal(modByName[["epsilon"]], "other")
})

test_that("LHS vs RHS distinction is correct", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
F <- function(sim) {
  x <- sim$readMe       # get
  sim$writeMe <- 1      # assign
  y <- sim$readMe + sim$readMe2    # get x2
}
'
  uses <- .cc_collectModule(text = src, currentModule = "x")
  reads  <- sort(uses$name[uses$kind == "sim_get"])
  writes <- sort(uses$name[uses$kind == "sim_assign"])
  expect_equal(reads,  c("readMe", "readMe", "readMe2"))
  expect_equal(writes, "writeMe")
})

test_that("must_assign_to_sim does not misfire on `sim <- scheduleEvent(...)`", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
F <- function(sim) {
  sim <- scheduleEvent(sim, 1, "m", "e")
  scheduleEvent(sim, 2, "m", "e")          # missing assign — should fire
  sim <- saveFiles(sim)
  return(invisible(sim))
}
'
  uses <- .cc_collectModule(text = src, currentModule = "x")
  bad <- uses[uses$kind == "assign_to_sim", , drop = FALSE]
  expect_equal(nrow(bad), 1L)
  expect_equal(bad$name, "scheduleEvent")
})
