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

test_that("enclosing fn is found through wrapper calls (e.g. cmpfun)", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
SummaryBGM <- compiler::cmpfun(function(sim) {
  sim$ANPPMap <- rasterizeReduced(x, sim$pixelGroupMap, "uniqueSumANPP")
  sim
})
anon <- function(sim) {
  lapply(1:2, function(z) sim$inner <- z)   # anonymous: not attributed
  sim
}
'
  uses <- .cc_collectModule(text = src, currentModule = "m")
  ## the assign wrapped in cmpfun() is attributed to its binding name
  anpp <- uses[uses$name == "ANPPMap" & uses$kind == "sim_assign", , drop = FALSE]
  expect_equal(anpp$fn, "SummaryBGM")
  ## an assign declared as an output is therefore seen as "used"
  meta <- list(module = "m", inputs = character(), outputs = "ANPPMap",
               params = character(), otherModuleParams = list(), moduleEnv = NULL)
  f <- .cc_runRules(uses, meta)
  expect_false("ANPPMap" %in% f$name[f$id == "out_declared_unused"])
  ## the anonymous lapply callback is not misattributed to the outer function
  inner <- uses[uses$name == "inner" & uses$kind == "sim_assign", , drop = FALSE]
  expect_true(is.na(inner$fn))
})

test_that("sim[[var]] in an anonymous lapply callback is not flagged unresolved", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  ## a dynamic accessor inside an anonymous callback (fn = NA) must not surface
  ## as an unresolved_accessor finding (which requires a known enclosing fn)
  src <- '
Init <- function(sim) {
  haveAllRasters <- all(!unlist(lapply(rasterNamesToCompare,
                                       function(rn) is.null(sim[[rn]]))))
  sim
}
'
  uses <- .cc_collectModule(text = src, currentModule = "m")
  meta <- list(module = "m", inputs = character(), outputs = character(),
               params = character(), otherModuleParams = list(), moduleEnv = NULL)
  f <- .cc_runRules(uses, meta)
  expect_false("unresolved_accessor" %in% f$id)
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

test_that("params(sim)[[currentModule(sim)]]$x resolves to the current module", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  params(sim)[[currentModule(sim)]]$pixelGroupAgeClass <- P(sim)$successionTimestep
  z <- params(sim)[[someVar]]$bar   # genuinely unresolved
  return(invisible(sim))
}
'
  uses <- .cc_collectModule(text = src, currentModule = "thisMod")
  pUses <- uses[uses$kind == "param", , drop = FALSE]
  ## the currentModule(sim) key resolves to the current module
  pgac <- pUses[pUses$name == "pixelGroupAgeClass", , drop = FALSE]
  expect_equal(nrow(pgac), 1L)
  expect_true(pgac$resolved)
  expect_equal(pgac$module, "thisMod")
  ## a non-literal, non-currentModule key is still unresolved
  expect_true(any(uses$kind == "param" & !uses$resolved))
})

test_that("inline # nolint suppresses findings (line and declaration span)", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
defineModule(sim, list(
  name = "nolintMod",
  inputObjects = bindrows(
    expectsInput("cloudFolderID", "character", desc = "x"), # nolint: in_no_default
    expectsInput("ecoregionRst", "RasterLayer",
                 desc = "multi-line"), # nolint
    expectsInput("needsIt", "character", desc = "still flagged")
  ),
  outputObjects = bindrows()
))
.inputObjects <- function(sim) sim
Init <- function(sim) {
  a <- scale(1)            # nolint: conflicting_fn_unqualified
  b <- levels(2)
  sim
}
'
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(src, tf)
  f <- codeCheckModule(tf, print = FALSE)
  inNoDef <- f$name[f$id == "in_no_default"]
  ## rule-specific nolint on the line, and blanket nolint within the
  ## (multi-line) declaration span, both silence in_no_default
  expect_false("cloudFolderID" %in% inNoDef)
  expect_false("ecoregionRst" %in% inNoDef)
  expect_true("needsIt" %in% inNoDef)
  ## rule-specific nolint silences only that rule on that line
  conf <- f$name[f$id == "conflicting_fn_unqualified"]
  expect_false("scale" %in% conf)
  expect_true("levels" %in% conf)
})

test_that("# nolint accepts a group name as well as a rule id", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  a <- scale(1)                            # nolint: globals
  b <- levels(2)                           # nolint: conflicting_fn_unqualified
  d <- scale(3)
  sim
}
'
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(src, tf)
  f <- codeCheckModule(tf, print = FALSE)
  conf <- f[f$id == "conflicting_fn_unqualified", , drop = FALSE]
  ## the group-name and rule-id markers each silence their own line; the
  ## un-marked scale(3) remains
  expect_equal(nrow(conf), 1L)
  expect_equal(conf$name, "scale")
})

test_that("options(spades.codeChecksIgnore) suppresses by rule + object name", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
defineModule(sim, list(
  name = "ignoreMod",
  inputObjects = bindrows(
    expectsInput("a", "character", desc = "x"),
    expectsInput("b", "character", desc = "y")
  ),
  outputObjects = bindrows()
))
.inputObjects <- function(sim) sim
'
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(src, tf)
  withr::local_options(spades.codeChecksIgnore = list(in_no_default = "a"))
  f <- codeCheckModule(tf, print = FALSE)
  inNoDef <- f$name[f$id == "in_no_default"]
  expect_false("a" %in% inNoDef)
  expect_true("b" %in% inNoDef)
})

test_that("options(spades.moduleCodeChecks=list(disable=)) disables a whole rule", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  a <- scale(1)
  sim
}
'
  uses <- .cc_collectModule(text = src, currentModule = "m")
  withr::local_options(spades.moduleCodeChecks = list(disable = "conflicting_fn_unqualified"))
  meta <- list(module = "m", inputs = character(), outputs = character(),
               params = character(), otherModuleParams = list(), moduleEnv = NULL)
  f <- .cc_runRules(uses, meta)
  expect_false("conflicting_fn_unqualified" %in% f$id)
})

test_that("suggestions end with a `# nolint: <rule_id>` acknowledgement", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  a <- scale(1)
  sim$undeclared <- 1
  sim
}
'
  uses <- .cc_collectModule(text = src, currentModule = "m")
  meta <- list(module = "m", inputs = character(), outputs = character(),
               params = character(), otherModuleParams = list(), moduleEnv = NULL)
  f <- .cc_runRules(uses, meta)
  withSug <- f[!is.na(f$suggestion), , drop = FALSE]
  expect_true(nrow(withSug) > 0)
  ## each suggestion references nolint with the finding's own rule id
  expect_true(all(mapply(function(s, id) grepl(paste0("# nolint: ", id), s, fixed = TRUE),
                         withSug$suggestion, withSug$id)))
  ## the old vague wording is gone
  expect_false(any(grepl("otherwise ignore", f$suggestion, fixed = TRUE)))
})

test_that("list2env(..., envir(sim)) bulk write counts local assigns as outputs", {
  skip_if_not_installed("xmlparsedata")
  skip_if_not_installed("xml2")
  src <- '
Init <- function(sim) {
  studyArea <- makeSA()
  rasterToMatch <- makeRTM()
  objsHere <- depends(sim)@dependencies[[currentModule(sim)]]@outputObjects$objectName
  list2env(mget(objsHere, envir = environment()), envir = envir(sim))
  sim
}
'
  uses <- .cc_collectModule(text = src, currentModule = "m")
  meta <- list(module = "m",
               inputs = character(),
               outputs = c("studyArea", "rasterToMatch", "neverProduced"),
               params = character(), otherModuleParams = list(), moduleEnv = NULL)
  f <- .cc_runRules(uses, meta)
  unused <- f$name[f$id == "out_declared_unused"]
  ## locally-assigned outputs are treated as produced via the bulk write
  expect_false("studyArea" %in% unused)
  expect_false("rasterToMatch" %in% unused)
  ## an output that is neither sim$-assigned nor a local assignment is still flagged
  expect_true("neverProduced" %in% unused)
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
