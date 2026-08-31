## Small pure helpers behind the module code checker.

ccCodetoolsOpts <- SpaDES.core:::.cc_codetoolsOpts
ccReqdPkgsFromDep <- SpaDES.core:::.cc_reqdPkgsFromDep
ccOtherModuleParams <- SpaDES.core:::.cc_otherModuleParams

test_that(".cc_codetoolsOpts returns the full defaults when checks are on", {
  testInit()

  withr::local_options(spades.moduleCodeChecks = TRUE)
  opts <- ccCodetoolsOpts()

  expect_type(opts, "list")
  expect_setequal(names(opts),
                  c("skipWith", "suppressNoLocalFun", "suppressParamUnused",
                    "suppressPartialMatchArgs", "suppressUndefined"))
  expect_true(opts$skipWith)
  expect_true(opts$suppressNoLocalFun)
  expect_false(opts$suppressParamUnused)
})

test_that(".cc_codetoolsOpts also returns the defaults when the option is unset", {
  testInit()

  withr::local_options(spades.moduleCodeChecks = NULL)
  expect_identical(ccCodetoolsOpts(),
                   local({
                     withr::local_options(spades.moduleCodeChecks = TRUE)
                     ccCodetoolsOpts()
                   }))
})

test_that(".cc_codetoolsOpts keeps only recognised names from a list option", {
  testInit()

  withr::local_options(spades.moduleCodeChecks =
                         list(skipWith = FALSE, notARealOption = 1))
  opts <- ccCodetoolsOpts()

  expect_identical(names(opts), "skipWith")
  expect_false(opts$skipWith)
})

test_that(".cc_codetoolsOpts returns nothing when checks are switched off", {
  testInit()

  withr::local_options(spades.moduleCodeChecks = FALSE)
  expect_identical(ccCodetoolsOpts(), list())
})

## a real .moduleDeps object -- constructing one by hand fails validity
sampleDep <- function(tmpdir, module = "randomLandscapes") {
  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list(module), paths = list(modulePath = mp))
  )
  sim@depends@dependencies[[module]]
}

test_that(".cc_reqdPkgsFromDep returns an empty frame when a module needs nothing", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  dep <- sampleDep(tmpdir)
  dep@reqdPkgs <- list()
  out <- ccReqdPkgsFromDep(dep)

  expect_s3_class(out, "data.frame")
  expect_identical(NROW(out), 0L)
  expect_setequal(names(out), c("spec", "pkg", "file", "line"))
})

test_that(".cc_reqdPkgsFromDep splits a package spec into spec and bare name", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  dep <- sampleDep(tmpdir)
  dep@reqdPkgs <- list("terra", "data.table (>= 1.14)")
  out <- ccReqdPkgsFromDep(dep, file = "m.R")

  expect_identical(NROW(out), 2L)
  expect_true("terra" %in% out$pkg)
  expect_true("data.table" %in% out$pkg)
  ## the full spec is kept alongside the bare name
  expect_true(any(grepl(">=", out$spec)))
  ## line numbers are unavailable on this path
  expect_true(all(is.na(out$line)))
  expect_true(all(out$file == "m.R"))
})

test_that(".cc_otherModuleParams lists parameters of every module but the current one", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes", "fireSpread"),
            paths = list(modulePath = mp))
  )

  out <- ccOtherModuleParams(sim, currentModule = "randomLandscapes")

  expect_type(out, "list")
  expect_false("randomLandscapes" %in% names(out))
  expect_true("fireSpread" %in% names(out))
  expect_true(is.character(out$fireSpread))
  expect_true(length(out$fireSpread) > 0)
})

test_that(".cc_otherModuleParams is empty when the sim has only the current module", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  sim <- suppressMessages(
    simInit(times = list(start = 0, end = 1, timeunit = "year"),
            modules = list("randomLandscapes"),
            paths = list(modulePath = mp))
  )

  expect_length(ccOtherModuleParams(sim, currentModule = "randomLandscapes"), 0L)
})

## ---- sim access: reads vs writes -----------------------------------------

## `.cc_collect_simAccess()` classifies each `sim$x` / `sim[["x"]]` node as a
## read (`sim_get`), a write (`sim_assign`), or -- for in-place updates -- both.
ccSimAccess <- function(text) {
  SpaDES.core:::.cc_collect_simAccess(SpaDES.core:::.cc_parseFile(text = text))
}

ccKinds <- function(text, name) {
  u <- ccSimAccess(text)
  sort(u$kind[!is.na(u$name) & u$name == name])
}

test_that("a bare assignment to sim is a write only", {
  testInit()

  expect_identical(ccKinds("sim$x <- 1", "x"), "sim_assign")
  expect_identical(ccKinds('sim[["x"]] <- 1', "x"), "sim_assign")
})

test_that("a plain read of sim is a read only", {
  testInit()

  expect_identical(ccKinds("y <- sim$x", "x"), "sim_get")
  expect_identical(ccKinds('y <- sim[["x"]]', "x"), "sim_get")
  ## reaching into an object does not write it, and does not record the column
  expect_identical(ccKinds("y <- sim$x$col", "x"), "sim_get")
  expect_false("col" %in% ccSimAccess("y <- sim$x$col")$name)
})

test_that("updating a sim object in place is both a read and a write", {
  testInit()

  ## these all call a replacement function, so `sim$x` is read then rewritten
  for (code in c("sim$x[] <- 1", "sim$x[i, j] <- 1", "sim$x$col <- 1",
                 "sim$x[[1]] <- 1", 'sim[["x"]][[1]] <- 1')) {
    expect_identical(ccKinds(code, "x"), c("sim_assign", "sim_get"), info = code)
  }
})

test_that("data.table modify-by-reference on a sim object is a write", {
  testInit()

  ## no `<-` at all, but `:=` rewrites sim$x in place
  expect_identical(ccKinds("sim$x[, col := 1]", "x"), c("sim_assign", "sim_get"))
  expect_identical(ccKinds("sim$x[cond, col := 1]", "x"), c("sim_assign", "sim_get"))
  expect_identical(ccKinds("sim$x[, `:=`(col = 1)]", "x"), c("sim_assign", "sim_get"))
})

test_that("subsetting a sim object without assigning is still only a read", {
  testInit()

  expect_identical(ccKinds("y <- sim$x[1]", "x"), "sim_get")
  expect_identical(ccKinds("y <- sim$x[, .N, by = col]", "x"), "sim_get")
  ## an unrelated object being assigned through does not make sim$x a write
  expect_identical(ccKinds("other[sim$x] <- 1", "x"), "sim_get")
})
