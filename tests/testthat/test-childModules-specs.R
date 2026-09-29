## A parent's `childModules` may list GitHub specs -- "owner/repo@branch",
## "owner/repo", "name@branch" -- as well as plain names. Everywhere SpaDES.core
## uses a child it needs the module NAME (the repository name), so a spec must
## resolve to the local folder <modulePath>/<name>.

childSpecs <- c("Owner/kidA@development", "kidB@some/branch", "Owner/kidC", "kidD")
childNames <- c("kidA", "kidB", "kidC", "kidD")

## children as local modules, plus a parent listing them as specs
makeSpecParent <- function(path, parent = "fam", specs = childSpecs) {
  for (m in childNames)
    suppressMessages(newModule(m, path, open = FALSE, unitTests = FALSE))
  suppressMessages(newModule(parent, path, open = FALSE, unitTests = FALSE,
                             children = specs, type = "parent"))
  path
}

test_that("a child spec resolves to the module name", {
  expect_identical(SpaDES.core:::.childModuleName(childSpecs), childNames)
  expect_identical(SpaDES.core:::.childModuleName(character(0)), character(0))
})

test_that("simInit expands a parent whose children are specs, by name", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "specParent"))

  expect_no_error(
    mySim <- suppressMessages(simInit(paths = list(modulePath = mp),
                                      modules = list("fam"),
                                      times = list(start = 0, end = 1)))
  )
  expect_setequal(unlist(modules(mySim)), childNames)
  expect_true(all(dirname(names(modules(mySim))) %in% mp))
})

test_that("defineModule validates spec children by name and stores the names", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "specDefine"))
  sim <- suppressMessages(simInit(paths = list(modulePath = mp)))
  md <- list(name = "fam", childModules = childSpecs, version = list(fam = "1.0.0"))

  expect_no_error(sim2 <- suppressMessages(suppressWarnings(defineModule(sim, md))))
  deps <- sim2@depends@dependencies
  expect_setequal(deps[[length(deps)]]@childModules, childNames)

  unlink(file.path(mp, "kidA"), recursive = TRUE)
  expect_error(suppressMessages(suppressWarnings(defineModule(sim, md))),
               "Module kidA\\(a child module of fam")
})

test_that("a spec child that is itself a parent expands to its own children", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "specGrand"))
  suppressMessages(newModule("top", mp, open = FALSE, unitTests = FALSE,
                             children = "Owner/fam@development", type = "parent"))

  mySim <- suppressMessages(simInit(paths = list(modulePath = mp), modules = list("top"),
                                    times = list(start = 0, end = 1)))
  expect_setequal(unlist(modules(mySim)), childNames)
  expect_setequal(SpaDES.core:::.leafModules("top", mp), childNames)
})

test_that("newModule keys a parent's children versions by module name", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "specVersion"))

  v <- SpaDES.core:::.parseModulePartial(filename = file.path(mp, "fam", "fam.R"),
                                         defineModuleElement = "version")
  expect_setequal(names(v), c("fam", childNames))
  ## moduleMetadata reports the specs as written in the file
  expect_setequal(unlist(moduleMetadata(module = "fam", path = mp)$childModules), childSpecs)
  expect_setequal(names(moduleParams("fam", mp)), childNames)
})

test_that("downloadModule finds spec children locally by name and version", {
  testInit("httr")
  mp <- makeSpecParent(file.path(tmpdir, "specDownload"))
  v <- SpaDES.core:::.parseModulePartial(filename = file.path(mp, "fam", "fam.R"),
                                         defineModuleElement = "version")

  ## everything is local at the right version, so nothing is fetched
  expect_no_error(
    f <- suppressMessages(downloadModule("fam", path = mp, version = as.character(v[["fam"]]),
                                         data = FALSE, quiet = TRUE))
  )
  ## no folder named after a spec was created
  expect_setequal(dir(mp), c("fam", childNames))
})

test_that("a parent with plain child names is unchanged", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "plainParent"), specs = childNames)

  mySim <- suppressMessages(simInit(paths = list(modulePath = mp), modules = list("fam"),
                                    times = list(start = 0, end = 1)))
  expect_setequal(unlist(modules(mySim)), childNames)
  expect_setequal(unlist(moduleMetadata(module = "fam", path = mp)$childModules), childNames)
})

test_that("newModule writes a parseable parent for a long list of child specs", {
  testInit()
  mp <- file.path(tmpdir, "specLong")
  ## long enough that dput()/deparse() wrap over several lines
  specs <- paste0("PredictiveEcology/fireSense_child", 1:9, "@development")
  suppressMessages(newModule("fireSense", mp, open = FALSE, unitTests = FALSE,
                             children = specs, type = "parent"))
  f <- file.path(mp, "fireSense", "fireSense.R")

  expect_no_error(parse(f))
  expect_identical(
    unlist(SpaDES.core:::.parseModulePartial(filename = f, defineModuleElement = "childModules")),
    specs)
  v <- SpaDES.core:::.parseModulePartial(filename = f, defineModuleElement = "version")
  expect_setequal(names(v), c("fireSense", paste0("fireSense_child", 1:9)))
})

test_that("simInit(modules = parent) equals simInit(modules = its children)", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "famExpand"), specs = c("kidA", "kidB", "kidC@modsForX"))
  kids <- c("kidA", "kidB", "kidC")
  args <- list(paths = list(modulePath = mp), times = list(start = 0, end = 1))
  simP <- suppressMessages(do.call(simInit, c(args, list(modules = list("fam")))))
  simK <- suppressMessages(do.call(simInit, c(args, list(modules = as.list(kids)))))

  expect_setequal(unlist(modules(simP)), kids)
  expect_identical(unlist(modules(simP)), unlist(modules(simK)))
  expect_identical(names(simP@depends@dependencies), names(simK@depends@dependencies))
  expect_false("fam" %in% names(simP@depends@dependencies))
  expect_identical(lapply(params(simP)[kids], names), lapply(params(simK)[kids], names))
  expect_identical(events(simP), events(simK))
})

test_that("moduleMetadata of a parent with children on disk keeps the parent's own fields", {
  testInit()
  mp <- makeSpecParent(file.path(tmpdir, "famMeta"), specs = c("kidA", "kidB", "kidC@modsForX"))
  f <- file.path(mp, "fam", "fam.R")
  x <- readLines(f)
  x <- sub('^  description = .*', '  description = "the parent description",', x)
  x <- sub('fam = "0.0.0.9000"', 'fam = "7.8.9"', x, fixed = TRUE)
  writeLines(x, f)

  md <- moduleMetadata(module = "fam", path = mp)
  expect_identical(md$name, "fam")
  expect_identical(as.character(md$version[["fam"]]), "7.8.9")
  expect_identical(md$description, "the parent description")
  expect_true(length(md$authors) > 0)
  expect_setequal(names(md$parameters), c("kidA", "kidB", "kidC"))
})
