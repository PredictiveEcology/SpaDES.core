## `spades.reqdPkgsAttach = FALSE`: a module's reqdPkgs are visible to that module's code, as a
## package's @imports are, but are not attached to the search path (R/module-imports.R).

mkImportsMod <- function(mp, name, reqdPkgs = character(), initBody = "NULL", laterBody = "NULL") {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = %s,
  parameters = rbind(defineParameter("failInit", "logical", FALSE, NA, NA, ""),
                     defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(), outputObjects = bindrows()))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = {
      sim$initResult_%s <- { %s }
      if (isTRUE(P(sim)$failInit)) stop("failInit")
      sim <- scheduleEvent(sim, time(sim) + 1, "%s", "later")
    },
    later = { sim$laterResult_%s <- { %s } })
  return(invisible(sim))
}
', name, name, deparse1(as.list(reqdPkgs)), name, name, initBody, name, name, laterBody),
    file.path(d, paste0(name, ".R")))
  invisible(name)
}

runImportsMods <- function(mods, mp, params = list(), attach = FALSE, spadesIt = TRUE, ...) {
  withr::local_options(spades.reqdPkgsAttach = attach, .local_envir = parent.frame())
  sim <- simInit(modules = as.list(mods), times = list(start = 0, end = 1),
                 paths = list(modulePath = mp), params = params, ...)
  if (spadesIt) spades(sim, debug = FALSE) else sim
}

skipIfAttached <- function(pkg) {
  skip_if_not_installed(pkg)
  skip_if(paste0("package:", pkg) %in% search(), paste(pkg, "is already attached"))
}

test_that("reqdPkgsAttach = FALSE: a module sees its packages, other modules and the session do not", {
  skipIfAttached("tools")
  testInit()
  mkImportsMod(tmpdir, "modA", "tools", initBody = 'toTitleCase("hello world")',
               laterBody = 'toTitleCase("good bye")')
  mkImportsMod(tmpdir, "modB", initBody = 'exists("toTitleCase")')

  sim <- runImportsMods(c("modA", "modB"), tmpdir)
  expect_identical(sim$initResult_modA, "Hello World")
  expect_identical(sim$laterResult_modA, "Good Bye")
  expect_false(sim$initResult_modB)
  expect_false("package:tools" %in% search())
  expect_false(exists("toTitleCase"))

  ## the module function environment chain: module -> imports -> SpaDES.core namespace
  imports <- parent.env(sim$.mods$modA)
  expect_identical(attr(imports, "name"), "imports:modA")
  expect_identical(parent.env(imports), asNamespace("SpaDES.core"))
  expect_identical(parent.env(sim$.mods$modB), asNamespace("SpaDES.core"))
})

test_that("reqdPkgsAttach = FALSE: modules with the same packages share one imports environment", {
  skipIfAttached("tools")
  testInit()
  mkImportsMod(tmpdir, "modA", "tools")
  mkImportsMod(tmpdir, "modB", list("tools (>= 1.0)"))
  mkImportsMod(tmpdir, "modC", c("tools", "grid"))
  sim <- runImportsMods(c("modA", "modB", "modC"), tmpdir, spadesIt = FALSE)
  expect_identical(parent.env(sim$.mods$modA), parent.env(sim$.mods$modB))
  expect_false(identical(parent.env(sim$.mods$modA), parent.env(sim$.mods$modC)))
})

test_that("reqdPkgsAttach = FALSE: of two packages exporting a name, the one listed later wins", {
  skip_if_not_installed("terra")
  skip_if_not_installed("grid")
  testInit()
  ## the imports come before the search path, so this holds whether or not either is attached
  ## grid::depth and terra::depth
  expect_true("depth" %in% getNamespaceExports("grid") && "depth" %in% getNamespaceExports("terra"))
  body <- 'c(terra = identical(depth, terra::depth), grid = identical(depth, grid::depth))'
  mkImportsMod(tmpdir, "terraLast", c("grid", "terra"), initBody = body)
  mkImportsMod(tmpdir, "gridLast", c("terra", "grid"), initBody = body)
  sim <- runImportsMods(c("terraLast", "gridLast"), tmpdir)
  expect_identical(sim$initResult_terraLast, c(terra = TRUE, grid = FALSE))
  expect_identical(sim$initResult_gridLast, c(terra = FALSE, grid = TRUE))
})

test_that("reqdPkgsAttach = FALSE: the imports include the Depends of a package", {
  ## CircStats Depends on MASS and boot, which attaching it would attach too
  skip_if_not_installed("CircStats")
  testInit()
  expect_identical(SpaDES.core:::.importPkgOrder("CircStats"), c("MASS", "boot", "CircStats"))
  mkImportsMod(tmpdir, "modA", "CircStats", initBody = 'c(exists("mvrnorm"), exists("boot"), exists("shrimp"))')
  mkImportsMod(tmpdir, "modB", initBody = 'c(exists("mvrnorm"), exists("boot"), exists("shrimp"))')
  sim <- runImportsMods(c("modA", "modB"), tmpdir)
  expect_identical(sim$initResult_modA, c(TRUE, TRUE, TRUE))
  ## another test may have attached them
  if (!any(c("package:MASS", "package:boot") %in% search()))
    expect_identical(sim$initResult_modB, c(FALSE, FALSE, FALSE))
})

test_that("reqdPkgsAttach = FALSE: Copy, saveSimList/loadSimList and restartSpades keep the imports", {
  skipIfAttached("tools")
  testInit(opts = list(reproducible.useMemoise = FALSE))
  withr::local_options(reproducible.cachePath = tmpCache, spades.recoveryMode = TRUE,
                       spades.saveSimOnExit = TRUE)
  mkImportsMod(tmpdir, "modA", "tools", laterBody = 'toTitleCase("good bye")')
  sim <- runImportsMods("modA", tmpdir, spadesIt = FALSE)
  parentOf <- function(s) attr(parent.env(s$.mods$modA), "name")

  cp <- Copy(sim)
  expect_identical(parentOf(cp), "imports:modA")
  expect_identical(spades(cp, debug = FALSE)$laterResult_modA, "Good Bye")

  f <- file.path(tmpdir, "sim.qs2")
  saveSimList(sim, f, projectPath = tmpdir, files = FALSE)
  expect_false("package:tools" %in% search())
  loaded <- loadSimList(f, projectPath = tmpdir)
  expect_identical(parentOf(loaded), "imports:modA")
  expect_identical(parent.env(parent.env(loaded$.mods$modA)), asNamespace("SpaDES.core"))
  expect_identical(spades(loaded, debug = FALSE)$laterResult_modA, "Good Bye")

  ## restartSpades re-parses the module code into the existing module environment
  failing <- simInit(modules = "modA", times = list(start = 0, end = 1),
                     paths = list(modulePath = tmpdir), params = list(modA = list(failInit = TRUE)))
  expect_error(spades(failing, debug = FALSE), "failInit")
  saved <- savedSimEnv()$.sim
  saved@params$modA$failInit <- NULL
  resumed <- restartSpades(saved, debug = FALSE)
  expect_identical(parentOf(resumed), "imports:modA")
  expect_identical(resumed$laterResult_modA, "Good Bye")
  expect_false("package:tools" %in% search())
})

test_that("reqdPkgsAttach = FALSE: event cache keys are the same as with TRUE", {
  skipIfAttached("tools")
  testInit(opts = list(reproducible.useMemoise = FALSE))
  mkImportsMod(tmpdir, "modA", "tools", initBody = 'toTitleCase("hello world")')
  mkImportsMod(tmpdir, "modB", initBody = '1')
  params <- list(modA = list(.useCache = "init"), modB = list(.useCache = "init"))
  ids <- function(attach) {
    cp <- withr::local_tempdir(tmpdir = tmpdir)
    withr::local_options(reproducible.cachePath = cp)
    runImportsMods(c("modA", "modB"), tmpdir, params = params, attach = attach)
    sort(unique(reproducible::showCache(cp, verbose = -2)$cacheId))
  }
  idsFalse <- ids(FALSE)
  on.exit(if ("package:tools" %in% search()) detach("package:tools", character.only = TRUE))
  idsTrue <- ids(TRUE)
  expect_gt(length(idsFalse), 1)
  expect_identical(idsFalse, idsTrue)
})

test_that("reqdPkgsAttach = FALSE: data.table's := and i expressions work in module code", {
  skip_if_not_installed("data.table")
  testInit()
  mkImportsMod(tmpdir, "modA", "data.table",
               initBody = 'DT <- data.table(x = 1:3); DT[, newcol := 1]; DT[x > 1, y := x * 2]; list(names(DT), DT[x > 1]$x, DT$y)')
  sim <- runImportsMods("modA", tmpdir)
  expect_identical(sim$initResult_modA, list(c("x", "newcol", "y"), 2:3, c(NA, 4, 6)))
})

test_that("reqdPkgsAttach = TRUE (default) attaches reqdPkgs as before", {
  skipIfAttached("tools")
  testInit()
  expect_true(getOption("spades.reqdPkgsAttach", TRUE))
  on.exit(if ("package:tools" %in% search()) detach("package:tools", character.only = TRUE))
  mkImportsMod(tmpdir, "modA", "tools", initBody = 'toTitleCase("hello world")')
  mkImportsMod(tmpdir, "modB", initBody = 'exists("toTitleCase")')
  sim <- runImportsMods(c("modA", "modB"), tmpdir, attach = TRUE)
  expect_true("package:tools" %in% search())
  expect_true(sim$initResult_modB)
  expect_identical(parent.env(sim$.mods$modA), asNamespace("SpaDES.core"))
})
