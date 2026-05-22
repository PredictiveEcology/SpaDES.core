# Shared scenario runner for the simInit() `.inputObjects` golden / characterization test
# (test-inputObjects-golden.R). The fixture in fixtures/inputObjects-golden.rds is generated
# by running THIS function on the pre-rework base commit; the test re-runs it on the current
# code and asserts an identical normalized snapshot. Because the same function is used to make
# the fixture and to check it, it must rely only on the public API (present on both versions).
#
# To (re)generate the fixture (from the repo root):
#   src <- tempfile(fileext = ".R")
#   ## use THIS version of the helper even when checked out on the base commit:
#   writeLines(system("git show <branch>:tests/testthat/helper-inputObjectsGolden.R", intern = TRUE), src)
#   git checkout <base-commit>
#   R: pkgload::load_all("."); source(src)
#      d <- tempfile(); dir.create(d)
#      saveRDS(ioGoldenSnapshot(d), "tests/testthat/fixtures/inputObjects-golden.rds")
#   git checkout <branch>

## write a minimal module (kept deliberately independent of any test-only helper)
.ioGoldenMkMod <- function(tmpdir, name, dotIO = "sim", initBody = "sim",
                           inObj = character(), outObj = character(),
                           params = 'rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, ""))') {
  dir.create(file.path(tmpdir, name), recursive = TRUE, showWarnings = FALSE)
  inRows  <- if (length(inObj))  paste0('expectsInput("', inObj,  '", "ANY", "")', collapse = ", ") else ""
  outRows <- if (length(outObj)) paste0('createsOutput("', outObj, '", "ANY", "")', collapse = ", ") else ""
  code <- sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = %s,
  inputObjects = bindrows(%s), outputObjects = bindrows(%s)))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") { %s }
  sim
}
.inputObjects <- function(sim) { %s; sim }
', name, name, params, inRows, outRows, name, initBody, dotIO)
  cat(code, file = file.path(tmpdir, name, paste0(name, ".R")), fill = TRUE)
}

## normalized, deterministic view of a (post-simInit) simList -- only the observable surface
## the `.inputObjects` rework can affect. Excludes nondeterministic bits (clock times, paths)
## and the `.inputObjects` completed-event priority (intentionally changed by the rework).
.ioGoldenNormalize <- function(sim) {
  objNames <- sort(ls(sim))  # ls() excludes dot-prefixed (internal) objects
  ev <- events(sim)
  cc <- completed(sim)
  list(
    objects = mget(objNames, envir = sim@.xData),
    events = data.frame(
      moduleName = ev$moduleName, eventType = ev$eventType,
      eventTime = round(as.numeric(ev$eventTime), 6), eventPriority = ev$eventPriority,
      stringsAsFactors = FALSE
    )[order(ev$eventTime, ev$eventPriority, ev$moduleName, ev$eventType), ],
    ## compare module + type only (the rework changed the `.inputObjects` row priority)
    completedIO = sort(if (NROW(cc)) cc[cc$eventType == ".inputObjects", ]$moduleName else character()),
    userSupplied = sort(sim$.userSuppliedObjNames %||% character()),
    modules = sort(basename2(unlist(modules(sim))))
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Run the `.inputObjects` golden scenarios and return their normalized snapshots
#'
#' @param tmpdir A writable directory for the modules/paths.
#' @return A named list of normalized snapshots, one per scenario.
ioGoldenSnapshot <- function(tmpdir) {
  oldOpts <- options(reproducible.useMemoise = FALSE, spades.recoveryMode = 1,
                     spades.moduleCodeChecks = FALSE, spades.dotInputObjects = TRUE,
                     spades.allowInitDuringSimInit = FALSE, spades.loadReqdPkgs = FALSE,
                     spades.useRequire = FALSE)
  on.exit(options(oldOpts), add = TRUE)
  out <- list()

  ## scenario 1 -- single module: .inputObjects creates objects (one default, one from RNG)
  d1 <- file.path(tmpdir, "s1"); dir.create(d1, showWarnings = FALSE)
  .ioGoldenMkMod(d1, "m1", inObj = c("a", "b"),
                 dotIO = 'sim$a <- 10L; set.seed(1); sim$b <- runif(2)')
  set.seed(42)
  s1 <- simInit(modules = "m1", paths = list(modulePath = d1), times = list(start = 0, end = 2))
  out$single <- .ioGoldenNormalize(s1)

  ## scenario 2 -- multi-module, explicit (non-alphabetical) loadOrder; later module reads
  ##   an object created by the earlier one's .inputObjects
  d2 <- file.path(tmpdir, "s2"); dir.create(d2, showWarnings = FALSE)
  .ioGoldenMkMod(d2, "first", inObj = "seed", outObj = "shared", dotIO = 'sim$shared <- 7L')
  .ioGoldenMkMod(d2, "second", inObj = c("shared", "extra"),
                 dotIO = 'sim$derived <- (if (is.null(sim$shared)) 0L else sim$shared) + 1L')
  set.seed(42)
  s2 <- simInit(modules = c("first", "second"), paths = list(modulePath = d2),
                loadOrder = c("first", "second"), times = list(start = 0, end = 1))
  out$multi <- .ioGoldenNormalize(s2)

  ## scenario 3 -- user-supplied objects + objectSynonyms visible to .inputObjects
  d3 <- file.path(tmpdir, "s3"); dir.create(d3, showWarnings = FALSE)
  .ioGoldenMkMod(d3, "syn", inObj = c("age", "ageMap"),
                 dotIO = 'sim$sawSyn <- suppliedElsewhere("ageMap", sim)')
  set.seed(42)
  s3 <- simInit(modules = "syn", paths = list(modulePath = d3),
                objects = list(age = 3L, objectSynonyms = list(c("age", "ageMap"))),
                times = list(start = 0, end = 1))
  out$synonyms <- .ioGoldenNormalize(s3)

  out
}
