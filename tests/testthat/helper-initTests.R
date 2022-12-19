cleanMessage <- function(mm) {
  mm1 <- gsub(".{1}\\[.{1,2}m", "", mm)
  mm1 <- gsub("\\n", "", mm1)
  mm1 <- gsub(" *$", "", mm1)
  #mm1 <- gsub("\\(.*\\)", "", mm1)
  mm1 <- gsub("\\.inputObjects", "\\_inputObjects", mm1) ## TODO: workaround to omit .inputObjects matches
  mm1 <- gsub("\\.", "", mm1)
  mm1 <- gsub("\\_inputObjects", "\\.inputObjects", mm1) ## TODO: end workaround
  mm1 <- gsub("‘", "", mm1, ignore.case = TRUE) # Doesn't actually work non-interactively
  mm1 <- gsub("’", "", mm1, ignore.case = TRUE) # Doesn't actually work non-interactively
  mm1
}

# puts tmpdir, tmpCache, opts, optsDebug in this environment,
# loads and libraries indicated plus testthat,
# sets options("spades.moduleCodeChecks" = FALSE) if smcc is FALSE,
# sets options("spades.debug" = FALSE) if debug = FALSE
testInit <- function(libraries, smcc = FALSE, debug = FALSE, ask = FALSE, setPaths = TRUE,
                     opts = list(), tmpFileExt = "") {
  a <- list(reproducible.inputPaths = NULL,
            reproducible.showSimilar = FALSE,
            spades.moduleCodeChecks = smcc,
            spades.useRequire = FALSE,
            reproducible.useNewDigestAlgorithm = 2)
  a[names(opts)] <- opts
  opts1 <- a

  optsDebug <- if (!debug)
    list(spades.debug = debug)
  else
    list()

  if (length(optsDebug)) {
    opts1 <- append( opts1, optsDebug)
  }

  optsAsk <- if (!ask)
    list(reproducible.ask = ask)
  else
    list()
  if (length(optsAsk)) {
    opts1 <- append(opts1, optsAsk)
  }
  opts <- options(opts1)

  if (missing(libraries)) libraries <- list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- file.path(tempdir(), rndstr(1, 6))
  if (setPaths)
    setPaths(cachePath = tmpdir)


  checkPath(tmpdir, create = TRUE)
  origDir <- setwd(tmpdir)
  tmpCache <- checkPath(file.path(tmpdir, "testCache"), create = TRUE)
  try(clearCache(tmpdir, ask = FALSE), silent = TRUE)
  try(clearCache(tmpCache, ask = FALSE), silent = TRUE)

  if (!is.null(tmpFileExt)) {
    ranfiles <- unlist(lapply(tmpFileExt, function(x) paste0(rndstr(1, 7), ".", x)))
    tmpfile <- file.path(tmpdir, ranfiles)
    tmpfile <- gsub(pattern = "\\.\\.", tmpfile, replacement = "\\.")
    file.create(tmpfile)
    tmpfile <- normPath(tmpfile)
  }

  outList <- list(opts = opts, optsDebug = optsDebug, tmpdir = tmpdir,
                  origDir = origDir, libs = libraries,
                  tmpCache = tmpCache, optsAsk = optsAsk,
                  tmpfile = tmpfile)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$optsVerbose))
    options("reproducible.verbose" = testInitOut$optsVerbose[[1]])
  if (length(testInitOut$optsAsk))
    options("reproducible.ask" = testInitOut$optsAsk[[1]])
  if (length(testInitOut$opts))
    options(testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  lapply(testInitOut$libs, function(lib) {
    try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
  )
}

testCode <- '
      defineModule(sim, list(
      name = "test",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.md", "test.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
        defineParameter("testParA", "numeric", 1, NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("sdf", "sdf", "sdfd")
      ),
      outputObjects = bindrows(
        createsOutput("testPar1", "numeric", "")
      )
      ))

      doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      mod$a <- 2 # should have mod$x here
      sim$testPar1 <- Par$testParA

      if (tryCatch(exists("Init", envir = asNamespace("test"), inherits = FALSE), error = function(x) FALSE)) {
        sim <- Init(sim)
      }

      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }

      .inputObjects <- function(sim) {
        mod$x <- "sdf"
        return(sim)

      }
      '

test2Code <- '
      defineModule(sim, list(
      name = "test2",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test2 = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.md", "test2.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
        defineParameter("testParB", "numeric", 2, NA, NA, ""),
        defineParameter("testParC", "numeric", 22, NA, NA, ""),
        defineParameter("testParD", "numeric", 12, NA, NA, "")
      ),
      inputObjects = bindrows(
        expectsInput("sdf", "sdf", "sdfd")
      ),
      outputObjects = bindrows(
        createsOutput("testPar2", "numeric", "")
      )
      ))

      doEvent.test2 = function(sim, eventTime, eventType, debug = FALSE) {
      P(sim)$testParF <- 77
      P(sim)$testParA <- 42
      P(sim, "testParG") <- 79
      P(sim, "testParH") <- 48
      switch(
      eventType,
      init = {
      if (tryCatch(exists("Init", envir = asNamespace("test2"), inherits = FALSE), error = function(x) FALSE)) {
        sim <- Init(sim)
      }

      if (isTRUE(P(sim)$testParB >= 1100)) {
         P(sim, "testParB") <-  P(sim)$testParB + 756
      }

      if (any(grepl("testCommonPar", names(unlist(params(sim)))))) {
            errorText <- try(paramCheckOtherMods(sim, "testCommonPar"), silent = TRUE)
            if (identical("try-error", attr(errorText, "class")))
              message("There was an error")
            warn <- capture_warnings(paramCheckOtherMods(sim, "testCommonPar", ifSetButDifferent = "warning"))
            if (length(warn))
              message("There was a warning")
            paramCheckOtherMods(sim, "testCommonPar", ifSetButDifferent = "message")
            paramCheckOtherMods(sim, "testCommonPar", ifSetButDifferent = "silent")
      }
      if (isTRUE(!is.null(P(sim)$testRestartSpades))) {
        stop("testing restartSpades")#browser()
      }

      mod$a <- 1 # should have mod$y here
      sim$testPar2 <- Par$testParB
      sim <- scheduleEvent(sim, start(sim), "test2", "event1", .skipChecks = TRUE)
      },
      event1 = {
      if (isTRUE(P(sim)$testParB >= 1100)) {
         P(sim, "testParB") <-  P(sim)$testParB + 800
      }
      mod$b <- mod$a + 1 # shoudl have mod$a, mod$y by here
      mod$y <- paste0(mod$y, " is test2") # should have mod$a, mod$b, mod$y
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      .inputObjects <- function(sim) {
      if (isTRUE(P(sim)$testParB >= 543)) {
         P(sim, "testParB") <-  P(sim)$testParB + 654
      }

      if (isTRUE(P(sim)$testParB > 321321)) {
         P(sim, "checkpoint")
      }
      mod$y <- "This module"
        return(sim)
      }
      '


runTestsWithTimings <- function(nameOfOuterList = "ff", envir = parent.frame(), authorizeGoogle = FALSE) {
  if (isTRUE(authorizeGoogle))
    if (Sys.info()[["user"]] == "emcintir")
      googledrive::drive_auth(cache = "~/.secret", email = "predictiveecology@gmail.com")
  prepend <- "/home/emcintir/GitHub/reproducible/tests/testthat"
  testFiles <- dir(prepend, pattern = "^test-", full.names = TRUE)
  testFiles <- grep("large", testFiles, value = TRUE, invert = TRUE)
  rrrr <- get(nameOfOuterList, envir = envir)
  testFiles <- setdiff(testFiles, file.path(prepend, names(rrrr)))
  for (tf in testFiles) {
    messageDF(colour = "blue", basename(tf))
    a <- parse(tf, keep.source = TRUE)
    labels <- unlist(lapply(a, function(x) x[[2]]))
    # Sys.setenv("NOT_CRAN" = "false") # doesn't work
    dd <- Map(testLabel = labels, parsed = a, function(parsed, testLabel) {
      message(testLabel)
      skipOnCran <- any(grepl("skip_on_cran", parsed[[3]]))
      start <- Sys.time()
      try(eval(parsed))
      end <- Sys.time()
      b <- difftime(end, start)
      print(format(b))
      data.table(elapsed = as.numeric(b), skipOnCRAN = skipOnCran)
    })
    ee <- data.table::rbindlist(dd, idcol = "Label")
    ee <- setNames(list(ee), basename(tf))
    rrrr <- append(rrrr, ee)
    assign(nameOfOuterList, rrrr, envir = envir)

    testFiles <- testFiles[-1]
  }

  gg <- data.table::rbindlist(get(nameOfOuterList, envir = envir),
                              idcol = "TestFile")
  gg[, TestFile := basename(TestFile)]
  gg
}
