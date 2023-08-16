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
# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
# sets options("reproducible.ask" = FALSE) if ask = FALSE
#   optsAsk in this environment,
testInit <- function(libraries = character(), ask = FALSE, verbose,
                     debug = FALSE, tmpFileExt = "",
                     opts = NULL, needGoogleDriveAuth = FALSE, smcc = FALSE) {
  set.randomseed()

  pf <- parent.frame()

  if (isTRUE(needGoogleDriveAuth))
    libraries <- c(libraries, "googledrive")
  if (length(libraries)) {
    libraries <- unique(libraries)
    loadedAlready <- vapply(libraries, function(pkg)
      any(grepl(paste0("package:", pkg), search())), FUN.VALUE = logical(1))
    libraries <- libraries[!loadedAlready]

    if (length(libraries)) {
      pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
      if (!all(pkgsLoaded)) {
        lapply(libraries[!pkgsLoaded], skip_if_not_installed)
      }
      suppressWarnings(lapply(libraries, withr::local_package, .local_envir = pf))
    }
  }

  out <- list()
  withr::local_options(.new = list(
    reproducible.ask = ask,
    reproducible.verbose = FALSE,
    spades.debug = debug,
    spades.moduleCodeChecks = smcc,
    spades.recoveryMode = FALSE,
    spades.sessionInfo = FALSE,
    spades.useRequire = FALSE
  ), .local_envir = pf)

  if (!missing(verbose))
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  if (!is.null(opts))
    withr::local_options(opts, .local_envir = pf)
  tmpdir <- normPath(withr::local_tempdir(tmpdir = tempdir2(), .local_envir = pf))
  tmpCache <- normPath(withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf))
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart))
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    out$tmpfile <- normPath(withr::local_tempfile(tmpdir = tmpdir, fileext = tmpFileExt))
  }
  withr::local_dir(tmpdir, .local_envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  return(invisible(out))
}

sampleModReqdPkgs <- c("NLMR", # Only randomLandscapes
                       "terra", "SpaDES.tools", "RColorBrewer", # randomLandscapes & fireSpread
                       "sf", "CircStats") # caribouMovement

testCode <- '
      defineModule(sim, list(
      name = "test",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
      spatialExtent = terra::ext(rep(0, 4)),
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
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test2 = "0.0.1"),
      spatialExtent = terra::ext(rep(0, 4)),
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

runTestsWithTimings <- function(pkgPath = ".",
                                nameOfOuterList = "ff", envir = parent.frame(), authorizeGoogle = FALSE) {
  # if (isTRUE(authorizeGoogle))
  #   if (Sys.info()[["user"]] == "emcintir")
  #     googledrive::drive_auth(cache = "~/.secret", email = "predictiveecology@gmail.com")
  prepend <- file.path(pkgPath, "tests/testthat")
  testFiles <- dir(prepend, pattern = "^test-", full.names = TRUE)
  testFiles <- grep("large", testFiles, value = TRUE, invert = TRUE)
  if (!exists(nameOfOuterList, envir = envir)) assign(nameOfOuterList, list(), envir = envir)
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
      b <- difftime(end, start, units = "secs")
      print(format(b))
      data.table(elapsed = round(as.numeric(b), 2), skipOnCRAN = skipOnCran)
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
  gg[, elapsed := round(elapsed, 2)]
  data.table::setorderv(gg, c("skipOnCRAN", "elapsed"), order = c(1L, -1L))
  gg[]
}
