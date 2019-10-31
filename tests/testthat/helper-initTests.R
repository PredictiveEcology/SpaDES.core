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
                     opts = list(reproducible.inputPaths = NULL), tmpFileExt = "",
                     needGoogle = FALSE) {
  opts1 <- if (smcc)
    list(spades.moduleCodeChecks = smcc)
  else
    list()

  if (length(opts)) {
    opts1 <- append( opts1, opts)
  }

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

  if (isTRUE(needGoogle)) {
    if (utils::packageVersion("googledrive") >= "1.0.0")
      googledrive::drive_deauth()
    else
      googledrive::drive_auth_config(active = TRUE)

    if (quickPlot::isRstudioServer()) {
      options(httr_oob_default = TRUE)
    }

    ## #119 changed use of .httr-oauth (i.e., no longer used)
    ## instead, uses ~/.R/gargle/gargle-oauth/long_random_token_name_with_email
    if (interactive()) {
      if (utils::packageVersion("googledrive") >= "1.0.0") {
        googledrive::drive_deauth()
      } else {
        if (file.exists("~/.httr-oauth")) {
          linkOrCopy("~/.httr-oauth", to = file.path(tmpdir, ".httr-oauth"))
        } else {
          googledrive::drive_auth()
          print("copying .httr-oauth to ~/.httr-oauth")
          file.copy(".httr-oauth", "~/.httr-oauth", overwrite = TRUE)
        }

        if (!file.exists("~/.httr-oauth"))
          message("Please put an .httr-oauth file in your ~ directory")
      }
    }
  }

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
    if (isTRUE(testInitOut$needGoogle)) {
      if (utils::packageVersion("googledrive") < "1.0.0")
        googledrive::drive_auth_config(active = FALSE)
    }
    lapply(testInitOut$libs, function(lib) {
      try(detach(paste0("package:", lib), character.only = TRUE), silent = TRUE)}
    )
  }
