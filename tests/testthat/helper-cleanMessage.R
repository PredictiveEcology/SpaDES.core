cleanMessage <- function(mm) {
  mm1 <- gsub(".{1}\\[.{1,2}m", "", mm)
  mm1 <- gsub("\\n", "", mm1)
  mm1 <- gsub(" *$", "", mm1)
  #mm1 <- gsub("\\(.*\\)", "", mm1)
  mm1 <- gsub("\\.", "", mm1)
  mm1 <- gsub("‘", "", mm1, ignore.case = TRUE) # Doesn't actually work non-interactively
  mm1 <- gsub("’", "", mm1, ignore.case = TRUE) # Doesn't actually work non-interactively
  mm1
}

testInit <- function(libraries, smcc = TRUE) {
  opts <- if (!smcc)
    options("spades.moduleCodeChecks" = smcc)
  else
    list()
  unlist(lapply(libraries, require, character.only = TRUE))
  require("testthat")
  tmpdir <- file.path(tempdir(), rndstr(1,6)) %>% checkPath(create = TRUE)
  origDir <- setwd(tmpdir)
  outList <- list(opts = opts, tmpdir = tmpdir, origDir = origDir, libs = libraries)
  list2env(outList, envir = parent.frame())
  return(outList)
}

testOnExit <- function(testInitOut) {
  if (length(testInitOut$opts))
    options("spades.moduleCodeChecks" = testInitOut$opts)
  setwd(testInitOut$origDir)
  unlink(testInitOut$tmpdir, recursive = TRUE)
  lapply(testInitOut$libs, function(lib) {
    detach(paste0("package:", lib), character.only = TRUE)}
    )

}
