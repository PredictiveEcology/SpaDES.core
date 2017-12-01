## be sure to update the 'Package Options' section of the package help file
##   in R/spades-core-package.R
##
.onLoad <- function(libname, pkgname) {
  tmpdir <- file.path(tempdir(), "SpaDES")
  ## set options using the approach used by devtools
  opts <- options()
  opts.spades <- list( # nolint
    spades.cachePath = file.path(tmpdir, "cache"),
    spades.inputPath = file.path(tmpdir, "inputs"),
    spades.lowMemory = FALSE,
    spades.modulePath = file.path(tmpdir, "modules"),
    spades.moduleRepo = "PredictiveEcology/SpaDES-modules",
    spades.nCompleted = 10000L,
    spades.outputPath = file.path(tmpdir, "outputs"),
    spades.tolerance = .Machine$double.eps ^ 0.5,
    spades.useragent = "http://github.com/PredictiveEcology/SpaDES"
  )
  toset <- !(names(opts.spades) %in% names(opts))
  if (any(toset)) options(opts.spades[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Using SpaDES.core version ", packageVersion("SpaDES.core"), ".")
    packageStartupMessage("Default paths for SpaDES directories set to:\n",
                          "  cachePath:  ", getOption("spades.cachePath"), "\n",
                          "  inputPath:  ", getOption("spades.inputPath"), "\n",
                          "  modulePath: ", getOption("spades.modulePath"), "\n",
                          "  outputPath: ", getOption("spades.outputPath"), "\n",
                          "These can be changed using 'setPaths()'. See '?setPaths'.")
  }
}

.onUnload <- function(libpath) {
  ## if temp session dir is being used, ensure it gets reset each session
  tmpdir <- file.path(tempdir(), "SpaDES")
  if (getOption("spades.cachePath") == file.path(tmpdir, "cache")) {
    options(spades.cachePath = NULL)
  }
  if (getOption("spades.inputPath") == file.path(tmpdir, "inputs")) {
    options(spades.inputPath = NULL)
  }
  if (getOption("spades.modulePath") == file.path(tmpdir, "modules")) {
    options(spades.modulePath = NULL)
  }
  if (getOption("spades.outputPath") == file.path(tmpdir, "outputs")) {
    options(spades.outputPath = NULL)
  }
}

