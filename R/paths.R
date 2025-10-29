corePaths <- c("modulePath", "cachePath", "inputPath", "outputPath")
tmpPaths <- c("rasterPath", "scratchPath", "terraPath")
spPaths <- c(corePaths, tmpPaths)

#' Get and set default working directories
#'
#' Wrapper functions to access the packages options for default working directories.
#' Note: there is an active binding made to `Paths`, so a user can use
#' `Paths$cachePath` for example instead of `getPaths()$cachePath`
#'
#' @param cachePath   The default local directory in which to cache simulation outputs.
#'                    If not specified, defaults to `getOption("reproducible.cachePath")`.
#'
#' @param inputPath   The default local directory in which to look for simulation inputs
#'                    If not specified, defaults to `getOption("spades.inputPath")`.
#'
#' @param modulePath  The default local directory where modules and data will be
#'                    downloaded and stored.
#'                    If not specified, defaults to `getOption("spades.modulePath")`.
#'
#' @param outputPath  The default local directory in which to save simulation outputs.
#'                    If not specified, defaults to `getOption("spades.outputPath")`.
#'
#' @param rasterPath  The default local directory in which to save transient raster files.
#'                    If not specified, defaults to
#'                    `file.path(getOption("spades.scratchPath"), "raster")`.
#'                    *Important note:* this location may not be cleaned up automatically,
#'                    so be sure to monitor this directory and remove unnecessary temp files
#'                    that may contribute to excessive disk usage.
#'                     *This option will be deprecated in a future release.*
#'
#' @param scratchPath The default local directory in which to save transient files.
#'                    If not specified, defaults to `getOption("spades.scratchPath")`.
#'                    *Important note:* this location may not be cleaned up automatically,
#'                    so be sure to monitor this directory and remove unnecessary temp files
#'                    that may contribute to excessive disk usage.
#'
#' @param terraPath  The default local directory in which to save transient `terra` files.
#'                   If not specified, defaults to
#'                   `file.path(getOption("spades.scratchPath"), "terra")`.
#'                   *Important note:* this location may not be cleaned up automatically,
#'                   so be sure to monitor this directory and remove unnecessary temp files
#'                   that may contribute to excessive disk usage.
#'
#' @return `getPaths` returns a named list of the user's default working directories.
#' `setPaths` is invoked for the side effect of setting these directories.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @name setPaths
#' @rdname setPaths
#' @export
#'
#' @examples
#' \donttest{
#' getPaths() ## returns the current default working paths
#'
#' ## set individual custom paths
#' setPaths(cachePath = file.path(tempdir(), "cache"))
#' setPaths(inputPath = file.path(tempdir(), "inputs"))
#' setPaths(modulePath = file.path(tempdir(), "modules"))
#' setPaths(outputPath = file.path(tempdir(), "outputs"))
#' setPaths(scratchPath = file.path(tempdir(), "scratch"))
#'
#' # NOTE: on loading and attaching SpaDES.core,
#' # an active binding is made to "Paths"
#'
#' getPaths()
#' Paths ## same as getPaths() above
#' setPaths(outputPath = tempdir())
#' Paths # shows change
#' }
#'
.paths <- function() {
  if (!is.null(.getOption("spades.cachePath"))) {
    message(
      "option('spades.cachePath') is being deprecated. Please use ",
      "option('reproducible.cachePath').\n",
      "Setting option('reproducible.cachePath' = getOption('spades.cachePath'))"
    )
  }

  list(
    cachePath = .getOption("reproducible.cachePath"), # nolint
    inputPath = getOption("spades.inputPath"), # nolint
    modulePath = getOption("spades.modulePath"), # nolint
    outputPath = getOption("spades.outputPath"), # nolint
    rasterPath = file.path(getOption("spades.scratchPath"), "raster"), # nolint
    scratchPath = getOption("spades.scratchPath"), # nolint
    terraPath = file.path(getOption("spades.scratchPath"), "terra") # nolint
  )
}

#' @export
#' @rdname setPaths
getPaths <- function() {
  return(.paths())
}

#' @export
#' @rdname setPaths
Paths <- .paths()

#' @param silent Logical. Should the messaging occur.
#'
#' @export
#' @importFrom reproducible checkPath
#' @rdname setPaths
setPaths <- function(cachePath, inputPath, modulePath, outputPath, rasterPath, scratchPath,
                     terraPath, silent = FALSE) {
  defaults <- list(
    CP = FALSE,
    IP = FALSE,
    MP = FALSE,
    OP = FALSE,
    RP = FALSE,
    SP = FALSE,
    TP = FALSE
  )
  if (missing(cachePath)) {
    cachePath <- .getOption("reproducible.cachePath") # nolint
    defaults$CP <- TRUE
  }
  if (missing(inputPath)) {
    inputPath <- getOption("spades.inputPath") # nolint
    defaults$IP <- TRUE
  }
  if (missing(modulePath)) {
    modulePath <- getOption("spades.modulePath") # nolint
    defaults$MP <- TRUE
  }
  if (missing(outputPath)) {
    outputPath <- getOption("spades.outputPath") # nolint
    defaults$OP <- TRUE
  }
  if (missing(rasterPath)) {
    ## TODO: deprecate
    rasterPath <- file.path(getOption("spades.scratchPath"), "raster") # nolint
    defaults$RP <- TRUE
  }
  if (missing(scratchPath)) {
    scratchPath <- getOption("spades.scratchPath") # nolint
    defaults$SP <- TRUE
  }
  if (missing(terraPath)) {
    terraPath <- file.path(getOption("spades.scratchPath"), "terra") # nolint
    defaults$TP <- TRUE
  }

  allDefault <- all(unlist(defaults))

  originalPaths <- .paths()
  newPaths <- lapply(
    list(
      cachePath = cachePath,
      inputPath = inputPath,
      modulePath = modulePath,
      outputPath = outputPath,
      rasterPath = rasterPath,
      scratchPath = scratchPath,
      terraPath = terraPath
    ),
    checkPath,
    create = TRUE
  )
  newPaths <- as.list(normPath(newPaths))

  ## set the new paths via options
  options(
    rasterTmpDir = newPaths$rasterPath,
    reproducible.cachePath = cachePath,
    reproducible.inputPath = inputPath,
    spades.inputPath = inputPath,
    spades.modulePath = unlist(modulePath),
    spades.outputPath = outputPath,
    spades.scratchPath = scratchPath
  )

  if (requireNamespace("terra", quietly = TRUE)) {
    terra::terraOptions(tempdir = terraPath)
  }

  ## message the user
  modPaths <- if (length(modulePath) > 1) {
    paste0("c('", paste(normPath(modulePath), collapse = "', '"), "')")
  } else {
    normPath(modulePath)
  }

  if (!silent) {
    if (!allDefault) {
      message(
        "Setting:\n",
        "  options(\n",
        if (!defaults$CP) paste0("    reproducible.cachePath = '", normPath(cachePath), "'\n"),
        if (!defaults$IP) paste0("    spades.inputPath = '", normPath(inputPath), "'\n"),
        if (!defaults$OP) paste0("    spades.outputPath = '", normPath(outputPath), "'\n"),
        if (!defaults$MP) paste0("    spades.modulePath = '", modPaths, "'\n"),
        if (!defaults$SP) paste0("    spades.scratchPath = '", normPath(scratchPath), "'\n"),
        "  )"
      )
    }

    if (any(unlist(defaults))) {
      message(
        "Paths set to:\n",
        "  options(\n",
        "    rasterTmpDir = '", normPath(rasterPath), "'\n",
        "    reproducible.cachePath = '", normPath(cachePath), "'\n",
        "    spades.inputPath = '", normPath(inputPath), "'\n",
        "    spades.outputPath = '", normPath(outputPath), "'\n",
        "    spades.modulePath = '", modPaths, "'\n", # normPath'ed above
        "    spades.scratchPath = '", normPath(scratchPath), "'\n",
        "  )\n",
        "  terra::terraOptions(tempdir = '", normPath(terraPath), "'"
      )
    }
  }

  return(invisible(originalPaths))
}
