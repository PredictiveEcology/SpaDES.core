#' @importFrom reproducible normPath checkPath
#' @importFrom utils packageVersion
setupPaths <- function(name, paths, inProject, standAlone = TRUE, libPaths = NULL,
                       updateRprofile = getOption("Require.updateRprofile", FALSE),
                       overwrite = FALSE, envir = environment(),
                       verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {

  browser()
  messageVerbose(yellow("setting up paths ..."), verbose = verbose)

  if (missing(name))
    name <- basename(paths[["projectPath"]])

  if (is.null(paths[["modulePath"]])) paths[["modulePath"]] <- file.path(paths[["projectPath"]], "modules")

  absPathCurrent <- findAbsolutePartOfPaths(getPaths())
  absPathNew <- findAbsolutePartOfPaths(paths)

  if (nchar(absPathCurrent) > 0)
    relPathOrig <- makeRelative(normPath(unlist(getPaths())), absPathCurrent)
  # if (nchar(absPathNew) > 0)
  #   relPathNew <- makeRelative(normPath(paths), absPathNew)
  if (length(absPathNew) > 1)
    warning("The paths ")
  paths <- file.path(absPathNew, relPathOrig) |> setNames(names(relPathOrig))


  paths <- lapply(paths, checkPath, create = TRUE)

  if (is.null(paths$scratchPath)) {
    paths$scratchPath <- file.path(tempdir(), name)
  }
  if (!is.null(paths$scratchPath)) {
    paths <- Require::modifyList2(
      list(scratchPath = file.path(paths$scratchPath),
           rasterPath = file.path(paths$scratchPath, "raster"),
           terraPath = file.path(paths$scratchPath, "terra")
      ),
      paths)

  }

  paths <- Require::modifyList2(
    list(cachePath = file.path(paths[["projectPath"]], "cache"),
         inputPath = file.path(paths[["projectPath"]], "inputs"),
         outputPath = file.path(paths[["projectPath"]], "outputs")
    ),
    paths)

  paths <- lapply(paths, normPath)

  do.call(setPaths, paths[spPaths])

  paths[order(names(paths))]
}



corePaths <- c("modulePath", "cachePath", "inputPath", "outputPath")
tmpPaths <- c("rasterPath", "scratchPath", "terraPath")
spPaths <- c(corePaths, tmpPaths)
