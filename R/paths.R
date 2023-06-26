#' @importFrom Require normPath checkPath
#' @importFrom utils packageVersion
if (!exists("setupPaths"))
  setupPaths <- function(name, paths, inProject, standAlone = TRUE, libPaths = NULL,
                         updateRprofile = getOption("Require.updateRprofile", FALSE),
                         overwrite = FALSE, envir = environment(),
                         verbose = getOption("Require.verbose", 1L), dots, defaultDots, ...) {

    dotsSUB <- as.list(substitute(list(...)))[-1]
    dotsSUB <- dotsToHere(dots, dotsSUB, defaultDots)

    messageVerbose(yellow("setting up paths ..."), verbose = verbose)

    pathsSUB <- substitute(paths) # must do this in case the user passes e.g., `list(modulePath = file.path(paths[["projectPath"]]))`
    pathsSUB <- checkProjectPath(pathsSUB, envir, parent.frame())

    paths <- evalSUB(val = pathsSUB, valObjName = "paths", envir = envir, envir2 = parent.frame())
    paths <- parseFileLists(paths, paths[["projectPath"]], overwrite = overwrite,
                            envir = envir, verbose = verbose)

    if (missing(name))
      name <- basename(paths[["projectPath"]])
    if (missing(inProject))
      inProject <- isInProject(name)
    if (is.null(paths[["projectPath"]]))
      stop("Please specify paths[[\"projectPath\"]] as an absolute path")

    if (!is.null(libPaths)) {
      warning("libPaths argument is deprecated. Pass to `paths = list(packagePath = ...)`",
              "; it is being ignored", verbose = verbose)
      libPaths <- NULL
    }
    #if (is.null(libPaths) || is.call(libPaths)) {
    if (is.null(paths[["packagePath"]])) {
      pkgPth <- tools::R_user_dir(package = basename(name), which = "data")
      paths[["packagePath"]] <- normalizePath(
        file.path(pkgPth, "packages", version$platform, substr(getRversion(), 1, 3)),
        mustWork = FALSE, winslash = "/")
    }

    if (is.null(paths[["modulePath"]])) paths[["modulePath"]] <- file.path(paths[["projectPath"]], "modules")
    isAbs <- unlist(lapply(paths, isAbsolutePath))
    toMakeAbsolute <- isAbs %in% FALSE & names(paths) != "projectPath"
    if (inProject) {
      paths[toMakeAbsolute] <- lapply(paths[toMakeAbsolute], function(x) file.path(x))
    } else {
      paths[toMakeAbsolute] <- lapply(paths[toMakeAbsolute], function(x) file.path(paths[["projectPath"]], x))
    }
    paths <- lapply(paths, checkPath, create = TRUE)
    if (!inProject) {
      setwd(paths[["projectPath"]])
    }

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

    changedLibPaths <- !identical(normPath(.libPaths()[1]), paths[["packagePath"]])

    Require::setLibPaths(paths[["packagePath"]], standAlone = standAlone,
                         updateRprofile = updateRprofile,
                         exact = FALSE, verbose = verbose)

    do.call(setPaths, paths[spPaths])

    messageVerbose(yellow("  done setting up paths"), verbose = verbose)

    paths[order(names(paths))]
  }

