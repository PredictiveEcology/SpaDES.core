utils::globalVariables("str")

baseClassesCanHandle <- c("pdf", "jpeg", "png", "tiff", "bmp")
ggplotClassesCanHandle <- c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")

#' `Plot` wrapper intended for use in a SpaDES module
#'
#' This is a single function call that allows a user to change which format in which
#' the plots will occur.
#' Specifically, the two common formats would be to `"screen"` or to disk as an image file,
#' such as `"png"`.
#' *This has currently been tested with `ggplot2`, `RasterLayer`, and `tmap` objects.*
#' The default (or change with e.g., `fn = "print", usePlot = FALSE`) uses
#' `Plot` internally, so individual plots may be rearranged. When saved to
#' disk (e.g., via `type = 'png'`), then `Plot` will not be used and the single object
#' that is the result of this `Plots` call will be saved to disk.
#' This function requires at least 2 things: a plotting function and arguments passed
#' to that function (which could include `data`, but commonly would simply be named
#' arguments required by `fn`).
#' See below and examples.
#'
#' @note **This is still experimental and could change in the next release.**
#'
#' `Plots` now has experimental support for "just a `Plot` call", but with `types` specified.
#' See examples.
#' The devices to save on disk will have some different behaviours to the screen representation,
#' since "wiping" an individual plot on a device doesn't exist for a file device.
#'
#' This offers up to 4 different actions for a given plot:
#'     \itemize{
#'       \item To screen device
#'       \item To disk as raw data (limited testing)
#'       \item To disk as a saved plot object  (limited testing)
#'       \item To disk as a \file{.png} or other image file, e.g., \file{.pdf}
#'     }
#'
#' To turn off plotting both to screen and disk, set both
#' `.plotInititalTime = NA` and `.plots = NA` or any other
#' value that will not trigger a TRUE with a `grepl` with the `types`
#' argument (e.g., `""` will omit all saving).
#'
#' @param data An (optional) arbitrary data object. If supplied, it will be passed as
#'   the first argument to `Plot` function, and should contain all the data
#'   required for the inner plotting. If passing a `RasterLayer`,
#'   it may be a good idea to set `names(RasterLayer)` so that
#'   multiple layers can be plotted without overlapping each other. When a custom `fn`
#'   is used and all arguments for `fn` are supplied and named, then this can be omitted.
#'   See examples.
#' @param fn An arbitrary plotting function. If not provided, defaults to using `quickPlot::Plot`
#' @param filename A name that will be the base for the files that will be saved, i.e,
#'   do not supply the file extension, as this will be determined based on `types`.
#'   If a user provides this as an absolute path, it will override the `path`
#'   argument.
#' @param types Character vector, zero or more of types. If used within a module, this
#'   will be deduced from the `P(sim)$type` and can be omitted. See below.
#' @param path Currently a single path for the saved objects on disk. If `filename`
#'   is supplied as an absolute path, `path` will be set to `dirname(filename)`,
#'   overriding this argument value.
#' @param .plotInitialTime A numeric. If `NA` then no visual on screen. Anything
#'   else will have visuals plotted to screen device. This is here for backwards
#'   compatibility. A developer should set in the module to the intended initial
#'   plot time and leave it, i.e., *not* `NA`.
#' @param ggsaveArgs An optional list of arguments passed to `ggplot2::ggsave`
#' @param deviceArgs An optional list of arguments passed to one of `png`,
#'       `pdf`, `tiff`, `bmp`, or `jgeg`.
#'       This is useful when the plotting function is not creating a `ggplot` object,
#'       e.g., plotting a `RasterLayer`.
#'
#' @param usePlot Logical. If `TRUE`, the default, then the plot will occur
#'   with `quickPlot::Plot`, so it will be arranged with previously existing plots.
#'
#' @param useCache Logical or character vector. If `TRUE`, caching is enabled for
#'   all non-screen output types. If a character vector, caching is enabled only
#'   for the matching `types`. Default is `FALSE`.
#'
#' @param envir The environment where the `data` argument should be evaluated if it is
#'   a `call`. Normally, this should be left at its default, `parent.frame()`.
#'
#' @param ... Anything needed by `fn`, all named.
#'
#' @return Called for its side effect of plot creation.
#'
#' @details
#'
#' \itemize{
#'   \item `type`
#'     \itemize{
#'       \item `"screen"` -- Will plot to the current device, normally a plot window
#'       \item `"object"` -- Will save the plot object, e.g., `ggplot` object
#'       \item `"raw"` -- Will save the raw data prior to plotting, e.g.,
#'                           the data argument
#'       \item `"png"` -- or any other type save-able with `ggsave`
#'     }
#' }
#'
#' @section Recording of files saved:
#' In cases where files are saved, and where `Plots` is used within a SpaDES module,
#' the file(s) that is/are saved will be appended to the `outputs` slot of the
#' `simList` of the module. This will, therefore, keep a record of figures saved
#' *within* the `simList`
#'
#' @export
#' @include simList-accessors.R
#' @importFrom grDevices dev.off dev.cur
#' @importFrom qs2 qs_save
#' @importFrom quickPlot clearPlot Plot
#' @importFrom rlang as_label is_quosure
#' @importFrom terra writeRaster
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \donttest{
#'   # Note: if this is used inside a SpaDES module, do not define this
#'   #  function inside another function. Put it outside in a normal
#'   #  module script. Otherwise, it will cause a memory leak.
#'   if (requireNamespace("ggplot2")) {
#'     fn <- function(d)
#'       ggplot2::ggplot(d, ggplot2::aes(a)) +
#'       ggplot2::geom_histogram()
#'     sim <- simInit()
#'     sim$something <- data.frame(a = sample(1:10, replace = TRUE))
#'
#'     Plots(data = sim$something, fn = fn,
#'           types = c("png"),
#'           path = file.path(tempdir(), "figures"),
#'           filename = tempfile(),
#'           .plotInitialTime = 1)
#'
#'     # plot to active device and to png
#'     Plots(
#'       data = sim$something, fn = fn,
#'       types = c("png", "screen"),
#'       path = file.path(tempdir(), "figures"),
#'       filename = tempfile(),
#'       .plotInitialTime = 1
#'     )
#'
#'     # Can also be used like quickPlot::Plot, but with control over output type
#'     r <- terra::rast(terra::ext(0,10,0,10),
#'                      vals = sample(1:3, size = 100, replace = TRUE))
#'     Plots(r, types = c("screen", "png"), filename = tempfile(),
#'           deviceArgs = list(width = 700, height = 500),
#'           usePlot = TRUE)
#'
#'     # with ggplotify, Plots can also be used to plot/save
#'     # non-ggplot objects:
#'
#'
#'     if (require("ggplotify")) {
#'       if (!require("lattice")) stop("please install lattice")
#'
#'       p1 <- densityplot(~mpg|cyl, data=mtcars)
#'       Plots(data = p1, fn = as.ggplot, filename = tempfile(),
#'             ggsaveArgs = list(width = 5, height = 4, dpi = 300,
#'                               bg = "white", units = "in"),
#'             types = c("screen", "png"),
#'             .plotInitialTime = 1)
#'     }
#'   } # end ggplot
#'   unlink("figures") # clean up
#' } # end of dontrun
Plots <- function(data, fn, filename,
                  types = quote(params(sim)[[currentModule(sim)]]$.plots),
                  path = quote(figurePath(sim)),
                  .plotInitialTime = quote(params(sim)[[currentModule(sim)]]$.plotInitialTime),
                  ggsaveArgs = list(), usePlot = getOption("spades.PlotsUsePlot", FALSE),
                  deviceArgs = list(), envir = parent.frame(), useCache = FALSE,
                  ...) {
  simIsIn <- NULL
  if (any(is(types, "call") || is(path, "call") || is(.plotInitialTime, "call"))) {
    simIsIn <- parent.frame() # try for simplicity sake... though the .whereInStack would get this too
    if (!exists("sim", simIsIn, inherits = FALSE)) {
      simIsIn <- try(.whereInStack("sim"), silent = TRUE)
      if (is(simIsIn, "try-error"))
        simIsIn <- NULL
    }
  }

  ## Deal with non-sim cases
  if (is.null(simIsIn)) {
    if (is.call(types) && any(grepl("sim", types)))
      types <- "screen"
    if (is.call(path) && any(grepl("sim", path)))
      path = "."
    if (is.call(.plotInitialTime) && any(grepl("sim", .plotInitialTime)))
      .plotInitialTime <- 0L
  }

  if (!is.null(simIsIn))
    if (is(types, "call"))
      types <- eval(types, envir = simIsIn)
  if (is(types, "list"))
    types <- unlist(types)

  if (!is.null(simIsIn)) {

    if (is(simIsIn, "try-error")) {
      .plotInitialTime <- 0L
    } else {
      envir <- simIsIn
      sim <- get("sim", envir = simIsIn)
      ## only look in the metadata -- not the simList (which will have a default of NA)
      isPlotITinSim <- ".plotInitialTime" %in% moduleMetadata(sim, currentModule(sim))$parameters$paramName
      if (isFALSE(isPlotITinSim))
        .plotInitialTime <- NULL

      if (is(.plotInitialTime, "call")) {
        .plotInitialTime = try(eval(.plotInitialTime, envir = simIsIn), silent = TRUE)
        if (is(.plotInitialTime, "try-error"))
          .plotInitialTime <- 0L
      }
    }
  } else {
    .plotInitialTime <- 0L
  }

  ggplotClassesCanHandleBar <- paste(ggplotClassesCanHandle, collapse = "|")
  needSave <- any(grepl(paste(ggplotClassesCanHandleBar, "|object"), types))

  ## has to be "screen" in .plots and also .plotInitialTime, if set, must be non-NA. Best way is don't set.
  needScreen <- !isTRUE(is.na(.plotInitialTime)) && any(grepl("screen", types))

  if (missing(data)) {
    data <- NULL
  } else {
    if (is.call(data))
      data <- eval(data, envir)
  }
  if (missing(fn)) {
    if (isTRUE(usePlot)) {
      fn <- Plot
    } else {
      if (inherits(data, c("SpatRaster", "SpatVector", "sf", "Raster", "sp")))
        fn <- terra::plot
      else
        fn <- plot
    }
  }
  fnIsPlot <- identical(fn, Plot) # || identical(fn, plot) || identical(fn, terra::plot)

  saveTypes <- setdiff(types, "screen")
  filenamesForSave <- character(length(saveTypes)) |> setNames(saveTypes)
  funsUsed <- character(length(saveTypes)) |> setNames(saveTypes)
  needSaveRaw <- any(grepl("raw", types))
  if (needSave || needSaveRaw) {
    if (missing(filename)) {
      dataObjName <- deparse(substitute(data))
      simTime <- if (exists("sim", inherits = FALSE)) round(as.numeric(time(sim)), 2) else 0
      filename <- paste0(dataObjName, "_time", simTime)
    } else {
      filename <- filename |> tools::file_path_sans_ext()
    }

    if (isAbsolutePath(filename)) {
      path <- dirname(filename)
    }

    filename <- basename(filename)

    isDefaultPath <- identical(eval(formals(Plots)$path), path)
    if (!is.null(simIsIn)) {
      if (is(path, "call"))
        path <- eval(path, envir = simIsIn)
    }

    if (is(path, "character")) {
      checkPath(path, create = TRUE)
    }

    if (needSaveRaw) {
      if (is(data, "Raster") || is(data, "SpatRaster")) {
        filenamesForSave[["raw"]] <- file.path(path, paste0(filename, "_data.tif"))
        funsUsed[["raw"]] <- "terra::writeRaster"
      } else {
        filenamesForSave[["raw"]] <- file.path(path, paste0(filename, "_data.qs2"))
        funsUsed[["raw"]] <- "qs2::qs_save"
      }
    }
  }
  objChar <- "object"

  # Setup one special case of gg
  gg <- NULL
  if (is(data, "gg")) {
    gg <- data
  }

  if (needSave) {
    if (is.null(simIsIn)) {
      if (is.call(path)) {
        path <- "."
      }
    }
    filenameStart <- file.path(path, filename)
    if (fnIsPlot || ( !is(gg, "gg") && !is(data, "gg")) ) {
      for (bsf in intersect(baseClassesCanHandle, types)) {
        filenamesForSave[[bsf]] <- paste0(filenameStart, ".", bsf)
        funsUsed[[bsf]] <- .guessPkgFun(bsf)
      }
    } else {
      for (ggsf in intersect(ggplotClassesCanHandle, types)) {
        filenamesForSave[[ggsf]] <- paste0(filenameStart, ".", ggsf)
        funsUsed[[ggsf]] <- "ggplot2::ggsave"
      }
    }

    if (any(grepl(objChar, types))) {
      filenamesForSave[[objChar]] <- paste0(filenameStart, "_gg.qs2")
      funsUsed[[objChar]] <- "qs2::qs_save"
    }
  }

  needNewPlot <- TRUE
  
  hasNonScreenTypes <- length(setdiff(types, "screen")) > 0
  useCache <- hasNonScreenTypes && (isTRUE(useCache) || length(intersect(types, useCache)) > 0)
  if (useCache) {
    useCacheNNP <- useCacheNeedNewPlot(filenamesForSave, ...) # creates a TRUE if it needs new plot
    cacheId <- cacheId(useCacheNNP)
    needNewPlot <- useCacheNNP
  }

  if (needNewPlot) {
    if (fnIsPlot) {
      ## make dummies
      gg <- 1
      objNamePassedToData1 <- substitute(data)
      origEnv <- parent.frame()
      objNamePassedToData <- evalAttempt(objNamePassedToData1, origEnv)
      if (!is.character(objNamePassedToData)) {
        objNamePassedToData <- deparse1(objNamePassedToData)
      }

      ## Try to see if the object is in the parent.frame(). If it isn't, default back to here.
      if (!objNamePassedToData %in% ls(origEnv))
        origEnv <- environment()
      if (!(is(data, "list") && length(names(data)) == length(data))) {
        ggListToScreen <- list(data)
        names(ggListToScreen) <- objNamePassedToData
      } else {
        ggListToScreen <- data
      }
    } else {
      if ( (needScreen || needSave) ) {
        if (missing(data) || is.null(data)) {
          gg <- fn(...)
        } else {
          gg <- NULL
          if (is(data, "ggplot")) {
            gg <- data
          } else {
            # don't plot or terra::plot if needScreen is FALSE
            if ( (!(identical(plot, fn) || identical(terra::plot, fn)) ) || needScreen)
              gg <- fn(data, ...) # This will plot to screen if it is base::plot or terra::plot
          }
        }

        if (!is(gg, ".quickPlot")) {
          ggListToScreen <- setNames(list(gg), "gg")
          if (!is.null(gg$labels$title) && needScreen) {
            ggListToScreen <- setNames(ggListToScreen,
                                       format(paste(gg$labels$title, collapse = " ")))
            ggListToScreen[[1]]$labels$title <- NULL
          }
        }
      }
    }

    if (needScreen) {
      if (fnIsPlot) {
        if (is.list(data)) {# || is(data, "RasterStack") || is(data, "RasterBrick") ||
          #    (is(data, "SpatRaster") || is(data, "SpatVector")) && nlayers2(data) > 1)
          #  {
          dataListToScreen <- data
        } else {
          dataListToScreen <- list(data)
        }
        if (is(data, "ggplot")) {
          dataListToScreen <- setNames(list(data), "gg")
          if (!is.null(data$labels$title) && needScreen) {
            dataListToScreen <- setNames(dataListToScreen, data$labels$title)
            dataListToScreen[[1]]$labels$title <- NULL
          }
        } else {
          if (!is.null(objNamePassedToData)) {
            dataListToScreen <- setNames(dataListToScreen, objNamePassedToData)
          } else {
            if (!is.null(names(data))) {
              dataListToScreen <- setNames(dataListToScreen, names(data))
            } else {
              dataListToScreen <- setNames(dataListToScreen, "data")
            }
          }
        }

        ## Necessary for inheritance -- pass the environment with correct inheritance
        if (!is.null(simIsIn)) {
          newEnv <- new.env(parent = simIsIn)
        } else {
          newEnv <- environment()
        }
        newEnv$dataListToScreen <- dataListToScreen
        gg <- fn(dataListToScreen, ..., env = newEnv)

      } else {
        if (is(gg, "gg"))
          if (!requireNamespace("ggplot2")) stop("Please install ggplot2")
        if (usePlot) {
          names(ggListToScreen) <- gsub(names(ggListToScreen), pattern = " |(\\\n)|[[:punct:]]", replacement = "_")
          Plot(ggListToScreen, addTo = gg$labels$title)
        } else {
          if ((!(identical(fn, plot) || identical(fn, terra::plot)) || is(gg, "gg")) &&
              !is(gg, ".quickPlot")) {
            print(gg)
          }
        }
      }
    }
  }
  
  if (!is(data, "ggplot")) {
    if (needSaveRaw) {
      if (is(data, "Raster") || is(data, "SpatRaster")) {
        writeRaster(data, filename = filenamesForSave[["raw"]], overwrite = TRUE)
      } else {
        qs2::qs_save(data, filenamesForSave[["raw"]])
      }
    }
  } else {
    if (needSaveRaw) {
      message("Can't save raw ggplot2 objects as the file is too large; saving as image...")
      needSave <- TRUE
    }
  }
  isBaseFormat <- fnIsPlot || (!is(gg, "gg") && !is(data, "gg"))
  failedFiles <- character(0)
  if (needSave && needNewPlot) {
    if (isBaseFormat) {
      for (bsf in intersect(baseClassesCanHandle, types)) {
        type <- get(bsf)
        theFilename <- filenamesForSave[[bsf]]

        # the plot saving ###
        do.call(type, modifyList2(list(theFilename), deviceArgs))
        if (isTRUE(fnIsPlot)) clearPlot()
        if (missing(data) || is.null(data)) {
          plotted <- fn(...)
        } else {
          plotted <- try(fn(data, ...), silent = TRUE) # if this fails, catch so it can be dev.off'd
        }
        dev.off()
        if (is(plotted, "try-error")) {
          warning("Plots: fn(data, ...) failed for file '", theFilename, "': ",
                  conditionMessage(attr(plotted, "condition")))
          failedFiles <- c(failedFiles, theFilename)
        } else {
          funsUsed[[bsf]] <- .guessPkgFun(bsf)
        }
        # end plot saving ###
      }
    } else {
      if (!requireNamespace("ggplot2")) stop("To save gg objects, need ggplot2 installed")
      for (ggsf in intersect(ggplotClassesCanHandle, types)) {
        theFilename <- filenamesForSave[[ggsf]]

        # the plot saving ###
        args <- list(plot = gg, filename = theFilename)
        if (length(ggsaveArgs))
          args <- modifyList2(args, ggsaveArgs)
        do.call(ggplot2::ggsave, args = args)
        funsUsed[[ggsf]] <- "ggplot2::ggsave"
        # end plot saving ###
      }
    }

    if (any(grepl(objChar, types)))
      qs2::qs_save(gg, file = filenamesForSave[[objChar]])
  }

  if (needSave || needSaveRaw) {
    if (exists("sim", inherits = FALSE)) {
      for (i in seq(filenamesForSave)) {
        if (filenamesForSave[i] %in% failedFiles) next

        sim@outputs <- outputsAppend(
          outputs = sim@outputs, saveTime = time(sim),
          objectName = tools::file_path_sans_ext(basename(filenamesForSave[i])),
          file = filenamesForSave[i],
          fun = funsUsed[i],
          ...
        )
        sim <- rmDups(sim)

        if (needNewPlot)
          message("Saved figure to: ", filenamesForSave[i])
        else
          message("Previously saved figure (", filenamesForSave[i], ") still valid")
      }
    }
  }

  if (useCache) {
    typesNoScreen <- setdiff(types, "screen")
    for (i in seq(filenamesForSave)) {
      type <- typesNoScreen[i]
      filenameSaved <- .robustDigest(asPath(filenamesForSave[i]))[[1]]
      if (needNewPlot)
        .addTagsRepo(cacheId, cachePath = cachePath(sim), tagKey = tagKeySavedFile, tagValue = paste0(type, ":", filenameSaved))
    }
  }

  if (exists("sim", inherits = FALSE)) {
    assign("sim", sim, envir = simIsIn)
  }

  on.exit() # clear the clearCache if it gets to here
  if (exists("gg", inherits = FALSE))
    return(invisible(gg))
  else
    return(invisible(NULL))
}

#' Test whether there should be any plotting from `.plots` module parameter
#'
#' This will do all the various tests needed to determine whether
#' plotting of one sort or another will occur.
#' Testing any of the types as listed in [Plots()] argument `types`.
#' Only the first 3 letters of the type are required.
#'
#' @param .plots Usually will be the `P(sim)$.plots` is used within
#'   a module.
#'
#' @return logical of length 1
#'
#' @export
anyPlotting <- function(.plots) {
  needSaveRaw <- any(grepl("raw", .plots))
  ggplotClassesCanHandleBar <- paste(ggplotClassesCanHandle, collapse = "|")
  needSave <- any(grepl(paste(ggplotClassesCanHandleBar, "|obj"), .plots))
  needScreen <- any(grepl("scr", .plots))

  needSaveRaw || needSave || needScreen
}

#' Guess package of a function
#'
#' @param bsf character. A function name
#'
#' @return character. The package and function name as `"pkg::bsf"`
.guessPkgFun <- function(bsf) {
  pkgName <- eval(parse(text = paste0("environmentName(environment(", bsf, "))")))
  return(paste0(pkgName, "::", bsf))
}

evalAttempt <- function(subs, envir) {
  if (length(subs) > 2) {
    subsOrig <- subs
    out <- try(eval(subs[[3]], envir = envir), silent = TRUE)
    if (is(out, "try-error"))
      subs <- subsOrig
    else
      subs[[3]] <- out

    if (is.call(subs[[2]])) {
      out <- try(evalAttempt(subs[[2]], envir = envir), silent = TRUE)
      if (!is(out, "try-error"))
        subs[[2]] <- out

    }
    if (is(out, "try-error"))
      subs <- subsOrig
  }
  subs
}



useCacheNeedNewPlot <- function(filenamesForSave, envir = parent.frame(), ...) {
  allArgs <- mget(formalArgs(Plots), envir = envir)
  # check dots
  mc <- match.call(Plots, sys.call(-1), expand.dots = TRUE)[-1]
  allNamed <- names(mc) %in% formalArgs(Plots)
  if (any(allNamed %in% FALSE)) {
    dotArgs <- list(...)
    allArgs <- append(allArgs, dotArgs)
  }
  allArgs$envir <- NULL
  allArgs$... <- NULL
  if (is(allArgs$data, "ggplot")) {
    metadata <- strip_ggplot_metadata(allArgs$data)
    allArgs[["data"]] <- .robustDigest(metadata)
  }
  cached <- list(allArgs) |> Cache(.functionName = paste0("Plots_", basename(filenamesForSave[[1]])))
  reproducible:::on.exit2({
    clearCache(cacheId = cacheId(cached), ask = FALSE, verbose = FALSE)
    message("Plots did not complete; clearing the cached record")
    })
  
  ret <- attr(cached, ".Cache")$newCache %in% TRUE
  attributes(ret) <- attributes(cached)
  if (ret %in% FALSE) { # means it doesn't need a new plotting
    sc <- showCacheFast(cacheId = cacheId(cached))
    files <- sc[tagKey %in% tagKeySavedFile]$tagValue
    digs <- .robustDigest(asPath(filenamesForSave))
    digsToCompare <- paste0(names(digs), ":", unname(digs))
    if (!all(files %in% digsToCompare)) {# don't have the correct files
      reproducible::messageCache("*** but saved files are incorrect; rerunning to reproduce figures ***")
      ret <- TRUE
    }
  }
  ret
}

tagKeySavedFile <- "savedFile"



rmDups <- function(sim) {
  cantUniqueOn <- sapply(sim@outputs, function(x) is(x, "list") || is(x, "AsIs"))
  checkForDups <- duplicated(sim@outputs, by = names(cantUniqueOn)[!cantUniqueOn])
  if (any(checkForDups)) {
    for (nam in names(cantUniqueOn)[cantUniqueOn]) {
      dupsInCantDoCols <- duplicated(sim@outputs[[nam]])
      alsoDup <- checkForDups[checkForDups] == dupsInCantDoCols[checkForDups]
      checkForDups[!alsoDup] <- FALSE
    }
    sim@outputs <- sim@outputs[checkForDups %in% FALSE]
  }
  sim
}

# Canonical digestable representation of a ggplot object
# Returns a base R list with deterministic ordering & values.
# You can serialize with jsonlite::toJSON(auto_unbox=TRUE, null="null", digits=15)
# and hash with digest::digest(., algo = "sha256")

# Canonical digestable representation of a ggplot object
# Adds data-dependent fingerprints from ggplot_build().
# Output is a base R list; serialize (e.g., jsonlite::toJSON) then hash.

# Canonical digestable representation of a ggplot object
# Adds data-dependent fingerprints from ggplot_build().
# Output is a base R list; serialize (e.g., jsonlite::toJSON) then hash.

strip_ggplot_metadata <- canonicalize_ggplot <- function(
    p,
    numeric_digits = 15#,
    #hash_fun = function(x) digest::digest(x, algo = "xxhash64")
) {
  stopifnot(inherits(p, "ggplot"))
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  as_label_safe <- function(x) {
    if (is.null(x)) return(NULL)
    tryCatch(rlang::as_label(x), error = function(e) NULL)
  }
  
  map_labels <- function(m) {
    if (is.null(m)) return(NULL)
    out <- lapply(m, as_label_safe)
    out[order(names(out))]
  }
  
  drop_nulls <- function(x) if (is.null(x)) NULL else x[!vapply(x, is.null, logical(1))]
  sort_list_recursive <- function(x) {
    if (is.list(x) && !is.null(names(x))) x <- x[order(names(x))]
    lapply(x, function(v) if (is.list(v)) sort_list_recursive(v) else v)
  }
  
  keep_simple_fields <- function(x) {
    if (is.null(x)) return(NULL)
    y <- list()
    nms <- names(x)
    for (nm in nms) {
      v <- x[[nm]]
      if (is.atomic(v) && !is.object(v)) {
        y[[nm]] <- v
      } else if (is.list(v) && all(vapply(v, function(i) is.atomic(i) && !is.object(i), logical(1)))) {
        y[[nm]] <- v
      }
    }
    drop_nulls(y)
  }
  
  # --- RAW DATA DIGEST -------------------------------------------------------
  raw_plot_data_digest <- if (is.data.frame(p$data)) .robustDigest(p$data) else NULL
  
  raw_layer_data <- lapply(p$layers, function(l) {
    if (is.null(l$data)) {
      list(source = "plot", digest = raw_plot_data_digest)
    } else if (is.data.frame(l$data)) {
      list(source = "layer", digest = .robustDigest(l$data))
    } else {
      # data as function or other object -> hash its deparsed form
      list(source = "other", digest = .robustDigest(utils::capture.output(str(l$data))))
    }
  })
  
  # --- SPEC PART (same as before) --------------------------------------------
  plot_data_cols <- if (is.data.frame(p$data)) sort(names(p$data)) else NULL
  plot_mapping   <- map_labels(p$mapping)
  
  layers_spec <- lapply(seq_along(p$layers), function(i) {
    l <- p$layers[[i]]
    list(
      geom_class     = class(l$geom)[1],
      stat_class     = class(l$stat)[1],
      position_class = class(l$position)[1],
      inherit_aes    = isTRUE(l$inherit.aes),
      mapping        = map_labels(l$mapping),
      aes_params     = keep_simple_fields(l$aes_params),
      geom_params    = keep_simple_fields(l$geom_params),
      stat_params    = keep_simple_fields(l$stat_params),
      show_legend    = if (is.null(l$show.legend)) NULL else as.character(l$show.legend),
      data_digest    = raw_layer_data[[i]]   # <-- added here
    )
  })
  
  facet <- NULL
  if (!is.null(p$facet$params)) {
    fp <- p$facet$params
    facet <- list(
      facet_class = class(p$facet)[1],
      vars        = if (!is.null(fp$facets)) sort(names(fp$facets)) else NULL,
      nrow        = fp$nrow %||% NULL,
      ncol        = fp$ncol %||% NULL,
      free        = fp$free   %||% NULL,
      strip_pos   = fp$strip.position %||% NULL
    )
  }
  
  scales <- lapply(p$scales$scales, function(s) {
    list(
      scale_class = class(s)[1],
      aesthetics  = sort(unique(s$aesthetics %||% character(0))),
      limits      = s$limits,
      trans       = tryCatch(s$trans$name, error = function(e) NULL),
      position    = s$position %||% NULL,
      guide       = if (!is.null(s$guide) && !is.logical(s$guide)) as.character(s$guide) else s$guide
    )
  })
  
  coords <- list(
    coord_class = class(p$coordinates)[1],
    params      = keep_simple_fields(p$coordinates)
  )
  
  theme_elems <- NULL
  if (length(p$theme)) {
    theme_elems <- lapply(as.list(p$theme), keep_simple_fields)
    theme_elems <- drop_nulls(theme_elems)
    if (length(theme_elems)) theme_elems <- theme_elems[order(names(theme_elems))]
  }
  
  labels <- NULL
  if (!is.null(p$labels) && length(p$labels)) {
    labels <- lapply(p$labels, function(v) {
      if (is.symbol(v) || rlang::is_quosure(v)) as_label_safe(v) else as.character(v)
    })
    labels <- labels[order(names(labels))]
  }
  
  out <- list(
    spec_version = "ggplot-dna/3",
    data_cols    = plot_data_cols,
    data_digest  = raw_plot_data_digest,   # <-- also top-level digest
    mapping      = plot_mapping,
    layers       = layers_spec,
    scales       = scales,
    coordinates  = coords,
    facet        = facet,
    labels       = labels,
    theme        = theme_elems
  )
  
  out <- drop_nulls(out)
  out <- sort_list_recursive(out)
  out
}
      
`%||%` <- function(x, y) if (is.null(x)) y else x
