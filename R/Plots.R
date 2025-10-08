baseClassesCanHandle <- c("pdf", "jpeg", "png", "tiff", "bmp")
ggplotClassesCanHandle <- c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")

#' Plotting wrapper intended for use in SpaDES modules
#'
#' This is a single function call that allows a user to change which format in which
#' the plots will occur.
#' Specifically, the two common formats would be to `"screen"` or to disk as an image file,
#' such as `"png"`.
#' *This has currently been tested with `ggplot2`, `RasterLayer`, and `tmap` objects.*
#' This function requires at least 2 things: a plotting function and arguments passed
#' to that function (which could include `data`, but commonly would simply be named
#' arguments required by `fn`).
#' See below and examples.
#'
#' @note **This is still experimental and could change in the next release.**
#'
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
#'   the first argument to the plotting function, and should contain all the data
#'   required for the inner plotting.
#'   If passing a `RasterLayer`, it may be a good idea to set `names(RasterLayer)` so that
#'   multiple layers can be plotted without overlapping each other.
#'   When a custom `fn` is used and all arguments are supplied and named, this can be omitted.
#'   See examples.
#' @param fn An arbitrary plotting function.
#' @param filename A name that will be the base for the files that will be saved, i.e,
#'   do not supply the file extension, as this will be determined based on `types`.
#'   If a user provides this as an absolute path, it will override the `path`
#'   argument.
#' @param types Character vector, zero or more of types. If used within a module, this
#'   will be deduced from the `P(sim)$type` and can be omitted. See below.
#' @param path Currently a single path for the saved objects on disk.
#'   If `filename` is supplied as an absolute path, `path` will be set to `dirname(filename)`,
#'   overriding this argument value.
#' @param .plotInitialTime A numeric. If `NA` then no visual on screen. Anything
#'   else will have visuals plotted to screen device. This is here for backwards
#'   compatibility. A developer should set in the module to the intended initial
#'   plot time and leave it, i.e., *not* `NA`.
#' @param ggsaveArgs An optional list of arguments passed to `ggplot2::ggsave`
#' @param deviceArgs An optional list of arguments passed to one of [grDevices::png()],
#'       [grDevices::pdf()], [grDevices::tiff()], [grDevices::bmp()], or [grDevices::jpeg()].
#'       This is useful when the plotting function is not creating a `ggplot` object,
#'       e.g., plotting a `RasterLayer`.
#'
#' @param usePlot Deprecated; not used. Will be removed in a future release.
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
#'       \item `"screen"` -- Will plot to the current device, normally a plot window;
#'       \item `"object"` -- Will save the plot object, e.g., `ggplot` object;
#'       \item `"raw"` -- Will save the raw data prior to plotting, e.g., the data argument;
#'       \item `"png"` -- or any other type save-able with `ggsave`;
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
#' @importFrom qs qsave
#' @importFrom terra writeRaster
#' @importFrom tools file_path_sans_ext
#'
#' @examples
#' \donttest{
#'   # Note: if this is used inside a SpaDES module, do not define this
#'   #  function inside another function. Put it outside in a normal
#'   #  module script. Otherwise, it will cause a memory leak.
#'   if (requireNamespace("ggplot2")) {
#'     fn <- function(d) {
#'       ggplot2::ggplot(d, ggplot2::aes(a)) +
#'       ggplot2::geom_histogram()
#'     }
#'     sim <- simInit()
#'     sim$something <- data.frame(a = sample(1:10, replace = TRUE))
#'
#'     Plots(data = sim$something,
#'           fn = fn,
#'           types = c("png"),
#'           path = file.path(tempdir(), "figures"),
#'           filename = tempfile(),
#'           .plotInitialTime = 1)
#'
#'     ## plot to active device and to png
#'     Plots(
#'       data = sim$something,
#'       fn = fn,
#'       types = c("png", "screen"),
#'       path = file.path(tempdir(), "figures"),
#'       filename = tempfile(),
#'       .plotInitialTime = 1
#'     )
#'
#'     ## with ggplotify, can also be used to plot/save non-ggplot objects:
#'     if (require("ggplotify")) {
#'       if (!require("lattice")) stop("please install lattice")
#'
#'       p1 <- densityplot(~mpg|cyl, data = mtcars)
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
                  ggsaveArgs = list(),
                  usePlot = FALSE,
                  deviceArgs = list(),
                  envir = parent.frame(),
                  ...) {
  if (isTRUE(usePlot)) {
    stop("usePlot is deprecated and must be FALSE; it will removed in a future release.")
  }

  simIsIn <- NULL
  if (any(is(types, "call") || is(path, "call") || is(.plotInitialTime, "call"))) {
    simIsIn <- parent.frame() # try for simplicity sake... though the whereInStack would get this too
    if (!exists("sim", simIsIn, inherits = FALSE)) {
      simIsIn <- try(whereInStack("sim"), silent = TRUE)
      if (is(simIsIn, "try-error")) {
        simIsIn <- NULL
      }
    }
  }

  ## Deal with non-sim cases
  if (is.null(simIsIn)) {
    if (is.call(types) && any(grepl("sim", types))) {
      types <- "screen"
    }
    if (is.call(path) && any(grepl("sim", path))) {
      path = "."
    }
    if (is.call(.plotInitialTime) && any(grepl("sim", .plotInitialTime))) {
      .plotInitialTime <- 0L
    }
  }

  if (!is.null(simIsIn)) {
    if (is(types, "call")) {
      types <- eval(types, envir = simIsIn)
    }
  }
  if (is(types, "list")) {
    types <- unlist(types)
  }

  if (!is.null(simIsIn)) {
    if (is(simIsIn, "try-error")) {
      .plotInitialTime <- 0L
    } else {
      envir <- simIsIn
      sim <- get("sim", envir = simIsIn)
      ## only look in the metadata -- not the simList (which will have a default of NA)
      isPlotITinSim <- ".plotInitialTime" %in%
        moduleMetadata(sim, currentModule(sim))$parameters$paramName
      if (isFALSE(isPlotITinSim)) {
        .plotInitialTime <- NULL
      }

      if (is(.plotInitialTime, "call")) {
        .plotInitialTime = try(eval(.plotInitialTime, envir = simIsIn), silent = TRUE)
        if (is(.plotInitialTime, "try-error")) {
          .plotInitialTime <- 0L
        }
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
    if (is.call(data)) {
      data <- eval(data, envir)
    }
  }
  if (missing(fn)) {
    if (inherits(data, c("SpatRaster", "SpatVector", "sf", "Raster", "sp"))) {
      fn <- terra::plot
    } else {
      fn <- plot
    }
  }

  fnIsPlot <- identical(fn, Plot) # || identical(fn, plot) || identical(fn, terra::plot)
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
    if (!objNamePassedToData %in% ls(origEnv)) {
      origEnv <- environment()
    }
    if (!(is(data, "list") && length(names(data)) == length(data))) {
      ggListToScreen <- list(data)
      names(ggListToScreen) <- objNamePassedToData
    } else {
      ggListToScreen <- data
    }
  } else {
    if ((needScreen || needSave)) {
      if (is.null(data)) {
        gg <- fn(...)
      } else {
        gg <- NULL
        if (is(data, "ggplot")) {
          gg <- data
        } else {
          gg <- fn(data, ...) # This will plot to screen if it is base::plot or terra::plot
        }
      }

      if (!is(gg, ".quickPlot")) {
        ggListToScreen <- setNames(list(gg), "gg")
        if (!is.null(gg$labels$title) && needScreen) {
          ggListToScreen <- setNames(ggListToScreen, format(paste(gg$labels$title, collapse = " ")))
          ggListToScreen[[1]]$labels$title <- NULL
        }
      }
    }
  }

  if (needScreen) {
    if (is(gg, "gg")) {
      if (!requireNamespace("ggplot2")) stop("Please install ggplot2")
    }

    if ((!(identical(fn, plot) || identical(fn, terra::plot)) || is(gg, "gg")) &&
        !is(gg, ".quickPlot")) { ## TODO: remove this as part of quickPlot deprecation?
      print(gg)
    }
  }

  needSaveRaw <- any(grepl("raw", types))
  if (needSave || needSaveRaw) {
    if (missing(filename)) {
      dataObjName <- deparse(substitute(data))
      ## TODO: can we use e.g. 'the object name + sim time' for the filename ??
      filename <- paste0(dataObjName, "_", basename(gsub("file", "", tempfile(fileext = ""))))
      if (exists("sim", inherits = FALSE)) {
        simTime <- round(as.numeric(time(sim)), 3)
        filename <- paste0("sim", "_", filename)
      }
    } else {
      filename <- filename |> tools::file_path_sans_ext()
    }

    if (isAbsolutePath(filename)) {
      path <- dirname(filename)
    }

    filename <- basename(filename)

    isDefaultPath <- identical(eval(formals(Plots)$path), path)
    if (!is.null(simIsIn)) {
      if (is(path, "call")) {
        path <- eval(path, envir = simIsIn)
      }
    }

    if (is(path, "character")) {
      checkPath(path, create = TRUE)
    }
  }

  if (needSaveRaw) {
    if (is(data, "Raster") || is(data, "SpatRaster")) {
      rasterFilename <- file.path(path, paste0(filename, "_data.tif"))
      writeRaster(data, filename = rasterFilename, overwrite = TRUE)
      if (exists("sim", inherits = FALSE)) {
        sim@outputs <- outputsAppend(
          outputs = sim@outputs,
          saveTime = time(sim),
          objectName = tools::file_path_sans_ext(basename(rasterFilename)),
          file = rasterFilename,
          fun = "terra::writeRaster",
          ...
        )
      }
    } else {
      rawFilename <- file.path(path, paste0(filename, "_data.qs"))
      qs::qsave(data, rawFilename)
      if (exists("sim", inherits = FALSE)) {
        sim@outputs <- outputsAppend(
          outputs = sim@outputs,
          saveTime = time(sim),
          objectName = tools::file_path_sans_ext(basename(rawFilename)),
          file = rawFilename,
          fun = "qs::qsave",
          ...
        )
      }
    }
  }
  if (needSave) {
    if (is.null(simIsIn)) {
      if (is.call(path)) {
        path <- "."
      }
      if (is.call(path)) {
        path <- "."
      }
    }
    if (fnIsPlot || !is(gg, "gg")) {
      baseSaveFormats <- intersect(baseClassesCanHandle, types)
      for (bsf in baseSaveFormats) {
        type <- get(bsf)
        theFilename <- file.path(path, paste0(filename, ".", bsf))
        do.call(type, modifyList2(list(theFilename), deviceArgs))
        # curDev <- dev.cur()

        plotted <- try(fn(data, ...)) # if this fails, catch so it can be dev.off'd
        dev.off()
        if (!is(plotted, "try-error")) {
          if (exists("sim", inherits = FALSE)) {
            pkgAndFn <- .guessPkgFun(bsf)
            sim@outputs <- outputsAppend(
              outputs = sim@outputs,
              saveTime = time(sim),
              objectName = tools::file_path_sans_ext(basename(theFilename)),
              file = theFilename,
              fun = pkgAndFn,
              ...
            )
          }
          message("Saved figure to: ", theFilename)
        }
      }
    } else {
      ggSaveFormats <- intersect(ggplotClassesCanHandle, types)
      for (ggsf in ggSaveFormats) {
        theFilename <- file.path(path, paste0(filename, ".", ggsf))
        if (!requireNamespace("ggplot2")) {
          stop("To save gg objects, need ggplot2 installed")
        }
        args <- list(plot = gg, filename = theFilename)
        if (length(ggsaveArgs)) {
          args <- modifyList2(args, ggsaveArgs)
        }
        do.call(ggplot2::ggsave, args = args)

        if (exists("sim", inherits = FALSE)) {
          sim@outputs <- outputsAppend(
            outputs = sim@outputs,
            saveTime = time(sim),
            objectName = tools::file_path_sans_ext(basename(theFilename)),
            file = theFilename,
            fun = "ggplot2::ggsave",
            ...
          )
        }
        message("Saved figure to: ", theFilename)
      }
    }

    if (any(grepl("object", types))) {
      filename11 <- file.path(path, paste0(filename, "_gg.qs"))
      qs::qsave(gg, file = filename11)

      if (exists("sim", inherits = FALSE)) {
        sim@outputs <- outputsAppend(
          outputs = sim@outputs,
          saveTime = time(sim),
          objectName = tools::file_path_sans_ext(basename(filename11)),
          file = filename11,
          fun = "qs::qsave",
          ...
        )
      }
    }
  }

  if (exists("sim", inherits = FALSE)) {
    assign("sim", sim, envir = simIsIn)
  }

  if (exists("gg", inherits = FALSE)) {
    return(invisible(gg))
  } else {
    return(invisible(NULL))
  }
}

#' Test whether there should be any plotting from `.plots` module parameter
#'
#' This will do all the various tests needed to determine whether
#' plotting of one sort or another will occur.
#' Testing any of the types as listed in [Plots()] argument `types`.
#' Only the first 3 letters of the type are required.
#'
#' @param .plots Usually will be the `P(sim)$.plots` is used within a module.
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
    if (is(out, "try-error")) {
      subs <- subsOrig
    } else {
      subs[[3]] <- out
    }

    if (is.call(subs[[2]])) {
      out <- try(evalAttempt(subs[[2]], envir = envir), silent = TRUE)
      if (!is(out, "try-error")) {
        subs[[2]] <- out
      }
    }
    if (is(out, "try-error")) {
      subs <- subsOrig
    }
  }
  subs
}
