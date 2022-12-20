#' `Plot` wrapper intended for use in a SpaDES module
#'
#' This is a single function call that allows a user to change which format in which
#' the plots will occur.
#' Specifically, the two common formats would be to `"screen"` or to disk as an image file,
#' such as `"png"`.
#' *THIS CURRENTLY HAS BEEN TESTED WITH `ggplot2`, `RasterLayer`, and
#' `tmap` objects.*
#' The default (or change with e.g., `fn = "print", usePlot = FALSE`) uses
#' `Plot` internally, so individual plots may be rearranged. When saved to
#' disk (e.g., via `type = 'png'`), then `Plot` will not be used and the single object
#' that is the result of this `Plots` call will be saved to disk.
#' This function requires at least 2 things: a plotting function and arguments passed
#' to that function (which could include `data`, but commonly would simply be named
#' arguments required by `fn`).
#' See below and examples.
#'
#' @note THIS IS STILL EXPERIMENTAL and could change in the next release.
#'
#' `Plots` now has experimental support for "just a `Plot` call",
#' but with `types` specified.
#' See example.
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
#'
#' To turn off plotting both to screen and disk, set both
#' `.plotInititalTime = NA` and `.plots = NA` or any other
#' value that will not trigger a TRUE with a `grepl` with the `types`
#' argument (e.g., `""` will omit all saving).
#'
#' @export
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
#' @param ... Anything needed by `fn`, all named.
#'
#' @importFrom grDevices dev.off dev.cur
#' @importFrom qs qsave
#' @importFrom raster writeRaster
#' @importFrom quickPlot clearPlot Plot whereInStack
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
#' @examples
#'
#' \donttest{
#' # Note: if this is used inside a SpaDES module, do not define this
#' #  function inside another function. Put it outside in a normal
#' #  module script. It will cause a memory leak, otherwise.
#' if (!require("ggplot2")) stop("please install ggplot2")
#' fn <- function(d)
#'   ggplot(d, aes(a)) +
#'   geom_histogram()
#' sim <- simInit()
#' sim$something <- data.frame(a = sample(1:10, replace = TRUE))
#'
#' Plots(data = sim$something, fn = fn,
#'       types = c("png"),
#'       path = file.path("figures"),
#'       filename = tempfile(),
#'       .plotInitialTime = 1
#'       )
#'
#' # plot to active device and to png
#' Plots(data = sim$something, fn = fn,
#'       types = c("png", "screen"),
#'       path = file.path("figures"),
#'       filename = tempfile(),
#'       .plotInitialTime = 1
#'       )
#'
#' # Can also be used like quickPlot::Plot, but with control over output type
#' r <- raster::raster(raster::extent(0,10,0,10), vals = sample(1:3, size = 100, replace = TRUE))
#' Plots(r, types = c("screen", "png"), deviceArgs = list(width = 700, height = 500))
#'
#' } # end of dontrun
Plots <- function(data, fn, filename,
                  types = quote(params(sim)[[currentModule(sim)]]$.plots),
                  path = quote(file.path(outputPath(sim), "figures")),
                  .plotInitialTime = quote(params(sim)[[currentModule(sim)]]$.plotInitialTime),
                  ggsaveArgs = list(), usePlot = getOption("spades.PlotsUsePlot", FALSE),
                  deviceArgs = list(),
                  ...) {

  simIsIn <- NULL
  if (any(is(types, "call") || is(path, "call") || is(.plotInitialTime, "call"))) {
    simIsIn <- parent.frame() # try for simplicity sake... though the whereInStack would get this too
    if (!exists("sim", simIsIn, inherits = FALSE)) {
      simIsIn <- try(whereInStack("sim"), silent = TRUE)
      if (is(simIsIn, "try-error"))
        simIsIn <- NULL
    }
  }

  # Deal with non sim cases
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
      sim <- get("sim", envir = simIsIn)
      # only look in the metadata -- not the simList (which will have a default of NA)
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

  # has to be "screen" in .plots and also .plotInitialTime, if set, must be non-NA. Best way is don't set.
  needScreen <- !isTRUE(is.na(.plotInitialTime)) && any(grepl("screen", types))
  if (missing(fn) && isTRUE(usePlot)) {
    fn <- Plot
  }
  fnIsPlot <- identical(fn, Plot)
  if (fnIsPlot) {
    # make dummies
    gg <- 1
    objNamePassedToData <- deparse1(substitute(data))
    origEnv <- parent.frame()

    # Try to see if the object is in the parent.frame(). If it isn't, default back to here.
    if (!objNamePassedToData %in% ls(origEnv))
      origEnv <- environment()
    ggListToScreen <- list(data)
    names(ggListToScreen) <- objNamePassedToData
  } else {
    if ( (needScreen || needSave) ) {
      if (missing(data)) {
        gg <- fn(...)
      } else {
        gg <- fn(data, ...)
      }

      if (!is(gg, ".quickPlot")) {
        ggListToScreen <- setNames(list(gg), "gg")
        if (!is.null(gg$labels$title) && needScreen) {
          ggListToScreen <- setNames(ggListToScreen, gg$labels$title)
          ggListToScreen[[1]]$labels$title <- NULL
        }
      }
    }
  }

  if (needScreen) {
    if (fnIsPlot) {
      if (is.list(data) || !is(data, "RasterStack") || !is(data, "RasterBrick")) {
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
        if (!is.null(names(data))) {
          dataListToScreen <- setNames(dataListToScreen, names(data))
        } else {
          dataListToScreen <- setNames(dataListToScreen, "data")
        }
      }

      gg <- fn(ggListToScreen, ...)
      .quickPlotEnv <- getFromNamespace(".quickPlotEnv", "quickPlot")
      qpob <- get(paste0("quickPlot", dev.cur()), .quickPlotEnv)
      objNamesInQuickPlotObj <- sapply(qpob$curr@quickPlotGrobList, function(x) slot(x[[1]], "objName"))
      objNamesInQuickPlotObj <- seq_along(objNamesInQuickPlotObj %in% names(ggListToScreen))
      curPlotDev <- paste0("quickPlot", dev.cur())
      ignore <- lapply(objNamesInQuickPlotObj, function(x) {
        slot(.quickPlotEnv[[curPlotDev]]$curr@quickPlotGrobList[[x]][[1]], "envir") <- origEnv
      })
    } else {
      if (is(gg, "gg"))
        if (!requireNamespace("ggplot2")) stop("Please install ggplot2")
      if (usePlot) {
        names(ggListToScreen) <- gsub(names(ggListToScreen), pattern = " |(\\\n)|[[:punct:]]", replacement = "_")
        Plot(ggListToScreen, addTo = gg$labels$title)
      } else {
        print(gg)
      }
    }

  }
  needSaveRaw <- any(grepl("raw", types))
  if (needSave || needSaveRaw) {
    if (missing(filename)) {
      filename <- tempfile(fileext = "")
    }
    isDefaultPath <- identical(eval(formals(Plots)$path), path)
    if (!is.null(simIsIn)) {
      if (is(path, "call"))
        path <- eval(path, envir = simIsIn)
    }

    if (isAbsolutePath(filename)) {
      path <- dirname(filename)
      filename <- basename(filename)
    }

    if (is(path, "character"))
      checkPath(path, create = TRUE)
  }

  if (needSaveRaw) {
    if (is(data, "Raster")) {
      rasterFilename <- file.path(path, paste0(filename, "_data.tif"))
      writeRaster(data, filename = rasterFilename, overwrite = TRUE)
      if (exists("sim", inherits = FALSE))
        sim@outputs <- outputsAppend(outputs = sim@outputs, endTime = end(sim),
                                     objectName = filePathSansExt(basename(rasterFilename)),
                                     file = rasterFilename, fun = "terra::writeRaster", args = NA,  ...)

    } else {
      rawFilename <- file.path(path, paste0(filename, "_data.qs"))
      qs::qsave(data, rawFilename)
      if (exists("sim", inherits = FALSE))
        sim@outputs <- outputsAppend(outputs = sim@outputs, endTime = end(sim),
                                     objectName = filePathSansExt(basename(rawFilename)),
                                     file = rawFilename, fun = "qs::qsave", args = NA,  ...)
    }

  }

  if (needSave) {
    if (is.null(simIsIn)) {
      if (is.call(path))
        path <- "."
      if (is.call(path))
        path <- "."
    }
    if (fnIsPlot) {
      baseSaveFormats <- intersect(baseClassesCanHandle, types)
      for (bsf in baseSaveFormats) {
        type <- get(bsf)
        theFilename <- file.path(path, paste0(filename, ".", bsf))
        do.call(type, modifyList2(list(theFilename), deviceArgs))
        # curDev <- dev.cur()
        clearPlot()
        plotted <- try(fn(data, ...)) # if this fails, catch so it can be dev.off'd
        dev.off()
        if (!is(plotted, "try-error")) {
          if (exists("sim", inherits = FALSE)) {
            pkgAndFn <- .guessPkgFun(bsf)
            sim@outputs <- outputsAppend(outputs = sim@outputs, endTime = end(sim),
                                         objectName = filePathSansExt(basename(theFilename)),
                                         file = theFilename, fun = pkgAndFn, args = NA,  ...)
          }
          message("Saved figure to: ", theFilename)
        }

      }
    } else {
      ggSaveFormats <- intersect(ggplotClassesCanHandle, types)
      for (ggsf in ggSaveFormats) {
        theFilename <- file.path(path, paste0(filename, ".", ggsf))
        if (!requireNamespace("ggplot2")) stop("To save gg objects, need ggplot2 installed")
        args <- list(plot = gg,
                     filename = theFilename)
        if (length(ggsaveArgs)) {
          args <- modifyList2(args, ggsaveArgs)
        }
        do.call(ggplot2::ggsave, args = args)
        if (exists("sim", inherits = FALSE))
          sim@outputs <- outputsAppend(outputs = sim@outputs, endTime = end(sim),
                                       objectName = filePathSansExt(basename(theFilename)),
                                       file = theFilename, fun = "ggplot2::ggsave", args = NA,  ...)
        message("Saved figure to: ", theFilename)
      }
    }

    if (any(grepl("object", types))) {
      filename11 <- file.path(path, paste0(filename, "_gg.qs"))
      qs::qsave(gg, file = filename11)
      if (exists("sim", inherits = FALSE))
        sim@outputs <- outputsAppend(outputs = sim@outputs, endTime = end(sim),
                                     objectName = filePathSansExt(basename(filename11)),
                                     file = filename11, fun = "qs::qsave", args = NA,  ...)
    }

  }
  if (exists("sim", inherits = FALSE))
    assign("sim", sim, envir = simIsIn)
  return(invisible(NULL))
}

outputsAppend <- function(outputs, endTime, objectName, file, fun, args, ...) {
  outs <- .fillOutputRows(data.frame(objectName = objectName, file = file, fun = fun,
                                     saved = TRUE, arguments = I(args)),
                          endTime = endTime)
  if (!is(outputs[["arguments"]], "AsIs"))
    outputs[["arguments"]] <- I(outputs[["arguments"]])
  rbindlist(list(outputs, outs), use.names = TRUE, fill = TRUE)
}

#' Test whether there should be any plotting from `.plot` parameter
#'
#' This will do all the various tests needed to determine whether
#' plotting of one sort or another will occur.
#' Testing any of the types as listed in [Plots()] argument `types`.
#' Only the first 3 letters of the type are required.
#'
#' @param .plots Usually will be the `P(sim)$.plots` is used within
#'   a module.
#'
#' @export
anyPlotting <- function(.plots) {
  needSaveRaw <- any(grepl("raw", .plots))
  ggplotClassesCanHandleBar <- paste(ggplotClassesCanHandle, collapse = "|")
  needSave <- any(grepl(paste(ggplotClassesCanHandleBar, "|obj"), .plots))
  needScreen <- any(grepl("scr", .plots))

  needSaveRaw || needSave || needScreen
}

ggplotClassesCanHandle <- c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
baseClassesCanHandle <- c("pdf", "jpeg", "png", "tiff", "bmp")

filePathSansExt <- getFromNamespace("filePathSansExt", ns = "reproducible")

#' Guess package of a function
#'
#' @param bsf character. A function name
#'
#' @return character. The package and function name as "pkg::bsf"

.guessPkgFun <- function(bsf) {
  pkgName <- eval(parse(text = paste0("environmentName(environment(", bsf, "))")))
  return(paste0(pkgName, "::", bsf))
}
