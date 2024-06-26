if (!isGeneric("Plot")) {
  setGeneric("Plot", function(..., new, addTo, gp, gpText, gpAxis, axes,
                              speedup, size, cols, zoomExtent, visualSqueeze,
                              legend, legendRange, legendText, pch, title,
                              na.color, zero.color, length) {
    standardGeneric("Plot")
  })
}

#' Plot method for `simList` objects
#'
#' Extends `quickPlot::Plot` for `simList` objects.
#'
#' See `quickPlot::Plot`.
#' This method strips out stuff from a `simList` class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems, or machines.
#' This will likely still not allow identical digest results across R versions.
#'
#' @inheritParams quickPlot::Plot
#'
#' @return invoked for side effect of plotting
#'
#' @exportMethod Plot
#' @export
#' @importFrom quickPlot gpar Plot
#' @importMethodsFrom quickPlot Plot
#' @include simList-class.R
#' @rdname Plot
#' @seealso `quickPlot::Plot`
#'
setMethod(
  "Plot",
  signature("simList"),
  definition = function(..., new, addTo, gp, gpText, gpAxis, axes,
                        speedup, size, cols, zoomExtent, visualSqueeze,
                        legend, legendRange, legendText, pch, title,
                        na.color, zero.color, length) {
    # Section 1 - extract object names, and determine which ones need plotting,
    # which ones need replotting etc.
    sim <- list(...)[[1]]
    plotList <- ls(sim@.xData, all.names = TRUE)
    plotables <- sapply(plotList, function(x)
      is(get(x, envir = sim@.xData), ".quickPlottables"))
    if (any(plotables)) {
      plotObjects <- mget(plotList[plotables], sim@.xData) |>
        append(list(env = sim@.xData))
      Plot(plotObjects)
    }
})

if (!isGeneric(".parseElems")) {
  setGeneric(".parseElems", function(object, objects,
                                     length = 1e6,
                                     algo = "xxhash64") {
    standardGeneric(".parseElems")
  })
}

#' `.parseElems` for `simList` class objects
#'
#' See [quickPlot::.parseElems()].
#'
#' @inheritParams quickPlot::.parseElems
#'
#' @return An object, parsed from a character string and an environment.
#'
#' @exportMethod .parseElems
#' @export
#' @importFrom quickPlot .parseElems
#' @importMethodsFrom quickPlot .parseElems
#' @include simList-class.R
#' @rdname parseElems
#' @seealso [quickPlot::.parseElems]
setMethod(
  ".parseElems",
  signature = "simList",
  definition = function(tmp, elems, envir) {
    useElem <- 1
    # If the user is passing a sub-element to say a Raster Stack
    if (length(rev(elems)[-1]) > 1) {
      # Only RasterStack implemented yet
      if (is(get(deparse(rev(elems)[[2]]), envir = tmp@.xData), "RasterStack")) {
        useElem <- 2
      }
    }
    out <- tryCatch(
      eval(parse(text = deparse(elems[[useElem]])), envir = tmp@.xData),
      error = function(x) eval(parse(text = deparse(elems[[useElem]])), envir = envir)
    )
    return(out)
})
