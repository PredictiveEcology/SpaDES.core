
#' Plot wrapper intended for use in a SpaDES module
#'
#' THIS IS STILL EXPERIMENTAL and could change in the next release.
#'
#' This offers up to 4 different actions for a given plot:
#'     \itemize{
#'       \item To screen device
#'       \item To disk as raw data
#'       \item To disk as a saved plot object
#'       \item To disk as a png or other "image" file, like pdf
#'     }
#' To turn off plotting both to screen and disk, set both
#' \code{.plotInititalTime = NA} and \code{.plotsToDisk = NA} or any other
#' value that will not trigger a TRUE with a \code{grepl} with the \code{types}
#' argument (e.g., \code{""} will omit all saving).
#'
#' @export
#' @param data An arbitrary data object. It should be used inside the \code{Plots}
#'   function, and should contain all the data required for the inner plotting
#' @param fn An arbitrary plotting function.
#' @param filename A name that will be the base for the files that will be saved. If
#'   a user providees this as an absolute path, it will override the \code{path}
#'   argument.
#' @param types Character vector, zero or more of types. See below.
#' @param path Currently a single path for the saved objects on disk. If \code{filename}
#'   is supplied as an absolute path, \code{path} will be set to \code{dirname(filename)},
#'   overriding this argument value.
#' @param .plotInitialTime A numeric. If \code{NA} then no visual on screen. Anything
#'   else will have visuals plotted to screen device. This is here for backwards
#'   compatibility. A developer should set in the module to the intended initial
#'   plot time and leave it.
#' @param ... Anything needed by \code{fn}
#'
#' @importFrom qs qsave
#' @importFrom quickPlot whereInStack Plot
#'
#' @details
#'
#' \itemize{
#'   \item \code{type}
#'     \itemize{
#'       \item \code{"object"} -- Will save the plot object, e.g., ggplot object
#'       \item \code{"raw"} -- Will save the raw data prior to plotting, e.g.,
#'                           the data argument
#'       \item \code{"png"} -- or any other type saveble with \code{ggsave}
#'     }
#' }
#'
#' @examples
#'
#' \dontrun{
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
#'  } # end of dontrun
Plots <- function(data, fn, filename,
                  types = quote(params(sim)[[currentModule(sim)]]$.plotsToDisk),
                  path = quote(file.path(outputPath(sim), "figures")),
                  .plotInitialTime = quote(params(sim)[[currentModule(sim)]]$.plotInitialTime),
                  ...) {
  if (any(is(types, "call") || is(path, "call") || is(.plotInitialTime, "call"))){
    simIsIn <- parent.frame() # try for simplicity sake... though the whereInStack would get this too
    if (!exists("sim", simIsIn))
      simIsIn <- whereInStack("sim")
  }

  if (is(types, "call"))
    types = eval(types, envir = simIsIn)
  if (is(path, "call"))
    path = eval(path, envir = simIsIn)
  if (is(.plotInitialTime, "call"))
    .plotInitialTime = eval(.plotInitialTime, envir = simIsIn)

  ggplotClassesCanHandle <- c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
  ggplotClassesCanHandleBar <- paste(ggplotClassesCanHandle, collapse = "|")
  needSave <- any(grepl(paste(ggplotClassesCanHandleBar, "|object"), types))
  needScreen <- !is.na(.plotInitialTime) && any(grepl("screen", types))
  if (needScreen || needSave) {
    gg <- fn(data, ...)
  }

  if (needScreen) {
    if (is(gg, "gg"))
      if (!requireNamespace("ggplot2")) stop("Please install ggplot2")
    Plot(gg)
  }
  needSaveRaw <- any(grepl("raw", types))
  if (needSave || needSaveRaw) {
    if (isAbsolutePath(filename)) {
      message("filename is an absolute path; overriding path")
      path <- dirname(filename)
      filename <- basename(filename)
    }
    checkPath(path, create = TRUE)
  }

  if (needSaveRaw)
    qs::qsave(data, file.path(path, paste0(filename, "_data.qs")))
  if (needSave) {
    ggSaveFormats <- intersect(ggplotClassesCanHandle, types)
    for (ggsf in ggSaveFormats) {
      if (!requireNamespace("ggplot2")) stop("To save gg objects, need ggplot2 installed")
        ggsave(plot = gg, filename = file.path(path, paste0(filename, ".", ggsf)))
    }

    if (any(grepl("object", types)))
      qs::qsave(gg, file = file.path(path, paste0(filename, "_gg.qs")))
  }
}
