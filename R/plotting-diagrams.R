utils::globalVariables(c(".", "moduleName"))

################################################################################
#' `ganttStatus`
#'
#' Internal function assign the "status" of each event to be passed to `DiagrammeR::mermaid()`
#' to make a Gantt chart representing the events in a completed simulation.
#' `init` events are set as "done"; `plot` events as "critical"; and all others as "active".
#'
#' @param eventType Character vector of events.
#'
#' @return A character vector.
#'
#' @include simList-accessors.R
#' @keywords internal
#' @rdname ganttStatus
#'
#' @author Alex Chubaty
#'
setGeneric("ganttStatus", function(eventType) {
  standardGeneric("ganttStatus")
})

#' @rdname ganttStatus
setMethod("ganttStatus",
          signature(eventType = "character"),
          definition = function(eventType) {
            status <- lapply(eventType, function(x) {
              if (x == "init") {
                "done"
              } else if (x == "plot") {
                "crit"
              } else {
                "active"
              }
            })
            return(unlist(status))
})

################################################################################
#' sim2gantt
#'
#' Internal function to convert the completed events list of a `simList`
#' object to a list of `data.frame`s suitable to pass to a call to
#' `DiagrammeR::mermaid` to make a Gantt chart representing the
#' events in a completed simulation.
#'
#' @param sim  A `simList` object (typically corresponding to a
#'             completed simulation).
#'
#' @param n    The number of most recently completed events to plot.
#'
#' @param startDate  A character representation of date in `YYYY-MM-DD` format.
#'
#' @param width  Numeric. Passed to determine scale of vertical bars.
#'
#' @return A list of data.frames
#'
#' @author Alex Chubaty
#' @include simList-accessors.R
#' @keywords internal
#' @rdname sim2gantt
#'
setGeneric(".sim2gantt", function(sim, n, startDate, width) {
  standardGeneric(".sim2gantt")
})

#' @rdname sim2gantt
#' @importFrom utils tail
setMethod(
  ".sim2gantt",
  signature(sim = "simList", n = "numeric", startDate = "character", width = "numeric"),
  definition = function(sim, n, startDate, width) {
    DT <- tail(completed(sim), n)
    modules <- unique(DT$moduleName)
    width <- 4500 / as.numeric(width) # fixed at 3 days

    # simulation timestep in 'days'
    ts <- sim@simtimes[["timeunit"]] |>
      inSeconds(envir = sim@.xData) |>
      convertTimeunit("day", envir = sim@.xData) |>
      as.numeric()

    out <- lapply(modules, function(x) {
      data.frame(
        task = DT[moduleName == x]$eventType,
        status = ganttStatus(DT[moduleName == x]$eventType),
        pos = paste0(x, seq_len(nrow(DT[moduleName == x]))),
        start = as.Date(
          DT[moduleName == x]$eventTime * ts, origin = startDate
        ),
        end = as.Date(
          DT[moduleName == x]$eventTime * ts + width, origin = startDate
        )
      )
    })
    names(out) <- modules
    return(out)
})

################################################################################
#' Simulation event diagram
#'
#' Create a Gantt Chart representing the events in a completed simulation.
#' This event diagram is constructed using the completed event list
#' To change the number of events shown, provide an `n` argument.
#'
#' Simulation time is presented on the x-axis, starting at date `startDate`.
#' Each module appears in a colour-coded row, within which each event for that
#' module is displayed corresponding to the sequence of events for that module.
#' Note that only the start time of the event is meaningful is these figures:
#' the width of the bar associated with a particular module's event DOES NOT
#' correspond to an event's "duration".
#'
#' Based on this Stack Overflow answer: <https://stackoverflow.com/a/29999300/1380598>.
#'
#' @note
#' A red vertical line corresponding to the current date may appear on the figure.
#' This is useful for Gantt Charts generally but can be considered a 'bug' here.
#'
#' @param sim  A `simList` object (typically corresponding to a completed simulation).
#'
#' @param n    The number of most recently completed events to plot.
#'
#' @param startDate  A character representation of date in `YYYY-MM-DD` format.
#'
#' @param ...  Additional arguments passed to `mermaid`.
#'             Useful for specifying `height` and `width`.
#'
#' @return Plots an event diagram as Gantt Chart, invisibly returning a `mermaid` object.
#'
#' @seealso `DiagrammeR::mermaid`.
#'
#' @include simList-accessors.R
#' @export
#' @rdname eventDiagram
#'
#' @author Alex Chubaty
#'
setGeneric("eventDiagram", function(sim, n, startDate, ...) {
  standardGeneric("eventDiagram")
})

#' @export
#' @rdname eventDiagram
setMethod(
  "eventDiagram",
  signature(sim = "simList", n = "numeric", startDate = "character"),
  definition = function(sim, n, startDate, ...) {
    ## get automatic scaling of vertical bars in Gantt chart
    needInstall("DiagrammeR", minVersion = "0.8.2",
                messageStart = "Please install DiagrammeR: ")
    dots <- list(...)
    dots$width <- if (any(grepl(pattern = "width", names(dots)))) {
      as.numeric(dots$width)
    } else {
      1000
    }
    ll <- .sim2gantt(sim, n, startDate, dots$width)

    ## remove progress bar events
    ll <- ll[names(ll) != "progress"]

    if (length(ll)) {
      ## estimate the height of the diagram
      dots$height <- if (any(grepl(pattern = "height", names(dots)))) {
        as.numeric(dots$height)
      } else {
        sapply(ll, NROW) |> sum() |> (\(x) x * 26L)()
      }

      diagram <- paste0(
        ## mermaid "header"
        "gantt", "\n",
        "dateFormat  YYYY-MM-DD", "\n",
        "title SpaDES event diagram", "\n",

        ## mermaid "body"
        paste("section ", names(ll), "\n", lapply(ll, function(df) {
          paste0(df$task, ":", df$status, ",", df$pos, ",",
                 df$start, ",", df$end, collapse = "\n")
        }), collapse = "\n"), "\n"
      )
      do.call(DiagrammeR::mermaid, args = append(diagram, dots))
    } else {
      stop("Unable to create eventDiagram for a simulation that hasn't been run.\n",
           "Run your simulation using `mySim <- spades(mySim)` and try again.")
    }
})

#' @export
#' @rdname eventDiagram
setMethod(
  "eventDiagram",
  signature(sim = "simList", n = "missing", startDate = "character"),
  definition = function(sim, startDate, ...) {
    eventDiagram(sim = sim, n = NROW(completed(sim)), startDate = startDate, ...)
})

#' @export
#' @rdname eventDiagram
setMethod(
  "eventDiagram",
  signature(sim = "simList", n = "missing", startDate = "missing"),
  definition = function(sim, startDate, ...) {
    d <- as.Date(start(sim), format(Sys.time(), "%Y-%m-%d")) |> as.character()
    eventDiagram(sim = sim, n = NROW(completed(sim)), startDate = d, ...)
})

#' Simulation object dependency diagram
#'
#' Create a sequence diagram illustrating the data object dependencies of a
#' simulation. Offers a more detailed view of specific objects than does
#' plotting the `depsEdgeList` directly with [moduleDiagram()].
#'
#' @param sim  A `simList` object (typically corresponding to a
#'             completed simulation).
#'
#' @param ...  Additional arguments passed to `DiagrammeR::mermaid`.
#'             Useful for specifying `height` and `width`.
#'
#' @return Plots a sequence diagram, invisibly returning a `DiagrammeR::mermaid` object.
#'
#' @seealso `DiagrammeR::mermaid`.
#'
#' @include simList-accessors.R
#' @export
#' @rdname objectDiagram
#'
#' @author Alex Chubaty
#' @examples
#' \donttest{
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   sim <- simInit()
#'   objectDiagram(sim)
#'   # if there are lots of objects, may need to increase width and/or height
#'   objectDiagram(sim, height = 3000, width = 3000)
#' }
#' }
#'
setGeneric("objectDiagram", function(sim, ...) {
  standardGeneric("objectDiagram")
})

#' @export
#' @rdname objectDiagram
setMethod(
  "objectDiagram",
  signature(sim = "simList"),
  definition = function(sim, ...) {
    dt <- depsEdgeList(sim, FALSE)
    needInstall("DiagrammeR", minVersion = "0.8.2",
                messageStart = "Please install DiagrammeR: ")
    DiagrammeR::mermaid(...,
      paste0(
        ## mermaid "header"
        "sequenceDiagram", "\n",

        ## mermaid "body"
        paste(dt$from, "->>", dt$to, ":", dt$objName, collapse = "\n"),
        "\n"
      )
    )
})

#' Simulation module dependency diagram
#'
#' Create a network diagram illustrating the simplified module dependencies of a
#' simulation. Offers a less detailed view of specific objects than does
#' plotting the `depsEdgeList` directly with [objectDiagram()].
#'
#' @param sim  A `simList` object (typically corresponding to a
#'             completed simulation).
#'
#' @param type  Character string, either `"rgl"` for `igraph::rglplot`
#' or `"tk"` for `igraph::tkplot`, `"Plot"` to use `quickPlot::Plot()`
#' or `"plot"` to use `base::plot()`, the default.
#'
#' @param showParents Logical. If TRUE, then any children that are grouped into parent
#'                    modules will be grouped together by coloured blobs. Internally,
#'                    this is calling [moduleGraph()]. Default `FALSE`.
#'
#'
#' @param ...  Additional arguments passed to plotting function specified by `type`.
#'
#' @return invoked for its side effect of plotting the module dependency diagram.
#'
#' @seealso [igraph::igraph], [moduleGraph()] for a version that accounts for
#' parent and children module structure.
#'
#' @include simList-accessors.R
#' @export
#' @rdname moduleDiagram
#'
#' @author Alex Chubaty
#' @examples
#' \donttest{
#' if (requireNamespace("SpaDES.tools", quietly = TRUE) &&
#'       packageVersion("SpaDES.tools") > "3.0.0") {
#' library(igraph)
#' times <- list(start = 0, end = 6, "month")
#' parameters <- list(
#'   .globals = list(stackName = "landscape"),
#'   caribouMovement = list(
#'     .saveObjects = "caribou",
#'     .saveInitialTime = 1, .saveInterval = 1
#'   ),
#'   randomLandscapes = list(.plotInitialTime = NA, nx = 20, ny = 20))
#'
#' modules <- list("randomLandscapes", "caribouMovement")
#' paths <- list(
#'   modulePath = getSampleModules(tempdir())
#' )
#'
#' # Set some options so example runs faster
#' opts <- options(spades.moduleCodeChecks = FALSE, spades.loadReqdPkgs = FALSE)
#' sim <- simInit(times = times, params = parameters, modules = modules,
#'                paths = paths)
#' options(opts)
#' moduleDiagram(sim)
#' # Can also use default base::plot
#' modDia <- depsGraph(sim, plot = TRUE)
#' # See ?plot.igraph
#' plot(modDia, layout = layout_as_star)
#'
#' # Or for more control - here, change the label "_INPUT_" to "DATA"
#' edgeList <- depsEdgeList(sim)
#' edgeList <- edgeList[, list(from, to)]
#' edgeList[from == "_INPUT_", from := "Data"]
#' edgeList[to == "_INPUT_", to := "Data"]
#' edgeList <- unique(edgeList)
#' ig <- graph_from_data_frame(edgeList[, list(from, to)])
#' plot(ig)
#' }
#' }
#'
setGeneric("moduleDiagram", function(sim, type, showParents = TRUE, ...) {
  standardGeneric("moduleDiagram")
})

#' @export
#' @rdname moduleDiagram
setMethod(
  "moduleDiagram",
  signature = c(sim = "simList", type = "character", showParents = "logical"),
  definition = function(sim, type = "plot", showParents = TRUE,  ...) {
    if (type == "rgl") {
      rglplot(depsGraph(sim, TRUE), ...)
    } else if (type == "tk") {
      tkplot(depsGraph(sim, TRUE), ...)
    } else {
      if (grep("plot", type, ignore.case = TRUE))
        moduleDiagram(sim, showParents = showParents, plot = type, ... )
      else
        stop("type must be one of 'rgl', 'tk', 'Plot' or 'plot'")
    }
})

#' @export
#' @rdname moduleDiagram
setMethod(
  "moduleDiagram",
  signature = c(sim = "simList"),
  definition = function(sim, ...) {
    modDia <- depsGraph(sim, TRUE)
    dots <- list(...)
    nDots <- names(dots)
    if (missing(showParents)) showParents <- FALSE
    if (showParents) {
      moduleGraph(sim = sim, ...)
    } else {
      ## need to remove dots ... not as easy as hoped -- define new function which removes
      PlotRemovingDots <- function(modDia, plotFn, axes, ...,
                                   vertex.color,
                                   vertex.size,
                                   vertex.size2,
                                   vertex.shape,
                                   vertex.label.cex,
                                   vertex.label.family,
                                   layout,
                                   rescale,
                                   xlim, ylim, asp) {
        namesModDia <- names(V(modDia))
        vcol <- if (!("vertex.color" %in% nDots)) {
          sapply(namesModDia, function(v) {
            ifelse(v == "_INPUT_", "orange", "lightblue")
          })
        } else {
          "lightblue"
        }

        vertexSize <- if (!("vertex.size" %in% nDots)) {
          c(nchar(namesModDia)^0.8 * 10) # use exponent to stretch out, and multiply to make bigger
        } else {
          dots$vertex.size
        }

        vertexSize2 <- if (!("vertex.size2" %in% nDots)) {
          25
        } else {
          dots$vertex.size2
        }

        vertexLabelCex <- if (!("vertex.label.cex" %in% nDots)) {
          1.7
        } else {
          dots$vertex.label.cex
        }

        vertexLabelFamily <- if (!("vertex.label.family" %in% nDots)) {
          "sans"
        } else {
          dots$vertex.label.family
        }

        vertexShape <- if (!("vertex.shape" %in% nDots)) {
          "rectangle"
        } else {
          dots$vertex.shape
        }

        layout2 <- if (!("layout" %in% nDots)) {
          if ("_INPUT_" %in% V(modDia)) {
            igraph::layout_as_star(modDia, center = "_INPUT_")
          } else {
            igraph::layout_in_circle(modDia)
          }
        } else {
          dots$layout
        }

        rescale2 <- if (!("rescale" %in% nDots)) FALSE else dots$rescale
        xlim2 <- if (!("xlim" %in% nDots)) c(-1.7, 1.7) else dots$xlim
        ylim2 <- if (!("ylim" %in% nDots)) c(-1.1, 1.1) else dots$ylim
        asp2 <-  if (!("asp" %in% nDots)) 0 else dots$asp

        notPlot <- isTRUE(dots$plot)
        if (!isTRUE(notPlot)) {
          plotTry <- try(Plot(modDia, plotFn = "plot", axes = FALSE,
               vertex.color = vcol,
               vertex.size = vertexSize,
               vertex.size2 = vertexSize2,
               vertex.shape = vertexShape,
               vertex.label.cex = vertexLabelCex,
               vertex.label.family = vertexLabelFamily,
               layout = layout2,
               rescale = rescale2,
               xlim = xlim2, ylim = ylim2, asp = asp2, ...), silent = TRUE)
          notPlot <- is(plotTry, "try-error")
          if (notPlot)
            message("Plot encountered an error; trying base::plot")
        }
        if (notPlot)
          plot(modDia, axes = FALSE,
               vertex.color = vcol,
               vertex.size = vertexSize,
               vertex.size2 = vertexSize2,
               vertex.shape = vertexShape,
               vertex.label.cex = vertexLabelCex,
               vertex.label.family = vertexLabelFamily,
               layout = layout2,
               rescale = rescale2,
               xlim = xlim2, ylim = ylim2, asp = asp2, ...)

      }

      if ("title" %in% nDots) {
        PlotRemovingDots(modDia = modDia, plotFn = "plot", axes = FALSE, ...)
      } else {
        PlotRemovingDots(modDia = modDia, plotFn = "plot", axes = FALSE,
                         title = "Module Diagram", ...)
      }
    }
})

#' Build a module dependency graph
#'
#' This is still experimental, but this will show the hierarchical structure of
#' parent and children modules and return a list with an `igraph` object
#' and an `igraph` communities object, showing the groups.
#' Currently only tested with relatively simple structures.
#'
#' @inheritParams depsEdgeList
#'
#' @param ... Arguments passed to `Plot`
#'
#' @return A list with 2 elements, an [igraph::igraph] object and an `igraph` communities object.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom data.table rbindlist
#' @include simList-class.R
#' @rdname moduleGraph
#' @seealso [moduleDiagram()]
#'
setGeneric("moduleGraph", function(sim, plot, ...) {
  standardGeneric("moduleGraph")
})

#' @export
#' @rdname moduleGraph
setMethod(
  "moduleGraph",
  signature(sim = "simList", plot = "logical"),
  definition = function(sim, plot, ...) {
    msgMissingGLPK <- paste("GLPK not found on this system.\n",
                            "igraph is used internally and requires a GLPK installation.\n")
    msgInstallDarwin <- paste("It can be installed using, e.g., `brew install glpk`.\n")
    msgInstallLinux <- paste("It can be installed using, e.g., `apt install libglpk-dev`.\n")
    msgReinstallIgraph <- paste("If GLPK is installed you should reinstall igraph from source using:\n",
                                "`install.packages('igraph', type = 'source')`\n",
                                "For more info see https://github.com/igraph/rigraph/issues/273.")

    if (Sys.which("glpsol") == "") {
      if (Sys.info()[['sysname']] == "Darwin") {
        message(msgMissingGLPK, msgInstallDarwin, msgReinstallIgraph)
      } else if (Sys.info()[['sysname']] == "Linux") {
        message(msgMissingGLPK, msgInstallLinux, msgReinstallIgraph)
      }
      return(invisible(NULL))
    } else {
      mg <- attr(sim@modules, "modulesGraph")
      mg[["from"]] <- basename(mg[["from"]])
      mg[["to"]] <- basename(mg[["to"]])
      parents <- unique(mg[, "from"]) |> basename()

      deps <- depsEdgeList(sim)[, list(from, to)]
      el <- rbind(mg, deps)

      ## This is just for the dummy case of having no object dependencies
      if (NROW(deps) == 0) deps <- mg

      grph <- graph_from_data_frame(el, directed = TRUE)
      grps <- try(cluster_optimal(grph))

      if (is(grps, "try-error")) {
        msgIgraphNoGLPK <- paste("Unable to create moduleGraph.",
                                 "Likely reason: igraph not compiled with GLPK support.\n")
        message(msgIgraphNoGLPK, msgReinstallIgraph) ## avoid error for tests
        return(invisible(NULL))
      } else {
        membership <- as.numeric(as.factor(mg[match(names(V(grph)), mg[, 2]), 1]))
        membership[is.na(membership)] <- 1
        membership[which(names(V(grph)) == "_INPUT_")] <- max(membership, na.rm = TRUE) + 1
        grps$membership <- membership

        el1 <- lapply(parents, function(par) data.frame(el["from" == par]))
        el1 <- rbindlist(el1)
        e <- apply(el1, 1, paste, collapse = "|")
        e <- edges(e)

        if (plot) {
          vs <- c(15, 0)[(names(V(grph)) %in% parents) + 1]
          dots <- list(...)
          if ("title" %in% names(dots)) {
            Plot(grps, grph - e, vertex.size = vs, plotFn = "plot", axes = FALSE, ...)
          } else {
            Plot(grps, grph - e, vertex.size = vs, plotFn = "plot", axes = FALSE,
                 title = "Module Graph", ...)
          }
        }
        return(invisible(list(graph = grph, communities = grps)))
      }
    }
})

#' @export
#' @rdname moduleGraph
setMethod("moduleGraph",
          signature(sim = "simList", plot = "missing"),
          definition = function(sim, ...) {
            return(moduleGraph(sim, TRUE, ...))
})
