utils::globalVariables(c(
  ".", "from", "fromOrd", "i.module", "i.objectClass", "module", "module.x", "module.y",
  "name", "objectClass", "objectName", "other", "sourceURL", "to", "toOrd"
))

# register the S3 `igraph` class for use with S4 methods.
setOldClass("igraph")
selectMethod("show", "igraph")

################################################################################
#' Build edge list for module dependency graph
#'
#' @param sim A `simList` object.
#'
#' @param plot  Logical indicating whether the edgelist (and subsequent graph)
#'              will be used for plotting. If `TRUE`, duplicated rows
#'              (i.e., multiple object dependencies between modules) are removed
#'              so that only a single arrow is drawn connecting the modules.
#'              Default is `FALSE`.
#'
#' @return A `data.table` whose first two columns give a list of edges
#'          and remaining columns the attributes of the dependency objects
#'          (object name, class, etc.).
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table := data.table rbindlist setkeyv setorder
#' @include simList-class.R
#' @rdname depsEdgeList
#'
setGeneric("depsEdgeList", function(sim, plot) {
  standardGeneric("depsEdgeList")
})

#' @rdname depsEdgeList
setMethod(
  "depsEdgeList",
  signature(sim = "simList", plot = "logical"),
  definition = function(sim, plot) {
    deps <- sim@depends
    DT <- .depsEdgeList(deps, plot)
    correctOrd <- unlist(sim@modules, use.names = FALSE)
    DT[, fromOrd := factor(from, levels = correctOrd)]
    DT[, toOrd := factor(to, levels = correctOrd)]
    DT <- setorderv(DT, c("fromOrd", "toOrd"))
    return(DT)
})

#' @rdname depsEdgeList
setMethod("depsEdgeList",
          signature(sim = "simList", plot = "missing"),
          definition = function(sim, plot) {
            depsEdgeList(sim, plot = FALSE)
})

#' @importFrom data.table as.data.table data.table rbindlist setkeyv setorder
.depsEdgeList <- function(deps, plot) {
  sim.in <- sim.out <- data.table(objectName = character(0),
                                  objectClass = character(0),
                                  module = character(0))
  lapply(deps@dependencies, function(x) {
    if (!is.null(x)) {
      z.in <- as.data.table(x@inputObjects)[, .(objectName, objectClass)]
      if (NROW(z.in) == 0)
        z.in <- as.data.table(list(objectName = ".dummyIn", objectClass = NA))
      z.out <- as.data.table(x@outputObjects)[, .(objectName, objectClass)]
      if (NROW(z.out) == 0)
        z.in <- as.data.table(list(objectName = ".dummyOut", objectClass = NA))
      z.in$module <- z.out$module <- x@name
      if (!all(is.na(z.in[, objectName]), is.na(z.in[, objectClass]))) {
        sim.in <<- rbindlist(list(sim.in, z.in), use.names = TRUE)
      }
      if (!all(is.na(z.out[, 1:2]), is.na(z.out[, objectClass]))) {
        sim.out <<- rbindlist(list(sim.out, z.out), use.names = TRUE)
      }
    }
    return(invisible(NULL)) # return from the lapply
  })

  setkeyv(sim.in, "objectName")
  setkeyv(sim.out, "objectName")

  if ((nrow(sim.in)) && (nrow(sim.out))) {
    dx <- sim.out[sim.in, nomatch = NA_character_, allow.cartesian = TRUE]
    dx[is.na(module), module := "_INPUT_"]
    DT <- dx[, list(from = module, to = i.module,
                    objName = objectName, objClass = i.objectClass)]

    if (plot) DT <- DT[!duplicated(DT[, 1:2, with = FALSE]), ]
  } else {
    DT <- data.table(from = character(0), to = character(0),
                     objName = character(0), objClass = character(0))
  }
  setorder(DT, "from", "to", "objName")
}


################################################################################
#' Build a module dependency graph
#'
#' @inheritParams depsEdgeList
#'
#' @return An `igraph()` object.
#'
#' @author Alex Chubaty
#' @export
#' @include simList-class.R
#' @rdname depsGraph
#'
setGeneric("depsGraph", function(sim, plot) {
  standardGeneric("depsGraph")
})

#' @export
#' @rdname depsGraph
setMethod("depsGraph",
          signature(sim = "simList", plot = "logical"),
          definition = function(sim, plot) {
            if (plot) {
              el <- depsEdgeList(sim, plot)
            } else {
              el <- depsEdgeList(sim, plot) |> .depsPruneEdges()
            }
            m <- modules(sim) |> unlist() # modules(sim) doesn't return hidden modules
            v <- unique(c(el$to, el$from, m)) # so no need to remove them
            return(graph_from_data_frame(el, vertices = v, directed = TRUE))
})

#' @export
#' @rdname depsGraph
setMethod("depsGraph",
          signature(sim = "simList", plot = "missing"),
          definition = function(sim) {
            return(depsGraph(sim, FALSE))
})

################################################################################
#' Prune edges to remove cycles in module dependencies
#'
#' Internal function.
#' Attempts to identify cycles in the dependency graph and remove edges representing
#' object dependencies which are provided by other modules in the simulation.
#'
#' @param simEdgeList An edge list (`data.table`) produced by [depsEdgeList()].
#'
#' @return An updated edge list object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table as.data.table data.table rbindlist shift
#' @importFrom stats na.omit
#' @include simList-class.R
#' @keywords internal
#' @rdname depsPruneEdges
#'
setGeneric(".depsPruneEdges", function(simEdgeList) {
  standardGeneric(".depsPruneEdges")
})

#' @rdname depsPruneEdges
setMethod(
  ".depsPruneEdges",
  signature(simEdgeList = "data.table"),
  definition = function(simEdgeList) {
    simGraph <- graph_from_data_frame(simEdgeList)
    M <- distances(simGraph, mode = "out")
    if (nrow(M) > 1) {
      pth <- data.table(from = character(0), to = character(0))
      for (row in 1L:(nrow(M) - 1L)) {
        for (col in (row + 1L):ncol(M)) {
          current <- M[row, col]
          partner <- M[col, row]
          if (all((current > 0), !is.infinite(current), (partner > 0),
                  !is.infinite(partner))) {
            pth1 <- shortest_paths(simGraph,
                                   from = rownames(M)[row],
                                   to = colnames(M)[col])$vpath[[1]]
            pth1 <- data.frame(from = rownames(M)[pth1],
                               to = rownames(M)[shift(match(names(pth1), rownames(M)), -1)],
                               stringsAsFactors = FALSE) |>
                    na.omit() |> as.data.table()

            pth2 <- shortest_paths(simGraph,
                                   from = colnames(M)[col],
                                   to = rownames(M)[row])$vpath[[1]]
            pth2 <- data.frame(from = rownames(M)[pth2],
                               to = rownames(M)[shift(match(names(pth2), rownames(M)), -1)],
                               stringsAsFactors = FALSE) |>
                    na.omit() |> as.data.table()

            pth <- rbindlist(list(pth, rbindlist(list(pth1, pth2))))
          }
        }
      }
      pth <- unique(pth)
      pth <- simEdgeList[pth, on = c("from", "to")]

      ## what is not provided in modules, but needed
      missingObjects <- pth[!simEdgeList[from != to], on = c("from", "to")]
      if (nrow(missingObjects)) {
        warning("Problem resolving the module dependencies:\n",
                paste(missingObjects), collapse = "\n")
      }

      ## what is provided in modules, and can be omitted from simEdgeList object
      newEdgeList <- simEdgeList[from != to][!pth, on = c("from", "to")]

    } else {
      newEdgeList <- simEdgeList
    }

    newEdgeList <- newEdgeList |> setorder("fromOrd", "toOrd", "objName")
    newEdgeList <- newEdgeList[, `:=`(fromOrd = as.character(fromOrd), toOrd = as.character(toOrd))]

    return(newEdgeList)
})

################################################################################
#' Determine module load order
#'
#' Internal function.
#' Checks module dependencies and attempts to ensure that cyclic dependencies
#' can be resolved, checking objects in the global environment, and finally,
#' attempts to determine the load order for modules in the simulation.
#'
#' Uses [igraph::topo_sort()] to try to find a load order satisfying
#' all module object dependencies.
#'
#' @param sim         A `simList` object.
#'
#' @param simGraph    An [igraph()] object produced by [depsGraph()].
#'
#' @return Character vector of module names, sorted in correct load order.
#'
#' @author Alex Chubaty
#' @export
#' @include simList-class.R
#' @keywords internal
#' @rdname depsLoadOrder
setGeneric(".depsLoadOrder", function(sim, simGraph) {
  standardGeneric(".depsLoadOrder")
})

#' @rdname depsLoadOrder
setMethod(".depsLoadOrder",
          signature(sim = "simList", simGraph = "igraph"),
          definition = function(sim, simGraph) {
            # only works if simGraph is acyclic!
            doTopoSort <- TRUE

            if (!is.null(sim@depends@dependencies[[1]])) {
              loadOrdersInMetaData <- Map(mod = sim@depends@dependencies, function(mod) {
                if (length(mod@loadOrder)) mod@loadOrder else NULL})
              loadOrdersInMetaData <- loadOrdersInMetaData[!vapply(loadOrdersInMetaData, is.null, FUN.VALUE = logical(1))]

              if (length(loadOrdersInMetaData)) {
                dt <- as.data.table(as_data_frame(simGraph))

                Map(lo = loadOrdersInMetaData, nam = names(loadOrdersInMetaData),
                    function(lo, nam) {
                      lapply(lo[["after"]], function(aft) {
                        a <- setDT(list(from = aft, to = nam, objName = .rndstr(1)))
                        dt <<- rbindlist(list(dt, a), fill = TRUE)
                      })
                      lapply(lo[["before"]], function(bef) {
                        a <- setDT(list(from = nam, to = bef, objName = .rndstr(1)))
                        dt <<- rbindlist(list(dt, a), fill = TRUE)
                      })
                    })
                simGraph2 <- graph_from_data_frame(dt)
                tsort <- try(topo_sort(simGraph2, "out"), silent = TRUE)
                if (exists("tsort", inherits = FALSE))
                  if (!is(tsort, "try-error")) {
                    doTopoSort <- FALSE
                    simGraph <- simGraph2
                  } else {
                    message("Could not automatically determine module order, even with `loadOrder` metadata; ",
                            "it may be wise to set the order manually and pass to `simInit(... loadOrder = xxx)`")
                  }
              }
            }
            if (doTopoSort) {
              tsort <- topo_sort(simGraph, "out")
            }

            # depsGrDF <- as.data.table(as_data_frame(simGraph))
            if (length(tsort)) {
              loadOrder <- names(simGraph[[tsort, ]]) %>% .[!(. %in% "_INPUT_" )]
            } else {
              modules <- unlist(sim@modules)
              if (length(sim@modules)) {
                loadOrder <- modules
              } else {
                loadOrder <- character()
              }
            }

            # cyclic ones are absent; the topo-sort above just puts them in randomly; this is bad
            # fromSet <- setdiff(unique(c(depsGrDF$from)), "_INPUT_")
            # toSet <- setdiff(unique(c(depsGrDF$to)), "_INPUT_")
            # toSet <- setdiff(toSet, fromSet)
            # doFirst <- loadOrder[loadOrder %in% fromSet]
            # doSecond <- loadOrder[loadOrder %in% toSet]
            # doThird <- setdiff(loadOrder, c(doFirst, doSecond))
            #
            # loadOrder <- c(doFirst, doSecond, doThird)

            # New -- loadOrder element in metadata
            # Map(lo = loadOrdersInMetaData, nam = names(loadOrdersInMetaData),
            #     function(lo, nam) {
            #       lapply(lo[["after"]], function(aft) {
            #         aftI <- grep(aft, loadOrder)
            #         namI <- grep(nam, loadOrder)
            #         if (aftI > namI) {
            #           loadOrder <<- loadOrder[-aftI]
            #           loadOrder <<- append(loadOrder, aft, after=namI)
            #           message("Reordering modules due to metadata: ", aft, " is being moved before ", nam)
            #         }
            #       })
            #       lapply(lo[["before"]], function(bef) {
            #         befI <- grep(bef, loadOrder)
            #         namI <- grep(nam, loadOrder)
            #         if (befI < namI) {
            #           loadOrder <<- append(loadOrder, bef, after=namI)
            #           loadOrder <<- loadOrder[-befI]
            #           message("Reordering modules due to metadata: ", bef, " is being moved after ", nam)
            #         }
            #       })
            #     })

            # make sure modules with no deps get added
            if (!all(sim@modules %in% loadOrder)) {
              ids <- which(sim@modules %in% loadOrder)
              noDeps <- unname(unlist(sim@modules)[-ids])
              loadOrder <- c(loadOrder, noDeps)
            }
            return(loadOrder)
})
