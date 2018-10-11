################################################################################
#' The \code{simList} class
#'
#' Contains the minimum components of a \code{SpaDES} simulation.
#' Various slot accessor methods (i.e., get and set functions) are provided
#' (see 'Accessor Methods' below).
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a discrete event simulation in a more modular fashion so
#' it's easier to add simulation components (i.e., "simulation modules").
#' We use S4 classes and methods, and use \code{\link{data.table}} instead of
#' \code{\link{data.frame}} to implement the event queue (because it is much
#' more efficient).
#'
#' @note The \code{simList} class extends the \code{.simList} superclass by adding
#' a slot \code{.envir} to store the simulation environment containing references
#' to simulation objects.
#' The \code{\link{simList_}} class extends the \code{.simList} superclass, by
#' adding a slot \code{.list} containing the simulation objects.
#' Thus, \code{simList} is identical to \code{simList_}, except that the former
#' uses an environment for objects and the latter uses a list.
#' The class \code{simList_} is only used internally.
#'
#' @slot modules    List of character names specifying which modules to load.
#'
#' @slot params     Named list of potentially other lists specifying simulation
#'                  parameters.
#'
#' @slot events     The list of scheduled events (i.e., event queue), as a
#'                  \code{data.table}. See 'Event Lists' for more information.
#'
#' @slot current    The current event, as a \code{data.table}.
#'                  See 'Event Lists' for more information..
#'
#' @slot completed  The list of completed events, as a \code{list}.
#'                  See 'Event Lists' for more information. It is kept
#'                  as a list of individual events for speed. The \code{completed}
#'                  method converts it to a sorted \code{data.table}.
#'
#' @slot depends    A \code{.simDeps} list of \code{\link{.moduleDeps}} objects
#'                  containing module object dependency information.
#'
#' @slot simtimes   List of numerical values describing the simulation start
#'                  and end times; as well as the current simulation time.
#'
#' @slot inputs     a \code{data.frame} or \code{data.table} of files and
#'                  metadata
#'
#' @slot outputs    a \code{data.frame}  or \code{data.table} of files and
#'                  metadata
#'
#' @slot paths      Named list of \code{modulePath}, \code{inputPath},
#'                  and \code{outputPath} paths. Partial matching is performed.
#'
#' @section Accessor Methods:
#'
#' Several slot (and sub-slot) accessor methods are provided for use, and
#' categorized into separate help pages:
#' \tabular{ll}{
#'   \code{\link{simList-accessors-envir}} \tab Simulation environment. \cr
#'   \code{\link{simList-accessors-events}} \tab Scheduled and completed events. \cr
#'   \code{\link{simList-accessors-inout}} \tab Passing data in to / out of simulations. \cr
#'   \code{\link{simList-accessors-modules}} \tab Modules loaded and used; module dependencies. \cr
#'   \code{\link{simList-accessors-objects}} \tab Accessing objects used in the simulation. \cr
#'   \code{\link{simList-accessors-params}} \tab Global and module-specific parameters. \cr
#'   \code{\link{simList-accessors-paths}} \tab File paths for modules, inputs, and outputs. \cr
#'   \code{\link{simList-accessors-times}} \tab Simulation times. \cr
#' }
#'
#' @section Event Lists:
#'
#' The main event list is a sorted data.table (keyed) on eventTime, and eventPriority.
#' The completed event list is an ordered list in the exact order that the events
#' were executed.
#' Each event is represented by a \code{\link{data.table}} row consisting of:
#' \tabular{ll}{
#'   \code{eventTime} \tab The time the event is to occur.\cr
#'   \code{moduleName} \tab The module from which the event is taken.\cr
#'   \code{eventType} \tab A character string for the programmer-defined event type.\cr
#'   \code{eventPriority} \tab The priority given to the event. \cr
#' }
#'
#' @aliases .simList
#' @rdname simList-class
#' @importFrom data.table as.data.table data.table
#' @include helpers.R misc-methods.R module-dependencies-class.R
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty and Eliot McIntire
#'
setClass(
  ".simList",
  contains = "environment",
  slots = list(
    modules = "list", params = "list", events = "list",#data.table",
    current = "list", #"data.table",
    completed = "list", depends = ".simDeps",
    simtimes = "list", inputs = "data.frame", outputs = "data.frame", paths = "list"
  ),
  validity = function(object) {
    # check for valid sim times
    if (is.na(object@simtimes$end)) {
      stop("simulation end time must be specified.")
    } else {
      if (object@simtimes$start > object@simtimes$end) {
        stop("simulation end time cannot be before start time.")
      }
    }
  }
)

### `initialize` generic is already defined in the methods package
#' Generate a \code{simList} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @param .Object  A \code{simList} object.
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
#'
setMethod("initialize",
          signature(.Object = ".simList"),
          definition = function(.Object, ...) {

            sn <- slotNames(.Object)
            dots <- list(...)
            slotsProvided <- sn %in% names(dots)
            for (ss in sn[slotsProvided]) {
              slot(.Object, ss) <- dots[[ss]]
            }


            if ("modules" %in% sn[!slotsProvided])
              .Object@modules = as.list(NULL)

            if ("params" %in% sn[!slotsProvided])
              .Object@params = list(
                .checkpoint = list(interval = NA_real_, file = NULL),
                .progress = list(type = NULL, interval = NULL)
              )
            if ("depends" %in% sn[!slotsProvided])
              .Object@depends = new(".simDeps", dependencies = list(NULL))
            if ("simtimes" %in% sn[!slotsProvided])
              .Object@simtimes = list(
                current = 0.00, start = 0.00, end = 1.00, timeunit = NA_character_
              )
            if ("inputs" %in% sn[!slotsProvided])
              .Object@inputs = .fileTableIn()
            if ("outputs" %in% sn[!slotsProvided])
              .Object@outputs = .fileTableOut()
            if ("paths" %in% sn[!slotsProvided])
              .Object@paths = .paths()

            .Object@.xData <- new.env(parent = asNamespace("SpaDES.core"))
            attr(.Object@.xData, "name") <- "sim"
            #
            return(.Object)
          })
################################################################################
#' @inheritParams .simList
#'
#' @slot .envir     Environment referencing the objects used in the simulation.
#'                  Several "shortcuts" to accessing objects referenced by this
#'                  environment are provided, and can be used on the
#'                  \code{simList} object directly instead of specifying the
#'                  \code{.envir} slot: \code{$}, \code{[[}, \code{ls},
#'                  \code{ls.str}, \code{objs}. See examples.
#'
#' @aliases simList
#' @rdname simList-class
#' @exportClass simList
#'
setClass("simList",
         contains = ".simList",
         slots = list(.envir = "environment"),
         prototype = list(.envir = new.env(parent = asNamespace("SpaDES.core"))) #emptyenv()))#
)

################################################################################
#' The \code{simList_} class
#'
#' Internal use only. Used when saving/loading a \code{simList}.
#'
#' This is identical to class \code{simList}, except that the \code{.envir} slot
#' is replaced by a \code{.list} containing a list to store the objects from the
#' environment contained within the \code{simList}.
#' Saving/loading a list behaves more reliably than saving/loading an environment.
#'
#' @inheritParams .simList
#'
#' @seealso \code{\link{simList}}
#'
#' @aliases simList_
#' @keywords internal
#' @rdname simList_-class
#'
#' @author Alex Chubaty
#'
setClass("simList_",
         contains = "list",
         slots = list(
           modules = "list", params = "list", events = "list",#data.table",
           current = "list", #"data.table",
           completed = "list", depends = ".simDeps",
           simtimes = "list", inputs = "data.frame", outputs = "data.frame", paths = "list",
           .list = "list"
         )
)

setAs(from = "simList_", to = "simList", def = function(from) {
  browser()
  x <- new(to,
           modules = from@modules,
           params = from@params,
           events = from@events,
           current = from@current,
           completed = from@completed,
           depends = from@depends,
           simtimes = from@simtimes,
           inputs = from@inputs,
           outputs = from@outputs,
           paths = from@paths)
  #x <- as(as(from, ".simList"), "simList")
  x@.xData <- new.env(new.env(parent = emptyenv()))
  x@.envir <- x@.xData
  list2env(from@.Data, envir = x@.xData)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  return(x)
})

setAs(from = ".simList", to = "simList_", def = function(from) {
  browser()
  to <- from
  x <- as(as(from, ".simList"), "simList")
  x@.envir <- new.env(new.env(parent = emptyenv()))
  list2env(from@.list, envir = x@.envir)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  return(x)
})

setAs(from = "simList_", to = ".simList", def = function(from, to) {
  browser()
  x <- new(to,
           modules = from@modules,
           params = from@params,
           events = from@events,
           current = from@current,
           completed = from@completed,
           depends = from@depends,
           simtimes = from@simtimes,
           inputs = from@inputs,
           outputs = from@outputs,
           paths = from@paths)
  x@.Data <- as.list(envir(from), all.names = TRUE)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  return(x)
})

setAs(from = "simList", to = "simList_", def = function(from, to) {
  browser()
  x <- new(to,
           modules = from@modules,
           params = from@params,
           events = from@events,
           current = from@current,
           completed = from@completed,
           depends = from@depends,
           simtimes = from@simtimes,
           inputs = from@inputs,
           outputs = from@outputs,
           paths = from@paths)
  x@.Data <- as.list(envir(from), all.names = TRUE)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  return(x)
})

### `initialize` generic is already defined in the methods package
#' Generate a \code{simList} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @param .Object  A \code{simList} object.
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
#'
setMethod("initialize",
          signature(.Object = "simList"),
          definition = function(.Object, ...) {
            browser()
            .Object <- callNextMethod(.Object, ...)
            .Object@.envir <- .Object@.xData # backwards compatibility
            #.Object@.envir <- new.env(parent = asNamespace("SpaDES.core"))
            #attr(.Object@.envir, "name") <- "sim"
            return(.Object)
})

### `initialize` generic is already defined in the methods package
#' Generate a \code{simList} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @param .Object  A \code{simList} object.
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
#'
setMethod("initialize",
          signature(.Object = "simList_"),
          definition = function(.Object, ...) {
            browser()
            .Object <- callNextMethod(.Object, ...)

            .Object@.list <- .Object@.Data # backwards compatibility
            #.Object@.envir <- new.env(parent = asNamespace("SpaDES.core"))
            #attr(.Object@.envir, "name") <- "sim"
            return(.Object)
          })
