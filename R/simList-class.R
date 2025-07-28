#' The `simList` class
#'
#' Contains the minimum components of a `SpaDES` simulation.
#' Various slot accessor methods (i.e., get and set functions) are provided
#' (see 'Accessor Methods' below).
#'
#' Based on code from chapter 7.8.3 of Matloff (2011): "Discrete event simulation".
#' Here, we implement a discrete event simulation in a more modular fashion so
#' it's easier to add simulation components (i.e., "simulation modules").
#' We use S4 classes and methods, and use [data.table::data.table()] instead of
#' [data.frame()] to implement the event queue (because it is much
#' more efficient).
#'
#' @note The `simList` class extends the `environment`, by adding
#' several slots that provide information about the metadata for a discrete
#' event simulation. The environment slot, if accessed directly is `.xData`
#' and this is where input and output objects from modules are placed.
#' The [simList_()] class is similar, but it extends the `list`
#' class. All other slots are the same.
#' Thus, `simList` is identical to `simList_`, except that the former
#' uses an environment for objects and the latter uses a list.
#' The class `simList_` is only used internally when saving/loading, because
#' saving/loading a list behaves more reliably than saving/loading an environment.
#'
#' @slot modules    List of character names specifying which modules to load.
#'
#' @slot params     Named list of potentially other lists specifying simulation
#'                  parameters.
#'
#' @slot events     The list of scheduled events (i.e., event queue), which can
#'                  be converted to a sorted `data.table` with `events(sim)`.
#'                  See 'Event Lists' for more information.
#'
#' @slot current    The current event, as a `data.table`.
#'                  See 'Event Lists' for more information..
#'
#' @slot completed  An environment consisting of completed events, with
#'                  each object named a character representation of the order
#'                  of events. This was converted from a previous version which
#'                  was a list. This was changed because the list became
#'                  slow as number of events increased.
#'                  See 'Event Lists' for more information. It is kept
#'                  as an environment of individual events for speed. The `completed`
#'                  method converts it to a sorted `data.table`.
#'
#' @slot depends    A `.simDeps` list of [.moduleDeps()] objects
#'                  containing module object dependency information.
#'
#' @slot simtimes   List of numerical values describing the simulation start
#'                  and end times; as well as the current simulation time.
#'
#' @slot inputs     a `data.frame` or `data.table` of files and
#'                  metadata
#'
#' @slot outputs    a `data.frame`  or `data.table` of files and
#'                  metadata
#'
#' @slot paths      Named list of paths. See `?.paths`. Partial matching is performed.
#'
#' @slot .xData     Environment referencing the objects used in the simulation.
#'                  Several "shortcuts" to accessing objects referenced by this
#'                  environment are provided, and can be used on the
#'                  `simList` object directly instead of specifying the
#'                  `.xData` slot: `$`, `[[`, `ls`,
#'                  `ls.str`, `objs`. See examples.
#' @slot .envir     Deprecated. Please do not use any more.
#'
#' @section Accessor Methods:
#'
#' Several slot (and sub-slot) accessor methods are provided for use, and
#' categorized into separate help pages:
#' \tabular{ll}{
#'   [simList-accessors-envir()] \tab Simulation environment. \cr
#'   [simList-accessors-events()] \tab Scheduled and completed events. \cr
#'   [simList-accessors-inout()] \tab Passing data in to / out of simulations. \cr
#'   [simList-accessors-modules()] \tab Modules loaded and used; module dependencies. \cr
#'   [simList-accessors-objects()] \tab Accessing objects used in the simulation. \cr
#'   [simList-accessors-params()] \tab Global and module-specific parameters. \cr
#'   [simList-accessors-paths()] \tab File paths for modules, inputs, and outputs. \cr
#'   [simList-accessors-times()] \tab Simulation times. \cr
#' }
#'
#' @section Event Lists:
#'
#' The main event list is a sorted `data.table` (keyed) on `eventTime`, and `eventPriority.`
#' The completed event list is an ordered list in the exact order that the events were executed.
#' Each event is represented by a [data.table::data.table()] row consisting of:
#' \tabular{ll}{
#'   `eventTime` \tab The time the event is to occur.\cr
#'   `moduleName` \tab The module from which the event is taken.\cr
#'   `eventType` \tab A character string for the programmer-defined event type.\cr
#'   `eventPriority` \tab The priority given to the event. \cr
#' }
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
#'
#' @aliases simList
#' @author Alex Chubaty and Eliot McIntire
#' @exportClass simList
#' @importFrom data.table as.data.table data.table
#' @include helpers.R misc-methods.R module-dependencies-class.R
#' @rdname simList-class
setClass(
  "simList",
  contains = "environment",
  slots = list(
    modules = "list", params = "list",
    events = "list", current = "list", ## formerly data.tables
    completed = "environment", depends = ".simDeps",
    simtimes = "list", inputs = "data.frame", outputs = "data.frame", paths = "list",
    .envir = "environment"
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
#' Generate a `simList` object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, `new` returns an object from that class.
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
setMethod("initialize",
          signature(.Object = "simList"),
          definition = function(.Object, ...) {
            # browser(expr = exists("._initialize_1"))
            sn <- slotNames(.Object)
            dots <- list(...)
            slotsProvided <- sn %in% names(dots)
            for (ss in sn[slotsProvided]) {
              slot(.Object, ss) <- dots[[ss]]
            }

            expected <- c("modules", "params", "depends", "simtimes",
                          "inputs", "outputs", "paths")
            haves <- na.omit(match(sn[!slotsProvided], expected))
            if (any(1 == haves)) {
              .Object@modules <- as.list(NULL)
            }
            if (any(2 == haves)) {
              .Object@params <- list(
                checkpoint = list(interval = NA_real_, file = NULL),
                .progress = list(type = NULL, interval = NULL)
              )
            }
            if (any(3 == haves)) {
              .Object@depends <- .emptySimDeps
            }
            if (any(4 == haves)) {
              .Object@simtimes <- list(
                current = 0.00, start = 0.00, end = 1.00, timeunit = NA_character_
              )
            }
            if (any(5 == haves)) {
              .Object@inputs <- .fileTableInDF
            }
            if (any(6 == haves)) {
              .Object@outputs <- .fileTableOutDF
            }
            if (any(7 == haves)) {
              .Object@paths <- .paths()
            }

            .Object@completed <- new.env(parent = emptyenv())

            # .Object@.xData <- new.env(parent = asNamespace("SpaDES.core"))
            # browser(expr = exists("._initialize_2"))
            .Object@.xData <- new.env(parent = emptyenv())
            # .Object@.xData$.objects <- new.env(parent = emptyenv())
            .Object@.xData$.mods <- new.env(parent = emptyenv())
            .Object@.envir <- .Object@.xData
            attr(.Object@.xData, "name") <- "sim"

            return(.Object)
})

################################################################################
#' @aliases simList_
#' @aliases simList_-class
#' @rdname simList-class
setClass("simList_",
         contains = "list",
         slots = list(
           modules = "list", params = "list",
           events = "list", current = "list", ## formerly data.tables
           completed = "list", depends = ".simDeps",
           simtimes = "list", inputs = "data.frame", outputs = "data.frame", paths = "list",
           .list = "list"
         )
)

setAs(from = "simList_", to = "simList", def = function(from) {
  x <- new(to,
           modules = from@modules,
           params = from@params,
           events = from@events,
           current = from@current,
           completed = new.env(parent = emptyenv()),
           depends = from@depends,
           simtimes = from@simtimes,
           inputs = from@inputs,
           outputs = from@outputs,
           paths = from@paths)
  x@.xData <- new.env(parent = emptyenv())
  x@.envir <- x@.xData
  list2env(from, envir = x@.xData)
  list2env(from@completed, envir = x@completed)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  if (!is.null(x$objectSynonyms)) {
    x <- .checkObjectSynonyms(x)
  }
  return(x)
})

setAs(from = "simList", to = "simList_", def = function(from, to) {
  x <- new(to,
           modules = from@modules,
           params = from@params,
           events = from@events,
           current = from@current,
           completed = as.list(from@completed, all.names = TRUE, sorted = TRUE),
           depends = from@depends,
           simtimes = from@simtimes,
           inputs = from@inputs,
           outputs = from@outputs,
           paths = from@paths)
  x@.Data <- as.list(envir(from), all.names = TRUE)
  x <- .keepAttrs(from, x) # the as methods don't keep attributes
  if (!is.null(from$objectSynonyms)) {
    activeBindingsToDel <- unlist(lapply(from$objectSynonyms, function(os) os[-1]))
    attr(x$objectSynonyms, "bindings") <- NULL
    x[activeBindingsToDel] <- NULL
  }
  return(x)
})

### `initialize` generic is already defined in the methods package
#' Generate a `simList` object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, `new` returns an object from that class.
#'
#' @param .Object  A `simList` object.
#' @param ... Optional Values passed to any or all slot
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
#'
setMethod("initialize",
          signature(.Object = "simList_"),
          definition = function(.Object, ...) {
            .Object <- callNextMethod(.Object, ...)

            .Object@.list <- .Object@.Data # backwards compatibility
            #.Object@.envir <- new.env(parent = asNamespace("SpaDES.core"))
            #attr(.Object@.envir, "name") <- "sim"
            return(.Object)
})
