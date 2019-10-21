################################################################################
#' The \code{simLists} class
#'
#' This is a grouping of \code{simList} objects
#'
#' @note The \code{simList} class extends the \code{environment}, by adding
#' several slots that provide information about the metadata for a discrete
#' event simulation. The environment slot, if accessed directly is \code{.xData}
#' and this is where input and output objects from modules are placed.
#' The \code{\link{simList_}} class is similar, but it extends the \code{list}
#' class. All other slots are the same.
#' Thus, \code{simList} is identical to \code{simList_}, except that the former
#' uses an environment for objects and the latter uses a list.
#' The class \code{simList_} is only used internally.
#'
#' @slot paths      Named list of \code{modulePath}, \code{inputPath},
#'                  and \code{outputPath} paths. Partial matching is performed. These
#'                  will be prepended to the relative paths of each \code{simList}
#' @slot .xData  Environment holding the \code{simLists}.
#'
#' @section Accessor Methods:
#'
#' None yet defined:
#' \tabular{ll}{
#'   \code{\link{simList-accessors-envir}} \tab Simulation environment. \cr
#' }
#'
#'
#' @aliases simLists
#' @rdname simLists-class
#' @rdname simLists
#' @importFrom data.table as.data.table data.table
#' @include simList-class.R helpers.R misc-methods.R module-dependencies-class.R
#'
#' @author Eliot McIntire
#' @exportClass simLists
#'
setClass(
  "simLists",
  contains = "environment",
  slots = list(
    .xData = "environment", paths = "list"
  ),
  validity = function(object) {
    browser()
  }
)

#' Generate a \code{simLists} object
#'
#' Given the name or the definition of a class, plus optionally data to be
#' included in the object, \code{new} returns an object from that class.
#'
#' @export
#' @include misc-methods.R
#' @rdname initialize-method
#'
setMethod("initialize",
          signature(.Object = "simLists"),
          definition = function(.Object, ...) {

            .Object@paths = .paths()

            .Object@.xData <- new.env(parent = emptyenv())

            #.Object@.xData <- new.env(parent = asNamespace("SpaDES.core"))
            attr(.Object@.xData, "name") <- "simLists"
            #
            return(.Object)
          })
