if (!isGeneric("Copy")) {
  setGeneric("Copy", function(object, ...) {
    standardGeneric("Copy")
  })
}

#' Copy for simList class objects
#'
#' Because a simList works with an environment to hold all objects,
#' all objects within that slot are pass-by-reference. That means
#' it is not possible to simply copy an object with an assignment operator:
#' the two objects will share the same objects. As one simList object changes
#' so will the other. when this is not the desired behaviour, use this function.
#' NOTE: use capital C, to limit confusion with \code{data.table::copy()}
#' See \code{\link[reproducible]{Copy}}.
#'
#' @inheritParams reproducible::Copy
#' @param objects  Whether the objects contained within the simList environment
#'                 should be copied. Default \code{TRUE}, which may be slow.
#' @param queues Logical. Should the events queues (\code{events},
#'               \code{current}, \code{completed}) be deep copied via
#'               \code{data.table::copy}
#'
#' @author Eliot MciIntire
#' @exportMethod Copy
#' @export
#' @importFrom reproducible Copy
#' @importMethodsFrom reproducible Copy
#' @include simList-class.R
#' @rdname Copy
#' @seealso \code{\link[reproducible]{Copy}}
setMethod("Copy",
          signature(object = "simList"),
          definition = function(object, objects, queues) {
            if (missing(objects)) objects <- TRUE
            if (missing(queues)) queues <- TRUE
            sim_ <- object
            if (queues) {
              sim_@events <- data.table::copy(object@events)
              sim_@current <- data.table::copy(object@current)
            }
            if (objects) {
              sim_@.envir <- Copy(sim_@.envir)
            }
            return(sim_)
})
