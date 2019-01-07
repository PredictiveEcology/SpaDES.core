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
#' @author Eliot McIntire
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
              sim_@events <- object@events
              sim_@current <- object@current
            }
            #sim_@.xData <- new.env(parent = asNamespace("SpaDES.core"))
            #sim_@.xData <- new.env(parent = as.environment("package:SpaDES.core"))
            sim_@.xData <- new.env(parent = emptyenv())
            attr(sim_@.xData, "name") <- "sim"
            if (objects) {
              objNames <- ls(object, all.names = TRUE)
              names(objNames) <- objNames
              isEnv <- unlist(lapply(objNames,
                                     function(obj) is.environment(get(obj, envir = object))))
              list2env(mget(objNames[!isEnv], envir = object@.xData), envir = sim_@.xData)
              list2env(lapply(objNames[isEnv], function(x) {
                e <- new.env(parent = asNamespace("SpaDES.core"))
                attr(e, "name") <- x
                e
              }
              ),
              envir = sim_@.xData)

              lapply(objNames[isEnv], function(en) {
                list2env(as.list(object@.xData[[en]], all.names = TRUE),
                         envir = sim_@.xData[[en]])
                isFn <- unlist(lapply(ls(sim_@.xData[[en]]), function(obj)
                  if (is.function(get(obj, envir = sim_@.xData[[en]]))) {
                    environment(sim_@.xData[[en]][[obj]]) <- sim_@.xData[[en]]
                  }
                ))
              })

              # Deal with activeBinding for mod
              lapply(objNames[isEnv], function(en) {
                if (exists("mod", object[[en]], inherits = FALSE)) {
                  if (bindingIsActive("mod", object[[en]])) {
                    rm(list = "mod", envir = sim_[[en]])
                    makeActiveBinding(sym = "mod",
                                      fun = function(value){
                                        if (missing(value)) {
                                          get(en, envir = sim_, inherits = FALSE)
                                        } else {
                                          stop("Can't overwrite mod")
                                        }
                                      },
                                      env = sim_[[en]])
                  }
                }

              })

            }
            sim_@.envir <- sim_@.xData
            return(sim_)
})


