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
#' @details
#' \code{simList} objects can contain a lot of information, much of which could be
#' in pass-by-reference objects (e.g., \code{data.table} class), and objects that are
#' file-backed, such as some \code{Raster*}-class objects. For all the objects that
#' are file-backed, it is likely \emph{very} important to give unique file-backed
#' directories. This should be passed here, which gets passed on to the many methods
#' of \code{Copy} in \code{reproducible}.
#'
#' @author Eliot McIntire
#' @exportMethod Copy
#' @export
#' @importFrom reproducible Copy tempdir2
#' @importMethodsFrom reproducible Copy
#' @include simList-class.R
#' @rdname Copy
#' @seealso \code{\link[reproducible]{Copy}}
setMethod("Copy",
          signature(object = "simList"),
          definition = function(object, filebackedDir,
                                objects, queues) {
            if (missing(filebackedDir)) {
              filebackedDir <- tempdir2(rndstr(1, 8))
            }
            if (missing(objects)) objects <- TRUE
            if (missing(queues)) queues <- TRUE
            sim_ <- object
            sim_@completed <- new.env(parent = emptyenv())
            if (queues) {
              sim_@events <- object@events
              sim_@current <- object@current
              list2env(as.list(object@completed), envir = sim_@completed)
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
              # # Make sure that the file-backed objects get a copy too -- use Copy -- makes a list
              # list2env(Copy(mget(objNames[!isEnv], envir = object@.xData), filebackedDir = filebackedDir),
              #          envir = sim_@.xData)
              # list2env(lapply(objNames[isEnv], function(x) {
              #   e <- new.env(parent = asNamespace("SpaDES.core"))
              #   attr(e, "name") <- x
              #   e
              # }
              # ),
              # envir = sim_@.xData)
              #
              # lapply(objNames[isEnv], function(en) {
              #   list2env(as.list(object@.xData[[en]], all.names = TRUE),
              #            envir = sim_@.xData[[en]])
              #   isFn <- unlist(lapply(ls(sim_@.xData[[en]]), function(obj)
              #     if (is.function(get(obj, envir = sim_@.xData[[en]]))) {
              #       environment(sim_@.xData[[en]][[obj]]) <- sim_@.xData[[en]]
              #     }
              #   ))
              # })
              #
              # # Deal with data.table objects
              # anyDataTables <- unlist(lapply(objs(sim_), is.data.table))
              # anyDataTables <- anyDataTables[anyDataTables]
              # lapply(names(anyDataTables), function(dt) {
              #   sim_@.xData[[dt]] <- data.table::copy(sim_@.xData[[dt]])
              # })

              # Copy the whole environment, recursively through environments
              sim_@.xData <- Copy(object@.xData, filebackedDir = filebackedDir)

              # This chunk makes the environment of each function in a module,
              #   the module itself. This is unique to functions in `simList` objs
              #   i.e., can't rely on generic reproducible::Copy
              lapply(objNames[isEnv], function(en) {
                list2env(as.list(object@.xData[[en]], all.names = TRUE),
                         envir = sim_@.xData[[en]])
                isFn <- unlist(lapply(ls(sim_@.xData[[en]]), function(obj) {
                  if (is.function(get(obj, envir = sim_@.xData[[en]]))) {
                    environment(sim_@.xData[[en]][[obj]]) <- sim_@.xData[[en]]
                  }
                }
                ))
              })

              # Deal with activeBinding for mod
              lapply(objNames[isEnv], function(mod) {
                if (exists("mod", object[[mod]], inherits = FALSE)) {
                  if (bindingIsActive("mod", object[[mod]])) {
                    rm(list = "mod", envir = sim_[[mod]], inherits = FALSE)
                    rm(list = ".objects", envir = sim_[[mod]], inherits = FALSE)
                    sim_[[mod]]$.objects <- new.env(parent = emptyenv())
                    list2env(as.list(object@.xData[[mod]]$.objects, all.names = TRUE),
                             envir = sim_@.xData[[mod]]$.objects)

                    makeModActiveBinding(sim = sim_, mod = mod)
                  }
                }

              })

            }
            sim_@.envir <- sim_@.xData
            return(sim_)
})


