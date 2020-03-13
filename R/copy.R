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
              if (isTRUE(objects)) {
                filebackedDir <- tempdir2(rndstr(1, 8))
              }
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
            sim_@.xData$.mods <- new.env(parent = asNamespace("SpaDES.core"))
            attr(sim_@.xData, "name") <- "sim"
            if (objects) {
              browser(expr = exists("._Copy_6"))
              objNames <- ls(object@.xData$.mods, all.names = TRUE)
              names(objNames) <- objNames
              isEnv <- unlist(lapply(objNames,
                                     function(obj)
                                       is.environment(get(obj, envir = object@.xData$.mods))))
              # # Make sure that the file-backed objects get a copy too -- use Copy -- makes a list

              # Copy the whole environment, recursively through environments
              sim_@.xData <- Copy(object@.xData, filebackedDir = filebackedDir)

              # This chunk makes the environment of each function in a module,
              #   the module itself. This is unique to functions in `simList` objs
              #   i.e., can't rely on generic reproducible::Copy
              lapply(objNames[isEnv], function(en) {
                list2env(as.list(object@.xData$.mods[[en]], all.names = TRUE),
                         envir = sim_@.xData$.mods[[en]])
                isFn <- unlist(lapply(ls(sim_@.xData$.mods[[en]]), function(obj) {
                  if (is.function(get(obj, envir = sim_@.xData$.mods[[en]]))) {
                    environment(sim_@.xData$.mods[[en]][[obj]]) <- sim_@.xData$.mods[[en]]
                  }
                }
                ))
              })

              # Deal with activeBinding for mod
              lapply(objNames[isEnv], function(mod) {
                if (exists("mod", object@.xData$.mods[[mod]], inherits = FALSE)) {
                  if (bindingIsActive("mod", object@.xData$.mods[[mod]])) {
                    rm(list = "mod", envir = sim_@.xData$.mods[[mod]], inherits = FALSE)
                    rm(list = ".objects", envir = sim_@.xData$.mods[[mod]], inherits = FALSE)
                    sim_@.xData$.mods[[mod]]$.objects <- new.env(parent = emptyenv())
                    list2env(as.list(object@.xData$.mods[[mod]]$.objects, all.names = TRUE),
                             envir = sim_@.xData$.mods[[mod]]$.objects)

                    makeModActiveBinding(sim = sim_, mod = mod)
                  }
                }

              })
              lapply(modules(sim_), function(mod) {
                if (exists("mod", object@.xData$.mods[[mod]], inherits = FALSE)) {
                  if (bindingIsActive("mod", object@.xData$.mods[[mod]])) {
                    rm(list = "Par", envir = sim_@.xData$.mods[[mod]], inherits = FALSE)
                    makeParActiveBinding(sim = sim_, mod = mod)
                  }}
              })



            }
            sim_@.envir <- sim_@.xData
            return(sim_)
})


