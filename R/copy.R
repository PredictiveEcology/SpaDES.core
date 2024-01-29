if (!isGeneric("Copy")) {
  setGeneric("Copy", function(object, ...) {
    standardGeneric("Copy")
  })
}

#' Copy for `simList` class objects
#'
#' Because a `simList` works with an environment to hold all objects,
#' all objects within that slot are pass-by-reference.
#' That means it is not possible to simply copy an object with an assignment operator:
#' the two objects will share the same objects.
#' As one `simList` object changes so will the other.
#' When this is not the desired behaviour, use this function.
#'
#' @note uses capital C, to limit confusion with e.g., `data.table::copy()`.
#'
#' @seealso [reproducible::Copy()]
#'
#' @inheritParams reproducible::Copy
#'
#' @param objects  Whether the objects contained within the `simList` environment
#'                 should be copied. Default `TRUE`, which may be slow.
#' @param queues Logical. Should the events queues (`events`, `current`, `completed`)
#'               be deep copied via `data.table::copy()`
#'
#' @return a copy of `object`
#'
#' @details
#' `simList` objects can contain a lot of information, much of which could be
#' in pass-by-reference objects (e.g., `data.table` class), and objects that are
#' file-backed, such as some `Raster*`-class objects. For all the objects that
#' are file-backed, it is likely *very* important to give unique file-backed
#' directories. This should be passed here, which gets passed on to the many methods
#' of `Copy` in `reproducible`.
#'
#' @author Eliot McIntire
#' @exportMethod Copy
#' @export
#' @importFrom reproducible Copy tempdir2
#' @importMethodsFrom reproducible Copy
#' @include simList-class.R
#' @rdname Copy
#' @seealso [reproducible::Copy()]
setMethod("Copy",
          signature(object = "simList"),
          definition = function(object,
                                objects, queues, modules, ...) {
            if (missing(objects)) objects <- TRUE
            if (missing(queues)) queues <- TRUE
            if (missing(modules)) modules <-  TRUE
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


            # # set up mod environments, including .objects
            for (modNam in names(object@.xData$.mods)) {
              sim_ <- newEnvsByModule(sim_, modNam)
            }

            if (objects == 2) {
              dotObjsNotDotMods <- grep("\\.mods", ls(object@.xData, pattern = "^\\.", all.names = TRUE),
                                        invert = TRUE, value = TRUE)
              list2env(Copy(mget(dotObjsNotDotMods, envir = object@.xData)),
                       envir = sim_@.xData)
            }
            if (objects > 0) {

              # browser(expr = exists("._Copy_6"))
              objNames <- ls(object@.xData$.mods, all.names = TRUE)
              if (isTRUE(is.character(modules))) {
                objNames <- objNames[match(modules, objNames)]
              }
              names(objNames) <- objNames
              isEnv <- unlist(lapply(objNames,
                                     function(obj)
                                       is.environment(get(obj, envir = object@.xData$.mods))))
              # # Make sure that the file-backed objects get a copy too -- use Copy -- makes a list

              if (objects == 1) {
                # Copy the whole environment, recursively through environments
                sim_@.xData <- Copy(object@.xData, ...) # filebackedDir = filebackedDir)
              }

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

              # Copy .objects
              modsToCopy <- modules(sim_)
              if (is.character(modules)) {
                modsToCopy <- intersect(modules, modsToCopy)
              }
              lapply(modsToCopy, function(mod) {
                if (exists(mod, envir = sim_@.xData$.mods, inherits = FALSE)) {
                  rm(list = ".objects", envir = sim_@.xData$.mods[[mod]], inherits = FALSE)
                  sim_@.xData$.mods[[mod]]$.objects <- new.env(parent = emptyenv())
                  list2env(as.list(object@.xData$.mods[[mod]]$.objects, all.names = TRUE),
                           envir = sim_@.xData$.mods[[mod]]$.objects)
                }
              })

              # Deal with activeBindings
              makeSimListActiveBindings(sim_)
            }
            sim_@.envir <- sim_@.xData
            return(sim_)
          })

