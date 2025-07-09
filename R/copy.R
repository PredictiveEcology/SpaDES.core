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
#' @note uses capital C, to limit confusion with e.g., [data.table::copy()].
#'
#' @inheritParams reproducible::Copy
#'
#' @param objects  Whether the objects contained within the `simList` environment
#'                 should be copied. Default `TRUE`, which may be slow.
#' @param queues Logical. Should the events queues (`events`, `current`, `completed`)
#'               be deep copied via [data.table::copy()]
#'
#' @param modules Logical. Should list of modules be copied.
#'
#' @return a copy of `object`
#'
#' @details
#' `simList` objects can contain a lot of information, much of which could be in
#' pass-by-reference objects (e.g., `data.table` class), and objects that are file-backed,
#' such as some `Raster*`-class objects. For all the objects that are file-backed,
#' it is likely *very* important to give unique file-backed directories.
#' This should be passed here, which gets passed on to the many methods of [reproducible::Copy()].
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
          definition = function(object, objects, queues, modules, ...) {
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
            # sim_@.xData <- new.env(parent = asNamespace("SpaDES.core"))
            # sim_@.xData <- new.env(parent = as.environment("package:SpaDES.core"))
            sim_@.xData <- new.env(parent = emptyenv())
            sim_@.xData[[dotMods]] <- new.env(parent = asNamespace("SpaDES.core"))
            ## Setup dotObjs later because it is not vectorized over module
            attr(sim_@.xData, "name") <- "sim"

            ## set up mod environments, including .objects
            for (dotType in dotObjsAndMods)
              for (modNam in names(object@.xData[[dotType]])) {
                sim_ <- newEnvsByModule(sim_, modNam)
              }

            if (objects == 2) {
              dotObjsNotDotMods <- grep(paste0("\\", dotMods, "|", "\\", dotObjs), # "\\.mods",
                                        ls(object@.xData, pattern = "^\\.", all.names = TRUE),
                                        invert = TRUE, value = TRUE)
              mget(dotObjsNotDotMods, envir = object@.xData) |>
                Copy() |>
                list2env(envir = sim_@.xData)
            }
            if (objects > 0) {
              # browser(expr = exists("._Copy_6"))
              ## Make sure that the file-backed objects get a copy too -- use Copy -- makes a list
              if (objects == 1) {
                ## Copy the whole environment, recursively through environments
                sim_@.xData <- Copy(object@.xData, ...) # filebackedDir = filebackedDir)
              }

              ## TODO: confirm that skipping if-null is correct
              if (!is.null(object@.xData[[dotObjs]])) {
                objNames <- ls(object@.xData[[dotObjs]], all.names = TRUE)
                if (isTRUE(is.character(modules))) {
                  objNames <- objNames[match(modules, objNames)]
                }
                names(objNames) <- objNames
                isEnv <- lapply(objNames, function(obj) {
                  is.environment(get(obj, envir = object@.xData[[dotObjs]]))
                }) |> unlist()

                ## This chunk makes the environment of each function in a module,
                ##   the module itself. This is unique to functions in `simList` objs
                ##   i.e., can't rely on generic reproducible::Copy
                lapply(objNames[isEnv], function(en) {
                  list2env(as.list(object@.xData[[dotObjs]][[en]], all.names = TRUE),
                           envir = sim_@.xData[[dotObjs]][[en]])
                  isFn <- unlist(lapply(ls(sim_@.xData[[dotObjs]][[en]]), function(obj) {
                    if (is.function(get(obj, envir = sim_@.xData[[dotObjs]][[en]]))) {
                      environment(sim_@.xData[[dotObjs]][[en]][[obj]]) <- sim_@.xData[[dotObjs]][[en]]
                    }
                  }))
                })
              }

              ## Copy .objects
              modsToCopy <- modules(sim_)
              if (is.character(modules)) {
                modsToCopy <- intersect(modules, modsToCopy)
              }
              lapply(modsToCopy, function(mod) {
                if (exists(mod, envir = sim_@.xData[[dotObjs]], inherits = FALSE)) {
                  # rm(list = ".objects", envir = sim_@.xData[[dotMods]][[mod]], inherits = FALSE)
                  # sim_@.xData[[dotMods]][[mod]]$.objects <- new.env(parent = emptyenv())
                  # list2env(as.list(object@.xData[[dotMods]][[mod]]$.objects, all.names = TRUE),
                  #          envir = sim_@.xData[[dotMods]][[mod]]$.objects)
                  rm(list = mod, envir = sim_@.xData[[dotObjs]], inherits = FALSE)
                  sim_ <- setupModObjsEnv(sim_, moduleName = mod)
                  # rm(list = ".objects", envir = sim_@.xData[[dotMods]][[mod]], inherits = FALSE)
                  # sim_@.xData[[dotMods]][[mod]]$.objects <- new.env(parent = emptyenv())
                  list2env(as.list(object@.xData[[dotObjs]][[mod]], all.names = TRUE),
                           envir = sim_@.xData[[dotObjs]][[mod]])
                }
              })

              ## Deal with activeBindings
              makeSimListActiveBindings(sim_)
            }
            sim_@.envir <- sim_@.xData
            return(sim_)
          }
)
