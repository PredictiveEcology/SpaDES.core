utils::globalVariables(c(
  ".SD", "clockTime", "diffTime", "eventNumber", "eventTime", "eventType", "exts",
  "minEventTime", "maxEventTime", "savetime", "unit"
))

### `show` generic is already defined in the methods package
#' Show an Object
#'
#' @param object  \code{simList}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom stats na.omit
#' @importFrom utils capture.output ls.str
#' @include simList-class.R
#' @rdname show-method
setMethod(
  "show",
  signature = "simList",
  definition = function(object) {
    out <- list()
    out[[1]] <- capture.output(
      cat(rep("=", getOption("width"), sep = ""), "\n", sep = "")
    )

    ### simulation dependencies
    out[[2]] <- capture.output(cat(">> Simulation dependencies:\n"))
    out[[3]] <- "use `depends(sim)` to view dependencies for each module"
    out[[4]] <- capture.output(cat("\n"))

    ### simtimes
    out[[5]] <- capture.output(cat(">> Simulation times:\n"))
    out[[6]] <- capture.output(print(rbind(times(object))))
    out[[7]] <- capture.output(cat("\n"))

    ### modules loaded
    out[[8]] <- capture.output(cat(">> Modules:\n"))
    ord <- match(unlist(modules(object)), names(timeunits(object))) %>% na.omit
    out[[9]] <- capture.output(print(
      cbind(Name = unname(modules(object)),
            #Timeunit = c(rep(NA_character_, 4), unname(timeunits(object))[ord])),
            Timeunit = unname(timeunits(object))[ord]),
      quote = FALSE, row.names = FALSE))
    out[[10]] <- capture.output(cat("\n"))

    ### objects loaded
    out[[11]] <- capture.output(cat(">> Objects Loaded:\n"))

    out[[12]] <- if (NROW(inputs(object)[na.omit(inputs(object)$loaded == TRUE), ])) {
      capture.output(print(inputs(object)[na.omit(inputs(object)$loaded == TRUE), ]))
    }
    out[[13]] <- capture.output(cat("\n"))

    ### list stored objects
    out[[14]] <- capture.output(cat(">> Objects stored:\n"))
    out[[15]] <- capture.output(print(ls.str(envir(object))))
    out[[16]] <- capture.output(cat("\n"))

    ### params
    omit <- which(names(params(object)) == ".progress")

    p <- mapply(
      function(x, y) {
        if (length(names(y)) > 0)
        data.frame(Module = x, Parameter = names(y), Value = I(as.list(y)),
                   stringsAsFactors = FALSE, row.names = NULL)
      },
      x = names(params(object))[-omit],
      y = params(object)[-omit],
      USE.NAMES = TRUE, SIMPLIFY = FALSE
    )
    if (length(p)) {
      q <- do.call(rbind, p)
      q <- q[order(q$Module, q$Parameter), ]
    } else {
      q <- cbind(Module = list(), Parameter = list())
    }
    out[[17]] <- capture.output(cat(">> Parameters:\n"))
    out[[18]] <- capture.output(print(q, row.names = FALSE))
    out[[19]] <- capture.output(cat("\n"))

    ### completed events
    out[[20]] <- capture.output(cat(">> Completed Events:\n"))
    out[[21]] <- capture.output(completed(object))
    out[[22]] <- capture.output(cat("\n"))

    ### Current events
    out[[23]] <- capture.output(cat(">> Current Event:\n"))
    out[[24]] <- capture.output(current(object))
    out[[25]] <- capture.output(cat("\n"))

    ### scheduled events
    out[[26]] <- capture.output(cat(">> Scheduled Events:\n"))
    out[[27]] <- capture.output(events(object))
    out[[28]] <- capture.output(cat("\n"))

    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
})


################################################################################
#' Simulation environment
#'
#' Accessor functions for the \code{.xData} slot, which is the default virtual
#' slot for an S4 class object that inherits from an S3 object (specifically,
#' the \code{simList} inherits from \code{environment}) in a \code{simList} object.
#' These are included for advanced users.
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.8 on simList environment.
#'
#' @aliases simList-accessors-envir
#' @author Alex Chubaty
#' @export
#' @family functions to access elements of a 'simList' object
#' @include simList-class.R
#' @rdname simList-accessors-envir
#'
setGeneric("envir", function(sim) {
  standardGeneric("envir")
})

#' @rdname simList-accessors-envir
setMethod("envir",
          signature = "simList",
          definition = function(sim) {
            return(sim@.xData)
})

#' @export
#' @rdname simList-accessors-envir
setGeneric("envir<-",
           function(sim, value) {
             standardGeneric("envir<-")
})

#' @name envir<-
#' @aliases envir<-,simList-method
#' @rdname simList-accessors-envir
setReplaceMethod("envir",
                 signature = "simList",
                 function(sim, value) {
                   if (!is.environment(value)) stop("Must be an environment")
                   sim@.xData <- value
                   return(sim)
})

################################################################################
#' Extract or replace an object from the simulation environment
#'
#' The \code{[[} and \code{$} operators provide "shortcuts" for accessing
#' objects in the simulation environment.
#' I.e., instead of using \code{envir(sim)$object} or \code{envir(sim)[["object"]]},
#' one can simply use \code{sim$object} or \code{sim[["object"]]}.
#'
#' \code{objs} can take \code{...} arguments passed to \code{ls},
#' allowing, e.g. \code{all.names=TRUE}
#' \code{objs<-} requires takes a named list of values to be assigned in
#' the simulation environment.
#'
#' @param sim      A \code{simList} object from which to extract element(s) or
#'                 in which to replace element(s).
#' @param value objects to assign to the \code{simList}
#' @param ... passed to \code{ls}
#'
#' @return Returns or sets a list of objects in the \code{simList} environment.
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.1 on Simulation Parameters.
#'
#' @export
#' @include simList-class.R
#' @aliases simList-accessors-objects
#' @rdname objects
#'
setGeneric("objs", function(sim, ...) {
  standardGeneric("objs")
})

#' @export
#' @rdname objects
setMethod("objs",
          signature = "simList",
          definition = function(sim, ...) {
            w <- lapply(ls(sim@.xData, ...), function(z) {
              eval(parse(text = z), envir = sim@.xData)
            })
            names(w) <- ls(sim@.xData, ...)
            return(w)
})

#' @export
#' @rdname objects
setGeneric("objs<-",
           function(sim, value) {
             standardGeneric("objs<-")
})

#' @name objs<-
#' @aliases objs<-,simList-method
#' @rdname objects
#' @export
setReplaceMethod(
  "objs",
  signature = "simList",
  function(sim, value) {
    if (is.list(value)) {
     list2env(value, envir = sim@.xData)
     newInputs <- data.frame(
       objectName = names(value),
       loadTime = as.numeric(sim@simtimes[["current"]]),
       loaded = TRUE,
       stringsAsFactors = FALSE) %>% .fillInputRows(startTime = start(sim))
     inputs(sim) <- rbind(inputs(sim), newInputs)

    # lapply(names(value), function(z) {
    #   sim@.xData[[z]] <- value[[z]]
    # })
    } else {
     stop("must provide a named list.")
    }
    validObject(sim)
    return(sim)
})

################################################################################
#' Simulation modules and dependencies
#'
#' Accessor functions for the \code{depends} and \code{modules} slots in a
#' \code{simList} object.
#' These are included for advanced users.
#' \tabular{ll}{
#'    \code{\link{depends}} \tab List of simulation module dependencies. (advanced) \cr
#'    \code{\link{modules}} \tab List of simulation modules to be loaded. (advanced) \cr
#'    \code{\link{inputs}} \tab List of loaded objects used in simulation. (advanced) \cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot.
#'
#' @param hidden Logical. If TRUE, show the default core modules.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.7 on Modules and dependencies.
#'
#' @aliases simList-accessors-modules
#' @author Alex Chubaty
#' @export
#' @include simList-class.R
#' @rdname simList-accessors-modules
setGeneric("modules", function(sim, hidden = FALSE) {
  standardGeneric("modules")
})

#' @rdname simList-accessors-modules
setMethod(
  "modules",
  signature = "simList",
  definition = function(sim, hidden) {
    if (hidden) {
      mods <- sim@modules
    } else {
      hiddenMods <- unlist(sim@modules) %in% (.pkgEnv$.coreModules %>% unlist())
      mods <- sim@modules[!hiddenMods]
    }
    return(mods)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("modules<-",
           function(sim, value) {
             standardGeneric("modules<-")
})

#' @name modules<-
#' @aliases modules<-,simList-method
#' @rdname simList-accessors-modules
setReplaceMethod("modules",
                 signature = "simList",
                 function(sim, value) {
                   sim@modules <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams modules
#' @export
#' @include simList-class.R
#' @rdname simList-accessors-modules
#'
setGeneric("depends", function(sim) {
  standardGeneric("depends")
})

#' @export
#' @rdname simList-accessors-modules
setMethod("depends",
          signature("simList"),
          definition = function(sim) {
            return(sim@depends)
})

#' @export
#' @rdname simList-accessors-modules
setGeneric("depends<-",
           function(sim, value) {
             standardGeneric("depends<-")
})

#' @name depends<-
#' @aliases depends<-,simList-method
#' @rdname simList-accessors-modules
#' @export
setReplaceMethod("depends",
                 signature("simList"),
                 function(sim, value) {
                   sim@depends <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Namespacing within \code{SpaDES}
#'
#' \code{.callingModuleName} returns the name of the module that is currently
#' the active module calling functions like \code{scheduleEvent}.
#' This will only return the module name if it is inside a \code{spades} call,
#' i.e., it will return \code{NULL} if used in interactive mode.
#' The related function \code{currentModule} is simply a rapid accessor for the
#' current module name. This latter will return the module that is in the current
#' event queue, which will never be \code{NULL}.
#'
#' @inheritParams modules
#'
#' @author Eliot McIntire
#' @export
#' @importFrom reproducible .grepSysCalls
#' @include simList-class.R
#' @keywords internal
#' @rdname namespacing
#'
setGeneric(".callingModuleName", function(sim) {
  standardGeneric(".callingModuleName")
})

#' @export
#' @rdname namespacing
setMethod(
  ".callingModuleName",
  signature = c("simList"),
  definition = function(sim) {
    # Only return module name if inside a spades call,
    #  because this only makes sense if there is an "active" module
    sc <- sys.calls()
    st <- .grepSysCalls(sc, pattern = "moduleCall")
    #rep(FALSE, length = length(sc))
    #st <- grepl(sc, pattern = "moduleCall")
    if (length(st)) {
      mod <- parse(text = "moduleCall") %>%
        eval(., envir = sys.frame(st[1] - 1)) %>%
        strsplit(., split = "\\.")[[1]][2]
    } else {
      mod <- NULL
    }
    return(mod)
})

#' @export
#' @rdname namespacing
setGeneric("currentModule", function(sim) {
  standardGeneric("currentModule")
})

#' @export
#' @rdname namespacing
setMethod(
  "currentModule",
  signature = c("simList"),
  definition = function(sim) {
    ret <- sim@current$moduleName
    if (length(ret))
      return(ret)
    else
      return(character(0))
})

################################################################################
#' Get and set simulation parameters
#'
#' \code{params} and \code{P} access the parameter slot in the \code{simList}.
#' \code{params} has a replace method, so can be used to update a parameter value.
#'
#' @inheritParams objs
#'
#' @param value The parameter value to be set (in the corresponding `module` and `param`).
#'
#' @param module Optional character string indicating which module params should come from.
#'
#' @param param Optional character string indicating which parameter to choose.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @note The differences between P, params and being explicit with passing arguments
#' are mostly a question of speed and code compactness.
#' The computationally fastest way to get a parameter is to specify moduleName and parameter name, as in:
#' \code{P(sim, "paramName", "moduleName")} (replacing moduleName and paramName with your
#' specific module and parameter names), but it is more verbose than P(sim)$paramName. Note: the important
#' part for speed (e.g., 2-4x faster) is specifying the moduleName.
#' Specifying the parameter name is <5% faster.
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.1 on Simulation parameters.
#'
#' @aliases parameters
#' @aliases simList-accessors-params
#' @export
#' @include simList-class.R
#' @rdname params
setGeneric("params", function(sim) {
  standardGeneric("params")
})

#' @export
#' @rdname params
setMethod("params",
          signature = "simList",
          definition = function(sim) {
            return(sim@params)
})

#' @export
#' @rdname params
setGeneric("params<-",
           function(sim, value) {
             standardGeneric("params<-")
})

#' @name params<-
#' @aliases params<-,simList-method
#' @rdname params
#' @export
setReplaceMethod("params",
                 signature = "simList",
                 function(sim, value) {
                   sim@params <- value
                   validObject(sim)
                   return(sim)
})

#' \code{P} is a concise way to access parameters within a module. It works more like
#' a namespaced function in the sense that the module from which it is called is the
#' default place it will look for the parameter. To access a parameter from within
#' a module, you can use \code{P(sim)$paramName} instead of
#' \code{params(sim)$moduleName$paramName}
#'
#' @aliases simList-accessors-params
#' @export
#' @importFrom reproducible .grepSysCalls
#' @importFrom utils getSrcFilename
#' @include simList-class.R
#' @rdname params
P <- function(sim, param, module) UseMethod("P")

#' @export
P.simList <- function(sim, param, module) {
  if (missing(sim)) stop("P takes a simList as first argument")
  # Check for changed order

  # first check if inside an event
  module1 <- sim@current$moduleName
  if (length(module1) == 0) {
    # then check if inside a .inputObjects call
    inSimInit <- .grepSysCalls(sys.calls(), pattern = "(^.parseModule)")
    if (length(inSimInit)) {
      module1 <- get("m", sys.frame(inSimInit[2]))
    } else {
      inManualCall <- .grepSysCalls(sys.calls(), pattern = "(\\.mods\\$|\\[)")
      if (length(inManualCall)) { # this is the case where a user calls the function using the full path
        #   sim$.mods$module$fn(sim)
        pp <- parse(text = sys.calls()[[inManualCall[1]]])
        gg <- gsub("^.+\\.mods(\\$|\\[\\[)", "", as.character(pp)[[1]])
        module1 <- strsplit(gg, split = "\\$|\\[")[[1]][1]
      } else {
        mods <- modules(sim)
        modFilePaths <- checkPath(names(mods))

        scalls <- sys.calls();
        whereInSC <-  .grepSysCalls(scalls, "^P\\(")
        while (whereInSC > 1) {
          poss <- scalls[whereInSC - 1]
          fn <- as.character(poss[[1]][[1]])
          fn <- try(get(fn, envir = sys.frames()[whereInSC - 1][[1]]), silent = TRUE)
          if (!is(fn, "try-error")) {

            modulePath <- getSrcFilename(fn, full.names = TRUE)
            if (length(modulePath) > 0) {
              module1 <- lapply(modFilePaths, function(x) grep(pattern = x, checkPath(modulePath)))
              if (length(module1[[1]]) > 0)  break
            }
          }
          whereInSC <- whereInSC - 1
        }

        if (length(module1[[1]])) {
          module1 <- mods[[module1[[1]]]]
        }
      }
    }
  }

  reversalMessage <- paste0("P has changed the order of its parameters, with 'param' now second to ",
                            " allow for easier access to specific parameters inside a module. ",
                            "It looks like this call to P is still using the old order with 'module' second. ",
                            "Returning the old behaviour for now; this may not reliably work in the future. ",
                            "Please change the order")
  # This is to catch cases of reverse order -- order of args changed to sim, params, then module
  if (!missing(param)) {
    if (param %in% names(sim@params)) {# test if the param is a module name
      if (!missing(module)) {
        if (module %in% ls(sim@params[[param]], all.names = TRUE) ||
            module %in% .knownDotParams) {
          warning(reversalMessage)
          module1 <- param
          param <- module
        }
      } else {

        # Module missing, only have parameter --> this could be old case of P(sim, module = "something")
        if (param %in% ls(sim@params[[param]], all.names = TRUE) ||
            module1 %in% ls(sim@params)) {
          # module1 is in list of modules; param is not in parameters --> this is likely a reversal
          warning(reversalMessage)
          param <- NULL
        }
      }
    } else {
      module1 <- module
    } # param is not a module name --> probably using new parameter order --> correct
  } else {
    param <- NULL
    if (!missing(module))
      module1 <- module
  }
  module <- module1

  if (length(module) == 0) {
    return(sim@params)
  }

  if (is.null(param)) {
    return(sim@params[[module]])
  } else {
    return(sim@params[[module]][[param]])
  }
}

#' @rdname params
#' @export
`P<-` <- function(sim, param, module, value) {
  if (missing(sim)) stop("P takes a simList as first argument")
  if (missing(module)) {
    # first check if inside an event
    module <- sim@current$moduleName
    if (length(module) == 0) {
      # then check if inside a .inputObjects call
      inSimInit <- .grepSysCalls(sys.calls(), pattern = "(^.parseModule)")
      if (any(inSimInit)) {
        module <- get("m", sys.frame(inSimInit[2]))
      }
    }
    if (length(module) == 0) {
      # if not in either place, it is a fail
      stop("Please specify module")
    }
  }
  if (missing(param)) {
    if (is(value, "list")) {
      if (all(names(P(sim, module = module)) %in% names(value))) {
        sim@params[[module]] <- value
      } else {
        stop("Please specify param as an argument")
      }
    } else {
      stop("Please specify param as an argument")
    }
  } else {
    sim@params[[module]][[param]] <- value
  }
  return(sim)
}

################################################################################
#' Get and set global simulation parameters
#'
#' \code{globals}, and the alias \code{G}, accesses or sets the "globals"
#' in the \code{simList}. This currently is not an explicit slot in the \code{simList},
#' but it is a \code{.globals} element in the \code{params} slot of the \code{simList}.
#'
#' @inheritParams params
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.1 on Simulation Parameters.
#'
#' @export
#' @include simList-class.R
#' @rdname globals
setGeneric("globals", function(sim) {
  standardGeneric("globals")
})

#' @export
#' @rdname globals
setMethod("globals",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$.globals)
})

#' @export
#' @rdname globals
setGeneric("globals<-",
           function(sim, value) {
             standardGeneric("globals<-")
})

#' @aliases globals<-,simList-method
#' @export
#' @name globals<-
#' @rdname globals
setReplaceMethod("globals",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$.globals <- value
                   validObject(sim)
                   return(sim)
})

#' @export
#' @rdname globals
setGeneric("G", function(sim) {
  standardGeneric("G")
})

#' @export
#' @rdname globals
setMethod("G",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$.globals)
})

#' @export
#' @rdname globals
setGeneric("G<-",
           function(sim, value) {
             standardGeneric("G<-")
})

#' @name G<-
#' @aliases G<-,simList-method
#' @rdname globals
#' @export
setReplaceMethod("G",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$.globals <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @details
#' \code{parameters} will extract only the metadata with the metadata defaults,
#' NOT the current values that may be overwritten by a user. See examples.
#' @inheritParams params
#' @param asDF Logical. For \code{parameters}, if TRUE, this will produce a single
#'                 data.frame of all model parameters. If FALSE, then it will return
#'                 a data.frame with 1 row for each parameter within nested lists,
#'                 with the same structure as \code{params}.
#'
#' @include simList-class.R
#' @export
#' @rdname params
#' @examples
#' modules <- list("randomLandscapes")
#' paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#' mySim <- simInit(modules = modules, paths = paths,
#'                  params = list(.globals = list(stackName = "landscape")))
#'
#' # update some parameters using assignment -- currently only params will work
#' params(mySim)$randomLandscapes$nx <- 200
#' params(mySim)$randomLandscapes$ny <- 200
#'
#' parameters(mySim) # Does not contain these user overridden values
#'
#' # These next 2 are same here because they are not within a module
#' P(mySim)          # Does contain the user overridden values
#' params(mySim)     # Does contain the user overridden values
#'
#' # NOTE -- deleting a parameter will affect params and P, not parameters
#' params(mySim)$randomLandscapes$nx <- NULL
#' params(mySim)$randomLandscapes$ny <- NULL
#'
#' parameters(mySim) # Shows nx and ny
#'
#' # These next 2 are same here because they are not within a module
#' P(mySim)          # nx and ny are Gone
#' params(mySim)     # nx and ny are Gone
setGeneric("parameters", function(sim, asDF = FALSE) {
  standardGeneric("parameters")
})

#' @export
#' @rdname params
setMethod("parameters",
          signature = "simList",
          definition = function(sim, asDF) {
            if (any(!unlist(lapply(sim@depends@dependencies, is.null)))) {
              if (asDF) {
                tmp <- lapply(sim@depends@dependencies, function(x) {
                  out <- x@parameters
                })
                tmp <- do.call(rbind, tmp)
              } else {
                tmp <- lapply(sim@depends@dependencies, function(x) {
                  out <- lapply(seq_len(NROW(x@parameters)),
                                function(y) x@parameters[y, -1])
                  names(out) <- x@parameters$paramName
                  out
                })
              }
            } else {
              tmp <- NULL
            }
            return(tmp)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @rdname checkpoint
#' @family functions to access elements of a 'simList' object
#'
setGeneric("checkpointFile", function(sim) {
  standardGeneric("checkpointFile")
})

#' @export
#' @rdname checkpoint
setMethod("checkpointFile",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$checkpoint$file)
})

#' @export
#' @rdname checkpoint
setGeneric("checkpointFile<-",
           function(sim, value) {
             standardGeneric("checkpointFile<-")
})

#' @name checkpointFile<-
#' @aliases checkpointFile<-,simList-method
#' @rdname checkpoint
#' @export
setReplaceMethod("checkpointFile",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$checkpoint$file <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams params
#' @export
#' @include simList-class.R
#' @rdname checkpoint
#'
setGeneric("checkpointInterval", function(sim) {
  standardGeneric("checkpointInterval")
})

#' @export
#' @rdname checkpoint
setMethod("checkpointInterval",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$checkpoint$interval)
})

#' @export
#' @rdname checkpoint
setGeneric("checkpointInterval<-",
           function(sim, value) {
             standardGeneric("checkpointInterval<-")
})

#' @name checkpointInterval<-
#' @aliases checkpointInterval<-,simList-method
#' @rdname checkpoint
#' @export
setReplaceMethod("checkpointInterval",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$checkpoint$interval <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Get and set simulation progress bar details
#'
#' The progress bar can be set in two ways in SpaDES. First, by setting values
#' in the .progress list element in the params list element passed to \code{\link{simInit}}.
#' Second, at the \code{\link{spades}} call itself, which can be simpler. See examples.
#'
#' @details Progress Bar:
#' Progress type can be one of  \code{"text"}, \code{"graphical"}, or \code{"shiny"}.
#' Progress interval can be a numeric.
#' These both can get set by passing a
#' \code{.progress = list(type = "graphical", interval = 1)} into the \code{simInit} call.
#' See examples.
#'
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @family functions to access elements of a 'simList' object
#' @rdname progress
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(
#'   times = list(start=0.0, end=100.0),
#'   params = list(.globals = list(stackName = "landscape"),
#'   .progress = list(type = "text", interval = 10),
#'   checkpoint = list(interval = 10, file = "chkpnt.RData")),
#'   modules = list("randomLandscapes"),
#'   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core")))
#'
#' # progress bar
#' progressType(mySim) # "text"
#' progressInterval(mySim) # 10
#'
#' # parameters
#' params(mySim) # returns all parameters in all modules
#'               # including .global, .progress, checkpoint
#' globals(mySim) # returns only global parameters
#'
#' # checkpoint
#' checkpointFile(mySim) # returns the name of the checkpoint file
#'                       # In this example, "chkpnt.RData"
#' checkpointInterval(mySim) # 10
#' }
setGeneric("progressInterval", function(sim) {
  standardGeneric("progressInterval")
})

#' @export
#' @rdname progress
setMethod("progressInterval",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$.progress$interval)
})

#' @export
#' @rdname progress
setGeneric("progressInterval<-",
           function(sim, value) {
             standardGeneric("progressInterval<-")
})

#' @name progressInterval<-
#' @aliases progressInterval<-,simList-method
#' @rdname progress
#' @export
setReplaceMethod("progressInterval",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$.progress$interval <- value
                   validObject(sim)
                   return(sim)
})

################################################################################
#' @inheritParams params
#' @include simList-class.R
#' @export
#' @rdname progress
#'
setGeneric("progressType", function(sim) {
  standardGeneric("progressType")
})

#' @export
#' @rdname progress
setMethod("progressType",
          signature = "simList",
          definition = function(sim) {
            return(sim@params$.progress$type)
})

#' @export
#' @rdname progress
setGeneric("progressType<-",
           function(sim, value) {
             standardGeneric("progressType<-")
})

#' @name progressType<-
#' @aliases progressType<-,simList-method
#' @rdname progress
#' @export
setReplaceMethod("progressType",
                 signature = "simList",
                 function(sim, value) {
                   sim@params$.progress$type <- as.character(value)
                   validObject(sim)
                   return(sim)
})

################################################################################
#' Simulation inputs
#'
#' Accessor functions for the \code{inputs} slots in a \code{simList} object.
#'
#' These functions are one of three mechanisms to add the information about which input files
#' to load in a \code{spades} call.
#' \enumerate{
#'   \item As arguments to a \code{simInit} call. Specifically, \code{inputs} or \code{outputs}.
#'         See \code{?simInit}.
#'   \item With the \code{outputs(simList)} function call.
#'   \item By adding a function called \code{.inputObjects} inside a module, which will be executed
#'         during the \code{simInit} call. This last way is the most "modular" way to create
#'         default data sets for your model.
#' }
#'
#' See below for more details.
#'
#' @section inputs function or argument in \code{simInit}:
#'
#' \code{inputs} accepts a data.frame, with up to 7 columns.
#' Columns are:
#'
#' \tabular{ll}{
#' \code{file} \tab required, a character string indicating the file path. There is no
#' default.\cr
#'
#' \code{objectName} \tab optional, character string indicating the name of the object
#' that the loaded file will be assigned to in the \code{simList}. This object
#' can therefore be accessed with \code{sim$xxx} in any module, where
#' \code{objectName = "xxx"}. Defaults to the filename without file extension or
#' directory information.\cr
#'
#' \code{fun} \tab optional, a character string indicating the function to use to
#' load that file. Defaults to the known extensions in \code{SpaDES} (found by
#' examining \code{.fileExtensions()}). The \code{package} and \code{fun} can be
#' jointly specified here as \code{"packageName::functionName"}, e.g.,
#' \code{"raster::raster"}.\cr
#'
#' \code{package} \tab optional character string indicating the package in
#' which to find the \code{fun});\cr
#'
#' \code{loadTime} \tab optional numeric, indicating when in simulation time the file
#' should be loaded. The default is the highest priority at \code{start(sim)},
#' i.e., at the very start. \cr
#'
#' \code{interval} \tab optional numeric, indicating at what interval should this same
#' exact file  be reloaded from disk, e.g,. 10 would mean every 10 time units. The
#' default is NA or no interval, i.e, load the file only once as described in
#' \code{loadTime} \cr
#'
#' \code{arguments} \tab is a list of lists of named arguments, one list for each
#' \code{fun}. For example, if \code{fun="raster"}, \code{arguments = list(native = TRUE)}
#' will pass the argument "native = TRUE" to raster.  If there is only one list,
#' then it is assumed to apply to all files and will be recycled as per normal R
#' rules of recycling for each \code{fun}.\cr
#' }
#'
#' Currently, only \code{file} is required. All others will be filled with defaults
#' if not specified.
#'
#' See the modules vignette for more details (\code{browseVignettes("SpaDES.core")}).
#'
#' @section \code{.inputObjects} function placed inside module:
#'
#' Any code placed inside a function called \code{.inputObjects} will be run during
#' \code{simInit()} for the purpose of creating any objects required by this module,
#' i.e., objects  identified in the \code{inputObjects} element of \code{defineModule}.
#' This is useful if there is something required before simulation to produce the module
#' object dependencies, including such things as downloading default datasets, e.g.,
#' \code{downloadData('LCC2005', modulePath(sim))}.
#' Nothing should be created here that does not create an named object in \code{inputObjects}.
#' Any other initiation procedures should be put in the "init" eventType of the doEvent function.
#' Note: the module developer can use 'sim$.userSuppliedObjNames' inside the function to
#' selectively skip unnecessary steps because the user has provided those inputObjects in the
#' simInit call. e.g., the following code would look to see if the user had passed \code{defaultColor}
#' into during \code{simInit}. If the user had done this, then this function would not override
#' that value with 'red'. If the user has not passed in a value for \code{defaultColor}, then
#' the module will get it here:
#'
#' \code{if (!('defaultColor' \%in\% sim$.userSuppliedObjNames)) \{
#'  sim$defaultColor <- 'red'
#' \}}
#'
#' @inheritParams objs
#'
#' @param value The object to be stored at the slot. See Details.
#'
#' @return Returns or sets the value(s) of the \code{input} or \code{output} slots
#' in the \code{simList} object.
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.2 on loading and saving.
#'
#' @include simList-class.R
#' @importFrom data.table is.data.table
#' @importFrom stats na.omit
#' @export
#' @name inputs
#' @aliases simList-accessors-inout
#' @rdname simList-accessors-inputs
#' @example inst/examples/example_inputs.R
setGeneric("inputs", function(sim) {
  standardGeneric("inputs")
})

#' @export
#' @rdname simList-accessors-inputs
setMethod("inputs",
          signature = "simList",
          definition = function(sim) {

            simUnit <- sim@simtimes[["timeunit"]]
            loadTimeUnit <- attr(sim@inputs$loadTime, "unit")
            if (is.null(loadTimeUnit)) loadTimeUnit <- simUnit
            out <- if (is.na(pmatch(loadTimeUnit, simUnit)) &
                       (length(sim@inputs$loadTime) > 0)) {
              # note the above line captures empty loadTime,
              # whereas is.na does not
              if (any(!is.na(sim@inputs$loadTime))) {
                if (!is.null(sim@inputs$loadTime)) {
                  obj <- copy(sim@inputs) # don't change original sim
                  set(obj, NULL, j = "loadTime", convertTimeunit(obj$loadTime, obj$unit, sim@.xData))
                  #obj[, loadTime := convertTimeunit(loadTime, unit, sim@.xData)]
                  obj[]
                }
              } else {
                sim@inputs
              }
            } else {
              sim@inputs
            }

            return(out)
})

#' @export
#' @rdname simList-accessors-inputs
setGeneric("inputs<-",
           function(sim, value) {
             standardGeneric("inputs<-")
})

#' @name inputs<-
#' @aliases inputs<-,simList-method
#' @rdname simList-accessors-inputs
#' @export
setReplaceMethod(
  "inputs",
  signature = "simList",
  function(sim, value) {
   if (length(value) > 0) {
     whFactors <- sapply(value, function(x) is.factor(x))
     if (any(whFactors)) {
       value[, whFactors] <- sapply(value[, whFactors], as.character)
     }

     if (!is.data.frame(value)) {
       if (!is.list(value)) {
         stop("inputs must be a list, data.frame")
       }
        value <- data.frame(value, stringsAsFactors = FALSE)
     }
     sim@inputs <- .fillInputRows(value, start(sim))
   } else {
     sim@inputs <- value
   }
   # Deal with objects and files differently... if files (via inputs arg in simInit)...
     # Deal with file names
     # 2 things: 1. if relative, concatenate inputPath
     #           2. if absolute, don't use inputPath
   if (NROW(value) > 0) {
     sim@inputs[is.na(sim@inputs$file), "file"] <- NA

     # If a filename is provided, determine if it is absolute path, if so,
     # use that, if not, then append it to inputPath(sim)
     isAP <- isAbsolutePath(as.character(sim@inputs$file))
     sim@inputs[!isAP & !is.na(sim@inputs$file), "file"] <-
       file.path(inputPath(sim),
                 sim@inputs$file[!isAP & !is.na(sim@inputs$file)])

     if (!all(names(sim@inputs) %in% .fileTableInCols)) {
       stop(paste("input table can only have columns named",
                  paste(.fileTableInCols, collapse = ", ")))
     }
     if (any(is.na(sim@inputs[, "loaded"]))) {
       if (!all(is.na(sim@inputs[, "loadTime"]))) {
         newTime <- sim@inputs[is.na(sim@inputs$loaded), "loadTime"]
         attributes(newTime)$unit <- sim@simtimes[["timeunit"]]

         for (nT in newTime) {
           attributes(nT)$unit <- timeunit(sim)
           sim <- scheduleEvent(sim, nT, "load", "inputs", .first() - 1)
         }
         toRemove <- duplicated(rbindlist(list(current(sim), events(sim))),
                                by = c("eventTime", "moduleName", "eventType"))
         if (any(toRemove)) {
           if (NROW(current(sim)) > 0)
             toRemove <- toRemove[-seq_len(NROW(current(sim)))]
           events(sim) <- events(sim)[!toRemove]
         }

       } else {
         sim@inputs[is.na(sim@inputs$loadTime), "loadTime"] <-
           sim@simtimes[["current"]]
         newTime <- sim@inputs[is.na(sim@inputs$loaded), "loadTime"] %>%
           min(., na.rm = TRUE)
         attributes(newTime)$unit <- "seconds"
         sim <- scheduleEvent(sim, newTime, "load", "inputs", .first() - 1)
       }
     }
   }

   return(sim)
})

################################################################################
#' Simulation outputs
#'
#' Accessor functions for the \code{outputs} slots in a \code{simList} object.
#'
#' These functions are one of three mechanisms to add information about which output files to save.
#' \enumerate{
#'   \item As arguments to a \code{simInit} call. Specifically, \code{inputs} or \code{outputs}.
#'         See \code{?simInit}.
#'   \item With the \code{outputs(simList)} function call.
#'   \item By adding a function called \code{.inputObjects} inside a module, which will be executed
#'         during the \code{simInit} call. This last way is the most "modular" way to create
#'         default data sets for your model.
#' }
#'
#' See below for more details.
#'
#' @section outputs function or argument in \code{simInit}:
#'
#' \code{outputs} accepts a data.frame similar to the \code{inputs} data.frame, but
#' with up to 6 columns.
#'
#' \tabular{ll}{
#' \code{objectName} \tab required, character string indicating the name of the object
#' in the \code{simList} that will be saved to disk (without the \code{sim$} prefix).\cr
#'
#' \code{file} \tab optional, a character string indicating the file path to save to.
#' The default is to concatenate \code{objectName} with the model timeunit and
#' \code{saveTime}, separated by underscore, '\code{_}'. So a default filename would be
#' \code{"Fires_year1.rds"}.\cr
#'
#' \code{fun} \tab optional, a character string indicating the function to use to
#' save that file. The default is \code{\link{saveRDS}} \cr
#'
#' \code{package} \tab optional character string indicating the package in
#' which to find the \code{fun});\cr
#'
#' \code{saveTime} \tab optional numeric, indicating when in simulation time the file
#' should be saved. The default is the lowest priority at \code{end(sim)},
#' i.e., at the very end. \cr
#'
#' \code{arguments} \tab is a list of lists of named arguments, one list for each
#' \code{fun}. For example, if \code{fun = "write.csv"},
#' \code{arguments = list(row.names = TRUE)} will pass the argument
#' \code{row.names = TRUE} to \code{write.csv}  If there is only one list,
#' then it is assumed to apply to all files and will be recycled as per normal R
#' rules of recycling for each \code{fun}.\cr
#' }
#'
#' See the modules vignette for more details (\code{browseVignettes("SpaDES.core")}).
#'
#' @note The automatic file type handling only adds the correct extension from a given
#' \code{fun} and \code{package}. It does not do the inverse, from a given extension find the
#' correct \code{fun} and \code{package}.
#'
#' @inheritParams inputs
#'
#' @export
#' @include simList-class.R
#' @importFrom data.table := data.table
#' @importFrom stats na.omit
#' @name outputs
#' @rdname simList-accessors-outputs
#'
#' @example inst/examples/example_outputs.R
setGeneric("outputs", function(sim) {
  standardGeneric("outputs")
})

#' @export
#' @rdname simList-accessors-outputs
setMethod(
  "outputs",
  signature = "simList",
  definition = function(sim) {
    simUnit <- sim@simtimes[["timeunit"]]
    saveTimeUnit <- attr(sim@outputs$saveTime, "unit")
    if (is.null(saveTimeUnit)) saveTimeUnit <- simUnit

    out <- if (is.na(pmatch(saveTimeUnit, simUnit)) &
               length(sim@outputs$saveTime) > 0) {
      ## note the above line captures empty saveTime, whereas is.na does not
      if (any(!is.na(sim@outputs$saveTime))) {
        if (!is.null(sim@outputs$saveTime)) {
          obj <- copy(sim@outputs) # don't change original sim
          obj[, saveTime := convertTimeunit(saveTime, unit, sim@.xData)]
          obj[]
          obj
        }
      } else {
        sim@outputs
      }
    } else {
      sim@outputs
    }
    return(out)
  })

#' @export
#' @rdname simList-accessors-outputs
setGeneric("outputs<-",
           function(sim, value) {
             standardGeneric("outputs<-")
})

#' @aliases outputs<-,simList-method
#' @export
#' @importFrom data.table setDT
#' @name outputs<-
#' @rdname simList-accessors-outputs
setReplaceMethod(
  "outputs",
  signature = "simList",
  function(sim, value) {
    if (NROW(value)) {
       if (!is.data.frame(value)) {
         if (!is.list(value)) {
           stop("outputs must be a list or data.frame")
         }
         value <- data.frame(value, stringsAsFactors = FALSE)
       }

       sim@outputs <- .fillOutputRows(value, end(sim))

       # coerce any factors to the correct class
       for (col in which(sapply(sim@outputs, is.factor))) {
         sim@outputs[, col] <- as(sim@outputs[[col]], class(.fileTableOut()[[col]]))
       }

       # if saveTime not provided, give it end(sim)
       sim@outputs[is.na(sim@outputs$saveTime), "saveTime"] <-
         end(sim, sim@simtimes[["timeunit"]])
       attributes(sim@outputs$saveTime)$unit <- sim@simtimes[["timeunit"]]

       # Deal with file names
       # 3 things: 1. if relative, concatenate outputPath
       #           2. if absolute, don't use outputPath
       #           3. concatenate time to file name in all cases
       # If no filename provided, use the object name
       sim@outputs[is.na(sim@outputs$file), "file"] <-
         paste0(sim@outputs$objectName[is.na(sim@outputs$file)])
       # If a filename is provided, determine if it is absolute path, if so,
       # use that, if not, then append it to outputPath(sim)
       alreadyWithOutputPath <- grepl(pattern = paste0("^", outputPath(sim)), sim@outputs$file)
       if (any(!alreadyWithOutputPath)) {
         isAP <- isAbsolutePath(as.character(sim@outputs$file))

         sim@outputs[!isAP[!alreadyWithOutputPath], "file"] <-
           file.path(outputPath(sim), sim@outputs$file[!isAP])
       }

       # If there is no function provided, then use saveRDS, from package base
       sim@outputs[is.na(sim@outputs$fun), "fun"] <- "saveRDS"
       sim@outputs[is.na(sim@outputs$package), "package"] <- "base"

       # file extension stuff
       fileExts <- .saveFileExtensions()
       fe <- setDT(fileExts)[setDT(sim@outputs[,c("fun", "package")]), on = c("fun","package")]$exts

       # grep allows for file extensions from 1 to 5 characters
       wh <- !grepl(pattern = "\\..{1,5}$", sim@outputs$file) &
         (nzchar(fe, keepNA = TRUE))
       sim@outputs[wh, "file"] <- paste0(sim@outputs[wh, "file"], ".", fe[wh])

       # If the file name already has a time unit on it,
       # i.e., passed explicitly by user, then don't postpend again
       txtTimeA <- paste0(attr(sim@outputs[, "saveTime"], "unit"))
       txtTimeB <- paddedFloatToChar(
         sim@outputs[, "saveTime"],
         ceiling(log10(end(sim, sim@simtimes[["timeunit"]]) + 1))
       )
       # Add time unit and saveTime to filename, without stripping extension
       wh <- !grepl(txtTimeA, sim@outputs$file)
       sim@outputs[wh, "file"] <- paste0(
         filePathSansExt(sim@outputs[wh, "file"]),
         "_", txtTimeA, txtTimeB[wh],
         ifelse(nzchar(fileExt(sim@outputs[wh, "file"]), keepNA = TRUE) , ".", ""),
         ifelse(nzchar(fileExt(sim@outputs[wh, "file"]), keepNA = TRUE) ,
                fileExt(sim@outputs[wh, "file"]),
                "")
       )
     } else {
       sim@outputs <- value
     }

     if (!all(.fileTableOutCols %in% names(sim@outputs))) {
       stop(paste("output table must have columns named",
                  paste(.fileTableOutCols, collapse = ", ")))
     }

    return(sim)
})

################################################################################
#' \code{inputArgs} and \code{outputArgs} are ways to specify any arguments that are needed for
#' file loading and file saving. This is still somewhat experimental.
#'
#' @inheritParams inputs
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-inputs
#'
setGeneric("inputArgs", function(sim) {
  standardGeneric("inputArgs")
})

#' @export
#' @rdname simList-accessors-inputs
setMethod("inputArgs",
          signature = "simList",
          definition = function(sim) {
            return(sim@inputs[[.fileTableInCols[pmatch("arg", .fileTableInCols)]]])
})

#' @export
#' @rdname simList-accessors-inputs
setGeneric("inputArgs<-",
           function(sim, value) {
             standardGeneric("inputArgs<-")
})

#' @name inputArgs<-
#' @aliases inputArgs<-,simList-method
#' @rdname simList-accessors-inputs
#' @export
setReplaceMethod(
  "inputArgs",
  signature = "simList",
  function(sim, value) {
   if (is.list(value) & !is.data.frame(value)) {
     sim@inputs$args <- value
   } else if (is.null(value)) {
     sim@inputs$args <- rep(list(NULL), NROW(inputs(sim)))
   } else {
     stop("value passed to inputArgs() must be a list of named elements")
   }

   validObject(sim)
   return(sim)
})

#' @inheritParams inputs
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-outputs
setGeneric("outputArgs", function(sim) {
  standardGeneric("outputArgs")
})

#' @export
#' @rdname simList-accessors-outputs
setMethod("outputArgs",
          signature = "simList",
          definition = function(sim) {
            return(sim@outputs$arg)
})

#' @export
#' @rdname simList-accessors-outputs
setGeneric("outputArgs<-",
           function(sim, value) {
             standardGeneric("outputArgs<-")
})

#' @name outputArgs<-
#' @aliases outputArgs<-,simList-method
#' @rdname simList-accessors-outputs
#' @export
setReplaceMethod(
  "outputArgs",
  signature = "simList",
  function(sim, value) {
    argName <- .fileTableOutCols[pmatch("arg", .fileTableOutCols)]
   if (is.list(value) & !is.data.frame(value)) {
     sim@outputs[[argName]] <- value
   } else if (is.null(value)) {
     sim@outputs[[argName]] <- rep(list(NULL), NROW(outputs(sim)))
   } else {
     stop("value passed to outputArgs() must be a list of named elements")
   }
   validObject(sim)
   return(sim)
})

################################################################################
#' Specify paths for modules, inputs, outputs, and temporary rasters
#'
#' Accessor functions for the \code{paths} slot in a \code{simList} object.
#'
#' These are ways to add or access the file paths used by \code{\link{spades}}.
#' There are five file paths: \code{cachePath}, \code{modulePath},
#' \code{inputPath}, \code{outputPath}, and \code{rasterPath}.
#' Each has a function to get or set the value in a \code{simList} object.
#' If no paths are specified, the defaults are as follows:
#'
#' \itemize{
#'   \item \code{cachePath}: \code{getOption("reproducible.cachePath")};
#'   \item \code{inputPath}: \code{getOption("spades.modulePath")};
#'   \item \code{modulePath}: \code{getOption("spades.inputPath")};
#'   \item \code{outputPath}: \code{getOption("spades.outputPath")};
#'   \item \code{rasterPath}: \code{raster::tmpDir()}
#' }
#'
#' @inheritParams params
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @family functions to access elements of a 'simList' object
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.4 on Simulation Paths.
#'
#' @include simList-class.R
#' @importFrom stats na.omit
#' @export
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#'
setGeneric("paths", function(sim) {
  standardGeneric("paths")
})

#' @export
#' @rdname simList-accessors-paths
setMethod("paths",
          signature = "simList",
          definition = function(sim) {
            return(sim@paths)
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("paths<-",
           function(sim, value) {
             standardGeneric("paths<-")
})

#' @name paths<-
#' @aliases paths<-,simList-method
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
#' @export
setReplaceMethod(
  "paths",
  signature = "simList",
  function(sim, value) {
    N <- 4 # total number of named paths (cache, madule, input, output)

    # get named elements and their position in value list
    wh <- pmatch(names(sim@paths), names(value)) # always length 4, NA if no name match, number if yes
    whValueNamed <- which(!is.na(pmatch(names(value), names(sim@paths)))) # length of names of value
    whValueUnnamed <- rep(TRUE, length(value))
    if (length(whValueNamed)) whValueUnnamed[whValueNamed] <- FALSE

    # keep named elements, use unnamed in remaining order:
    #  cache, input, module, output
    # if (length(na.omit(wh)) < length(value)) {
    #   wh1 <- !(wh[1:length(value)] %in% (1:N)[1:length(value)])
    #   wh2 <- !((1:N)[1:length(value)] %in% wh[1:length(value)])
    #   if (length(wh1) < N) wh1 <- c(wh1, rep(FALSE, N - length(wh1)))
    #   if (length(wh2) < N) wh2 <- c(wh2, rep(FALSE, N - length(wh2)))
    #   wh[wh1] <- (1:N)[wh2]
    # }

    # start with .paths()
    emptyOnes <- unlist(lapply(sim@paths, is.null))
    if (sum(emptyOnes) > 0) sim@paths[emptyOnes] <- .paths()[emptyOnes]

    # override with named ones
    sim@paths[!is.na(wh)] <- value[na.omit(wh)]

    #sim@paths[is.na(wh)] <- .paths()[is.na(wh)]
    # keep named elements, use unnamed in remaining order:
    #  cache, input, module, output
    if (length(na.omit(wh)) < length(value)) {
      whichNamed <- which(!is.na(wh))
      whichUnnamed <- (1:length(sim@paths))
      if (length(whichNamed) > 0) whichUnnamed <- whichUnnamed[-whichNamed]
      sim@paths[whichUnnamed][seq_len(sum(whValueUnnamed))] <- value[whValueUnnamed]
    }

    # Don't need to create an archive in the paths directory, just have to create
    #  the directory
    checkPath(sim@paths$cachePath, create = TRUE)

    validObject(sim)
    return(sim)
})

# cachePath ----------------------------------------------------------------------------------

#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
#'
setGeneric("cachePath", function(sim) {
  standardGeneric("cachePath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("cachePath",
          signature = "simList",
          definition = function(sim) {
            return(sim@paths$cachePath)
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("cachePath<-",
           function(sim, value) {
             standardGeneric("cachePath<-")
})

#' @name cachePath<-
#' @aliases cachePath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "cachePath",
  signature = "simList",
  function(sim, value) {
    sim@paths$cachePath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})

# inputPath ----------------------------------------------------------------------------------

#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#'
setGeneric("inputPath", function(sim) {
  standardGeneric("inputPath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("inputPath",
          signature = "simList",
          definition = function(sim) {
            return(sim@paths$inputPath)
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("inputPath<-",
           function(sim, value) {
             standardGeneric("inputPath<-")
})

#' @name inputPath<-
#' @aliases inputPath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "inputPath",
  signature = "simList",
  function(sim, value) {
    sim@paths$inputPath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})


# outputPath ----------------------------------------------------------------------------------

#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#'
setGeneric("outputPath", function(sim) {
  standardGeneric("outputPath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("outputPath",
          signature = "simList",
          definition = function(sim) {
            return(sim@paths$outputPath)
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("outputPath<-",
           function(sim, value) {
             standardGeneric("outputPath<-")
})

#' @name outputPath<-
#' @aliases outputPath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "outputPath",
  signature = "simList",
  function(sim, value) {
    sim@paths$outputPath <- unname(unlist(value))
    checkPath(sim@paths$outputPath, create = TRUE)
    if (NROW(outputs(sim)) > 0) {
     if ("saved" %in% colnames(outputs(sim))) {
       notYetSaved <- !outputs(sim)$saved | is.na(outputs(sim)$saved)
       outputs(sim)$file[notYetSaved] <- file.path(sim@paths$outputPath,
                                                   basename(outputs(sim)$file[notYetSaved]))
     }
    }
    validObject(sim)
    return(sim)
})

# modulePath ----------------------------------------------------------------------------------

#' @inheritParams paths
#' @param module The optional character string of the module(s) whose
#'               paths are desired. If omitted, will return all modulePaths,
#'               if more than one exist.
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("modulePath", function(sim, module) {
  standardGeneric("modulePath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("modulePath",
          signature = "simList",
          definition = function(sim, module) {
            if (!missing(module)) {
              mods <- unlist(lapply(sim@modules, function(x) x %in% module))
              dirname(names(mods)[mods])
            } else {
              sim@paths$modulePath
            }
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("modulePath<-",
           function(sim, value) {
             standardGeneric("modulePath<-")
})

#' @name modulePath<-
#' @aliases modulePath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "modulePath",
  signature = "simList",
  function(sim, value) {
    sim@paths$modulePath <- unname(unlist(value))
    validObject(sim)
    return(sim)
})


# scratchPath ---------------------------------------------------------------------------------

#' @inheritParams paths
#'
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("scratchPath", function(sim) {
  standardGeneric("scratchPath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("scratchPath",
          signature = "simList",
          definition = function(sim) {
            sim@paths$scratchPath
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("scratchPath<-",
           function(sim, value) {
             standardGeneric("scratchPath<-")
})

#' @name scratchPath<-
#' @aliases scratchPath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "scratchPath",
  signature = "simList",
  function(sim, value) {
    sim@paths$scratchPath <- unname(unlist(value))
    checkPath(sim@paths$scratchPath, create = TRUE)
    validObject(sim)
    return(sim)
})


# rasterPath ----------------------------------------------------------------------------------

#' @inheritParams paths
#'
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("rasterPath", function(sim) {
  standardGeneric("rasterPath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("rasterPath",
          signature = "simList",
          definition = function(sim) {
            sim@paths$rasterPath
})

#' @export
#' @rdname simList-accessors-paths
setGeneric("rasterPath<-",
           function(sim, value) {
             standardGeneric("rasterPath<-")
})

#' @name rasterPath<-
#' @aliases rasterPath<-,simList-method
#' @aliases simList-accessors-paths
#' @rdname simList-accessors-paths
#' @export
setReplaceMethod(
  "rasterPath",
  signature = "simList",
  function(sim, value) {
    sim@paths$rasterPath <- unname(unlist(value))
    checkPath(sim@paths$rasterPath, create = TRUE)
    validObject(sim)
    return(sim)
})

#' @description
#' \code{dataPath} will return \code{file.path(modulePath(sim), currentModule(sim), "data")}.
#' \code{dataPath}, like \code{currentModule},is namespaced. This means that when
#' it is used inside a module, then it will return \emph{that model-specific} information.
#' For instance, if used inside a module called \code{"movingAgent"},
#' then \code{currentModule(sim)}
#' will return \code{"movingAgent"}, and \code{dataPath(sim)} will return
#' \code{file.path(modulePath(sim), "movingAgent", "data")}
#'
#' @inheritParams paths
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setGeneric("dataPath", function(sim) {
  standardGeneric("dataPath")
})

#' @export
#' @rdname simList-accessors-paths
#' @aliases simList-accessors-paths
setMethod("dataPath",
          signature = "simList",
          definition = function(sim) {
            return(file.path(modulePath(sim, currentModule(sim)), currentModule(sim), "data"))
})

################################################################################
#' Time usage in \code{SpaDES}
#'
#' Functions for the \code{simtimes} slot of a \code{simList} object
#' and its elements. To maintain modularity, the behaviour of these functions depends
#' on where they are used. In other words, different modules can have their own
#' timeunit. \code{SpaDES} converts these to seconds when running a simulation, but
#' shows the user time in the units of the model as shown with \code{timeunit(sim)}
#'
#' @note These have default behaviour that is based on the calling frame timeunit.
#' When used inside a module, then the time is in the units of the module.
#' If used in an interactive mode, then the time will be in the units of the
#' simulation.
#'
#' Additional methods are provided to access the current, start, and end times
#' of the simulation:
#'
#' \tabular{ll}{
#'    \code{time} \tab Current simulation time.\cr
#'    \code{start} \tab Simulation start time.\cr
#'    \code{end} \tab Simulation end time.\cr
#'    \code{timeunit} \tab Simulation timeunit.\cr
#'    \code{timeunits} \tab Module timeunits.\cr
#'    \code{times} \tab List of all simulation times (current, start, end, timeunit).\cr
#' }
#'
#' @param unit   Character. One of the time units used in \code{SpaDES}.
#'
#' @param x A \code{simList}
#'
#' @param value  A time, given as a numeric, optionally with a unit attribute,
#'               but this will be deduced from the model time units or module
#'               time units (if used within a module).
#'
#' @param ...    Additional parameters.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.5 on Simulation times;
#'   \code{\link{elapsedTime}},
#'
#' @aliases simList-accessors-times
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @family functions to access elements of a 'simList' object
#' @include simList-class.R
#' @include times.R
#' @rdname simList-accessors-times
setGeneric("times", function(x, ...) {
  standardGeneric("times")
})

#' @export
#' @rdname simList-accessors-times
#' @aliases simList-accessors-times
setMethod(
  "times",
  signature = "simList",
  definition = function(x) {
    mUnit <- .callingFrameTimeunit(x)
    if (is.null(mUnit)) {
      mUnit <- NA_character_
    }
    t <- list(
      current = time(x, x@simtimes[["timeunit"]]),
      start = start(x, x@simtimes[["timeunit"]]),
      end = end(x, x@simtimes[["timeunit"]]),
      timeunit = x@simtimes[["timeunit"]]
    )
    return(t)
})

#' @export
#' @rdname simList-accessors-times
#' @aliases simList-accessors-times
setGeneric("times<-", function(x, value) {
  standardGeneric("times<-")
})

#' @name times<-
#' @aliases times<-,simList-method
#' @export
#' @rdname simList-accessors-times
#' @aliases simList-accessors-times
setReplaceMethod(
  "times",
   signature = "simList",
   function(x, value) {
     value <- as.list(value)
     if (!all(is(value$current, "numeric"),
            is(value$start, "numeric"),
            is(value$end, "numeric"),
            is(value$timeunit, "character"))) {
       stop("Please supply a named list, current, start, end, and timeunit")
     }

     if (is.null(attributes(value$current)$unit))
       attributes(value$current)$unit <- value$timeunit
     if (is.null(attributes(value$start)$unit))
       attributes(value$start)$unit <- value$timeunit
     if (is.null(attributes(value$end)$unit))
       attributes(value$end)$unit <- value$timeunit

     x@simtimes$current <- convertTimeunit(value$current, "second", x@.xData)
     x@simtimes$start <- convertTimeunit(value$start, "second", x@.xData)
     x@simtimes$end <- convertTimeunit(value$end, "second", x@.xData)
     x@simtimes$timeunit <- value$timeunit

     validObject(x)

     return(x)
})

################################################################################
#' @export
#' @importFrom stats time
#' @include simList-class.R
#' @include times.R
#' @rdname simList-accessors-times
time.simList <- function(x, unit, ...) {
    if (missing(unit)) {
      unit <- .callingFrameTimeunit(x)
      if (is.null(unit)) {
        unit <- NA_character_
      }
    }
    if (isTRUE(!startsWith(unit, "second"))) {

      t <- convertTimeunit(x@simtimes[["current"]], unit, x@.xData,
                           skipChecks = TRUE)
      return(t)
    }
    t <- x@simtimes[["current"]]
    return(t)
}

#' @aliases simList-accessors-times
#' @export
#' @rdname simList-accessors-times
setGeneric("time<-", function(x, value) {
  standardGeneric("time<-")
})

#' @aliases time<-,simList-method
#' @aliases simList-accessors-times
#' @export
#' @name time<-
#' @rdname simList-accessors-times
setReplaceMethod(
  "time",
   signature = "simList",
   function(x, value) {
     if (is.null(attributes(value)$unit)) {
       attributes(value)$unit <- x@simtimes[["timeunit"]]
     }
     x@simtimes$current <- convertTimeunit(value, "second", x@.xData)

     if (!is.numeric(x@simtimes$current)) stop("time must be a numeric")
     if (!any(pmatch(.spadesTimes, attr(x@simtimes$current, "unit")))) {
       stop("time must be one of", paste(.spadesTimes, collapse = ", "))
     }
     return(x)
})

################################################################################
#' @export
#' @importFrom stats end
#' @include times.R
#' @include simList-class.R
#' @rdname simList-accessors-times
end <- function(x, ...) UseMethod("end")

#' @export
#' @rdname simList-accessors-times
end.simList <- function(x, unit, ...) {
    if (missing(unit)) {
      unit <- .callingFrameTimeunit(x)
      if (is.null(unit))
        unit <- NA_character_
    }
    if (!is.na(unit)) {
      if (is.na(pmatch("second", unit))) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$end, unit, x@.xData)
        return(t)
      }
    }
    t <- x@simtimes$end
    return(t)
}

#' @aliases simList-accessors-times
#' @export
#' @rdname simList-accessors-times
setGeneric("end<-", function(x, value) {
  standardGeneric("end<-")
})

#' @aliases end<-,simList-method
#' @aliases simList-accessors-times
#' @export
#' @name end<-
#' @rdname simList-accessors-times
setReplaceMethod(
  "end",
  signature = "simList",
  function(x, value) {
    # convert time units, if required
    if (is.null(attributes(value)$unit)) {
      attributes(value)$unit <- x@simtimes[["timeunit"]]
    }
    x@simtimes$end <- convertTimeunit(value, "second", x@.xData)
    validObject(x)
    return(x)
})

################################################################################
#' @export
#' @importFrom stats start
#' @include simList-class.R
#' @include times.R
#' @rdname simList-accessors-times
start <- function(x, ...) UseMethod("start")

#' @export
#' @rdname simList-accessors-times
start.simList <- function(x, unit = NULL, ...) {
    if (is.null(unit)) {
      unit <- .callingFrameTimeunit(x)
      if (is.null(unit)) {
        unit <- NA_character_
      }
    }

    if (!is.na(unit)) {
      if (is.na(pmatch("second", unit))) {
        # i.e., if not in same units as simulation
        t <- convertTimeunit(x@simtimes$start, unit, x@.xData)
        return(t)
      }
    }
    t <- x@simtimes$start
    return(t)
}

#' @export
#' @rdname simList-accessors-times
#' @aliases simList-accessors-times
setGeneric("start<-", function(x, value) {
  standardGeneric("start<-")
})

#' @name start<-
#' @aliases start<-,simList-method
#' @aliases simList-accessors-times
#' @rdname simList-accessors-times
setReplaceMethod(
  "start",
   signature = "simList",
   function(x, value) {
     # convert time units, if required
     if (is.null(attributes(value)$unit)) {
       attributes(value)$unit <- x@simtimes[["timeunit"]]
     }
     x@simtimes$start <- convertTimeunit(value, "second", x@.xData)
     validObject(x)
     return(x)
})

################################################################################
#' @inheritParams times
#' @include simList-class.R
#' @include times.R
#' @keywords internal
#' @rdname namespacing
.callingFrameTimeunit <- function(x) {
  if (is.null(x)) return(NULL)
  mod <- x@current[["moduleName"]]
  out <- x@simtimes[["timeunit"]] # default -- whole simList
  if (!is.null(x@.xData[[".timeunits"]])) {
    outPoss <- x@.xData[[".timeunits"]]
  } else {
    outPoss <- timeunits(x)
  }
  outPoss <- if (length(mod) > 0) {
    outPoss[[mod]]
  } else {
    out
  }
  if (!is.null(outPoss)) out <- unlist(outPoss)
  return(out)
}

################################################################################
#' @details \code{timeunit} will extract the current units of the time used in a
#' simulation (i.e., within a \code{spades} call).
#' If it is set within a \code{simInit}, e.g.,
#' \code{times=list(start=0, end=52, timeunit = "week")}, it will set the
#' units for that simulation.
#' By default, a \code{simInit} call will use the smallest unit contained within
#' the metadata for the modules being used. If there are parent modules, then the
#' parent module timeunit will be used even if one of its children is a smaller timeunit.
#' If all modules, including parents, are set to \code{NA}, \code{timeunit} defaults to seconds.
#' If parents are set to \code{NA}, then the set of modules defined by that parent module
#' will be given the smallest units of the children.
#'
#' Currently, available units are "second", "hours", day", "week", "month", and
#' "year" can be used in the metadata of a module.
#'
#' The user can also define a new unit. The unit name can be anything, but the
#' function definition must be of the form \code{dunitName}, e.g., \code{dyear}
#' or \code{dfortnight}.
#' The unit name is the part without the \code{d} and the function name definition
#' includes the \code{d}. This new function, e.g.,
#' \code{dfortnight <- function(x) lubridate::duration(dday(14))}
#' can be placed anywhere in the search path or in a module.
#'
#' @aliases simList-accessors-times
#' @export
#' @include simList-class.R
#' @rdname simList-accessors-times
setGeneric("timeunit", function(x) {
  standardGeneric("timeunit")
})

#' @aliases simList-accessors-times
#' @export
#' @rdname simList-accessors-times
setMethod("timeunit",
          signature = "simList",
          definition = function(x) {
            return(x@simtimes[["timeunit"]])
})

#' @aliases simList-accessors-times
#' @export
#' @rdname simList-accessors-times
setGeneric("timeunit<-",
           function(x, value) {
             standardGeneric("timeunit<-")
})

#' @aliases timeunit<-,simList-method
#' @aliases simList-accessors-times
#' @export
#' @name timeunit<-
#' @rdname simList-accessors-times
setReplaceMethod(
  "timeunit",
  signature = "simList",
  function(x, value) {
    value <- as.character(value)
    if (checkTimeunit(value, envir = x@.xData)) {
        x@simtimes$timeunit <- value
    } else {
      x@simtimes$timeunit <- NA_character_
    }
    validObject(x)
    return(x)
})

################################################################################
#' @details \code{timeunits} will extract the current units of the time of all
#' modules used in a simulation.
#' This is different from \code{timeunit} because it is not necessarily
#' associated with a \code{spades} call.
#'
#' In many cases, the "simpler" use of each of these functions may be slower
#' computationally. For instance, it is much faster to use \code{time(sim, "year")}
#' than \code{time(sim)}. So as a module developer, it is advantageous to
#' write out the longer one, minimizing the looking up that R must do.
#'
#' @aliases simList-accessors-times
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-times
setGeneric("timeunits", function(x) {
  standardGeneric("timeunits")
})

#' @aliases simList-accessors-times
#' @export
#' @rdname simList-accessors-times
setMethod(
  "timeunits",
  signature = "simList",
  definition = function(x) {
    isNonParent <- !unlist(lapply(x@depends@dependencies, function(y) {
      if (!is.null(y)) {
        length(y@childModules) > 0
      } else {
        FALSE
      }
    }))
    if (all(unlist(lapply(x@depends@dependencies[isNonParent], is.null)))) {
      timestepUnits <- list(simInitDefaults()$times$timeunit)
    } else {
      timestepUnits <- lapply(x@depends@dependencies[isNonParent], function(y) {
        y@timeunit
      })
      names(timestepUnits) <- unlist(lapply(x@depends@dependencies[isNonParent], function(y) {
        y@name
      }))
    }
    return(timestepUnits)
})

################################################################################
#' Simulation event lists
#'
#' Accessor functions for the \code{events} and \code{completed} slots of a
#' \code{simList} object. These path functions will extract the values that were
#' provided to the \code{simInit} function in the \code{path} argument.
#'
#' By default, the event lists are shown when the \code{simList} object is printed,
#' thus most users will not require direct use of these methods.
#' \tabular{ll}{
#'    \code{events} \tab Scheduled simulation events (the event queue).\cr
#'    \code{completed} \tab Completed simulation events.\cr
#' }
#'
#' Currently, only get and set methods are defined. Subset methods are not.
#'
#' @note Each event is represented by a \code{\link{data.table}} row consisting of:
#'  \itemize{
#'    \item \code{eventTime}: The time the event is to occur.
#'    \item \code{moduleName}: The module from which the event is taken.
#'    \item \code{eventType}: A character string for the programmer-defined event type.
#'  }
#'
#' @inheritParams objs
#'
#' @param unit   Character. One of the time units used in \code{SpaDES}.
#'
#' @param value The object to be stored at the slot.
#'
#' @return Returns or sets the value of the slot from the \code{simList} object.
#'
#' @seealso \code{\link{SpaDES.core-package}}, specifically the section 1.2.6 on Simulation event queues.
#'
#' @aliases simList-accessors-events
#' @export
#' @family functions to access elements of a 'simList' object
#' @importFrom data.table := copy data.table
#' @importFrom stats setNames
#' @include simList-class.R
#' @rdname simList-accessors-events
setGeneric("events", function(sim, unit) {
  standardGeneric("events")
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod(
  "events",
  signature = c("simList", "character"),
  definition = function(sim, unit) {
    obj <- rbindlist(sim@events)
    if (length(unit) != 1) stop("unit must be length 1")
    if (is.na(pmatch("second", unit)) && (length(sim@events) > 0)) {
      # note the above line captures empty eventTime, whereas is.na does not
      if (any(!is.na(obj$eventTime))) {
        if (!is.null(obj$eventTime)) {
          obj[, eventTime := convertTimeunit(eventTime, unit, sim@.xData)]
          obj[]
        }
       } #else {
    }
    return(obj)
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod("events",
          signature = c("simList", "missing"),
          definition = function(sim, unit) {
            res <- events(sim, sim@simtimes[["timeunit"]])
            return(res)
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setGeneric("events<-",
           function(sim, value) {
             standardGeneric("events<-")
})

#' @aliases events<-,simList-method
#' @aliases simList-accessors-events
#' @export
#' @name events<-
#' @rdname simList-accessors-events
setReplaceMethod(
  "events",
   signature = "simList",
   function(sim, value) {
     if (!is(value, "data.table")) stop("Event queue must be a data.table")
     if (!identical(names(value), .emptyEventListCols))
       stop("Event queue must be a data.table with columns: ",
            paste(.emptyEventListCols, collapse = ", "), ".")
     if (is.null(attributes(value$eventTime)$unit)) {
       attributes(value$eventTime)$unit <- sim@simtimes[["timeunit"]]
     }
     if (is.na(pmatch("second", attributes(value$eventTime)$unit))) {
       value[, eventTime := convertTimeunit(eventTime, "second", sim@.xData)]
     }

     if (NROW(value)) {
       sim@events <- lapply(seq_along(1:NROW(value)), function(x) as.list(value[x,]))
     } else {
       sim@events <- list()
     }
     return(sim)
})

#############################
#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setGeneric("conditionalEvents", function(sim, unit) {
  standardGeneric("conditionalEvents")
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod(
  "conditionalEvents",
  signature = c("simList", "character"),
  definition = function(sim, unit) {
    if (length(unit) != 1) stop("unit must be length 1")
    if (exists("._conditionalEvents", envir = sim, inherits = FALSE)) {
      conds <- sim$._conditionalEvents
      conds <- lapply(conds, function(x) {
        if (is.call(x$condition)) {
          x$condition <- deparse(x$condition);
        } else {
          x$condition <- as.character(x$condition);
        }
        x
      })
      obj <- rbindlist(conds)
      if (is.na(pmatch("second", unit)) &&
          (length(conds) > 0)) {
        # note the above line captures empty eventTime, whereas is.na does not
        if (any(!is.na(obj$minEventTime)) && (any(!is.na(obj$maxEventTime)))) {
          if (!is.null(obj$minEventTime) && !is.null(obj$maxEventTime)) {
            obj[, minEventTime := convertTimeunit(minEventTime, unit, sim@.xData)]
            obj[, maxEventTime := convertTimeunit(maxEventTime, unit, sim@.xData)]
            obj[]
          }
        }
      }
      return(obj)
    } else {
      return(NULL)
    }
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod("conditionalEvents",
          signature = c("simList", "missing"),
          definition = function(sim, unit) {
            res <- conditionalEvents(sim, sim@simtimes[["timeunit"]])
            return(res)
})

################################################################################
#' @inheritParams events
#'
#' @aliases simList-accessors-events
#' @export
#' @importFrom data.table rbindlist
#' @importFrom stats setNames
#' @include simList-class.R
#' @rdname simList-accessors-events
#'
setGeneric("current", function(sim, unit) {
  standardGeneric("current")
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod(
  "current",
  signature = c("simList", "character"),
  definition = function(sim, unit) {
    out <- if (is.na(pmatch("second", unit)) & (length(sim@current$eventTime))) {
      # note the above line captures empty eventTime, whereas `is.na` does not
      if (any(!is.na(sim@current$eventTime))) {
        if (!is.null(sim@current$eventTime)) {
          sim@current$eventTime <- convertTimeunit(sim@current$eventTime, unit, sim@.xData)
          sim@current
        }
      } else {
        sim@current
      }
    } else {
      sim@current
    }
    return(rbindlist(list(out)))
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod("current",
          signature = c("simList", "missing"),
          definition = function(sim, unit) {
            out <- current(sim, sim@simtimes[["timeunit"]])
            return(out)
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setGeneric("current<-",
           function(sim, value) {
             standardGeneric("current<-")
})

#' @aliases current<-,simList-method
#' @aliases simList-accessors-events
#' @export
#' @name current<-
#' @rdname simList-accessors-events
setReplaceMethod("current",
                 signature = "simList",
                 function(sim, value) {
                   if (!is(value, "data.table")) stop("Event queue must be a data.table")
                   if (!identical(names(value), .emptyEventListCols)) {
                     stop("Event queue must be a data.table with columns: ",
                          paste(.emptyEventListCols, collapse = ", "), ".")
                   }
                   sim@current <- as.list(value)
                   return(sim)
})

################################################################################
#' @inheritParams events
#' @param times Logical. Should this function report the clockTime
#'
#' @aliases simList-accessors-events
#' @export
#' @importFrom data.table := data.table
#' @importFrom stats setNames
#' @include simList-class.R
#' @rdname simList-accessors-events
setGeneric("completed", function(sim, unit, times = TRUE) {
  standardGeneric("completed")
})

#' @aliases simList-accessors-events
#' @export
#' @importFrom data.table rbindlist set setkeyv :=
#' @rdname simList-accessors-events
setMethod(
  "completed",
  signature = c("simList", "character"),
  definition = function(sim, unit, times = TRUE) {

    obj <- as.list(sim@completed)
    obj <- rbindlist(obj, idcol = if (length(sim@completed)) "eventNumber" else NULL)

    if (length(sim@completed)) {
      obj[, eventNumber := as.numeric(eventNumber)]
      setkeyv(obj, "eventNumber")
      if (!isTRUE(times)) {
        set(obj, NULL, "._clockTime", NULL)
      }
      if (is.na(pmatch("second", unit)) & (length(sim@completed))) {
        # note the above line captures empty eventTime, whereas `is.na` does not
        if (any(!is.na(obj$eventTime))) {
          if (!is.null(obj$eventTime)) {
            if (!is.null(obj$._clockTime))
              obj[, `:=`(eventTime = convertTimeunit(eventTime, unit, sim@.xData),
                         clockTime = obj$._clockTime,
                         ._clockTime = NULL)]
          }
        }
      }
      obj[]
      set(obj, NULL, "eventNumber", NULL) # remove the eventNumber column to match other event queues
    }
    return(obj)
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setMethod("completed",
          signature = c("simList", "missing"),
          definition = function(sim, unit, times = TRUE) {
            out <- completed(sim, sim@simtimes[["timeunit"]], times = times)
            return(out)
})

#' @aliases simList-accessors-events
#' @export
#' @rdname simList-accessors-events
setGeneric("completed<-",
           function(sim, value) {
             standardGeneric("completed<-")
})

#' @aliases completed<-,simList-method
#' @aliases simList-accessors-events
#' @export
#' @name completed<-
#' @rdname simList-accessors-events
setReplaceMethod(
  "completed",
  signature = "simList",
  function(sim, value) {
    if (!is(value, "data.table")) stop("Completed queue must be a data.table")
    if (!identical(names(value), .emptyEventListCols)) {
      stop("Event queue must be a data.table with columns, ",
        paste(.emptyEventListCols, collapse = ", "), ".")
    }
    sim@completed <- new.env(parent = emptyenv())
    if (NROW(value)) {
      integerVals <- seq(NROW(value))
      outList <- lapply(integerVals,
                      function(x) as.list(value[x, ]))
      names(outList) <- as.character(integerVals)
      list2env(outList, envir = sim@completed)
    }
    return(sim)
})

################################################################################
#' Add simulation dependencies
#'
#' Internal function.
#' Adds a \code{\link{.moduleDeps}} object to the simulation dependency list.
#'
#' @inheritParams objs
#'
#' @param x   A named list containing the parameters used to construct a new
#'            \code{\link{.moduleDeps}} object.
#'
#' @return A \code{simList} object.
#'
#' @include simList-class.R
#' @family functions to access elements of a 'simList' object
#' @keywords internal
#' @rdname addDepends
#'
#' @author Alex Chubaty
#'
setGeneric(".addDepends", function(sim, x) {
  standardGeneric(".addDepends")
})

#' @rdname addDepends
setMethod(
  ".addDepends",
  signature(sim = "simList", x = ".moduleDeps"),
  definition = function(sim, x) {
    deps <- sim@depends
    n <- length(deps@dependencies)
    if (n == 1L) {
      if (is.null(deps@dependencies[[1L]])) n <- 0L
    }
    deps@dependencies[[n + 1L]] <- x
    dupes <- which(duplicated(deps@dependencies))
    if (length(dupes)) deps@dependencies <- deps@dependencies[-dupes]
    depends(sim) <- deps
    return(sim)
})

################################################################################
#' Get module or simulation package dependencies
#'
#' @param sim  A \code{simList} object.
#'
#' @param modules Character vector, specifying the name or
#'             vector of names of module(s)
#' @param paths Character vector, specifying the name or
#'             vector of names of paths(s) for those modules. If path not specified,
#'             it will be taken from getOption("spades.modulePath"), which is set
#'             with \code{setPaths})
#' @param filenames Character vector specifying filenames of modules (i.e.
#'                 combined path & module. If this is specified, then \code{modules} and
#'                 \code{path} are ignored.
#' @param clean Optional logical. If \code{TRUE}, it will scrub any references to
#'              github repositories, e.g., "PredictiveEcology/reproducible" will be
#'              returned as "reproducible"
#'
#' @inheritParams .parseModulePartial
#'
#' @return A sorted character vector of package names.
#'
#' @aliases simList-accessors-packages
#' @author Alex Chubaty & Eliot McIntire
#' @export
#' @include simList-class.R
#' @family functions to access elements of a 'simList' object
#' @rdname packages
#'
# igraph exports %>% from magrittr
setGeneric("packages", function(sim, modules, paths, filenames, envir,
                                clean = FALSE, ...) {
  standardGeneric("packages")
})

#' @export
#' @rdname packages
#' @aliases simList-accessors-packages
setMethod(
  "packages",
  signature(sim = "ANY"),
  definition = function(sim, modules, paths, filenames, envir,
                        clean = FALSE, ...) {
    if (missing(sim)) { # can either have no sim, or can have a sim that is incomplete,
                        #   i.e., with no reqdPkgs slot filled
      depsInSim <- list(NULL)
    } else {
      depsInSim <- sim@depends@dependencies
      if (is.null(depsInSim[[1]])) return(character()) # basically for empty simList objects
    }

    if (!is.null(depsInSim[[1]])) { # check within dependencies slot for any elements,
                                    #  if not NULL, one will be reqdPkgs
      pkgs <- lapply(depsInSim, function(x) {
        x@reqdPkgs
      }) %>% unlist() %>% c("SpaDES.core") %>% unique()
      if (!is.null(pkgs)) pkgs <- sort(pkgs)
    } else {
      if (!missing(filenames))  {
        paths <- filenames
        if (missing(modules)) {
          modules <- sub(basename(paths), replacement = "", pattern = ".R")
        }
      } else if (!missing(modules)) {
        prefix <- if (!all(file.exists(modules))) {
          if (!missing("paths")) {
            pre <- paths
          } else {
            pre <- getOption("spades.modulePath")
          }
          file.path(pre, modules)
        } else {
          modules
        }
        paths <- file.path(prefix, paste0(modules, ".R"))
      } else {
        stop("one of sim, modules, or filename must be supplied.")
      }

      if (missing(envir)) {
        envir <- NULL
      }

      pkgs <- lapply(paths, function(paths) {
        pkgs <- .parseModulePartial(filename = paths, defineModuleElement = "reqdPkgs",
                                    envir = envir) %>%
          unlist() %>% unique()
        if (!is.null(pkgs)) {
          pkgs <- sort(pkgs)
        } else {
          pkgs <- character(0)
        }
        pkgs <- pkgs[nzchar(pkgs)]
        pkgs <- unique(c("SpaDES.core", pkgs))
        return(pkgs)
      })
      names(pkgs) <- modules
    }
    if (isTRUE(clean)) {
      pkgs <- gsub(".*\\/+(.+)(@.*)",  "\\1", pkgs)
      pkgs <- gsub(".*\\/+(.+)",  "\\1", pkgs)
    }
    return(pkgs)
})

################################################################################
#' Metadata accessors
#'
#' These accessors extract the metadata for a module (if specified) or all modules
#' in a \code{simList} if not specified.
#'
#' @inheritParams P
#' @param path The path to the module., i.e., the \code{modulePath}.
#'    Only relevant if \code{sim} not supplied.
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
#'
setGeneric("inputObjects", function(sim, module, path) {
  standardGeneric("inputObjects")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("inputObjects",
          signature(sim = "simList"),
          definition = function(sim, module, path) {
            if (missing(module)) {
              module <- current(sim)[["moduleName"]]
              if (NROW(module) == 0)
                module <- unlist(modules(sim))
            }
            module <- setdiff(module, .coreModules())
            out <- if (length(module) > 0) {
              if (length(module) > 1) {
                lapply(sim@depends@dependencies[module], function(deps)
                  deps@inputObjects)
              } else {
                sim@depends@dependencies[[module]]@inputObjects
              }
            } else {
              ._inputObjectsDF()
            }
            return(out)
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("inputObjects",
          signature(sim = "missing"),
          definition = function(sim, module, path) {
            out <- inputOrOutputObjects(type = "inputObjects", module = module, path = path)
            return(out)
})

inputOrOutputObjects <- function(type, module, path) {
  if (missing(path)) {
    path <- getPaths()$modulePath
  }
  names(module) <- module
  mm <- lapply(module, function(m)
    moduleMetadata(module = m, path = path, defineModuleListItems = type)[[type]])
  mm
}

################################################################################
#' @inheritParams P
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
#'
setGeneric("outputObjects", function(sim, module, path) {
  standardGeneric("outputObjects")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("outputObjects",
          signature = "simList",
          definition = function(sim, module, path) {
            if (missing(module)) {
              module <- current(sim)[["moduleName"]]
              if (NROW(module) == 0)
                module <- unlist(modules(sim))
            }
            module <- setdiff(module, .coreModules())
            out <- if (length(module) > 0) {
              if (length(module) > 1) {
                lapply(sim@depends@dependencies[module], function(deps)
                  deps@outputObjects)
              } else {
                sim@depends@dependencies[[module]]@outputObjects
              }
            } else {
              ._outputObjectsDF()
            }
            return(out)
          })

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("outputObjects",
          signature(sim = "missing", module = "ANY"),
          definition = function(sim, module, path) {
            inputOrOutputObjects(type = "outputObjects", module = module, path = path)
})

################################################################################
#' @inheritParams P
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
#'
setGeneric("outputObjectNames", function(sim, module) {
  standardGeneric("outputObjectNames")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("outputObjectNames",
          signature = "simList",
          definition = function(sim, module) {
            out <- if (NROW(modules(sim)) > 0) {
              outObjs <- outputObjects(sim)
              allObjNames <- if (is(outObjs, "list")) {
                allObjNames <- lapply(outObjs, function(x) x$objectName)
                if (!missing(module)) {
                  allObjNames <- allObjNames[[module]]
                }
                allObjNames
              } else {
                list(outObjs$objectName)
              }
            } else {
              NULL
            }
            return(out)
})

################################################################################
#' @inheritParams P
#' @param module Character vector of module name(s)
#' @param modulePath That path where \code{module} can be found. If set already
#'   using \code{setPaths}, it will use that. This will be ignored if \code{sim}
#'   is supplied and is required if \code{sim} not supplied
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
#'
#' @examples
#' \dontrun{
#' # To pre-install and pre-load all packages prior to \code{simInit}.
#'
#' # set modulePath
#' setPaths(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#' # use Require and reqdPkgs
#' if (!interactive()) chooseCRANmirror(ind = 1) #
#' pkgs <- reqdPkgs(module = c("caribouMovement", "randomLandscapes", "fireSpread"))
#' pkgs <- unique(unlist(pkgs))
#' Require(pkgs)
#' }
setGeneric("reqdPkgs", function(sim, module, modulePath) {
  standardGeneric("reqdPkgs")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("reqdPkgs",
          signature = "simList",
          definition = function(sim, module, modulePath) {
            if (missing(module)) {
              module <- current(sim)
              if (NROW(module) == 0)
                module <- unlist(modules(sim))
            }
            out <- if (length(module) > 1) {
              lapply(sim@depends@dependencies[module], function(deps)
                unlist(deps@reqdPkgs))
            } else {
              unlist(sim@depends@dependencies[[module]]@reqdPkgs)
            }
            return(out)
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("reqdPkgs",
          signature = "missing",
          definition = function(sim, module, modulePath) {
            if (missing(modulePath))
              modulePath <- getOption("spades.modulePath")
            names(module) <- module
            out <- lapply(module, function(mod) {
              unlist(.parseModulePartial(filename = file.path(modulePath, mod, paste0(mod, ".R")),
                                  defineModuleElement = "reqdPkgs") )
            })
            return(out)
})

################################################################################
#' @inheritParams P
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
#'
setGeneric("documentation", function(sim, module) {
  standardGeneric("documentation")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("documentation",
          signature = "simList",
          definition = function(sim, module) {
            if (missing(module)) {
              module <- current(sim)
              if (NROW(module) == 0)
                module <- unlist(modules(sim))
            }
            out <- if (length(module) > 1) {
              lapply(sim@depends@dependencies[module], function(deps)
                deps@documentation)
            } else {
              sim@depends@dependencies[[module]]@documentation
            }
            return(out)
})

################################################################################
#' @param package For compatibility with \code{\link[utils]{citation}}. This can be
#'                a \code{simList} or a character string for a package name.
#' @inheritParams P
#' @inheritParams utils::citation
#' @include simList-class.R
#' @export
#' @rdname simList-accessors-metadata
#'
#' @aliases simList-accessors-metadata
setGeneric("citation", function(package, lib.loc = NULL, auto = NULL, module = character()) {
  standardGeneric("citation")
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("citation",
          signature = "simList",
          definition = function(package, lib.loc, auto, module) {
            if (missing(module)) {
              module <- current(package)
              if (NROW(module) == 0)
                module <- unlist(modules(package))
            }
            out <- if (length(module) > 1) {
              lapply(package@depends@dependencies[module], function(deps)
                deps@citation)
            } else {
              package@depends@dependencies[[module]]@citation
            }
            return(out)
})

#' @export
#' @rdname simList-accessors-metadata
#' @aliases simList-accessors-metadata
setMethod("citation",
          signature = "character",
          definition = function(package, lib.loc, auto, module) {
            utils::citation(package = package, lib.loc = lib.loc, auto = auto)
})

################################################################################
#' @export
#' @include simList-class.R
#' @include times.R
#' @rdname simList-accessors-times
elapsedTime <- function(x, ...) UseMethod("elapsedTime")

#' @param byEvent Logical. If \code{TRUE}, the elapsed time will be by module and event;
#'                \code{FALSE} will report only by module. Default is \code{TRUE}.
#'
#' @inheritParams base::difftime
#' @export
#' @rdname simList-accessors-times
#' @examples
#'
#' # Elapsed Time
#' s1 <- simInit()
#' s2 <- spades(s1)
#' elapsedTime(s2)
#' elapsedTime(s2, units = "mins")
elapsedTime.simList <- function(x, byEvent = TRUE, units = "auto", ...) {
  comp <- completed(x)

  if (!is.null(comp)) {
    comp <- comp[, list(moduleName, eventType,
                          diffTime = diff(c(x@.xData[["._firstEventClockTime"]], clockTime)))]
    theBy <- if (isTRUE(byEvent)) {
      c("moduleName", "eventType")
    } else {
      c("moduleName")
    }
    ret <- comp[, list(elapsedTime = sum(diffTime)), by = theBy] #nolint
    a <- ret$elapsedTime
    if (identical(units, "auto")) {
      st <- Sys.time()
      a <- a + st - st # work around for forcing a non seconds unit, allowing "auto"
    } else {
      # This one won't allow "auto"
      units(a) <- units
    }
    ret[, elapsedTime := a]
  } else {
    ret <- NULL
  }
  return(ret[])
}


.knownDotParams <- c(".plots", ".plotInitialTime", ".plotInterval", ".saveInitialTime", ".saveInterval", ".useCache")
