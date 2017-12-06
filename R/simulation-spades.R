if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

################################################################################
#' Process a simulation event
#'
#' Internal function called from \code{spades}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim Character string for the \code{simList} simulation object.
#'
#' @param debug Optional. Either Logical or character. If logical, entire \code{simList}
#'              will be printed at each event. If a character string, then it can be one
#'              of the many simList accessors, such as \code{events}, \code{params}.
#'              It can also be any R expression that will be evaluated with access
#'              to the \code{sim} object.
#'              If \code{"current"} is used, then it will be a compact list of the events
#'              as they go by.
#' @inheritParams spades
#' @return Returns the modified \code{simList} object.
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Fransisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table data.table rbindlist setkey
#' @importFrom stringi stri_pad_right stri_pad stri_length
#' @importFrom reproducible Cache
#' @include helpers.R
#' @keywords internal
#' @rdname doEvent
#'
doEvent <- function(sim, debug, notOlderThan) {
  if (missing(debug)) debug <- FALSE
  if (class(sim) != "simList") {
    # use inherits()?
    stop("doEvent can only accept a simList object")
  }

  # core modules
  core <- .pkgEnv$.coreModules

  cur <- sim@current
  if (NROW(cur) == 0) {
    #evnts <- sim@events #events(sim, "second")
    # get next event from the queue and remove it from the queue
    nrowEvnts <- NROW(sim@events)
    if (nrowEvnts) {

      # Next block  much faster than sim@current <- sim@events[1L,]!
      sim@current <- sim@events[[1]]
      sim@events <- sim@events[-1]
      # if (nrowEvnts < .lengthEventsDT) {
      #   for (i in 1:.numColsEventList) {
      #     set(.currentEventDT, 1L, i, sim@events[[i]][[1]])
      #     set(.eventsDT[[nrowEvnts]], , i, sim@events[[i]][-1])
      #   }
      #   sim@current <- .currentEventDT
      #   sim@events <- .eventsDT[[nrowEvnts]]
      # } else {
      #   # above replaces these two lines
      #   sim@current <- sim@events[1L, ]
      #   sim@events <- sim@events[-1L, ]
      # }
    } else {
      # no more events, return empty event list
      sim@current <- list()#.emptyEventListObj
    }
  }

  # catches the situation where no future event is scheduled,
  #  but stop time is not reached
  cur <- sim@current
  if  (NROW(cur) == 0) {
    sim@simtimes[["current"]] <- sim@simtimes[["end"]] + 1
  } else {
    if (cur[["eventTime"]] <= sim@simtimes[["end"]]) {
      # update current simulated time
      sim@simtimes[["current"]] <- cur[["eventTime"]]

      # call the module responsible for processing this event
      moduleCall <- paste("doEvent", cur[["moduleName"]], sep = ".")

      # Debug internally in the doEvent?
      debugDoEvent <- FALSE

      # check the module call for validity
      if (!(all(unlist(lapply(debug, identical, FALSE))))) {
        for (i in seq_along(debug)) {
          if (isTRUE(debug[[i]]) | debug[[i]] == "current") {
            if (NROW(cur) > 0) {
              evnts1 <- data.frame(current(sim))
              widths <- stri_length(format(evnts1))
              .pkgEnv[[".spadesDebugWidth"]] <- pmax(widths, .pkgEnv[[".spadesDebugWidth"]])
              evnts1[1L, ] <- format(evnts1) %>%
                stri_pad_right(., .pkgEnv[[".spadesDebugWidth"]])

              if (.pkgEnv[[".spadesDebugFirst"]]) {
                evnts2 <- evnts1
                evnts2[1L:2L, ] <- names(evnts1) %>%
                  stri_pad(., .pkgEnv[[".spadesDebugWidth"]]) %>%
                  rbind(., evnts1)
                cat("This is the current event, printed as it is happening:\n")
                write.table(evnts2, quote = FALSE, row.names = FALSE, col.names = FALSE)
                .pkgEnv[[".spadesDebugFirst"]] <- FALSE
              } else {
                colnames(evnts1) <- NULL
                write.table(evnts1, quote = FALSE, row.names = FALSE)
              }
            }
          } else if (debug[[i]] == "simList") {
            print(sim)
          } else if (grepl(debug[[i]], pattern = "\\(")) {
            print(eval(parse(text = debug[[i]])))
          } else if (any(debug[[i]] == unlist(sim@modules))) {
            if (debug[[i]] == cur[["moduleName"]]) {
              debugonce(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
              on.exit(get(paste0("doEvent.", cur[["moduleName"]]), envir = sim@.envir))
            }
          } else if (!any(debug[[i]] == c("step", "browser"))) {
            print(do.call(debug[[i]], list(sim)))
          }

          if (debug[[i]] == "step") {
            readline("Press any key to continue...")
          }
        }
      }

      if (cur[["moduleName"]] %in% sim@modules) {
        if (cur[["moduleName"]] %in% core) {
          sim <- get(moduleCall)(sim, cur[["eventTime"]],
                                 cur[["eventType"]], debugDoEvent)
        } else {
          # for future caching of modules
          cacheIt <- FALSE
          a <- sim@params[[cur[["moduleName"]]]][[".useCache"]]
          if (!is.null(a)) {
            #.useCache is a parameter
            if (!identical(FALSE, a)) {
              #.useCache is not FALSE
              if (!isTRUE(a)) {
                #.useCache is not TRUE
                if (cur[["eventType"]] %in% a) {
                  cacheIt <- TRUE
                } else if (is(a, "POSIXt")) {
                  cacheIt <- TRUE
                  notOlderThan <- a
                }
              } else {
                cacheIt <- TRUE
              }
            }
          }

          # This is to create a namespaced module call
          .modifySearchPath(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs,
                            removeOthers = FALSE)

          if (cacheIt) { # means that a module or event is to be cached
            objNam <- sim@depends@dependencies[[cur[["moduleName"]]]]@outputObjects$objectName
            moduleSpecificObjects <- c(grep(ls(sim@.envir, all.names = TRUE),
                                            pattern = cur[["moduleName"]], value = TRUE),
                                       na.omit(objNam))
            moduleSpecificOutputObjects <- objNam
            classOptions <- list(events = FALSE, current=FALSE, completed=FALSE, simtimes=FALSE,
                                 params = sim@params[[cur[["moduleName"]]]],
                                 modules = cur[["moduleName"]])
            sim <- Cache(FUN = get(moduleCall, envir = sim@.envir[[paste0("._", cur[["moduleName"]])]]),
                         sim = sim,
                         eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                         debug = debugDoEvent,
                         objects = moduleSpecificObjects,
                         notOlderThan = notOlderThan,
                         outputObjects = moduleSpecificOutputObjects,
                         classOptions = classOptions,
                         cacheRepo = sim@paths[["cachePath"]],
                         userTags = c("function:doEvent"))
          } else {
            sim <- get(moduleCall,
                       envir = sim@.envir[[paste0("._", cur[["moduleName"]])]])(sim, cur[["eventTime"]],
                                                                                cur[["eventType"]], debugDoEvent)
          }
        }
      } else {
        stop(
          paste(
            "Invalid module call. The module `",
            cur[["moduleName"]],
            "` wasn't specified to be loaded."
          )
        )
      }

      # add to list of completed events
      lenCompl <- length(sim@completed)
      if (lenCompl) {
        # Do not use pre-existing data.tables that get updated b/c completed will almost
        #  always be large (NROW(completed) > 20), so can't realistically pre-create
        #  many data.tables
        sim@completed <- append(sim@completed, list(cur))
        if (lenCompl > getOption("spades.nCompleted")) {
          sim@completed <- sim@completed[(lenCompl+1) - getOption("spades.nCompleted"):(lenCompl+1)]
        }
      } else {
        sim@completed <- list(cur)
      }
      # current event completed, replace current with empty
      sim@current <- list()
    } else {
      # update current simulated time and event
      sim@simtimes[["current"]] <- sim@simtimes[["end"]] + 1
      if (NROW(sim@events)) {
        # i.e., if no more events
        sim@events <- append(list(sim@current), sim@events)
        sim@current <- list()
        #sim@events <- rbind(sim@current, sim@events)
        #        sim@current <- .emptyEventListObj
      }
    }
  }
  return(invisible(sim))
}

################################################################################
#' Schedule a simulation event
#'
#' Adds a new event to the simulation's event queue, updating the simulation object.
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim            A \code{simList} simulation object.
#'
#' @param eventTime      A numeric specifying the time of the next event.
#'
#' @param moduleName     A character string specifying the module from which to
#'                       call the event.
#'
#' @param eventType      A character string specifying the type of event from
#'                       within the module.
#'
#' @param eventPriority  A numeric specifying the priority of the event.
#'                       Lower number means higher priority.
#'                       See \code{\link{priority}}.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @importFrom data.table setkey
#' @include priority.R
#' @export
#' @rdname scheduleEvent
#' @seealso \code{\link{priority}}
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Fransisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn") # default priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()) # default priority
#'
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()-1) # higher priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .normal()+1) # lower priority
#'
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .highest()) # highest priority
#'  scheduleEvent(x, time(sim) + 1.0, "firemodule", "burn", .lowest()) # lowest priority
#' }
scheduleEvent <- function(sim,
                          eventTime,
                          moduleName,
                          eventType,
                          eventPriority) {
  if (!class(sim)=="simList") stop("sim must be a simList")
  #if (!is(sim, "simList")) stop("sim must be a simList")
  if (!is.numeric(eventTime)) stop(paste(
    "Invalid or missing eventTime. eventTime must be a numeric. This is usually",
    "caused by an attempt to scheduleEvent at time NULL",
    "or by using an undefined parameter."
  ))
  if (!is.character(eventType)) stop("eventType must be a character")
  if (!is.character(moduleName)) stop("moduleName must be a character")
  if (missing(eventPriority)) eventPriority <- .pkgEnv$.normalVal
  if (!is.numeric(eventPriority)) stop("eventPriority must be a numeric")

  if (length(eventTime)) {
    if (!is.na(eventTime)) {
      # if there is no metadata, meaning for the first
      #  "default" modules...load, save, checkpoint, progress
      if (!is.null(sim@depends@dependencies[[1]])) {
        # first check if this moduleName matches the name of a module
        #  with meta-data (i.e., depends(sim)@dependencies filled)
        if (moduleName %in% unlist(lapply(sim@depends@dependencies, function(x) {
          x@name
        }))) {
          # If the eventTime doesn't have units, it's a user generated
          #  value, likely because of times in the simInit call.
          #  This must be intercepted, and units added based on this
          #  assumption, that the units are in \code{timeunit}
          if (is.null(attr(eventTime, "unit"))) {
            attributes(eventTime)$unit <- .callingFrameTimeunit(sim)
            eventTimeInSeconds <- as.numeric(convertTimeunit((
              eventTime -
                convertTimeunit(sim@simtimes[["start"]],
                                sim@simtimes[["timeunit"]], sim@.envir,
                                skipChecks = TRUE)
            ),
            "seconds",
            sim@.envir, skipChecks = TRUE) +
              sim@simtimes[["current"]])
          } else {
            eventTimeInSeconds <-
              as.numeric(convertTimeunit(eventTime, "seconds", sim@.envir,
                                         skipChecks = TRUE))
          }
        } else {
          # for core modules because they have no metadata
          eventTimeInSeconds <-
            as.numeric(convertTimeunit(eventTime, "seconds", sim@.envir,
                                       skipChecks = TRUE))
        }
      } else {
        # when eventTime is NA... can't seem to get an example
        eventTimeInSeconds <-
          as.numeric(convertTimeunit(eventTime, "seconds", sim@.envir,
                                     skipChecks = TRUE))
      }
      attributes(eventTimeInSeconds)$unit <- "second"

      #newEvent <- copy(.singleEventListDT)
      newEventList <- list(list(
        eventTime = eventTimeInSeconds,
        moduleName = moduleName,
        eventType = eventType,
        eventPriority = eventPriority
      ))
      nrowEvnts <- NROW(sim@events)
      if (nrowEvnts == 0L) {
        sim@events <- newEventList
      } else {
        sim@events <- append(sim@events, newEventList)
        needSort <- TRUE
        if (eventTimeInSeconds>sim@events[[nrowEvnts]][[1]]) {
          needSort <- FALSE
        } else if (eventTimeInSeconds==sim@events[[nrowEvnts]][[1]] & eventPriority>=sim@events[[nrowEvnts]][[4]]){
          needSort <- FALSE
        }
        if(needSort) {
          ord <- order(unlist(lapply(sim@events, function(x) x$eventTime)),
                       unlist(lapply(sim@events, function(x) x$eventPriority)))
          sim@events <- sim@events[ord]
        }

      }


      # for (i in 1:.numColsEventList) set(newEvent, , i, newEventList[[i]])
      #
      # # if the event list is empty, set it to consist of newEvent and return;
      # # otherwise, add newEvent and re-sort (rekey).
      # #evnts <- sim@events #events(sim, "second")
      # nrowEvnts <- NROW(sim@events)
      #
      # if (nrowEvnts == 0L) {
      #   sim@events <- newEvent
      # } else {
      #   # This is faster than rbindlist below. So, use for smaller event queues
      #   if (nrowEvnts < .lengthEventsDT) {
      #
      #     #for speed -- the special case where there are only one event in the queue
      #     if (nrowEvnts == 1L) {
      #       if (eventTimeInSeconds<sim@events[[1]][1]) {
      #         for (i in 1:.numColsEventList) {
      #           set(.eventsDT[[nrowEvnts + 2]], , i, c(newEvent[[i]], sim@events[[i]]))
      #         }
      #       } else {
      #         for (i in 1:.numColsEventList) {
      #           set(.eventsDT[[nrowEvnts + 2]], , i, c(sim@events[[i]], newEvent[[i]]))
      #         }
      #
      #       }
      #     } else {
      #       for (i in 1:.numColsEventList) {
      #         set(.eventsDT[[nrowEvnts + 2]], , i, c(sim@events[[i]], newEvent[[i]]))
      #       }
      #     }
      #
      #     sim@events <- .eventsDT[[nrowEvnts + 2]]
      #   } else {
      #     sim@events <- rbindlist(list(sim@events, newEvent))
      #   }
      #
      #   needSort <- TRUE
      #   # only sort if new event is not already at the end
      #   if (eventTimeInSeconds>sim@events[[1]][nrowEvnts]) {
      #     needSort <- FALSE
      #   } else if (eventTimeInSeconds==sim@events[[1]][nrowEvnts] & eventPriority>=sim@events[[4]][nrowEvnts]){
      #     needSort <- FALSE
      #   }
      #
      #   if (needSort) {
      #     browser()
      #
      #     setkey(sim@events, "eventTime", "eventPriority")
      #   }
      # }
    }
  } else {
    warning(
      paste(
        "Invalid or missing eventTime. ",
        "This is usually caused by an attempt to scheduleEvent at an empty eventTime ",
        "or by using an undefined parameter."
      )
    )
    #)
  }

  return(invisible(sim))
}

# @rdname scheduleEvent
#' setMethod(
#'   "scheduleEvent",
#'   signature(
#'     sim = "simList",
#'     eventTime = "NULL",
#'     moduleName = "character",
#'     eventType = "character",
#'     eventPriority = "numeric"
#'   ),
#'   definition = function(sim,
#'                         eventTime,
#'                         moduleName,
#'                         eventType,
#'                         eventPriority) {
#'     warning(
#'       paste(
#'         "Invalid or missing eventTime. This is usually",
#'         "caused by an attempt to scheduleEvent at time NULL",
#'         "or by using an undefined parameter."
#'       )
#'     )
#'     return(invisible(sim))
#'   })
#'
#' #' @rdname scheduleEvent
#' setMethod(
#'   "scheduleEvent",
#'   signature(
#'     sim = "simList",
#'     eventTime = "numeric",
#'     moduleName = "character",
#'     eventType = "character",
#'     eventPriority = "missing"
#'   ),
#'   definition = function(sim,
#'                         eventTime,
#'                         moduleName,
#'                         eventType,
#'                         eventPriority) {
#'     scheduleEvent(
#'       sim = sim,
#'       eventTime = eventTime,
#'       moduleName = moduleName,
#'       eventType = eventType,
#'       eventPriority = .normal()
#'     )
#'   })

################################################################################
#' Run a spatial discrete event simulation
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim A \code{simList} simulation object, generally produced by \code{simInit}.
#'
#' @param debug Optional logical flag or character vector indicating what to print to
#'              console at each event. See details.
#'              Default is to use the value in \code{getOption("spades.debug")}.
#'
#' @param progress Logical (\code{TRUE} or \code{FALSE} show a graphical progress bar),
#'                 character (\code{"graphical"}, \code{"text"}) or numeric indicating
#'                 the number of update intervals to show in a graphical progress bar.
#'
#' @param cache Logical. If \code{TRUE}, then the \code{spades} call will be cached.
#'              This means that if the call is made again with the same simList,
#'              then `spades`` will return the return value from the previous run
#'              of that exact same simList. Default \code{FALSE}. See Details.
#'              See also the vignette on caching for examples.
#'
#' @param .plotInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param notOlderThan Date or time. Passed to \code{reproducible::Cache} to update the cache.
#'                     Default is \code{NULL}, meaning don't update the cache.
#'                     If \code{Sys.time()} is provided, then it will force a recache,
#'                     i.e., remove old value and replace with new value.
#'                     Ignored if \code{cache} is \code{FALSE}.
#'
#' @param ... Any. Can be used to make a unique cache identity, such as "replicate = 1".
#'            This will be included in the \code{Cache} call, so will be unique
#'            and thus \code{spades} will not use a cached copy as long as
#'            anything passed in \code{...} is unique, i.e., not cached previously.
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{SpaDES.core-package}}, \code{\link[reproducible]{Cache}}
#'
#' @details
#' The is the workhorse function in the SpaDES package. It runs simulations by
#' implementing the rules outlined in the \code{simList}.
#'
#' This function gives simple access to two sets of module parameters:
#' \code{.plotInitialTime} and with \code{.plotInitialTime}. The primary use of
#' these arguments is to temporarily turn off plotting and saving. "Temporary"
#' means that the \code{simList} is not changed, so it can be used again with
#' the simList values reinstated. To turn off plotting and saving, use
#' \code{.plotInitialTime = NA} or \code{.saveInitialTime = NA}. NOTE: if a
#' module did not use \code{.plotInitialTime} or \code{.saveInitialTime}, then
#' these arguments will not do anything.
#'
#' If \code{cache} is TRUE, this allows for a seamless way to "save" results
#' of a simulation. The  user does not have to intentionally do any saving manually.
#' Instead, upon a call to \code{spades} in which the simList is identical,
#' the function will simply return the result that would have come if it had
#' been rerun. Use this with caution, as it will return exactly the result from
#' a previous run, even if there is stochasticity internally.
#' Caching is only based on the input simList. See also \code{experiment} for
#' the same mechanism, but it can be used with replication.
#' See also the vignette on caching for examples.
#'
#' If \code{debug} is specified, it can be a logical or character vector.
#' If not specified, the package option \code{spades.debug} is used.
#' In all cases, something will be printed to the console immediately before each
#' event is being executed.
#' If \code{TRUE}, then the event immediately following will be printed as it
#' runs (equivalent to \code{current(sim)}).
#' If a character string, then it can be one of the many \code{simList} accessors,
#' such as \code{events}, \code{params}, \code{"simList"} (print the entire simList),
#' or any R expression.
#' If an R expression it will be evaluated with access to the \code{sim} object.
#' If this is more than one character string, then all will be printed to the
#' screen in their sequence.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user.
#' Will print additional outputs informing the user of updates to the values of
#' various \code{simList} slot components.
#' See \url{https://github.com/PredictiveEcology/SpaDES/wiki/Debugging} for details.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @rdname spades
#' @seealso \code{\link{experiment}} for using replication with \code{spades}.
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Fransisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @examples
#' \dontrun{
#' mySim <- simInit(
#'  times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'  params = list(
#'    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'  ),
#'  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#' )
#' spades(mySim)
#'
#' # set default debug printing for the current session
#' # setOption(spades.debug = TRUE)
#'
#' # Different debug options (overrides the package option 'spades.debug')
#' spades(mySim, debug = TRUE) # Fastest
#' spades(mySim, debug = "simList")
#' spades(mySim, debug = "print(table(sim$landscape$Fires[]))")
#'
#' # Can turn off plotting, and inspect the output simList instead
#' out <- spades(mySim, .plotInitialTime = NA) # much faster
#' completed(out) # shows completed events
#'
#' # use cache -- simInit should generally be rerun each time a spades call is made
#' #   to guarantee that it is identical. Here, run spades call twice, first
#' #   time to establish cache, second time to return cached result
#' for (i in 1:2) {
#'  mySim <- simInit(
#'    times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#'  )
#'  print(system.time(out <- spades(mySim, cache = TRUE)))
#' }
#' }
#'
setGeneric(
  "spades",
  function(sim, debug = getOption("spades.debug"), progress = NA, cache,
           .plotInitialTime = NULL, .saveInitialTime = NULL, notOlderThan = NULL, ...) {
    standardGeneric("spades")
  })

#' @rdname spades
setMethod(
  "spades",
  signature(sim = "simList", cache = "missing"),
  definition = function(sim,
                        debug,
                        progress,
                        cache,
                        .plotInitialTime,
                        .saveInitialTime,
                        notOlderThan,
                        ...) {
    # The event queues are not uncopied data.tables, for speed during simulation
    #  Must, therefore, break connection between spades calls
    #.refreshEventQueues()
    .pkgEnv$searchPath <- search()

    # timeunits gets accessed every event -- this should only be needed once per simList
    sim@.envir$.timeunits <- timeunits(sim)
    on.exit({
      .modifySearchPath(.pkgEnv$searchPath, removeOthers = TRUE)
      rm(".timeunits", envir = sim@.envir)
    })

    if (!is.null(.plotInitialTime)) {
      if (!is.numeric(.plotInitialTime))
        .plotInitialTime <- as.numeric(.plotInitialTime)
      paramsLocal <- sim@params
      whNonHiddenModules <-
        !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <-
        lapply(paramsLocal[whNonHiddenModules], function(x) {
          x$.plotInitialTime <- .plotInitialTime
          x
        })
      sim@params <- paramsLocal
    }
    if (!is.null(.saveInitialTime)) {
      if (!is.numeric(.saveInitialTime))
        .saveInitialTime <- as.numeric(.saveInitialTime)
      paramsLocal <- sim@params
      whNonHiddenModules <-
        !grepl(names(paramsLocal), pattern = "\\.")
      paramsLocal[whNonHiddenModules] <-
        lapply(paramsLocal[whNonHiddenModules], function(x) {
          x$.saveInitialTime <- NA_real_
          x
        })
      sim@params <- paramsLocal
    }

    if (!is.na(progress)) {
      tu <- sim@simtimes[["timeunit"]]
      if (isTRUE(progress)) {
        progress <- "graphical"
      }
      if (is.numeric(progress)) {
        sim@params$.progress$interval <- (end(sim, tu) - start(sim, tu)) / progress
        progress <- "graphical"
      }

      if (!is.na(pmatch(progress, "graphical"))) {
        sim@params$.progress$type <- "graphical"
      } else if (!is.na(pmatch(progress, "text"))) {
        sim@params$.progress$type <- "text"
      }

      if (!is.na(sim@params$.progress$type) &&
          is.na(sim@params$.progress$interval)) {
        sim@params$.progress$interval <- NULL
      }
    }

    if (!(all(unlist(lapply(debug, identical, FALSE))))) {
      .pkgEnv[[".spadesDebugFirst"]] <- TRUE
      .pkgEnv[[".spadesDebugWidth"]] <- c(9, 10, 9, 13)
    }
    while (sim@simtimes[["current"]] <= sim@simtimes[["end"]]) {
      sim <- doEvent(sim, debug = debug, notOlderThan = notOlderThan)  # process the next event

    }
    sim@simtimes[["current"]] <- sim@simtimes[["end"]]
    return(invisible(sim))
  })

#' @rdname spades
#' @importFrom reproducible Cache
setMethod(
  "spades",
  signature(cache = "logical"),
  definition = function(sim,
                        debug,
                        progress,
                        cache,
                        .plotInitialTime,
                        .saveInitialTime,
                        notOlderThan = NULL,
                        ...) {
    stopifnot(class(sim) == "simList")

    if (cache) {
      return(
        Cache(
          cacheRepo = sim@paths$cachePath,
          spades,
          sim = sim,
          debug = debug,
          progress = progress,
          .plotInitialTime = .plotInitialTime,
          .saveInitialTime = .saveInitialTime,
          notOlderThan = notOlderThan,
          ...
        )
      )
    } else {
      return(
        spades(
          sim,
          debug = debug,
          progress = progress,
          .plotInitialTime = .plotInitialTime,
          .saveInitialTime = .saveInitialTime
        )
      )
    }
  })
