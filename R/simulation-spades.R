utils::globalVariables(c(".", ".I", "whi"))

################################################################################
#' Process a simulation event
#'
#' Internal function called from \code{spades}.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods.
#'
#' @param sim Character string for the \code{simList} simulation object.
#' @param useFuture Experimental use of future::future package. Not fully implemented.
#'
#' @inheritParams spades
#' @return Returns the modified \code{simList} object.
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table data.table rbindlist fread
#' @importFrom reproducible Cache messageDF
#' @importFrom utils write.table
#' @include helpers.R memory-leaks.R
#' @keywords internal
#' @rdname doEvent
#'
doEvent <- function(sim, debug = FALSE, notOlderThan,
                    useFuture = getOption("spades.futureEvents", FALSE),
                    events = NULL,
                    ...) {
  #if (missing(debug)) debug <- FALSE
  #if (!inherits(sim, "simList")) stop("sim must be a simList")
  #if (!is(sim, "simList")) stop("sim must be a simList")
  if (class(sim) != "simList") { # faster than `is` and `inherits`
    stop("doEvent can only accept a simList object")
  }

  # core modules
  core <- .pkgEnv$.coreModules

  if (isTRUE(useFuture)) {
    # Check here if resolved
    curForFuture <- sim@events[[1]]
    if (!curForFuture[["moduleName"]] %in% .pkgEnv$.coreModulesMinusSave) {
      if (length(sim$simFuture)) {
        modInFuture <- modNameInFuture(sim$simFuture)
        futureNeeds <- getFutureNeeds(deps = sim@depends@dependencies,
                                      curModName = modInFuture)#sim$simFuture[[1]]$thisModOutputs$dontAllowModules)#curForFuture[["moduleName"]])
        canProceed <- if (length(futureNeeds)) {
          # with the assumption that the "unresolved" future could schedule itself,
          # must block any module who's outputs are needed by the same module as the
          # unresolved future module
          !any(names(futureNeeds$dontAllowModules)[futureNeeds$dontAllowModules] %in% curForFuture$moduleName) #&&
            #modInFuture != curForFuture[["moduleName"]]
        } else {
          TRUE
        }
        if (!canProceed || curForFuture$moduleName == "save" || future::resolved(sim$simFuture[[1]][[1]])) {
          cause <- if (!canProceed) paste0(curForFuture$moduleName, " requires outputs from ", modInFuture)
          else if (curForFuture$moduleName == "save") paste0("Current event is 'save'; so resolving all")
          else paste0(modInFuture, " finished running")
          sim <- resolveFutureNow(sim, cause = cause)
        }

      }

    }

  }

  if (length(sim@current) == 0) {
    # get next event from the queue and remove it from the queue
    if (length(sim@events)) {

      # Section for allowing events to be specified in `spades` call
      dots <- list(...)
      eventIndex <- if (!is.null(events))
        isListedEvent(sim@events, events) else 1L

      if (eventIndex == 0L) {
        slot(sim, "current", check = FALSE) <- list() # same as no events left
      } else {
        # Do same check as would be done with "slot(..., check = FALSE)", but much faster
        if (is.list(sim@events[[eventIndex]]))
          slot(sim, "current", check = FALSE) <- sim@events[[eventIndex]]
        if (is.list(sim@events[-eventIndex]))
          slot(sim, "events", check = FALSE) <- sim@events[-eventIndex]
      }

    } else {
      # no more events, return empty event list
      slot(sim, "current", check = FALSE) <- list() # this is guaranteed to be a list
    }
  }

  # catches the situation where no future event is scheduled,
  #  but stop time is not reached
  cur <<- sim@current
  curModuleName <- cur[["moduleName"]]
  if  (length(cur) == 0) {
    # Test replacement for speed
    #slot(sim, "simtimes")[["current"]] <- sim@simtimes[["end"]] + 1
    st <- slot(sim, "simtimes")
    st[["current"]] <- sim@simtimes[["end"]] + 1
    slot(sim, "simtimes", check = FALSE) <- st
  } else {
    # if the current time is greater than end time, then don't run it
    if (cur[["eventTime"]] <= sim@simtimes[["end"]]) {
      fnEnv <- sim@.xData$.mods[[curModuleName]]

      # This allows spades to run even if all the functions are in the .GlobalEnv;
      #   while this is generally bad practice, and none of the tools in spades
      #   enable this to happen, this allows a user can manually run individual functions
      #   like `doEvent.XXX` and `scheduleEvent`, then run `spades(sim)` without failing
      if (is.null(fnEnv)) {
        if (!curModuleName %in% .coreModules())
          fnEnv <- asNamespace("SpaDES.core")
      }
      fnEnvIsSpaDES.core <- identical(fnEnv, asNamespace("SpaDES.core"))

      # update current simulated time
      # Test replacement for speed
      st <- slot(sim, "simtimes")
      st[["current"]] <- cur[["eventTime"]]
      slot(sim, "simtimes", check = FALSE) <- st

      # call the module responsible for processing this event
      moduleCall <- paste("doEvent", curModuleName, sep = ".")
      # Modules can use either the doEvent approach or defineEvent approach, with doEvent taking priority
      if (!is.null(fnEnv))
        if (!exists(moduleCall, envir = fnEnv)) {
          moduleCall2 <- paste("doEvent", curModuleName, cur$eventType, sep = ".")
          if (exists(moduleCall2, envir = fnEnv)) { # don't specify inherits = FALSE because might be elsewhere
            moduleCall <- moduleCall2
          }
        }

      # if debug is TRUE
      if (is.null(attr(sim, "needDebug"))) {
        attr(sim, "needDebug") <- if (length(debug) > 1) {
          !(all(unlist(lapply(debug, identical, FALSE))))
        } else {
          !identical(debug, FALSE)
        }
      }
      if (attr(sim, "needDebug")) {
        debugMessage(debug, sim, cur, fnEnv, curModuleName)
      }

      # if the moduleName exists in the simList -- i.e,. go ahead with doEvent
      moduleIsInSim <- curModuleName %in% sim@modules
      if (!moduleIsInSim && !fnEnvIsSpaDES.core)
        stop("Invalid module call. The module `", curModuleName, "` wasn't specified to be loaded.")
      # if (curModuleName %in% sim@modules) {
      if (curModuleName %in% core) {
        sim <- get(moduleCall)(sim, cur[["eventTime"]], cur[["eventType"]])
      } else {
        # for future caching of modules
        cacheIt <- FALSE
        eventSeed <- sim@params[[curModuleName]][[".seed"]][[cur[["eventType"]]]]
        a <- sim@params[[curModuleName]][[".useCache"]]
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

        # browser(expr = exists("._doEvent_2"))
        showSimilar <- if (is.null(sim@params[[curModuleName]][[".showSimilar"]]) ||
                           isTRUE(is.na(sim@params[[curModuleName]][[".showSimilar"]]))) {
          isTRUE(getOption("reproducible.showSimilar", FALSE))
        } else {
          isTRUE(sim@params[[curModuleName]][[".showSimilar"]])
        }

        # This is to create a namespaced module call
        if (!.pkgEnv[["skipNamespacing"]])
          .modifySearchPath(sim@depends@dependencies[[curModuleName]]@reqdPkgs,
                            removeOthers = FALSE)

        skipEvent <- FALSE
        if (!is.null(eventSeed)) {
          if (exists(".Random.seed", inherits = FALSE, envir = .GlobalEnv))
            initialRandomSeed <- .Random.seed
          set.seed(eventSeed) # will create .Random.seed
        }

        .pkgEnv <- as.list(get(".pkgEnv", envir = asNamespace("SpaDES.core")))
        if (useFuture) {
          # stop("using future for spades events is not yet fully implemented")
          futureNeeds <- getFutureNeeds(deps = sim@depends@dependencies,
                                        curModName = cur[["moduleName"]])

          if (!any(futureNeeds$thisModOutputs %in% futureNeeds$anyModInputs)) {
            sim <- .runEventFuture(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan,
                                   showSimilar = showSimilar, .pkgEnv, envir = environment(),
                                   futureNeeds = futureNeeds)
            skipEvent <- TRUE
          }
        }

        if (!skipEvent) {
          sim <- .runEvent(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan,
                           showSimilar = showSimilar, .pkgEnv)
        }

        if (!is.null(eventSeed)) {
          if (exists("initialRandomSeed", inherits = FALSE))
            .Random.seed <- initialRandomSeed
        }

        # browser(expr = exists("._doEvent_3"))
        if (!fnEnvIsSpaDES.core) {
          if (!exists(curModuleName, envir = sim@.xData$.mods, inherits = FALSE))
            stop("The module named ", curModuleName, " just corrupted the object with that ",
                 "name from from the simList. ",
                 "Please remove the section of code that does this in the event named: ",
                 cur[["eventType"]])

          if (!is.environment(get(curModuleName, envir = sim@.xData$.mods)))
            stop("The module named ", curModuleName, " just corrupted the object with that ",
                 "name from from the simList. ",
                 "Please remove the section of code that does this in the event named: ",
                 cur[["eventType"]])

          if (!exists("mod", envir = sim@.envir$.mods[[curModuleName]], inherits = FALSE)) {
            if (!isNamespace(tryCatch(asNamespace(.moduleNameNoUnderscore(curModuleName)),
                                      silent = TRUE, error = function(x) FALSE)
            ))
              stop("The module named ", curModuleName, " just deleted the object named 'mod' from ",
                   "sim$", curModuleName, ". ",
                   "Please remove the section of code that does this in the event named: ",
                   cur[["eventType"]])
          }
        }
      }
        # }

        # add to list of completed events
      if (.pkgEnv[["spades.keepCompleted"]]) { # can skip it with option
        cur$._clockTime <- Sys.time() # adds between 1 and 3 microseconds, per event b/c R won't let us use .Internal(Sys.time())
        if (!is.null(attr(sim, "completedCounter"))) { # use attr(sim, "completedCounter")
          #instead of sim@.xData because collisions with parallel sims from same sim object

          # next section replaces sim@completed <- append(sim@completed, list(cur)),
          # which gets slower with size of sim@completed
          # sim@completed <- append(sim@completed, list(cur))
          #   following does not: it is more or less O(1). Algorithm from: https://stackoverflow.com/questions/17046336/here-we-go-again-append-an-element-to-a-list-in-r?lq=1
          #   it basically increases size of list by *2 every time it fills up
          attr(sim, "completedCounter") <- attr(sim, "completedCounter") + 1

          # increase length of list by doubling as it grows
          # if(attr(sim, "completedCounter")==attr(sim, "completedSize")) {
          #   lenCompl <- length(sim@completed)
          #   if (lenCompl > .pkgEnv[["spades.nCompleted"]]) { # we are above desired
          #     if (attr(sim, "completedCounter") >= lenCompl ) { # We can now cull earlier ones
          #       keepFrom <- lenCompl - .pkgEnv[["spades.nCompleted"]]
          #       sim@completed <- sim@completed[keepFrom:lenCompl] # keep 10000 of them, from lenCompl - nCompleted to current
          #       attr(sim, "completedSize") <- length(sim@completed)
          #       attr(sim, "completedCounter") <- attr(sim, "completedSize")
          #     }
          #   }
          #   attr(sim, "completedSize") <- attr(sim, "completedSize") * 2
          #   #length(sim@completed) <- attr(sim, "completedSize")
          # }

          # faster to use assign
          assign(as.character(attr(sim, "completedCounter")), value = cur, envir = sim@completed)
          #sim@completed[[as.character(attr(sim, "completedCounter"))]] <- cur
        } else {
          attr(sim, "completedCounter") <- 1
          attr(sim, "completedSize") <- 2
          sim@completed[["1"]] <- cur
        }
      }

      # current event completed, replace current with empty
      slot(sim, "current", check = FALSE) <- list() # is a list
    } else {
      # update current simulated time and event
      # Test replacement for speed
      #slot(sim, "simtimes")[["current"]] <- sim@simtimes[["end"]] + 1
      st <- slot(sim, "simtimes")
      st[["current"]] <- sim@simtimes[["end"]] + 1
      slot(sim, "simtimes", check = FALSE) <- st

      # i.e., if no more events
      slot(sim, "events", check = FALSE) <- append(list(sim@current), sim@events) # will be a list b/c append
      slot(sim, "current", check = FALSE) <- list() # is a list
    }
  }

  if (exists("objectSynonyms", envir = sim, inherits = FALSE)) {
    sim <- .checkObjectSynonyms(sim)
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
#'                       call the event. If missing, it will use
#'                       \code{currentModule(sim)}
#'
#' @param eventType      A character string specifying the type of event from
#'                       within the module.
#'
#' @param eventPriority  A numeric specifying the priority of the event.
#'                       Lower number means higher priority. As a best practice, it is
#'                       recommended that decimal values are conceptual
#'                       grouped by their integer values (e.g., 4.0, 4.25, 4.5 are conceptually
#'                       similar).
#'                       See \code{\link{priority}}.
#' @param .skipChecks Logical. If \code{TRUE}, then internal checks that arguments match
#'                    expected types are skipped. Should only be used if speed is critical.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @include priority.R
#' @export
#' @rdname scheduleEvent
#' @seealso \code{\link{priority}}, \code{\link{scheduleConditionalEvent}}
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://nostarch.com/artofr.htm}
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
                          eventPriority = .pkgEnv$.normalVal,
                          .skipChecks = FALSE) {
  #if (!inherits(sim, "simList")) stop("sim must be a simList")
  #if (!is(sim, "simList")) stop("sim must be a simList")

  if (missing(moduleName)) moduleName <- currentModule(sim)

  if (!.skipChecks) {
    if (class(sim) != "simList") stop("sim must be a simList") # faster than `is` and `inherits`

    if (!is.numeric(eventTime)) {
      if (is.na(eventTime)) {
        eventTime <- NA_real_
      } else {
        stop(paste(
          "Invalid or missing eventTime. eventTime must be a numeric.",
          "This is usually caused by an attempt to scheduleEvent at time NULL",
          "or by using an undefined parameter."
        ))
      }
    }
    if (!is.character(eventType)) stop("eventType must be a character")
    if (!is.character(moduleName)) stop("moduleName must be a character")
    #if (missing(eventPriority)) eventPriority <- .pkgEnv$.normalVal
    if (!is.numeric(eventPriority)) stop("eventPriority must be a numeric")

  }
  if (length(eventTime)) {
    if (!is.na(eventTime)) {
      if (eventTime < 0) {
          stop("You have tried to schedule an event with negative time. You cannot do this. ",
               " Reschedule event (", eventType," event in ", moduleName, " module) with positive time.")
      }
      eventTimeInSeconds <- calculateEventTimeInSeconds(sim, eventTime, moduleName)
      attr(eventTimeInSeconds, "unit") <- "second"

      if (eventTimeInSeconds < sim@simtimes$start)
        stop("You have tried to schedule an event before start(sim). You cannot do this.",
             " Reschedule event (", eventType," event in ", moduleName, " module) at or after start(sim).")

      newEventList <- list(list(
        eventTime = eventTimeInSeconds,
        moduleName = moduleName,
        eventType = eventType,
        eventPriority = eventPriority
      ))
      numEvents <- length(sim@events)

      # put new event into event queue
      if (numEvents == 0L) {
        slot(sim, "events", check = FALSE) <- newEventList
      } else {
        slot(sim, "events", check = FALSE) <- append(sim@events, newEventList)
        needSort <- TRUE
        if (eventTimeInSeconds > sim@events[[numEvents]][[1]]) {
          needSort <- FALSE
        } else if (eventTimeInSeconds == sim@events[[numEvents]][[1]] &&
                   eventPriority >= sim@events[[numEvents]][[4]]) {
          needSort <- FALSE
        }
        if (needSort) {
          ord <- order(unlist(lapply(sim@events, function(x) x$eventTime)),
                       unlist(lapply(sim@events, function(x) x$eventPriority)))
          slot(sim, "events") <- sim@events[ord]
        }
      }
    }
  } else {
    warning(
      paste(
        "Invalid or missing eventTime. ",
        "This is usually caused by an attempt to scheduleEvent at an empty eventTime ",
        "or by using an undefined parameter."
      )
    )
  }

  return(invisible(sim))
}

################################################################################
#' Schedule a conditional simulation event
#'
#' Adds a new event to the simulation's conditional event queue,
#' updating the simulation object by creating or appending to
#' \code{sim$._conditionalEvents}. This is very experimental. Use with caution.
#'
#' @inheritParams scheduleEvent
#'
#' @param minEventTime   A numeric specifying the time before which the event should not occur,
#'         even if the condition is met. Defaults to \code{start(sim)}
#'
#' @param maxEventTime   A numeric specifying the time after which the event should not occur,
#'         even if the condition is met. Defaults to \code{end(sim)}
#'
#' @param condition A string, call or expression that will be assessed for \code{TRUE}
#'      after each event in the regular event queue.
#'      It can access objects in the \code{simList} by using functions of \code{sim},
#'      e.g., \code{"sim$age > 1"}
#'
#' @return Returns the modified \code{simList} object, i.e., \code{sim$._conditionalEvents}.
#'
#' This conditional event queue will be assessed at every single event in the normal event
#' queue. If there are no conditional events, then \code{spades} will proceed as normal.
#' As conditional event conditions are found to be true, then it will trigger a call to
#' \code{scheduleEvent(...)} with the current time passed to \code{eventTime} \emph{and}
#' it will remove the conditional event from the conditional queue.
#' If the user would like the triggered conditional event to occur as the very next event,
#' then a possible strategy would be to set \code{eventPriority} of the conditional event
#' to very low or even negative to ensure it gets inserted at the top of the event queue.
#'
#' @include priority.R
#' @export
#' @rdname scheduleConditionalEvent
#' @seealso \code{\link{scheduleEvent}}, \code{\link{conditionalEvents}}
#'
#' @author Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://nostarch.com/artofr.htm}
#'
#' @examples
#'   sim <- simInit(times = list(start = 0, end = 2))
#'   condition <- "sim$age > 1" # provide as string
#'   condition <- quote(sim$age > 1) # provide as a call
#'   condition <- expression(sim$age > 1) # provide as an expression
#'   sim <- scheduleConditionalEvent(sim, condition, "firemodule", "burn")
#'   conditionalEvents(sim)
#'   sim <- spades(sim) # no changes to sim$age, i.e., it is absent
#'   events(sim) # nothing scheduled
#'   sim$age <- 2 # change the value
#'   sim <- spades(sim) # Run spades, the condition is now true, so event is
#'                      #  scheduled at current time
#'   events(sim)        # now scheduled in the normal event queue
scheduleConditionalEvent <- function(sim,
                                     condition,
                                     moduleName,
                                     eventType,
                                     eventPriority = .pkgEnv$.normalVal,
                                     minEventTime = start(sim),
                                     maxEventTime = end(sim)) {
  if (class(sim) != "simList") stop("sim must be a simList") # faster than `is` and `inherits`

  if (!is.numeric(minEventTime)) {
    if (is.na(minEventTime)) {
      eventTime <- NA_real_
    } else {
      stop(paste(
        "Invalid or missing minEventTime. minEventTime must be a numeric.",
        "This is usually caused by an attempt to scheduleEvent at time NULL",
        "or by using an undefined parameter."
      ))
    }
  }
  if (!is.numeric(maxEventTime)) {
    if (is.na(maxEventTime)) {
      eventTime <- NA_real_
    } else {
      stop(paste(
        "Invalid or missing maxEventTime. maxEventTime must be a numeric.",
        "This is usually caused by an attempt to scheduleEvent at time NULL",
        "or by using an undefined parameter."
      ))
    }
  }

  if (!is.character(eventType)) stop("eventType must be a character")
  if (!is.character(moduleName)) stop("moduleName must be a character")
  #if (missing(eventPriority)) eventPriority <- .pkgEnv$.normalVal
  if (!is.numeric(eventPriority)) stop("eventPriority must be a numeric")

  if (length(condition)) {
    #if (!is.na(condition)) {

    # Convert quote or "" to expression
    if (is.call(condition)) {
      cond <- as.expression(condition)
    } else if (is.character(condition)) {
      cond <- parse(text = condition)
    } else if (is.expression(condition)) {
      cond <- condition
    } else {
      stop("condition must be a character string or call or expression")
    }

    # minEventTime
    minEventTimeInSeconds <- calculateEventTimeInSeconds(sim, minEventTime, moduleName)
    attr(minEventTimeInSeconds, "unit") <- "second"

    # maxEventTime
    maxEventTimeInSeconds <- calculateEventTimeInSeconds(sim, maxEventTime, moduleName)
    attr(maxEventTimeInSeconds, "unit") <- "second"

    newEventList <- list(list(
      condition = cond,
      minEventTime = minEventTimeInSeconds,
      maxEventTime = maxEventTimeInSeconds,
      moduleName = moduleName,
      eventType = eventType,
      eventPriority = eventPriority
    ))
    numEvents <- length(sim$._conditionalEvents)

    # put new event into event queue
    if (numEvents == 0L) {
      sim$._conditionalEvents <- newEventList
    } else {
      sim$._conditionalEvents <- append(sim$._conditionalEvents, newEventList)
      needSort <- TRUE
      if (minEventTimeInSeconds > sim$._conditionalEvents[[numEvents]]$minEventTime) {
        needSort <- FALSE
      } else if (minEventTimeInSeconds == sim$._conditionalEvents[[numEvents]]$minEventTime &
                 eventPriority >= sim$._conditionalEvents[[numEvents]]$eventPriority) {
        needSort <- FALSE
      }
      if (needSort) {
        ord <- order(unlist(lapply(sim$._conditionalEvents, function(x) x$eventTime)),
                     unlist(lapply(sim$._conditionalEvents, function(x) x$eventPriority)))
        sim$._conditionalEvents <- sim$._conditionalEvents[ord]
      }
      #      }
    }
  } else {
    warning(
      paste(
        "Invalid or missing condition ",
        "This is usually caused by an attempt to scheduleEvent at an empty condition ",
        "or by using an undefined parameter."
      )
    )
  }

  return(invisible(sim))
}

################################################################################
#' Run a spatial discrete event simulation
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods, and use `data.table`
#' instead of `data.frame` to implement the event queue (because it is much faster).
#'
#' @param sim A \code{simList} simulation object, generally produced by \code{simInit}.
#'
#' @param debug Optional tools for invoking debugging. Supplying a \code{list}
#'              will invoke the more powerful \code{logging} package. See details.
#'              Default is to use the value in \code{getOption("spades.debug")}.
#'
#' @param progress Logical (\code{TRUE} or \code{FALSE} show a graphical progress bar),
#'                 character (\code{"graphical"}, \code{"text"}) or numeric indicating
#'                 the number of update intervals to show in a graphical progress bar.
#'
#' @param cache Logical. If \code{TRUE}, then the \code{spades} call will be cached.
#'              This means that if the call is made again with the same \code{simList},
#'              then `spades`` will return the return value from the previous run
#'              of that exact same \code{simList}. Default \code{FALSE}. See Details.
#'              See also the vignette on caching for examples.
#'
#' @param .plotInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the \code{.plotInitialTime}
#'                                  parameter for all modules. See Details.
#'
#' @param .plots Character. Sets the parameter of this name in all modules.
#'   See \code{\link{Plots}} for possible values. The parameter is intended to slowly
#'   take over from \code{.plotInitialTime} as a mechanism to turn on or off plotting.
#'   For backwards compatibility, if \code{.plotInitialTime} is not set
#'   in this \code{spades} call, but this
#'   \code{.plots} is used, two things will happen: setting this without \code{"screen"}
#'   will turn off all plotting; setting this with \code{"screen"} will trigger
#'   plotting for any modules that use this parameter but will have no effect on
#'   other modules. To get plotting, therefore, it may be necessary to also set
#'   \code{.plotInitialTime = start(sim)}.
#'
#' @param notOlderThan Date or time. Passed to \code{reproducible::Cache} to update the cache.
#'                     Default is \code{NULL}, meaning don't update the cache.
#'                     If \code{Sys.time()} is provided, then it will force a recache,
#'                     i.e., remove old value and replace with new value.
#'                     Ignored if \code{cache} is \code{FALSE}.
#' @param events A character vector or a named list of character vectors. If specified,
#'   the simulations will only do the events indicated here. If a named list, the names
#'   must correspond to the modules and the character vectors can be specific events within
#'   each of the named modules. With the \code{list} form, all unspecified modules
#'   will run \emph{all} their events, including internal spades modules, e.g., \code{save},
#'   that get invoked with the \code{outputs} argument in  \code{simInit}. See example.
#'
#' @param ... Any. Can be used to make a unique cache identity, such as "replicate = 1".
#'            This will be included in the \code{Cache} call, so will be unique
#'            and thus \code{spades} will not use a cached copy as long as
#'            anything passed in \code{...} is unique, i.e., not cached previously.
#'
#' @return Invisibly returns the modified \code{simList} object.
#'
#' @seealso \code{\link{SpaDES.core-package}},
#' \code{\link{simInit}}, and the caching vignette (very important for reproducibility):
#' \url{https://CRAN.R-project.org/package=SpaDES.core/vignettes/iii-cache.html} which
#' uses \code{\link[reproducible]{Cache}}.
#'
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
#' @section Caching with SpaDES:
#'
#' There are numerous ways in which Caching can be used within SpaDES. Please
#' see the vignette
#' \url{https://CRAN.R-project.org/package=SpaDES.core/vignettes/iii-cache.html}
#' for many examples. Briefly, functions, events, modules, entire spades calls or
#' experiment calls (see \url{https://github.com/PredictiveEcology/SpaDES.experiment})
#' can be cached and mixtures of all of these will work. For functions, simply
#' wrap the call with \code{Cache}, moving the original function name into
#' the first argument of Cache. For events or modules, set the module \code{parameters},
#' \code{.useCache}, e.g.,
#' \code{simInit(..., parameters = list(myModule = list(.useCache = "init")))}.
#' This can be set to an event name, which will cache that event, or a logical (e.g., \code{}),
#' which will cache \emph{every} event in that module. Event and module caching
#' makes most sense when the event or module only runs once, such as an initialization
#' or data preparation event/module. Caching an entire simulation is actually just
#' a function call to \code{simInitAndSpades}, for example. So, simply writing
#' \code{Cache(simInitAndSpades, modules = ...)} will effectively cache a whole simulation.
#' Finally for experiments, it is just like a function call:
#' \code{Cache(simInitandExperiment, ...)}. The final way Caching can be done is in
#' \code{experiment} or \code{spades}, by setting the \code{cache} argument.
#'
#' If \code{cache} is TRUE, this allows for a seamless way to "save" results
#' of a simulation. The  user does not have to intentionally do any saving manually.
#' Instead, upon a call to \code{spades} in which the simList is identical,
#' the function will simply return the result that would have come if it had
#' been rerun. Use this with caution, as it will return exactly the result from
#' a previous run, even if there is stochasticity internally.
#' Caching is only based on the input simList.
#' See also the vignette on caching for examples.
#'
#' @section \code{debug}:
#'
#' The most powerful way to use debug is to invoke the \code{logging}
#' R package. To invoke this, \code{debug} must be a list with up to 3
#' named elements:
#' \code{console}, \code{file}, and \code{debug}. Each of these list elements
#' must be a list (including empty \code{list()} for defaults) with the
#' sub-list elements here:
#' \tabular{lll}{
#'   \code{console} \tab \code{level} \tab The \code{level}, see below, of information shown\cr
#'   \code{file} \tab \code{append} \tab Logical. If \code{TRUE}, the default, then
#'                                       log entries are appended to file, if it exists\cr
#'               \tab \code{file} \tab A filename. Defaults to \code{log.txt}\cr
#'               \tab \code{level} \tab The \code{level}, see below, of information shown\cr
#'   \code{debug} \tab See possible values below\cr
#'   }
#'
#' \code{level} can be a number from 0 to 100 or a character string matching one
#' of the values in \code{logging::loglevels}. These are hierarchical levels of
#' information passed to the console. Set a lower number for more information and a
#' higher number for less information. Errors in code will be shown if \code{level}
#' is set to \code{"ERROR"} or \code{40} or above; warnings in code will be shown if
#' \code{level} is set to \code{"WARN"} or \code{30} or above;
#' normal messages in code will
#' be shown if \code{level} is set to \code{"INFO"} or \code{20} or above. For
#' consistency with base R messaging, if default level is used, then normal
#' messaging via \code{message} will be shown; this means that \code{suppressMessages}
#' will work to suppress messaging only when level is set to \code{"INFO"} or \code{20}.
#' Some functions in the SpaDES ecosystem may have information at the lower levels,
#' but currently, there are few to none.
#'
#' \code{debug} is specified as a non-list argument to \code{spades} or as
#' \code{list(debug = ...)}, then it can be a logical, a quoted call, a character vector
#' or a numeric scalar (currently 1 or 2) or a list of any of these to get multiple
#' outputs. This will be run at the start of every event. The following options for debug
#' are available. Each of these can also be in a list to get multiple outputs:
#'
#' \tabular{ll}{
#'   \code{TRUE} \tab \code{current(sim)} will be printed at the start of each event as
#'                     it runs\cr
#'   a function name (as character string) \tab If a function, then it will be run on the
#'                                            simList, e.g., "time" will run
#'                                            \code{time(sim)} at each event.\cr
#'   moduleName (as character string) \tab All calls to that module will be entered
#'                                         interactively\cr
#'   eventName (as character string) \tab All calls that have that event name (in any module)
#'                                        will be entered interactively\cr
#'   \code{c(<moduleName>, <eventName>)}  \tab Only the event in that specified module
#'                                             will be entered into. \cr
#'   Any other R expression expressed as a character string or quoted call \tab
#'                                 Will be evaluated with access to the simList as 'sim'.
#'                                If this is more than one character string, then all will
#'                                be printed to the screen in their sequence. \cr
#'   A numeric scalar, currently 1 or 2 (maybe others) \tab This will print out alternative forms of event
#'                                           information that users may find useful \cr
#' }
#'
#' If not specified in the function call, the package
#' option \code{spades.debug} is used.
#'
#' If \code{options("spades.browserOnError" = TRUE)} (experimental still) if
#' there is an error, it will attempt to open a browser
#' in the event where the error occurred. You can edit, and then press \code{c} to continue
#' or \code{Q} to quit, plus all other normal interactive browser tools.
#' \code{c} will trigger a reparse and events will continue as scheduled, starting
#' with the one just edited. There may be some unexpected consequences if the
#' \code{simList} objects had already been changed before the error occurred.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user.
#' Will print additional outputs informing the user of updates to the values of
#' various \code{simList} slot components.
#' See \url{https://github.com/PredictiveEcology/SpaDES/wiki/Debugging} for details.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @importFrom data.table setDTthreads
#' @export
#' @rdname spades
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://nostarch.com/artofr.htm}
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
#' # To get a combination -- use list(debug = list(..., ...))
#' spades(mySim, debug = list(debug = list(1, quote(as.data.frame(table(sim$landscape$Fires[]))))))
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
#'
#' # E.g., with only the init events
#' outInitsOnly <- spades(mySim, events = "init")
#'
#' # or more fine grained control
#' outSomeEvents <- spades(mySim,
#'         events = list(randomLandscapes = c("init"),
#'                       fireSpread = c("init", "burn")))
#'
#' # with outputs, the save module gets invoked and must be explicitly limited to "init"
#' mySim <- simInit(
#'  times = list(start = 0.0, end = 2.0, timeunit = "year"),
#'  params = list(
#'    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'  ),
#'  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'  outputs = data.frame(objectName = "landscape", saveTime = 0:2),
#'  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#' )
#' # This will print a message saying that caribouMovement will run its events
#' outSomeEvents <- spades(mySim,
#'         events = list(randomLandscapes = c("init"),
#'                       fireSpread = c("init", "burn"),
#'                       save = "init"))
#' }
#'
setGeneric(
  "spades",
  function(sim, debug = getOption("spades.debug"), progress = NA, cache,
           .plotInitialTime = NULL, .saveInitialTime = NULL, notOlderThan = NULL,
           events = NULL, .plots = NULL, ...) {
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
                        events,
                        .plots,
                        ...) {

    opt <- options("encoding" = "UTF-8")
    on.exit(options(opt), add = TRUE)

    if (is.character(getOption("spades.covr", FALSE)) &&  getOption("spades.covr2", TRUE) ) {
      modNam <- getOption("spades.covr")
      tf <- tempfile();
      on.exit(unlink(tf))
      cat(file = tf, paste('spades(sim, events = ',capture.output(dput(events)),', .plotInitialTime = ', .plotInitialTime, ')', collapse = "\n"))
      # unlockBinding(modNam, sim$.mods)
      sim$.mods[[modNam]]$sim <- sim
      opts <- options("spades.covr2" = FALSE) # turn off this chunk 2nd time through
      on.exit(options(opts), add = TRUE)
      aa <- covr::environment_coverage(sim$.mods[[modNam]], test_files = tf)
      rm(list = "sim", envir = sim$.mods[[modNam]])
      options("spades.covr2" = TRUE)
      if (is.null(.pkgEnv$._covr)) .pkgEnv$._covr <- list()
      .pkgEnv$._covr <- append(.pkgEnv$._covr, list(aa))
      return(.pkgEnv$.sim)
    }
    oldWd <- getwd()
    on.exit({
      setwd(oldWd)
    }, add = TRUE)
    useNormalMessaging <- TRUE
    newDebugging <- is.list(debug)
    if (newDebugging) {
      if (requireNamespace("logging", quietly = TRUE)) {
        debug <- setupDebugger(debug)
        useNormalMessaging <- !newDebugging ||
          all(!grepl("writeToConsole", names(logging::getLogger()[["handlers"]])))
      } else {
        warning("debug cannot be a list unless logging package is installed: install.packages('logging')\n",
                "setting debug to non-list using unlist(debug)")
        debug <- unlist(debug)

      }
    }

    # need to recheck package loading because `simInit` may have been cached
    pkgs <- packages(sim)
    loadPkgs(pkgs)

    sim <- withCallingHandlers({

      recoverModeWrong <- getOption("spades.recoverMode")
      if (!is.null(recoverModeWrong))
        warning("Please set options('recoveryMode') with a 'y', not options('recoverMode')")
      recoverMode <- getOption("spades.recoveryMode", FALSE)

      # If there already is a sim object saved in the .pkgEnv, it may have objects,
      #   and those objects may have temporary files from file-backed objects stored.
      #   This will remove those file-backed temp files
      clearFileBackedObjs(.pkgEnv$.sim$.recoverableObjs, recoverMode)
      .pkgEnv$.sim <- NULL # Clear anything that was here.
      .pkgEnv$.sim <- sim # set up pointer

      # set the options("spades.xxxPath") to the values in the sim@paths
      oldGetPaths <- getPaths()
      do.call(setPaths, append(sim@paths, list(silent = TRUE)))
      on.exit({
        do.call(setPaths, append(list(silent = TRUE), oldGetPaths))
      }, add = TRUE)

      if (!is.null(sim@.xData[["._randomSeed"]])) {
        message("Resetting .Random.seed of session because sim$._randomSeed is not NULL. ",
                "To get a different seed, run: sim$._randomSeed <- NULL to clear it.")
        assign(".Random.seed", sim@.xData$._randomSeed[[1]], envir = .GlobalEnv)
        if (!is.null(sim$._rng.kind)) {
          do.call("RNGkind", as.list(sim$._rng.kind))
        }
        sim@.xData[["._randomSeed"]] <- NULL
        sim@.xData[["._rng.kind"]] <- NULL
      }
      if (is.null(sim@.xData[["._startClockTime"]]))
        sim@.xData[["._startClockTime"]] <- Sys.time()

      if (is.list(events)) {
        unspecifiedEvents <- setdiff(unlist(modules(sim, TRUE)), names(events))
        unspecifiedEvents <- setdiff(unspecifiedEvents, "progress")
        if (NROW(sim@outputs) == 0L)
          unspecifiedEvents <- setdiff(unspecifiedEvents, "save")
        if (NROW(sim@inputs) == 0L)
          unspecifiedEvents <- setdiff(unspecifiedEvents, "load")
        useChkpnt <- !any(is.na(params(sim)$checkpoint))
        if (!useChkpnt)
          unspecifiedEvents <- setdiff(unspecifiedEvents, "checkpoint")
        if (length(unspecifiedEvents)) {
          message("NOTE: ", paste(unspecifiedEvents, collapse = ", "), " not specified in events argument. ",
                  "This means all events the module(s) will run. You may have intended to add e.g., list(",
                  unspecifiedEvents[1], "= 'init')")
        }
      }

      # This sets up checking for memory leaks
      if (is.null(sim@.xData[["._knownObjects"]])) {
        moduleNames <- unname(modules(sim))
        names(moduleNames) <- moduleNames
        sim@.xData[["._knownObjects"]] <- lapply(moduleNames, function(x) character())
        sim@.xData[["._knownObjects"]]$sim <- character()
      }
      if (is.null(sim@.xData[["._simRndString"]]))
        sim@.xData[["._simRndString"]] <- rndstr(1, 8, characterFirst = TRUE)
      .pkgEnv$searchPath <- search()
      .pkgEnv[["spades.browserOnError"]] <- (interactive() & !identical(debug, FALSE) &
                                               getOption("spades.browserOnError"))
      .pkgEnv[["spades.nCompleted"]] <- getOption("spades.nCompleted")
      .pkgEnv[["skipNamespacing"]] <- !getOption("spades.switchPkgNamespaces")
      .pkgEnv[["spades.keepCompleted"]] <- getOption("spades.keepCompleted", TRUE)

      # Memory Use
      # memory estimation of each event/sim
      if (getOption("spades.memoryUseInterval", 0) > 0) {
        if (requireNamespace("future", quietly = TRUE)) {
          originalPlan <- future::plan()
          sim <- memoryUseSetup(sim, originalPlan)
          on.exit({
            sim <- memoryUseOnExit(sim, originalPlan)
          }, add = TRUE)
        } else {
          message(futureMessage)
        }
      }

      # timeunits gets accessed every event -- this should only be needed once per simList
      sim@.xData$.timeunits <- timeunits(sim)
      on.exit({
        if (!.pkgEnv[["skipNamespacing"]])
          .modifySearchPath(.pkgEnv$searchPath, removeOthers = TRUE)
        rm(".timeunits", envir = sim@.xData)
        if (isTRUE(getOption("spades.saveSimOnExit", FALSE))) {
          if (!isTRUE(.pkgEnv$.cleanEnd)) {
            if (recoverMode > 0) {
              sim <- recoverModeOnExit(sim, rmo, recoverMode)
            }
            messageInterrupt1(recoverMode)
          } else {
            message(crayon::magenta("simList saved in\n",
                                    crayon::blue("SpaDES.core:::.pkgEnv$.sim"),
                                    "\nIt will be deleted at next spades() call."))
          }
          .pkgEnv$.sim <- sim # no copy of objects -- essentially 2 pointers throughout
          .pkgEnv$.cleanEnd <- NULL
        }
        # For restarting R -- a few extra pieces, including saving the simList as the last thing
        if (!is.null(sim$._restartRList)) {
          sim@simtimes[["current"]] <- sim@events[[1]]$eventTime
          sim$._restartRList$.spadesCall <- match.call()

          restartFormals <- formals(restartR)
          # can change end(sim) back to original now because we are already ending
          end(sim) <- sim$._restartRList$endOrig
          restartR(
            sim = sim,
            reloadPkgs = getOption("spades.restartR.reloadPkgs", restartFormals$reloadPkgs),
            .First = getOption("spades.restartR..First", restartFormals$.First),
            .RDataFile = getOption("spades.restartR.filename", sim$._restartRList$simFilename),
            restartDir = getOption("spades.restartR.restartDir", restartFormals$restartDir)
          )
        }
      }, add = TRUE)

      if (!is.null(.plots)) {
        sim@params <- updateParamSlotInAllModules(
          sim@params, .plots, ".plots",
          needClass = "character",
          needValuesMess = paste0("It must be one or more of 'screen', ",
                                  "'object', 'raw' and any of the classes that ggplot2::ggsave ",
                                  "can handle, e.g., 'png'"))
        if (is.null(.plotInitialTime) && !any(.plots %in% "screen"))
          sim@params <- updateParamSlotInAllModules(
            sim@params, NA_integer_, ".plotInitialTime",
            needClass = "numeric")
      }
      if (!is.null(.plotInitialTime)) {
        sim@params <- updateParamSlotInAllModules(
          sim@params, .plotInitialTime, ".plotInitialTime",
          needClass = "numeric")
        if (is.na(.plotInitialTime))
          sim@params <- updateParamSlotInAllModules(
            sim@params, NA_character_, ".plots",
            needClass = "character")
      }
      if (!is.null(.saveInitialTime)) {
        sim@params <- updateParamSlotInAllModules(
          sim@params, .saveInitialTime, ".saveInitialTime",
          needClass = "numeric")
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

      sim@.xData[["._firstEventClockTime"]] <- Sys.time()

      # This was introduced when sim@completed became an environment for speed purposes
      # (list got slow as size increased)
      # This is an attempt to deal with the expected behaviour of a list --
      # i.e., delete it if this appears to be the original sim object again passed in
      if (length(sim@completed)) {
        existingCompleted <- sort(as.integer(ls(sim@completed, sorted = FALSE)))
        prevStart <- get(as.character(existingCompleted[1]), envir = sim@completed)
        prevEnd <- get(as.character(existingCompleted[length(existingCompleted)]), envir = sim@completed)
        if (length(.grepSysCalls(sys.calls(), "restartSpades")) == 0)
          if (start(sim, unit = attr(prevStart[["eventTime"]], "unit")) <= prevStart[["eventTime"]] &&
              (time(sim, unit = attr(prevStart[["eventTime"]], "unit")) ==
               start(sim, unit = attr(prevStart[["eventTime"]], "unit"))))
            sim@completed <- new.env(parent = emptyenv())
      }

      if (recoverMode > 0) {
        rmo <- NULL # The recovery mode object
        allObjNames <- outputObjectNames(sim)
        if (is.null(allObjNames)) recoverMode <- 0
      }
      useFuture <- getOption("spades.futureEvents", FALSE)
      if (useFuture) {
        if (!requireNamespace("future", quietly = TRUE))
          stop("To use 'spades.futureEvents', please run \ninstall.packages('future') ")

        message("useFuture is set to TRUE; this will attempt to spawn events in a separate process, ",
                "if their outputs are not needed by other events. STILL EXPERIMENTAL. Use cautiously.",
                "User must manage future::plan, e.g., \nfuture::plan(multisession(workers = 2))")
        sim$.futureEventsSkipped <- 0
        sim$simFuture <- list()
      }

      # There are some edge cases where there is an event scheduled before current time,
      #   even though current time is after end time
      if (length(sim@events)) {
        specialStart <- sim@events[[1]][["eventTime"]] < sim@simtimes[["current"]] &&
          sim@simtimes[["current"]] > sim@simtimes[["end"]]
        if (isTRUE(specialStart)) sim@simtimes[["current"]] <- sim@events[[1]][["eventTime"]]
      }

      simDTthreads <- getOption("spades.DTthreads", 1L)
      message("Using setDTthreads(", simDTthreads, "). To change: 'options(spades.DTthreads = X)'.")
      origDTthreads <- setDTthreads(simDTthreads)
      on.exit(setDTthreads(origDTthreads), add = TRUE)

      while (sim@simtimes[["current"]] <= sim@simtimes[["end"]]) {
        if (recoverMode > 0) {
          rmo <- recoverModePre(sim, rmo, allObjNames, recoverMode)
        }
        sim <- doEvent(sim, debug = debug, notOlderThan = notOlderThan,
                       events = events, ...)  # process the next event

        if (recoverMode > 0) {
          rmo <- recoverModePost(sim, rmo, recoverMode)
        }
        # Conditional Scheduling -- adds only 900 nanoseconds per event, if none exist
        if (exists("._conditionalEvents", envir = sim, inherits = FALSE)) {
          condEventsToOmit <- integer()
          for (condNum in seq(sim$._conditionalEvents)) {
            cond <- sim$._conditionalEvents[[condNum]]
            if (isTRUE(eval(cond$condition))) {
              curTime <- time(sim)
              if (curTime >= cond$minEventTime && curTime <= cond$maxEventTime) {
                message("  Conditional Event -- ", cond$condition, " is true. Scheduling for now")
                sim <- scheduleEvent(sim, eventTime = curTime, moduleName = cond$moduleName,
                                     eventType = cond$eventType, eventPriority = cond$eventPriority)
                condEventsToOmit <- c(condEventsToOmit, condNum)
              }
            }
          }
          if (length(condEventsToOmit)) {
            sim$._conditionalEvents <- sim$._conditionalEvents[-condEventsToOmit]
            if (length(sim$._conditionalEvents) == 0) {
              rm("._conditionalEvents", envir = sim)
            }
          }
        }
        if (useFuture) {
          if (length(sim$simFuture) > 1) {
            for (simFut in seq_along(sim$simFuture)) {
              if (future::resolved(sim$simFuture[[1]][[1]])) {
                sim <- resolveFutureNow(sim, cause = paste0(modNameInFuture(sim$simFuture[1]), " finished running"))
              }
            }
          }
        }
      }
      if (useFuture) {
        if (length(sim$simFuture)) {
          for (simFut in seq_along(sim$simFuture)) {
            sim <- resolveFutureNow(sim, cause = "End of simulation")
          }
        }
        message(crayon::magenta(sim$.futureEventsSkipped, " events ran while events ran in futures"))
      }
      sim@simtimes[["current"]] <- sim@simtimes[["end"]]

      # For determining if clean ending to spades call
      .pkgEnv$.cleanEnd <- TRUE
      return(invisible(sim))
    },
    warning = function(w) { if (requireNamespace("logging", quietly = TRUE)) {
      logging::logwarn(paste0(collapse = " ", c(names(w), w)))
      }
    },
    error = function(e) { if (requireNamespace("logging", quietly = TRUE)) {
      logging::logerror(e)
    } else {
      stop(e)
    }},
    message = function(m) {
      if (newDebugging && requireNamespace("logging", quietly = TRUE)) {
        logging::loginfo(m$message)
      }
      if (useNormalMessaging) {
        message(loggingMessage(m$message))
      }
      # This will "muffle" the original message
      tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
      # tryCatch(rlang::cnd_muffle(m), error = function(e) NULL)
    }
    )
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
                        events,
                        .plots,
                        ...) {
    stopifnot(class(sim) == "simList")

    oldGetPaths <- getPaths()
    do.call(setPaths, append(list(silent = TRUE), sim@paths))
    on.exit({do.call(setPaths, append(list(silent = TRUE), oldGetPaths))}, add = TRUE)

    dots <- list(...)
    omitArgs <- c("cl", "notOlderThan")
    if (isTRUE("omitArgs" %in% names(dots))) {
      omitArgs <- c(dots$omitArgs, omitArgs)
      dots$omitArgs <- NULL
    }

    if (cache) {
      return(
        do.call(quote = TRUE, Cache,
                args = append(
                  list(
                    spades,
                    sim = sim,
                    debug = debug,
                    progress = progress,
                    .plotInitialTime = .plotInitialTime,
                    .saveInitialTime = .saveInitialTime,
                    omitArgs = omitArgs,
                    notOlderThan = notOlderThan,
                    events = events,
                    .plots = .plots
                  ),
                  dots
               )
        )
      )
    } else {
      return(
        spades(
          sim,
          debug = debug,
          progress = progress,
          .plotInitialTime = .plotInitialTime,
          .saveInitialTime = .saveInitialTime,
          events = events,
          .plots = .plots
        )
      )
    }
})

#' @keywords internal
.runEvent <- function(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan, showSimilar, .pkgEnv) {
  if (!is.null(sim@depends@dependencies[[cur[["moduleName"]]]])) { # allow for super simple simList without a slot outputObjects
    createsOutputs <- sim@depends@dependencies[[cur[["moduleName"]]]]@outputObjects$objectName
    if (cacheIt) { # means that a module or event is to be cached
      fns <- ls(fnEnv, all.names = TRUE)
      moduleSpecificObjects <-
        c(ls(sim@.xData, all.names = TRUE, pattern = cur[["moduleName"]]), # functions in the main .xData that are prefixed with moduleName
          paste0(attr(fnEnv, "name"), ":", fns), # functions in the namespaced location
          na.omit(createsOutputs)) # objects outputted by module
      #fnsWOhidden <- paste0(cur[["moduleName"]], ":",
      #                      grep("^\\._", fns, value = TRUE, invert = TRUE))
      moduleSpecificOutputObjects <- c(createsOutputs, paste0(".mods$", cur[["moduleName"]]))
      classOptions <- list(events = FALSE, current = FALSE, completed = FALSE, simtimes = FALSE,
                           params = sim@params[[cur[["moduleName"]]]],
                           modules = cur[["moduleName"]])
    }
  }
  fnCallAsExpr <- if (cacheIt) { # means that a module or event is to be cached
    expression(Cache(FUN = get(moduleCall, envir = fnEnv),
                sim = sim,
                     eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                     .objects = moduleSpecificObjects,
                     notOlderThan = notOlderThan,
                     outputObjects = moduleSpecificOutputObjects,
                     classOptions = classOptions,
                showSimilar = showSimilar,
                     cacheRepo = sim@paths[["cachePath"]]))
  } else {
    # Faster just to pass the NULL and just call it directly inside .runEvent
    expression(get(moduleCall,
                   envir = fnEnv)(sim, cur[["eventTime"]], cur[["eventType"]]))
  }

  if (.pkgEnv[["spades.browserOnError"]]) {
    sim <- .runEventWithBrowser(sim, fnCallAsExpr, moduleCall, fnEnv, cur)
  } else {
    #fnEnv[[moduleCall]](sim, cur[["eventTime"]], cur[["eventType"]])
    sim <- eval(fnCallAsExpr) # slower than more direct version just above
  }
  # Test for memory leaks
  if (getOption("spades.testMemoryLeaks", TRUE)) {
    if (!is.null(sim@.xData$.mods[[cur[["moduleName"]]]]$.objects))
      sim$._knownObjects <- testMemoryLeaks(simEnv = sim@.xData,
                                            modEnv = sim@.xData$.mods[[cur[["moduleName"]]]]$.objects,
                                            modName = cur[["moduleName"]],
                                            knownObjects = sim@.xData$._knownObjects)
  }

  return(sim)
}

#' @keywords internal
.runEventWithBrowser <- function(sim, fnCallAsExpr, moduleCall, fnEnv, cur) {
  canContinue <- TRUE
  numTries <- 0
  while (canContinue) {
    out <- try(eval(fnCallAsExpr))
    if (isTRUE(is(out, "try-error"))) {
      numTries <- numTries + 1
      if (numTries > 1) {
        tmp <- .parseConditional(filename = sim@.xData$.mods[[cur$moduleName]]$._sourceFilename)
        eval(tmp[["parsedFile"]][!tmp[["defineModuleItem"]]],
             envir = sim@.xData$.mods[[cur[["moduleName"]]]])
        numTries <- 0
      } else {
        message("There was an error in the code in the ", moduleCall, ".\n",
                "Entering browser. You can correct it and press c to continue or Q to quit.")
        debugonce(get(moduleCall, envir = fnEnv))
      }
    } else {
      canContinue <- FALSE
    }
  }
  sim <- out
}

#' @keywords internal
calculateEventTimeInSeconds <- function(sim, eventTime, moduleName) {
  #if (!is.null(sim@depends@dependencies[[1]])) {
  unitAttr <- attr(eventTime, "unit")
  if (is.null(unitAttr)) {
    attributes(eventTime)$unit <- .callingFrameTimeunit(sim)
    eventTime <- as.numeric(convertTimeunit((
      eventTime -
        convertTimeunit(sim@simtimes[["start"]],
                        sim@simtimes[["timeunit"]], sim@.xData,
                        skipChecks = TRUE)
    ),
    "seconds",
    sim@.xData, skipChecks = TRUE) +
      sim@simtimes[["current"]])
  } else {
    #if (!startsWith(unitAttr, "second")) {
    if (unitAttr != "second") { # faster to do 2 sequential tests like this than !startsWith
      if (unitAttr != "seconds") {
        eventTime <- as.numeric(convertTimeunit(eventTime, "seconds", sim@.xData,
                                                skipChecks = TRUE))
      }
    }
  } #else eventTime

  #} else {
  # when eventTime is NA... can't seem to get an example
  #eventTimeInSeconds <-
  #  as.numeric(convertTimeunit(eventTime, "seconds", sim@.xData,
  #                             skipChecks = TRUE))
  #}
  eventTime
}

#' @keywords internal
#' @importFrom stats runif
recoverModePre <- function(sim, rmo = NULL, allObjNames = NULL, recoverMode) {
  if (is.null(allObjNames)) {
    allObjNames <- outputObjectNames(sim)
  }

  if (is.null(rmo))
    rmo <- list(
      recoverModeTiming = 0,
      recoverableObjs = list(),
      addedEvents = list(list()),
      randomSeed = list(list())
    )

  # Remove the tail entry in each of the lists
  if (length(rmo$addedEvents) > (recoverMode - 1))
    rmo$addedEvents <- rmo$addedEvents[seq_len(recoverMode - 1)]
  if (length(rmo$randomSeed) > (recoverMode - 1))
    rmo$randomSeed <- rmo$randomSeed[seq_len(recoverMode - 1)]
  startTime <- Sys.time()
  if (length(rmo$recoverableObjs) > (recoverMode - 1)) {
    # remove the previous rmo files, making way for subsequent Copy below. These files
    #   should be temporary versions and so can be safely deleted
    clearFileBackedObjs(rmo$recoverableObjs, recoverMode)
    rmo$recoverableObjs <- rmo$recoverableObjs[seq_len(recoverMode - 1)]
  }

  if (length(sim@events) > 0) {
    objsInSimListAndModule <- ls(sim) %in% allObjNames[[sim@events[[1]][["moduleName"]]  ]]
    # This makes a copy of the objects that are needed, and adds them to the list of rmo$recoverableObjs
    mess <- capture.output(type = "message",
                           rmo$recoverableObjs <- append(list(if (any(objsInSimListAndModule)) {
                             Copy(mget(ls(sim)[objsInSimListAndModule], envir = sim@.xData),
                                  filebackedDir = file.path(getOption("spades.scratchPath"), "._rmo"))
                           } else {
                             list()
                           }), rmo$recoverableObjs)
    )
    mess <- grep("Hardlinked version", mess, invert = TRUE)
    if (length(mess) > 0)
      lapply(mess, message)
  }
  endTime <- Sys.time()
  rmo$preEvents <- sim@events
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) tmp <- runif(1)
  rmo$randomSeed <- append(list(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)),
                           rmo$randomSeed)

  timeElapsedHere <- difftime(endTime, startTime, units = "secs")
  if (timeElapsedHere > 1)
    message(crayon::magenta(paste0("... spades.recoveryMode used ",
                                   format(timeElapsedHere, units = "auto", digits = 3))))

  rmo$recoverModeTiming <- rmo$recoverModeTiming + timeElapsedHere

  rmo
}

#' @keywords internal
recoverModePost <- function(sim, rmo, recoverMode) {
  rmo$postEvents <- sim@events
  rmo$addedEvents <- append(list(setdiff(rmo$postEvents, rmo$preEvents)), rmo$addedEvents)
  rmo
}

#' @keywords internal
recoverModeOnExit <- function(sim, rmo, recoverMode) {
  sim@.xData$.recoverableObjs <- rmo$recoverableObjs
  recoverableObjsSize <- sum(unlist(objSize(rmo$recoverableObjs)))
  class(recoverableObjsSize) <- "object_size"
  rmo$postEvents <- sim@events
  rmo$addedEvents <- append(list(setdiff(rmo$postEvents, rmo$preEvents)), rmo$addedEvents)
  sim@.xData$.addedEvents <- rmo$addedEvents
  sim@.xData$._randomSeed <- rmo$randomSeed
  message(crayon::magenta(paste0("Setting options('spades.recoveryMode' = ",recoverMode,") used ",
                                 format(rmo$recoverModeTiming, units = "auto", digits = 3),
                                 " and ", format(recoverableObjsSize, units = "auto"))))
  message(crayon::magenta("The initial state of the last", as.numeric(recoverMode), "events are cached and saved",
                          "in the simList located at SpaDES.core:::.pkgEnv$.sim,",
                          "as sim$.recoverableObjs, with the most recent event",
                          "the first element in the list, 2nd most recent event = the second most recent event, etc.",
                          " The objects contained in each of those are only the objects that may have",
                          "changed, according to the metadata for each module. To recover, use:\n",
                          "restartSpades()"))
  return(sim)
}

#' @keywords internal
messageInterrupt1 <- function(recoverMode) {
  message(
    crayon::magenta("Because of an interrupted spades call, the sim object ",
                    c("at the time of interruption ", "at the start of the interrupted event ")[(recoverMode > 0) + 1],
                    "was saved in \n", crayon::blue("SpaDES.core:::.pkgEnv$.sim"),
                    "\nIt will be deleted on next call to spades"))
}

setupDebugger <- function(debug = getOption("spades.debug")) {
  if (!missing(debug)) {
    if (!isFALSE(debug)) {
      if (is.list(debug)) {
        needInstall("logging",
                    messageStart = "debug cannot be a list unless logging package is installed: ")

        logging::logReset()
        if (is.null(names(debug))) stop("debug must be a named list if it is a list. See ?spades")
        hasConsole <- grepl("console", names(debug))
        if (any(hasConsole)) {
          if (!is.list(debug$console)) stop("debug has an element named 'console', which is not a list.",
                                            "Try 'debug = list(console = list())'")
          consoleLevel <- if (!is.null(debug$console$level)) {
            debug$console$level
          } else {
            "INFO"
          }
          if (!any(grepl("20|INFO", consoleLevel))) {
            if (!"basic.stdout" %in% names(logging::getLogger()[["handlers"]])) {
              #basicConfig()
              logging::addHandler(logging::writeToConsole, level = consoleLevel#,
                                  #formatter = spadesDefaultFormatter
              )
            }
            logging::setLevel(consoleLevel, logging::getHandler(logging::writeToConsole))
            #setLevel(consoleLevel, getHandler('basic.stdout'))
          }

        }
        hasFile <- grepl("file", names(debug))
        if (any(hasFile)) {
          fileLevel <- if (!is.null(debug$file$level)) {
            debug$file$level
          } else {
            "INFO"
          }
          if (!"writeToFile" %in% names(logging::getLogger()[["handlers"]])) {
            if (is.null(debug$file$file))
              debug$file$file <- "log.txt"
            logging::addHandler(logging::writeToFile, file=debug$file$file, level = fileLevel)
          }
          logging::setLevel(fileLevel, logging::getHandler(logging::writeToFile))
          cat(file = debug$file$file, "##################################\n",
              append = !isFALSE(debug$file$append)) # default append it TRUE
        }
        # with(getLogger(), names(handlers))

        hasDebug <- grepl("debug", names(debug))
        if (any(hasDebug)) {
          debug <- debug$debug
        } else {
          debug <- 1
        }


      } else {
        stop("debug cannot be a list unless logging package is installed: ",
             "install.packages('logging')")
      }
    }
  }
  debug
}

spadesDefaultFormatter <- function(record) {
  text <- paste(record$timestamp, paste(record$levelname, record$logger, gsub("\n$", "", record$msg), sep=':'), sep = "")
}

#' @importFrom reproducible Filenames
clearFileBackedObjs <- function(recoverableObjs, recoverMode) {
  if (isTRUE(recoverMode > 0)) {
    toClear <- recoverableObjs[[as.numeric(recoverMode)]]
    if (length(toClear)) {
      out <- lapply(toClear, function(x) {
        if (is(x, "Raster")) {
          Filenames(x)
        }
      })
      files <- unname(unlist(out))
      files <- files[nzchar(files)]
      if (length(files) != 0 ) {
        unlink(files)
        dirs <- unique(dirname(files))
        filesLeft <- dir(dirs, full.names = TRUE)
        if (length(filesLeft) == 0 || all(grepl("cache", filesLeft))) {
          unlink(dirs, recursive = TRUE)
        }
      }
    }
  }
  return(invisible())
}

resolveFutureNow <- function(sim, cause = "") {

  futureRunning <- sim@events[[1]]
  futureRunning[1:4] <- as.list(strsplit(names(sim$simFuture)[1], split = "_")[[1]])
  futureRunning[["eventTime"]] <- as.numeric(futureRunning[[1]])
  futureRunning[["eventPriority"]] <- as.numeric(futureRunning[["eventPriority"]])
  futureRunningSimTU <- futureRunning
  futureRunningSimTU[["eventTime"]] <- convertTimeunit(as.numeric(futureRunningSimTU[[1]]), unit = timeunit(sim))
  setDT(futureRunningSimTU)
  setDT(futureRunning)
  message(crayon::magenta("        -- Resolving", capture.output(print(futureRunningSimTU, row.names = FALSE, col.names = "none"))))
  message(crayon::magenta("           ", cause))

  tmpSim <- future::value(sim$simFuture[[1]][[1]])
  simMetadata <- sim$simFuture[[1]][[2]]

  # objects
  list2env(mget(simMetadata$objects, envir = envir(tmpSim)), envir = envir(sim))

  allCols <- c("eventTime", "moduleName", "eventTime", "eventPriority")
  # events
  evntsFut <- events(tmpSim, unit = "seconds")
  compltd <- completed(sim, unit = "seconds")[, 1:4]
  evntsNormal <- rbindlist(list(compltd, current(sim, unit = "seconds"), events(sim, unit = "seconds")))
  newEvents <- evntsFut[!evntsNormal, on = allCols]

  sim$.futureEventsSkipped <- sim$.futureEventsSkipped + NROW(compltd) - futureRunning[compltd, whi := .I, on = allCols]$wh
  if (NROW(newEvents)) {
    newEvents <- lapply(seq(NROW(newEvents)), function(x) as.list(newEvents[x]))
    slot(sim, "events", check = FALSE) <- append(sim@events, newEvents)
    ord <- order(unlist(lapply(sim@events, function(x) x$eventTime)),
                 unlist(lapply(sim@events, function(x) x$eventPriority)))
    slot(sim, "events") <- sim@events[ord]
  }
  sim$simFuture <- sim$simFuture[-1]

  sim
}

getFutureNeeds <- function(deps, curModName) {
  #browser(expr = curModName == "fireSpread")
  out <- list()
  mods <- names(deps)
  if (isTRUE(curModName %in% mods)) {
    moduleNamesNotThisOne <- mods[!mods %in% curModName]
    out$thisMod <- curModName
    allOtherModNames <- deps[names(deps) != curModName]
    out$anyModInputs <- na.omit(unique(unlist(lapply(
      deps, # can be a different event, don't exclude self
      function(modu)
        modu@inputObjects$objectName
    ))))
    out$thisModsInputs <- na.omit(unique(unlist(lapply(
      deps[curModName], # can be a different event, don't exclude self
      function(modu)
        modu@inputObjects$objectName
    ))))
    out$thisModOutputs <- na.omit(unique(unlist(lapply(
      deps[curModName],
      function(modu)
        modu@outputObjects$objectName
    ))))
    out$anyModOutputs <- lapply(
      deps,
      function(modu)
        modu@outputObjects$objectName
    )
    out$dontAllowModules <- unlist(lapply(out$anyModOutputs, function(x) any(x %in% out$thisModsInputs)))
  }
  out

}

.runEventFuture <- function(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan,
                            showSimilar = showSimilar, .pkgEnv, envir, futureNeeds) {
  message(crayon::magenta("        -- Spawning in a future"))
  modEnv <- sim$.mods[[cur[["moduleName"]]]]
  modObjs <- mget(ls(envir = modEnv), envir = modEnv)
  pkgs <- getFromNamespace("extractPkgName", "Require")(unlist(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs))
  list2env(modObjs, envir = envir)
  sim$simFuture[[paste(unlist(cur), collapse = "_")]] <-
    list(sim = future::future(getFromNamespace(".runEvent", "SpaDES.core")(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan,
                                                      showSimilar = showSimilar, .pkgEnv),

                              globals = c("sim", "cacheIt", "debug", "moduleCall", "fnEnv", "cur", "notOlderThan",
                                          "showSimilar", ".pkgEnv", names(modObjs)),
                              packages = c("SpaDES.core", pkgs),
                              envir = envir),
         thisModOutputs = list(moduleName = cur[["moduleName"]],
                               objects = futureNeeds$thisModOutputs,
                               dontAllowModules = names(futureNeeds$dontAllowModules)[futureNeeds$dontAllowModules]))
  sim
}

modNameInFuture <- function(simFuture) {
  gsub("^[[:digit:]]+\\_(.+)\\_.+\\_.+", "\\1", names(simFuture))
}

isListedEvent <- function(eventQueue, eventsToDo) {
  foundEventToDo <- FALSE
  i <- 1
  if (!is.null(eventsToDo)) {
    while (isFALSE(foundEventToDo)) {
      if (length(eventQueue) < i) { # check for
        # slot(sim, "current", check = FALSE) <- list() # same as no events left
        foundEventToDo <- NA
      } else {
        eventsToDoThisMod <- if (is.list(eventsToDo))
          eventsToDo[[eventQueue[[i]]$moduleName]]
        else
          eventsToDo
        if (is.null(eventsToDoThisMod)) {
          foundEventToDo <- TRUE
        } else if (eventQueue[[i]]$eventType %in% eventsToDoThisMod) {
          foundEventToDo <- TRUE
        } else {
          i <- i + 1
        }
      }
    }
  } else {
    foundEventToDo <- TRUE
  }
  if (is.na(foundEventToDo)) i <- 0L
  i
}

debugMessage <- function(debug, sim, cur, fnEnv, curModuleName) {
  if (!is(debug, "list") && !is.character(debug)) debug <- list(debug)
  for (i in seq_along(debug)) {
    if (isTRUE(debug[[i]]) | identical(debug[[i]], "current") | identical(debug[[i]], "step")) {
      if (length(cur) > 0) {
        if (debug[[i]] == "step") {
          if (interactive())
            readline("Press any key to continue...")
        }

        evnts1 <- data.frame(current(sim))
        evnts1new <- data.frame(current(sim))
        widths <- unname(unlist(lapply(format(evnts1), nchar)))
        .pkgEnv[[".spadesDebugWidth"]] <- pmax(widths, .pkgEnv[[".spadesDebugWidth"]])
        evnts1[1L, ] <- sprintf(paste0("%-",.pkgEnv[[".spadesDebugWidth"]],"s"), evnts1)
        if (.pkgEnv[[".spadesDebugFirst"]]) {
          evnts2 <- evnts1
          evnts2 <- evnts1
          evnts2[1L:2L, ] <- rbind(sprintf(paste0("%-",.pkgEnv[[".spadesDebugWidth"]],"s"), names(evnts2)),
                                   sprintf(paste0("%-",.pkgEnv[[".spadesDebugWidth"]],"s"), evnts2))

          outMess <- paste(unname(evnts2[1, ]), collapse = ' ')
          outMess <- c(outMess, paste(unname(evnts2[2, ]), collapse = ' '))
          # write.table(evnts2, quote = FALSE, row.names = FALSE, col.names = FALSE)
          .pkgEnv[[".spadesDebugFirst"]] <- FALSE
        } else {
          colnames(evnts1) <- NULL
          # write.table(evnts1, quote = FALSE, row.names = FALSE)
          outMess <- paste(unname(evnts1), collapse = ' ')
        }
      }
    } else if (identical(debug[[i]], 1)) {
      outMess <- paste0(" total elpsd: ", format(Sys.time() - sim@.xData$._startClockTime, digits = 2),
                        " | ", paste(format(unname(current(sim)), digits = 4), collapse = " "))
    } else if (identical(debug[[i]], 2)) {
      compareTime <- if (is.null(attr(sim, "completedCounter")) ||
                         attr(sim, "completedCounter") == 1) {
        sim@.xData$._startClockTime
      } else {
        .POSIXct(sim@completed[[as.character(attr(sim, "completedCounter") - 1)]]$._clockTime)
      }
      outMess <- paste0(" elpsd: ", format(Sys.time() - compareTime, digits = 2),
                        " | ", paste(format(unname(current(sim)), digits = 4), collapse = " "))
    } else {
      if (is(debug[[i]], "call")) {
        outMess <- try(eval(debug[[i]]))
      } else if (identical(debug[[i]], "simList")) {
        outMess <- try(capture.output(sim))
      } else if (isTRUE(grepl(debug[[i]], pattern = "\\("))) {
        outMess <- try(eval(parse(text = debug[[i]])))
      } else if (isTRUE(any(debug[[i]] %in% unlist(cur[c("moduleName", "eventType")])))) {
        outMess <- NULL
        if (is.environment(fnEnv)) {
          if (all(debug[[i]] %in% unlist(cur[c("moduleName", "eventType")]))) {
            debugonce(get(paste0("doEvent.", curModuleName), envir = fnEnv))
            on.exit(get(paste0("doEvent.", curModuleName), envir = fnEnv))
          }
        }
      } else if (!any(debug[[i]] %in% c("browser"))) { # any other
        if (!is.function(debug[[i]])) {
          outMess <- try(do.call(debug[[i]], list(sim)))
        } else  {
          outMess <- try(debug[[i]](sim))
        }
      }
    }
    if (is.data.frame(outMess)) {
      reproducible::messageDF(outMess, colour = "green", colnames = FALSE)
    } else {
      w <- getOption("width")
      suppress <- lapply(outMess, function(x)
        message(crayon::green(substring(x, first = 1, last = w - 30))))
    }
  }

}

updateParamSlotInAllModules <- function(paramsList, newParamValues, paramSlot,
                                        needClass, needValuesMess) {
  if (!is(newParamValues, needClass) && !is.na(newParamValues)) {
    if (missing(needValuesMess))
      needValuesMess <- ""
    stop(newParamValues, " must be class '",needClass,"'. It must be ", needValuesMess)
  }
  paramsLocal <- paramsList
  whNonHiddenModules <- !grepl(names(paramsList), pattern = "\\.")
  paramsList[whNonHiddenModules] <- lapply(paramsList[whNonHiddenModules], function(x) {
    x[[paramSlot]] <- newParamValues
    x
  })
  paramsList
}


loggingMessage <- function(mess, suffix = NULL, prefix = NULL) {
  st <- Sys.time()
  stForm1 <- "%h%d"
  stForm2 <- paste(stForm1, "%H:%M:%S")
  numCharsMax <- max(0, getOption("spades.messagingNumCharsModule", 21) - 15)
  if (numCharsMax > 0) {
    modName8Chars <- paste(rep(" ", numCharsMax), collapse = "")
    simEnv <- whereInStack("sim")
    sim <- get("sim", simEnv, inherits = FALSE)

    # If this is a nested spades call, then it will have a previous value in sim$._simPrevs
    #  That will be sufficient
    if (length(sim$._simPrevs)) {
      if (startsWith(mess, strftime(st, format = "%h%d"))) {
        mess <- gsub("^.{11,14} ", ": ", mess) # remove date
        sim <- get("sim", sim$._simPrevs[[1]])
      }
    }

    if (length(sim@current)) {
      modName <- sim@current$moduleName

      if (is.null(modName)) modName <- sim@events[[1]]$moduleName
      nchr <- nchar(modName)
      tooManyVowels <- nchr - numCharsMax
      numConsonnants <- nchar(gsub("[AEIOUaeiou]", "", modName))
      tooFewVowels <- if (numConsonnants >= numCharsMax) 0 else tooManyVowels
      modName8Chars <-
        paste0(" ", substr(gsub(paste0("(?<=\\S)[AEIOUaeiou]{",
                                       tooFewVowels,",",tooManyVowels,"}"), "",
                                modName, perl=TRUE), 1, numCharsMax))
      if (nchr < numCharsMax)
        modName8Chars <- paste0(modName8Chars,
                                paste(collapse = "", rep(" ", numCharsMax - nchr)))
    }
  } else {
    modName8Chars <- ""
  }
  if (!startsWith(mess, " ")) mess <- paste0(" ", mess)
  if (!is.null(suffix))
    modName8Chars <- paste0(modName8Chars, suffix)
  if (!is.null(prefix))
    modName8Chars <- paste0(prefix, modName8Chars)

  mess <- paste0(modName8Chars, mess)
  mess <- gsub("\\n", "", mess)

  paste0(strftime(st, format = stForm2), mess)
}


#' Alternative way to define events in SpaDES.core
#'
#' There are two ways to define what occurs during an event: defining a function
#' called doEvent.\emph{moduleName}, where \emph{moduleName} is the actual module name. This
#' approach is the original approach used in SpaDES.core, and it must have an
#' explicit \code{switch} statement branching on \code{eventType}. The newer approach
#' (still experimental) uses \code{defineEvent}. Instead of creating the
#' function called, `doEvent.XXXX`, where XXXX is the module name, it creates one function
#' for each event, each with the name `doEvent.XXXX.YYYY`, where `YYYY` is the event
#' name. This may be a little bit cleaner, but both with still work.
#'
#' @param sim A simList
#' @param eventName Character string of the desired event name to define. Default is "init"
#' @param moduleName Character string of the name of the module. If this function is
#'    used within a module, then it will try to find the module name.
#' @param code An expression that defines the code to execute during the event. This will
#'    be captured, and pasted into a new function (`doEvent.XXXX.YYYY`), where `XXXX` is the
#'    \code{moduleName} and \code{YYYY} is the \code{eventName}, remaining unevaluated until
#'    that new function is called.
#' @param envir An optional environment to specify where to put the resulting function.
#'     The default will place a function called `doEvent.moduleName.eventName` in the
#'     module function location, i.e., `sim$.mods[[moduleName]]`. However, if this
#'     location does not exist, then it will place it in the `parent.frame()`, with a message.
#'     Normally, especially, if used within SpaDES module code, this should be left missing.
#' @export
#' @seealso \code{\link{defineModule}}, \code{\link{simInit}}, \code{\link{scheduleEvent}}
#' @examples
#' sim <- simInit()
#' defineEvent(sim, "init", moduleName = "thisTestModule", code = {
#'   sim <- Init(sim) # initialize
#'   # Now schedule some different event for "current time", i.e., will
#'   #   be put in the event queue to run *after* this current event is finished
#'   sim <- scheduleEvent(sim, time(sim), "thisTestModule", "grow")
#' })
#'
#' defineEvent(sim, "grow", moduleName = "thisTestModule", code = {
#'   sim <- grow(sim) # grow
#'   # Now schedule this same event for "current time plus 1", i.e., a "loop"
#'   sim <- scheduleEvent(sim, time(sim) + 1, "thisTestModule", "grow") # for "time plus 1"
#' })
#'
#' Init <- function(sim) {
#'   sim$messageToWorld <- "Now the sim has an object in it that can be accessed"
#'   sim$size <- 1 # initializes the size object --> this can be anything, Raster, list, whatever
#'   message(sim$messageToWorld)
#'   return(sim)   # returns all the things you added to sim as they are in the simList
#' }
#'
#' grow <- function(sim) {
#'   sim$size <- sim$size + 1 # increments the size
#'   message(sim$size)
#'   return(sim)
#' }
#'
#' # schedule that first "init" event
#' sim <- scheduleEvent(sim, 0, "thisTestModule", "init")
#' out <- spades(sim)
#'
defineEvent <- function(sim, eventName = "init", code, moduleName = NULL, envir) {
  code <- substitute(code)
  curMod <- currentModule(sim)
  if (is.null(moduleName))
    moduleName <- currentModule(sim)

  if (missing(envir)) {
    useSimModsEnv <- FALSE
    if (is.null(moduleName)) {
      if (length(curMod) > 0) {
        useSimModsEnv <- TRUE
      }
    } else {
      if (exists(moduleName, sim$.mods, inherits = FALSE))
        useSimModsEnv <- TRUE
    }
    envir <- if (useSimModsEnv) sim$.mods[[moduleName]] else parent.frame()
  }
  fn <- paste0("
    fn <- function(sim, eventTime, eventType, priority) {
    ",
         paste(format(substitute(code)), collapse = "\n")
    ,"
    return(sim)
    }
  ")
  assign(paste0("doEvent.", moduleName, ".", eventName), eval(parse(text = fn)), envir = envir)
  return(invisible(sim))
}

