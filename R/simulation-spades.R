utils::globalVariables(c(".", ".I", "whi"))

################################################################################
#' Process a simulation event
#'
#' Internal function called from `spades`.
#'
#' Calls the module corresponding to the event call, and executes the event.
#'
#' Here, we implement a simulation in a more modular fashion so it's easier to add
#' submodules to the simulation. We use S4 classes and methods.
#'
#' @param sim Character string for the `simList` simulation object.
#' @param useFuture Experimental use of future::future package. Not fully implemented.
#'
#' @inheritParams spades
#' @return Returns the modified `simList` object.
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
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
  if (!inherits(sim, "simList")) {  ## July 2022: R 4.2 flags against using class()
    stop("doEvent can only accept a simList object")
  }

  # core modules
  core <- .pkgEnv$.coreModules

  if (isTRUE(useFuture)) {
    # Check here if resolved
    curForFuture <- sim@events[[1]]
    if (!curForFuture[["moduleName"]] %in% .pkgEnv$.coreModulesMinusSave) {
      if (length(sim$.simFuture)) {
        modInFuture <- modNameInFuture(sim$.simFuture)
        futureNeeds <- getFutureNeeds(deps = sim@depends@dependencies,
                                      curModName = modInFuture[1])#sim$.simFuture[[1]]$thisModOutputs$dontAllowModules)#curForFuture[["moduleName"]])
        canProceed <- if (length(futureNeeds)) {
          # with the assumption that the "unresolved" future could schedule itself,
          # must block any module who's outputs are needed by the same module as the
          # unresolved future module
          nextScheduledEvent <- curForFuture$moduleName
          if (nextScheduledEvent %in% "save") {
            outs <- outputs(sim)
            objsNeeded <- outs$objectName[outs$saveTime == events(sim)$eventTime[1]]
            nOtherFutures <- length(sim$.simFuture) - 1
            futureNeedsOther <- lapply(seq(nOtherFutures) + 1, function(i)
              getFutureNeeds(deps = sim@depends@dependencies,
                             curModName = modInFuture[i]))
            futureNeedsOther <- append(list(futureNeeds), futureNeedsOther)
            futureNeedsOtherDAM <- lapply(futureNeedsOther, function(fno) fno$dontAllowModules)
            futureNeeds$dontAllowModules <- colSums(Reduce(rbind, futureNeedsOtherDAM)) > 0
          }
          immediateDownstream <- names(futureNeeds$dontAllowModules)[futureNeeds$dontAllowModules]

          if (length(immediateDownstream))
            immediateDownstream <- immediateDownstream[immediateDownstream %in% curForFuture$moduleName]#&&
          length(immediateDownstream) == 0
          #modInFuture != curForFuture[["moduleName"]]
        } else {
          TRUE
        }


        if (!canProceed || future::resolved(sim$.simFuture[[1]][[1]])) {
          cause <- if (!canProceed) paste0(curForFuture$moduleName, " requires outputs from ", modInFuture[1])
          else if (curForFuture$moduleName == "save") paste0("Current event is 'save'; so resolving all")
          else paste0(modInFuture[1], " finished running")

          # resolveN <- if (curForFuture$moduleName == "save") length(sim$.simFuture) else 1L
          # for (iii in seq(resolveN))
          sim <- resolveFutureNow(sim, cause = cause)
          return(invisible(sim))
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
          moduleCallSeparateEventFns <- makeEventFn(curModuleName, cur$eventType)
          if (!is.null(sim@.xData[[eventFnElementEnvir()]])) {
            fnEnv <- sim@.xData[[eventFnElementEnvir()]][[moduleCallSeparateEventFns]]$envir
            moduleCall <- moduleCallSeparateEventFns
          } else {
            if (exists(moduleCallSeparateEventFns, envir = fnEnv)) { # don't specify inherits = FALSE because might be elsewhere
              moduleCall <- moduleCallSeparateEventFns
            }

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

          # In general... allow a spawning, unless it is literally next event
          #   This is just a heuristic because other events could be inserted before
          #    the next event ... but this is a decent guess
          nextScheduledEvent <- sim@events[[1]]$moduleName
          objsNeeded <- NULL
          if (nextScheduledEvent %in% "save") {
            outs <- outputs(sim)
            objsNeeded <- outs$objectName[outs$saveTime == events(sim)$eventTime[1]]
          }
          objsNeededForNextMod <- futureNeeds$anyModInputs[[nextScheduledEvent]]
          selfObjects <- futureNeeds$thisModOutputs[futureNeeds$thisModOutputs %in% futureNeeds$thisModsInputs]
          objsNeeded <- unique(c(objsNeeded, objsNeededForNextMod, selfObjects))
          if (!any(futureNeeds$thisModOutputs %in% objsNeeded)) {
            spacing <- paste(rep(" ", sim[[".spadesDebugWidth"]][1] + 1), collapse = "")
            messageVerbose(
              crayon::magenta(paste0(spacing, cur[["moduleName"]], " outputs not needed by ",
                            "next module (", nextScheduledEvent, ")")),
              verbose = 1 - (debug %in% FALSE))
            # don't use cur from above because it is "seconds" which mess with future
            cur2 <- unlist(current(sim))
            cur2[["eventTime"]] <- as.numeric(cur2[["eventTime"]])
            simFuture <- sim$.simFuture
            sim$.simFuture <- list()
            sim <- .runEventFuture(sim, cacheIt, debug, moduleCall, fnEnv, cur2, notOlderThan,
                                   showSimilar = showSimilar, .pkgEnv, envir = environment(),
                                   futureNeeds = futureNeeds)
            sim$.simFuture <- append(simFuture, sim$.simFuture)
            sim <- runScheduleEventsOnly(sim, currnt = cur2) # run the scheduleEvents only
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
          if (!isFALSE(sim@.xData$._ranInitDuringSimInit)) {
            cc <- NROW(sim@completed) + 1
            cs <- cc * 2
          } else {
            cc <- 1
            cs <- 2
          }
          attr(sim, "completedCounter") <- cc
          attr(sim, "completedSize") <- cs
          sim@completed[[as.character(cc)]] <- cur
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
#' @param sim            A `simList` simulation object.
#'
#' @param eventTime      A numeric specifying the time of the next event.
#'
#' @param moduleName     A character string specifying the module from which to
#'                       call the event. If missing, it will use
#'                       `currentModule(sim)`
#'
#' @param eventType      A character string specifying the type of event from
#'                       within the module.
#'
#' @param eventPriority  A numeric specifying the priority of the event.
#'                       Lower number means higher priority. As a best practice, it is
#'                       recommended that decimal values are conceptual
#'                       grouped by their integer values (e.g., 4.0, 4.25, 4.5 are conceptually
#'                       similar).
#'                       See [priority()].
#' @param .skipChecks Logical. If `TRUE`, then internal checks that arguments match
#'                    expected types are skipped. Should only be used if speed is critical.
#'
#' @return Returns the modified `simList` object.
#'
#' @include priority.R
#' @export
#' @rdname scheduleEvent
#' @seealso [priority()], [scheduleConditionalEvent()]
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
#'
#' @examples
#'  sim <- simInit()
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn") # default priority
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn", .normal()) # default priority
#'
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn", .normal()-1) # higher priority
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn", .normal()+1) # lower priority
#'
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn", .highest()) # highest priority
#'  sim <- scheduleEvent(sim, time(sim) + 1.0, "fireSpread", "burn", .lowest()) # lowest priority
#'  events(sim) # shows all scheduled events, with eventTime and priority
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
    if (!inherits(sim, "simList")) {
      stop("sim must be a simList")  ## July 2022: R 4.2 flags against using class()
    }

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

#' Schedule a conditional simulation event
#'
#' Adds a new event to the simulation's conditional event queue,
#' updating the simulation object by creating or appending to
#' `sim$._conditionalEvents`.
#' *This is very experimental. Use with caution.*
#'
#' This conditional event queue will be assessed at every single event in the normal event
#' queue. If there are no conditional events, then `spades` will proceed as normal.
#' As conditional event conditions are found to be true, then it will trigger a call to
#' `scheduleEvent(...)` with the current time passed to `eventTime` *and*
#' it will remove the conditional event from the conditional queue.
#' If the user would like the triggered conditional event to occur as the very next event,
#' then a possible strategy would be to set `eventPriority` of the conditional event
#' to very low or even negative to ensure it gets inserted at the top of the event queue.
#'
#' @inheritParams scheduleEvent
#'
#' @param minEventTime   A numeric specifying the time before which the event should not occur,
#'         even if the condition is met. Defaults to `start(sim)`
#'
#' @param maxEventTime   A numeric specifying the time after which the event should not occur,
#'         even if the condition is met. Defaults to `end(sim)`
#'
#' @param condition A string, call or expression that will be assessed for `TRUE`
#'      after each event in the regular event queue.
#'      It can access objects in the `simList` by using functions of `sim`,
#'      e.g., `"sim$age > 1"`
#'
#' @return Returns the modified `simList` object, i.e., `sim$._conditionalEvents`.
#'
#'
#' @include priority.R
#' @export
#' @rdname scheduleConditionalEvent
#' @seealso [scheduleEvent()], [conditionalEvents()]
#'
#' @author Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
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
                                     eventPriority = .normal(),
                                     minEventTime = start(sim),
                                     maxEventTime = end(sim)) {
  if (!inherits(sim, "simList")) {
    stop("sim must be a simList") ## July 2022: R 4.2 flags against using class()
  }

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
#' @param sim A `simList` simulation object, generally produced by `simInit`.
#'
#' @param debug Optional tools for invoking debugging. Supplying a `list`
#'              will invoke the more powerful `logging` package. See details.
#'              Default is to use the value in `getOption("spades.debug")`.
#'
#' @param progress Logical (`TRUE` or `FALSE` show a graphical progress bar),
#'                 character (`"graphical"`, `"text"`) or numeric indicating
#'                 the number of update intervals to show in a graphical progress bar.
#'
#' @param cache Logical. If `TRUE`, then the `spades` call will be cached.
#'              This means that if the call is made again with the same `simList`,
#'              then `spades` will return the return value from the previous run
#'              of that exact same `simList`. Default `FALSE`. See Details.
#'              See also the vignette on caching for examples.
#'
#' @param .plotInitialTime Numeric. Temporarily override the `.plotInitialTime`
#'                                  parameter for all modules. See Details.
#'
#' @param .saveInitialTime Numeric. Temporarily override the `.plotInitialTime`
#'                                  parameter for all modules. See Details.
#'
#' @param .plots Character. Sets the parameter of this name in all modules.
#'   See [Plots()] for possible values. The parameter is intended to slowly
#'   take over from `.plotInitialTime` as a mechanism to turn on or off plotting.
#'   For backwards compatibility, if `.plotInitialTime` is not set in this `spades` call,
#'   but this `.plots` is used, two things will happen: setting this without `"screen"`
#'   will turn off all plotting; setting this with `"screen"` will trigger
#'   plotting for any modules that use this parameter but will have no effect on
#'   other modules. To get plotting, therefore, it may be necessary to also set
#'   `.plotInitialTime = start(sim)`.
#'
#' @param notOlderThan Date or time. Passed to `reproducible::Cache` to update the cache.
#'                     Default is `NULL`, meaning don't update the cache.
#'                     If `Sys.time()` is provided, then it will force a recache,
#'                     i.e., remove old value and replace with new value.
#'                     Ignored if `cache` is `FALSE`.
#' @param events A character vector or a named list of character vectors. If specified,
#'   the simulations will only do the events indicated here. If a named list, the names
#'   must correspond to the modules and the character vectors can be specific events within
#'   each of the named modules. With the `list` form, all unspecified modules
#'   will run *all* their events, including internal spades modules, e.g., `save`,
#'   that get invoked with the `outputs` argument in  `simInit`. See example.
#'
#' @param ... Any. Can be used to make a unique cache identity, such as "replicate = 1".
#'            This will be included in the `Cache` call, so will be unique
#'            and thus `spades` will not use a cached copy as long as
#'            anything passed in `...` is unique, i.e., not cached previously.
#'
#' @return Invisibly returns the modified `simList` object.
#'
#' @seealso [SpaDES.core-package()],
#' [simInit()], and the caching vignette (very important for reproducibility):
#' <https://spades-core.predictiveecology.org/articles/iii-cache.html> which
#' uses [reproducible::Cache()].
#'
#'
#' @details
#' The is the workhorse function in the SpaDES package. It runs simulations by
#' implementing the rules outlined in the `simList`.
#'
#' This function gives simple access to two sets of module parameters:
#' `.plotInitialTime` and with `.plotInitialTime`. The primary use of
#' these arguments is to temporarily turn off plotting and saving. "Temporary"
#' means that the `simList` is not changed, so it can be used again with
#' the simList values reinstated. To turn off plotting and saving, use
#' `.plotInitialTime = NA` or `.saveInitialTime = NA`. NOTE: if a
#' module did not use `.plotInitialTime` or `.saveInitialTime`, then
#' these arguments will not do anything.
#'
#' @section Caching with SpaDES:
#'
#' There are numerous ways in which Caching can be used within SpaDES. Please
#' see the vignette
#' <https://spades-core.predictiveecology.org/articles/iii-cache.html>
#' for many examples. Briefly, functions, events, modules, entire spades calls or
#' experiment calls (see <https://github.com/PredictiveEcology/SpaDES.experiment>)
#' can be cached and mixtures of all of these will work. For functions, simply
#' wrap the call with `Cache`, moving the original function name into
#' the first argument of Cache. For events or modules, set the module `parameters`,
#' `.useCache`, e.g.,
#' `simInit(..., parameters = list(myModule = list(.useCache = "init")))`.
#' This can be set to an event name, which will cache that event, or a logical (e.g., \code{}),
#' which will cache *every* event in that module. Event and module caching
#' makes most sense when the event or module only runs once, such as an initialization
#' or data preparation event/module. Caching an entire simulation is actually just
#' a function call to `simInitAndSpades`, for example. So, simply writing
#' `Cache(simInitAndSpades, modules = ...)` will effectively cache a whole simulation.
#' Finally for experiments, it is just like a function call:
#' `Cache(simInitandExperiment, ...)`. The final way Caching can be done is in
#' `experiment` or `spades`, by setting the `cache` argument.
#'
#' If `cache` is TRUE, this allows for a seamless way to "save" results
#' of a simulation. The  user does not have to intentionally do any saving manually.
#' Instead, upon a call to `spades` in which the simList is identical,
#' the function will simply return the result that would have come if it had
#' been rerun. Use this with caution, as it will return exactly the result from
#' a previous run, even if there is stochasticity internally.
#' Caching is only based on the input simList.
#' See also the vignette on caching for examples.
#'
#' @section `debug`:
#'
#' The most powerful way to use debug is to invoke the `logging`
#' R package. To invoke this, `debug` must be a list with up to 3
#' named elements:
#' `console`, `file`, and `debug`. Each of these list elements
#' must be a list (including empty `list()` for defaults) with the
#' sub-list elements here:
#' \tabular{lll}{
#'   `console` \tab `level` \tab The `level`, see below, of information shown\cr
#'   `file` \tab `append` \tab Logical. If `TRUE`, the default, then
#'                                       log entries are appended to file, if it exists\cr
#'               \tab `file` \tab A filename. Defaults to `log.txt`\cr
#'               \tab `level` \tab The `level`, see below, of information shown\cr
#'   `debug` \tab See possible values below\cr
#'   }
#'
#' `level` can be a number from 0 to 100 or a character string matching one
#' of the values in `logging::loglevels`. These are hierarchical levels of
#' information passed to the console. Set a lower number for more information and a
#' higher number for less information. Errors in code will be shown if `level`
#' is set to `"ERROR"` or `40` or above; warnings in code will be shown if
#' `level` is set to `"WARN"` or `30` or above;
#' normal messages in code will
#' be shown if `level` is set to `"INFO"` or `20` or above. For
#' consistency with base R messaging, if default level is used, then normal
#' messaging via `message` will be shown; this means that `suppressMessages`
#' will work to suppress messaging only when level is set to `"INFO"` or `20`.
#' Some functions in the SpaDES ecosystem may have information at the lower levels,
#' but currently, there are few to none.
#'
#' `debug` is specified as a non-list argument to `spades` or as
#' `list(debug = ...)`, then it can be a logical, a quoted call, a character vector
#' or a numeric scalar (currently 1 or 2) or a list of any of these to get multiple
#' outputs. This will be run at the start of every event. The following options for debug
#' are available. Each of these can also be in a list to get multiple outputs:
#'
#' \tabular{ll}{
#'   `TRUE` \tab `current(sim)` will be printed at the start of each event as
#'                     it runs\cr
#'   a function name (as character string) \tab If a function, then it will be run on the
#'                                            simList, e.g., "time" will run
#'                                            `time(sim)` at each event.\cr
#'   moduleName (as character string) \tab All calls to that module will be entered
#'                                         interactively\cr
#'   eventName (as character string) \tab All calls that have that event name (in any module)
#'                                        will be entered interactively\cr
#'   `c(<moduleName>, <eventName>)`  \tab Only the event in that specified module
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
#' option `spades.debug` is used.
#'
#' If `options("spades.browserOnError" = TRUE)` (experimental still) if
#' there is an error, it will attempt to open a browser
#' in the event where the error occurred. You can edit, and then press `c` to continue
#' or `Q` to quit, plus all other normal interactive browser tools.
#' `c` will trigger a reparse and events will continue as scheduled, starting
#' with the one just edited. There may be some unexpected consequences if the
#' `simList` objects had already been changed before the error occurred.
#'
#' @note The debug option is primarily intended to facilitate building simulation
#' models by the user.
#' Will print additional outputs informing the user of updates to the values of
#' various `simList` slot components.
#' See <https://github.com/PredictiveEcology/SpaDES/wiki/Debugging> for details.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @importFrom crayon blue magenta
#' @importFrom data.table setDTthreads
#' @export
#' @rdname spades
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from <https://nostarch.com/artofr.htm>
#'
#' @examples
#' \donttest{
#' if (requireNamespace("SpaDES.tools", quietly = TRUE) &&
#'     requireNamespace("NLMR", quietly = TRUE)) {
#'   opts <- options("spades.moduleCodeChecks" = FALSE) # not necessary for example
#'   if (!interactive()) opts <- append(opts, options("spades.plots" = NA))
#'   mySim <- simInit(
#'    times = list(start = 0.0, end = 1.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#'   )
#'   spades(mySim)
#'
#'   # Different debug options (overrides the package option 'spades.debug')
#'   spades(mySim, debug = TRUE) # Fastest
#'   spades(mySim, debug = "simList")
#'   spades(mySim, debug = "print(table(sim$landscape$Fires[]))")
#'   # To get a combination -- use list(debug = list(..., ...))
#'   spades(mySim, debug = list(debug = list(1, quote(as.data.frame(table(sim$landscape$Fires[]))))))
#'
#'   # Can turn off plotting, and inspect the output simList instead
#'   out <- spades(mySim, .plots = NA) # much faster
#'   completed(out) # shows completed events
#'
#'   # use cache -- simInit should generally be rerun each time a spades call is made
#'   #   to guarantee that it is identical. Here, run spades call twice, first
#'   #   time to establish cache, second time to return cached result
#'   for (i in 1:2) {
#'    mySim <- simInit(
#'      times = list(start = 0.0, end = 1.0, timeunit = "year"),
#'      params = list(
#'        .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'      ),
#'      modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'      paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#'    )
#'    print(system.time(out <- spades(mySim, cache = TRUE)))
#'   }
#'
#'   # E.g., with only the init events
#'   outInitsOnly <- spades(mySim, events = "init")
#'
#'   # or more fine grained control
#'   outSomeEvents <- spades(mySim,
#'           events = list(randomLandscapes = c("init"),
#'                         fireSpread = c("init", "burn")))
#'
#'   # with outputs, the save module gets invoked and must be explicitly limited to "init"
#'   mySim <- simInit(
#'    times = list(start = 0.0, end = 1.0, timeunit = "year"),
#'    params = list(
#'      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
#'    ),
#'    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'    outputs = data.frame(objectName = "landscape", saveTime = 0:2),
#'    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#'   )
#'   # This will print a message saying that caribouMovement will run its events
#'   outSomeEvents <- spades(mySim,
#'           events = list(randomLandscapes = c("init"),
#'                         fireSpread = c("init", "burn"),
#'                         save = "init"))
#'
#'   options(opts) # reset options
#' }
#' }
#'
setGeneric(
  "spades",
  function(sim, debug = getOption("spades.debug"), progress = NA, cache,
           .plotInitialTime = NULL, .saveInitialTime = NULL, notOlderThan = NULL,
           events = NULL, .plots = getOption("spades.plots", NULL), ...) {
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
        if (requireNamespace("future", quietly = TRUE) &&
            requireNamespace("future.callr", quietly = TRUE)) {
          originalPlan <- future::plan()
          sim <- memoryUseSetup(sim, originalPlan)
          on.exit({
            sim <- memoryUseOnExit(sim, originalPlan)
          }, add = TRUE)
        } else {
          stop(futureMessage)
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
            needClass = "integer")
        if (!is.null(.plotInitialTime)) {
          message("Both .plots and .plotInitialTime are supplied; using .plots")
        }
        .plotInitialTime <- .plots ## TODO: .plotInitialTime is numeric, not character! fails check below
      }

      if (!is.null(.plotInitialTime)) {
        sim@params <- updateParamSlotInAllModules(
          sim@params, .plotInitialTime, ".plotInitialTime",
          needClass = "numeric") ## TODO: fails if .plotInitialTime takes .plots value above
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
        sim[[".spadesDebugWidth"]] <- c(9, 10, 9, 13)
      }

      sim@.xData[["._firstEventClockTime"]] <- Sys.time()

      # This was introduced when sim@completed became an environment for speed purposes
      # (list got slow as size increased)
      # This is an attempt to deal with the expected behaviour of a list --
      # i.e., delete it if this appears to be the original sim object again passed in
      if (length(sim@completed)) {
        existingCompleted <- sort(as.integer(ls(sim@completed, sorted = FALSE)))
        prevStart <- get(as.character(existingCompleted[1]), envir = sim@completed)
        # prevEnd <- get(as.character(existingCompleted[length(existingCompleted)]), envir = sim@completed)
        if (length(.grepSysCalls(sys.calls(), "restartSpades")) == 0 &&
            length(sim@.xData$._ranInitDuringSimInit) == 0)  { # don't crop off completed events if Init(s) ran during simInit
          prevEvUnit <- attr(prevStart[["eventTime"]], "unit")
          stTime <- start(sim, unit = prevEvUnit)
          if (stTime <= prevStart[["eventTime"]] && (time(sim, unit = prevEvUnit) == stTime))
            sim@completed <- new.env(parent = emptyenv())
        }
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

        message(crayon::magenta("useFuture is set to TRUE; this will attempt to spawn events"))
        message(crayon::magenta("  in a separate process, if their outputs are not needed immediately"))
        message(crayon::magenta("  STILL EXPERIMENTAL. Use cautiously."))
        message(crayon::magenta("  User must manage future::plan, e.g., \nfuture::plan(multisession(workers = 2))"))
        sim$.futureEventsSkipped <- 0
        sim$.simFuture <- list()
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
        # if (useFuture) {
        #   if (length(sim$.simFuture) > 1) {
        #     for (simFut in seq_along(sim$.simFuture)) {
        #       if (future::resolved(sim$.simFuture[[1]][[1]])) {
        #         sim <- resolveFutureNow(sim, cause = paste0(modNameInFuture(sim$.simFuture[1]), " finished running"))
        #       }
        #     }
        #   }
        # }
      }
      if (useFuture) {
        if (length(sim$.simFuture)) {
          for (simFut in seq_along(sim$.simFuture)) {
            sim <- resolveFutureNow(sim, cause = "End of simulation")
          }
        }
        message(crayon::magenta(sim$.futureEventsSkipped, " events ran in futures"))
      }
      sim@simtimes[["current"]] <- sim@simtimes[["end"]]

      # For determining if clean ending to spades call
      .pkgEnv$.cleanEnd <- TRUE
      return(invisible(sim))
    },
    warning = function(w) {
      if (newDebugging && requireNamespace("logging", quietly = TRUE)) {
        logging::logwarn(paste0(collapse = " ", c(names(w), w)))
      }
    },
    error = function(e) {
      if (newDebugging && requireNamespace("logging", quietly = TRUE)) {
        if (debug > 0)
          logging::logerror(e)
      } else {
        fn <- get0("onError")
        if (!is.null(fn))
          fn(sim)
        stop(e)
      }
    },
    message = function(m) {
      if (newDebugging && requireNamespace("logging", quietly = TRUE)) {
        logging::loginfo(m$message)
      }
      if (useNormalMessaging) {
        if (startsWith(m$message, "\b")) {
          message(gsub("\n$", "", m$message))
        } else {
          message(loggingMessage(m$message))
        }
      }
      # This will "muffle" the original message
      tryCatch(invokeRestart("muffleMessage"), error = function(e) NULL)
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
    stopifnot(inherits(sim, "simList")) ## July 2022: R 4.2 flags against using class()

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
        Cache(spades(sim = sim,
                     debug = debug,
                     progress = progress,
                     .plotInitialTime = .plotInitialTime,
                     .saveInitialTime = .saveInitialTime,
                     omitArgs = omitArgs,
                     notOlderThan = notOlderThan,
                     events = events,
                     .plots = .plots,
                     cache = FALSE,
                     ...
        ))
        # do.call(quote = TRUE, Cache,
        #         args = append(
        #           list(
        #             spades,
        #             sim = sim,
        #             debug = debug,
        #             progress = progress,
        #             .plotInitialTime = .plotInitialTime,
        #             .saveInitialTime = .saveInitialTime,
        #             omitArgs = omitArgs,
        #             notOlderThan = notOlderThan,
        #             events = events,
        #             .plots = .plots
        #           ),
        #           dots
        #        )
        # )
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
                           paths = FALSE, outputs = FALSE,
                           params = sim@params[[cur[["moduleName"]]]],
                           .globals = sim@params[[".globals"]],
                           modules = cur[["moduleName"]])
    }
  }
  fnCallAsExpr <- if (cacheIt) { # means that a module or event is to be cached
    modCall <- get(moduleCall, envir = fnEnv)
    expression(Cache(FUN = modCall,
                     sim = sim,
                     eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                     .objects = moduleSpecificObjects,
                     notOlderThan = notOlderThan,
                     outputObjects = moduleSpecificOutputObjects,
                     classOptions = classOptions,
                     showSimilar = showSimilar,
                     cachePath = sim@paths[["cachePath"]],
                     .functionName = moduleCall))
  } else {
    # Faster just to pass the NULL and just call it directly inside .runEvent
    expression(get(moduleCall,
                   envir = fnEnv)(sim, cur[["eventTime"]], cur[["eventType"]]))
  }

  if (!(FALSE %in% debug || any(is.na(debug))) )
    objsIsNullBefore <- objsAreNull(sim)

  if (.pkgEnv[["spades.browserOnError"]]) {
    sim <- .runEventWithBrowser(sim, fnCallAsExpr, moduleCall, fnEnv, cur)
  } else {
    sim <- eval(fnCallAsExpr) # slower than more direct version just above
  }
  if (!(FALSE %in% debug || any(is.na(debug))) )
    objectsCreatedPost(sim, objsIsNullBefore)

  # Test for memory leaks
  if (getOption("spades.testMemoryLeaks", TRUE)) {
    if (!is.null(sim@.xData$.mods[[cur[["moduleName"]]]]$.objects))
      sim$._knownObjects <- testMemoryLeaks(simEnv = sim@.xData,
                                            modEnv = sim@.xData$.mods[[cur[["moduleName"]]]]$.objects,
                                            modName = cur[["moduleName"]],
                                            knownObjects = sim@.xData$._knownObjects)
  }

  return(sim) # TEMPORARY THIS IS FOR TESTING DEBUGGING IN FUTURE
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
  text <- paste(record$timestamp, paste(record$levelname, record$logger,
                                        gsub("\n$", "", record$msg), sep = ":"), sep = "")
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
  futureRunning[1:4] <- as.list(strsplit(names(sim$.simFuture)[1], split = "_")[[1]])
  futureRunning[["eventTime"]] <- as.numeric(futureRunning[["eventTime"]])
  futureRunning[["eventPriority"]] <- as.numeric(futureRunning[["eventPriority"]])
  futureRunningSimTU <- futureRunning
  setDT(futureRunningSimTU)
  setDT(futureRunning)
  spacing <- paste(rep(" ", sim[[".spadesDebugWidth"]][1] + 1), collapse = "")

  outMess <- debugMessTRUE(sim, events = futureRunningSimTU)

  # # If it had no outputs, then skip -- wait -- comment out for now ... the "events" stuff; but that may be unnecessary
  #   because of the runScheduleEventsOnly, which "should" have found all the events.
  # if (length(sim$.simFuture[[1]]$thisModOutputs$objects)) {
  tmpSim <- future::value(sim$.simFuture[[1]][[1]])
  tmpSim <- .unwrap(tmpSim)
  simMetadata <- sim$.simFuture[[1]][[2]]

  # objects
  list2env(mget(simMetadata$objects, envir = envir(tmpSim)), envir = envir(sim))
  # }
  message(crayon::magenta(paste0(" Resolved: ", outMess)))
  message(crayon::magenta("   ", spacing, "because", cause))
  allCols <- c("eventTime", "moduleName", "eventTime", "eventPriority")
  # events
  evntsFut <- events(tmpSim, unit = "seconds")
  compltd <- completed(sim, unit = "seconds")[, 1:4]
  evntsNormal <- rbindlist(list(compltd, current(sim, unit = "seconds"), events(sim, unit = "seconds")))
  newEvents <- evntsFut[!evntsNormal, on = allCols]

  if (NROW(newEvents)) {
    warning("Seeing this message means that the runScheduleEventsOnly did not find all events; ",
            "please use `switch` and `scheduleEvent` inside the doEvent.", currentModule(tmpSim),
            " function")
    newEvents <- lapply(seq(NROW(newEvents)), function(x) as.list(newEvents[x]))
    slot(sim, "events", check = FALSE) <- append(sim@events, newEvents)
    ord <- order(unlist(lapply(sim@events, function(x) x$eventTime)),
                 unlist(lapply(sim@events, function(x) x$eventPriority)))
    slot(sim, "events") <- sim@events[ord]
  }
  sim$.simFuture <- sim$.simFuture[-1]

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
    out$anyModInputs <- lapply(
      deps,
      function(modu)
        modu@inputObjects$objectName
    )
    out$dontAllowModules <- unlist(lapply(out$anyModInputs, function(x) any(x %in% out$thisModOutputs)))
  }
  out
}

.runEventFuture <- function(sim, cacheIt, debug, moduleCall, fnEnv, cur, notOlderThan,
                            showSimilar = showSimilar, .pkgEnv, envir, futureNeeds) {
  spacing <- paste(rep(" ", sim[[".spadesDebugWidth"]][1]), collapse = "")
  message(crayon::magenta(spacing, "-- Spawning in a future"))
  sim$.futureEventsSkipped <- sim$.futureEventsSkipped + 1
  modEnv <- sim$.mods[[cur[["moduleName"]]]]
  objsToGet <- grep("^\\._", ls(envir = modEnv, all.names = TRUE), value = TRUE, invert = TRUE)
  modObjs <- mget(objsToGet, envir = modEnv)
  pkgs <- getFromNamespace("extractPkgName", "Require")(unlist(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs))
  list2env(modObjs, envir = envir)
  objsKepts <- na.omit(inputObjects(sim, module = currentModule(sim))[["objectName"]])
  globs <- list(sim = .wrap(sim[objsKepts]), cacheIt, debug, moduleCall, fnEnv, cur,
                notOlderThan, showSimilar, .pkgEnv)# names(modObjs))
  names(globs) <- c("sim", "cacheIt", "debug", "moduleCall", "fnEnv", "cur", "notOlderThan",
                    "showSimilar", ".pkgEnv") #names(modObjs))

  # When a name begins with a number, touchy to access it. Here reverse 1st 2 so character is first
  futureListLabel <- paste(unlist(cur), collapse = "_")
  sim$.simFuture[[futureListLabel]] <-
    list(sim = future::future(
      getFromNamespace(".runForFutureWrapper", "SpaDES.core")(sim, cacheIt = cacheIt,
                                                              debug = debug, moduleCall = moduleCall,
                                                              fnEnv = fnEnv, cur = cur, notOlderThan = notOlderThan,
                                                              showSimilar = showSimilar, .pkgEnv = .pkgEnv),
      globals = globs,
      packages = c("SpaDES.core", pkgs),
      # envir = envir,
      seed = TRUE),
      thisModOutputs = list(moduleName = cur[["moduleName"]],
                            objects = futureNeeds$thisModOutputs,
                            dontAllowModules = names(futureNeeds$dontAllowModules)[futureNeeds$dontAllowModules]))
  sim
}

.runForFutureWrapper <- function(sim, ...) {
  sim <- .unwrap(sim)
  sim <- .runEvent(sim, ...)
  .wrap(sim)
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

#' @importFrom crayon green
debugMessage <- function(debug, sim, cur, fnEnv, curModuleName) {
  if (!is(debug, "list") && !is.character(debug)) debug <- list(debug)
  if (!any(vapply(debug, function(x) x %in% 1:2, FUN.VALUE = logical(1))))
    debug <- append(list(1L), debug)
  for (i in seq_along(debug)) {
    if (isTRUE(debug[[i]]) | identical(debug[[i]], "current") | identical(debug[[i]], "step")) {
      if (length(cur) > 0) {
        if (debug[[i]] == "step") {
          if (interactive())
            readline("Press any key to continue...")
        }
        outMess <- debugMessTRUE(sim)
      }
    } else if (isTRUE(debug[[i]] %in% 1)) {
      outMess <- paste0(" total elpsd: ", format(Sys.time() - sim@.xData$._startClockTime, digits = 2),
                        " | ", paste(format(unname(current(sim)), digits = 4), collapse = " "))
    } else if (isTRUE(debug[[i]] %in% 2)) {
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
          outMess <- tryCatch(do.call(debug[[i]], list(sim)), silent = TRUE,
                              error = function(e) NULL)
        } else  {
          outMess <- try(debug[[i]](sim))
        }
      } else {
        outMess <- NULL
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
    stop(newParamValues, " must be class '", needClass, "'. It must be ", needValuesMess)
  }
  paramsLocal <- paramsList
  whNonHiddenModules <- !grepl(names(paramsList), pattern = "\\.")
  paramsList[whNonHiddenModules] <- lapply(paramsList[whNonHiddenModules], function(x) {
    x[[paramSlot]] <- newParamValues
    x
  })
  paramsList
}

loggingMessagePrefixLength <- 15

loggingMessage <- function(mess, suffix = NULL, prefix = NULL) {
  st <- Sys.time()
  stForm1 <- "%h%d"
  stForm2 <- paste(stForm1, "%H:%M:%S")
  numCharsMax <- max(0, getOption("spades.messagingNumCharsModule", 21) - loggingMessagePrefixLength)
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
      modName <- sim@current$moduleName # only defined if in doEvent
      if (is.null(modName)) modName <- sim@events[[1]]$moduleName

      modName8Chars <- moduleNameStripped(modName, numCharsMax)
      # nchr <- nchar(modName)
      # tooManyVowels <- nchr - numCharsMax
      # numConsonnants <- nchar(gsub("[AEIOUaeiou]", "", modName))
      # tooFewVowels <- if (numConsonnants >= numCharsMax) 0 else tooManyVowels
      # modName8Chars <-
      #   paste0(" ", substr(gsub(paste0("(?<=\\S)[AEIOUaeiou]{",
      #                                  tooFewVowels,",",tooManyVowels,"}"), "",
      #                           modName, perl=TRUE), 1, numCharsMax))
      # if (nchr < numCharsMax)
      #   modName8Chars <- paste0(modName8Chars,
      #                           paste(collapse = "", rep(" ", numCharsMax - nchr)))
    }
  } else {
    modName8Chars <- ""
  }
  prependTime <- strftime(st, format = stForm2)
  if (grepl(prependTime, mess)) { # spades or simInit inside a spades or simInit
    prependTime <- " -- "
  } else {
    if (!startsWith(mess, " ")) mess <- paste0(" ", mess)
    if (!is.null(suffix))
      modName8Chars <- paste0(modName8Chars, suffix)
    if (!is.null(prefix))
      modName8Chars <- paste0(prefix, modName8Chars)

    mess <- paste0(modName8Chars, mess)
  }
  mess <- gsub("\\n", "", mess)
  paste0(prependTime, mess)
}


#' Alternative way to define events in SpaDES.core
#'
#' There are two ways to define what occurs during an event: defining a function
#' called doEvent.*moduleName*, where *moduleName* is the actual module name. This
#' approach is the original approach used in SpaDES.core, and it must have an
#' explicit `switch` statement branching on `eventType`. The newer approach
#' (still experimental) uses `defineEvent`. Instead of creating the
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
#'    `moduleName` and `YYYY` is the `eventName`, remaining unevaluated until
#'    that new function is called.
#' @param envir An optional environment to specify where to put the resulting function.
#'     The default will place a function called `doEvent.moduleName.eventName` in the
#'     module function location, i.e., `sim$.mods[[moduleName]]`. However, if this
#'     location does not exist, then it will place it in the `parent.frame()`, with a message.
#'     Normally, especially, if used within SpaDES module code, this should be left missing.
#' @export
#' @seealso [defineModule()], [simInit()], [scheduleEvent()]
#' @examples
#' sim <- simInit()
#'
#' # these put the functions in the parent.frame() which is .GlobalEnv for an interactive user
#' defineEvent(sim, "init", moduleName = "thisTestModule", code = {
#'   sim <- Init(sim) # initialize
#'   # Now schedule some different event for "current time", i.e., will
#'   #   be put in the event queue to run *after* this current event is finished
#'   sim <- scheduleEvent(sim, time(sim), "thisTestModule", "grow")
#' }, envir = envir(sim))
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
#' # Look at event queue
#' events(sim) # shows the "init" we just added
#' \donttest{
#'   # this is skipped when running in automated tests; it is fine in interactive use
#'   out <- spades(sim)
#' }
#'
defineEvent <- function(sim, eventName = "init", code, moduleName = NULL, envir) {
  code <- substitute(code)
  curMod <- currentModule(sim)
  if (is.null(moduleName))
    moduleName <- currentModule(sim)

  useSimModsEnv <- FALSE
  if (missing(envir)) {
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

  eventFnName <-  makeEventFn(moduleName, eventName)
  parsedFn <- parse(text = fn)
  if (!useSimModsEnv) {
    if (is.null(sim@.xData[[eventFnElementEnvir()]])) {
      sim@.xData[[eventFnElementEnvir()]] <- new.env(parent = asNamespace("SpaDES.core"))
    }
    sim@.xData[[eventFnElementEnvir()]][[eventFnName]] <- list(envir = envir,
                                                          digest = .robustDigest(parsedFn))
  }

  assign(eventFnName, eval(parsedFn, envir = new.env(parent = asNamespace("SpaDES.core"))),
         envir = envir)
  theEvalEnvir <- environment(get(eventFnName, envir = envir))
  rm(list = ls(theEvalEnvir), envir = theEvalEnvir)
  return(invisible(sim))
}

makeEventFn <- function(curModuleName, eventType) {
  paste("doEvent", curModuleName, eventType, sep = ".")
}

eventFnElement <- function() ".eventFnDigest"
eventFnElementEnvir <- function() ".eventFnEnvir"

moduleNameStripped <- function(modName, numCharsMax) {
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
  modName8Chars
}

debugMessTRUE <- function(sim, events) {
  if (missing(events))
    events <- current(sim)
  evnts1 <- data.frame(events)
  widths <- unname(unlist(lapply(format(evnts1), nchar)))
  sim[[".spadesDebugWidth"]] <- pmax(widths, sim[[".spadesDebugWidth"]])
  evnts1[1L, ] <- sprintf(paste0("%-", sim[[".spadesDebugWidth"]],"s"), evnts1)
  evnts1[1L, 1L] <- sprintf(paste0("%.4", "g"), as.numeric(evnts1[1L, 1L]))
  evnts1[1L, 1L] <- sprintf(paste0("%-", sim[[".spadesDebugWidth"]][1L],"s"), evnts1[1L, 1L])
  if (.pkgEnv[[".spadesDebugFirst"]]) {
    evnts2 <- evnts1
    evnts2[1L:2L, ] <- rbind(sprintf(paste0("%-",sim[[".spadesDebugWidth"]],"s"), names(evnts2)),
                             sprintf(paste0("%-",sim[[".spadesDebugWidth"]],"s"), evnts2))

    outMess <- paste(unname(evnts2[1, ]), collapse = ' ')
    outMess <- c(outMess, paste(unname(evnts2[2, ]), collapse = ' '))
    # write.table(evnts2, quote = FALSE, row.names = FALSE, col.names = FALSE)
    .pkgEnv[[".spadesDebugFirst"]] <- FALSE
  } else {
    colnames(evnts1) <- NULL
    # write.table(evnts1, quote = FALSE, row.names = FALSE)
    outMess <- paste(unname(evnts1), collapse = ' ')
  }
  outMess
}


runScheduleEventsOnly <- function(sim, fn, env, wh = c("switch", "scheduleEvent"), currnt) {

  if (missing(currnt)) {
    currnt <- unlist(current(sim))
    currnt[["eventTime"]] <- as.numeric(currnt[["eventTime"]])
  }

  if (missing(fn)) {
    fn <- parse(text = deparse(sim$.mods[[currnt[["moduleName"]]]][[paste0("doEvent.", currnt[["moduleName"]])]]))[[1]]
    env <- environment(sim$.mods[[currnt[["moduleName"]]]][[paste0("doEvent.", currnt[["moduleName"]])]])
  }
  num <- grep(wh[1], fn)
  if (wh[1] != "scheduleEvent") {
    if (num == 1) {
      num <- which(names(fn) %in% currnt[["eventType"]])
      wh <- wh[-1]
    }
    sim <- runScheduleEventsOnly(sim = sim, fn = fn[[num]], env = env, wh = wh, currnt = currnt)
  } else {
    env2 <- new.env(parent = env)
    env2$sim <- sim
    for (i in num) {
      sim <- eval(fn[[i]], env = env2)
    }
  }
  sim



}
