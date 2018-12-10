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
#' @inheritParams spades
#' @return Returns the modified \code{simList} object.
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
#'
#' @author Alex Chubaty
#' @export
#' @importFrom data.table data.table rbindlist setkey
#' @importFrom stringi stri_pad_right stri_pad stri_length
#' @importFrom reproducible Cache
#' @importFrom utils write.table
#' @include helpers.R
#' @keywords internal
#' @rdname doEvent
#'
doEvent <- function(sim, debug = FALSE, notOlderThan) {
  #if (missing(debug)) debug <- FALSE
  #if (!inherits(sim, "simList")) stop("sim must be a simList")
  #if (!is(sim, "simList")) stop("sim must be a simList")
  if (class(sim) != "simList") { # faster than `is` and `inherits`
    stop("doEvent can only accept a simList object")
  }

  # core modules
  core <- .pkgEnv$.coreModules

  if (length(sim@current) == 0) {
    # get next event from the queue and remove it from the queue
    if (length(sim@events)) {
      # Do same check as would be done with "slot(..., check = FALSE)", but much faster
      if (is.list(sim@events[[1]]))
        slot(sim, "current", check = FALSE) <- sim@events[[1]]
      if (is.list(sim@events[-1]))
        slot(sim, "events", check = FALSE) <- sim@events[-1]
    } else {
      # no more events, return empty event list
      slot(sim, "current", check = FALSE) <- list() # this is guaranteed to be a list
    }
  }

  # catches the situation where no future event is scheduled,
  #  but stop time is not reached
  cur <- sim@current
  if  (length(cur) == 0) {
    # Test replacement for speed
    #slot(sim, "simtimes")[["current"]] <- sim@simtimes[["end"]] + 1
    st <- slot(sim, "simtimes")
    st[["current"]] <- sim@simtimes[["end"]] + 1
    slot(sim, "simtimes", check = FALSE) <- st
  } else {

    # if the current time is greater than end time, then don't run it
    if (cur[["eventTime"]] <= sim@simtimes[["end"]]) {
      fnEnv <- sim@.xData[[cur[["moduleName"]]]]
      # update current simulated time
      # Test replacement for speed
      #slot(sim, "simtimes")[["current"]] <- cur[["eventTime"]]
      st <- slot(sim, "simtimes")
      st[["current"]] <- cur[["eventTime"]]
      slot(sim, "simtimes", check = FALSE) <- st

      # call the module responsible for processing this event
      moduleCall <- paste("doEvent", cur[["moduleName"]], sep = ".")

      # if debug is TRUE
      if (is.null(attr(sim, "needDebug"))) {
        attr(sim, "needDebug") <- if (length(debug) > 1) {
          !(all(unlist(lapply(debug, identical, FALSE))))
        } else {
          !identical(debug, FALSE)
        }
      }
      if (attr(sim, "needDebug")) {
        for (i in seq_along(debug)) {
          if (isTRUE(debug[[i]]) | debug[[i]] == "current" | debug[[i]] == "step") {
            if (length(cur) > 0) {
              if(debug[[i]] == "step") readline("Press any key to continue...")

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
          } else if (debug[[i]] == 1) {
            print(paste0(Sys.time(),
                         " | total elpsd: ", format(Sys.time() - sim@.xData$._startClockTime, digits = 2),
                         " | ", paste(unname(current(sim)), collapse = ' ')))
          } else if (debug[[i]] == 2) {
            compareTime <- if (is.null(attr(sim, "completedCounter")) || attr(sim, "completedCounter")==1) {
              sim@.xData$._startClockTime
            } else {
              .POSIXct(sim@completed[[attr(sim, "completedCounter")-1]]$._clockTime)
            }
            print(paste0(format(Sys.time(), format = "%H:%M:%S"),
                         " | elpsd: ", format(Sys.time() - compareTime, digits = 2),
                         " | ", paste(unname(current(sim)), collapse = ' ')))
          } else if (debug[[i]] == "simList") {
            print(sim)
          } else if (grepl(debug[[i]], pattern = "\\(")) {
            print(eval(parse(text = debug[[i]])))
          } else if (any(debug[[i]] %in% cur[c("moduleName", "eventType")])) {
            if(is.environment(fnEnv)) {
              if (all(debug %in% cur[c("moduleName", "eventType")])) {
                debugonce(get(paste0("doEvent.", cur[["moduleName"]]), envir = fnEnv))
                on.exit(get(paste0("doEvent.", cur[["moduleName"]]), envir = fnEnv))
              }
            }
          } else if (!any(debug[[i]] == c("browser"))) {

            tryCatch(print(do.call(debug[[i]], list(sim))), error = function(x) NULL)
          }
        }
      }

      # if the moduleName exists in the simList -- i.e,. go ahead with doEvent
      if (cur[["moduleName"]] %in% sim@modules) {
        if (cur[["moduleName"]] %in% core) {
          sim <- get(moduleCall)(sim, cur[["eventTime"]],
                                 cur[["eventType"]])
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
          if (!.pkgEnv[["skipNamespacing"]])
            .modifySearchPath(sim@depends@dependencies[[cur[["moduleName"]]]]@reqdPkgs,
                              removeOthers = FALSE)

          sim <- .runEvent(sim, cacheIt, debug,
                           moduleCall, fnEnv, cur, notOlderThan)

          if (!exists(cur[["moduleName"]], envir = sim, inherits = FALSE))
            stop("The module named ", cur[["moduleName"]], " just corrupted the object with that ",
                           "name from from the simList. ",
                           "Please remove the section of code that does this in the event named: ",
                           cur[["eventType"]])

          if (!is.environment(get(cur[["moduleName"]], envir = sim)))
            stop("The module named ", cur[["moduleName"]], " just corrupted the object with that ",
                 "name from from the simList. ",
                 "Please remove the section of code that does this in the event named: ",
                 cur[["eventType"]])

          if (!exists("mod", envir = sim[[cur[["moduleName"]]]], inherits = FALSE))
            stop("The module named ", cur[["moduleName"]], " just deleted the object named 'mod' from sim$",
                 cur[["moduleName"]],". ",
                 "Please remove the section of code that does this in the event named: ",
                 cur[["eventType"]])

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
          if(attr(sim, "completedCounter")==attr(sim, "completedSize")) {
            lenCompl <- length(sim@completed)
            if (lenCompl > .pkgEnv[["spades.nCompleted"]]) { # we are above desired
              if (attr(sim, "completedCounter") >= lenCompl ) { # We can now cull earlier ones
                keepFrom <- lenCompl - .pkgEnv[["spades.nCompleted"]]
                sim@completed <- sim@completed[keepFrom:lenCompl] # keep 10000 of them, from lenCompl - nCompleted to current
                attr(sim, "completedSize") <- length(sim@completed)
                attr(sim, "completedCounter") <- attr(sim, "completedSize")
              }
            }
            attr(sim, "completedSize") <- attr(sim, "completedSize") * 2
            length(sim@completed) <- attr(sim, "completedSize")
          }
          sim@completed[attr(sim, "completedCounter")] <- list(cur)

        } else {
          attr(sim, "completedCounter") <- 1
          attr(sim, "completedSize") <- 2
          sim@completed <- list(cur)
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
#'                       call the event.
#'
#' @param eventType      A character string specifying the type of event from
#'                       within the module.
#'
#' @param eventPriority  A numeric specifying the priority of the event.
#'                       Lower number means higher priority.
#'                       See \code{\link{priority}}.
#' @param .skipChecks Logical. If \code{TRUE}, then internal checks that arguments match
#'                    expected types are skipped. Should only be used if speed is critical.
#'
#' @return Returns the modified \code{simList} object.
#'
#' @importFrom data.table setkey
#' @include priority.R
#' @export
#' @rdname scheduleEvent
#' @seealso \code{\link{priority}}, \code{\link{scheduleConditionalEvent}}
#'
#' @author Alex Chubaty
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
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
                          eventPriority = .pkgEnv$.normalVal,
                          .skipChecks = FALSE) {
  #if (!inherits(sim, "simList")) stop("sim must be a simList")
  #if (!is(sim, "simList")) stop("sim must be a simList")

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
      eventTimeInSeconds <- calculateEventTimeInSeconds(sim, eventTime, moduleName)
      attr(eventTimeInSeconds, "unit") <- "second"

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
                   eventPriority >= sim@events[[numEvents]][[4]]){
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
#' @return Returns the modified \code{simList} object, i.e., \code{sim$._conditionalEvents}
#'
#' This conditional event queue will be assessed at every single event in the normal event
#' queue. If there are no conditional events, then \code{spades} will proceed as normal. As
#' conditional event conditions are found to be true, then it will trigger a call to
#' \code{scheduleEvent(...)} with the current time passed to \code{eventTime} \emph{and} it will
#' remove the conditional event from the conditional queue. If the user
#' would like the triggered conditional event to occur as the very next event, then
#' a possible strategy would be to set \code{eventPriority} of the conditional event to
#' very low or even negative to ensure it gets inserted at the top of the event queue.
#'
#' @importFrom data.table setkey
#' @include priority.R
#' @export
#' @rdname scheduleConditionalEvent
#' @seealso \code{\link{scheduleEvent}}, \code{\link{conditionalEvents}}
#'
#' @author Eliot McIntire
#'
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
#'             Retrieved from \url{https://www.nostarch.com/artofr.htm}
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
                 eventPriority >= sim$._conditionalEvents[[numEvents]]$eventPriority){
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
#' @seealso \code{\link{SpaDES.core-package}},
#' \code{\link{experiment}} for using replication with \code{spades},
#' \code{\link{simInit}}, and the caching vignette (very important for reproducibility):
#' \url{https://CRAN.R-project.org/package=SpaDES/vignettes/iii-cache.html} which
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
#' @section \code{debug}:
#'
#' \code{debug} can be a logical, character vector or a numeric scalar (currently
#' 1 or 2).
#' If \code{debug} is specified and is not \code{FALSE}, 2 things could happen:
#' 1) there can be messages sent to console, such as events as they pass by, and
#' 2) if \code{options("spades.browserOnError" = TRUE)} (experimental still) if
#' there is an error, it will attempt to open a browser
#' in the event where the error occurred. You can edit, and then press \code{c} to continue
#' or \code{Q} to quit, plus all other normal interactive browser tools.
#' \code{c} will trigger a reparse and events will continue as scheduled, starting
#' with the one just edited. There may be some unexpected consequences if the
#' \code{simList} objects had already been changed before the error occurred.
#'
#' If not specified in the function call, the package
#' option \code{spades.debug} is used. The following
#' options for debug are available:
#'
#' \tabular{ll}{
#'   \code{TRUE} \tab the event immediately following will be printed as it
#' runs (equivalent to \code{current(sim)}).\cr
#'   function name (as character string) \tab If a function, then it will be run on the
#'                                            simList, e.g., "time" will run
#'                                            \code{time(sim)} at each event.\cr
#'   moduleName (as character string) \tab All calls to that module will be entered
#'                                         interactively\cr
#'   eventName (as character string) \tab All calls that have that event name (in any module)
#'                                        will be entered interactively\cr
#'   \code{c(<moduleName>, <eventName>)}  \tab Only the event in that specified module
#'                                             will be entered into. \cr
#'   Any other R expression expressed as a character string  \tab
#'                                 Will be evaluated with access to the simList as 'sim'.
#'                                If this is more than one character string, then all will
#'                                be printed to the screen in their sequence. \cr
#'   A numeric scalar, currently 1 or 2 (maybe others) \tab This will print out alternative forms of event
#'                                           information that users may find useful \cr
#'                                           information that users may find useful \cr
#'
#' }
#'
#'
#'
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
#' @references Matloff, N. (2011). The Art of R Programming (ch. 7.8.3).
#'             San Francisco, CA: No Starch Press, Inc..
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

    # set the options("spades.xxxPath") to the values in the sim@paths
    oldGetPaths <- getPaths()
    do.call(setPaths, append(sim@paths, list(silent = TRUE)))
    on.exit({do.call(setPaths, append(list(silent = TRUE), oldGetPaths))}, add = TRUE)

    sim@.xData[["._startClockTime"]] <- Sys.time()
    .pkgEnv$searchPath <- search()
    .pkgEnv[["spades.browserOnError"]] <-
      (interactive() & !identical(debug, FALSE) & getOption("spades.browserOnError"))
    .pkgEnv[["spades.nCompleted"]] <- getOption("spades.nCompleted")
    .pkgEnv[["skipNamespacing"]] <- !getOption("spades.switchPkgNamespaces")
    .pkgEnv[["spades.keepCompleted"]] <- getOption("spades.keepCompleted", TRUE)

    # timeunits gets accessed every event -- this should only be needed once per simList
    sim@.xData$.timeunits <- timeunits(sim)
    on.exit({
      if (!.pkgEnv[["skipNamespacing"]])
        .modifySearchPath(.pkgEnv$searchPath, removeOthers = TRUE)
      rm(".timeunits", envir = sim@.xData)
    }, add = TRUE)

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

    sim@.xData[["._firstEventClockTime"]] <- Sys.time()

    while (sim@simtimes[["current"]] <= sim@simtimes[["end"]]) {
      sim <- doEvent(sim, debug = debug, notOlderThan = notOlderThan)  # process the next event

      # Conditional Scheduling -- adds only 900 nanoseconds per event, if none exist
      if (exists("._conditionalEvents", envir = sim, inherits = FALSE)) {
        condEventsToOmit <- integer()
        for(condNum in seq(sim$._conditionalEvents)) {
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

    oldGetPaths <- getPaths()
    do.call(setPaths, append(list(silent = TRUE), sim@paths))
    on.exit({do.call(setPaths, append(list(silent = TRUE), oldGetPaths))}, add = TRUE)

    dots <- list(...)
    omitArgs <- "notOlderThan"
    if (isTRUE("omitArgs" %in% names(dots))) {
      omitArgs <- c(dots$omitArgs, omitArgs)
      dots$omitArgs <- NULL
    }

    if (cache) {
      return(
        do.call(quote = TRUE, Cache,
                args = append(list(
                  spades,
                  sim = sim,
                  debug = debug,
                  progress = progress,
                  .plotInitialTime = .plotInitialTime,
                  .saveInitialTime = .saveInitialTime,
                  omitArgs = omitArgs, notOlderThan = notOlderThan),
                  #cacheRepo = sim@paths$cachePath),
                  dots))
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


.runEvent <- function(sim, cacheIt, debug,
                      moduleCall, fnEnv, cur, notOlderThan) {
  if (cacheIt) { # means that a module or event is to be cached
    createsOutputs <- sim@depends@dependencies[[cur[["moduleName"]]]]@outputObjects$objectName
    fns <- ls(fnEnv, all.names = TRUE)
    moduleSpecificObjects <-
      c(ls(sim@.xData, all.names = TRUE, pattern = cur[["moduleName"]]), # functions in the main .xData that are prefixed with moduleName
        fns, # functions in the namespaced location
        na.omit(createsOutputs)) # objects outputted by module
    #fnsWOhidden <- paste0(cur[["moduleName"]], ":",
    #                      grep("^\\._", fns, value = TRUE, invert = TRUE))
    moduleSpecificOutputObjects <- c(createsOutputs, cur[["moduleName"]])
    classOptions <- list(events = FALSE, current=FALSE, completed=FALSE, simtimes=FALSE,
                         params = sim@params[[cur[["moduleName"]]]],
                         modules = cur[["moduleName"]])
  }
  fnCallAsExpr <- if (cacheIt) { # means that a module or event is to be cached
    expression(Cache(FUN = get(moduleCall, envir = fnEnv),
                sim = sim,
                eventTime = cur[["eventTime"]], eventType = cur[["eventType"]],
                objects = moduleSpecificObjects,
                notOlderThan = notOlderThan,
                outputObjects = moduleSpecificOutputObjects,
                classOptions = classOptions,
                cacheRepo = sim@paths[["cachePath"]]))
  } else {
    # Faster just to pass the NULL and just call it directly inside .runEvent
    expression(get(moduleCall,
          envir = fnEnv)(sim, cur[["eventTime"]], cur[["eventType"]]))
  }

  if (.pkgEnv[["spades.browserOnError"]]) {
    .runEventWithBrowser(sim, fnCallAsExpr, moduleCall, fnEnv, cur)
  } else {
    #fnEnv[[moduleCall]](sim, cur[["eventTime"]], cur[["eventType"]])
    eval(fnCallAsExpr) # slower than more direct version just above
  }
}

.runEventWithBrowser <- function(sim, fnCallAsExpr, moduleCall, fnEnv, cur) {
  canContinue <- TRUE
  numTries <- 0
  while(canContinue) {
    out <- try(eval(fnCallAsExpr))
    if (isTRUE(is(out, "try-error"))) {
      numTries <- numTries + 1
      if (numTries > 1) {
        tmp <- .parseConditional(filename = sim@.xData[[cur$moduleName]]$._sourceFilename)
        eval(tmp[["parsedFile"]][!tmp[["defineModuleItem"]]],
             envir = sim@.xData[[cur[["moduleName"]]]])
        numTries <- 0
      } else {
        message("There was an error in the code in the ", moduleCall,
                ". Entering browser. You can correct it and press c to continue",
                " or Q to quit")
        debugonce(get(moduleCall, envir = fnEnv))
      }
    } else {
      canContinue <- FALSE
    }
  }
  sim <- out
}

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
