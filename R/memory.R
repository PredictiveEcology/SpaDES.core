utils::globalVariables(c("memory", "maxMemory"))

#' @importFrom reproducible tempfile2
ongoingMemoryThisPid <- function(seconds = 1000,
                                 interval = getOption("spades.memoryUseInterval", 0.5),
                                 thisPid,
                                 outputFile) {
  numTimes <- 1
  if (missing(thisPid)) thisPid <- Sys.getpid()
  if (missing(outputFile)) {
    outputFile <- outputFilename(thisPid)
  }
  suppressWarnings(file.remove(outputFile))
  if (interval > 0) {
    op <- options(digits.secs = 5)
    stopFilename <- stopFilename(outputFile)
    while (numTimes < (seconds/interval) && !file.exists(stopFilename)) { # will go infinitely long!
      Sys.sleep(getOption("spades.memoryUseInterval", 0.5))
      a <- memoryUseThisSession(thisPid)
      data.table::fwrite(list(memory = a, time = Sys.time()),
                         dateTimeAs = "write.csv",
                         append = file.exists(outputFile), file = outputFile)
      numTimes <- numTimes + 1
    }
    options(op)
  } else {
    message("interval is 0; no memoryUse activated")
  }
  invisible(outputFile)
}

#' Estimate memory used with `system("ps")`
#'
#' This will give a slightly different estimate than `pryr::mem_used`, which uses `gc()` internally.
#' The purpose of this function is to allow continuous monitoring, external to the R session.
#' Normally, this is run in a different session.
#'
#' @param thisPid Numeric or integer, the `PID` of the process.
#'                If omitted, it will be found with `Sys.getpid()`.
#'
#' @return estimated memory use in MiB
#'
#' @export
#' @rdname memoryUse
memoryUseThisSession <- function(thisPid) {
  ps <- Sys.which("ps")
  if (missing(thisPid)) thisPid <- Sys.getpid()
  needTasklist <- isWindows()
  if (nzchar(ps) && !needTasklist) {
    aa <- try(suppressWarnings(system(paste(ps, "-eo rss,pid | grep", thisPid), intern = TRUE)), silent = TRUE)
    needTasklist <- !is.null(attr(aa, "status"))
    if (!needTasklist)
      aa2 <- try(strsplit(aa, split = " +")[[1]][1], silent = TRUE)
  }

  if (!nzchar(ps) || needTasklist) {
    aa <- try(system(paste0('tasklist /fi "pid eq ', thisPid, '"'), intern = TRUE), silent = TRUE)
    aa2 <- strsplit(aa[grepl(thisPid, aa)], split = " +")[[1]][5] # pull out 5th item in character string
    aa2 <- gsub(",", "", aa2) # comes with human readable comma -- must remove
  }

  aa3 <- as.numeric(aa2) * 1024
  class(aa3) <- "object_size"
  return(aa3)
}

futureOngoingMemoryThisPid <- function(outputFile = NULL,
                                       seconds = Inf,
                                       interval = getOption("spades.memoryUseInterval", 0.5)) {
  thisPid <- Sys.getpid()
  if (is.null(outputFile))
    outputFile <- outputFilename(thisPid)
  message("Writing memory to ", outputFile)
  a <- future::future({
    getFromNamespace("ongoingMemoryThisPid", "SpaDES.core")(seconds = seconds,
                                                            interval = interval,
                                                            thisPid = thisPid,
                                                            outputFile = outputFile)
  }, globals = list(memoryUseThisSession = memoryUseThisSession,
                    outputFile = outputFile, thisPid = thisPid,
                    seconds = seconds, interval = interval)
  )
  return(a)
}

# b <- futureOngoingMemoryThisPid()



#' Show memory use
#'
#' This will only work if the user has specified before running the `spades` call,
#' set the interval, in seconds, that `ps` is run.
#' E.g., `options("spades.memoryUseInterval" = 0.5)`, will assess memory use every 0.5 seconds.
#' The default is `0`, meaning no interval, "off".
#'
#' @note The suggested `future` and `future.callr` packages must be available.
#'
#' @param sim A completed `simList`
#'
#' @param max Logical. If TRUE, then it the return value will be summarized by
#'   module/event, showing the maximum memory used. If `FALSE`, then
#'   the raw memory used during each event will be shown.
#'
#' @return `data.table` summarizing the estimated memory use (in MiB) for each event type,
#' for each module, during the simulation.
#'
#' @export
#' @seealso The `vignette("iv-modules")`
memoryUse <- function(sim, max = TRUE) {
  compl <- Copy(completed(sim))
  mem <- Copy(sim@.xData$.memoryUse$obj)
  if (is.null(mem)) {
    message("There are no data in the sim@.xData$.memoryUse$obj ... try running spades again?")
  } else {
    # make sure same tz
    if (any(grepl("^time$", names(mem))))
      setnames(mem, old = "time", new = "clockTime")
    mem[, clockTime := as.POSIXct(as.character(clockTime))] # In case these two objects are in different tz
    compl[, clockTime := as.POSIXct(as.character(clockTime))]
    a <- mem[compl, on = c("clockTime"), roll = TRUE, allow.cartesian = TRUE]
    if (isTRUE(max)) {
      a <- a[, list(maxMemory = max(memory, na.rm = TRUE)), by = c("moduleName", "eventType")]
    } else {
      a <- a[, list(maxMemory = max(memory, na.rm = TRUE)),
             by = c("moduleName", "eventType", "eventTime")]
    }
    a[is.infinite(maxMemory), maxMemory := NA]
    return(a[])
  }
}

isWindows <- function() identical(.Platform$OS.type, "windows")

memoryUseSetup <- function(sim, originalFuturePlan) {
  if (requireNamespace("future", quietly = TRUE) &&
      requireNamespace("future.callr", quietly = TRUE)) {

    thePlan <- getOption("spades.futurePlan", NULL)
    # originalFuturePlan <- future::plan()
    if (!is(originalFuturePlan, "sequential")) {
      theActualPlan <- originalFuturePlan
      message("getOption('spades.futurePlan') disagreed with future::plan(); ",
              "using future::plan() and setting options('spades.futurePlan' = future::plan())")
      options("spades.futurePlan" = theActualPlan)
    } else if (!identical(thePlan, "sequential") && is.null(thePlan)) {
      stop("To use options('spades.memoryUseInterval'), you must set a future::plan(...)",
           " to something other than sequential")
    }
    if (is.character(thePlan))
      if (!is(originalFuturePlan, thePlan)) {
        if (grepl("callr", thePlan)) {
          future::plan(future.callr::callr)
        } else {
          future::plan(thePlan)
        }
      }

    # Set up element in simList for recording the memory use stuff
    sim@.xData$.memoryUse <- list()

    st <- format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S")
    sim@.xData$.memoryUse$filename <-
      file.path(logPath(sim), paste0("_memoryUse_", st, "_", Sys.getpid(), ".csv"))

    checkPath(dirname(sim@.xData$.memoryUse$filename), create = TRUE)

    sim@.xData$.memoryUse$futureObj <-
      futureOngoingMemoryThisPid(seconds = Inf,
                                 interval = getOption("spades.memoryUseInterval", 0.2),
                                 outputFile = sim@.xData$.memoryUse$filename)
    initialFileDoesntExist <- !file.exists(sim@.xData$.memoryUse$filename)
    if (initialFileDoesntExist) {
      message("Pausing while memoryUse infrastructure is set up (should take <5 seconds)")
    }

    counterForMax <- 0
    maxCounterForMax <- 30
    while (!file.exists(sim@.xData$.memoryUse$filename) && counterForMax < maxCounterForMax) {
      Sys.sleep(0.5)
      counterForMax <- counterForMax + 1
      message("\b.")
    }
    if (counterForMax >= maxCounterForMax) {
      stop("memoryUse encountered an error; perhaps logPath(sim) not writable?")
    }

    if (initialFileDoesntExist)
      message("\bDone!")

  } else {
    stop(futureMessage)
  }

  return(sim)
}

#' @importFrom data.table fwrite
memoryUseOnExit <- function(sim, originalFuturePlan) {
  if (requireNamespace("future", quietly = TRUE)) {
    # May not have killed it ... kill it manually
    x <- data.frame(x = 1)

    # This is the trigger that causes the future to end ... every "interval", the function that is
    #   being run searches for this file
    data.table::fwrite(x = x, file = stopFilename(sim$.memoryUse$filename))
    while (!future::resolved(sim$.memoryUse$futureObj)) {
      Sys.sleep(0.1)
    }
    unlink(stopFilename(sim$.memoryUse$filename))
    future::plan("sequential") # kill all processes
    future::plan(originalFuturePlan) # reset to original

    if (file.exists(sim@.xData$.memoryUse$filename)) {
      sim@.xData$.memoryUse$obj <- data.table::fread(sim@.xData$.memoryUse$filename)
      unlink(sim@.xData$.memoryUse$filename, force = TRUE)
      sim@.xData$.memoryUse$futureObj <- NULL
      message("Memory use saved in simList; see memoryUse(sim); removing memoryUse txt file")
    }
  } else {
    stop(futureMessage)
  }
  return(sim)
}

stopFilename <- function(outputFile) {
  gsub("\\.csv", "done.csv", outputFile)
}

outputFilename <- function(thisPid) {
  reproducible::tempfile2("memoryUse", fileext = paste0("..memAvail", "_", thisPid, ".txt"))
}

futureMessage <- paste0(
  "To use 'spades.memoryUseInterval', packages 'future' and 'future.callr' must be installed:\n",
  "  install.packages(c('future', 'future.callr'))")

