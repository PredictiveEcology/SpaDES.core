if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("memory", "maxMemory"))
}

ongoingMemoryThisPid <- function(seconds = 1000, interval = getOption("spades.memoryUseInterval", 0.5),
                                 thisPid, outputFile) {
  numTimes = 1
  if (missing(thisPid)) thisPid <- Sys.getpid()
  if (missing(outputFile))
    outputFile <- paste0("..memAvail", "_", thisPid, ".txt")
  #cat("memory,  time", "\n", file = outputFile, append = FALSE)
  suppressWarnings(file.remove(outputFile))
  #data.table::fwrite(list(a, Sys.time()), dateTimeAs = "ISO", append = FALSE, file = outputFile)
  #data.table::fwrite(list("mem", "time"), append = FALSE, file = outputFile)
  op <- options(digits.secs = 5)
  while(numTimes < (seconds/interval)) { # will go infinitely long!
    Sys.sleep(getOption("spades.memoryUseInterval", 0.5))
    a <- memoryUseThisSession(thisPid)
    #cat(a, ", ", format(Sys.time()), "\n", file = outputFile, append = TRUE)
    data.table::fwrite(list(memory = a, time = Sys.time()),
                       dateTimeAs = "write.csv",
                       append = file.exists(outputFile), file = outputFile)
    numTimes <- numTimes + 1
  }
  options(op)
  invisible(outputFile)
}

#' Estimate memory used with \code{system("ps")}
#'
#' This will give a slightly different estimate than \code{pryr::mem_used},
#' which uses \code{gc()} internally. The purpose of this function is
#' to allow continuous monitoring, external to the R session. Normally,
#' this is run in a different session.
#' @param thisPid Numeric or Integer, the PID of the process. If omitted, it will
#'   be found with \code{Sys.getpid()}
#' @export
#' @rdname memoryUse
memoryUseThisSession <- function (thisPid)
{
  ps <- Sys.which("ps")
  if (missing(thisPid)) thisPid <- Sys.getpid()
  if (nzchar(ps)) {
    #aa <- try(system(paste("ps -eo rss,pid --sort -rss | grep", thisPid), intern = TRUE), silent = TRUE)
    aa <- try(system(paste("ps -eo rss,pid | grep", thisPid), intern = TRUE), silent = TRUE)
    aa2 <- strsplit(aa, split = " +")[[1]][1]
  } else {
    aa <- try(system(paste0('tasklist /fi "pid eq ', thisPid, '"'), inter = TRUE), silent = TRUE)
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
    outputFile <- paste0("..memAvail", "_", thisPid, ".txt")
  message("Writing memory to ", outputFile)
  a <- future::future(
    getFromNamespace("ongoingMemoryThisPid", "SpaDES.core")(seconds = seconds,
                                       interval = interval,
                                       thisPid = thisPid,
                                       outputFile = outputFile),
                      globals = list(memoryUseThisSession = memoryUseThisSession,
                                     outputFile = outputFile, thisPid = thisPid,
                                     seconds = seconds, interval = interval))
}

# b <- futureOngoingMemoryThisPid()



#' Show memory use
#'
#' This will only work if the user has specified before running
#' the \code{spades} call, set the interval, in seconds, that ps is
#' run with \code{options("spades.memoryUseInterval" = 0.5)}, will assess
#' memory use every 0.5 seconds. The default
#' is 0, meaning no interval, "off".
#'
#' @export
#' @param sim A completed simList
#' @param max Logical. If TRUE, then it the return value will be summarized by
#'   module/event, showing the maximum memory used. If \code{FALSE}, then
#'   the raw memory used during each event will be shown.
#' @seealso The \code{vignette("iv-modules")}
memoryUse <- function(sim, max = TRUE) {
  compl <- completed(sim)
  mem <- sim@.xData$.memoryUse$obj
  if (is.null(mem)) {
    message("There are no data in the sim@.xData$.memoryUse$obj ... try running spades again?")
  } else {
    if (is.character(mem$time))
      mem[, time := as.POSIXct(time)]
    if (any(grepl("^time$", names(mem))))
      setnames(mem, old = "time", new = "clockTime")
    a <- mem[compl, on = c("clockTime"), roll = TRUE, allow.cartesian = TRUE]
    if (isTRUE(max)) {
      a <- a[, list(maxMemory = max(memory, na.rm = TRUE)), by = c("moduleName", "eventType")]
    } else {
      a <- a[, list(maxMemory = max(memory, na.rm = TRUE)), by = c("moduleName", "eventType", "eventTime")]
    }
    a[is.infinite(maxMemory), maxMemory:=NA]
    return(a[])
  }
}

isWindows <- function() identical(.Platform$OS.type, "windows")

memoryUseSetup <- function(sim, originalFuturePlan) {
  if (requireNamespace("future") && requireNamespace("future.callr")) {

    thePlan <- getOption("spades.futurePlan", NULL)
    # originalFuturePlan <- future::plan()
    if (!is(originalFuturePlan, "sequential") && !identical(thePlan, "sequential") &&
        !is.null(thePlan) && !is(originalFuturePlan, thePlan))
      stop("To use options('spades.memoryUseInterval'), you must set a future::plan(...) to something other than sequential")
    if (!is(originalFuturePlan, thePlan)) {
      if (grepl("callr", thePlan)) {
        future::plan(future.callr::callr)
      } else {
        future::plan(thePlan)
      }
    }

    # Set up element in simList for recording the memory use stuff
    sim@.xData$.memoryUse <- list()
    sim@.xData$.memoryUse$filename <- file.path(cachePath(sim), paste0("._memoryUseFilename", Sys.getpid(),".txt"))
    sim@.xData$.memoryUse$futureObj <-
      futureOngoingMemoryThisPid(seconds = Inf,
                                 interval = getOption("spades.memoryUseInterval", 0.2),
                                 outputFile = sim@.xData$.memoryUse$filename)
  } else {
    message("Can't use spades.memoryUseInterval without future (and optionally future.callr) packages")
  }

  return(sim)
}

memoryUseOnExit <- function(sim, originalFuturePlan) {
  future::plan("sequential") # kill all processes
  future::plan(originalFuturePlan) # reset to original

  if (file.exists(sim@.xData$.memoryUse$filename)) {
    sim@.xData$.memoryUse$obj <- data.table::fread(sim@.xData$.memoryUse$filename)
    file.remove(sim@.xData$.memoryUse$filename)
    message("Memory use saved in simList; see memoryUse(sim)")
  }
  return(sim)

}
