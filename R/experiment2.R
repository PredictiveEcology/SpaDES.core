################################################################################
#' Run experiment, algorithm 2, using \code{\link{spades}}
#'
#' Given one or more \code{simList} objects, run a series of \code{spades} calls
#' in a structured, organized way. Methods are available to deal with outputs,
#' such as \code{as.data.table.simList} which can pull out simple to complex
#' values from every resulting \code{simList} or object saved by \code{outputs}
#' in every \code{simList} run. This uses \code{future} internally, allowing
#' for various backends and parallelism.0
#'
#' @param ... One or more \code{simList} objects
#' @param replicates The number of replicates to run of the same \code{simList}.
#'                   See details and examples.
#'
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list
#'                    is emptied. This is to reduce RAM load of large return object.
#'                    Default FALSE.
#' @param createUniquePaths A character vector of the \code{paths} passed to \code{simInit},
#'   indicating which should create a new, unique path, as a sub-path to the original
#'   \code{paths} of \code{simInit}. Default, and currently only option, is \code{"outputPath"}
#'
#' @param useCache Logical. Passed to \code{spades}. This will be passed with the \code{simList}
#'   name and replicate number, allowing each replicate and each \code{simList} to be
#'   seen as a non-cached call to \code{spades}. This will, however, may prevent \code{spades}
#'   calls from running a second time during second call to the same
#'   \code{experiment2} function.
#'
#' @details
#'
#' This function, because of its class formalism, allows for methods to be used. For example,
#' \code{\link{as.data.table.simList}} allows user to pull out specific objects (in
#' the \code{simList} objects or on disk saved in \code{outputPath(sim)}).
#'
#' The \code{outputPath} is changed so that every simulation puts outputs in a
#' sub-directory
#' of the original \code{outputPath} of each \code{simList}.
#'
#' @return Invisibly returns a \code{simLists} object. This class
#' extends the \code{environment} class and
#' contains \code{simList} objects.
#'
#' @seealso \code{\link{as.data.table.simList}},
#'   \code{\link{simInit}}, \code{\link{spades}}, \code{\link{experiment}}
#'
#' @author Eliot McIntire
#' @export
#' @rdname experiment2
#'
setGeneric(
  "experiment2",
  signature = "...",
  function(..., replicates = 1, clearSimEnv = FALSE,
           createUniquePaths = c("outputPath"), useCache = FALSE) {
    standardGeneric("experiment2")
  })

#' @rdname experiment2
#' @importFrom future.apply future_lapply
setMethod(
  "experiment2",
  signature("simList"),
  definition = function(..., replicates, clearSimEnv,
                        createUniquePaths = c("outputPath"),
                        useCache = FALSE) {
    # determine packages to load in the workers
    if (any(createUniquePaths != "outputPath")) {
      message("createUniquePaths only accepts outputPath, currently",
              ". Setting it to 'outputPath'")
      createUniquePaths <- "outputPath"
    }
    pkg <- unique(unlist(lapply(list(...), packages)))
    outSimLists <- new("simLists")
    ll <- list(...)
    possSimNames <- as.character(seq_along(list(...)))

    ll <- updateNames(ll, possSimNames)
    simNames <- names(ll)

    # names(ll) <- simNames
    if (length(simNames) != length(replicates) && length(replicates) != 1) {
      stop("replicates argument must be length 1 or the same length as the number of simLists")
    }

    if (!missing(replicates)) {
      if (length(replicates) == 1) replicates <- rep(replicates, length(ll))

      simNames <- unlist(Map(x = simNames, each = replicates, rep))
      repNums <- unlist(lapply(replicates, seq_len))
      namsExpanded <- paste(simNames, paddedFloatToChar(repNums, padL = max(nchar(repNums))),
                     sep = "_rep")
      names(simNames) <- namsExpanded

    } else {
      namsExpanded <- simNames
    }

      # do copy of sim inside workers, so there is only 1 copy per worker,
      # rather than 1 copy per sim
    iters <- seq_along(namsExpanded)
    names(iters) <- namsExpanded
    list2env(future_lapply(X = iters, ll = ll, clearSimEnv = clearSimEnv,
                           createUniquePaths = createUniquePaths,
                           names = namsExpanded,
                           simNames = simNames,
                           useCache = useCache,
                           .spades = .spades,
                           FUN = experiment2Inner,
                           future.packages = pkg
    ),
    envir = outSimLists@.xData)
    return(outSimLists)
  })

experiment2Inner <- function(X, ll, clearSimEnv, createUniquePaths,
                             simNames, names, useCache = FALSE, ...) {
  simName <- simNames[X]
  name <- names[X]
  outputPath(ll[[simName]]) <- checkPath(file.path(outputPath(ll[[simName]]), name),
                                   create = TRUE)
  s <- Cache(.spades, ll[[simName]], useCache = useCache, simName, ...)
  if (isTRUE(clearSimEnv))
    rm(ls(s), envir = envir(s))
  s
}

.spades <- function(sim, ...) {
  s <- spades(Copy(sim), ...)
}

#' Show method for \code{simLists}
#' @param object  \code{simLists}
#'
#' @author Eliot McIntire
#' @export
setMethod(
  "show",
  signature = "simLists",
  definition = function(object) {
    out <- list()
    out[[1]] <- capture.output(
      cat(rep("=", getOption("width"), sep = ""), "\n", sep = "")
    )

    simListsBySimList <- .objNamesBySimList(object)
    simLists <- unlist(simListsBySimList)
    simLists <- gsub("_.*", "", simLists)

    lengths <- lapply(simListsBySimList, length)
    uniqueLengths <- unique(unlist(lengths))
    out2 <- paste(">> ",length(unique(simLists)),"simLists;")
    out3 <- if (length(uniqueLengths) == 1) {
      paste("with", uniqueLengths, "replicates each")
    } else if (isTRUE(uniqueLengths) == 1) {
      paste0("with only 1 replicate each")
    } else {
      paste("with", paste(uniqueLengths, collapse = ", "), "replicates respectively")
    }
    out[[2]] <- capture.output(cat(out2, out3))
    ll <- lapply(simListsBySimList, function(s) {
      paste0(s[1], ", ..., ", tail(s,1))
    })
    simListChStr <- paste0(names(ll), ": ", ll)
    simListEntries <- (seq_along(unique(simLists))-1)*2 + length(out) + 1
    out[simListEntries] <- lapply(simListChStr, function(x) x)
    out[simListEntries + 1] <- lapply(simListsBySimList, function(x) {
      paste("  ", capture.output(ls.str(object[[x[1]]])))
      })

    out[[length(out) + 1]] <- capture.output(cat("\n"))
    ### print result
    cat(unlist(out), fill = FALSE, sep = "\n")
  })


#' Coerce elements of a simLists object to a data.table
#'
#' This is particularly useful to build plots using the tidyverse,
#' e.g., \code{ggplot2}
#' @importFrom purrr transpose
#' @param vals A character vector or list of object names to extract from each
#'   simList, or a list of quoted expressions to calculate for each \code{simList},
#'   or a mix of character and quoted expressions.
#' @param byRep Should the data.table have a column labelled "rep", indicating replicate
#'   number/label. Currently, only \code{TRUE} is accepted.
#' @param objectsFromSim Character vector of objects to extract from the simLists. If
#'   omitted, it will extract all objects from each simList in order to calculate the
#'   \code{vals}. This may have a computational cost.
#' @param objectsFromOutputs Character vector of objects to load from the
#'   \code{outputs(sim)} prior to evaluating \code{vals}. If there already is an object
#'   with that same name in the \code{simList}, then it will be overwritten with
#'   the object loaded from \code{outputs(sim)}. If there are many objects with the
#'   same name, specifically from several \code{saveTime} values in the \code{outputs(sim)},
#'   these will all be loaded, one at a time, \code{vals} evaluated one at a time, and
#'   all the unique values will be returned. A column, \code{saveTime}, will be
#'   part of the returned value.
#' @param ... Currently unused.
#' @details
#' See examples.
#'
#' @examples
#' \dontrun {
#' # Make 3 simLists -- set up scenarios
#' endTime <- 5
#' tmpdir <- file.path(tempdir(), "testing")
#' tmpCache <- file.path(tempdir(), "testingCache")
#' # Example of changing parameter values
#' mySim1 <- simInit(
#'   times = list(start = 0.0, end = endTime, timeunit = "year"),
#'   params = list(
#'     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'     # Turn off interactive plotting
#'     fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(10)),
#'     caribouMovement = list(.plotInitialTime = NA),
#'     randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
#'   ),
#'   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'                outputPath = tmpdir,
#'                cachePath = tmpCache),
#'   # Save final state of landscape and caribou
#'   outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
#'                        saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' mySim2 <- simInit(
#'   times = list(start = 0.0, end = endTime, timeunit = "year"),
#'   params = list(
#'     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'     # Turn off interactive plotting
#'     fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(20)),
#'     caribouMovement = list(.plotInitialTime = NA),
#'     randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
#'   ),
#'   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'                outputPath = tmpdir,
#'                cachePath = tmpCache),
#'   # Save final state of landscape and caribou
#'   outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
#'                        saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' mySim3 <- simInit(
#'   times = list(start = 0.0, end = endTime, timeunit = "year"),
#'   params = list(
#'     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#'     # Turn off interactive plotting
#'     fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(30)),
#'     caribouMovement = list(.plotInitialTime = NA),
#'     randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
#'   ),
#'   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
#'   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#'                outputPath = tmpdir,
#'                cachePath = tmpCache),
#'   # Save final state of landscape and caribou
#'   outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
#'                        saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' # Run experiment
#' sims <- experiment2(sim1 = mySim1, sim2 = mySim2, sim3 = mySim3,
#'       replicates = 3, useCache = FALSE)
#'
#' # Convert to data.table so can do stuff with
#' # Just pull out a variable from the simLists -- simplest case
#' df1 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned"))
#'
#' measure.cols <- grep("nPixelsBurned", names(df1), value = TRUE)
#' df1Short <- data.table::melt(df1, measure.vars = measure.cols, variable.name = "year")
#' df1Short[, year := as.numeric(gsub(".*V([[:digit:]])", "\\1", df1Short$year))]
#' library(ggplot2)
#' p<- ggplot(df1Short, aes(x=year, y=value, group=simList, color=simList)) +
#'   stat_summary(geom = "point", fun.y = mean) +
#'   stat_summary(geom = "line", fun.y = mean) +
#'   stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
#'
#' print(p)
#'
#' # A quoted function -- do not prefix objects with 'sim' -- next two lines are identical
#' df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = quote(length(caribou$x1))))
#' df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = "length(caribou$x1)"))
#'
#'   p<- ggplot(df1, aes(x=simList, y=NCaribou, group=simList, color=simList)) +
#'     stat_summary(geom = "point", fun.y = mean) +
#'     stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
#'   print(p)
#'
#' # A much more complicated object to calculate -- an estimate of perimeter to area ratio of fires
#' library(raster)
#' perimToAreaRatioFn <- quote({
#'   landscape$Fires[landscape$Fires[]==0] <- NA;
#'   a <- boundaries(landscape$Fires, type = "inner");
#'   a[landscape$Fires[] > 0 & a[] == 1] <- landscape$Fires[landscape$Fires[] > 0 & a[] == 1];
#'   peri <- table(a[]);
#'   area <- table(landscape$Fires[]);
#'   keep <- match(names(area),names(peri));
#'   mean(peri[keep]/area)
#' })
#'
#' df1 <- as.data.table(sims, byRep = TRUE,
#'                      vals = c(perimToArea = perimToAreaRatioFn,
#'                               meanFireSize = quote(mean(table(landscape$Fires[])[-1]))),
#'                      objectsFromOutputs = c("landscape")) # need to get landscape obj from disk
#' if (interactive()) {
#'   # with an unevaluated string
#'   p <- ggplot(df1, aes(x=saveTime, y=perimToArea, group=simList, color=simList)) +
#'     stat_summary(geom = "point", fun.y = mean) +
#'     stat_summary(geom = "line", fun.y = mean) +
#'     stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
#'   print(p) # may have NAs, giving warning, if perimeter calculation couldn't be completed
#' }
#'
#'
#' } # end /dontrun
#' @exportMethod as.data.table
as.data.table.simLists <- function(x, byRep = TRUE, vals,
                                   objectsFromSim = NULL,
                                   objectsFromOutputs = NULL,  ...) {
  if (!isTRUE(byRep)) stop("byRep must be TRUE, currently")
  objs <- ls(x)
  names(objs) <- objs
  simLists <- gsub("_.*", "", objs)
  if (!is.list(vals)) {
    vals <- if (is.character(vals)) {
      as.list(vals)
    } else {
      list(vals)
    }
  }
  vals <- updateNames(vals)
  # namesVals <- names(vals)
  # emptyChar <- nchar(namesVals) == 0
  # if (is.null(namesVals) || any(emptyChar)) {
  #   valNames <- unlist(lapply(vals, function(x) format(x)))
  #   if (any(emptyChar)) {
  #     namesVals[emptyChar] <- valNames[emptyChar]
  #     valNames <- namesVals
  #   }
  #   names(vals) <- valNames
  # }

  # Evaluate the expression
  reps <- gsub(".*_", "", objs)
  ll <- lapply(objs, vals = vals, function(sName, vals) {
    n <- new.env(parent = .GlobalEnv)
    if (!is.null(objectsFromOutputs)) {
      outpts <- setDT(outputs(x[[sName]]))[objectsFromOutputs == objectName]
      Times <- outpts$saveTime
    } else {
      Times <- end(x[[sName]])
    }
    names(Times) <- as.character(Times)

    out <- lapply(Times, function(t) {
      if (!is.null(objectsFromOutputs)) {
        lapply(objectsFromOutputs, function(ob) {
          theLine <- outpts[objectsFromOutputs == objectName & saveTime == t, ]
          theFile <- theLine[, file]
          ext <- file_ext(theFile)
          dt1 <- data.table(exts = ext)
          fun <- setDT(.fileExtensions())[dt1, on = "exts"]$fun
          tmpObj <- get(fun)(theFile)
          assign(theLine$objectName, tmpObj, envir = x[[sName]])
        })
      }
      # get ALL objects from simList -- could be slow -- may need to limit
      if (is.null(objectsFromSim)) {
        objectsFromSim <- ls(x[[sName]])
      }
      list2env(mget(objectsFromSim, envir = envir(x[[sName]])), envir = n)
      lapply(vals, n = n, function(o, n) {
        if (is.call(o)) {
          eval(o, envir = n)
        } else {
          eval(parse(text = o), envir = n)
        }
      })
    })
    if (length(Times) == 1) {
      out <- out[[1]]
    } else {
      ll2 <- purrr::transpose(out)
      labels <- seq_along(ll2)
      names(labels) <- names(ll2)
      ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
      dt <- as.data.table(ll3)
      out <- data.table(saveTime = Times, dt)
    }
    out
  })

  if (!all(unlist(lapply(ll, is.data.table)))) {
    ll2 <- purrr::transpose(ll)
    labels <- seq_along(ll2)
    names(labels) <- names(ll2)
    ll3 <- lapply(labels, ll2 = ll2, function(n, ll2)  t(rbindlist(ll2[n])))
    dt <- data.table(simName = rownames(ll3[[1]]), as.data.table(ll3))
  } else {
    dt <- rbindlist(ll, use.names = TRUE, idcol = "simName", fill = TRUE)
  }
  dt[, `:=`(simList = gsub("_.*", "", simName), reps = gsub(".*_", "", simName))]
  varNameOnly <- gsub(".V[[:digit:]]+", "", names(dt))
  counts <- table(varNameOnly)
  whichSingleton <- which(counts == 1)

  # out <- lapply(names(whichSingleton), dt = dt, function(n, dt) {
  #   setnames(dt, old = grep(n, names(dt), value = TRUE), new = n)
  # })
  # dt <- data.table(simList = simLists, reps = reps, dt)
  dt[]

}
.objNamesBySimList <- function(simLists) {
  objs <- ls(simLists)
  simLists <- gsub("_.*", "", objs)
  simListsBySimList <- split(objs, f = simLists)
  simListsBySimList <- lapply(simListsBySimList, sort)

}

updateNames <- function(lst, newNames) {
  namesVals <- names(lst)
  emptyChar <- nchar(namesVals) == 0
  if (is.null(namesVals) || any(emptyChar)) {
    if (missing(newNames))
      newNames <- unlist(lapply(lst, function(x) format(x)))
    if (any(emptyChar)) {
      namesVals[emptyChar] <- newNames[emptyChar]
      newNames <- namesVals
    }
    names(lst) <- newNames
  }
  lst
}
