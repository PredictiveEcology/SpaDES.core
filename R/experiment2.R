################################################################################
#' Run experiment, algorithm 2, using \code{\link{spades}}
#'
#' @inheritParams spades
#' @param replicates The number of replicates to run of the same \code{simList}.
#'                   See details and examples.
#'
#' @param clearSimEnv Logical. If TRUE, then the envir(sim) of each simList in the return list
#'                    is emptied. This is to reduce RAM load of large return object.
#'                    Default FALSE.
#'
#' @param ... Passed to \code{spades}. Specifically, \code{debug}, \code{.plotInitialTime},
#'            \code{.saveInitialTime}, \code{cache} and/or \code{notOlderThan}. Caching
#'            is still experimental. It is tested to work under some conditions, but not
#'            all. See details.
#'
#' @inheritParams POM
#'
#' @details
#' This function requires at least 1 complete simList.
#'
#' This function is parallel aware, using the same mechanism as used in the \code{raster}
#' package. Specifically, if you start a cluster using \code{\link{beginCluster}}, then
#' this experiment function will automatically use that cluster. It is always a good
#' idea to stop the cluster when finished, using \code{\link{endCluster}}.
#'
#'
#' Output directories are changed using this function: this is one of the dominant
#' side effects of this function. If there are only replications, then a set of
#' subdirectories will be created, one for each replicate.
#' If there are varying parameters and or modules, \code{outputPath} is updated
#' to include a subdirectory for each level of the experiment.
#' These are not nested, i.e., even if there are nested factors, all subdirectories
#' due to the experimental setup will be at the same level.
#' Replicates will be one level below this.
#' The subdirectory names will include the module(s), parameter names, the parameter values,
#' and input index number (i.e., which row of the inputs data.frame).
#' The default rule for naming is a concatenation of:
#'
#' 1. The experiment level (arbitrarily starting at 1). This is padded with zeros if there are
#' many experiment levels.
#'
#' 2. The module, parameter name and parameter experiment level (not the parameter value,
#' as values could be complex), for each parameter that is varying.
#'
#' 3. The module set.
#'
#' 4. The input index number
#'
#' 5. Individual identifiers are separated by a dash.
#'
#' 6. Module - Parameter - Parameter index triplets are separated by underscore.
#'
#' Replication is treated slightly differently. \code{outputPath} is always 1 level below the
#' experiment level for a replicate.
#' If the call to \code{experiment} is not a factorial experiment (i.e., it is just
#' replication), then the
#' default is to put the replicate subdirectories at the top level of \code{outputPath}.
#' To force this one level down, \code{dirPrefix} can be used or a manual change to
#' \code{outputPath} before the call to experiment.
#'
#' If \code{cache = TRUE} is passed, then this will pass this to \code{spades},
#' with the additional argument \code{replicate = x}, where x is the replicate number.
#' That means that if a user runs \code{experiment} with \code{replicate = 4} and
#' \code{cache = TRUE}, then SpaDES will run 4 replicates, caching the results,
#' including replicate = 1, replicate = 2, replicate = 3, and replicate = 4.
#' Thus, if a second call to experiment with the exact same simList is passed,
#' and \code{replicates = 6}, the first 4 will be taken from the cached copies,
#' and replicate 5 and 6 will be run (and cached) as normal.
#' If \code{notOlderThan} used with a time that is more recent than the cached copy,
#' then a new spades will be done, and the cached copy will be deleted from the
#' cache repository, so there will only ever be one copy of a particular replicate
#' for a particular simList.
#' NOTE: caching may not work as desired on a Windows machine because the sqlite
#' database can only be written to one at a time, so there may be collisions.
#'
#' @return Invisibly returns a \code{simLists} object.
#'
#' @seealso \code{\link{simInit}}, \code{\link{spades}}, \code{\link{experiment}}
#'
#' @author Eliot McIntire
#' @export
#' @importFrom parallel clusterApplyLB clusterEvalQ stopCluster
#' @importFrom raster getCluster returnCluster
#' @rdname experiment2
#'
setGeneric(
  "experiment2",
  signature = "...",
  function(..., replicates = 1, clearSimEnv = FALSE,
           createUniquePaths = c("outputPath")) {
    standardGeneric("experiment2")
  })

#' @rdname experiment
#' @importFrom future.apply future_lapply
setMethod(
  "experiment2",
  signature("simList"),
  definition = function(..., replicates, clearSimEnv,
                        createUniquePaths = c("outputPath")) {
    # determine packages to load in the workers
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

                           FUN = experiment2Inner,
                           future.packages = pkg
    ),
    envir = outSimLists@.xData)
    return(outSimLists)
  })

experiment2Inner <- function(X, ll, clearSimEnv, createUniquePaths,
                             simNames, names) {
  simName <- simNames[X]
  name <- names[X]
  outputPath(ll[[simName]]) <- checkPath(file.path(outputPath(ll[[simName]]), name),
                                   create = TRUE)
  s <- spades(Copy(ll[[simName]]))
  if (isTRUE(clearSimEnv))
    rm(ls(s), envir = envir(s))
  s
}

#' @param object  \code{simList}
#'
#' @author Alex Chubaty
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
    #objs <- ls(object)
    #
    #simListsBySimList <- split(objs, f = simLists)
    #simListsBySimList <- lapply(simListsBySimList, sort)

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
#' @examples
#' \dontrun {
#' endTime <- 5
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
#' # Save final state of landscape and caribou
#'   outputs = data.frame(objectName = c("landscape", "caribou"),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' mySim2 <- simInit(
#'   times = list(start = 0.0, end = endTime, timeunit = "year"),
#'   params = list(
#' .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
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
#'   outputs = data.frame(objectName = c("landscape", "caribou"),
#'                        stringsAsFactors = FALSE)
#' )
#'
#' # Run experiment
#' sims <- experiment2(mySim1, mySim2, replicates = c(5,5))
#'
#'
#'   df1 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned"))
#'
#'   measure.cols <- grep("nPixelsBurned", names(df1), value = TRUE)
#'   df1Short <- data.table::melt(df1, measure.vars = measure.cols, variable.name = "year")
#'   df1Short[, year := as.numeric(gsub(".*V([[:digit:]])", "\\1", df1Short$year))]
#'   p<- ggplot(df1Short, aes(x=year, y=value, group=simList, color=simList)) +
#'     stat_summary(geom = "point", fun.y = mean) +
#'     stat_summary(geom = "line", fun.y = mean) +
#' stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
#'
#'   print(p)
#'
#' # Simple, single variable output, quoted -- next two lines are identical
#' df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = quote(length(caribou$x1))))
#' df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = "length(caribou$x1)"))
#'
#'   p<- ggplot(df1, aes(x=simList, y=NCaribou.V1, group=simList, color=simList)) +
#'     stat_summary(geom = "point", fun.y = mean, position = "dodge") +
#'     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge")
#'   print(p)
#'
#' }
#' } # end /dontrun
#'
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
    dt <- data.table(sim1 = rownames(ll3[[1]]), as.data.table(ll3))
  } else {
    dt <- rbindlist(ll, idcol = "sim1")
  }
  dt[, `:=`(simList = gsub("_.*", "", sim1), reps = gsub(".*_", "", sim1))]
  varNameOnly <- gsub(".V[[:digit:]]+", "", names(dt))
  counts <- table(varNameOnly)
  whichSingleton <- which(counts == 1)
  out <- lapply(names(whichSingleton), dt = dt, function(n, dt) {
    setnames(dt, old = grep(n, names(dt), value = TRUE), new = n)
  })
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
