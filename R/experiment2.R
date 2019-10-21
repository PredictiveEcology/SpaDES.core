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
#' # @example inst/examples/example_experiment2.R
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
    pkg <- unique(unlist(lapply(list(...), packages)))
    outSimLists <- new("simLists")
    ll <- list(...)
    simNames <- as.character(seq_along(list(...)))
    names(ll) <- simNames

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
