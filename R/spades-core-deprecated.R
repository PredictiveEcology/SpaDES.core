gitSite1 <- c("PredictiveEcology/SpaDES.experiment@development",
             "PredictiveEcology/SpaDES.experiment@master")

deprecatedMsg <- function(fnName = as.character(match.call()[[1]]),
                          newPackage = "SpaDES.experiment",
                          gitSite = gitSite1) {
  paste0(fnName, " has been moved to ", newPackage, ". ",
         "Please install with ",
         paste0("devtools::install_github('",gitSite1, "')", collapse = " or "))
}

#' Deprecated functions
#'
#' These functions have been moved to \code{SpaDES.experiment} package.
#'
#' @export
#' @param ... Unused.
#' @rdname deprecated
experiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}


#' @export
#' @rdname deprecated
experiment2 <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}

#' @export
#' @rdname deprecated
POM <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]))
}
