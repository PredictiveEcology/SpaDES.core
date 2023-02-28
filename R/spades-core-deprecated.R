deprecatedMsg <- function(fnName = as.character(match.call()[[1]]),
                          newPackage = NULL) {

  gitSite <- c(paste0("PredictiveEcology/", newPackage, "@development"),
               paste0("PredictiveEcology/", newPackage))

  paste0(fnName, " has been moved to ", newPackage, ". ",
         "Please install with ",
         paste0("remotes::install_github('", gitSite, "')", collapse = " or "))
}

#' Deprecated functions
#'
#' These functions have been moved to other packages.
#'
#' @export
#' @param ... Unused.
#' @rdname deprecated
experiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]), "SpaDES.experiment")
}


#' @export
#' @rdname deprecated
experiment2 <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]), "SpaDES.experiment")
}

#' @export
#' @rdname deprecated
POM <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]), "SpaDES.experiment")
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = deprecatedMsg(match.call()[[1]]), "SpaDES.experiment")
}

#' @rdname deprecated
loadPackages <- function(...) {
  .Deprecated("Require", "Require")
}
