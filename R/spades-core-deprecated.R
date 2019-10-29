gitSite <- c("PredictiveEcology/SpaDES.experiment@development",
             "PredictiveEcology/SpaDES.experiment@master")
deprecatedExpt <- paste0("experiment and experiment2 are no longer in SpaDES.core. ",
                         "Please use SpaDES.experiment::experiment or SpaDES.experiment::experiment2, ",
                         "which for now can be done with ",
                         "devtools::install_github('",gitSite,"')")

deprecatedMsg <- function(fnName, newPackage = "SpaDES.experiment", gitSite) {
  paste0(fnName, " has been moved to ", newPackage, ". ",
         "Please install with ",
         paste0("devtools::install_github('",gitSite, "')", collapse = " or "))
}
#' Deprecated functions
#'
#' These functions have been moved to \code{SpaDES.experiment} package.
#'
#' @export
#' @param ... Unused.
#' @rdname deprecated
experiment <- function(...) {
  .Deprecated(msg = deprecatedExpt)
}


#' @export
#' @rdname deprecated
experiment2 <- function(...) {
  .Deprecated(msg = deprecatedExpt)
}

#' @export
#' @rdname deprecated
POM <- function(...) {
  fnName <- "POM"
  .Deprecated(msg = deprecatedMsg(fnName, "SpaDES.experiment", gitSite))
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = paste0("simInitAndExperiment is no longer in SpaDES.core.",
                           "Please use SpaDES.experiment::simInitAndExperiment, ",
                           "which for now can be done with ",
                           "/ndevtools::install_github('",gitSite,"')"))
}
