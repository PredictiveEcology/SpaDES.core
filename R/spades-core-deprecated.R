gitSite <- "PredictiveEcology/SpaDES.experiment@master"
deprecatedExpt <- paste0("experiment and experiment2 are no longer in SpaDES.core. ",
                         "Please use SpaDES.experiment::experiment or SpaDES.experiment::experiment2, ",
                         "which for now can be done with ",
                         "/ndevtools::install_github('",gitSite,"')")

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
  .Deprecated(msg = paste0("POM is no longer in SpaDES.core.",
                           "Please use SpaDES.experiment::POM, ",
                           "which for now can be done with ",
                           "/ndevtools::install_github('",gitSite,"')"))
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = paste0("simInitAndExperiment is no longer in SpaDES.core.",
                           "Please use SpaDES.experiment::simInitAndExperiment, ",
                           "which for now can be done with ",
                           "/ndevtools::install_github('",gitSite,"')"))
}
