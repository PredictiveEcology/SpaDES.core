.messageDeprecatedFn <- function(fnName = as.character(match.call()[[1]]),
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
  .Deprecated(msg = .messageDeprecatedFn(match.call()[[1]], "SpaDES.project"))
}


#' @export
#' @rdname deprecated
experiment2 <- function(...) {
  .Deprecated(msg = .messageDeprecatedFn(match.call()[[1]], "SpaDES.project"))
}

#' @export
#' @rdname deprecated
POM <- function(...) {
  .Deprecated(msg = .messageDeprecatedFn(match.call()[[1]], "SpaDES.experiment"))
}

#' @export
#' @rdname deprecated
simInitAndExperiment <- function(...) {
  .Deprecated(msg = .messageDeprecatedFn(match.call()[[1]], "SpaDES.project"))
}

#' @rdname deprecated
loadPackages <- function(...) {
  .Deprecated("Require", "Require")
}

## `.plotInitialTime` was removed as an argument to `spades()` (#231); `.plots`
## does the job instead. Warn when a caller still passes it, and ignore it.
## The *module parameter* of the same name is unaffected.
.warnPlotInitialTimeArg <- function(dotNames) {
  if (".plotInitialTime" %in% dotNames) {
    warning(
      "The `.plotInitialTime` argument to spades() is deprecated and is being ",
      "ignored. Use `.plots` instead: `.plots = NA` where you previously used ",
      "`.plotInitialTime = NA`. To set the `.plotInitialTime` module parameter, ",
      "pass it through `params`, e.g. ",
      "`params = list(<module> = list(.plotInitialTime = 0))`.",
      call. = FALSE
    )
  }
  invisible(NULL)
}
