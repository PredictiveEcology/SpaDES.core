#' The `SpaDES.core` package environment
#'
#' Environment used internally to store internal package objects and methods.
#'
#' @keywords internal
#' @rdname pkgEnv
#'
.pkgEnv <- new.env(parent = emptyenv())

#' The `SpaDES.core` variable to switch between quick and robust checking
#'
#' A variable that can be use by module developers and model users to switch between
#' a quick check of functions like downloadData, Cache.
#' The module developer must actually use this in their code.
#'
#' @rdname quickCheck
#' @export
#'
.quickCheck <- FALSE
