################################################################################
#' Event priority
#'
#' Preset event priorities: 1 = first (highest); 5 = normal; 10 = last (lowest).
#'
#' @return A numeric.
#'
#' @author Alex Chubaty
#' @export
#' @name priority
#' @rdname priority
#'
.first <- function() {
  .highest()
}

#' @export
#' @rdname priority
.highest <- function() {
  return(1)
}

#' @export
#' @rdname priority
.last <- function() {
  .lowest()
}

#' @export
#' @rdname priority
.lowest <- function() {
  return(10)
}

#' @export
#' @include environment.R
#' @rdname priority
.normal <- function() {
  5
}

.pkgEnv$.normalVal <- .normal()
