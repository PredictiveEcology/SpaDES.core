#' Write simulation event info to file
#'
#' Useful for debugging.
#'
#' @param sim     A `simList` object.
#' @param file    Character specifying the filename (default \file{"events.txt"}).
#' @param append  Logical indicating whether to append to the file (default `FALSE`).
#'
#' @return Nothing returned. Invoked for its side-effect of writing to file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible normPath
writeEventInfo <- function(sim, file = "events.txt", append = FALSE) {
  f <- normPath(file)
  curr <- current(sim)
  cat(paste(curr$moduleName, curr$eventType, curr$eventTime), ":\n",
      file = file, append = append)
}

#' Write RNG state info to file
#'
#' Useful for debugging and ensuring reproducibility.
#'
#' @param file    Character specifying the filename (default `"seed.txt"`).
#' @param append  Logical indicating whether to append to the file (default `FALSE`).
#'
#' @return Nothing returned. Invoked for its side-effect of writing to file.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom reproducible normPath
writeRNGInfo <- function(file = "seed.txt", append = FALSE) {
  fseed <- normPath(file)
  cat("\tStart of new RNG stream: ", file = fseed, append = append)
  ## NOTE: the first element of .Random.seed specifies the RNG type, so omit it
  cat(.Random.seed[2:11], file = fseed, sep = ", ", append = append)
  cat(".", file = fseed, sep = "\n", append = append)
}
