## URL access logging hookup for simInit / spades.
##
## SpaDES wires the simList's environment into `reproducible.urlLog` so any
## prepInputs() / preProcess() call inside simInit or spades is recorded
## against the sim. The records live at envir(sim)$.urlLog$records and use a
## leading dot so they don't surface in default ls() output.
##
## Each event dispatch updates envir(sim)$.urlLog$extra with the current
## module + event so reproducible tags every recorded URL access with the
## right context.

## Install the option for the duration of a simInit / spades call. Returns
## a sentinel the caller must hand back to .restoreUrlLog() on exit.
## NULL sentinel means nothing was installed (option already opted out, or
## a user has set their own sink, or no sim available).
.installUrlLog <- function(sim) {
  if (!isTRUE(getOption("spades.urlLog", TRUE))) return(NULL)
  e <- envir(sim)
  if (is.null(e$.urlLog)) {
    e$.urlLog <- new.env(parent = emptyenv())
    e$.urlLog$records <- list()
    e$.urlLog$seen    <- character()
  }
  prev <- getOption("reproducible.urlLog")
  options(reproducible.urlLog = e$.urlLog)
  list(prev = prev)
}

.restoreUrlLog <- function(token) {
  if (is.null(token)) return(invisible())
  options(reproducible.urlLog = token$prev)
  invisible()
}

## Set envir(sim)$.urlLog$extra to the current module + event so that the
## next URL access recorded via reproducible is tagged accordingly. No-op
## if logging is off or no .urlLog is installed.
.updateUrlLogExtra <- function(sim) {
  log <- envir(sim)$.urlLog
  if (!is.environment(log)) return(invisible())
  cur <- sim@current
  if (!length(cur)) return(invisible())
  log$extra <- list(
    module = if (!is.null(cur$moduleName)) as.character(cur$moduleName) else NA_character_,
    event  = if (!is.null(cur$eventType))  as.character(cur$eventType)  else NA_character_
  )
  invisible()
}
