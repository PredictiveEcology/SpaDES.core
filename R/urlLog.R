## URL access logging hookup for simInit / spades.
##
## SpaDES wires the simList's environment into `reproducible.urlLog` so any
## prepInputs() / preProcess() call inside simInit or spades is recorded
## against the sim. The records live at envir(sim)$._urlLog$records. The name
## uses the leading-dot-underscore (`._`) convention for volatile SpaDES
## bookkeeping, so it is hidden from default ls() AND ignored by
## all.equal.simList() and other `._`-aware machinery.
##
## Each event dispatch updates envir(sim)$._urlLog$extra with the current
## module + event so reproducible tags every recorded URL access with the
## right context.

## Install the option for the duration of a simInit / spades call. Returns
## a sentinel the caller must hand back to .restoreUrlLog() on exit.
##
## spades.urlLog = FALSE is a hard off-switch: because reproducible's urlLog
## is on by default, we must set reproducible.urlLog = FALSE for the duration
## (otherwise reproducible would still log at the package level). Otherwise we
## point reproducible.urlLog at the sim's hidden ._urlLog env.
.installUrlLog <- function(sim) {
  prev <- getOption("reproducible.urlLog")
  if (!isTRUE(getOption("spades.urlLog", TRUE))) {
    options(reproducible.urlLog = FALSE)
    return(list(prev = prev))
  }
  e <- envir(sim)
  if (is.null(e$._urlLog)) {
    e$._urlLog <- new.env(parent = emptyenv())
    e$._urlLog$records <- list()
    e$._urlLog$seen    <- character()
  }
  options(reproducible.urlLog = e$._urlLog)
  list(prev = prev)
}

.restoreUrlLog <- function(token) {
  if (is.null(token)) return(invisible())
  options(reproducible.urlLog = token$prev)
  invisible()
}

## Set envir(sim)$._urlLog$extra to the current module + event so that the
## next URL access recorded via reproducible is tagged accordingly. No-op
## if logging is off or no ._urlLog is installed.
.updateUrlLogExtra <- function(sim) {
  log <- envir(sim)$._urlLog
  if (!is.environment(log)) return(invisible())
  cur <- sim@current
  if (!length(cur)) return(invisible())
  log$extra <- list(
    module = if (!is.null(cur$moduleName)) as.character(cur$moduleName) else NA_character_,
    event  = if (!is.null(cur$eventType))  as.character(cur$eventType)  else NA_character_
  )
  invisible()
}
