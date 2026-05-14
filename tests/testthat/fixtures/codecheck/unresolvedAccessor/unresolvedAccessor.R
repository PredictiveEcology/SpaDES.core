defineModule(sim, list(
  name = "unresolvedAccessor",
  description = "fixture: uses sim[[Par$x]] -- statically unresolvable",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(unresolvedAccessor = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("name", "character", "stack", NA, NA, "name of input")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows()
))

doEvent.unresolvedAccessor <- function(sim, eventTime, eventType) {
  sim <- Init(sim)
  return(invisible(sim))
}

Init <- function(sim) {
  z <- sim[[Par$name]]
  w <- get("dynamic", envir = envir(sim))
  sim
}
