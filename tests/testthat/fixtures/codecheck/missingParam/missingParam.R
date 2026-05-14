defineModule(sim, list(
  name = "missingParam",
  description = "fixture: declares 'gamma' but never uses it; uses 'delta' without declaring",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(missingParam = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("gamma", "numeric", 3, NA, NA, "declared, never used")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows(
    createsOutput(objectName = "y", objectClass = "numeric", desc = "out")
  )
))

doEvent.missingParam <- function(sim, eventTime, eventType) {
  sim <- Init(sim)
  return(invisible(sim))
}

Init <- function(sim) {
  sim$y <- Par$delta + P(sim)$epsilon
  return(invisible(sim))
}
