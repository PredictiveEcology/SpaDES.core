defineModule(sim, list(
  name = "usedUndeclared",
  description = "fixture: writes sim$out2 without declaring it; reads sim$in2 without declaring",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(usedUndeclared = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(),
  inputObjects = bindrows(
    expectsInput(objectName = "in1", objectClass = "numeric", desc = "x", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "out1", objectClass = "numeric", desc = "y")
  )
))

doEvent.usedUndeclared <- function(sim, eventTime, eventType) {
  sim <- Init(sim)
  return(invisible(sim))
}

Init <- function(sim) {
  sim$out1 <- sim$in1 + 1
  sim$out2 <- sim$in2 + 1
  return(invisible(sim))
}
