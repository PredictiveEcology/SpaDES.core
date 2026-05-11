defineModule(sim, list(
  name = "clean",
  description = "fixture: well-formed metadata; no malformed-check findings expected",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(clean = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, NA, NA, "first"),
    defineParameter("beta",  "numeric", 2, NA, NA, "second")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "x", objectClass = "numeric", desc = "in", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "y", objectClass = "numeric", desc = "out")
  )
))

doEvent.clean <- function(sim, eventTime, eventType) {
  return(invisible(sim))
}
