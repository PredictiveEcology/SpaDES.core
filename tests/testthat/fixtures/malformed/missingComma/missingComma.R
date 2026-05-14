defineModule(sim, list(
  name = "missingComma",
  description = "fixture: missing comma after the first defineParameter() row",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(missingComma = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, NA, NA, "first")
    defineParameter("beta",  "numeric", 2, NA, NA, "second")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows()
))

doEvent.missingComma <- function(sim, eventTime, eventType) return(invisible(sim))
