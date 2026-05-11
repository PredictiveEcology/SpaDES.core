defineModule(sim, list(
  name = "unquotedObjName",
  description = "fixture: bare symbol as expectsInput()'s objectName (should be a string)",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(unquotedObjName = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(),
  inputObjects = bindrows(
    expectsInput(objectName = myInput, objectClass = "numeric", desc = "should be \"myInput\"", sourceURL = NA)
  ),
  outputObjects = bindrows()
))

doEvent.unquotedObjName <- function(sim, eventTime, eventType) return(invisible(sim))
