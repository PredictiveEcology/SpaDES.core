defineModule(sim, list(
  name = "trailingComma",
  description = "fixture: trailing comma at the end of the defineModule list",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(trailingComma = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(),
  inputObjects = bindrows(),
  outputObjects = bindrows(),
),)

doEvent.trailingComma <- function(sim, eventTime, eventType) return(invisible(sim))
