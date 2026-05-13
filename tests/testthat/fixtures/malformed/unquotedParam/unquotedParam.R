defineModule(sim, list(
  name = "unquotedParam",
  description = "fixture: bare symbol as defineParameter() name (should be a string)",
  keywords = "test",
  childModules = character(),
  authors = person("Test"),
  version = list(unquotedParam = "0.0.1"),
  spatialExtent = NULL,
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(alpha, "numeric", 1, NA, NA, "should be quoted: \"alpha\"")
  ),
  inputObjects = bindrows(),
  outputObjects = bindrows()
))

doEvent.unquotedParam <- function(sim, eventTime, eventType) return(invisible(sim))
