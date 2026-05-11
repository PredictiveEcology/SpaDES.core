defineModule(sim, list(
  name = "clean",
  description = "fixture: clean module — no findings expected",
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
    defineParameter("alpha", "numeric", 1, NA, NA, "first param"),
    defineParameter("beta",  "numeric", 2, NA, NA, "second param")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "x", objectClass = "numeric", desc = "in", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "y", objectClass = "numeric", desc = "out")
  )
))

doEvent.clean <- function(sim, eventTime, eventType) {
  switch(eventType,
    init = {
      sim <- Init(sim)
    })
  return(invisible(sim))
}

Init <- function(sim) {
  v <- sim$x * Par$alpha + Par$beta
  sim$y <- v
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("x", sim)) sim$x <- 0
  return(invisible(sim))
}
