defineModule(sim, list(
  name = "SpaDES_sampleModules",
  description = "Sample SpaDES modules included with the 'SpaDES.core' package",
  keywords = c("demo, example"),
  authors = c(person(c("Alex", "M."), "Chubaty", email = "alexander.chubaty@canada.ca",
                     role = c("aut", "cre"))),
  childModules = c("caribouMovement", "fireSpread", "randomLandscapes"),
  version = list(SpaDES.core = "0.1.0", SpaDES_sampleModules = "1.3.0",
                 randomLandscapes = "1.6.0", caribouMovement = "1.6.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year"
  citation = list(),
  documentation = list("SpaDES_sampleModules.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = NA_character_, objectClass = NA_character_,
                 desc = NA_character_, sourceURL = NA_character_, other = NA_character_)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = NA_character_, objectClass = NA_character_,
                  desc = NA_character_, other = NA_character_)
  )
))

### no other code is needed for this module group
