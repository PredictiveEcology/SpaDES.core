defineModule(sim, list(
  name = "SpaDES_sampleModules",
  description = "Sample SpaDES modules included with the 'SpaDES.core' package",
  keywords = c("demo, example"),
  authors = c(person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca",
                     role = c("aut", "cre"))),
  childModules = c("caribouMovement", "fireSpread", "randomLandscapes"),
  version = list(SpaDES_sampleModules = "2.0.0", fireSpread = "2.0.0", caribouMovement = "2.0.0",
                 randomLandscapes = "2.0.0"),
  spatialExtent = terra::ext(rep(0, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year"
  citation = list(),
  documentation = list("SpaDES_sampleModules.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plots", "character", NA, NA, NA,
                    "A modular mechanism to create plots, using png, screen device or other. See ?Plots."),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA,
                    "This describes the simulation time at which the first save event should occur")
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
