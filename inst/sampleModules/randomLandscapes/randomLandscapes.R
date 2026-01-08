SpaDES.core.version <- "2.0.0"
if (utils::packageVersion("SpaDES.core") < SpaDES.core.version) {
  stop("This 'randomLandscapes' module was built with 'SpaDES.core' version",
       SpaDES.core.version, ".\n",
       "Please update 'SpaDES.core' to use this module.")
}
rm(SpaDES.core.version)

## this version of the 'randomLandscapes' module loads data from file instead of
## generating it using 'SpaDES.tools::neutralLandscapeMap'

defineModule(sim, list(
  name = "randomLandscapes",
  description = paste("Generate RasterStack of random maps representative of a forest landscape",
                      "(DEM, forestAge, forestCover, habitatQuality, percentPine).",
                      "Requires a global simulation parameter `stackName` be set."),
  keywords = c("random map", "random landscape"),
  childModules = character(),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca",
           role = c("aut", "cre")),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca",
           role = c("aut", "cre"))
  ),
  version = list(randomLandscapes = "1.7.0"),
  spatialExtent = terra::ext(rep(0, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("ropensci/NLMR (>= 1.1.1)", "terra", "RColorBrewer", "reproducible (>= 2.0.2)",
                  "SpaDES.tools (>= 2.0.0)"),
  parameters = rbind(
    # defineParameter("inRAM", "logical", FALSE, TRUE, FALSE, "should the raster be stored in memory?"),
    defineParameter("nx", "numeric", 100L, 10L, 500L, "size of map (number of pixels) in the x dimension"),
    defineParameter("ny", "numeric", 100L, 10L, 500L, "size of map (number of pixels) in the y dimension"),
    defineParameter("stackName", "character", "landscape", NA, NA, "name of the RasterStack"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "A modular mechanism to create plots, using png, screen device or other. See ?Plots."),
    defineParameter(".plotInitialTime", "numeric", start(sim), start(sim), NA, "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time interval between save events"),
    defineParameter(".seed", "list", list(), NA, NA,
                    paste("Named list of seeds to use for each event (names).",
                          "E.g., `list('init' = 123)` will `set.seed(123)`",
                          "for the `init` event only.")),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should the module (or it's events) be cached for future use? Accepts logical",
                    "or a vector of events to cache.")
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "inRAM", "logical",
                 sourceURL = NA_character_, desc = "should the raster be stored in memory?")
  ),
  outputObjects = bindrows(
    createsOutput(objectName = SpaDES.core::P(sim, module = "randomLandscapes")$stackName,
                  objectClass = "SpatRaster",
                  desc = paste("NOTE: resulting stack may be slightly smaller than specified",
                               "because `nlm_mpd()` may drop pixels along raster edges."))
  )
))

## event types4347526b7f30225e6198f8802475b22285d885e8
doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      # schedule the next events
      sim <- scheduleEvent(sim, Par$.plotInitialTime, "randomLandscapes", "plot", .last())
      sim <- scheduleEvent(sim, Par$.saveInitialTime, "randomLandscapes", "save", .last() + 1)
    },
    plot = {
      # do stuff for this event
      Plots(sim[[Par$stackName]], usePlot = TRUE)
    },
    save = {
      # do stuff for this event
      sim <- saveFiles(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + Par$.saveInterval, "randomLandscapes",
                           "save", .last() + 1)
    },
    warning(paste(
      "Undefined event type: \'", events(sim)[1, "eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE], "\'", sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
Init <- function(sim) {
  if (is.null(sim$inRAM)) {
    inMemory <- FALSE
  } else {
    inMemory <- sim$inRAM
  }

  ## Give dimensions of dummy raster
  nx <- Par$nx
  ny <- Par$ny
  template <- rast(nrows = ny, ncols = nx, xmin = -nx / 2, xmax = nx / 2, ymin = -ny / 2, ymax = ny / 2)

  ## Make dummy maps for testing of models
  DEM <- SpaDES.tools::neutralLandscapeMap(template,
                                           roughness = 0.3,
                                           rand_dev = 10,
                                           rescale = TRUE,
                                           verbose = FALSE)

  DEM[] <- round(values(DEM), 1) * 300
  # terra::plot(DEM)

  forestAge <- SpaDES.tools::neutralLandscapeMap(template,
                                                 roughness = 0.7,
                                                 rand_dev = 10,
                                                 rescale = FALSE,
                                                 verbose = FALSE)

  forestAge[] <- round(values(forestAge), 1) * 10
  # terra::plot(forestAge)

  percentPine <- SpaDES.tools::neutralLandscapeMap(template,
                                                   roughness = 0.5,
                                                   rand_dev = 10,
                                                   rescale = TRUE,
                                                   verbose = FALSE)

  percentPine[] <- round(values(percentPine), 1)
  # terra::plot(percentPine)

  ## Scale them as needed
  forestAge <- forestAge + abs(minFn(forestAge))
  forestAge <- forestAge / maxFn(forestAge) * 100
  percentPine <- percentPine / maxFn(percentPine) * 100

  # Make layers that are derived from other layers
  habitatQuality <- (DEM + 10 + (forestAge + 2.5) * 10) / 100
  habitatQuality <- habitatQuality / maxFn(habitatQuality)

  # Stack them into a single stack and assign to global env
  mapStack <- c(DEM, forestAge, habitatQuality, percentPine)
  names(mapStack) <- c("DEM", "forestAge", "habitatQuality", "percentPine")

  cols <- list(DEM = brewer.pal(9, "YlOrBr"),
               forestAge = brewer.pal(9, "BuGn"),
               habitatQuality = brewer.pal(8, "Spectral"),
               percentPine = brewer.pal(9, "Greens"))
  for (i in seq(cols))
    coltab(mapStack, layer = i) <- cols[[i]]

  sim[[Par$stackName]] <- mapStack
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("inRAM", sim)) {
    sim$inRAM <- TRUE
  }
  sim

}
