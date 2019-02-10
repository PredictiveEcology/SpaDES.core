## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(spades.moduleCodeChecks = FALSE)

## ----module-metadata, eval=FALSE, echo=TRUE------------------------------
#  ## sample module metadata for the default `randomLandscapes` module
#  ## NOTE: long lines have been truncated
#  defineModule(sim, list(
#    name = "randomLandscapes",
#    description = "Generate RasterStack of random maps representative of a forest landsc...",
#    keywords = c("random map", "random landscape"),
#    authors = c(person(c("Alex", "M"), "Chubaty",
#                       email = "alexander.chubaty@canada.ca",
#                       role = c("aut", "cre")),
#                person(c("Eliot", "J", "B"), "McIntire",
#                       email = "eliot.mcintire@canada.ca",
#                       role = c("aut", "cre"))),
#    version = numeric_version("0.2.0"),
#    spatialExtent = raster::extent(rep(NA_real_, 4)),
#    timeframe = as.POSIXlt(c(NA, NA)),
#    timeunit = NA_real_,
#    citation = list(),
#    reqdPkgs = list("raster", "RColorBrewer", "SpaDES.tools"),
#    parameters = rbind(
#      defineParameter("stackName", "character", "randomLandscape", NA, NA, "..."),
#      defineParameter("nx", "numeric", 100L, NA, NA, "size of map (number ..."),
#      defineParameter("ny", "numeric", 100L, NA, NA, "size of map (number ..."),
#      defineParameter("inRAM", "logical", FALSE, NA, NA, "should the raster ..."),
#      defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "time to ..."),
#      defineParameter(".plotInterval", "numeric", 1, NA, NA, "time interval ..."),
#      defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time ..."),
#      defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time ...")
#    ),
#    inputObjects = bind_rows(
#      expectsInput(objectName = NA_character_, objectClass = NA_character_,
#                   desc = NA_character_, sourceURL = NA_character_, other = NA_character_)
#    ),
#    outputObjects = bind_rows(
#      createsOutput(objectName = globals(sim)$stackName, objectClass = "RasterStack",
#                    desc = NA_character_, other = NA_character_)
#    )
#  ))

## ----passing-params, eval=FALSE, echo=TRUE-------------------------------
#  library(SpaDES.core)
#  
#  outputDir <- file.path(tempdir(), "simOutputs")
#  times <- list(start = 0.0, end = 20.0)
#  parameters <- list(
#    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#    .progress = list(NA),
#    randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
#    fireSpread = list(
#      nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
#      returnInterval = 10, startTime = 0,
#      .plotInitialTime = 0, .plotInterval = 10
#    ),
#    caribouMovement = list(
#      N = 100L, moveInterval = 1, torus = TRUE,
#      .plotInitialTime = 1, .plotInterval = 1
#    )
#  )
#  modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
#  objects <- list()
#  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#                outputPath = outputDir)
#  
#  mySim <- simInit(times = times, params = parameters, modules = modules,
#                   objects = objects, paths = paths)
#  
#  # Access parameters
#  params(mySim)               # shows all parameters
#  P(mySim)                    # same, but more concise
#  P(mySim, "caribouMovement") # only parameters in caribouMovement
#  P(mySim)$caribouMovement    # same
#  P(mySim)$caribouMovement$N  # Only one parameter
#  
#  # If used within the module source code, then module name can be omitted:
#  # This will return NULL here, but will return the actual value if used
#  # in a module
#  P(sim)$N  # Only one parameter

## ----event-types, echo=TRUE, eval=FALSE----------------------------------
#  ## sample event type definitions from the default `randomLandscapes` module
#  doEvent.randomLandscapes <- function(sim, eventTime, eventType, debug = FALSE) {
#    if (eventType == "init") {
#      # do stuff for this event
#      sim <- randomLandscapesInit(sim)
#  
#      # schedule the next events
#      sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.plotInitialTime,
#                           "randomLandscapes", "plot")
#      sim <- scheduleEvent(sim, params(sim)$randomLandscapes$.saveInitialTime,
#                           "randomLandscapes", "save")
#  
#    } else if (eventType=="plot") {
#      # do stuff for this event
#      Plot(sim[[globals(sim)$stackName]])
#  
#      # schedule the next event
#      sim <- scheduleEvent(sim, time(sim) +
#                             params(sim)$randomLandscapes$.plotInterval,
#                           "randomLandscapes", "plot")
#    } else if (eventType == "save") {
#      # do stuff for this event
#      saveFiles(sim)
#  
#      # schedule the next event
#      sim <- scheduleEvent(sim, time(sim) +
#                             params(sim)$randomLandscapes$.saveInterval,
#                           "randomLandscapes", "save")
#  
#    } else {
#      warning(paste("Undefined event type: \'",
#                    events(sim)[1, "eventType", with = FALSE],
#                    "\' in module \'",
#                    events(sim)[1, "moduleName", with = FALSE],
#                    "\'", sep = ""))
#    }
#    return(invisible(sim))
#  }

## ----event-functions, echo=TRUE, eval=FALSE------------------------------
#  ## sample event functions from the default `randomLandscapes` module
#  library(raster)
#  if (require(SpaDES.tools)) {
#  
#    randomLandscapesInit <- function(sim) {
#      if (is.null(params(sim)$randomLandscapes$inRAM)) {
#        inMemory <- FALSE
#      } else {
#        inMemory <- params(sim)$randomLandscapes$inRAM
#      }
#      # Give dimensions of dummy raster
#      nx <- params(sim)$randomLandscapes$nx
#      ny <- params(sim)$randomLandscapes$ny
#      r <- raster(nrows = ny, ncols = nx, xmn = -nx/2, xmx = nx/2,
#                  ymn = -ny/2, ymx = ny/2)
#      speedup <- max(1, nx/5e2)
#      # Make dummy maps for testing of models
#      DEM <- gaussMap(template, scale = 300, var = 0.03,
#                      speedup = speedup, inMemory = inMemory)
#      DEM[] <- round(getValues(DEM), 1) * 1000
#      forestAge <- gaussMap(template, scale = 10, var = 0.1,
#                            speedup = speedup, inMemory = inMemory)
#      forestAge[] <- round(getValues(forestAge), 1) * 20
#      percentPine <- gaussMap(template, scale = 50, var = 1,
#                              speedup = speedup, inMemory = inMemory)
#      percentPine[] <- round(getValues(percentPine), 1)
#  
#      # Scale them as needed
#      forestAge <- forestAge / maxValue(forestAge) * 100
#      percentPine <- percentPine / maxValue(percentPine) * 100
#  
#      # Make layers that are derived from other layers
#      habitatQuality <- (DEM + 10 + (forestAge + 2.5) * 10) / 100
#      habitatQuality <- habitatQuality / maxValue(habitatQuality)
#  
#      # Stack them into a single stack and assign to sim envir
#      mapStack <- stack(DEM, forestAge, habitatQuality, percentPine)
#      names(mapStack) <- c("DEM", "forestAge", "habitatQuality", "percentPine")
#      setColors(mapStack) <- list(DEM = brewer.pal(9, "YlOrBr"),
#                                  forestAge = brewer.pal(9, "BuGn"),
#                                  habitatQuality = brewer.pal(8, "Spectral"),
#                                  percentPine = brewer.pal(9, "Greens"))
#      sim[[globals(sim)$stackName]] <- mapStack
#      return(invisible(sim))
#    }
#  }

