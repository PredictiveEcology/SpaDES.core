SpaDES.core.version <- "2.0.0"
if (utils::packageVersion("SpaDES.core") < SpaDES.core.version) {
  stop("This 'fireSpread' module was built with 'SpaDES.core' version",
       SpaDES.core.version, ".\n",
       "Please update 'SpaDES.core' to use this module.")
}
rm(SpaDES.core.version)

## this version of the module

defineModule(sim, list(
  name = "fireSpread",
  description = paste("Simulate fire ignition and spread on a landscape, where",
                      "spread probability varies according to percent pine.",
                      "Fire size statistics are collected immediately after each burn event.",
                      "Requires a global simulation parameter `stackName` be set."),
  keywords = c("fire", "percolation model", "spread algorithm"),
  childModules = character(),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))
  ),
  version = list(fireSpread = "2.0.0"),
  spatialExtent = terra::ext(rep(0, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("methods", "RColorBrewer", "SpaDES.tools (>= 2.0.0)", "terra"),
  parameters = rbind(
    defineParameter("stackName", "character", "landscape", NA, NA, "name of the RasterStack"),
    defineParameter("burnStats", "character", "nPixelsBurned", NA, NA, "name of the burn statistics reported"),
    defineParameter("nFires", "numeric", 10L, 1L, 100L, "number of fires to initiate"),
    defineParameter("its", "numeric", 1e6, 1e6, 1e6, "number of iterations for fire spread"),
    defineParameter("persistprob", "numeric", 0.00, 0, 1, "probability of fire persisting in a pixel"),
    defineParameter("returnInterval", "numeric", 1.0, 1.0, 1.0, "fire return interval"),
    defineParameter("spreadprob", "numeric", 0.225, 0.05, 0.5, "probability of fire spreading into a pixel"),
    defineParameter("startTime", "numeric", start(sim) + 1, 0, end(sim), "time of initial fire ignition"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "A modular mechanism to create plots, using png, screen device or other. See ?Plots."),
    defineParameter(".plotInitialTime", "numeric", start(sim), start(sim), end(sim) + 1,
                    "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", 1, 1, 1, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, "time interval between save events"),
    defineParameter(".seed", "list", list(), NA, NA,
                    paste("Named list of seeds to use for each event (names).",
                          "E.g., `list('init' = 123)` will `set.seed(123)`",
                          "for the `init` event only."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = SpaDES.core::P(sim, module = "fireSpread")$stackName,
                 objectClass = "SpatRaster", desc = NA_character_, sourceURL = NA_character_),
    expectsInput(objectName = P(sim)$burnStats, objectClass = "numeric",
                 desc = NA_character_, sourceURL = NA_character_)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = P(sim, module = "fireSpread")$stackName,
                  objectClass = "SpatRaster", desc = NA_character_, other = NA_character_),
    createsOutput(objectName = P(sim)$burnStats, objectClass = "numeric",
                  desc = NA_character_, other = NA_character_)
  )
))

## event types
doEvent.fireSpread <- function(sim, eventTime, eventType, debug = FALSE) {

  switch(
    eventType,
    init = {
      ### check for more object dependencies:
      ### (use `checkObject` or similar)
      SpaDES.core::checkObject(sim, Par$stackName, layer = "habitatQuality")

      if (is.null(sim[[P(sim)$burnStats]])) {
        sim[[P(sim)$burnStats]] <- numeric()
      } else {
        npix <- sim[[(P(sim)$burnStats)]]
        stopifnot("numeric" %in% is(npix), "vector" %in% is(npix))
      }

      # do stuff for this event
      sim <- Init(sim)

      # schedule the next events
      sim <- scheduleEvent(sim, Par$startTime, "fireSpread", "burn")
      sim <- scheduleEvent(sim, Par$.saveInterval, "fireSpread", "save", .last())
      sim <- scheduleEvent(sim, Par$.plotInitialTime, "fireSpread", "plot.init", .last())
    },
    burn = {
      # do stuff for this event
      sim <- Burn(sim)

      # schedule the next events
      sim <- scheduleEvent(sim, time(sim), "fireSpread", "stats") # do stats immediately following burn
      sim <- scheduleEvent(sim, time(sim) + Par$returnInterval, "fireSpread", "burn")
    },
    stats = {
      # do stuff for this event
      sim <- Stats(sim)

      # schedule the next event
      ## stats scheduling done by burn event
    },
    plot.init = {
      # do stuff for this event
      cols <- list(
        DEM = brewer.pal(9, "YlOrBr"),
        forestAge = brewer.pal(9, "BuGn"),
        habitatQuality = brewer.pal(8, "Spectral"),
        percentPine = brewer.pal(9, "Greens"),
        Fires = c("white", rev(heat.colors(9)))
      )

      for (i in seq(cols))
        coltab(sim[[Par$stackName]], layer = i) <- cols[[i]]

      Plots(sim[[Par$stackName]],
           legendRange = list(0:maxFn(sim[[Par$stackName]]$DEM), 0:100,
                              c(0, 1), 0:100, 0:10), new = TRUE, usePlot = FALSE)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + Par$.plotInterval,
                           "fireSpread", "plot", .last())
    },
    plot = {
      # do stuff for this event
      Plots(sim[[Par$stackName]]$Fires, new = FALSE, usePlot = FALSE)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + Par$.plotInterval,
                           "fireSpread", "plot", .last())
    },
    save = {
      # do stuff for this event
      sim <- saveFiles(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + Par$.saveInterval,
                           "fireSpread", "save", .last() + 1)
    },
    warning(paste(
      "Undefined event type: \'", events(sim)[1, "eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions
Init <- function(sim) {
  landscapes <- sim[[Par$stackName]]

  ### create burn map that tracks fire locations over time
  Fires <- rast(ext(landscapes), ncol = ncol(landscapes), nrow = nrow(landscapes), vals = 0)
  names(Fires) <- "Fires"
  Fires <- setValues(Fires, 0)
  coltab(Fires) <- c("white", rev(heat.colors(9)))

  # add Fires map to global$stackName stack
  landscapes$Fires <- Fires
  sim[[Par$stackName]] <- landscapes

  return(invisible(sim))
}

Burn <- function(sim) {
  landscapes <- sim[[Par$stackName]]

  Fires <- spread(landscapes[[1]],
                  loci = as.integer(sample(1:ncell(landscapes), Par$nFires)),
                  spreadProb = Par$spreadprob,
                  persistance = Par$persistprob,
                  mask = NULL,
                  maxSize = 1e8,
                  directions = 8,
                  iterations = Par$its,
                  plot.it = FALSE,
                  id = TRUE)
  names(Fires) <- "Fires"
  coltab(Fires) <- c("white", rev(heat.colors(9)))
  landscapes$Fires <- Fires

  sim[[Par$stackName]] <- landscapes

  return(invisible(sim))
}

Stats <- function(sim) {
  npix <- sim[[P(sim)$burnStats]]

  landscapes <- sim[[Par$stackName]]

  sim[[P(sim)$burnStats]] <- c(npix, length(which(values(landscapes$Fires) > 0)))

  return(invisible(sim))
}
