SpaDES.core.version <- "1.0.7"
if (utils::packageVersion("SpaDES.core") < SpaDES.core.version) {
  stop("This 'caribouMovement' module was built with 'SpaDES.core' version",
       SpaDES.core.version, ".\n",
       "Please update 'SpaDES.core' to use this module.")
}
rm(SpaDES.core.version)

## module metadata
defineModule(sim, list(
  name = "caribouMovement",
  description = "Simulate caribou movement via correlated random walk.",
  keywords = c("caribou", "individual based movement model", "correlated random walk"),
  childModules = character(),
  authors = c(person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca",
                     role = c("aut", "cre"))),
  version = list(caribouMovement = "1.6.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "month",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("grid", "raster", "sp", "stats", "SpaDES.tools"),
  parameters = rbind(
    defineParameter("stackName", "character", "landscape", NA, NA, "name of the RasterStack"),
    defineParameter("moveInitialTime", "numeric", start(sim) + 1, start(sim) + 1, end(sim),
                    "time to schedule first movement event"),
    defineParameter("moveInterval", "numeric", 1.0, 1, 1,
                    "time interval between movoment events"),
    defineParameter("N", "numeric", 100L, 10L, 1000L, "initial number of caribou"),
    defineParameter("torus", "logical", FALSE, FALSE, TRUE,
                    "should the map wrap around like a torus?"),
    defineParameter(".plotInitialTime", "numeric", start(sim), -Inf, Inf,
                    "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", 1, -Inf, Inf,
                    "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, -Inf, Inf,
                    "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", NA_real_, -Inf, Inf,
                    "time interval between save events"),
    defineParameter(".seed", "list", list(), NA, NA,
                    paste("Named list of seeds to use for each event (names).",
                          "E.g., `list('init' = 123)` will `set.seed(123)`",
                          "for the `init` event only."))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = SpaDES.core::P(sim, module = "caribouMovement")$stackName,
                 objectClass = "RasterStack", desc = "layername = \"habitatQuality\"",
                 sourceURL = NA_character_),
    expectsInput(objectName = "caribou",
                 objectClass = "SpatialPointsDataFrame", desc = "Object holding caribou locations",
                 sourceURL = NA_character_)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "caribou", objectClass = "SpatialPointsDataFrame",
                  desc = NA_character_),
    createsOutput(objectName = "habitatQuality", objectClass = "RasterLayer",
                  desc = NA_character_)
  )
))

## event types
doEvent.caribouMovement <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      SpaDES.core::checkObject(sim, name = SpaDES.core::P(sim)$stackName, layer = "habitatQuality")

      # do stuff for this event
      sim <- Init(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, SpaDES.core::P(sim)$moveInitialTime,
                           "caribouMovement", "move")
      sim <- scheduleEvent(sim, SpaDES.core::P(sim)$.plotInitialTime,
                           "caribouMovement", "plot.init", .last())
      sim <- scheduleEvent(sim, SpaDES.core::P(sim)$.saveInitialTime,
                           "caribouMovement", "save", .last() + 1)
    },
    move = {
      # do stuff for this event
      sim <- Move(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + SpaDES.core::P(sim)$moveInterval, "caribouMovement", "move")
    },
    plot.init = {
      # do stuff for this event
      Plot(sim$caribou, addTo = paste("sim", SpaDES.core::P(sim)$stackName, "habitatQuality", sep = "$"),
           new = FALSE, size = 0.2, pch = 19, gp = gpar(cex = 0.6))

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + SpaDES.core::P(sim)$.plotInterval, "caribouMovement", "plot", .last())
    },
    plot = {
      # do stuff for this event
      Plot(sim$caribou, addTo = paste("sim", SpaDES.core::P(sim)$stackName, "habitatQuality", sep = "$"),
           new = FALSE, pch = 19, size = 0.2, gp = gpar(cex = 0.6))
      Plot(sim$caribou, new = FALSE, pch = 19, size = 0.1, gp = gpar(cex = 0.6))

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + SpaDES.core::P(sim)$.plotInterval, "caribouMovement", "plot", .last())
    },
    save = {
      # do stuff for this event
      sim <- saveFiles(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + SpaDES.core::P(sim)$.saveInterval, "caribouMovement", "save", .last() + 1)

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
  yrange <- c(ymin(sim[[SpaDES.core::P(sim)$stackName]]),
              ymax(sim[[SpaDES.core::P(sim)$stackName]]))
  xrange <- c(xmin(sim[[SpaDES.core::P(sim)$stackName]]),
              xmax(sim[[SpaDES.core::P(sim)$stackName]]))

  # initialize caribou agents
  N <- SpaDES.core::P(sim)$N
  IDs <- as.character(1:N)
  sex <- sample(c("female", "male"), N, replace = TRUE)
  age <- round(rnorm(N, mean = 8, sd = 3))
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  starts <- cbind(x = runif(N, xrange[1], xrange[2]),
                  y = runif(N, yrange[1], yrange[2]))

  # create the caribou agent object
  sim$caribou <- SpatialPointsDataFrame(coords = starts,
                                        data = data.frame(x1, y1, sex, age))
  row.names(sim$caribou) <- IDs # alternatively, add IDs as column in data.frame above

  return(invisible(sim))
}

Move <- function(sim) {
  # crop any caribou that went off maps
  sim$caribou <- crop(sim$caribou, sim[[SpaDES.core::P(sim)$stackName]])
  if (length(sim$caribou) == 0) stop("All agents are off map")

  habitatQuality <- sim[[SpaDES.core::P(sim)$stackName]][["habitatQuality"]]

  # find out what pixels the individuals are on now
  ex <- habitatQuality[sim$caribou]

  # step length is a function of current cell's habitat quality
  sl <- 0.25 / ex

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  sim$caribou <- move("crw", agent = sim$caribou,
                      extent = extent(sim[[SpaDES.core::P(sim)$stackName]]),
                      stepLength = ln, stddev = sd, lonlat = FALSE,
                      torus = SpaDES.core::P(sim)$torus)

  ## export habitat quality
  sim$habitatQuality <- habitatQuality

  return(invisible(sim))
}
