## ----examples, echo=TRUE, message=FALSE----------------------------------
library(magrittr)
library(raster)
library(reproducible)
library(SpaDES.core)

opts <- options("spades.moduleCodeChecks" = FALSE) # turns off syntactic checking of modules
mySim <- simInit(
  times = list(start = 0.0, end = 5.0),
  params = list(
    .globals = list(stackName = "landscape", burnStats = "testStats"),
    randomLandscapes = list(.plotInitialTime = NA),
    fireSpread = list(.plotInitialTime = NA)
  ),
  modules = list("randomLandscapes", "fireSpread"),
  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core")))
options(opts)

## ----spades--------------------------------------------------------------
# compare caching ... run once to create cache
system.time({
  outSim <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time())
})

## ----spades-cached-------------------------------------------------------
# vastly faster 2nd time
system.time({
  outSimCached <- spades(Copy(mySim), cache = TRUE)
})
all.equal(outSim, outSimCached) 

## ----experiment-cache----------------------------------------------------
system.time({
  sims1 <- experiment(mySim, replicates = 2, cache = TRUE)
})

# internal -- second time faster
system.time({
  sims2 <- experiment(mySim, replicates = 2, cache = TRUE)
})
all.equal(sims1, sims2)

## ----Cache-experiment----------------------------------------------------
# External
outputs(mySim) <- data.frame(objectName = "landscape")
system.time({
  sims3 <- Cache(experiment, mySim, replicates = 3, .plotInitialTime = NA,
                 clearSimEnv = TRUE)
})

## ----Cache-experiment-2--------------------------------------------------
system.time({
  sims4 <- Cache(experiment, mySim, replicates = 3, .plotInitialTime = NA,
                 clearSimEnv = TRUE)
})
# test they are all equal
lapply(1:2, function(x) all.equal(sims3[[x]], sims4[[x]])) 

dir(outputPath(mySim), recursive = TRUE)

## ----module-level, echo=TRUE---------------------------------------------
# Module-level
params(mySim)$randomLandscapes$.useCache <- TRUE
system.time({
  randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
                      notOlderThan = Sys.time(), debug = TRUE)
})

# vastly faster the second time
system.time({
  randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
})

## ----test-module-level---------------------------------------------------
layers <- list("DEM", "forestAge", "habitatQuality", "percentPine", "Fires")
same <- lapply(layers, function(l)
  identical(randomSim$landscape[[l]], randomSimCached$landscape[[l]]))
names(same) <- layers
print(same) # Fires is not same because all non-init events in fireSpread are not cached

## ----event-level, echo=TRUE----------------------------------------------
params(mySim)$fireSpread$.useCache <- "init"
system.time({
  randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
                      notOlderThan = Sys.time(), debug = TRUE)
})

# vastly faster the second time
system.time({
  randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
})

