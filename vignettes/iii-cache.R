## ----setup, include = FALSE---------------------------------------------------
SuggestedPkgsNeeded <- c("knitr", "NLMR", "RColorBrewer", "SpaDES.tools")
hasSuggests <- all(sapply(SuggestedPkgsNeeded, require, character.only = TRUE, quietly = TRUE))
useSuggests <- !(tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_")) == "true")

knitr::opts_chunk$set(eval = hasSuggests && useSuggests)

options(
  spades.loadReqdPkgs = FALSE,
  spades.moduleCodeChecks = FALSE,
  spades.useRequire = FALSE
)

## ----examples, echo=TRUE, message=FALSE---------------------------------------
library(terra)
library(reproducible)
library(SpaDES.core)

mySim <- simInit(
  times = list(start = 0.0, end = 3.0),
  params = list(
    .globals = list(stackName = "landscape", burnStats = "testStats"),
    randomLandscapes = list(.plotInitialTime = NA),
    fireSpread = list(.plotInitialTime = NA)
  ),
  modules = list("randomLandscapes", "fireSpread"),
  paths = list(modulePath = getSampleModules(tempdir()))
)

## ----spades-------------------------------------------------------------------
# compare caching ... run once to create cache
system.time({
  outSim <- spades(Copy(mySim), cache = TRUE, notOlderThan = Sys.time())
})

## ----spades-cached------------------------------------------------------------
# faster 2nd time
system.time({
  outSimCached <- spades(Copy(mySim), cache = TRUE)
})
all.equal(outSim, outSimCached) ## TODO: not equal
## [1] "Length mismatch: comparison on first 4 components"                                                                                         
## [2] "Component “landscape”: Attributes: < Component “pntr”: Component “dataType”: Lengths (5, 1) differ (string compare on first 1) >"          
## [3] "Component “landscape”: Attributes: < Component “pntr”: Component “inMemory”: Lengths (5, 1) differ (comparison on first 1 components) >"   
## [4] "Component “landscape”: Attributes: < Component “pntr”: Component “isMD”: Lengths (5, 1) differ (comparison on first 1 components) >"       
## [5] "Component “landscape”: Attributes: < Component “pntr”: Component “is_multidim”: Lengths (5, 1) differ (comparison on first 1 components) >"

## ----module-level, echo=TRUE--------------------------------------------------
## Module-level
params(mySim)$randomLandscapes$.useCache <- TRUE
system.time({
  randomSim <- spades(Copy(mySim), .plotInitialTime = NA,
                      notOlderThan = Sys.time(), debug = TRUE)
})

## faster the second time
system.time({
  randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
})

## ----test-module-level--------------------------------------------------------
layers <- list("DEM", "forestAge", "habitatQuality", "percentPine", "Fires")
same <- lapply(layers, function(l) {
  identical(randomSim$landscape[[l]], randomSimCached$landscape[[l]])
})
names(same) <- layers

## Fires is not same because all non-init events in fireSpread are not cached
print(same)

## ----event-level, echo=TRUE---------------------------------------------------
params(mySim)$fireSpread$.useCache <- "init"
system.time({
  randomSim <- spades(
    Copy(mySim),
    .plotInitialTime = NA,
    notOlderThan = Sys.time(),
    debug = TRUE
  )
})

# faster the second time
system.time({
  randomSimCached <- spades(Copy(mySim), .plotInitialTime = NA, debug = TRUE)
})

## ----function-level, echo=TRUE------------------------------------------------
ras <- terra::rast(terra::ext(0, 1e3, 0, 1e3), res = 1, vals = 1)
system.time({
  map <- Cache(SpaDES.tools::neutralLandscapeMap(ras),
               cachePath = cachePath(mySim),
               userTags = "neutralLandscapeMap",
               notOlderThan = Sys.time())
})

# faster the second time
system.time({
  mapCached <- Cache(SpaDES.tools::neutralLandscapeMap(ras),
                     cachePath = cachePath(mySim),
                     userTags = "neutralLandscapeMap")
})

## NOTE: can't use all.equal on SpatRaster (they are pointers); use compareGeom()
all.equal(map[], mapCached[]) 

## ----manual-cache-------------------------------------------------------------
cacheDB <- showCache(mySim, userTags = "neutralLandscapeMap")

## get the RasterLayer that was produced with neutralLandscapeMap()
map <- loadFromCache(cacheId = cacheDB$cacheId, cachePath = cachePath(mySim))
plot(map)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# simInit() --> many .inputObjects calls
# 
# spades() call --> many module calls --> many event calls --> many function calls

## ----eval=FALSE, echo=TRUE----------------------------------------------------
# parameters = list(
#   FireModule = list(.useCache = TRUE)
# )
# mySim <- simInit(..., params = parameters)
# mySimOut <- spades(mySim)

