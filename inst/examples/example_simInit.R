#' \dontrun{
mySim <- simInit(
 times = list(start = 0.0, end = 2.0, timeunit = "year"),
 params = list(
   .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
 ),
 modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
)
spades(mySim, .plotInitialTime = NA)

# Change more parameters, removing plotting
mySim <- simInit(
 times = list(start = 0.0, end = 2.0, timeunit = "year"),
 params = list(
   .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
   fireSpread = list(.plotInitialTime = NA)
 ),
 modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
)
outSim <- spades(mySim)

# A little more complicated with inputs and outputs
if (require(rgdal)) {
 mapPath <- system.file("maps", package = "quickPlot")
 mySim <- simInit(
   times = list(start = 0.0, end = 2.0, timeunit = "year"),
   params = list(
     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
   ),
   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                outputPath = tempdir()),
   inputs = data.frame(
     files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
     functions = "raster",
     package = "raster",
     loadTime = 0,
     stringsAsFactors = FALSE),
   outputs = data.frame(
     expand.grid(objectName = c("caribou","landscape"),
     saveTime = 1:2,
     stringsAsFactors = FALSE))
 )

 # Use accessors for inputs, outputs, times
 mySim2 <- simInit(
   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
   params = list(.globals = list(stackName = "landscape", burnStats = "nPixelsBurned")),
   paths = list(
     modulePath = system.file("sampleModules", package = "SpaDES.core"),
     outputPath = tempdir()
   )
 )

 # add by accessor: note need current in times() accessor
 times(mySim2) <- list(current = 0, start = 0.0, end = 2.0, timeunit = "year")
 inputs(mySim2) <- data.frame(
     files = dir(file.path(mapPath), full.names = TRUE, pattern = "tif")[1:2],
     functions = "raster",
     package = "raster",
     loadTime = 3,
     stringsAsFactors = FALSE)
 outputs(mySim2) <- data.frame(
     expand.grid(objectName = c("caribou","landscape"),
     saveTime = 1:2,
     stringsAsFactors = FALSE))
 all.equal(mySim, mySim2) # TRUE
}
#' }
