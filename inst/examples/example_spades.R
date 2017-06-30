#' \dontrun{
mySim <- simInit(
 times = list(start = 0.0, end = 2.0, timeunit = "year"),
 params = list(
   .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
 ),
 modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
)
spades(mySim)

# Different debug options
spades(mySim, debug = TRUE) # Fastest
spades(mySim, debug = "simList")
spades(mySim, debug = "print(table(sim$landscape$Fires[]))")

# Can turn off plotting, and inspect the output simList instead
out <- spades(mySim, .plotInitialTime = NA) # much faster
completed(out) # shows completed events

# use cache -- simInit should generally be rerun each time a spades call is made
#   to guarantee that it is identical. Here, run spades call twice, first
#   time to establish cache, second time to return cached result
for (i in 1:2) {
 mySim <- simInit(
   times = list(start = 0.0, end = 2.0, timeunit = "year"),
   params = list(
     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
   ),
   modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
 )
 print(system.time(out <- spades(mySim, cache = TRUE)))
}
#' }
