path <- system.file("sampleModules", package = "SpaDES.core")
sampleModules <- dir(path)
x <- moduleMetadata(sampleModules[3], path)

# using simList
mySim <- simInit(
   times = list(start = 2000.0, end = 2002.0, timeunit = "year"),
   params = list(
     .globals = list(stackName = "landscape")
   ),
   modules = list("caribouMovement"),
   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
)
moduleMetadata(sim = mySim)
