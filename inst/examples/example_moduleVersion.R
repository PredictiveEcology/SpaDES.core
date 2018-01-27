path <- system.file("sampleModules", package = "SpaDES.core")

# using filepath
moduleVersion("caribouMovement", path)

# using simList
mySim <- simInit(
   times = list(start = 2000.0, end = 2002.0, timeunit = "year"),
   params = list(
     .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
   ),
   modules = list("caribouMovement"),
   paths = list(modulePath = path)
)
moduleVersion("caribouMovement", sim = mySim)
