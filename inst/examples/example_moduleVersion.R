# using filepath
path <- system.file("sampleModules", package = "SpaDES.core")
moduleVersion("caribouMovement", path)

# using simList
options("spades.useRequire" = FALSE)
if (require("SpaDES.tools", quietly = TRUE)) {
   mySim <- simInit(
      times = list(start = 2000.0, end = 2002.0, timeunit = "year"),
      params = list(
        .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
      ),
      modules = list("caribouMovement"),
      paths = list(modulePath = path)
   )
   moduleVersion("caribouMovement", sim = mySim)
}
