path <- system.file("sampleModules", package = "SpaDES.core")
sampleModules <- dir(path)
## turn off code checking -- don't need it here
opts <- options("spades.moduleCodeChecks" = FALSE,
                "spades.useRequire" = FALSE)

x <- moduleMetadata(sampleModules[3], path = path)

## using simList
if (require("SpaDES.tools", quietly = TRUE)) {
   mySim <- simInit(
      times = list(start = 2000.0, end = 2001.0, timeunit = "year"),
      params = list(
        .globals = list(stackName = "landscape")
      ),
      modules = list("caribouMovement"),
      paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
   )
   moduleMetadata(sim = mySim)
}

# turn code checking back on -- don't need it here
options(opts)
