#######################
# inputs
#######################

# Start with a basic empty simList
sim <- simInit()

test <- 1:10
library(reproducible) # for checkPath
tmpdir <- file.path(tempdir(), "inputs") |> checkPath(create = TRUE)
tmpFile <- file.path(tmpdir, "test.rds")
saveRDS(test, file = tmpFile)
inputs(sim) <- data.frame(file = tmpFile) # using only required column, "file"
inputs(sim) # see that it is not yet loaded, but when it is scheduled to be loaded
simOut <- spades(sim)
inputs(simOut) # confirm it was loaded
simOut$test

# can put data.frame for inputs directly inside simInit call
allTifs <- dir(system.file("maps", package = "quickPlot"),
               full.names = TRUE, pattern = "tif")

# next: .objectNames are taken from the filenames (without the extension)
# This will load all 5 tifs in the SpaDES sample directory, using
#   the rast fuction in the terra package, all at time = 0
sim <- simInit(
  inputs = data.frame(
    files = allTifs,
    functions = "rast",
    package = "terra",
    loadTime = 0,
    stringsAsFactors = FALSE)
)

##############################
#A fully described inputs object, including arguments:
files <- dir(system.file("maps", package = "quickPlot"),
             full.names = TRUE, pattern = "tif")
# arguments must be a list of lists. This may require I() to keep it as a list
#   once it gets coerced into the data.frame.
# arguments = I(rep(list(native = TRUE), length(files)))
filelist <- data.frame(
  objectName = paste0("Maps", 1:5),
  files = files,
  functions = "terra::rast",
  # arguments = arguments,
  loadTime = 0,
  intervals = c(rep(NA, length(files) - 1), 10)
)
inputs(sim) <- filelist
spades(sim)

# Example showing loading multiple objects from global environment onto the
#   same object in the simList, but at different load times
a1 <- 1
a2 <- 2
# Note arguments must be a list of NROW(inputs), with each element itself being a list,
#  which is passed to do.call(fun[x], arguments[[x]]), where x is row number, one at a time
args <- lapply(1:2, function(x) {
               list(x = paste0("a", x),
                    envir = environment()) # may be necessary to specify in which envir a1, a2
                                           # are located, if not in an interactive sessino
               })
inputs <- data.frame(objectName = "a", loadTime = 1:2, fun = "base::get", arguments = I(args))
a <- simInit(inputs = inputs, times = list(start = 0, end = 1))
a <- spades(a)
identical(a1, a$a)

end(a) <- 3
a <- spades(a) # different object (a2) loaded onto a$a
identical(a2, a$a)

# Clean up after
unlink(tmpdir, recursive = TRUE)
