## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE)

hasSuggests <- all(
  require("NLMR", quietly = TRUE),
  require("SpaDES.tools", quietly = TRUE)
)

options("spades.moduleCodeChecks" = FALSE,
        "spades.useRequire" = FALSE)

## ----module-metadata, echo=FALSE, eval=TRUE, results="asis"-------------------
cat(c(
  "```",
  "## sample module metadata for the default `caribouMovement` module",
  readLines(system.file("sampleModules", "caribouMovement", "caribouMovement.R", package = "SpaDES.core"))[10:45],
  "```"
  ), sep = "\n")

## ----passing-params, eval=hasSuggests, echo=TRUE------------------------------
library(SpaDES.core)

outputDir <- file.path(tempdir(), "simOutputs")
times <- list(start = 0.0, end = 20.0)
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
  .progress = list(NA),
  randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
  fireSpread = list(
    nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
    returnInterval = 10, startTime = 0,
    .plotInitialTime = 0, .plotInterval = 10
  ),
  caribouMovement = list(
    N = 100L, moveInterval = 1, torus = TRUE,
    .plotInitialTime = 1, .plotInterval = 1
  )
)
modules <- list("randomLandscapes", "fireSpread", "caribouMovement")
objects <- list()
paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
              outputPath = outputDir)

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects, paths = paths)

## ----accessing-params, eval=FALSE, echo=TRUE----------------------------------
#  ## Access parameters
#  P(mySim)                    # shows all parameters
#  P(mySim, module = "caribouMovement") # only parameters in caribouMovement module
#  P(mySim)$caribouMovement    # same
#  P(mySim)$caribouMovement$N  # Only one parameter
#  
#  ## If used within the module source code, then module name can be omitted:
#  ## This will return NULL here, but will return the actual value if used
#  ## in a module
#  P(mySim)$N  # Only one parameter if used within a module

## ----event-types, echo=FALSE, eval=TRUE, results="asis"-----------------------
cat(c(
  "```",
  "## sample event type definitions for the default `caribouMovement` module",
  readLines(system.file("sampleModules", "caribouMovement", "caribouMovement.R", package = "SpaDES.core"))[61:116],
  "```"
  ), sep = "\n")

## ----event-functions, echo=FALSE, eval=TRUE, results="asis"-------------------
cat(c(
  "```",
  "## sample Init event function from the default `caribouMovement` module",
  readLines(system.file("sampleModules", "caribouMovement", "caribouMovement.R", package = "SpaDES.core"))[121:143],
  "```"
  ), sep = "\n")

## ----sim-eventDiagram, eval=hasSuggests, echo=FALSE, message=FALSE, warning=FALSE----
library(magrittr)
library(SpaDES.core)

parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
  .progress = list(NA),
  randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
  fireSpread = list(nFires = 10L, spreadprob = 0.225, its = 1e6,
                    persistprob = 0, returnInterval = 1, startTime = 0,
                    .plotInitialTime = 0, .plotInterval = 10),
  caribouMovement = list(N = 100L, moveInterval = 1, torus = TRUE,
                         .plotInitialTime = 1, .plotInterval = 1)
)

ftmp <- tempfile("spades_vignetteOutputs", fileext = ".pdf")
pdf(ftmp)
clearPlot()
mySim <- simInit(
  times = list(start = 0.0, end = 2.0, timeunit = "year"),
  params = parameters,
  modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
  objects = list(),
  paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
  ) %>% 
  spades()
dev.off()
unlink(normalizePath(ftmp))

## ----eventDiagram, echo=FALSE, eval=hasSuggests, fig.height=10, fig.width=7----
# overview of the events in the simulation
if (requireNamespace("DiagrammeR"))
  eventDiagram(mySim, "0000-06-01", n = 200, width = 720)

## ----checksums, eval=FALSE----------------------------------------------------
#  ## 1. specify your module here
#  moduleName <- "my_module"
#  
#  ## 2. use a temp dir to ensure all modules get fresh copies of the data
#  tmpdir <- file.path(tempdir(), "SpaDES_modules")
#  
#  ## 3. download your module's data to the temp dir
#  downloadData(moduleName, tmpdir)
#  
#  ## 4. initialize a dummy simulation to ensure any 'data prep' steps in the .inputObjects section are run
#  simInit(modules = moduleName)
#  
#  ## 5. recalculate your checksums and overwrite the file
#  checksums(moduleName, tmpdir, write = TRUE)
#  
#  ## 6. copy the new checksums file to your working module directory (the one not in the temp dir)
#  file.copy(from = file.path(tmpdir, moduleName, 'data', 'CHECKSUMS.txt'),
#            to = file.path('path/to/my/moduleDir', moduleName, 'data', 'CHECKSUMS.txt'),
#            overwrite = TRUE)

## ----module-object-diagrams, eval=hasSuggests, echo=TRUE, message=FALSE, fig.width=7----
library(SpaDES.core)

times <- list(start = 0.0, end = 20)
parameters <- list(
  .globals = list(stackName = "landscape", burnStats = "nPixelsBurned")
)
modules <- list("SpaDES_sampleModules")
paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

## examine simulation module (object) dependencies
depsEdgeList(mySim, FALSE)      # all object dependency relationships
clearPlot()
moduleDiagram(mySim)            # simplified visual representation of modules

clearPlot()
moduleDiagram(mySim, showParents = TRUE) # similar, but showing parent module grouping

# detailed visual representation of objects
objectDiagram(mySim, width = 720)

## ----checkpoints, echo=TRUE, eval=hasSuggests, message=FALSE------------------
# initialize a new simulation, setting the checkpoint interval and filename.
times <- list(start = 0, end = 30)
parameters <- list(
  .globals = list(stackName = "landscape"),
  .checkpoint = list(interval = 10, file = "chkpnt.RData")
)
modules <- list("randomLandscapes", "caribouMovement")
paths <- list(
  modulePath = system.file("sampleModules", package = "SpaDES.core")
)

mySim <- simInit(times = times, params = parameters, modules = modules, paths = paths)

# retrieve the checkpoint params from the simulation object
checkpointFile(mySim)
checkpointInterval(mySim)

## ----progress, echo=TRUE, eval=hasSuggests, message=FALSE---------------------
# initialize a new simulation, setting the progress parameters
mySim <- simInit(times = list(start = 0.0, end = 100.0),
                 params = list(.globals = list(stackName = "landscape"),
                               .progress = list(type = "text", interval = 10)),
                 modules = list("randomLandscapes"),
                 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core")))

# retrieve the checkpoint params from the simulation object
progressType(mySim)
progressInterval(mySim)

## ----load-save, echo=TRUE, eval=hasSuggests, message=FALSE--------------------
# initialize a new simulation, setting the load and save parameters
library(data.table)

outputDir <- file.path(tempdir(), "simOutputs")
mySim <- simInit(times = list(start = 0.0, end = 10),
                 params = list(
                   .globals = list(stackName = "landscape"),
                   randomLandscapes = list(
                     .saveInitialTime = 0, .saveInterval = 10,
                     .saveObjects = c("landscape"),
                     .savePath = file.path(outputDir, "randomLandscapes"))
                   ),
                 modules = list("randomLandscapes"),
                 paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                              outputPath = outputDir),
                 outputs = data.frame(objectName = "landscape")
)

# retrieve the load and save params from the simulation object
inputs(mySim)    # shows all files and objects that are "inputs"
outputs(mySim)   # shows all files and objects that are "outputs"

params(mySim)$randomLandscapes$.saveInitialTime
params(mySim)$randomLandscapes$.saveInterval
params(mySim)$randomLandscapes$.saveObjects
params(mySim)$randomLandscapes$.savePath
ftmp <- tempfile(pattern = "spades_vignetteOutputs", fileext = ".pdf")
pdf(ftmp)
clearPlot()
mySim2 <- spades(mySim)

# More sophisticated, passing arguments to outputs()
outputs(mySim) <- data.frame(
  objectName = "landscape", fun = "writeRaster", package = "raster",
  saveTime = c(3,6), arguments = I(lapply(c(3,6), function(x) {
    list(datatype = "FLT4S", format = "raster", overwrite = TRUE)
})))
mySim2 <- spades(mySim)
dev.off()
unlink(normalizePath(ftmp))

## ----save-events, echo=TRUE, eval=FALSE, message=FALSE------------------------
#  ### WITHIN A MODULE:
#  
#  # schedule a recurring save event
#  nextSave <- time(mySim) + params(mySim)$randomLandscapes$.saveInterval
#  sim <- scheduleEvent(mySim, nextSave, "randomLandscapes", "save")

## ----plotting, echo=TRUE, eval=FALSE, message=FALSE---------------------------
#  # initialize a new simulation, setting the load and save parameters
#  mySim <- simInit(times = list(start = 0.0, end = 100),
#                   params = list(
#                     .globals = list(stackName = "landscape"),
#                     randomLandscapes = list(.plotInitialTime = 0, .plotInterval = 1)
#                   ),
#                   modules = list("randomLandscapes"),
#                   paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
#  )
#  
#  # retrieve the plotting params from the simulation object
#  params(mySim)$randomLandscapes$.plotInitialTime
#  params(mySim)$randomLandscapes$.plotInterval

## ----plot-events, echo=TRUE, eval=FALSE, message=FALSE------------------------
#  ### WITHIN A MODULE:
#  
#  # schedule a recurring plot event
#  nextPlot <- time(mySim) + params(mySim)$randomLandscapes$.plotInterval
#  mySim <- scheduleEvent(mySim, nextPlot, "randomLandscapes", "save")

## ----caribouMovement, echo=TRUE, eval=FALSE-----------------------------------
#  openModules(system.file("sampleModules", package = "SpaDES.core"), "moduleName")

## ----download-module, echo=TRUE, eval=FALSE-----------------------------------
#  downloadModule("moduleName")

## ----create-new-module, eval=FALSE, echo=TRUE, message=FALSE------------------
#  # create a new module called "randomLandscape" in the "custom-modules" subdirectory
#  # and open the resulting file immediately for editing.
#  newModule(name = "randomLandscapes", path = "custom-modules", open = TRUE)

## ----module-group-init, eval=FALSE--------------------------------------------
#  library(DiagrammeR)
#  library(SpaDES.core)
#  
#  outputDir <- file.path(tempdir(), "simOutputs")
#  times <- list(start = 0.0, end = 20.0)
#  parameters <- list(
#    .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#    .progress = list(NA),
#    randomLandscapes = list(nx = 100L, ny = 100L, inRAM = TRUE),
#    fireSpread = list(
#      nFires = 10L, spreadprob = 0.225, its = 1e6, persistprob = 0,
#      returnInterval = 10, startTime = 0,
#      .plotInitialTime = 0, .plotInterval = 10
#    ),
#    caribouMovement = list(
#      N = 100L, moveInterval = 1, torus = TRUE,
#      .plotInitialTime = 1, .plotInterval = 1
#    )
#  )
#  modules <- list("SpaDES_sampleModules")
#  objects <- list()
#  paths <- list(
#    modulePath = system.file("sampleModules", package = "SpaDES.core"),
#    outputPath = outputDir
#  )
#  
#  mySim <- simInit(times = times, params = parameters, modules = modules,
#                   objects = objects, paths = paths)
#  
#  modules(mySim) # note the child modules are initialized

## ----module-group-dl, eval=FALSE----------------------------------------------
#  downloadModule("SpaDES_sampleModules")

## ----cleanup, eval=TRUE, echo=FALSE-------------------------------------------
unlink(outputDir, recursive = TRUE)

