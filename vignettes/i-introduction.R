## ----setup, include=FALSE-----------------------------------------------------
# List the vignette-only packages this file *uses*:
vignette_pkgs <- c("SpaDES.tools", "NLMR")

# Helper that checks availability without attaching:
has_pkgs <- function(pkgs) all(vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)))

# CRAN sets NOT_CRAN = "false"; locally it's usually "true" via devtools::check().
not_cran <- identical(Sys.getenv("NOT_CRAN"), "true")

# Evaluate code chunks only if:
#  (a) we're NOT on CRAN, and
#  (b) all vignette packages are available
knitr::opts_chunk$set(
  eval = not_cran && has_pkgs(vignette_pkgs),
  message = FALSE,
  warning = FALSE
)

options(
  spades.moduleCodeChecks = FALSE,
  spades.useRequire = FALSE)

## ----SpaDES-demo, eval=FALSE, echo=TRUE---------------------------------------
# ## NOTE: Suggested packages SpaDES.tools and NLMR packages must be installed
# #install.packages("SpaDES.taols")
# #install.packages("NLMR", repos = "https://predictiveecology.r-universe.dev/")
# 
# knitr::opts_chunk$set(eval = requireNamespace("SpaDES.tools") && !requireNamespace("NLMR"))
# 
# library(SpaDES.core)
# 
# demoSim <- suppressMessages(simInit(
#   times = list(start = 0, end = 100),
#   modules = "SpaDES_sampleModules",
#   params = list(
#     .globals = list(burnStats = "nPixelsBurned"),
#     randomLandscapes = list(
#       nx = 1e2, ny = 1e2, .saveObjects = "landscape",
#       .plotInitialTime = NA, .plotInterval = NA, inRAM = TRUE
#     ),
#     caribouMovement = list(
#       N = 1e2, .saveObjects = "caribou",
#       .plotInitialTime = 1, .plotInterval = 1, moveInterval = 1
#     ),
#     fireSpread = list(
#       nFires = 1e1, spreadprob = 0.235, persistprob = 0, its = 1e6,
#       returnInterval = 10, startTime = 0,
#       .plotInitialTime = 0, .plotInterval = 10
#     )
#   ),
#   path = list(modulePath = getSampleModules(tempdir()))
# ))
# spades(demoSim)

## ----SpaDES-modules, eval=FALSE, echo=TRUE------------------------------------
# downloadModule(name = "moduleName")

## ----view-sim, eval=FALSE, echo=TRUE------------------------------------------
# # full simulation details:
# #  simList object info + simulation data
# mySim
# 
# # less detail:
# # simList object isn't shown; object details are
# ls.str(mySim)
# 
# # least detail:
# # simList object isn't shown; object names only
# ls(mySim)

## ----view-dependencies, eval=FALSE, echo=TRUE---------------------------------
# library(igraph)
# library(DiagrammeR)
# depsEdgeList(mySim, FALSE)  # data.frame of all object dependencies
# moduleDiagram(mySim)        # plots simplified module (object) dependency graph
# objectDiagram(mySim)        # plots object dependency diagram

## ----view-event-sequences, eval=FALSE, echo=TRUE------------------------------
# options(spades.nCompleted = 50)   # default: store 1000 events in the completed event list
# mySim <- simInit(...)             # initialize a simulation using valid parameters
# mySim <- spades(mySim)            # run the simulation, returning the completed sim object
# eventDiagram(mySim)               # visualize the sequence of events for all modules

