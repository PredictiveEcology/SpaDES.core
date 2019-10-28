## ----SpaDES-demo, eval=FALSE, echo=TRUE----------------------------------
#  library("SpaDES.core")
#  demo("spades-simulation", package = "SpaDES.core")

## ----SpaDES-modules, eval=FALSE, echo=TRUE-------------------------------
#  downloadModule(name = "moduleName")

## ----view-sim, eval=FALSE, echo=TRUE-------------------------------------
#  # full simulation details:
#  #  simList object info + simulation data
#  mySim
#  
#  # less detail:
#  # simList object isn't shown; object details are
#  ls.str(mySim)
#  
#  # least detail:
#  # simList object isn't shown; object names only
#  ls(mySim)

## ----view-dependencies, eval=FALSE, echo=TRUE----------------------------
#  library(igraph)
#  depsEdgeList(mySim, FALSE)  # data.frame of all object dependencies
#  moduleDiagram(mySim)        # plots simplified module (object) dependency graph
#  objectDiagram(mySim)        # plots object dependency diagram

## ----view-event-sequences, eval=FALSE, echo=TRUE-------------------------
#  options(spades.nCompleted = 50)   # default: store 1000 events in the completed event list
#  mySim <- simInit(...)             # initialize a simulation using valid parameters
#  mySim <- spades(mySim)            # run the simulation, returning the completed sim object
#  eventDiagram(mySim)               # visualize the sequence of events for all modules

