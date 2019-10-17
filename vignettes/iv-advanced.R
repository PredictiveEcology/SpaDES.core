## ----memoryUse, eval=FALSE, echo=TRUE------------------------------------
#  library(reproducible)
#  Require("future") # install it if necessary
#  Require("future.callr") # install it if necessary
#  options("spades.memoryUseInterval" = 0.5)
#  
#  # run your simInit and spades calls here
#  # sim <- simInit()
#  # sim <- spades(sim)
#  
#  memoryUse(sim, max = TRUE) # this should show peak memory use by eventType -- i.e., summarizes if multiple times
#  memoryUse(sim, max = FALSE) # this should show peak memory use by event

