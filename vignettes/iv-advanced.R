## ----memoryUse, eval=FALSE, echo=TRUE---------------------------------------------------------------------------------
# if (requireNamespace("future", quietly = TRUE) &&
#   requireNamespace("future.callr", quietly = TRUE)) {
#   options("spades.memoryUseInterval" = 0.5)
# 
#   # run your simInit and spades calls here
#   # sim <- simInit()
#   # sim <- spades(sim)
# 
#   memoryUse(sim, max = TRUE) # this should show peak memory use by eventType -- i.e., summarizes if multiple times
#   memoryUse(sim, max = FALSE) # this should show peak memory use by event
# }

