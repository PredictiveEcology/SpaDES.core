makeModActiveBinding <- function(sim, mod) {
  makeActiveBinding(sym = "mod",
                    fun = function(value){
                      if (missing(value)) {
                        get(".objects", envir = sim@.xData[[mod]], inherits = FALSE)
                      } else {
                        stop("Can't overwrite mod")
                      }
                    },
                    env = sim@.xData[[mod]])
}
