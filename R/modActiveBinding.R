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

makeParActiveBinding <- function(sim, mod) {
  makeActiveBinding(sym = "Par",
                    fun = function(value){
                      if (missing(value)) {
                        sim@params[[mod]]
                      } else {
                        stop("Can't overwrite Par")
                      }
                    },
                    env = sim@.xData[[mod]])
}
