makeModActiveBinding <- function(sim, mod) {
  makeActiveBinding(sym = "mod",
                    fun = activeModBindingFunction,
                    env = sim@.xData$.mods[[mod]])
}

makeParActiveBinding <- function(sim, mod) {
  makeActiveBinding(sym = "Par",
                    fun = activeParBindingFunction,
                    env = sim@.xData$.mods[[mod]])
}

activeModBindingFunction <- function(value) {
  if (missing(value)) {
    simEnv <- try(whereInStack("sim"), silent = TRUE)
    if (!is(simEnv, "try-error")) {
      sim <- get("sim", simEnv, inherits = FALSE)
      mod <- currentModule(sim)
      if (length(mod))
        get(".objects", envir = sim@.xData$.mods[[mod]], inherits = FALSE)
    } else {
      NULL
    }
  } else {
    stop("Can't overwrite mod")
  }
}

#' @importFrom quickPlot whereInStack
activeParBindingFunction <- function(value) {
  if (missing(value)) {
    simEnv <- try(whereInStack("sim"), silent = TRUE)
    if (!is(simEnv, "try-error")) {
      sim <- get("sim", simEnv, inherits = FALSE)
      mod <- currentModule(sim)
      if (length(mod))
        sim@params[[mod]]
    } else {
      NULL
    }
  } else {
    stop("Can't overwrite Par")
  }

}
