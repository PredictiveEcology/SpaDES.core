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
  ret <- NULL
  if (missing(value)) {
    simEnv <- try(whereInStack("sim"), silent = TRUE)
    if (!is(simEnv, "try-error")) {
      sim <- try(get("sim", simEnv, inherits = FALSE), silent = TRUE)
      if (!is(sim, "try-error")) {
        mod <- currentModule(sim)
        if (length(mod) && !is.null(sim@.xData$.mods[[mod]]))
          ret <- get(".objects", envir = sim@.xData$.mods[[mod]], inherits = FALSE)
      }
    }
  } else {
    stop("Can't overwrite mod")
  }
  return(ret)
}

#' @importFrom quickPlot whereInStack
activeParBindingFunction <- function(value) {
  ret <- NULL
  if (missing(value)) {
    simEnv <- try(whereInStack("sim"), silent = TRUE)
    if (!is(simEnv, "try-error")) {
      sim <- try(get("sim", simEnv, inherits = FALSE), silent = TRUE)
      if (!is(sim, "try-error")) {
        mod <- currentModule(sim)
        if (length(mod))
          ret <- sim@params[[mod]]
      }
    }
  } else {
    stop("Can't overwrite Par")
  }
  return(ret)
}
