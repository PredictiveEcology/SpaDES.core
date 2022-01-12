makeModActiveBinding <- function(sim, mod) {
  env <- if (.isPackage(fullModulePath = mod, sim = sim)) {
    asNamespace(.moduleNameNoUnderscore(mod))
  } else {
    sim@.xData$.mods[[mod]]
  }
  makeActiveBinding(sym = "mod",
                    fun = activeModBindingFunction,
                    env = env)
}

#' @include helpers.R
makeParActiveBinding <- function(sim, mod) {
  env <- if (.isPackage(fullModulePath = mod, sim = sim)) {
    asNamespace(.moduleNameNoUnderscore(mod))
  } else {
    sim@.xData$.mods[[mod]]
  }
  makeActiveBinding(sym = "Par",
                    fun = activeParBindingFunction,
                    env = env)
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
