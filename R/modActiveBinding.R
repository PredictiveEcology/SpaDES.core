makeSimListActiveBindings <- function(sim) {
  lapply(modules(sim), function(mod) {
    makeModActiveBinding(sim = sim, mod = mod)
    makeParActiveBinding(sim = sim, mod = mod)
  })
}


makeModActiveBinding <- function(sim, mod) {
  if (.isPackage(fullModulePath = mod, sim = sim)) {
    env <- asNamespace(.moduleNameNoUnderscore(mod))
  } else {
    env <- sim@.xData$.mods[[mod]]
    if (exists("mod", envir = env, inherits = FALSE))
      rm(list = "mod", envir = env, inherits = FALSE)
    makeActiveBinding(sym = "mod",
                      fun = activeModBindingFunction,
                      env = env)
    }

  if (exists("aaaaa", envir = .GlobalEnv, inherits = FALSE))
    browser()


}

#' @include helpers.R
makeParActiveBinding <- function(sim, mod) {
  if (.isPackage(fullModulePath = mod, sim = sim)) {
    env <- asNamespace(.moduleNameNoUnderscore(mod))
   } else {
    env <- sim@.xData$.mods[[mod]]
    if (exists("Par", envir = env, inherits = FALSE))
      rm(list = "Par", envir = env, inherits = FALSE)
    makeActiveBinding(sym = "Par",
                      fun = activeParBindingFunction,
                      env = env)
    }

}

activeModBindingFunction <- function(value) {
  ret <- NULL
  if (missing(value)) {
    simEnv <- try(whereInStack("sim"), silent = TRUE)
    if (exists("aaaaa", envir = .GlobalEnv, inherits = FALSE))
      browser()
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
