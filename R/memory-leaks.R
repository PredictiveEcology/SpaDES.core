#' @importFrom utils object.size packageVersion
testObjectForLeak <- function(objs, envir, class, type, customMessage) {
  lapply(seq(objs), function(i) {
    x <- get(objs[i], envir = envir)
    nam <- objs[i]
    if (is(x, class)) {
      if (!(isTopLevelEnv(environment(x)))) {
        os1 <- object.size(x)
        if (packageVersion("reproducible") > "1.2.6") {
          os2 <- sum(unlist(objSize(environment(x))))
        } else {
          os2 <- sum(unlist(objSize2(x)))
        }
        if (os2 > os1 * 50) { # was 50; probably should be
          memoryLeakWarning(class, type, nam, customMessage = customMessage)
        }
      }
    }
    NULL
  })
}

memoryLeakWarning <- function(class, where, objName, customMessage) {
  if (length(objName))
    warning(paste0("A ", class, ", ", objName,
                   ", has been added to the ", where, "; this is causing a memory leak; ",
                   customMessage))
}

testMemoryLeaks <- function(simEnv, modEnv, modName, knownObjects) {
  testedObjects <- knownObjects
  untested <- list()
  envirModObjects <- modEnv
  untested$simObjects <- setdiff(ls(simEnv, all.names = TRUE), testedObjects$sim)
  untested$modObjects <- setdiff(ls(envirModObjects, all.names = TRUE),
                                 testedObjects[[modName]])
  if (length(unlist(untested))) {
    out <- testObjectForLeak(untested$simObjects, simEnv, "formula", "simList",
                             customMessage = "It is suggested to put it in the simList as a character string, then eval it when needed")
    # browser()
    out <- testObjectForLeak(untested$simObjects, simEnv, "function", "simList",
                             customMessage = "It is suggested to add it as a normal function in the module, not a nested function.")
    out <- testObjectForLeak(untested$modObjects, modEnv, "formula", "mod",
                             customMessage = paste("It is suggested to put it in the mod object of the ",
                                                   modName,
                                                   " as a character string, then eval it when needed"))
    out <- testObjectForLeak(untested$modObjects, modEnv, "function", "mod",
                             customMessage = "It is suggested to add it as a normal function in the module, not a nested function.")

    knownObjects[[modName]] <- c(knownObjects[[modName]], untested$modObjects)
    knownObjects$sim <- c(knownObjects$sim, untested$simObjects)

  }
  knownObjects
}

isTopLevelEnv <- function(x) {
  identical(.GlobalEnv, x) ||
    isNamespace(x) ||
    identical(emptyenv(), x) ||
    identical(baseenv(), x)
}

objSize2 <- function(x, quick = getOption("reproducible.quick", FALSE),
                     enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  varName <- deparse(substitute(x))
  if (isTRUE(enclosingEnvs) && (!isTopLevelEnv(environment(x)))) {
    if (is.primitive(x)) {
      os <- list(object.size(x))
    } else {
      x <- mget(ls(envir = environment(x)), envir = environment(x))
      os <- lapply(x, function(xx) object.size(xx))
    }
  } else {
    os <- object.size(x)
  }
  return(os)
}
