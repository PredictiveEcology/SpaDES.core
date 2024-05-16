#' Test and update a parameter against same parameter in other modules
#'
#' This function is intended to be part of module code and will test whether
#' the value of a parameter within the current module matches the value of the
#' same parameter in other modules. This is a test for parameters that might expect
#' to be part of a `params = list(.globals = list(someParam = "test"))` passed to [simInit()].
#'
#' It is considered a "fail" under several conditions:
#' 1. current module has a value that is not `NULL` or `"default"` and another module
#'    has a different value;
#' 2. there is more than one value for the `paramToCheck` in the other modules,
#'    so it is ambiguous which one to return.
#'
#' Either the current module is different than other modules, unless it is "default" or NULL.
#'
#' @param sim A `simList` object
#'
#' @param paramToCheck A character string, length one, of a parameter name to
#'   check and compare between the current module and one or more or all others
#'
#' @param moduleToUse A character vector of module names to check against. This can be
#'   `"all"` which will compare against all other modules.
#'
#' @param ifSetButDifferent A character string indicating whether to `"error"` the default,
#'   or send a `"warning"`, `message` or just silently continue (any other value).
#'
#' @param verbose Logical or Numeric, follows `reproducible.verbose` value by default.
#'
#' @return If the value of the `paramToCheck` in the current module is either `NULL` or
#' `"default"`, and there is only one other value across all modules named in `moduleToUse`,
#' then this will return a character string with the value of the single parameter value
#' in the other module(s).
#' It will return the current value if there are no other modules with the same parameter.
#'
#' @export
#' @rdname paramCheckOtherMods
paramCheckOtherMods <- function(sim, paramToCheck, moduleToUse = "all",
                                ifSetButDifferent = c("error", "warning", "message", "silent"),
                                verbose = getOption("reproducible.verbose")) {
  currentModule <- currentModule(sim)
  paramsInSim <- params(sim)
  paramInCurrentMod <- P(sim)[[paramToCheck]]
  if (identical(moduleToUse, "all")) {
    moduleToUse <- names(paramsInSim)
  }
  paramInThisMod <- paramsInSim[[currentModule]][[paramToCheck]]
  params <- paramsInSim[setdiff(moduleToUse, currentModule)]

  ## preserve list for parameters composed of several values - will this work with lists of lists?
  ## may need a Reduce(..., identical)?
  paramToUpdateValInOtherMods <- lapply(params, function(p) p[[paramToCheck]])
  ## remove NULLs
  paramToUpdateValInOtherMods <- paramToUpdateValInOtherMods[!sapply(paramToUpdateValInOtherMods, is.null)]
  # paramToUpdateValInOtherMods <- lapply(paramToUpdateValInOtherMods, sort)
  dups <- duplicated(paramToUpdateValInOtherMods)
  paramInOtherMods <- paramToUpdateValInOtherMods[!dups]
  ## again, preserve list -- if there is only one entry, all definitions are identical
  # paramInOtherMods <- unique(paramToUpdateValInOtherMods)

  messSuff <- paste0("); they should not. Perhaps pass params = list(.globals = list(",
                     paramToCheck, " = '", paramInOtherMods[1], "')) in the simInit call?")

  newVal <- paramInThisMod
  fail <- FALSE

  test <- if (is.list(paramInOtherMods)) {
    all(sapply(paramInOtherMods, function(x, paramInThisMod) identical(paramInThisMod, x),
               paramInThisMod = paramInThisMod))
  } else {
    identical(paramInThisMod, paramInOtherMods)
  }

  if (!test) {
    if (is.null(paramInThisMod) || identical("default", paramInThisMod)) {
      if (length(paramInOtherMods) == 1) {
        newVal <- unlist(paramInOtherMods) ## can unlist here
        message(paramToCheck, " in ", currentModule," is set to 'default' or NULL;")
        message("... setting to '", newVal,
                "' to match value in ",paste(names(paramToUpdateValInOtherMods), collapse = ", ")," in the simList")
      } else if (length(paramInOtherMods) > 1) {
        mess <- paste0("Modules in this simList have multiple values for ", paramToCheck," (",
                       paste(paramInOtherMods, collapse = ", "),
                       messSuff)
        fail <- TRUE
      }
    } else {
      if (length(paramInOtherMods) > 0) {
        mess <- paste0("Including this module, there are multiple values for ",paramToCheck," (",
                       paste(c(paramInThisMod, paramInOtherMods), collapse = ", "),
                       messSuff)
        fail <- TRUE
      }
    }
    if (isTRUE(fail)) {
      if (is.null(paramInThisMod)) {
        paramInThisMod <- "NULL"  ## avoid failure below due to 0 length
      }

      # dfThis <- data.frame(module = currentModule, value = paramInThisMod, row.names = NULL)
      # dfOther <- data.frame(module = names(paramToUpdateValInOtherMods),
      #                       value = paramToUpdateValInOtherMods, row.names = NULL)
      ## this works better when the parameter has length()>1 :
      dfThis <- data.frame(modName = paramInThisMod, row.names = NULL)
      names(dfThis) <- currentModule
      dfOther <- as.data.frame(paramToUpdateValInOtherMods, row.names = NULL)
      messageVerbose("This module", verbose = verbose)
      messageDF(dfThis, colour = "green", verbose = verbose)
      messageVerbose("Other modules", verbose = verbose)
      messageDF(dfOther, verbose = verbose)

      if (identical(ifSetButDifferent[1], "error")) stop(mess)
      if (identical(ifSetButDifferent[1], "warning")) warning(mess)
      if (identical(ifSetButDifferent[1], "message")) message(mess)
    }
  }

  newVal
}
