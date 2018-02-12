if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}


# These are known functions that will definitely cause conflicts unless they are
# prefixed by their packages.
conflictingFns <- c("\\<raster::levels\\>", "\\<raster::scale\\>")
conflictingFnsClean <- gsub(pattern = "\\\\<", conflictingFns, replacement = "")
conflictingFnsClean <- gsub(pattern = "\\\\>", conflictingFnsClean, replacement = "")

conflictingFnsSimple <- gsub(conflictingFns, pattern = "^.*::", replacement = "\\\\<")
conflictingFnsSimple <- gsub(pattern = "\\\\<", conflictingFnsSimple, replacement = "")
conflictingFnsSimple <- gsub(pattern = "\\\\>", conflictingFnsSimple, replacement = "")

clashingFns <- c("\\<quickPlot::Plot\\>")
clashingFnsClean <- gsub(pattern = "\\\\<", clashingFns, replacement = "")
clashingFnsClean <- gsub(pattern = "\\\\>", clashingFnsClean, replacement = "")

clashingFnsSimple <- gsub(clashingFns, pattern = "^.*::", replacement = "\\\\<")
clashingFnsSimple <- gsub(pattern = "\\\\<", clashingFnsSimple, replacement = "")
clashingFnsSimple <- gsub(pattern = "\\\\>", clashingFnsSimple, replacement = "")


#' Find all references to sim$
#'
#' @param envToFindSim An environment where sim is defined
#' @param moduleEnv The environment where the module functions are
#' @param type Either "get" or "assign" indicating which side of <- we are searching
#'
#' @return
#' A character string with all sim objects found
#'
#' @author Eliot McIntire
#' @keywords internal
#' @rdname findSims
.findAllSims <- function(envToFindSim, moduleEnv, type) {
  out <- unlist(unique(lapply(names(moduleEnv), function(x) {
    if (is.function(moduleEnv[[x]])) {
      aa <- deparse(moduleEnv[[x]])
      bb <- as.call(parse(text = aa))
      if (type == "get") {
        y <- .findGetSims(bb)
      } else {
        y <- .findAssignSims(bb)
      }
      y <- na.omit(y)
      if (all(is.na(y))) y <- character()
      if (length(y)) {
        hasSim <- grepl(y, pattern = "sim")
        if (length(y[hasSim])) {
          y[hasSim] <- lapply(y[hasSim], function(yParts) {
            tryCatch(eval(parse(text = yParts), envir = envToFindSim),
                     error = function(x) yParts)
          })
        }
        names(y) <- rep(x, length(y))
        isNull <- unlist(lapply(y, is.null))
        if (any(isNull))
          y <- y[!unlist(isNull)]
        isNA <- unlist(lapply(y, is.na))
        if (any(isNA))
          y <- y[!isNA]
        y <- y[unlist(lapply(y, is.character))]
      }
      y
    }
  })))
  return(out)
}

#' @keywords internal
#' @param x A call in which to search for sim
#' @rdname findSims
.findAssignSims <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`))) {
      # if it is an assign, only keep left hand side
      x <- x[[2]]
      if (as.character(x)[1] %in% c("$", "[[") &&
          identical(as.character(x[[2]]), "sim") && is.name(x[[3]])) {
        lhs <- as.character(x[[3]])
      } else {
        lhs <- character()
      }
    } else {
      lhs <- character()
    }

    unique(c(lhs, unlist(lapply(x, .findAssignSims))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, .findAssignSims)))
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}

#' @inheritParams .findAssignSims
#' @keywords internal
#' @rdname findSims
.findGetSims <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(is.null))) {
      # This is case where module may check for absense of a sim object with is.null
      #   This shouldn't make a message
      return(character())
    } else if (identical(x[[1]], quote(`<-`)) ) {
      simObj <- character()
      # if on the left side of a function, deleted those from x, we don't care here
      x <- x[-(1:2)]
    } else {
      if (as.character(x)[1] %in% c("$", "[[") &&
          identical(as.character(x[[2]]), "sim") && is.name(x[[3]])) {
        simObj <- as.character(x[[3]])
      } else {
        simObj <- character()
      }
    }
    unique(c(simObj, unlist(lapply(x, .findGetSims))))
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, .findGetSims)))
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }

}

#' Runs a series of code checks during simInit
#'
#' This uses codetools::codeCheck for function consistency, and
#' codetools::findGlobals to check for function collisions with known,
#' common function collisions (raster::stack, raster::scale)
#'
#' @param m module name
#' @param message rest of message
#'
#' @return
#' A message with that starts with paste0(m, ":", message)
#'
#' @keywords internal
#' @rdname runCodeChecks
.runCodeChecks <- function(sim, m, k) {
  hadParseMessage <- FALSE # initiate this which will be changed if there are parse messages
  # From codetools -- experimental
  checks <- if (isTRUE(getOption("spades.moduleCodeChecks"))) {
    list(suppressParamUnused = FALSE, suppressUndefined = TRUE,
         suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
         skipWith = TRUE)
  } else {
      getOption("spades.moduleCodeChecks")
  }

  aa <- capture.output(
    do.call(checkUsageEnv, args = append(list(env = sim@.envir[[m]]), checks))
  )
  aa <- grep(aa, pattern = "doEvent.*: parameter", invert = TRUE, value = TRUE)

  if (length(aa)) {
    hadParseMessage <- .parseMessage(m, "", message(paste(aa, collapse = "\n")))
  }

  # search for all sim$xx <-  or sim[[xxx]] <- in module code
  findSimAssigns <- .findAllSims(environment(),
                                  sim@.envir[[m]], type = "assign")

  inputObjNames <- sim@depends@dependencies[[k]]@inputObjects$objectName
  outputObjNames <- sim@depends@dependencies[[k]]@outputObjects$objectName

  if (length(findSimAssigns)) {
    missingFrmMod <- outputObjNames[!(outputObjNames %in% findSimAssigns)]
    missingFrmMod <- unique(na.omit(missingFrmMod))
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code", paste0(paste(missingFrmMod, collapse = ", "),
          " ",verb," declared in outputObjects, ", "but ", verb, " not used in the module"
      ))
    }

    # does module name occur as a sim$ assign
    if (m %in% findSimAssigns) {
      stop(m, ": You have created an object with the same name as the module. ",
           "Currently, this is not allowed.", call. = FALSE)
    }

    simAssignsNotInIO <- findSimAssigns[names(findSimAssigns) != ".inputObjects"]
    simAssignsInIO <- findSimAssigns[names(findSimAssigns) == ".inputObjects"]

    # First do all but .inputObjects, i.e,. outputObjects
    missingInMetadata <- simAssignsNotInIO[!(simAssignsNotInIO %in% outputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <- .parseMessage(m, "outputObjects", paste0(paste(missingInMetadata, collapse = ", "),
        " ",verb," assigned to sim inside ", paste(unique(names(missingInMetadata)), collapse = ", "),
        ", but ",verb," not declared in outputObjects"
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simAssignsInIO[!(simAssignsInIO %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <- .parseMessage(m, "inputObjects", paste0(paste(missingInMetadata, collapse = ", "),
                                               " ",verb," assigned to sim inside ", paste(unique(names(missingInMetadata)), collapse = ", "),
                                               ", but ",verb," not declared in inputObjects"
      ))
    }
  }

  # search for all '<- sim$' or '<- sim[[xxx]]' in module code
  findSimGets <- .findAllSims(environment(),
                                sim@.envir[[m]], type = "get")
  # compare to inputObjNames -- this is about inputs
  if (length(findSimGets)) {
    missingFrmMod <- inputObjNames[!(inputObjNames %in% findSimGets)]
    missingFrmMod <- unique(na.omit(missingFrmMod))
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code",
                    paste0(paste(missingFrmMod, collapse = ", "), " ",verb,
                           " declared in inputObjects, ", "but ", verb, " not used in the module"
      ))
    }

    simGetsNotInIO <- findSimGets[names(findSimGets) != ".inputObjects"]
    simGetsInIO <- findSimGets[names(findSimGets) == ".inputObjects"]

    missingInMetadata <- simGetsNotInIO[!(simGetsNotInIO %in% c(inputObjNames, outputObjNames))]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <- .parseMessage(m, "inputObjects", paste0(paste(unique(missingInMetadata), collapse = ", "),
                                              " ",verb," assigned to sim inside ", paste(unique(names(missingInMetadata)), collapse = ", "),
                                              ", but ",verb," not declared in inputObjects"
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simGetsInIO[!(simGetsInIO %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <- .parseMessage(m, "inputObjects", paste0(paste(missingInMetadata, collapse = ", "),
                                              " ",verb," assigned to sim inside ", paste(unique(names(missingInMetadata)), collapse = ", "),
                                              ", but ",verb," not declared in inputObjects"
      ))
    }

  }

  # search for conflicts in function names with common problems
  conflictingFnsByElement <- lapply(sim@.envir[[m]], function(x) {
    if (is.function(x)) {
      fg <- findGlobals(x)
      conflictingFnsSimple %in% fg
    }
  })
  hasConflicts <- unlist(lapply(conflictingFnsByElement, any))
  if (any(hasConflicts)) {
    theFns <- names(hasConflicts)[hasConflicts]
    names(theFns) <- theFns
    conflictingFnsByConflict <- lapply(theFns, function(x) {
      fg <- findGlobals(get(x, sim@.envir[[m]]))
      problemFns <- fg[fg %in% conflictingFnsSimple]
    })

    whichFnsWithPackage <- unlist(lapply(conflictingFnsByElement[hasConflicts],
                                         function(z) conflictingFnsClean[z]))
    xx <- paste0(paste(conflictingFnsByConflict, sep = ", "),
                 ": used inside ", paste(names(conflictingFnsByConflict), sep = ", "))
    verb <- .verb(xx)
    hadParseMessage <- .parseMessage(m, paste0("the following function(s) ", verb," used that conflict(s)",
            " with base functions:\n", xx,
            "\nIt is a good idea to be explicit about the package sources",
            "\ne.g., ", paste(whichFnsWithPackage, collapse = ", ")))
  }

  # search for conflicts in module function names with common problems
  clashingFuns <- names(sim@.envir[[m]])[names(sim@.envir[[m]]) %in% clashingFnsSimple]
  if (length(clashingFuns)) {
    fnNames <- clashingFnsClean[clashingFnsSimple %in% names(sim@.envir[[m]])]
    verb <- .verb(clashingFuns)
    hadParseMessage <- .parseMessage(m, paste0(
            paste(clashingFuns, collapse = ", "), " ", verb,
            " defined, which ", verb, " in conflict with ",
            paste(fnNames, collapse = ", "), ". It is recommended to ",
            " use non-conflicting function names"))
  }
  if (!hadParseMessage) message(crayon::magenta("Module code appears clean"))
  return(invisible())
}

#' Prepend module name to a message
#'
#' Also makes it blue.
#'
#' @param m module name
#' @param message rest of message
#'
#' @return
#' A message with that starts with paste0(m, ":", message)
#'
#' @keywords internal
#' @rdname parseMessage
.parseMessage <- function(m, problem, message) {
  message(crayon::magenta(
    paste0(m, " -- ", problem, ": ", message)
  ))
  return(TRUE)
}

#' Chose verb conjugation for "to be"
#'
#' @param item The item to accord conjugation with. If length 1, then "is"
#' else "are"
#'
#' @return
#' "is" or "are"
#'
#' @keywords internal
#' @rdname verb
.verb <- function(item) {
  c("is", "are")[1 + as.numeric(length(item)>1)]
}

