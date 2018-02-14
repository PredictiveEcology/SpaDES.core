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

# module functions that must end with return(sim) or the like
mustBeReturnSim <- c("doEvent\\..*")
mustAssignToSim <- c("scheduleEvent", "saveFiles")

allCleanMessage <- "module code appears clean"
#' Find all references to sim$
#'
#' @param envToFindSim An environment where sim is defined. This is used when
#'                     the element accessing the simList is actually a call, e.g.,
#'                     \code{sim[[P(sim)$stackName]]}
#' @param moduleEnv The environment where the module functions are
#' @param type Either "get", "assign", or "globals". See details.
#'
#'
#' @details
#' \code{.findElementsInEnv} is a wrapper around \code{.findElements}. It will convert
#' function code to a call, and then pass it to \code{.findElements}. It also does
#' some cleaning for duplications, \code{NA} values, and cases where the element
#' inside a \code{sim[["xxx"]]} is a variable that should be evaluated, rather than
#' simply taken verbatim (e.g., \code{sim[[P(sim)$stackName]])}.
#'
#' When \bold{\code{type = "get"}}, the function scans for sim$xxx or sim[["xxx"]]] on
#' the RHS of an assignment operator or when there is no assignment. When
#' \bold{\code{type = "assign"}}, the function scans for sim$xxx or sim[['xxx']] on the
#' LHS of an assignment operator. When \bold{\code{type = "globals"}}, the function
#' scans for all functions (i.e., "globals") being used. This is similar to
#' \code{\link[codetools]{findGlobals}}, but faster.
#'
#' @return
#' A character string with all sim objects found
#'
#' @author Eliot McIntire
#' @keywords internal
#' @rdname findElements
.findElementsInEnv <- function(envToFindSim = parent.frame(), moduleEnv = parent.frame(),
                               type) {
  out <- unlist(unique(lapply(names(moduleEnv), function(x) {
    if (is.function(moduleEnv[[x]])) {
      xAsString <- deparse(moduleEnv[[x]])
      xAsCall <- as.call(parse(text = xAsString))
      if (identical(type, "returnSim")) {
        if (grepl(x, pattern = mustBeReturnSim)) {
          # only pull out last line
          # must end with sim or return(sim) or return(invisible(sim))
          xAsString <- xAsString[length(xAsString)-1]
          xAsCall <- as.call(parse(text = xAsString))
        } else {
          xAsCall <- ""
        }
      }
      y <- .findElement(xAsCall, type = type)
      if (length(y)) {
        y <- na.omit(y)
        if (all(is.na(y))) y <- character()
        hasSim <- grepl(y, pattern = "sim")
        isSim <- grepl(y, pattern = "^sim$")
        if (length(y[hasSim & !isSim])) {
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
  if (is.null(out)) out <- character()
  return(out)
}

#' @keywords internal
#' @param x A call in which to search for sim
#' @inheritParams .findElementsInEnv
#' @details
#' \code{.findElement} will omit whatever it finds inside a \code{is.null}, when
#' \code{type = "assign"}. Usually this is a test of existence of that object, in
#' order to assign to that object. It is only reading it to determine whether or
#' not it should write to it.
#'
#' @rdname findElements
.findElement <- function(x, type) {
  if (is.atomic(x)) {
    character()
  } else if (is.name(x)) {
    if (identical(type, "returnSim")) { # This is intended for only the last line of a function
      out <- as.character(x)
    } else {
      character()
    }
  } else if (is.call(x)) {
    if (identical(type, "assign")) {
      if (identical(x[[1]], quote(`<-`))) {
        # if it is an assign, only keep left hand side
        x <- x[[2]]
        out <- .parsingSim(x, type = type)
      } else {
        out <- character()
      }

      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    } else if (identical(type, "assignToSim")) {
      if (identical(x[[1]], quote(`<-`)) && (any(grepl(x, pattern = paste(mustAssignToSim, collapse="|"))))) {
        if (identical(x[[2]], quote(sim))) {
          out <- character()
          x <- "" # clear x so it doesn't go any further in these cases
        } else {
          out <- character()
        }
      } else if (is.name(x[[1]]) & (any(grepl(x[[1]], pattern = paste(mustAssignToSim, collapse="|"))))) {
        out <- as.character(x[[1]])
        x <- "" # clear x so it doesn't go any further in these cases
      } else {
        out <- character()
      }
      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    } else if (identical(type, "get")) {
      if (identical(x[[1]], quote(is.null))) {
        # This is case where module may check for absense of a sim object with is.null
        #   This shouldn't make a message
        return(character())
      } else if (identical(x[[1]], quote(`<-`)) ) {
        if (length(x[[2]]) > 1) {
          if (is.call(x[[2]][[2]])) {
            if (any(grepl(x[[2]][[2]], pattern = ".envir"))) {# i.e., sim@.envir
              assigner <- FALSE
            } else {
            assigner <- TRUE # accessor on LHS like P(sim$a) <- "hi"
            }
          } else {
            if (identical(as.character(x[[2]])[1], "[") | identical(as.character(x[[2]])[1], "[[")) {
              assigner <- TRUE
            } else {
              assigner <- FALSE
            }
          }
        } else {
          assigner <- FALSE
        }
        if (!assigner) {
          x <- x[-(1:2)]
        }
        out <- character()
        #if (is.call(x[[2]][[2]]))
        # if on the left side of a function, deleted those from x, we don't care here
      } else {
        out <- .parsingSim(x, type = type)
      }
      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    } else if (identical(type, "globals")) {
      if (identical(x[[1]], quote(`<-`)) && is.call(x[[2]])) { # left side function assign

        # This labels it with an assignment arrow -- e.g., levels<-
        out <- paste0(as.character(x[[2]][[1]]), as.character(x)[[1]])
        if (length(x[[2]]) > 2) {
          x[[2]] <- x[[2]][-(1:2)]
        } else {
          x[[2]] <- x[[2]][[2]]
        }
      } else if (is.name(x[[1]])) {
        out <- as.character(x[[1]])
      } else if (any(as.character(x[[1]]) %in% conflictingFnsSimple)) {
        tmp <- deparse(x[[1]])
        if (tmp %in% conflictingFnsClean) {
          out <- tmp
          x <- ""
        } else {
          out <- character()
        }
      } else {
        out <- character()
      }
      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    } else { # all other cases, just return empty
      out <- character()
      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    }
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, .findElement, type = type)))
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
  hadParseMessage <- FALSE # initiate this which will be changed if there are
                           #   parse messages

  inputObjNames <- na.omit(sim@depends@dependencies[[k]]@inputObjects$objectName)
  outputObjNames <- na.omit(sim@depends@dependencies[[k]]@outputObjects$objectName)
  # search for all sim$xx <-  or sim[[xxx]] <- in module code
  simAssigns <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "assign")
  simAssignsInDotInputObjects <- simAssigns[names(simAssigns)==".inputObjects"]
  simAssignsNotInDotInputObjects <- simAssigns[names(simAssigns)!=".inputObjects"]
  # search for all '<- sim$' or '<- sim[[xxx]]' in module code
  simGets <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "get")
  simGetsInDotInputObjects <- simGets[names(simGets)==".inputObjects"]
  simGetsNotInDotInputObjects <- simGets[names(simGets)!=".inputObjects"]

  #############################################################
  ###### Key fns return sim ###################################
  #############################################################
  returnsSim <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "returnSim")
  returnsSim <- tapply(returnsSim, names(returnsSim), function(xx) any(xx == "sim"))
  if (!all(returnsSim)) {
    verb <- .verb(returnsSim)
    hadParseMessage <- .parseMessage(m, "module code",
                                     paste0(paste(names(returnsSim), collapse = ", "),
                                            " must return the sim, e.g., return(invisible(sim))"
                                     ))

  }

  #############################################################
  ###### Key fns outputs get assigned to sim ##################
  #############################################################
  assignToSim <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "assignToSim")
  if (length(assignToSim)) {
    verb <- .verb(assignToSim)
    hadParseMessage <- .parseMessage(m, "module code",
                                     paste0(paste(assignToSim, collapse = ", "),
                                            " inside ", paste(names(assignToSim), collapse = ", "),
                                            " must assign to the sim, e.g., sim <- scheduleEvent(sim, ...)"
                                     ))

  }

  #############################################################
  ###### Sim Assignments ######################################
  #############################################################
  # 1
  if (length(outputObjNames)) { #
    missingFrmMod <- outputObjNames[!(outputObjNames) %in% simAssignsNotInDotInputObjects]
    missingFrmMod <- unique(missingFrmMod)
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "),
                                              " ",verb," declared in outputObjects, ",
                                              "but ", verb, " not assigned in the module"
      ))
    }
  }

  # 2
  if (length(inputObjNames)) {
    # inputObjects -- Assign in .inputObjects
    missingFrmMod <- inputObjNames[!(inputObjNames %in% simAssignsInDotInputObjects)]
    missingFrmMod <- unique(missingFrmMod)
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "),
                                              " ",verb," declared in inputObjects, ",
                                              "but no default(s) ", verb, " provided in .inputObjects"
                                       ))
    }

    # inputObjects -- Gets
    missingFrmMod <- inputObjNames[!(inputObjNames %in% simGets)]
    missingFrmMod <- unique(na.omit(missingFrmMod))
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "), " ",verb,
                                              " declared in inputObjects, ", "but ", verb,
                                              " not used in the module"
                                       ))
    }
  }

  #############################################################
  ##### checkUsage -- From codetools ##########################
  #############################################################
  checks <- if (isTRUE(getOption("spades.moduleCodeChecks"))) {
    list(suppressParamUnused = FALSE, suppressUndefined = TRUE,
         suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
         skipWith = TRUE)
  } else {
    getOption("spades.moduleCodeChecks")
  }

  #browser()
  checkUsageMsg <- capture.output(
    do.call(checkUsageEnv, args = append(list(env = sim@.envir[[m]]), checks))
  )
  checkUsageMsg <- grep(checkUsageMsg, pattern = "doEvent.*: parameter",
                        invert = TRUE, value = TRUE)

  if (length(checkUsageMsg)) {
    hadParseMessage <- .parseMessage(m, "module code",
                                     paste0("\n  ", paste(checkUsageMsg, collapse = "\n  ")))
  }



  #############################################################
  #######  Conflicting Functions ##############################
  #############################################################
  # search for conflicts in function names with common problems like raster::levels
  fg <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "globals")
  hasConflicts <- fg[fg %in% conflictingFnsSimple]
  if (length(hasConflicts)) {
    theFns <- names(hasConflicts)
    names(theFns) <- theFns
    whichFnsWithPackage <- conflictingFnsClean[conflictingFnsSimple %in% hasConflicts]
    verb <- .verb(length(whichFnsWithPackage))
    hadParseMessage <-
      .parseMessage(m, "module code", paste0("the following function(s) ", verb,
                                             " used that conflict(s)",
                                             "\n  with base functions: ", crayon::bold(paste(hasConflicts, collapse = ", ")),
                                             "\n  It is a good idea to be explicit about the package sources",
                                             ", e.g., ", paste(whichFnsWithPackage, collapse = ", ")))
  }

  if (length(simAssigns)) {

    # does module name occur as a sim$ assign
    if (m %in% simAssigns) {
      stop(m, ": You have created an object with the same name as the module. ",
           "Currently, this is not allowed.", call. = FALSE)
    }

    # First do all but .inputObjects, i.e,. outputObjects
    missingInMetadata <- simAssignsNotInDotInputObjects[!(simAssignsNotInDotInputObjects %in% outputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <- .parseMessage(m, "outputObjects",
        paste0(paste(missingInMetadata, collapse = ", "),
          " ",verb," assigned to sim inside ",
          paste(unique(names(missingInMetadata)), collapse = ", "),
          ", but ",verb," not declared in outputObjects"
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simAssignsInDotInputObjects[!(simAssignsInDotInputObjects %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <-
        .parseMessage(m, "inputObjects",
                      paste0(paste(missingInMetadata, collapse = ", "),
                             " ",verb," assigned to sim inside ",
                             paste(unique(names(missingInMetadata)), collapse = ", "),
                             ", but ",verb," not declared in inputObjects"
                      ))
    }
  }

  #############################################################
  ###### Sim Gets #############################################
  #############################################################
  # compare to inputObjNames -- this is about inputs
  if (length(simGets)) {
    missingInMetadata <- simGetsNotInDotInputObjects[!(simGetsNotInDotInputObjects %in%
                                            c(inputObjNames, outputObjNames))]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <-
        .parseMessage(m, "inputObjects",
                      paste0(paste(unique(missingInMetadata), collapse = ", "),
                      " ",verb," used from sim inside ",
                      paste(unique(names(missingInMetadata)), collapse = ", "),
                      ", but ",verb," not declared in inputObjects"
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simGetsInDotInputObjects[!(simGetsInDotInputObjects %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <-
        .parseMessage(m, "inputObjects", paste0(paste(missingInMetadata, collapse = ", "),
        " ",verb," used from sim inside ",
        paste(unique(names(missingInMetadata)), collapse = ", "),
        ", but ",verb," not declared in inputObjects"
      ))
    }

  }


  # search for conflicts in module function names with common problems, like quickPlot::Plot
  clashingFuns <- names(sim@.envir[[m]])[names(sim@.envir[[m]]) %in% clashingFnsSimple]
  if (length(clashingFuns)) {
    fnNames <- clashingFnsClean[clashingFnsSimple %in% names(sim@.envir[[m]])]
    verb <- .verb(clashingFuns)
    hadParseMessage <- .parseMessage(m, "module functions", paste0(
            paste(clashingFuns, collapse = ", "), " ", verb,
            " defined, which ", verb, " in conflict with ",
            paste(fnNames, collapse = ", "), ". It is recommended to ",
            " use non-conflicting function names"))
  }

  #############################################################
  ###### Message if all clean #################################
  #############################################################
  if (!hadParseMessage) .parseMessage(m, "", allCleanMessage)
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
  sub <- if (nzchar(problem)) {
    paste0(" -- ", problem)
  } else {
    ""
  }

  message <- if (nzchar(message)) {
    paste0(": ", message)
  } else {
    ""
  }

  message(crayon::magenta(
    paste0(m, sub, message)
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

#' \code{.parsingSim} will pull out the various ways to use sim, e.g.,
#' \code{sim$xxx}, \code{sim[['xxx']]}, \code{sim[[P(sim)$xxx]]}
#' @keywords internal
#' @inheritParams .findElements
#' @rdname findElements
.parsingSim <- function(x, type) {
  if (as.character(x)[1] %in% c("$") &&
      grepl(deparse(x[[2]]), "sim|sim@.envir") && is.name(x[[3]])) {
    out <- as.character(x[[3]])
  } else if (as.character(x)[1] %in% "[[" &&
             grepl(deparse(x[[2]]), "sim|sim@.envir") && is.character(x[[3]])) {
    out <- x[[3]]
  } else if (as.character(x)[1] %in% "[[" &&
             grepl(deparse(x[[2]]), "sim|sim@.envir") && is.call(x[[3]])) {
    out <- deparse(x[[3]])
  } else {
    if (type == "assign") {
      if (is.name(x)) {
        out <- character()
      } else if (identical(as.character(x)[1], "[") | identical(as.character(x)[1], "[[")) {
        out <- character()
      } else {
        out <- .findElement(x, type = "get")
      }
    } else {
      out <- character()
    }
  }
  return(out)
}
