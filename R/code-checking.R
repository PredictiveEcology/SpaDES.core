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
      aa <- deparse(moduleEnv[[x]])
      bb <- as.call(parse(text = aa))
      y <- .findElement(bb, type = type)
      if (length(y)) {
        y <- na.omit(y)
        if (all(is.na(y))) y <- character()
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
#' @inheritParams .findElementsInEnv
#' @details
#' \code{.findElement} will omit whatever it finds inside a \code{is.null}, when
#' \code{type = "assign"}. Usually this is a test of existence of that object, in
#' order to assign to that object. It is only reading it to determine whether or
#' not it should write to it.
#'
#' @rdname findElements
.findElement <- function(x, type) {
  if (is.atomic(x) || is.name(x)) {
    character()
  } else if (is.call(x)) {
    if (identical(type, "assign")) {
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

      unique(c(lhs, unlist(lapply(x, .findElement, type = type))))
    } else if (identical(type, "get")) {
      if (identical(x[[1]], quote(is.null))) {
        # This is case where module may check for absense of a sim object with is.null
        #   This shouldn't make a message
        return(character())
      } else if (identical(x[[1]], quote(`<-`)) ) {
        simObj <- character()
        # if on the left side of a function, deleted those from x, we don't care here
        x <- x[-(1:2)]
      } else {
        if (as.character(x)[1] %in% c("$") &&
            identical(as.character(x[[2]]), "sim") && is.name(x[[3]])) {
          simObj <- as.character(x[[3]])
        } else if (as.character(x)[1] %in% "[[" &&
                   identical(as.character(x[[2]]), "sim") && is.character(x[[3]])) {
          simObj <- x[[3]]
        } else if (as.character(x)[1] %in% "[[" &&
                   identical(as.character(x[[2]]), "sim") && is.call(x[[3]])) {
          simObj <- deparse(x[[3]])
        } else {
          simObj <- character()
        }
      }
      unique(c(simObj, unlist(lapply(x, .findElement, type = type))))
    } else if (identical(type, "globals")) {
      if (identical(x[[1]], quote(`<-`)) && is.call(x[[2]])) { # left side function assign
        lhs <- paste0(as.character(x[[2]][[1]]), as.character(x)[[1]])
      } else if (is.name(x[[1]])) {
        lhs <- as.character(x[[1]])
      } else {
        lhs <- character()
      }
      unique(c(lhs, unlist(lapply(x, .findElement, type = type))))
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

  #############################################################
  ###### Sim Assignments ######################################
  #############################################################
  # search for all sim$xx <-  or sim[[xxx]] <- in module code
  findSimAssigns <- .findElementsInEnv(environment(), sim@.envir[[m]], type = "assign")

  inputObjNames <- sim@depends@dependencies[[k]]@inputObjects$objectName
  outputObjNames <- sim@depends@dependencies[[k]]@outputObjects$objectName

  if (length(outputObjNames)) {
    missingFrmMod <- outputObjNames[!(outputObjNames %in% findSimAssigns)]
    missingFrmMod <- unique(na.omit(missingFrmMod))
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadParseMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "),
                                              " ",verb," declared in outputObjects, ",
                                              "but ", verb, " not used in the module"
      ))
    }
  }

  if (length(findSimAssigns)) {

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
      hadParseMessage <- .parseMessage(m, "outputObjects",
        paste0(paste(missingInMetadata, collapse = ", "),
          " ",verb," assigned to sim inside ",
          paste(unique(names(missingInMetadata)), collapse = ", "),
          ", but ",verb," not declared in outputObjects"
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simAssignsInIO[!(simAssignsInIO %in% inputObjNames)]
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
  # search for all '<- sim$' or '<- sim[[xxx]]' in module code
  findSimGets <- .findElementsInEnv(environment(),
                                sim@.envir[[m]], type = "get")
  # compare to inputObjNames -- this is about inputs
  if (length(inputObjNames)) {
    missingFrmMod <- inputObjNames[!(inputObjNames %in% findSimGets)]
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

  if (length(findSimGets)) {
    simGetsNotInIO <- findSimGets[names(findSimGets) != ".inputObjects"]
    simGetsInIO <- findSimGets[names(findSimGets) == ".inputObjects"]

    missingInMetadata <- simGetsNotInIO[!(simGetsNotInIO %in%
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
    missingInMetadata <- simGetsInIO[!(simGetsInIO %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadParseMessage <-
        .parseMessage(m, "inputObjects", paste0(paste(missingInMetadata, collapse = ", "),
        " ",verb," assigned to sim inside ",
        paste(unique(names(missingInMetadata)), collapse = ", "),
        ", but ",verb," not declared in inputObjects"
      ))
    }

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
  ##### checkUsage -- From codetools ##########################
  #############################################################
  checks <- if (isTRUE(getOption("spades.moduleCodeChecks"))) {
    list(suppressParamUnused = FALSE, suppressUndefined = TRUE,
         suppressPartialMatchArgs = FALSE, suppressNoLocalFun = TRUE,
         skipWith = TRUE)
  } else {
    getOption("spades.moduleCodeChecks")
  }

  checkUsageMsg <- capture.output(
    do.call(checkUsageEnv, args = append(list(env = sim@.envir[[m]]), checks))
  )
  checkUsageMsg <- grep(checkUsageMsg, pattern = "doEvent.*: parameter",
                        invert = TRUE, value = TRUE)

  if (length(checkUsageMsg)) {
    hadParseMessage <- .parseMessage(m, "module code",
                                     paste0("\n  ", paste(checkUsageMsg, collapse = "\n")))
  }

  #############################################################
  ###### Message if all clean #################################
  #############################################################
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
  sub <- if (nzchar(problem)) {
    paste0(" -- ", problem)
  } else {
    ""
  }
  message(crayon::magenta(
    paste0(m, sub, ": ", message)
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

