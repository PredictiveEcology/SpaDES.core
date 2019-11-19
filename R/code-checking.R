if (getRversion() >= "3.1.0") {
  utils::globalVariables(".")
}

# These are known functions that will definitely cause conflicts unless they are
# prefixed by their packages.
conflictingFns <- c("\\<raster::levels\\>", "\\<raster::scale\\>", "\\<raster::which.max\\>")
clashingFns <- c("\\<quickPlot::Plot\\>")
mustBeReturnSim <- c("doEvent\\..*") # module functions that must end with return(sim) or the like
mustAssignToSim <- c("scheduleEvent", "saveFiles") # module functions that must assign to sim <-
ignoreObjectsGet <- c(".userSuppliedObjNames") # objects that shouldn't return an error if "used"
ignoreObjectsAssign <- c("") # objects that shouldn't return an error if "assigned"

conflictingFnsClean <- gsub(pattern = "\\\\<", conflictingFns, replacement = "")
conflictingFnsClean <- gsub(pattern = "\\\\>", conflictingFnsClean, replacement = "")
conflictingFnsSimple <- gsub(conflictingFns, pattern = "^.*::", replacement = "\\\\<")
conflictingFnsSimple <- gsub(pattern = "\\\\<", conflictingFnsSimple, replacement = "")
conflictingFnsSimple <- gsub(pattern = "\\\\>", conflictingFnsSimple, replacement = "")

clashingFnsClean <- gsub(pattern = "\\\\<", clashingFns, replacement = "")
clashingFnsClean <- gsub(pattern = "\\\\>", clashingFnsClean, replacement = "")
clashingFnsSimple <- gsub(clashingFns, pattern = "^.*::", replacement = "\\\\<")
clashingFnsSimple <- gsub(pattern = "\\\\<", clashingFnsSimple, replacement = "")
clashingFnsSimple <- gsub(pattern = "\\\\>", clashingFnsSimple, replacement = "")

allCleanMessage <- "module code appears clean"
cantCodeCheckMessage <- ": line could not be checked "

#' Find all references to \code{sim$}
#'
#' @param envToFindSim An environment where sim is defined. This is used when
#'                     the element accessing the simList is actually a call, e.g.,
#'                     \code{sim[[P(sim)$stackName]]}
#'
#' @param moduleEnv The environment where the module functions are.
#'
#' @param type Either "get", "assign", or "globals". See details.
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
.findElementsInEnv <- function(envToFindSim = parent.frame(), moduleEnv = parent.frame(), type) {
  out <- unlist(unique(lapply(names(moduleEnv), function(x) {
    if (is.function(moduleEnv[[x]])) {

      xAsString <- deparse(body(moduleEnv[[x]]))#, backtick = TRUE, control = "all")

      if (identical(type, "returnSim")) {
        # returnSim case doesn't need to parse whole function, only last Number
        xAsCall <- .isLastLineSim(x = x, xAsString = xAsString)
        y <- .findElement(xAsCall, type = type)
      } else {
        parsedXAsString <- tryCatch(parse(text = xAsString), error = function(yy) NULL)

        # In some cases, e.g., Jean Marchal's
        if (is.null(parsedXAsString) > 0) {
          deparseBody <- deparse(body(moduleEnv[[x]]))
          bb <- deparseBody[-c(1, length(deparseBody))]
          funStarts <- grep("^    [[:alpha:]]", bb)
          bb[funStarts] <- gsub("^    ", "", bb[funStarts])
          funEnds <- funStarts - 1

          y <- lapply(seq(funStarts)[-length(funStarts)], function(yy) {
            parsedXAsString <- tryCatch(parse(text = bb[seq(funStarts[yy], funEnds[yy + 1])]),
                                        error = function(yy) NULL)
            if (!is.null(parsedXAsString)) {
              xAsCall <- as.call(parsedXAsString)
              if (identical(type, "returnSim")) {
                xAsCall <- .isLastLineSim(x = x, xAsString = bb[seq(funStarts[yy], funEnds[yy + 1])])
              }
              y <- .findElement(xAsCall, type = type)
            } else {
              #y = strsplit(bb[funStarts[yy]], split = "\\s+")[[1]][1]
              y <- paste0(cantCodeCheckMessage, "'",bb[funStarts[yy]], "'")
            }
          })
        } else {
          xAsCall <- as.call(parsedXAsString)

          if (identical(type, "returnSim")) {
            xAsCall <- .isLastLineSim(x = x, xAsString = xAsString)
          }
          y <- .findElement(xAsCall, type = type)
        }
      }
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

#' @param x A call in which to search for sim
#'
#' @details
#' \code{.findElement} will omit whatever it finds inside a \code{is.null}, when
#' \code{type = "assign"}. Usually this is a test of existence of that object, in
#' order to assign to that object. It is only reading it to determine whether or
#' not it should write to it.
#'
#' @keywords internal
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
      if (identical(x[[1]], quote(`<-`)) &&
          (any(grepl(x, pattern = paste(mustAssignToSim, collapse = "|"))))) {
        if (identical(x[[2]], quote(sim))) {
          out <- character()
          x <- "" # clear x so it doesn't go any further in these cases
        } else {
          out <- character()
        }
      } else if (is.name(x[[1]]) &
                 (any(grepl(x[[1]], pattern = paste(mustAssignToSim, collapse = "|"))))) {
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
            if (any(grepl(x[[2]][[2]], pattern = ".xData"))) {# i.e., sim@.xData
              assigner <- FALSE
            } else {
            assigner <- TRUE # accessor on LHS like P(sim$a) <- "hi"
            }
          } else {
            if (identical(as.character(x[[2]])[1], "[") |
                identical(as.character(x[[2]])[1], "[[")) {
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
      if (identical(x[[1]], quote(`<-`)) && is.call(x[[2]])) {
        # left side function assign

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
    } else {
      # all other cases, just return empty
      out <- character()
      unique(c(out, unlist(lapply(x, .findElement, type = type))))
    }
  } else if (is.pairlist(x)) {
    unique(unlist(lapply(x, .findElement, type = type)))
  } else {
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  }
}

#' Runs a series of code checks during simInit
#'
#' This uses codetools::codeCheck for function consistency, and
#' codetools::findGlobals to check for function collisions with known,
#' common function collisions (raster::stack, raster::scale)
#'
#' @param sim simList
#' @param m module name
#' @param message rest of message
#' @param hadPrevMessage
#'
#' @return
#' A message with that starts with paste0(m, ":", message)
#'
#' @keywords internal
#' @rdname runCodeChecks
.runCodeChecks <- function(sim, m, k, hadPrevMessage = FALSE) {
  inputObjNames <- na.omit(sim@depends@dependencies[[k]]@inputObjects$objectName)
  outputObjNames <- na.omit(sim@depends@dependencies[[k]]@outputObjects$objectName)

  # search for all sim$xx <-  or sim[[xxx]] <- in module code
  simAssigns <- .findElementsInEnv(environment(), sim@.xData[[m]], type = "assign")
  simAssigns <- simAssigns[!(simAssigns %in% ignoreObjectsAssign)]

  # search for all '<- sim$' or '<- sim[[xxx]]' in module code
  simGets <- .findElementsInEnv(environment(), sim@.xData[[m]], type = "get")
  simGets <- simGets[!(simGets %in% ignoreObjectsGet)]

  returnsSim <- .findElementsInEnv(environment(), sim@.xData[[m]], type = "returnSim")
  returnsSim <- tapply(returnsSim, names(returnsSim), function(xx) any(xx == "sim"))

  assignToSim <- .findElementsInEnv(environment(), sim@.xData[[m]], type = "assignToSim")

  fg <- .findElementsInEnv(environment(), sim@.xData[[m]], type = "globals")
  hasConflicts <- fg[fg %in% conflictingFnsSimple]

  # Can't code check:
  allChecks <- list(simAssigns = simAssigns, simGets = simGets,
                    returnsSim = returnsSim, assignToSim = assignToSim, fg = fg)
  cantCodeCheck <- lapply(allChecks, function(xx) grepl(cantCodeCheckMessage, xx))
  anyCantCodeCheck <- unlist(lapply(cantCodeCheck, any))
  if (any(anyCantCodeCheck)) {
    cant <- lapply(names(cantCodeCheck[anyCantCodeCheck]), function(objName) {
      localObj <- allChecks[[objName]]
      localObj[cantCodeCheck[[objName]]]
    })

    textCantCheck <- gsub(paste0(cantCodeCheckMessage, "'"), "", cant[[1]])
    textCantCheck <- gsub("'$", "", textCantCheck)

    lineNumbers <- .lineNumbersInSrcFile(sim, m, textCantCheck)

    dfCant <- data.frame(names = names(unlist(cant)), cant = unlist(cant), stringsAsFactors = FALSE)
    dfCant <- unique(dfCant)
    cant <- dfCant$cant
    names(cant) <- dfCant$names

    if (length(cant)) {
      verb <- .verb(cant)
      hadPrevMessage <- lapply(seq(cant), function(cantNamesIndex) {
        cantUnique <- cant[names(cant) == cant[cantNamesIndex]]
        .parseMessage(m, "module code",
                      paste(paste0(names(cant)[cantNamesIndex], " ",
                                   cant[cantNamesIndex],
                                   if (length(lineNumbers[[cantNamesIndex]]))
                                     paste0(" (possibly at ", lineNumbers[[cantNamesIndex]]), ")"),
                            collapse = "\n")
                      )
      })
    }

    allChecks[anyCantCodeCheck] <- lapply(names(cantCodeCheck[anyCantCodeCheck]), function(objName) {
      localObj <- allChecks[[objName]]
      localObj[!cantCodeCheck[[objName]]]
    })
    list2env(allChecks, envir = environment())
  }

  simAssignsInDotInputObjects <- simAssigns[names(simAssigns) == ".inputObjects"]
  simAssignsNotInDotInputObjects <- simAssigns[names(simAssigns) != ".inputObjects"]
  simGetsInDotInputObjects <- simGets[names(simGets) == ".inputObjects"]
  simGetsNotInDotInputObjects <- simGets[names(simGets) != ".inputObjects"]

  #############################################################
  ###### Key fns return sim ###################################
  #############################################################
  if (!all(returnsSim)) {
    verb <- .verb(returnsSim)
    hadPrevMessage <- .parseMessage(
      m, "module code",
      paste0(paste(names(returnsSim), collapse = ", "),
             " must return the sim, e.g., return(invisible(sim))")
    )
  }

  #############################################################
  ###### Key fns outputs get assigned to sim ##################
  #############################################################
  if (length(assignToSim)) {
    verb <- .verb(assignToSim)
    hadPrevMessage <- .parseMessage(
      m, "module code",
      paste0(paste(assignToSim, collapse = ", "),
             " inside ", paste(names(assignToSim), collapse = ", "),
             " must assign to the sim, e.g., sim <- scheduleEvent(sim, ...)")
    )
  }

  #############################################################
  ###### Sim Assignments ######################################
  #############################################################
  # 1
  if (length(outputObjNames)) {
    missingFrmMod <- outputObjNames[!(outputObjNames) %in% simAssignsNotInDotInputObjects]
    missingFrmMod <- unique(missingFrmMod)
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadPrevMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "),
                                              " ",verb," declared in metadata outputObjects, ",
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
      hadPrevMessage <- .parseMessage(
        m, "module code",
        paste0(paste(missingFrmMod, collapse = ", "),
               " ",verb," declared in metadata inputObjects, ",
               "but no default(s) ", verb, " provided in .inputObjects")
      )
    }

    # inputObjects -- Gets
    missingFrmMod <- inputObjNames[!(inputObjNames %in% simGets)]
    missingFrmMod <- unique(na.omit(missingFrmMod))
    if (length(missingFrmMod)) {
      verb <- .verb(missingFrmMod)
      hadPrevMessage <- .parseMessage(m, "module code",
                                       paste0(paste(missingFrmMod, collapse = ", "), " ",verb,
                                              " declared in metadata inputObjects, ", "but ", verb,
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

  checkUsageMsg <- capture.output(
    do.call(checkUsageEnv, args = append(list(env = sim@.xData[[m]]), checks))
  )
  checkUsageMsg <- grep(checkUsageMsg, pattern = "doEvent.*: parameter",
                        invert = TRUE, value = TRUE)
  if (length(checkUsageMsg)) {
    hadPrevMessage <- unique(unlist(lapply(checkUsageMsg, function(x)
      .parseMessage(m, "module code", message = x))))
  }



  #############################################################
  #######  Conflicting Functions ##############################
  #############################################################
  # search for conflicts in function names with common problems like raster::levels
  if (length(hasConflicts)) {
    theFns <- names(hasConflicts)
    names(theFns) <- theFns
    whichFnsWithPackage <- conflictingFnsClean[conflictingFnsSimple %in% hasConflicts]
    verb <- .verb(length(whichFnsWithPackage))
    hadPrevMessage <- .parseMessage(
      m, "module code",
      paste0("the following function(s) ", verb, " used that conflict(s)",
             "\n  with base functions: ", crayon::bold(paste(hasConflicts, collapse = ", ")),
             "\n  It is a good idea to be explicit about the package sources",
             ", e.g., ", paste(whichFnsWithPackage, collapse = ", "),
             " but only for the 'get' functions, not the 'set' function ","
             (e.g., don't change when on the left hand side of an assignment operator)")
    )
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
      missingInMetadataByFn <- tapply(missingInMetadata, names(missingInMetadata), unique)
      verbs <- lapply(missingInMetadataByFn, .verb)
      hadPrevMessage <- any(unlist(
        lapply(names(missingInMetadataByFn), function(fn)
          .parseMessage(
            m, "outputObjects",
            paste0(paste(missingInMetadataByFn[[fn]], collapse = ", "),
                   " ", verbs[[fn]], " assigned to sim inside ",
                   fn, ", but ", verbs[[fn]], " not declared in metadata outputObjects")
          )
        )
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simAssignsInDotInputObjects[!(simAssignsInDotInputObjects %in% inputObjNames)]
    if (length(missingInMetadata)) {
      missingInMetadataByFn <- tapply(missingInMetadata, names(missingInMetadata), unique)
      verbs <- lapply(missingInMetadataByFn, .verb)
      hadPrevMessage <- any(unlist(
        lapply(names(missingInMetadataByFn), function(fn)
          .parseMessage(
            m, "inputObjects",
            paste0(paste(missingInMetadataByFn[[fn]], collapse = ", "),
                   " ", verbs[[fn]], " assigned to sim inside ",
                   fn, ", but ", verbs[[fn]], " not declared in metadata inputObjects")
          )
        )
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
      missingInMetadataByFn <- tapply(missingInMetadata, names(missingInMetadata), unique)
      verbs <- lapply(missingInMetadataByFn, .verb)
      hadPrevMessage <- any(unlist(
        lapply(names(missingInMetadataByFn), function(fn)
          .parseMessage(
            m, "inputObjects",
            paste0(paste(missingInMetadataByFn[[fn]], collapse = ", "),
                   " ", verbs[[fn]], " used from sim inside ",
                   fn, ", but ", verbs[[fn]], " not declared in metadata inputObjects")
          )
        )
      ))
    }

    # Now do .inputObjects, i.e., inputObjects
    missingInMetadata <- simGetsInDotInputObjects[!(simGetsInDotInputObjects %in% inputObjNames)]
    if (length(missingInMetadata)) {
      verb <- .verb(missingInMetadata)
      hadPrevMessage <- .parseMessage(
        m, "inputObjects", paste0(paste(missingInMetadata, collapse = ", "),
                                  " ", verb, " used from sim inside ",
                                  paste(unique(names(missingInMetadata)), collapse = ", "),
                                  ", but ", verb, " not declared in metadata inputObjects")
      )
    }
  }

  # search for conflicts in module function names with common problems, like quickPlot::Plot
  clashingFuns <- names(sim@.xData[[m]])[names(sim@.xData[[m]]) %in% clashingFnsSimple]
  if (length(clashingFuns)) {
    fnNames <- clashingFnsClean[clashingFnsSimple %in% names(sim@.xData[[m]])]
    verb <- .verb(clashingFuns)
    hadPrevMessage <- .parseMessage(m, "module functions", paste0(
            paste(clashingFuns, collapse = ", "), " ", verb,
            " defined, which ", verb, " in conflict with ",
            paste(fnNames, collapse = ", "), ". It is recommended to ",
            " use non-conflicting function names"))
  }

  #############################################################
  ###### Message if all clean #################################
  #############################################################
  if (!hadPrevMessage) .parseMessage(m, "", allCleanMessage)
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
    paste0(": ", problem)
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
#' @param item The item to accord conjugation with. If length 1, then "is" else "are".
#'
#' @return
#' "is" or "are"
#'
#' @keywords internal
#' @rdname verb
.verb <- function(item) {
  c("is", "are")[1 + as.numeric(length(item) > 1)]
}

#' \code{.parsingSim} will pull out the various ways to use sim, e.g.,
#' \code{sim$xxx}, \code{sim[['xxx']]}, \code{sim[[P(sim)$xxx]]}
#'
#' @keywords internal
#' @rdname findElements
.parsingSim <- function(x, type) {
  if (length(x) > 1) {
    if (!is.pairlist(x[[2]])) {
      grepForSim <- grepl("^sim|^sim@.envir|^sim@.xData", deparse(x[[2]], backtick = TRUE))[1]
      if (as.character(x)[1] %in% c("$", "[[") &&
          grepForSim &&
          is.name(x[[3]])) {
        out <- as.character(x[[3]])
      } else if (as.character(x)[1] %in% "[[" &&
                 grepForSim &&
                   is.character(x[[3]])) {
        out <- x[[3]]
      } else if (as.character(x)[1] %in% "[[" &&
                 grepForSim &&
                 is.call(x[[3]])) {
        out <- deparse(x[[3]])
      }
    } else {
      out <- character()
    }
  }

  if (!exists("out", inherits = FALSE)) {
    if (type == "assign") {
      if (is.name(x)) {
        out <- character()
      } else if (identical(as.character(x)[1], "[") |
                 identical(as.character(x)[1], "[[")) {
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

.isLastLineSim <- function(x, xAsString) {
  if (grepl(x, pattern = mustBeReturnSim)) {
    # only pull out last line
    # must end with sim or return(sim) or return(invisible(sim))
    xAsString <- xAsString[length(xAsString) - 1]
    xAsCall <- as.call(parse(text = xAsString))
  } else {
    xAsCall <- ""
  }
}

.lineNumbersInSrcFile <- function(sim, module, namedTxt, pd) {
  if (!missing(sim)) {
    pd <- sim@.xData[[module]][["._parsedData"]]
  }
  lineNumbers <- lapply(seq(namedTxt), function(patternIndex) {
    patt <- gsub("\\(", "\\\\(", namedTxt[patternIndex])
    patt <- gsub("\\)", "\\\\)", patt)
    patt <- gsub("\\{", "\\\\{", patt)
    patt <- gsub("\\}", "\\\\}", patt)
    patt <- gsub("\\[", "\\\\[", patt)
    patt <- gsub("\\]", "\\\\]", patt)
    wh <- which(grepl(pattern = paste0("\\b", patt, "\\b"), pd$text) &
                  (pd$line1 == pd$line2) & (pd$token == "expr"))
    if (length(wh) == 0) {
      wh <- which(agrepl(pattern = paste0(patt), pd$text) &
                    (pd$line1 == pd$line2) & (pd$token == "expr"))
    }

    outerWh <- which(grepl(paste0("\\b", names(namedTxt)[patternIndex], "\\b"), pd$text) &
                       (pd$token == "expr"))
    linesWithFail <- unique(pd[wh, "line1"])
    #unique(pd[outerWh, "line1"])

    # Make sure they are inside the correct function
    linesWithFail <- lapply(linesWithFail, function(lwf) {
      whInner <- any((pd[outerWh,"line1"] < lwf) & (pd[outerWh,"line2"] > lwf) )
      if (isTRUE(whInner)) lwf else numeric()
    })
    if (length(linesWithFail) == length(patt))
      names(linesWithFail) <- patt
    unlist(linesWithFail)
  })
  unlist(lineNumbers)
}
