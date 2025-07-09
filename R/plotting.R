##
## NOTE: these have been copied and modified from defunct package quickPlot
##

###############################################################################
#' Parse arguments and find environments
#'
#' Internal function used within `.objectNames`.
#'
#' @param y  A character representation of the arguments passed to a function, e.g., `Plot`.
#'
#' @param e  Environment in which the function (e.g., `Plot`) was called.
#'
#' @param eminus1  The parent environment of `e`.
#'
#' @return A list of length 2, with names `objs` and `envs`, giving the standardized
#' representation (i.e., replacing `[[]]` with `$` notation for objects) of objects
#' and their layers (if `RasterStack`s).
#'
#' @author Eliot McIntire and Alex Chubaty
#' @importFrom grDevices dev.cur
#' @family internals from `quickPlot`
#' @keywords internal
#' @rdname parseArgs
.parseArgs <- function(y, e, eminus1) {
  elems <- list()
  i <- 1
  parseTxt <- parse(text = y)[[1]]
  elems[[i]] <- parseTxt
  lastOneDone <- TRUE

  while (length(parse(text = deparse(parseTxt))[[1]]) != 1) {
    if (length(parseTxt) == 2) {
      stop(
        "Please pass an object directly, or use get(x, envir = envName) or ",
        "eval(x, envir = envName). ",
        "Plot can not yet accept functions or complex objects internally."
      )
    }

    lastOneDone <- FALSE
    if (grepl(deparse(parseTxt[[1]]), pattern = "^eval")) {
      callEnv <- tryCatch(
        eval(match.call(definition = eval, call = parseTxt)$envir, envir = eminus1),
        error = function(x) {
          tryCatch(
            eval(match.call(definition = eval, call = parseTxt)$envir, envir = e),
            error = function(x) .GlobalEnv
          )
        }
      )

      parseTxt[[3]] <- match.call(definition = eval, call = parseTxt)$expr
      if (is.name(match.call(definition = parse, call = parseTxt[[3]])$text)) {
        parseTxt <- parseTxt[[3]]
        parseTxt[[3]] <- match.call(definition = parse, call = parseTxt)$text
      }
      lastOneDone <- TRUE
    }
    if (is.call(parseTxt[[3]])) {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (as.character(parseTxt[[1]]) == "[[") {
      parseTxt[[3]] <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )
    }
    if (grepl(deparse(parseTxt[[1]]), pattern = "^get")) {
      callEnv <- tryCatch(
        eval(match.call(definition = eval, call = parseTxt)$envir, envir = eminus1),
        error = function(x) {
          tryCatch(
            eval(match.call(definition = eval, call = parseTxt)$envir, envir = e),
            error = function(x) .GlobalEnv
          )
        }
      )
      parseTxt[[3]] <- match.call(definition = get, call = parseTxt)$x
      tmpParseTxt3 <- tryCatch(
        eval(parseTxt[[3]], envir = e),
        error = function(x) {
          eval(parseTxt[[3]], envir = eminus1)
        }
      )

      lastOneDone <- TRUE
      parseTxt[[3]] <- tmpParseTxt3
    }
    if (is.character(parseTxt[[3]])) {
      parseTxt[[3]] <- as.name(parseTxt[[3]])
    }
    if (is.numeric(parseTxt[[3]])) {
      if (!is.null(names(eval(parseTxt[[2]], envir = e)))) {
        parseTxt[[3]] <- names(eval(parseTxt[[2]], envir = e))[parseTxt[[3]]]
        if (is.na(parseTxt[[3]])) {
          stop(
            "Please pass an object directly, or use get(x, envir = envName) ",
            "or eval(x, envir = envName). ",
            "Plot can not yet accept functions or complex objects internally."
          )
        }
      }
    }

    ## override previous elems entry if length(parseTxt)>1:
    elems[[i]] <- parseTxt[[3]]

    ## if evaluating the parsed text is a character, then this is likely then name we want to keep:
    isChar <- tryCatch(
      is.character(eval(elems[[i]], envir = eminus1)),
      error = function(x) FALSE
    )
    if (isChar) {
      elems[[i]] <- as.name(eval(elems[[i]], envir = eminus1))
    }
    parseTxt <- parse(text = deparse(parseTxt[[2]]))[[1]]
    i <- i + 1
  }

  deparsedTxt <- deparse(parseTxt)
  sframes <- sys.frames()
  # envs <- append(.GlobalEnv, sframes) %>%
  #   .[c(TRUE, unlist(lapply(sframes, function(x) {
  #     exists(deparsedTxt, envir = x, inherits = FALSE)
  #   })))] %>%
  #   .[[length(.)]]

  envs <- append(.GlobalEnv, sframes)
  envs <- envs[c(TRUE, unlist(lapply(sframes, function(x) {
    exists(deparsedTxt, envir = x, inherits = FALSE)
  })))]
  envs <- envs[[length(envs)]]

  inGlobal <- identical(envs, .GlobalEnv)
  possEnv <- eval(parse(text = deparsedTxt), envir = envs)
  if (is.environment(possEnv)) {
    notPoss <- tryCatch(get(deparse(rev(elems)[[1]]), envir = possEnv), error = function(x) FALSE)
    if (!isFALSE(notPoss))
      envs <- possEnv
  } else {
    if (!lastOneDone) elems[[i]] <- parseTxt
  }

  if (exists("callEnv", inherits = FALSE)) {
    envs <- callEnv
  }

  if (!inGlobal) {
    devCur <- paste0("dev", dev.cur())
    if (!exists(devCur, envir = .quickPlotEnv)) {
      .quickPlotEnv[[devCur]] <- new.env(parent = emptyenv())
    }

    tmp <- get(deparse(rev(elems)[[1]]), envir = envs) ## the sim object
    .quickPlotEnv[[devCur]] <- .parseElems(tmp = tmp, elems = elems, envir = envs)
  }

  if (unlist(lapply(elems[[1]], is.numeric))) {
    return(list(
      objs = paste0(paste0(unlist(lapply(rev(elems), deparse)), collapse = "[["), "]]"),
      envs = envs
    ))
  }
  return(list(
    objs = paste(unlist(lapply(rev(elems), deparse, backtick = TRUE)), collapse = "$"),
    envs = envs
  ))
}

###############################################################################
#' Parsing of elements
#'
#' This is a generic definition that can be extended according to class.
#' Intended only for development use.
#'
#' @param tmp A evaluated object
#' @param elems A character string to be parsed
#' @param envir An environment
#'
#' @return An object, parsed from a character string and and environment
#'
#' @author Eliot McIntire
#' @family internals from `quickPlot`
#' @keywords internal
#' @rdname parseElems
setGeneric(".parseElems", function(tmp, elems, envir) {
  standardGeneric(".parseElems")
})

#' @export
#' @rdname parseElems
setMethod(
  ".parseElems",
  signature = "ANY",
  definition = function(tmp, elems, envir) {
    eval(parse(text = paste(unlist(lapply(rev(elems), deparse)), collapse = "$")), envir = envir)
  }
)

###############################################################################
#' Extract object names
#'
#' @param calledFrom  character vector of length 1, indicating which function
#'                    call is desired. Defaults to `"spades"`.
#'
#' @param argClass    character vector of length 1, indicating which class is
#'                    being searched for among the arguments.
#'                    Defaults to `"simList"`.
#'
#' @param argName     character vector of length 1, or `NULL`, indicating
#'                    if the arguments to select have a name, no name (empty
#'                    string), or do not use name (`NULL`).
#'
#' @return `NULL`. This function is invoked for its side effects.
#'
#' @author Eliot McIntire
#' @family internals from `quickPlot`
#' @keywords internal
#' @rdname objectNames
.objectNames <- function(calledFrom = "spades", argClass = "simList", argName = "") {
  scalls <- sys.calls()
  ## Extract from the sys.calls only the function "calledFrom"
  frameCalledFrom <- which(unlist(lapply(scalls, function(x) {
    grepl(x, pattern = paste0("^", calledFrom, "$"))[1]
  })))
  e <- sys.frame(frameCalledFrom[1])
  eminus1 <- sys.frame(frameCalledFrom - 1)

  if (!nzchar(argName, keepNA = TRUE)) {
    callNamedArgs <- as.character(substitute(list(...), env = e))[-1]
  } else {
    callNamedArgs <- as.character(substitute(parse(text = sim), env = e))[-1]
  }
  objs <- lapply(callNamedArgs, .parseArgs, e, eminus1)
  return(objs)
}

###############################################################################
#' Find the environment in the call stack that contains an object by name
#'
#' This is similar to `pryr::where`, except instead of working up the `search()` path
#' of packages, it searches up the call stack for an object.
#' Ostensibly similar to `base::dynGet`, but it will only return the environment,
#' not the object itself and it will try to extract just the object name from `name`,
#' even if supplied with a more complicated name
#' (e.g., if `obj$firstElement@slot1$size` is supplied, the function will only search for `obj`).
#' The function is fairly fast.
#'
#' @param name An object name to find in the call stack.
#'
#' @param whFrame A numeric indicating which `sys.frame` (by negative number) to start searching in.
#'
#' @details
#' The difference between this and what `get` and `exists` do, is that these other functions
#' search up the enclosing environments, i.e., it matters where the functions were defined.
#' `whereInStack` looks up the call stack environments. See the example for the difference.
#'
#' @return
#' The environment that is in the call stack where the object exists, that is closest to the
#' frame in which this function is called.
#'
#' @family internals from `quickPlot`
#' @keywords internal
#'
#' @examples
#' b <- 1
#' inner <- function(y) {
#'   objEnv <- SpaDES.core:::whereInStack("b")
#'   get("b", envir = objEnv)
#' }
#' findB <- function(x) {
#'   b <- 2
#'   inner()
#' }
#' findB() # Finds 2 because it is looking up the call stack, i.e., the user's perspective
#'
#' # defined outside of findB2, so its enclosing environment is the same as findB2
#' innerGet <- function(y) {
#'   get("b")
#' }
#' findB2 <- function(x) {
#'   b <- 2
#'   innerGet()
#' }
#' findB2() # Finds 1 because b has a value of 1 in the enclosing environment of innerGet
#' b <- 3
#' findB2() # Finds 3 because b has a value of 3 in the enclosing environment of innerGet,
#' #  i.e., the environment in which innerGet was defined
#' findB() # Still finds 2 because the call stack hasn't changed
#'
#' # compare base::dynGet
#' findB3 <- function(x) {
#'   b <- 2
#'   dynGet("b")
#' }
#'
#' # same as findB(), but marginally faster, because it omits the stripping on
#' # sub elements that may be part of the name argument
#' findB3()
#'
#' b <- list()
#' findB3 <- function(x) {
#'   b$a <- 2
#'   dynGet("b$a")
#' }
#' testthat::expect_error(findB3()) # fails because not an object name
#'
#' findB <- function(x) {
#'   b$a <- 2
#'   env <- SpaDES.core:::whereInStack("b$a")
#'   env
#' }
#' findB() # finds it
#'
whereInStack <- function(name, whFrame = -1) {
  pat <- "@|\\$|\\[\\["
  objectName <- if (grepl(name, pattern = pat)) {
    strsplit(name, split = pat)[[1]][1]
  } else {
    name
  }

  snframe <- sys.nframe()

  while (!(exists(objectName, envir = sys.frame(whFrame), inherits = FALSE))) {

    whFrame <- whFrame - 1
    if (snframe < (-whFrame)) {
      stop(objectName, " is not on the call stack.", call. = FALSE)
    }
  }
  ## success case
  sys.frame(whFrame)
}
