#' Check for existence of object(s) referenced by a `objects` slot of a
#' `simList` object
#'
#' Check that a named object exists in the provide `simList` environment slot,
#' and optionally has desired attributes.
#'
#' @param sim     A [simList()] object.
#'
#' @param name    A character string specifying the name of an object to be checked.
#'
#' @param object  An object. This is mostly used internally, or with layer,
#'                  because it will fail if the object does not exist.
#'
#' @param layer   Character string, specifying a layer name in a Raster, if the
#'                `name` is a `Raster*` object.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return Invisibly return `TRUE` indicating object exists; `FALSE` if not.
#'
#' @seealso [library()].
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @examples
#' sim <- simInit()
#' sim$a <- 1
#' sim$b <- list(d = 1)
#' sim$r <- terra::rast(terra::ext(0,2,0,2), res = 1, vals = 2)
#' sim$s <- c(sim$r, terra::rast(terra::ext(0,2,0,2), res = 1, vals = 3))
#' names(sim$s) <- c("r1", "r2") # give layer names
#' (checkObject(sim, name = "a")) # TRUE
#' (checkObject(sim, name = "b", layer = "d")) # TRUE
#' (checkObject(sim, name = "d")) # FALSE
#' (checkObject(sim, name = "r")) # TRUE
#' (checkObject(sim, object = sim$s)) # TRUE
#' (checkObject(sim, object = sim$s, layer = "r1")) # TRUE
#'
#' @importFrom quickPlot .objectNames
#' @include simList-class.R
#' @rdname checkObject
setGeneric("checkObject", function(sim, name, object, layer, ...) {
  standardGeneric("checkObject")
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", object = "ANY"),
  definition = function(sim, object, layer, ...) {
    ret <- TRUE # set default
    debug <- getDebug() # from options first, then override if in a simInitAndSpades
    if  (is.call(debug))
      debug <- eval(debug)
    verbose <- debugToVerbose(debug)

    # if (exists(deparse(substitute(object)), envir = sim@.xData)) { # can't use sim@.xData because it has parent of emptyenv
    obj <- tryCatch(
      eval(parse(text = deparse(substitute(object))),
           envir = parent.frame()), # envir needs to be inside the function
      silent = TRUE, error = function(x) FALSE)
    if (!isFALSE(obj) && !is.null(obj)) {
      if (!missing(layer)) {
        if (is.na(match(layer, names(object)))) {
          messageVerbose(paste(deparse(substitute(object, env = sim@.xData)),
                        "exists, but", layer, "is not a layer"), verbose = verbose)
          ret <- FALSE
        }
      }

    } else {
      messageVerbose(paste(deparse(substitute(object, env = sim@.xData)),
                    "does not exist."), verbose = verbose)
      ret <- FALSE
    }
    return(invisible(ret))
})



#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "simList", name = "character", object = "missing"),
  definition = function(sim, name, layer, ...) {
    object <- get0(name, envir = sim@.xData)
    ret <- checkObject(sim, object = object, layer = layer, ...)
    return(invisible(ret))
})

#' @export
#' @rdname checkObject
setMethod(
  "checkObject",
  signature(sim = "missing"),
  definition = function(name, ...) {
    stop(paste("Must provide a simList object"))
    return(FALSE)
})

#' Check use and existence of parameters passed to simulation.
#'
#' Checks that all parameters passed are used in a module,
#' and that all parameters used in a module are passed.
#'
#' @param sim    A `simList` simulation object.
#'
#' @param coreParams List of default core parameters.
#'
#' @param ...    Additional arguments. Not implemented.
#'
#' @return  Invisibly return `TRUE` indicating object exists; `FALSE` if not.
#'          Sensible messages are produced identifying missing parameters.
#'
#' @include simList-class.R
#' @export
#' @rdname checkParams
#'
#' @author Alex Chubaty
#'
setGeneric("checkParams", function(sim, coreParams, ...) {
  standardGeneric("checkParams")
})

#' @rdname checkParams
setMethod(
  "checkParams",
  signature(sim = "simList", coreParams = "list"),
  definition = function(sim, coreParams, ...) {
    params <- sim@params
    userModules <- modules(sim) # already removes core modules
    globalParams <- sim@params$.globals
    allFound <- TRUE

    debug <- getDebug() # from options first, then override if in a simInitAndSpades
    if  (is.call(debug))
      debug <- eval(debug)
    verbose <- debugToVerbose(debug)

    if (length(userModules)) {
      ### check whether each param in simInit occurs in a module's .R file
      globalsFound <- list()
      readFile <- list()
      userModulePaths <- names(userModules)

      for (uMP in userModulePaths) {
        uM <- basename(uMP)
        filename <- paste(uMP, "/", uM, ".R", sep = "")
        readFile[[uM]] <- if (!is.null(sim@.xData[[".parsedFiles"]][[filename]])) {
          # a little faster to use already parsed objects --
          #   might have happened earlier during simInit,
          #   if this checkParams was run from simInit
          tmp <- .parseConditional(envir = sim@.xData[[".parsedFiles"]],
                                   filename = filename)
          deparse(tmp$parsedFile)
        } else {
          readLines(filename)
        }


        # check global params
        #if (length(globalParams) > 0) {
        for (i in seq(globalParams)) {
          gP <- names(globalParams[i])
          result <- grep(gP, readFile[[uM]], value = FALSE, fixed = TRUE)
          if (length(result) > 0) {
            globalsFound <- append(globalsFound, gP)
          }
        }
        #}

        # check user params
        userParams <- params[[uM]][-which(names(params[[uM]]) %in% coreParams)]
        anyKnown <- names(userParams) %in% .knownDotParams
        if (any(anyKnown %in% TRUE)) {
          userParams <- userParams[!anyKnown]
        }
        collapsedSrc <- paste(readFile[[uM]], collapse = "")
        isInCode <- sapply(names(userParams), function(pp) grepl(pp, collapsedSrc, fixed = TRUE))
        if (any(!isInCode)) {
          allFound <- FALSE
          lapply(names(userParams)[!isInCode], function(uP) {
            messageVerbose(paste("Parameter", uP, "is not used in module", uM), verbose = verbose)
          })
        }
      }

      globalsFound <- unique(globalsFound)
      notFound <- setdiff(names(globalParams), globalsFound)
      if (length(notFound) > 0) {
        allFound <- FALSE
        messageVerbose("Global parameter(s) not used in any module: ",
                paste(notFound, collapse = ", "), ".", verbose = verbose)
      }

      ### check whether each param in a module's .R file occurs in simInit
      globalsFound <- list()
      for (uM in userModules) {
        # read in and cleanup/isolate the global params in the module's .R file
        moduleParams <- grep("globals\\(sim\\)\\$", readFile[[uM]], value = TRUE) |>
          strsplit(" ") |>
          lapply(function(x) x[nzchar(x, keepNA = TRUE)]) |>
          unlist() |>
          grep("globals\\(sim\\)\\$", x = _, value = TRUE) |>
          gsub(",", "", x = _) |>
          gsub("\\)\\)", "", x = _) |>
          gsub("^.*\\(globals\\(sim\\)", "\\globals\\(sim\\)", x = _) |>
          gsub("^globals\\(sim\\)", "", x = _) |>
          gsub("\\)\\$.*", "", x = _) |>
          unique() |>
          sort() |>
          gsub("\\$", "", x = _)

        if (length(moduleParams) > 0) {
          if (length(globalParams) > 0) {
            for (i in seq_along(moduleParams)) {
              mP <- moduleParams[i]
              if (mP %in% names(globalParams)) {
                globalsFound <- append(globalsFound, mP)
              }
            }
          }
        }

        # read in and cleanup/isolate the user params in the module's .R file
        moduleParams <- grep(paste0("params\\(sim\\)\\$", uM, "\\$"), readFile[[uM]],
                             value = TRUE) |>
          gsub(paste0("^.*params\\(sim\\)\\$", uM, "\\$"), "", x = _) |>
          gsub("[!\"#$%&\'()*+,/:;<=>?@[\\^`{|}~-].*$", "", x = _) |>
          gsub("]*", "", x = _) |>
          gsub(" *", "", x = _) |>
          unique() |>
          sort()

        if (length(moduleParams) > 0) {
          # which params does the user supply to simInit?
          userParams <- sort(unlist(names(params[[uM]])))
          if (length(userParams) > 0) {
            for (i in seq_along(moduleParams)) {
              mP <- moduleParams[i]
              if (!(mP %in% userParams)) {
                allFound <- FALSE
                messageVerbose(paste("Parameter", mP, "is not supplied to module",
                              uM, "during simInit"), verbose = verbose)
              }
            }
          }
        }

        globalsFound <- unique(globalsFound)
        notFound <- setdiff(globalsFound, names(globalParams))
        if (length(notFound) > 0) {
          allFound <- FALSE
          messageVerbose(paste(
            "The following global parameters are used in module", uM,
            "but not supplied to simInit in .globals:", unlist(notFound)
          ), verbose = verbose)
        }
      }
    } else {
      allFound <- FALSE
    }
    return(invisible(allFound))
})
