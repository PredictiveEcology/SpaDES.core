### deal with spurious httr warnings
if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("objName", "V1"))
}


#' Assess whether an object has or will be supplied from elsewhere
#'
#' When loading objects into a \code{simList}, especially during the
#' \code{simInit} call, and inside the \code{.inputObjects} functions of modules,
#' it is often useful to know if an object in question will or has been
#' by the user via the \code{inputs} or \code{objects} arguments, or by another
#' module's \code{.inputObjects} while preparing its expected inputs (via
#' \code{expectsInputs} in metadata), or if it will be supplied by another
#' module during its \code{"init"} event. In all these cases, it may not
#' be necessary for a given module to load any default value for its \code{expectsInputs}.
#' This function can be used as a check to determine whether the module needs
#' to proceed in getting and assigning its default value.
#'
#' @param object Character vector or sim object in the form sim$objName
#' @param sim A \code{simList} in which to evaluated whether the object is supplied elsewhere
#' @param where Character vector with one to three of "sim", "user", or "initEvent".
#'        Default is all three. Partial matching is used. See details.
#' @export
#'
#' @details
#'
#' \code{where} indicates which of three places to search, either \code{"sim"} i.e.,
#' the \code{simList}, which would be equivalent to \code{is.null(sim\$objName)}, or
#' \code{"user"} which would be supplied by the user in the \code{simInit} function
#' call via \code{outputs} or \code{inputs} (equivalent to
#' \code{(!('defaultColor' \%in\% sim$.userSuppliedObjNames))}),
#' or \code{"initEvent"}, which would test whether a module that gets loaded \bold{before}
#' the present one \bold{will} create it as part of its outputs (i.e., as indicated by
#' \code{createsOutputs} in that module's metadata). There is a caveat to this test,
#' however; if that other event also has the object as an \code{expectsInput}, then
#' it would fail this test, as it \emph{also} needs it as an input.
#' This final one (\code{"initEvent"})
#' does not explicitly test that the object will be created in the "init" event, only that
#' it is in the outputs of that module, and that it is a module that is loaded prior to
#' this one.
#' @examples
#' mySim <- simInit()
#' suppliedElsewhere("test", mySim) # FALSE
#'
#' # supplied in the simList
#' mySim$test <- 1
#' suppliedElsewhere("test", mySim) # TRUE
#' test <- 1
#'
#' # supplied from user at simInit time -- note, this object would eventually get into the simList
#' #   but the user supplied values come *after* the module's .inputObjects, so
#' #   a basic is.null(sim$test) would return TRUE even though the user supplied test
#' mySim <- simInit(objects = list("test" = test))
#' suppliedElsewhere("test", mySim) # TRUE
#'
#' \dontrun{
#' # Example with prepInputs
#' # Put chunks like this in your .inputObjects
#' if (!suppliedElsewhere("test", mySim))
#'   sim$test <- Cache(prepInputs, "raster.tif", "downloadedArchive.zip",
#'                     destinationPath = dataPath(sim), studyArea = sim$studyArea,
#'                     rasterToMatch = sim$otherRasterTemplate, overwrite = TRUE)
#' }
suppliedElsewhere <- function(object, sim, where = c("sim", "user", "initEvent")) {
  partialMatching <- c("s", "i", "u")
  where <- partialMatching[which(!is.na(pmatch(partialMatching, where)))]
  if (length(where) == 0) stop("where must be either sim, user or initEvent")
  objDeparsed <- substitute(object)
  if (missing(sim)) {
    theCall <- as.call(parse(text = deparse(objDeparsed)))
    objDeparsedIfHasSim <- .parsingSim(theCall[[1]], "assign")
    if (length(objDeparsedIfHasSim))
      objDeparsed <- objDeparsedIfHasSim
    env <- parent.frame()
    isSimList <- unlist(lapply(theCall[[1]], function(x)
      isTRUE(try(is(eval(x, envir = env), "simList"), silent = TRUE))))
    if (!all(isSimList)) {
      sim <- get("sim", envir = env)
    } else {
      sim <- eval(theCall[[1]][isSimList][[1]], envir = env)
    }

  }

  # if object was actually a variable of character names of objects inside sim
  objDeparsed <- tryCatch(eval(objDeparsed, parent.frame()), error = function(y) objDeparsed)

  objDeparsed <- as.character(objDeparsed)

  # Equivalent to !is.null(sim$xxx)
  inPrevDotInputObjects <- if ("s" %in% where) {
    match(objDeparsed, names(sim@.xData), nomatch = 0L) > 0L

  } else {
    FALSE
  }
  # Equivalent to !(names(sim) %in% sim$.userSuppliedObjNames)
  inUserSupplied <- if ("u" %in% where) {
    objDeparsed %in% sim$.userSuppliedObjNames
  } else {
    FALSE
  }

  # If one of the modules that has already been loaded has this object as an output,
  #   then don't create this
  inFutureInit <- if ("i" %in% where) {
    # The next line is subtle -- it must be provided by another module, previously loaded (thus in the depsEdgeList),
    #   but that does not need it itself. If it needed it itself, then it would have loaded it already in the simList
    #   which is checked in a different test of suppliedElsewhere -- i.e., "sim"
    isTRUE(depsEdgeList(sim, plot = FALSE)[!(from %in% c("_INPUT_", currentModule(sim))), ][
      objName == objDeparsed][, all(from != to), by = from][V1==TRUE]$V1)

  } else {
    FALSE
  }

  (inUserSupplied | inPrevDotInputObjects | inFutureInit)
}
