utils::globalVariables(c("objName", "V1"))


#' Assess whether an object has or will be supplied from elsewhere
#'
#' When loading objects into a `simList`, especially during the
#' `simInit` call, and inside the `.inputObjects` functions of modules,
#' it is often useful to know if an object in question will or has been
#' by the user via the `inputs` or `objects` arguments, or by another
#' module's `.inputObjects` while preparing its expected inputs (via
#' `expectsInputs` in metadata), or if it will be supplied by another
#' module during its `"init"` event. In all these cases, it may not
#' be necessary for a given module to load any default value for its `expectsInputs`.
#' This function can be used as a check to determine whether the module needs
#' to proceed in getting and assigning its default value.
#'
#' @details
#'
#' `where` indicates which of three places to search, either `"sim"` i.e.,
#' the `simList`, which would be equivalent to `is.null(sim\$objName)`, or
#' `"user"` which would be supplied by the user in the `simInit` function
#' call via `outputs` or `inputs` (equivalent to
#' `(!('defaultColor' \%in\% sim$.userSuppliedObjNames))`),
#' or `"initEvent"`, which would test whether a module that gets loaded **before**
#' the present one **will** create it as part of its outputs (i.e., as indicated by
#' `createsOutputs` in that module's metadata). There is a caveat to this test,
#' however; if that other event also has the object as an `expectsInput`, then
#' it would fail this test, as it *also* needs it as an input.
#' This final one (`"initEvent"`) does not explicitly test that the object will be created
#' in the "init" event, only that it is in the outputs of that module, and that it is a module
#' that is loaded prior to this one.
#'
#' @param object Character vector
#' @param sim A `simList` in which to evaluated whether the object is supplied elsewhere
#' @param where Character vector with one to three of `"sim"`, `"user"`, or `"initEvent"`.
#'        Default is all three. Partial matching is used. See details.
#' @param returnWhere Logical, default `FALSE`, whether the vector of length
#'   3 logical should be returned, or a logical of length one
#'
#' @return logical
#' @export
#'
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
#' \donttest{
#' # Example with prepInputs
#' # Put chunks like this in your .inputObjects
#' if (!suppliedElsewhere("test", mySim))
#'   sim$test <- Cache(prepInputs, "raster.tif", "downloadedArchive.zip",
#'                     destinationPath = dataPath(sim), studyArea = sim$studyArea,
#'                     rasterToMatch = sim$otherRasterTemplate, overwrite = TRUE)
#' }
suppliedElsewhere <- function(object, sim, where = c("sim", "user", "initEvent"),
                              returnWhere = FALSE) {
  mc <- as.list(match.call())[-1] # there is something weird about the argument "where"
                     # on my windows system -- shows something similar to sys.calls()
  forms <- formals()
  forms[names(mc)] <- mc
  partialMatching <- c("s", "i", "u")
  forms$where <- partialMatching[which(!is.na(pmatch(partialMatching, forms$where)))]
  if (length(forms$where) == 0) stop("where must be either sim, user or initEvent")
  objDeparsed <- substitute(object)
  if (missing(sim)) {
    theCall <- as.call(parse(text = deparse(objDeparsed)))
    objDeparsedIfHasSim <- .parsingSim(theCall[[1]], "assign")
    if (length(objDeparsedIfHasSim)) {
      objDeparsed <- objDeparsedIfHasSim
    }
    env <- parent.frame()
    isSimList <- unlist(lapply(theCall[[1]], function(x) {
      isTRUE(try(is(eval(x, envir = env), "simList"), silent = TRUE))
    }))
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
  inPrevDotInputObjects <- if ("s" %in% forms$where) {
    out <- match(objDeparsed, names(sim@.xData), nomatch = 0L) > 0L
    # check not in because it is just declared as a objectSynonym
    if (isTRUE(out)) {
      if (!is.null(sim$objectSynonyms)) {
        if (is.null(sim[[objDeparsed]]) && (objDeparsed %in% unlist(sim$objectSynonyms)))
          out <- FALSE
      }
    }
    out
  } else {
    FALSE
  }
  # Equivalent to !(names(sim) %in% sim$.userSuppliedObjNames)
  inUserSupplied <- if ("u" %in% forms$where) {
    objDeparsed %in% sim$.userSuppliedObjNames
  } else {
    FALSE
  }

  # If one of the modules that has already been loaded has this object as an output,
  #   then don't create this
  inFutureInit <- if ("i" %in% forms$where) {
    # The next line is subtle -- it must be provided by another module, previously loaded (thus in the depsEdgeList),
    #   but that does not need it itself. If it needed it itself, then it would have loaded it already in the simList
    #   which is checked in a different test of suppliedElsewhere -- i.e., "sim"
    isTRUE(depsEdgeList(sim, plot = FALSE)[!(from %in% c("_INPUT_", currentModule(sim))), ][
      objName == objDeparsed][, all(from != to), by = from][V1 == TRUE]$V1)
  } else {
    FALSE
  }

  out <- if (isTRUE(returnWhere)) {
    c(userSupplied = inUserSupplied, prevDotInputObjects = inPrevDotInputObjects,
             inFutureInit = inFutureInit)
  } else {
    (inUserSupplied | inPrevDotInputObjects | inFutureInit)
  }
  return(out)
}
