# register the S3 `numeric_version` class for use with S4 methods.
setOldClass("numeric_version")
selectMethod("show", "numeric_version")

# register the S3 `person` class for use with S4 methods.
setClass(
  "person4",
  slots = list(given = "character", family = "character", middle = "character",
               email = "character", role = "character", comment = "character",
               first = "character", last = "character")
)
setOldClass("person", S4Class = "person4")
selectMethod("show", "person")
removeClass("person4")

#' Create an empty `data.frame` object for use with `inputObjects` or
#' `outputObjects`
#'
#' Internal function.
#'
#' @param x Not used. Should be missing.
#'
#' @return A `data.frame` object.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @rdname inputObjectsDF
#' @seealso [defineModule()]
setGeneric("._inputObjectsDF", function(x) {
  standardGeneric("._inputObjectsDF")
})

#' @rdname inputObjectsDF
setMethod("._inputObjectsDF",
          signature(x = "missing"),
          definition = function() {
            data.frame(
              objectName = character(0), objectClass = character(0),
              desc = character(0), sourceURL = character(0),
              stringsAsFactors = FALSE
            )
})

#' @rdname inputObjectsDF
setGeneric("._outputObjectsDF", function(x) {
  standardGeneric("._outputObjectsDF")
})

#' @rdname inputObjectsDF
setMethod("._outputObjectsDF",
          signature(x = "missing"),
          definition = function() {
            out.df <- data.frame(
              objectName = character(0), objectClass = character(0),
              desc = character(0), stringsAsFactors = FALSE
            )
            return(out.df)
})

################################################################################
#' The `.moduleDeps` class
#'
#' Descriptor object for specifying SpaDES module dependencies.
#'
#' @slot name           Name of the module as a character string.
#'
#' @slot description    Description of the module as a character string.
#'
#' @slot keywords       Character vector containing a module's keywords.
#'
#' @slot authors        The author(s) of the module as a [person()] object.
#'
#' @slot childModules   A character vector of child module names.
#'                      Modules listed here will be loaded with this module.
#'
#' @slot version        The module version as a `numeric_version`.
#'                      Semantic versioning is assumed <https://semver.org/>.
#'
#' @slot spatialExtent  Specifies the module's spatial extent as an
#'                      [Extent()] object. Default is `NA`.
#'
#' @slot timeframe      Specifies the valid timeframe for which the module was
#'                      designed to simulate. Must be a [POSIXt()]
#'                      object of length 2, specifying the start and end times
#'                      (e.g., `as.POSIXlt(c("1990-01-01 00:00:00", "2100-12-31 11:59:59"))`).
#'                      Can be specified as `NA` using `as.POSIXlt(c(NA, NA))`.
#'
#' @slot timeunit       Describes the time (in seconds) corresponding to 1.0
#'                      simulation time units. Default is `NA`.
#'
#' @slot citation       A list of citations for the module, each as character strings.
#'                      Alternatively, list of filenames of `.bib` or similar files.
#'                      Defaults to `NA_character_`.
#'
#' @slot documentation  List of filenames referring to module documentation sources.
#'
#' @slot loadOrder      An optional list of up to 2 named character vectors, named
#'                      "before" and "after". If specified, then SpaDES.core will
#'                      use this information to help disentangle ambiguous module
#'                      load order estimation. Any module that is specified
#'                      in the "before" element will have its "init" event scheduled
#'                      before this module; any module specified in the "after"
#'                      element will have its "init" event scheduled after this module.
#'
#' @slot reqdPkgs       Character vector of R package names to be loaded.
#'                      Defaults to `NA_character_`.
#'
#' @slot parameters     A `data.frame` specifying the object dependencies
#'                      of the module, with columns `paramName`,
#'                      `paramClass`, and `default`, whose values are
#'                      of type `character`, `character`, and
#'                      `ANY`, respectively. Default values may be
#'                      overridden by the user by passing a list of parameters
#'                      to [simInit()].
#'
#' @slot inputObjects   A `data.frame` specifying the object dependencies of
#'                      the module, with columns `objectName`,
#'                      `objectClass`, and `other`.
#'                      For objects that are used within the module as both an
#'                      input and an output, add the object to each of these
#'                      `data.frame`s.
#'
#' @slot outputObjects  A `data.frame` specifying the objects output by the
#'                      module, following the format of `inputObjects`.
#'
#' @aliases .moduleDeps
#' @rdname moduleDeps-class
#' @keywords internal
#'
#' @seealso `.simDeps`, [spadesClasses()]
#'
#' @author Alex Chubaty
#'
setClass(
  ".moduleDeps",
  slots = list(
    name = "character", description = "character", keywords = "character",
    childModules = "character", authors = "person", version = "numeric_version",
    spatialExtent = "ANY", timeframe = "POSIXt", timeunit = "ANY",
    citation = "list", documentation = "list", loadOrder = "list", reqdPkgs = "list",
    parameters = "data.frame", inputObjects = "data.frame", outputObjects = "data.frame"
  ),
  prototype = list(
    name = character(0), description = character(0), keywords = character(0),
    childModules = character(0), authors = person(), version = numeric_version("0.0.0"),
    spatialExtent = terra::ext(rep(0L, 4L)), timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = NA_real_, citation = list(), documentation = list(),
    loadOrder = list(after = NULL, before = NULL), reqdPkgs = list(),
    parameters = data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE
    ),
    inputObjects = ._inputObjectsDF(),
    outputObjects = ._outputObjectsDF()
  ),
  validity = function(object) {
    if (!(isExtents(object@spatialExtent)))
      stop("spatialExtent must be an Extent object or SpatExtent")
    if (length(object@name) != 1L) stop("name must be a single character string.")
    if (length(object@description) != 1L) stop("description must be a single character string.")
    if (length(object@keywords) < 1L) stop("keywords must be supplied.")
    if (length(object@authors) < 1L) stop("authors must be specified.")
    if (length(object@timeframe) != 2L) stop("timeframe must be specified using two date-times.")
    if (length(object@timeunit) < 1L) stop("timeunit must be specified.")
    # if (length(object@loadOrder) < 1L) stop("loadOrder must be specified.") # optional
    if (length(object@reqdPkgs)) {
      if (!any(unlist(lapply(object@reqdPkgs, is.character)))) {
        stop("reqdPkgs must be specified as a list of package names.")
      }
    }

    # data.frame checking
    if (length(object@inputObjects) < 1L)
      stop("input object name and class must be specified, or NA.")
    if (length(object@outputObjects) < 1L)
      stop("output object name and class must be specified, or NA.")
    if (!all(names(._inputObjectsDF()) %in% colnames(object@inputObjects)))
      stop(paste("input object data.frame must use colnames",
                 paste(collapse = ", ", colnames(._inputObjectsDF()))))

    if ( !all(colnames(._outputObjectsDF()) %in% colnames(object@outputObjects)) ) {
     stop(paste("output object data.frame must use colnames",
                paste(collapse = ", ", colnames(._outputObjectsDF()))))
    }
    # try coercing to character because if data.frame was created without specifying
    # `stringsAsFactors=FALSE`, or used `NA` (logical) there will be problems...
    if (!is.character(object@inputObjects$objectName)) {
     object@inputObjects$objectName <- as.character(object@inputObjects$objectName)
    }
    if (!is.character(object@inputObjects$objectClass)) {
     object@inputObjects$objectClass <- as.character(object@inputObjects$objectClass)
    }
    if (!is.character(object@inputObjects$sourceURL)) {
      object@inputObjects$sourceURL <- as.character(object@inputObjects$sourceURL)
    }
    if (!is.character(object@inputObjects$other)) {
     object@inputObjects$desc <- as.character(object@inputObjects$desc)
    }
    if (!is.character(object@outputObjects$objectName)) {
     object@outputObjects$objectName <- as.character(object@outputObjects$objectName)
    }
    if (!is.character(object@outputObjects$objectClass)) {
     object@outputObjects$objectClass <- as.character(object@outputObjects$objectClass)
    }
    if (!is.character(object@outputObjects$desc)) {
     object@outputObjects$desc <- as.character(object@outputObjects$desc)
    }
})

#' The `.simDeps` class
#'
#' Defines all simulation dependencies for all modules within a SpaDES simulation.
#'
#' @slot dependencies   List of [.moduleDeps()] dependency objects.
#'
#' @seealso [.moduleDeps()], [spadesClasses()]
#'
#' @aliases .simDeps
#' @keywords internal
#' @rdname simDeps-class
#'
#' @author Alex Chubaty
#'
setClass(
  ".simDeps",
  slots = list(dependencies = "list"),
  prototype = list(dependencies = list(NULL)),
  validity = function(object) {
    # remove empty (NULL) elements
    object@dependencies <- object@dependencies[lapply(object@dependencies, length) > 0]

    # ensure list contains only .moduleDeps objects
    if (!all(unlist(lapply(object@dependencies, is, class2 = ".moduleDeps")))) {
      stop("invalid type: not a .moduleDeps object")
    }
})

.emptySimDeps <- new(".simDeps")

isExtents <- function(x)
  is(x, "SpatExtent") || is(x, "Extent")
