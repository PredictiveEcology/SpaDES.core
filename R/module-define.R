#' Defaults values used in defineModule
#'
#' Where individual elements are missing in \code{defineModule},
#' these defaults will be used.
#' @export
#'
moduleDefaults <- list(
  ## these need to match up with `.emptyMetadata` list in helpers.R
  timeunit = "year",
  name = NA_character_,
  description = "",
  keywords = "",
  authors = {
    pers = getOption("devtools.desc.author",
                      person(c("First", "Middle"), "Last",
                             email = "email@example.com",
                             role = c("aut", "cre")))
    if (!is(pers, "person"))
      pers <- tryCatch(eval(parse(text = pers)), error = function(e) pers)
    pers
    },
  childModules = character(0),
  version = "0.0.0.9000", ## numeric_versions don't deparse well
  extent = quote(raster::extent(rep(NA_real_, 4))),
  timeframe = quote(as.POSIXlt(c(NA, NA))),
  citation = list("citation.bib"),
  documentation = list(),
  reqdPkgs = list("ggplot2")
)

################################################################################
#' Define a new module.
#'
#' Specify a new module's metadata as well as object and package dependencies.
#' Packages are loaded during this call. Any or all of these can be missing, with missing
#' values set to defaults
#'
#' @section Required metadata elements:
#'
#' \tabular{ll}{
#'    \code{name} \tab Module name. Must match the filename (without the \code{.R} extension).
#'                     This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{description} \tab Brief description of the module.
#'                            This is currently not parsed by SpaDES;
#'                            it is for human readers only. \cr
#'    \code{keywords} \tab Author-supplied keywords.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    \code{childModules} \tab If this contains any character vector, then it will
#'                             be treated as a parent module. If this is a parent module,
#'                             then only this list entry will be read. For normal,
#'                             i.e., 'child modules', this should be \code{character(0)} or
#'                             \code{NA}.
#'                             If a character vector is provided, then these must be the
#'                             names of the modules located in the same file path as this
#'                             parent module that will be loaded during the \code{simInit}.\cr
#'    \code{authors} \tab Module author information (as a vector of \code{\link{person}}
#'                        objects. This is currently not parsed by SpaDES;
#'                        it is for human readers only.\cr
#'    \code{version} \tab Module version number (will be coerced to \code{\link{numeric_version}}
#'                        if a character or numeric are supplied).
#'                        The module developer should update manually this with each change
#'                        that is made to the module. See \url{https://semver.org/}
#'                        for a widely accepted standard for version numbering.\cr
#'    \code{spatialExtent} \tab The spatial extent of the module supplied via
#'                              \code{raster::extent}. This is currently unimplemented.
#'                              Once implemented, this should define what spatial region this
#'                              module is scientifically reasonable to be used in.\cr
#'    \code{timeframe} \tab Vector (length 2) of POSIXt dates specifying the temporal extent
#'                          of the module. Currently unimplemented.
#'                          Once implemented, this should define what time frame this
#'                          module is scientifically reasonable to be used for.\cr
#'    \code{timeunit} \tab Time scale of the module (e.g., "day", "year"). This
#'                         MUST be specified. It indicates what '1' unit of time
#'                         means for this module. \code{SpaDES} interprets this
#'                         and if modules have different \code{timeunit} values
#'                         then it will correctly schedule each module, using the
#'                         smallest (currently the default) timeunit as the
#'                         'model' timeunit in the \code{spades} call.\cr
#'    \code{citation} \tab List of character strings specifying module citation information.
#'                         Alternatively, a list of filenames of \code{.bib} or similar files.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only.\cr
#'    \code{documentation} \tab List of filenames referring to module documentation sources.
#'                              This is currently not parsed by SpaDES;
#'                              it is for human readers only.\cr\cr
#'    \code{reqdPkgs} \tab List of R package names required by the module. These
#'                         packages will be loaded when \code{simInit} is called.
#'                         \code{\link[Require]{Require}} will be used internally
#'                         to load if available, and install if not available.
#'                         Because \code{\link[Require]{Require}} can also download from
#'                         GitHub.com, these packages can specify package names stored
#'                         on GitHub, e.g., \code{"PredictiveEcology/SpaDES.core@development"}. \cr
#'    \code{parameters} \tab A data.frame specifying the parameters used in the module.
#'                           Usually produced by \code{rbind}-ing the outputs of multiple
#'                           \code{\link{defineParameter}} calls. These parameters indicate
#'                           the default values that will be used unless a module user
#'                           overrides them with the \code{params} argument in the
#'                           \code{\link{simInit}} call. The minimum and maximum are
#'                           currently used by the \code{SpaDES.shiny::shine} function and the
#'                           \code{POM} function, and they should indicate the range
#'                           of values that are reasonable scientifically.\cr
#'    \code{inputObjects} \tab A \code{data.frame} specifying the data objects expected as
#'                             inputs to the module,
#'                             with columns \code{objectName} (class \code{character}),
#'                             \code{objectClass} (class \code{character}),
#'                             \code{sourceURL} (class \code{character}), and \code{other}
#'                              (currently spades does nothing with this column).
#'                             This data.frame identifies the objects that are expected,
#'                             but does not do any loading of that object into the simList.
#'                             The \code{sourceURL} gives the developer the opportunity
#'                             to identify the source of a data file that can be used
#'                             with the model. This URL will be
#'                             used if the user calls \code{downloadData} (or
#'                             \code{downloadModule(..., data = TRUE)}. If the raw data
#'                             must be modified, the developer can use create a
#'                             function called \code{.inputObjects} in their module. That
#'                             function will be run during the \code{simInit} call. The
#'                             developer should ensure that if the object is supplied
#'                             by the module user as an argument in the \code{simInit}, then
#'                             the \code{.inputObjects} should not be run, i.e., use an
#'                             \code{(is.null(sim$xxx)))}.\cr
#'    \code{outputObjects} \tab A \code{data.frame} specifying the data objects output by
#'                              the module, with columns identical to those in
#'                              \code{inputObjects}. Like \code{inputObjects} above,
#'                              this only identifies the objects that this module will output
#'                              into the \code{simList}.
#'                              The module developer must create the necessary functions
#'                              that will cause these objects to be put into the
#'                              \code{simList}.\cr
#' }
#'
#' @inheritParams objs
#'
#' @param x A list with a number of named elements, referred to as the metadata. See details.
#'
#' @return Updated \code{simList} object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom raster extent
#' @importFrom utils person as.person
#' @include simList-class.R
#' @rdname defineModule
#' @seealso moduleDefaults
#'
#' @examples
#' \dontrun{
#'   ## a default version of the defineModule is created with a call to newModule
#'   newModule("test", path = tempdir(), open = FALSE)
#'
#'   ## view the resulting module file
#'   if (interactive()) file.edit(file.path(tempdir(), "test", "test.R"))
#' }
#'
setGeneric("defineModule", function(sim, x) {
  standardGeneric("defineModule")
})

#' @export
#' @rdname defineModule
setMethod(
  "defineModule",
  signature(sim = "simList", x = "list"),
  definition = function(sim, x) {
    # check that all metadata elements are present
    metadataRequired <- slotNames(new(".moduleDeps"))
    metadataProvided <- metadataRequired %in% names(x)
    metadataMissing <- metadataRequired[!metadataProvided]

    notEnforced <- c("spatialExtent", "keywords", "childModules", "timeframe", "citation", "documentation")
    if (!all(metadataProvided)) {
      # inputObjects and outputObjects are dealt with differently in parseModule
      #   don't trigger a warning here.
      metadataMissing <- setdiff(metadataMissing, c("inputObjects", "outputObjects"))
      metadataMissingHard <- setdiff(metadataMissing, notEnforced)
      if (length(metadataMissingHard))
        warning(paste0(
          "The \'", x$name, "\' module is missing the metadata for:\n",
          paste(" - ", metadataMissing, collapse = "\n"), "\n",
          "Using default values, which may not be desirable.\n",
          "See moduleDefaults"
        ))
    }

    ## enforce/coerce types for the user-supplied param list
    lapply(c("name", "description", "keywords"), function(z) {
      x[[z]] <<- if (is.null(x[[z]]) || (length(x[[z]]) == 0)) {
        moduleDefaults[[z]]
      } else {
        as.character(x[[z]])
      }
    })

    x$childModules <- if (is.null(x$childModules)) {
      moduleDefaults$childModules
    } else {
      if (any(is.na(x$childModules))) {
        moduleDefaults$childModules
      } else {
        x$childModules %>% as.character() %>% na.omit() %>% as.character() # nolint
      }
    }

    x$authors <- if (is.null(x$authors) || any(is.na(x$authors))) {
      moduleDefaults$authors
    } else {
      as.person(x$authors)
    }

    ## maintain backwards compatibility with SpaDES versions prior to 1.3.1.9044
    ## where `version` was a single `numeric_version` value instead of named list
    x$version <- if (is.null(names(x$version))) {
      eval(moduleDefaults[["version"]]) ## SpaDES < 1.3.1.9044
    } else {
      x$version[[x$name]] ## SpaDES >= 1.3.1.9044
    }
    x$version <- as.numeric_version(x$version)

    x$spatialExtent <- if (!is(x$spatialExtent, "Extent")) {
      if (is.null(x$spatialExtent)) {
        eval(moduleDefaults$extent)
      } else {
        if (is.na(x$spatialExtent)) {
          moduleDefaults$extent
        } else {
          extent(x$spatialExtent)
        }
      }
    }

    x$timeframe <- if (is.null(x$timeframe) || any(is.na(x$timeframe))) {
      eval(moduleDefaults$timeframe)
    } else if (!is.numeric.POSIXt(x$timeframe)) {
      as.POSIXlt(x$timeframe)
    } %>% `[`(1:2) # nolint

    if (is.null(x$timeunit) || any(is.na(x$timeunit))) {
      x$timeunit <- moduleDefaults$timeunit
    }

    lapply(c("citation", "documentation", "reqdPkgs"), function(z) {
      x[[z]] <<- if (is.null(x[[z]])) {
        moduleDefaults[[z]]
      } else {
        as.list(x[[z]])
      }
    })
    if (is.null(x$parameters)) {
      x$parameters <- defineParameter()
    } else {
      if (is(x$parameters, "data.frame")) {
        if (!all(colnames(x$parameters) %in% colnames(defineParameter())) ||
            !all(colnames(defineParameter()) %in% colnames(x$parameters))) {
          stop("invalid data.frame `parameters` in module `", x$name, "`")
        }
      } else {
        x$parameters <- defineParameter()
      }
    }

    if (is.null(x$inputObjects)) {
      x$inputObjects <- ._inputObjectsDF()
    } else {
      if (is(x$inputObjects, "data.frame")) {
        if (!all(colnames(x$inputObjects) %in% colnames(._inputObjectsDF())) ||
            !all(colnames(._inputObjectsDF()) %in% colnames(x$inputObjects))) {
          stop("invalid data.frame `inputObjects` in module `", x$name, "`:\n",
               "provided: ", paste(colnames(x$inputObjects), collapse = ", "),
               "expected: ", paste(colnames(._inputObjectsDF()), collapse = ", "))
        }
      } else {
        x$inputObjects <- ._inputObjectsDF()
      }
    }
    if (NROW(x$inputObjects)) {
      if (is.null(x$inputObjects$sourceURL)) {
        x$inputObjects$sourceURL <- rep(NA_character_, NROW(x$inputObjects))
      }
      ids <- which(x$inputObjects$sourceURL == "")
      if (length(ids)) {
        x$inputObjects$sourceURL[ids] <- NA_character_
      }
    }

    if (is.null(x$outputObjects)) {
      x$outputObjects <- ._outputObjectsDF()
    } else {
      if (is(x$outputObjects, "data.frame")) {
        if (!all(colnames(x$outputObjects) %in% colnames(._outputObjectsDF())) ||
            !all(colnames(._outputObjectsDF()) %in% colnames(x$outputObjects))) {
          stop("invalid data.frame `outputObjects` in module `", x$name, "`:",
               "provided: ", paste(colnames(x$outputObjects), collapse = ", "), "\n",
               "expected: ", paste(colnames(._outputObjectsDF()), collapse = ", "))
        }
      } else {
        x$outputObjects <- ._outputObjectsDF()
      }
    }

    ## check that documentation actually exists locally
    docs <- sapply(x$documentation, na.omit) %>%
      (function(x) if (length(x)) character(0) else as.character(x))
    if (length(docs)) {
      lapply(docs, function(y) {
        if (!file.exists(file.path(modulePath(sim), y))) {
          stop("Module documentation file ", y, " not found in modulePath.")
        }
      })
    }

    ## check that children actually exist locally, and add to list of child modules
    if (length(x$childModules)) {
      lapply(x$childModules, function(y) {
        if (file.exists(file.path(modulePath(sim), y))) {
          z <- y %>% lapply(., `attributes<-`, list(type = "child"))
          modules(sim) <- append_attr(sim@modules, z)
        } else {
          stop("Module ", y, "(a child module of ", x$name, ") not found in modulePath.")
        }
      })
    }

    ## create module deps object and add to sim deps
    m <- do.call(new, c(".moduleDeps", x))
    return(.addDepends(sim, m))
})

################################################################################
#' Define a parameter used in a module
#'
#' Used to specify a parameter's name, value, and set a default. The \code{min} and
#' \code{max} arguments are ignored by \code{simInit} or \code{spades}; they
#' are for human use only. To ensure that a user cannot set parameters outside of
#' a range of values, the module developer should use assertions in their module
#' code.
#'
#' @note Be sure to use the correct NA type: logical (\code{NA}), integer (\code{NA_integer_}),
#'       real (\code{NA_real_}), complex (\code{NA_complex_}), or character (\code{NA_character_}).
#'       See \code{\link{NA}}.
#'
#' @param name      Character string giving the parameter name.
#' @param class     Character string giving the parameter class.
#' @param default   The default value to use when none is specified by the user.
#'                  Non-standard evaluation is used for the expression.
#' @param min       With \code{max}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#'                  \emph{These are not tested by} \code{simInit} \emph{or}
#'                  \code{spades}. These are primarily for human use, i.e., to
#'                  tell a module user what values the module expects.
#' @param max       With \code{min}, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#'                  \emph{These are not tested by} \code{simInit} \emph{or}
#'                  \code{spades}. These are primarily for human use, i.e., to
#'                  tell a module user what values the module expects.
#' @param desc      Text string providing a brief description of the parameter.
#'                  If there are extra spaces or carriage returns, these will be stripped,
#'                  allowing for multi-line character strings without using \code{paste}
#'                  or multiple quotes.
#' @param ...       A convenience that allows writing a long \code{desc} without
#'                  having to use \code{paste}; any character strings after \code{desc}
#'                  will be \code{paste}d together with \code{desc}.
#'
#' @return data.frame
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @rdname defineParameter
#'
#' @seealso \code{\link{P}}, \code{\link{params}} for accessing these parameters in
#'          a module.
#' @examples
#' parameters = rbind(
#'   defineParameter("lambda", "numeric", 1.23, desc = "intrinsic rate of increase"),
#'   defineParameter("P", "numeric", 0.2, 0, 1, "probability of attack"),
#'
#'   # multi-line desc without quotes on each line -- spaces and carriage returns are stripped
#'   defineParameter("rate", "numeric", 0.2, 0, 1,
#'                   "rate of arrival. This is in individuals
#'                   per day. This can be modified
#'                   by the user"),
#'   # multi-line desc with quotes on each line
#'   defineParameter("times", "numeric", 0.2, 0, 1,
#'                   desc = "The times during the year ",
#'                          "that events will occur ",
#'                          "with possibility of random arrival times")
#'
#' )
#'
#' \dontrun{
#' # Create a new module, then access parameters using \code{P}
#' tmpdir <- file.path(tempdir(), "test")
#' checkPath(tmpdir, create = TRUE)
#'
#' # creates a  new, "empty" module -- it has defaults for everything that is required
#' newModule("testModule", tmpdir, open = FALSE)
#'
#' # Look at new module code -- see defineParameter
#' if (interactive()) file.edit(file.path(tmpdir, "testModule", "testModule.R"))
#'
#' # initialize the simList
#' mySim <- simInit(modules = "testModule",
#'                  paths = list(modulePath = tmpdir))
#'
#' # Access one of the parameters -- because this line is not inside a module
#' #  function, we must specify the module name. If used within a module,
#' #  we can omit the module name
#' P(mySim, "testModule")$.useCache
#' }
#'
setGeneric("defineParameter", function(name, class, default, min, max, desc, ...) {
  standardGeneric("defineParameter")
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character", default = "ANY",
                    min = "ANY", max = "ANY", desc = "character"),
          definition = function(name, class, default, min, max, desc, ...) {
            # for non-NA values warn if `default`, `min`, and `max` aren't the specified type
            # we can't just coerece these because it wouldn't allow for character,
            #  e.g., start(sim)
            anyNAs <- suppressWarnings(c(is.na(default), is.na(min), is.na(max)))
            if (!all(anyNAs)) {
              # if some or all are NA -- need to check
              wrongClass <- lapply(class, function(cla)
                # Note c() doesn't always produce a vector -- e.g., functions, calls -- need lapply
                lapply(c(default, min, max)[!anyNAs], function(val)
                  is(val, cla)))
              classWrong <- all(!unlist(wrongClass))
              if (classWrong) {
                # any messages here are captured if this is run from .parseModule
                #   It will append module name
                message(crayon::magenta("defineParameter: '", name, "' is not of specified type '",
                                        class, "'.", sep = ""))
              }
            }

            desc <- rmExtraSpacesEOLCollapse(append(list(desc), list(...)))

            # previously used `substitute()` instead of `I()`,
            # but it did not allow for a vector to be passed with `c()`
            df <- data.frame(
              paramName = name, paramClass = I(list(class)), default = I(list(default)),
              min = I(list(min)), max = I(list(max)), paramDesc = desc,
              stringsAsFactors = FALSE)
            return(df)
})

#' @rdname defineParameter
setMethod("defineParameter",
          signature(name = "character", class = "character",
                    default = "ANY", min = "missing", max = "missing",
                    desc = "character"),
          definition = function(name, class, default, desc, ...) {
            NAtypes <- c("character", "complex", "integer", "logical", "numeric") # nolint
            if (class %in% NAtypes) {
              # coerce `min` and `max` to same type as `default`
              min <- as(NA, class)
              max <- as(NA, class)
            } else {
              min <- NA
              max <- NA
            }
            df <- defineParameter(name = name, class = class, default = default,
                            min = min, max = max, desc = desc, ...)

            return(df)
})

#' @rdname defineParameter
setMethod(
  "defineParameter",
  signature(name = "missing", class = "missing", default = "missing",
            min = "missing", max = "missing", desc = "missing"),
  definition = function() {
    df <- data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE)
    return(df)
})

################################################################################
#' Define an input object that the module expects.
#'
#' Used to specify an input object's name, class, description, source url and
#' other specifications.
#'
#' @param objectName   Character string to define the input object's name.
#'
#' @param objectClass  Character string to specify the input object's class.
#'
#' @param desc         Text string providing a brief description of the input object.
#'                  If there are extra spaces or carriage returns, these will be stripped,
#'                  allowing for multi-line character strings without using \code{paste}
#'                  or multiple quotes.
#'
#' @param sourceURL    Character string to specify an URL to reach the input object,
#'                     default is \code{NA}.
#'
#' @param ...          Other specifications of the input object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{inputObjects} in a
#' module's metadata.
#'
#' @author Yong Luo
#' @export
#' @rdname expectsInput
#'
#' @examples
#' inputObjects <- bindrows(
#'   expectsInput(objectName = "inputObject1", objectClass = "character",
#'                desc = "this is for example", sourceURL = "not available"),
#'   expectsInput(objectName = "inputObject2", objectClass = "numeric",
#'                desc = "this is for example", sourceURL = "not available",
#'                otherInformation = "I am the second input object")
#' )
#'
setGeneric("expectsInput",
           function(objectName, objectClass, desc, sourceURL, ...) {
             standardGeneric("expectsInput")
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY", sourceURL = "ANY"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {
    return(expectsInput(as.character(objectName), as.character(objectClass),
                        as.character(desc), as.character(sourceURL), ...))
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "character"),
  definition = function(objectName, objectClass, desc, sourceURL, ...) {

    desc <- rmExtraSpacesEOLCollapse(append(list(desc), list(...)))

    returnDataframe <- data.frame(cbind(objectName, objectClass, desc, sourceURL),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (!is.null(names(templist)))
      returnDataframe <- addNamedEntry(returnDataframe, templist[nzchar(names(templist))],
                                     objectName, fn = "expectsInput")
    return(returnDataframe)
})

#' @export
#' @rdname expectsInput
setMethod(
  "expectsInput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character", sourceURL = "missing"),
  definition = function(objectName, objectClass, desc, ...) {

    desc <- rmExtraSpacesEOLCollapse(append(list(desc), list(...)))

    return(expectsInput(objectName, objectClass, desc, sourceURL = NA_character_, ...))
})

################################################################################
#' Define an output object of a module
#'
#' Used to specify an output object's name, class, description and other specifications.
#'
#' @param objectName   Character string to define the output object's name.
#'
#' @param objectClass  Character string to specify the output object's class.
#'
#' @param desc         Text string providing a brief description of the output object.
#'                  If there are extra spaces or carriage returns, these will be stripped,
#'                  allowing for multi-line character strings without using \code{paste}
#'                  or multiple quotes.
#'
#' @param ...          Other specifications of the output object.
#'
#' @return A \code{data.frame} suitable to be passed to \code{outputObjects} in
#' a module's metadata.
#'
#' @author Yong Luo
#' @export
#' @rdname createsOutput
#'
#' @examples
#' outputObjects <- bindrows(
#'   createsOutput(objectName = "outputObject1", objectClass = "character",
#'                 desc = "this is for example"),
#'   createsOutput(objectName = "outputObject2", objectClass = "numeric",
#'                 desc = "this is for example",
#'                 otherInformation = "I am the second output object")
#' )
#'
setGeneric(
  "createsOutput",
  function(objectName, objectClass, desc, ...) {
    standardGeneric("createsOutput")
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "ANY", objectClass = "ANY",
                        desc = "ANY"),
  definition = function(objectName, objectClass, desc, ...) {
    desc <- rmExtraSpacesEOLCollapse(append(list(desc), list(...)))
    return(createsOutput(as.character(objectName), as.character(objectClass),
                         as.character(desc)))
})

#' @export
#' @rdname createsOutput
setMethod(
  "createsOutput",
  signature = signature(objectName = "character", objectClass = "character",
                        desc = "character"),
  definition = function(objectName, objectClass, desc, ...) {

    desc <- rmExtraSpacesEOLCollapse(append(list(desc), list(...)))

    returnDataframe <- data.frame(cbind(objectName, objectClass, desc),
                                  stringsAsFactors = FALSE)
    templist <- list(...)
    if (!is.null(names(templist)))
      returnDataframe <- addNamedEntry(returnDataframe, templist[nzchar(names(templist))],
                                       objectName, fn = "createsOutput")
    return(returnDataframe)
})

#' An internal function for coercing a data.frame to inputs()
#'
#' @param inputDF A data.frame with partial columns to pass to inputs<-
#' @param startTime Numeric time. The start(sim).
#'
#' @keywords internal
#' @importFrom data.table setnames
#' @rdname fillInputRows
.fillInputRows <- function(inputDF, startTime) {
  factorCols <- sapply(inputDF, is.factor)
  if (any(factorCols)) {
    inputDF[, factorCols] <- sapply(inputDF[, factorCols], as.character)
  }
  needRenameArgs <- grepl(names(inputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(inputDF)[needRenameArgs] <- .fileTableInCols[pmatch("arg", .fileTableInCols)]
  }
  columns <- pmatch(substr(.fileTableInCols, 1, c(3, 5, 1, 3, 5, 5, 3, 5)), names(inputDF))
  setnames(inputDF, old = colnames(inputDF)[na.omit(columns)],
           new = .fileTableInCols[!is.na(columns)])
  if (any(is.na(columns))) {
    inputDF[, .fileTableInCols[is.na(columns)]] <- NA
  }

  if (any(is.na(inputDF[, "loadTime"]))) {
    inputDF[is.na(inputDF$loadTime), "loadTime"] <- startTime
  }

  if (any(is.na(inputDF[, "objectName"]))) {
    inputDF[is.na(inputDF$objectName), "objectName"] <- fileName(inputDF[is.na(inputDF$objectName), "file"]) # nolint
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(inputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- inputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    inputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    inputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  objectsOnly <- is.na(inputDF[, "file"])
  if (!all(objectsOnly)) {
    inputDF2 <- inputDF[!objectsOnly, ]
    if (any(is.na(inputDF2[, "fun"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- na.omit(match(fileExt(fl), .fileExts[, "exts"]))
      inputDF2$fun[is.na(inputDF2$fun)] <- .fileExts[exts, "fun"]
    }

    if (any(is.na(inputDF2[, "package"]))) {
      .fileExts <- .fileExtensions()
      fl <- inputDF2$file
      exts <- match(fileExt(fl), .fileExts[, "exts"])
      inputDF2$package[is.na(inputDF2$package)]  <- .fileExts[exts, "package"]
    }
    inputDF[!objectsOnly, ] <- inputDF2
  }
  return(inputDF)
}

#' An internal function for coercing a data.frame to outputs()
#'
#' @param outputDF A data.frame with partial columns to pass to outputs<-
#' @param endTime Numeric time. The end(sim).
#'
#' @keywords internal
#' @importFrom data.table setnames
#' @rdname fillOutputRows
.fillOutputRows <- function(outputDF, endTime) {
  needRenameArgs <- grepl(names(outputDF), pattern = "arg[s]?$")
  if (any(needRenameArgs)) {
    colnames(outputDF)[needRenameArgs] <-
      .fileTableOutCols[pmatch("arg", .fileTableOutCols)]
  }
  columns <- pmatch(substr(.fileTableOutCols, 1, c(3, 5, 1, 3, 5, 5, 3)), names(outputDF))
  setnames(outputDF, old = colnames(outputDF)[na.omit(columns)],
           new = .fileTableOutCols[!is.na(columns)])

  if (any(is.na(columns))) {
    outputDF[, .fileTableOutCols[is.na(columns)]] <- NA
  }
  if (any(is.na(outputDF[, "saveTime"]))) {
    outputDF[is.na(outputDF$saveTime), "saveTime"] <- endTime
  }

  # correct those for which a specific function is supplied in filelistDT$fun
  usesSemiColon <- grep(outputDF[, "fun"], pattern = "::")

  if (length(usesSemiColon) > 0) {
    loadFun <- outputDF$fun[usesSemiColon]
    splitPackFun <- strsplit(split = "::", loadFun)
    outputDF$package[usesSemiColon] <- sapply(splitPackFun, function(x) x[1])
    outputDF$fun[usesSemiColon] <- sapply(splitPackFun, function(x) x[2])
  }

  if (any(is.na(outputDF[, "fun"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA = TRUE))) {
      outputDF$fun[is.na(fl) | (!nzchar(exts, keepNA = TRUE))] <- .fileExts$fun[1]
    }
    if (any(is.na(outputDF[, "fun"]))) {
      extsAvail <- checkKnownExts(exts, .fileExts)
      outputDF$fun[is.na(outputDF$fun)] <- .fileExts[extsAvail, "fun"]
    }
  }

  if (any(is.na(outputDF[, "package"]))) {
    .fileExts <- .saveFileExtensions()
    fl <- outputDF$file
    exts <- fileExt(fl)
    if (any(is.na(fl)) | any(!nzchar(exts, keepNA = TRUE))) {
      outputDF$package[is.na(fl) | (!nzchar(exts, keepNA = TRUE))] <- .fileExts$package[1]
    }
    if (any(is.na(outputDF[, "package"]))) {
      exts <- fileExt(fl)
      extsAvail <- checkKnownExts(exts, .fileExts)
      outputDF$package[is.na(outputDF$package)] <- .fileExts[extsAvail, "package"]
    }
  }
  return(outputDF)
}

checkKnownExts <- function(exts, knownFileExts) {
  if (missing(knownFileExts))
    knownFileExts <- .saveFileExtensions()
  extsAvail <- na.omit(match(exts, knownFileExts[, "exts"]))
  extsMissing <- setdiff(exts, exts[extsAvail])
  if (length(extsMissing) > 0)
    stop("No known save method is available for class ", extsMissing)
  extsAvail
}


addNamedEntry <- function(returnDataframe, templist, objectName, fn) {
  if (length(templist) > 0) {
    for (i in 1:length(templist)) {
      returnDataframe <- data.frame(cbind(returnDataframe, I(list(templist[[i]])),
                                          stringsAsFactors = FALSE))
      nam <- names(templist)[i]
      if (is.null(nam))
        stop(fn, " for ", objectName, " has too many unnamed fields; perhaps forgot to use paste(...)?")
      names(returnDataframe)[ncol(returnDataframe)] <- nam
    }
  }
  returnDataframe
}

fileExt <- getFromNamespace("fileExt", "reproducible")
filePathSansExt <- getFromNamespace("filePathSansExt", "reproducible")
