#' Defaults values used in `defineModule`
#'
#' Where individual elements are missing in `defineModule`, these defaults will be used.
#'
#' @return named list of default module metadata
#'
#' @export
moduleDefaults <- list(
  ## these need to match up with `.emptyMetadata` list in helpers.R
  timeunit = .timeunitDefault(),
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
  extent = quote(terra::ext(rep(0, 4))),
  timeframe = quote(as.POSIXlt(c(NA, NA))),
  citation = list("citation.bib"),
  documentation = list(),
  loadOrder = list(after = NULL, before = NULL),
  reqdPkgs = list("ggplot2") # used in newModule template ggplotFn
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
#'    `name` \tab Module name. Must match the filename (without the `.R` extension).
#'                     This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    `description` \tab Brief description of the module.
#'                            This is currently not parsed by SpaDES;
#'                            it is for human readers only. \cr
#'    `keywords` \tab Author-supplied keywords.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only. \cr
#'    `childModules` \tab If this contains any character vector, then it will
#'                             be treated as a parent module. If this is a parent module,
#'                             then only this list entry will be read. For normal,
#'                             i.e., 'child modules', this should be `character(0)` or
#'                             `NA`.
#'                             If a character vector is provided, then these must be the
#'                             names of the modules located in the same file path as this
#'                             parent module that will be loaded during the `simInit`.\cr
#'    `authors` \tab Module author information (as a vector of [person()]
#'                        objects. This is currently not parsed by SpaDES;
#'                        it is for human readers only.\cr
#'    `version` \tab Module version number (will be coerced to [numeric_version()]
#'                        if a character or numeric are supplied).
#'                        The module developer should update manually this with each change
#'                        that is made to the module. See <https://semver.org/>
#'                        for a widely accepted standard for version numbering.\cr
#'    `spatialExtent` \tab The spatial extent of the module supplied via
#'                              `terra::ext`. This is currently unimplemented.
#'                              Once implemented, this should define what spatial region this
#'                              module is scientifically reasonable to be used in.\cr
#'    `timeframe` \tab Vector (length 2) of `POSIXt` dates specifying the temporal extent
#'                          of the module. Currently unimplemented.
#'                          Once implemented, this should define what time frame this
#'                          module is scientifically reasonable to be used for.\cr
#'    `timeunit` \tab Time scale of the module (e.g., "day", "year"). If this is
#'                         not specified, then `.timeunitDefault()` will be used.
#'                         It indicates what '1' unit of time
#'                         means for this module. `SpaDES` interprets this
#'                         and if modules have different `timeunit` values
#'                         then it will correctly schedule each module, using the
#'                         smallest (currently the default) timeunit as the
#'                         'model' timeunit in the `spades` call.\cr
#'    `citation` \tab List of character strings specifying module citation information.
#'                         Alternatively, a list of filenames of `.bib` or similar files.
#'                         This is currently not parsed by SpaDES;
#'                         it is for human readers only.\cr
#'    `documentation` \tab List of filenames referring to module documentation sources.
#'                              This is currently not parsed by SpaDES;
#'                              it is for human readers only.\cr
#'    `loadOrder` \tab Named list of length 0, 1, or 2, with names being `after` and `before`.
#'                     Each element should be a character string/vector naming 1 or more
#'                     modules that will be loaded `after` or `before` this module.
#'                     `after` and `before` are used from the current module's
#'                     perspective. So, `list(after = c("Biomass_core"))` means that
#'                     this module must come *after* `"Biomass_core"`. This should only
#'                     be used when there are cyclic dependencies (2 or more modules
#'                     have 1 or more objects that is in both `inputObjects` and
#'                     `outputObjects` of both/all modules) between this module
#'                     and other modules. In cases where the dependencies are not cyclic,
#'                     SpaDES is able to resolve the order correctly.\cr
#'    `reqdPkgs` \tab List of R package names required by the module. These
#'                         packages will be loaded when `simInit` is called.
#'                         [Require::Require()] will be used internally
#'                         to load if available, and install if not available.
#'                         Because [Require::Require()] can also download from
#'                         GitHub.com, these packages can specify package names stored
#'                         on GitHub, e.g., `"PredictiveEcology/SpaDES.core@development"`. \cr
#'    `parameters` \tab A data.frame specifying the parameters used in the module.
#'                           Usually produced by `rbind`-ing the outputs of multiple
#'                           [defineParameter()] calls. These parameters indicate
#'                           the default values that will be used unless a module user
#'                           overrides them with the `params` argument in the
#'                           [simInit()] call. The minimum and maximum are
#'                           currently used by the `SpaDES.shiny::shine` function and the
#'                           `POM` function, and they should indicate the range
#'                           of values that are reasonable scientifically.\cr
#'    `inputObjects` \tab A `data.frame` specifying the data objects expected as
#'                             inputs to the module,
#'                             with columns `objectName` (class `character`),
#'                             `objectClass` (class `character`),
#'                             `sourceURL` (class `character`), and `other`
#'                              (currently spades does nothing with this column).
#'                             This data.frame identifies the objects that are expected,
#'                             but does not do any loading of that object into the `simList`.
#'                             The `sourceURL` gives the developer the opportunity
#'                             to identify the source of a data file that can be used
#'                             with the model. This URL will be
#'                             used if the user calls `downloadData` (or
#'                             `downloadModule(..., data = TRUE)`. If the raw data
#'                             must be modified, the developer can use create a
#'                             function called `.inputObjects` in their module. That
#'                             function will be run during the `simInit` call. The
#'                             developer should ensure that if the object is supplied
#'                             by the module user as an argument in the `simInit`, then
#'                             the `.inputObjects` should not be run, i.e., use an
#'                             `(is.null(sim$xxx)))`.\cr
#'    `outputObjects` \tab A `data.frame` specifying the data objects output by
#'                              the module, with columns identical to those in
#'                              `inputObjects`. Like `inputObjects` above,
#'                              this only identifies the objects that this module will output
#'                              into the `simList`.
#'                              The module developer must create the necessary functions
#'                              that will cause these objects to be put into the
#'                              `simList`.\cr
#' }
#'
#' @inheritParams objs
#'
#' @param x A list with a number of named elements, referred to as the metadata. See details.
#'
#' @return Updated `simList` object.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom terra ext
#' @importFrom utils person as.person
#' @include simList-class.R
#' @rdname defineModule
#' @seealso [moduleDefaults()], [defineEvent()]
#'
#' @examples
#' \donttest{
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

    notEnforced <- c("spatialExtent", "keywords", "childModules", "timeframe", "citation", "documentation",
                     "loadOrder")
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

    x$spatialExtent <- if (!isExtents(x$spatialExtent)) {
      if (is.null(x$spatialExtent)) {
        eval(moduleDefaults$extent)
      } else {
        if (is.na(x$spatialExtent)) {
          moduleDefaults$extent
        } else {
          ext(x$spatialExtent)
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
#' Used to specify a parameter's name, value, and set a default. The `min` and
#' `max` arguments are ignored by `simInit` or `spades`; they
#' are for human use only. To ensure that a user cannot set parameters outside of
#' a range of values, the module developer should use assertions in their module
#' code.
#'
#' @note Be sure to use the correct NA type: logical (`NA`), integer (`NA_integer_`),
#'       real (`NA_real_`), complex (`NA_complex_`), or character (`NA_character_`).
#'       See [NA()].
#'
#' @seealso [P()], [params()] for accessing these parameters in a module.
#'
#' @param name      Character string giving the parameter name.
#' @param class     Character string giving the parameter class.
#' @param default   The default value to use when none is specified by the user.
#'                  Non-standard evaluation is used for the expression.
#' @param min       With `max`, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#'                  *These are not tested by* `simInit` *or*
#'                  `spades`. These are primarily for human use, i.e., to
#'                  tell a module user what values the module expects.
#' @param max       With `min`, used to define a suitable range of values.
#'                  Non-standard evaluation is used for the expression.
#'                  *These are not tested by* `simInit` *or*
#'                  `spades`. These are primarily for human use, i.e., to
#'                  tell a module user what values the module expects.
#' @param desc      Text string providing a brief description of the parameter.
#'                  If there are extra spaces or carriage returns, these will be stripped,
#'                  allowing for multi-line character strings without using `paste`
#'                  or multiple quotes.
#' @param ...       A convenience that allows writing a long `desc` without
#'                  having to use `paste`; any character strings after `desc`
#'                  will be `paste`d together with `desc`.
#'
#' @return a `data.frame`
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @importFrom crayon magenta
#'
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
#' \donttest{
#' # Create a new module, then access parameters using `P`
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
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   mySim <- simInit(modules = "testModule",
#'                    paths = list(modulePath = tmpdir))
#'
#'   # Access one of the parameters -- because this line is not inside a module
#'   #  function, we must specify the module name. If used within a module,
#'   #  we can omit the module name
#'   P(mySim, module = "testModule") # gets all params in a module
#'   P(mySim, ".useCache", "testModule") # just one param
#' }
#' unlink(tmpdir, recursive = TRUE)
#' }
#'
defineParameter <- function(name, class, default, min, max, desc, ...) {
  if (missing(name) && missing(class) && missing(default) && missing(min) && missing(max) && missing(desc))
    return(data.frame(
      paramName = character(0), paramClass = character(0),
      default = I(list()), min = I(list()), max = I(list()),
      paramDesc = character(0), stringsAsFactors = FALSE))
  if (is.character(name) && is.character(class) && missing(min) && missing(max)) {
    NAtypes <- c("character", "complex", "integer", "logical", "numeric") # nolint
    if (class %in% NAtypes) {
      # coerce `min` and `max` to same type as `default`
      min <- as(NA, class)
      max <- as(NA, class)
    } else {
      min <- NA
      max <- NA
    }
  }

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
  # df <- data.table::setDF(list(
  #   paramName = name, paramClass = I(list(class)), default = I(list(default)),
  #   min = I(list(min)), max = I(list(max)), paramDesc = desc))
  df <- data.frame(
    paramName = name, paramClass = I(list(class)), default = I(list(default)),
    min = I(list(min)), max = I(list(max)), paramDesc = desc,
    stringsAsFactors = FALSE)
  # if (!identical(df, df1)) browser()
  return(df)


}
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
#'                  allowing for multi-line character strings without using `paste`
#'                  or multiple quotes.
#'
#' @param sourceURL    Character string to specify an URL to reach the input object,
#'                     default is `NA`.
#'
#' @param ...          Other specifications of the input object.
#'
#' @return A `data.frame` suitable to be passed to `inputObjects` in a module's metadata.
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

    returnDataframe <- as.data.frame(cbind(objectName, objectClass, desc, sourceURL),
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
#'                  allowing for multi-line character strings without using `paste`
#'                  or multiple quotes.
#'
#' @param ...          Other specifications of the output object.
#'
#' @return A `data.frame` suitable to be passed to `outputObjects` in
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

#' An internal function for coercing a data.frame to `inputs()`
#'
#' @param inputDF A data.frame with partial columns to pass to `inputs<-`
#' @param startTime Numeric time. The `start(sim)`.
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

#' An internal function for coercing a data.frame to `outputs()`
#'
#' @param outputDF A data.frame with partial columns to pass to `outputs<-`
#' @param endTime Numeric time. The `end(sim)`.
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

  if (any(!outputDF$saved %in% TRUE)) {
    .fileExts <- .saveFileExtensions()
    fileExtFromReproducible <- getOption("reproducible.cacheSaveFormat", "rds")
    rowInDotFileExts <- which(.fileExts$exts %in% fileExtFromReproducible)

    if (any(is.na(outputDF[, "fun"]))) {
      fl <- outputDF$file
      exts <- fileExt(fl)
      if (any(is.na(fl)) | any(!nzchar(exts, keepNA = TRUE))) {
        outputDF$fun[is.na(fl) | (!nzchar(exts, keepNA = TRUE))] <-
          .fileExts$fun[rowInDotFileExts]
      }
      if (any(is.na(outputDF[, "fun"]))) {
        extsAvail <- checkKnownExts(exts, .fileExts)
        outputDF$fun[is.na(outputDF$fun)] <- .fileExts[extsAvail, "fun"]
      }
    }

    if (any(is.na(outputDF[, "package"]))) {
      fl <- outputDF$file
      exts <- fileExt(fl)
      if (any(is.na(fl)) | any(!nzchar(exts, keepNA = TRUE))) {
        outputDF$package[is.na(fl) | (!nzchar(exts, keepNA = TRUE))] <- .fileExts$package[rowInDotFileExts]
      }
      if (any(is.na(outputDF[, "package"]))) {
        exts <- fileExt(fl)
        extsAvail <- checkKnownExts(exts, .fileExts)
        outputDF$package[is.na(outputDF$package)] <- .fileExts[extsAvail, "package"]
      }
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
extractInequality <- getFromNamespace("extractInequality", "Require")
compareVersion2 <- getFromNamespace("compareVersion2", "Require")
