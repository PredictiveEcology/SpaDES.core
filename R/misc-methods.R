utils::globalVariables(c("newQuantity", "quantityAdj", "quantityAdj2"))

#' A slightly modified version of getOption
#'
#' This can take x as a character string or as a function that returns a character string.
#'
#' @inheritParams base::getOption
#' @rdname getOption
#' @keywords internal
.getOption <- function(x, default = NULL) {
  optionDefault <- options(x)[[1]]
  if (is.null(optionDefault)) optionDefault <- default
  if (is.function(optionDefault)) {
    optionDefault()
  } else {
    optionDefault
  }
}

################################################################################
#' Update elements of a named list with elements of a second named list
#'
#' Being deprecated. Use \code{\link[utils]{modifyList}} (which can not handle NULL) or
#' \code{\link[Require]{modifyList2}} for case with >2 lists and can handle NULL lists.
#'
#' @param x   a named list
#' @param y   a named list
#'
#' @return A named list, with elements sorted by name.
#'          The values of matching elements in list \code{y}
#'          replace the values in list \code{x}.
#'
#' @author Alex Chubaty
#' @export
#' @importFrom Require modifyList2
#' @rdname updateList
#'
updateList <- function(x, y) {
  .Deprecated("Require::modifyList2", "Require")
  if (missing(x)) x <- list()
  if (missing(y)) y <- list()
  if (is.null(y)) y <- list()
  if (is.null(x)) x <- list()
  modifyList2(x = x, val = y)
}


################################################################################
#' Add a module to a \code{moduleList}
#'
#' Ordinary base lists and vectors do not retain their attributes when subsetted
#' or appended. This function appends items to a list while preserving the
#' attributes of items in the list (but not of the list itself).
#'
#' Similar to \code{updateList} but does not require named lists.
#'
#' @param x,y  A \code{list} of items with optional attributes.
#'
#' @return An updated \code{list} with attributes.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @rdname append_attr
#'
#' @examples
#' library(igraph) # igraph exports magrittr's pipe operator
#' tmp1 <- list("apple", "banana") %>% lapply(., `attributes<-`, list(type = "fruit"))
#' tmp2 <- list("carrot") %>% lapply(., `attributes<-`, list(type = "vegetable"))
#' append_attr(tmp1, tmp2)
#' rm(tmp1, tmp2)
setGeneric("append_attr", function(x, y) {
  standardGeneric("append_attr")
})

#' @export
#' @rdname append_attr
setMethod("append_attr",
          signature = c(x = "list", y = "list"),
          definition = function(x, y) {
            attrs <- c(lapply(x, attributes), lapply(y, attributes))
            out <- append(x, y)
            if (length(out)) {
              for (i in length(out)) {
                attributes(out[i]) <- attrs[[i]]
              }
            }
            dups <- duplicated(out) # unique strips names ... out[!dups] does not
            return(out[!dups])
})

###############################################################################
#' Generate random strings
#'
#' Generate a vector of random alphanumeric strings each of an arbitrary length.
#'
#' @param n   Number of strings to generate (default 1).
#'            Will attempt to coerce to integer value.
#'
#' @param len Length of strings to generate (default 8).
#'            Will attempt to coerce to integer value.
#'
#' @param characterFirst Logical, if \code{TRUE}, then a letter will be the
#'        first character of the string (useful if being used for object names).
#'
#' @return Character vector of random strings.
#'
#' @export
#' @rdname rndstr
#'
#' @author Alex Chubaty and Eliot McIntire
#' @examples
#' set.seed(11)
#' rndstr()
#' rndstr(len = 10)
#' rndstr(characterFirst = FALSE)
#' rndstr(n = 5, len = 10)
#' rndstr(n = 5)
#' rndstr(n = 5, characterFirst = TRUE)
#' rndstr(len = 10, characterFirst = TRUE)
#' rndstr(n = 5, len = 10, characterFirst = TRUE)
#'
setGeneric("rndstr", function(n, len, characterFirst) {
  standardGeneric("rndstr")
})

#' @rdname rndstr
setMethod(
  "rndstr",
  signature(n = "numeric", len = "numeric", characterFirst = "logical"),
  definition = function(n, len, characterFirst) {
    if (!((n > 0) & (len > 0))) {
      stop("rndstr requires n > 0 and len > 0")
    }

    unlist(lapply(character(as.integer(n)), function(x) {
      i <- as.integer(characterFirst)
      x <- paste0(c(sample(c(letters, LETTERS), size = i),
                    sample(c((0:9), letters, LETTERS),
                           size = as.integer(len) - i, replace = TRUE)),
                  collapse = "")
      }))
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "numeric", characterFirst = "missing"),
          definition = function(n, len) {
            rndstr(n = n, len = len, characterFirst = TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "missing", characterFirst = "logical"),
          definition = function(n, characterFirst) {
            rndstr(n = n, len = 8, characterFirst = characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "numeric", characterFirst = "logical"),
          definition = function(len, characterFirst) {
            rndstr(n = 1, len = len, characterFirst = characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "numeric", len = "missing", characterFirst = "missing"),
          definition = function(n) {
            rndstr(n = n, len = 8, characterFirst = TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "numeric", characterFirst = "missing"),
          definition = function(len) {
            rndstr(n = 1, len = len, characterFirst = TRUE)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "missing", characterFirst = "logical"),
          definition = function(characterFirst) {
            rndstr(n = 1, len = 8, characterFirst = characterFirst)
})

#' @rdname rndstr
setMethod("rndstr",
          signature(n = "missing", len = "missing", characterFirst = "missing"),
          definition = function(n, len, characterFirst) {
            rndstr(n = 1, len = 8, characterFirst = TRUE)
})

################################################################################
#' Filter objects by class
#'
#' Based on \url{https://stackoverflow.com/a/5158978/1380598}.
#'
#' @param x Character vector of object names to filter, possibly from \code{ls}.
#'
#' @param include   Class(es) to include, as a character vector.
#'
#' @param exclude   Optional class(es) to exclude, as a character vector.
#'
#' @param envir     The environment ins which to search for objects.
#'                  Default is the calling environment.
#'
#' @return Vector of object names matching the class filter.
#'
#' @note \code{\link{inherits}} is used internally to check the object class,
#' which can, in some cases, return results inconsistent with \code{is}.
#' See \url{https://stackoverflow.com/a/27923346/1380598}.
#' These (known) cases are checked manually and corrected.
#'
#' @export
#' @rdname classFilter
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   ## from global environment
#'   a <- list(1:10)     # class `list`
#'   b <- letters        # class `character`
#'   d <- stats::runif(10)      # class `numeric`
#'   f <- sample(1L:10L) # class `numeric`, `integer`
#'   g <- lm( jitter(d) ~ d ) # class `lm`
#'   h <- glm( jitter(d) ~ d ) # class `lm`, `glm`
#'   classFilter(ls(), include=c("character", "list"))
#'   classFilter(ls(), include = "numeric")
#'   classFilter(ls(), include = "numeric", exclude = "integer")
#'   classFilter(ls(), include = "lm")
#'   classFilter(ls(), include = "lm", exclude = "glm")
#'   rm(a, b, d, f, g, h)
#' }
#'
#' ## from local (e.g., function) environment
#' local({
#'   e <- environment()
#'   a <- list(1:10)     # class `list`
#'   b <- letters        # class `character`
#'   d <- stats::runif(10)      # class `numeric`
#'   f <- sample(1L:10L) # class `numeric`, `integer`
#'   g <- lm( jitter(d) ~ d ) # class `lm`
#'   h <- glm( jitter(d) ~ d ) # class `lm`, `glm`
#'   classFilter(ls(), include=c("character", "list"), envir = e)
#'   classFilter(ls(), include = "numeric", envir = e)
#'   classFilter(ls(), include = "numeric", exclude = "integer", envir = e)
#'   classFilter(ls(), include = "lm", envir = e)
#'   classFilter(ls(), include = "lm", exclude = "glm", envir = e)
#'   rm(a, b, d, e, f, g, h)
#' })
#'
#' ## from another environment
#' e = new.env(parent = emptyenv())
#' e$a <- list(1:10)     # class `list`
#' e$b <- letters        # class `character`
#' e$d <- stats::runif(10)      # class `numeric`
#' e$f <- sample(1L:10L) # class `numeric`, `integer`
#' e$g <- lm( jitter(e$d) ~ e$d ) # class `lm`
#' e$h <- glm( jitter(e$d) ~ e$d ) # class `lm`, `glm`
#' classFilter(ls(e), include=c("character", "list"), envir = e)
#' classFilter(ls(e), include = "numeric", envir = e)
#' classFilter(ls(e), include = "numeric", exclude = "integer", envir = e)
#' classFilter(ls(e), include = "lm", envir = e)
#' classFilter(ls(e), include = "lm", exclude = "glm", envir = e)
#' rm(a, b, d, f, g, h, envir = e)
#' rm(e)
#'
setGeneric("classFilter", function(x, include, exclude, envir) {
  standardGeneric("classFilter")
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "character",
            envir = "environment"),
  definition = function(x, include, exclude, envir) {
    f <- function(w) {
      # -------------------- #
      # using `inherits` doesn't work as expected in some cases,
      #  so we tweak the 'include' to work with those cases:
      if (("numeric" %in% include) &
          (inherits(get(w, envir = envir), "integer")) ) {
        include <- c(include, "integer")
      }
      # --- end tweaking --- #

      if (is.na(exclude)) {
        inherits(get(w, envir = envir), include)
      } else {
        inherits(get(w, envir = envir), include) &
          !inherits(get(w, envir = envir), exclude)
      }
    }
    return(Filter(f, x))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "character",
            envir = "missing"),
  definition = function(x, include, exclude) {
    return(classFilter(x, include, exclude, envir = sys.frame(-1)))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "missing",
            envir = "environment"),
  definition = function(x, include, envir) {
    return(classFilter(x, include, exclude = NA_character_, envir = envir))
})

#' @rdname classFilter
setMethod(
  "classFilter",
  signature(x = "character", include = "character", exclude = "missing",
            envir = "missing"),
  definition = function(x, include) {
    return(classFilter(x, include, exclude = NA_character_, envir = sys.frame(-1)))
})

################################################################################
#' Create empty fileTable for inputs and outputs
#'
#' Internal functions.
#' Returns an empty fileTable to be used with inputs and outputs.
#'
#' @param x  Not used (should be missing)
#'
#' @return An empty data.frame with structure needed for input/output fileTable.
#'
#' @keywords internal
#' @rdname fileTable
#'
setGeneric(".fileTableIn", function(x) {
  standardGeneric(".fileTableIn")
})

#' @rdname fileTable
setMethod(
  ".fileTableIn",
  signature = "missing",
  definition = function() {
    ft <- data.frame(
      file = character(0), fun = character(0), package = character(0),
      objectName = character(0), loadTime = numeric(0), loaded = logical(0),
      arguments = I(list()), intervals = numeric(0), stringsAsFactors = FALSE
    )
    return(ft)
  })

#' @rdname fileTable
.fileTableInCols <- colnames(.fileTableIn())

#' @rdname fileTable
.fileTableInDF <- .fileTableIn()

#' @rdname fileTable
setGeneric(".fileTableOut", function(x) {
  standardGeneric(".fileTableOut")
})

#' @rdname fileTable
setMethod(
  ".fileTableOut",
  signature = "missing",
  definition = function() {
    ft <- data.frame(
      file = character(0), fun = character(0), package = character(0),
      objectName = character(0), saveTime = numeric(0), saved = logical(0),
      arguments = I(list()), stringsAsFactors = FALSE
    )
    return(ft)
})

#' @rdname fileTable
.fileTableOutCols <- colnames(.fileTableOut())

#' @rdname fileTable
.fileTableOutDF <- .fileTableOut()

################################################################################
#' Get and set default working directories
#'
#' Wrapper functions to access the packages options for default working directories.
#' Note: there is an active binding made to \code{Paths}, so a user can use
#' \code{Paths$cachePath} for example instead of \code{getPaths()$cachePath}
#'
#' @param cachePath   The default local directory in which to cache simulation outputs.
#'                    If not specified, defaults to \code{getOption("reproducible.cachePath")}.
#'
#' @param inputPath   The default local directory in which to look for simulation inputs
#'                    If not specified, defaults to \code{getOption("spades.inputPath")}.
#'
#' @param modulePath  The default local directory where modules and data will be
#'                    downloaded and stored.
#'                    If not specified, defaults to \code{getOption("spades.modulePath")}.
#'
#' @param outputPath  The default local directory in which to save simulation outputs.
#'                    If not specified, defaults to \code{getOption("spades.outputPath")}.
#'
#' @param rasterPath  The default local directory in which to save transient raster files.
#'                    If not specified, defaults to \code{\link[raster:rasterOptions]{tmpDir}}.
#'                    \emph{Important note:} this location may not be cleaned up automatically,
#'                    so be sure to monitor this directory and remove unnecessary temp files
#'                    that may contribute to excessive disk usage.
#'
#' @return Returns a named list of the user's default working directories.
#' \code{setPaths} is invoked for the side effect of setting these directories.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @importFrom raster tmpDir
#' @name setPaths
#' @rdname setPaths
#' @export
#'
#' @examples
#' \dontrun{
#' getPaths()                       ## returns the current default working paths
#' setPaths(cachePath = tempdir())  ## sets custom cachePath with other paths default
#' setPaths(inputPath = tempdir())  ## sets custom inputPath with other paths default
#' setPaths(modulePath = tempdir()) ## sets custom modulePath with other paths default
#' setPaths(outputPath = tempdir()) ## sets custom outputPath with other paths default
#'
#' # NOTE: on loading and attaching SpaDES.core,
#' # an active binding is made to "Paths"
#'
#' getPaths()
#' Paths # same
#' setPaths(outputPath = tempdir())
#' Paths # shows change
#' }
#'
.paths <- function() {
  if (!is.null(.getOption("spades.cachePath"))) {
    message("option('spades.cachePath') is being deprecated. Please use ",
            "option('reproducible.cachePath').\n",
            "Setting option('reproducible.cachePath' = getOption('spades.cachePath'))")
  }

  list(
    cachePath = .getOption("reproducible.cachePath"),
    inputPath = getOption("spades.inputPath"),
    modulePath = getOption("spades.modulePath"),
    outputPath = getOption("spades.outputPath"),
    rasterPath = tmpDir()
  )
}

#' @export
#' @rdname setPaths
getPaths <- function() {
  return(.paths())
}

#' @export
#' @rdname setPaths
Paths <- .paths()

#' @export
#' @rdname setPaths
#' @importFrom raster tmpDir
#' @importFrom Require checkPath
#' @param silent Logical. Should the messaging occur.
setPaths <- function(cachePath, inputPath, modulePath, outputPath, rasterPath, silent = FALSE) {
  defaults <- list(
    CP = FALSE,
    IP = FALSE,
    MP = FALSE,
    OP = FALSE,
    RP = FALSE
  )
  if (missing(cachePath)) {
    cachePath <- .getOption("reproducible.cachePath") # nolint
    defaults$CP <- TRUE
  }
  if (missing(inputPath)) {
    inputPath <- getOption("spades.inputPath") # nolint
    defaults$IP <- TRUE
  }
  if (missing(modulePath)) {
    modulePath <- getOption("spades.modulePath") # nolint
    defaults$MP <- TRUE
  }
  if (missing(outputPath)) {
    outputPath <- getOption("spades.outputPath") # nolint
    defaults$OP <- TRUE
  }
  if (missing(rasterPath)) {
    rasterPath <- tmpDir()
    defaults$RP <- TRUE
  }

  allDefault <- all(unlist(defaults))

  originalPaths <- .paths()
  options(rasterTmpDir = rasterPath,
          reproducible.cachePath = cachePath,
          spades.inputPath = inputPath,
          spades.modulePath = unlist(modulePath),
          spades.outputPath = outputPath)

  modPaths <- if (length(modulePath) > 1) {
    paste0("c('", paste(normPath(modulePath), collapse = "', '"), "')")
  } else {
    normPath(modulePath)
  }

  if (!silent) {
    if (!allDefault) {
      message(
        "Setting:\n",
        "  options(\n",
        if (!defaults$RP) paste0("    rasterTmpDir = '", normPath(rasterPath), "'\n"),
        if (!defaults$CP) paste0("    reproducible.cachePath = '", normPath(cachePath), "'\n"),
        if (!defaults$IP) paste0("    spades.inputPath = '", normPath(inputPath), "'\n"),
        if (!defaults$OP) paste0("    spades.outputPath = '", normPath(outputPath), "'\n"),
        if (!defaults$MP) paste0("    spades.modulePath = '" , modPaths, "'\n"),
        "  )"
      )
    }

    if (any(unlist(defaults))) {
      message(
        "Paths set to:\n",
        "  options(\n",
        "    rasterTmpDir = '", normPath(rasterPath), "'\n",
        "    reproducible.cachePath = '", normPath(cachePath), "'\n",
        "    spades.inputPath = '", normPath(inputPath), "'\n",
        "    spades.outputPath = '", normPath(outputPath), "'\n",
        "    spades.modulePath = '", modPaths, "'\n", # normPath'ed above
        "  )"
      )
    }
  }

  lapply(.paths(), checkPath, create = TRUE)
  return(invisible(originalPaths))
}

#' Simple wrapper around \code{data.table::rbindlist}
#'
#' This simply sets defaults to \code{fill = TRUE}, and
#' \code{use.names = TRUE}
#'
#' @param ... 1 or more \code{data.frame}, \code{data.table}, or \code{list} objects
#' @export
bindrows <- function(...) {
  # Deal with things like "trailing commas"
  rws <- try(list(...), silent = TRUE)
  if (any(grepl("argument is missing|bind_rows", rws))) {
    ll <- as.list(match.call(expand.dots = TRUE))
    nonEmpties <- unlist(lapply(ll, function(x) any(nchar(x) > 0)))
    eval(as.call(ll[nonEmpties]))
  } else if (is(rws, "try-error")) {
    stop(rws)
  } else {
    rbindlist(rws, fill = TRUE, use.names = TRUE)
  }
}

#' Extract the full file paths for R source code
#'
#' This can be used e.g., for Caching, to identify which files have changed.
#'
#' @export
#' @inheritParams simInit
moduleCodeFiles <- function(paths, modules) {
  path.expand(c(dir(file.path(paths$modulePath, modules, "R"), full.names = TRUE),
    file.path(paths$modulePath, modules, paste0(modules, ".R"))))
}



#' Test and update a parameter against same parameter in other modules
#'
#' This function is intended to be part of module code and will test whether
#' the value of a parameter within the current module matches the value of the
#' same parameter in other modules. This is a test for parameters that might expect
#' to be part of a \code{params = list(.globals = list(someParam = "test"))} passed
#' to the \code{simInit}
#'
#' @return
#' If the value of the \code{paramToCheck} in the current module is either \code{NULL} or
#' \code{"default"}, and there is only one other value across all modules named in \code{moduleToUse},
#' then this will return a character string with the value of the single parameter value
#' in the other module(s). It will return the current value if there are no other modules
#' with the same parameter.
#'
#' It is considered a "fail" under several conditions:
#' \enumerate{
#'   \item current module has a value that is not \code{NULL} or \code{"default"} and another module
#'     has a different value;
#'   \item there is more than one value for the \code{paramToCheck} in the other modules,
#'     so it is ambiguous which one to return.
#' }
#'
#' \code{} either the current module is different than other modules,
#' unless it is "default" or NULL.
#'
#' @export
#' @rdname paramCheckOtherMods
#' @param sim A simList
#' @param paramToCheck A character string, length one, of a parameter name to
#'   check and compare between the current module and one or more or all others
#' @param moduleToUse A character vector of module names to check against. This can be
#'   \code{"all"} which will compare against all other modules.
#' @param ifSetButDifferent A character string indicating whether to \code{"error"}
#'   the default, or send a \code{"warning"}, \code{message} or just silently continue
#'   (any other value).
paramCheckOtherMods <- function(sim, paramToCheck, moduleToUse = "all",
                                ifSetButDifferent = c("error", "warning", "message", "silent")) {
  currentModule <- currentModule(sim)
  paramsInSim <- params(sim)
  paramInCurrentMod <- P(sim)[[paramToCheck]]
  if (identical(moduleToUse, "all")) {
    moduleToUse <- names(paramsInSim)
  }
  paramInThisMod <- paramsInSim[[currentModule]][[paramToCheck]]
  params <- paramsInSim[setdiff(moduleToUse, currentModule)]

  paramToUpdateValInOtherMods <- unlist(lapply(params, function(p) p[[paramToCheck]]))
  paramInOtherMods <- unique(paramToUpdateValInOtherMods)

  messSuff <- paste0("); they should not. Perhaps pass params = list(.globals = list(",
                     paramToCheck, " = '", paramInOtherMods[1], "')) in the simInit call?")

  newVal <- paramInThisMod
  fail <- FALSE

  if (is.null(paramInThisMod) || identical("default", paramInThisMod)) {
    if (length(paramInOtherMods) == 1) {
      newVal <- paramInOtherMods
      message(paramToCheck, " in ", currentModule," is set to 'default' or NULL;")
      message("... setting to '", newVal,
              "' to match value in ",paste(names(paramToUpdateValInOtherMods), collapse = ", ")," in the simList")
    } else if (length(paramInOtherMods) > 1) {
      mess <- paste0("Modules in this simList have multiple values for ",paramToCheck," (",
                     paste(paramInOtherMods, collapse = ", "),
                     messSuff)
      fail <- TRUE
    }
  } else {
    if (length(paramInOtherMods) > 0) {
      mess <- paste0("Including this module, there are multiple values for ",paramToCheck," (",
                     paste(c(paramInThisMod, paramInOtherMods), collapse = ", "),
                     messSuff)
      fail <- TRUE
    }
  }
  if (isTRUE(fail)) {
    if (identical(ifSetButDifferent[1], "error")) stop(mess)
    if (identical(ifSetButDifferent[1], "warning")) warning(mess)
    if (identical(ifSetButDifferent[1], "message")) message(mess)
  }

  newVal
}
