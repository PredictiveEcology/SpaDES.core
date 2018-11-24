if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("newQuantity", "quantityAdj", "quantityAdj2"))
}

#' A slightly modified version of getOption
#'
#' This can take x as a character string or as a function that returns a character string.
#'
#' @inheritParams base::getOption
#' @rdname spadesOptions
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
#' Merge two named list based on their named entries. Where
#' any element matches in both lists, the value from the
#' second list is used in the updated list.
#' Subelements are not examined and are simply replaced. If one list is empty, then
#' it returns the other one, unchanged.
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
#' @rdname updateList
#'
#' @examples
#' L1 <- list(a = "hst", b = NA_character_, c = 43)
#' L2 <- list(a = "gst", c = 42, d = list(letters))
#' updateList(L1, L2)
#'
#' updateList(L1, NULL)
#' updateList(NULL, L2)
#' updateList(NULL, NULL) # should return empty list
#'
setGeneric("updateList", function(x, y) {
  standardGeneric("updateList")
})

#' @rdname updateList
setMethod("updateList",
          signature = c("list", "list"),
          definition = function(x, y) {
            if (any(is.null(names(x)), is.null(names(y)))) {
              # If one of the lists is empty, then just return the other, unchanged
              if (length(y) == 0) return(x)
              if (length(x) == 0) return(y)
              stop("All elements in lists x,y must be named.")
            } else {
              x[names(y)] <- y
              return(x[order(names(x))])
            }
})

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "list"),
          definition = function(x, y) {
            if (is.null(names(y))) {
              if (length(y) == 0) return(x)
              stop("All elements in list y must be named.")
            }
            return(y[order(names(y))])
})

#' @rdname updateList
setMethod("updateList",
          signature = c("list", "NULL"),
          definition = function(x, y) {
            if (is.null(names(x))) {
              if (length(x) == 0) return(x)
              stop("All elements in list x must be named.")
            }
            return(x[order(names(x))])
})

#' @rdname updateList
setMethod("updateList",
          signature = c("NULL", "NULL"),
          definition = function(x, y) {
            return(list())
})

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

################################################################################
#' Load packages.
#'
#' Load and optionally install additional packages.
#'
#' @param packageList A list of character strings specifying
#' the names of packages to be loaded.
#'
#' @param install Logical flag. If required packages are not
#' already installed, should they be installed?
#'
#' @param quiet Logical flag. Should the final "packages loaded"
#' message be suppressed?
#'
#' @return Specified packages are loaded and attached using \code{require()},
#'         invisibly returning a logical vector of successes.
#'
#' @seealso \code{\link{require}}.
#'
#' @export
#' @rdname loadPackages
#' @importFrom utils install.packages installed.packages
#'
#' @author Alex Chubaty
#'
#' @examples
#' \dontrun{
#'   pkgs <- list("raster", "lme4")
#'   loadPackages(pkgs) # loads packages if installed
#'   loadPackages(pkgs, install = TRUE) # loads packages after installation (if needed)
#' }
#'
setGeneric("loadPackages", function(packageList, install = FALSE, quiet = TRUE) {
  standardGeneric("loadPackages")
})

#' @rdname loadPackages
setMethod(
  "loadPackages",
  signature = "character",
  definition = function(packageList, install, quiet) {
    packageList <- na.omit(packageList) %>% as.character()
    if (length(packageList)) {
      if (install) {
        repos <- getOption("repos")
        if (is.null(repos) | any(repos == "")) {
          repos <- "https://cran.rstudio.com"
        }
        installed <- unname(installed.packages()[, "Package"])
        toInstall <- packageList[packageList %in% installed]
        install.packages(toInstall, repos = repos)
      }

      loaded <- suppressMessages(sapply(packageList, require, character.only = TRUE,
                                        quiet = TRUE, warn.conflicts = FALSE))
      if (any(!loaded)) {
        alreadyLoaded <- unlist(lapply(packageList[!loaded], isNamespaceLoaded))
        if (!all(alreadyLoaded)) {
          stop("Some packages required for the simulation are not installed:\n",
             "    ", paste(names(loaded[-which(loaded)]), collapse = "\n    "))
        } else {
          message("Older version(s) of ",
                  paste(collapse = ", ", packageList[!loaded]), " already loaded")
        }
      }

      if (!quiet) {
        message(paste("Loaded", length(which(loaded == TRUE)), "of",
                      length(packageList), "packages.", sep = " "))
      }
    } else {
      loaded <- character(0)
    }
    return(invisible(loaded))
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "list",
          definition = function(packageList, install, quiet) {
            loadPackages(unlist(packageList), install, quiet)
})

#' @rdname loadPackages
setMethod("loadPackages",
          signature = "NULL",
          definition = function(packageList, install, quiet) {
            return(invisible(character(0)))
})


###############################################################
#' Convert numeric to character with padding
#'
#' @param x numeric. Number to be converted to character with padding
#'
#' @param padL numeric. Desired number of digits on left side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param padR numeric. Desired number of digits on right side of decimal.
#'              If not enough, \code{pad} will be used to pad.
#'
#' @param pad character to use as padding (\code{nchar(pad)==1} must be \code{TRUE}).
#'            Passed to \code{\link[stringi]{stri_pad}}
#'
#' @return Character string representing the filename.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom fpCompare %==%
#' @importFrom stringi stri_pad_left stri_pad_right
#' @rdname paddedFloatToChar
#'
#' @examples
#' paddedFloatToChar(1.25)
#' paddedFloatToChar(1.25, padL = 3, padR = 5)
#'
# igraph exports %>% from magrittr
paddedFloatToChar <- function(x, padL = ceiling(log10(x + 1)), padR = 3, pad = "0") {
  xIC <- x %/% 1 %>%
    format(., trim = TRUE, digits = 5, scientific = FALSE) %>%
    stri_pad_left(., pad = pad, width = padL)
  xf <- x %% 1
  xFC <- ifelse(xf %==% 0, "",
    strsplit(format(xf, digits = padR, scientific = FALSE), split = "\\.")[[1]][2] %>%
      stri_pad_right(., width = padR, pad = pad) %>%
      paste0(".", .))

  return(paste0(xIC, xFC))
}

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
#' Based on \url{http://stackoverflow.com/a/5158978/1380598}.
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
#' See \url{http://stackoverflow.com/a/27923346/1380598}.
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
#' @return Returns a named list of the user's default working directories.
#' \code{setPaths} is invoked for the side effect of setting these directories.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @name setPaths
#' @rdname setPaths
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
  # message("Running:\n",
  #         "  getOption('reproducible.cachePath')\n",
  #         "  getOption('spades.inputPath')\n",
  #         "  getOption('spades.outputPath')\n",
  #         "  getOption('spades.modulePath')\n",
  #         "  )")

  list(
    cachePath = .getOption("reproducible.cachePath"),
    inputPath = getOption("spades.inputPath"),
    modulePath = getOption("spades.modulePath"),
    outputPath = getOption("spades.outputPath")
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
#' @importFrom reproducible checkPath
#' @importFrom R.utils getOption
setPaths <- function(cachePath, inputPath, modulePath, outputPath) {
  defaults <- list(
    CP = FALSE,
    IP = FALSE,
    MP = FALSE,
    OP = FALSE
  )
  if (missing(cachePath)) {
    cachePath <- .getOption("reproducible.cachePath")     # nolint
    defaults$CP <- TRUE
  }
  if (missing(inputPath)) {
    inputPath <- getOption("spades.inputPath")    # nolint
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

  allDefault <- all(unlist(defaults))

  suppressMessages(originalPaths <- .paths())
  options(spades.inputPath = inputPath,
          spades.modulePath = unlist(modulePath), spades.outputPath = outputPath,
          reproducible.cachePath = cachePath)

  if (!allDefault) {
    message("Setting:\n",
            "  options(\n",
            if (!defaults$CP) paste0("    reproducible.cachePath = '",normPath(cachePath),"'\n"),
            if (!defaults$IP) paste0("    spades.inputPath = '",normPath(inputPath),"'\n"),
            if (!defaults$OP) paste0("    spades.outputPath = '",normPath(outputPath),"'\n"),
            if (!defaults$MP) paste0("    spades.modulePath = '",normPath(modulePath),"'\n"),
            "  )")
  }
  message("Paths set to:\n",
            "  options(\n",
            "    reproducible.cachePath = '",normPath(cachePath),"'\n",
            "    spades.inputPath = '",normPath(inputPath),"'\n",
            "    spades.outputPath = '",normPath(outputPath),"'\n",
            "    spades.modulePath = '",normPath(modulePath),"'\n",
            "  )")

  suppressMessages(lapply(.paths(), checkPath, create = TRUE))
  return(invisible(originalPaths))

}

.basename <- function (x) {
  if (is.null(x)) {
    NULL
  }
  else {
    basename(x)
  }
}
