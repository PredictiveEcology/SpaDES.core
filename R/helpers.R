################################################################################
#' Named list of core \code{SpaDES} modules
#'
#' Internal function.
#'
#' @return Returns a named list of the core modules.
#'
#' @author Alex Chubaty
#' @keywords internal
#' @name .coreModules
#' @rdname coreModules
.coreModules <- function() {
  list(checkpoint = "checkpoint", save = "save", progress = "progress", load = "load")
}

.pkgEnv$.coreModules <- .coreModules() %>% unname()

.pkgEnv$.progressEmpty <- list(type = NA_character_, interval = NA_real_)

################################################################################
#' Blank (template) event list
#'
#' Internal function called from \code{spades}, returning an empty event list.
#'
#' Event lists are sorted (keyed) first by time, second by priority.
#' Each event is represented by a \code{\link{data.table}} row consisting of:
#' \tabular{ll}{
#'   \code{eventTime} \tab The time the event is to occur.\cr
#'   \code{moduleName} \tab The module from which the event is taken.\cr
#'   \code{eventType} \tab A character string for the programmer-defined event type.\cr
#'   \code{eventPriority} \tab The priority given to the event. \cr
#' }
#'
#' @param eventTime      The time the event is to occur.
#' @param moduleName     The module from which the event is taken.
#' @param eventType      A character string for the programmer-defined event type.
#' @param eventPriority  The priority given to the event.
#'
#' @return Returns an empty event list.
#'
#' @author Alex Chubaty
#' @importFrom data.table data.table
#' @keywords internal
#' @name emptyEventList
#' @rdname emptyEventList
.emptyEventListDT <- data.table(eventTime = integer(0L), moduleName = character(0L),
                                eventType = character(0L), eventPriority = numeric(0L))

#' @rdname emptyEventList
#' @importFrom data.table data.table
.singleEventListDT <- data.table(eventTime = integer(1L), moduleName = character(1L),
                          eventType = character(1L), eventPriority = numeric(1L))

#' @rdname emptyEventList
setGeneric(".emptyEventList", function(eventTime, moduleName, eventType, eventPriority) {
  standardGeneric(".emptyEventList")
})

#' @rdname emptyEventList
#' @importFrom data.table set copy
setMethod(
  ".emptyEventList",
  signature(eventTime = "numeric", moduleName = "character",
            eventType = "character", eventPriority = "numeric"),
  definition = function(eventTime, moduleName, eventType, eventPriority) {
    # This is faster than direct call to new data.table
    eeldt <- copy(.singleEventListDT)
    set(eeldt, , "eventTime", eventTime)
    set(eeldt, , "moduleName", moduleName)
    set(eeldt, , "eventType", eventType)
    set(eeldt, , "eventPriority", eventPriority)
    eeldt
    # don't set key because it is set later when used
})

#' @rdname emptyEventList
setMethod(
  ".emptyEventList",
  signature(eventTime = "missing", moduleName = "missing",
            eventType = "missing", eventPriority = "missing"),
  definition = function() {
    copy(.emptyEventListDT)
})

#' @rdname emptyEventList
.emptyEventListCols <- colnames(.emptyEventList())


################################################################################
#' Default (empty) metadata
#'
#' Internal use only.
#' Default values to use for metadata elements when not otherwise supplied.
#'
#' @param x  Not used. Should be missing.
#'
#' @author Alex Chubaty
#' @importFrom raster extent
#' @include simList-class.R
#' @keywords internal
#' @rdname emptyMetadata
#'
setGeneric(".emptyMetadata", function(x) {
  standardGeneric(".emptyMetadata")
})

#' @rdname emptyMetadata
setMethod(
  ".emptyMetadata",
  signature(x = "missing"),
  definition = function() {
    out <- list(
      name = character(0),
      description = character(0),
      keywords = character(0),
      childModules = character(0),
      authors = person("unknown"),
      version = numeric_version(NULL),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = NA_character_,
      citation = list(),
      documentation = list(),
      reqdPkgs = list(),
      parameters = defineParameter(),
      inputObjects = .inputObjects(),
      outputObjects = .outputObjects()
    )
    return(out)
})

#' Find objects if passed as character strings
#'
#' Objects are passed into simList via \code{simInit} call or \code{objects(simList)}
#' assignment. This function is an internal helper to find those objects from their
#' environments by searching the call stack.
#'
#' @param objects A character vector of object names
#' @param functionCall A character string identifying the function name to be
#' searched in the call stack. Default is "simInit"
#'
#' @author Eliot McIntire
#' @keywords internal
#' @name findObjects
#' @rdname findObjects
#'
.findObjects <- function(objects, functionCall = "simInit") {
  scalls <- sys.calls()
  grep1 <- grep(as.character(scalls), pattern = functionCall)
  grep1 <- pmax(min(grep1[sapply(scalls[grep1], function(x) {
    tryCatch(is(parse(text = x), "expression"), error = function(y) NA)
  })], na.rm = TRUE) - 1, 1)
  # Convert character strings to their objects
  lapply(objects, function(x) get(x, envir = sys.frames()[[grep1]]))
}

#' Modify package order in search path
#'
#' Intended for internal use only. It modifies the search path (i.e., \code{search()})
#' such that the packages required by the current module are placed first in the
#' search path. Note, several "core" packages are not touched; or more specifically,
#' they will remain in the search path, but may move down if packages are rearranged.
#' The current set of these core packages used by SpaDES can be found here:
#' \code{SpaDES.core:::.pkgEnv$corePackages}
#'
#' @param pkgs The packages that are to be placed at the beginning of the search path,
#'
#' @param removeOthers Logical. If \code{TRUE}, then only the packages in \code{pkgs}
#'                     will remain in the search path, i.e., all others will be removed.
#'
#' @param skipNamespacing Logical. If \code{FALSE}, then the running of an event in a module
#'                        will not trigger a rearrangement of the search() path. This will
#'                        generally speed up module simulations, but may create name
#'                        conflicts between packages.
#'
#' @return Nothing. This is used for its side effects, which are "severe".
#'
#' @author Eliot McIntire
#' @keywords internal
#' @rdname modifySearchPath
.modifySearchPath <- function(pkgs, removeOthers = FALSE,
                              skipNamespacing = !getOption("spades.switchPkgNamespaces")) {
  if (!skipNamespacing) {
    pkgs <- c("SpaDES.core", pkgs)
    pkgs <- unlist(pkgs)[!(pkgs %in% .pkgEnv$corePackagesVec)]
    pkgPositions <- pmatch(paste0("package:",unlist(pkgs)), search())

    # Find all packages that are not in the first sequence after .GlobalEnv
    whNotAtTop <- !((seq_along(pkgPositions) + 1) %in% pkgPositions)

    if (any(whNotAtTop)) {
      if (removeOthers) {
        pkgs <- setdiff(search(), pkgs)
        pkgs <- grep(pkgs, pattern = .pkgEnv$corePackages, invert = TRUE, value = TRUE)
        whRm <- seq_along(pkgs)
      } else {
        whRm <- which(pkgPositions > min(which(whNotAtTop)))
        whAdd <- which(is.na(pkgPositions))
      }

      if (length(whRm) > 0) { # i.e,. ones that need reordering
        suppressWarnings(
          lapply(unique(gsub(pkgs, pattern = "package:", replacement = "")[whRm]), function(pack) {
            try(detach(paste0("package:", pack), character.only = TRUE), silent = TRUE)
          })
        )
      }
      if (!removeOthers) {
        if (length(c(whAdd, whRm))) {
          suppressMessages(
            lapply(rev(pkgs[c(whAdd, whRm)]), function(pack) {
              try(attachNamespace(pack), silent = TRUE)
            })
          )
        }
      }
    }
  }

}


.pkgEnv$corePackages <- ".GlobalEnv|Autoloads|SpaDES.core|base|methods|utils|graphics|datasets|stats"

.pkgEnv$corePackagesVec <- unlist(strsplit(.pkgEnv$corePackages, split = "\\|"))
.pkgEnv$corePackagesVec <- c(.pkgEnv$corePackagesVec[(1:2)], paste0("package:",.pkgEnv$corePackagesVec[-(1:2)]))
