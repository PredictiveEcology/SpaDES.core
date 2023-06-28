################################################################################
#' Named list of core `SpaDES` modules
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
  list(checkpoint = "checkpoint", save = "save", progress = "progress", load = "load",
       restartR = "restartR")
}

#' @keywords internal
#' @include environment.R
.pkgEnv$.coreModules <- .coreModules() %>% unname()

#' @keywords internal
#' @include environment.R
.pkgEnv$.coreModulesMinusSave <- setdiff(.coreModules(), "save") %>% unname()

#' @keywords internal
.pkgEnv$.progressEmpty <- list(type = NA_character_, interval = NA_real_)

################################################################################
#' Blank (template) event list
#'
#' Internal function called from `spades`, returning an empty event list.
#'
#' Event lists are sorted (keyed) first by time, second by priority.
#' Each event is represented by a [data.table()] row consisting of:
#' \tabular{ll}{
#'   `eventTime` \tab The time the event is to occur.\cr
#'   `moduleName` \tab The module from which the event is taken.\cr
#'   `eventType` \tab A character string for the programmer-defined event type.\cr
#'   `eventPriority` \tab The priority given to the event. \cr
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

#' @importFrom data.table data.table
#' @keywords internal
#' @rdname emptyEventList
.singleEventListDT <- data.table(eventTime = integer(1L), moduleName = character(1L),
                                 eventType = character(1L), eventPriority = numeric(1L))

#' @keywords internal
#' @rdname emptyEventList
setGeneric(".emptyEventList", function(eventTime, moduleName, eventType, eventPriority) {
  standardGeneric(".emptyEventList")
})

#' @keywords internal
#' @rdname emptyEventList
#' @importFrom data.table set copy
setMethod(
  ".emptyEventList",
  signature(eventTime = "numeric", moduleName = "character",
            eventType = "character", eventPriority = "numeric"),
  definition = function(eventTime, moduleName, eventType, eventPriority) {
    # This is faster than direct call to new data.table
    eeldt <- copy(.singleEventListDT)
    set(eeldt, NULL, "eventTime", eventTime)
    set(eeldt, NULL, "moduleName", moduleName)
    set(eeldt, NULL, "eventType", eventType)
    set(eeldt, NULL, "eventPriority", eventPriority)
    eeldt # don't set key because it is set later when used
})

#' @keywords internal
#' @rdname emptyEventList
setMethod(
  ".emptyEventList",
  signature(eventTime = "missing", moduleName = "missing",
            eventType = "missing", eventPriority = "missing"),
  definition = function() {
    copy(.emptyEventListDT)
})

#' @keywords internal
#' @rdname emptyEventList
.emptyEventListCols <- colnames(.emptyEventList())

#' Default (empty) metadata
#'
#' Internal use only.
#' Default values to use for metadata elements when not otherwise supplied.
#'
#' @param x  Not used. Should be missing.
#'
#' @author Alex Chubaty
#' @importFrom terra ext
#' @include simList-class.R
#' @keywords internal
#' @rdname emptyMetadata
setGeneric(".emptyMetadata", function(x) {
  standardGeneric(".emptyMetadata")
})

#' @rdname emptyMetadata
setMethod(
  ".emptyMetadata",
  signature(x = "missing"),
  definition = function() {
    out <- list(
      name = moduleDefaults[["name"]],
      description = moduleDefaults[["description"]],
      keywords = moduleDefaults[["keywords"]],
      childModules = moduleDefaults[["childModules"]],
      authors = moduleDefaults[["authors"]],
      version = moduleDefaults[["version"]],
      spatialExtent = terra::ext(rep(0, 4)), ## match up with moduleDefaults
      timeframe = as.POSIXlt(c(NA, NA)),                ## match up with moduleDefaults
      timeunit = moduleDefaults[["timeunit"]],
      citation = moduleDefaults[["citation"]],
      documentation = moduleDefaults[["documentation"]],
      reqdPkgs = moduleDefaults[["reqdPkgs"]],
      parameters = defineParameter(),
      inputObjects = ._inputObjectsDF(),
      outputObjects = ._outputObjectsDF()
    )
    return(out)
})

#' Find objects if passed as character strings
#'
#' Objects are passed into simList via `simInit` call or `objects(simList)`
#' assignment. This function is an internal helper to find those objects from their
#' environments by searching the call stack.
#'
#' @param objects A character vector of object names
#' @param functionCall A character string identifying the function name to be
#' searched in the call stack. Default is "simInit"
#'
#' @author Eliot McIntire
#' @importFrom reproducible .grepSysCalls
#' @keywords internal
.findObjects <- function(objects, functionCall = "simInit") {
  scalls <- sys.calls()
  grep1 <- .grepSysCalls(scalls, functionCall)
  grep1 <- pmax(min(grep1[sapply(scalls[grep1], function(x) {
    tryCatch(is(parse(text = x), "expression"), error = function(y) NA)
  })], na.rm = TRUE) - 1, 1)

  # Convert character strings to their objects
  lapply(objects, function(x) get(x, envir = sys.frames()[[grep1]]))
}

#' Modify package order in search path
#'
#' Intended for internal use only. It modifies the search path (i.e., `search()`)
#' such that the packages required by the current module are placed first in the
#' search path. Note, several "core" packages are not touched; or more specifically,
#' they will remain in the search path, but may move down if packages are rearranged.
#' The current set of these core packages used by SpaDES can be found here:
#' `SpaDES.core:::.corePackages`
#'
#' @param pkgs The packages that are to be placed at the beginning of the search path,
#'
#' @param removeOthers Logical. If `TRUE`, then only the packages in
#'                     `c(pkgs, SpaDES.core:::.corePackages)`
#'                     will remain in the search path, i.e., all others will be removed.
#'
#' @param skipNamespacing Logical. If `FALSE`, then the running of an event in a module
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
    pkgs <- unlist(pkgs)[!(pkgs %in% .corePackages)]
    pkgsWithPrefix <- paste0("package:", unlist(pkgs))
    pkgPositions <- pmatch(pkgsWithPrefix, search())

    # Find all packages that are not in the first sequence after .GlobalEnv
    whNotAtTop <- !((seq_along(pkgPositions) + 1) %in% pkgPositions)

    if (any(whNotAtTop)) {
      whAdd <- which(is.na(pkgPositions))
      if (removeOthers) {
        pkgsToRm <- setdiff(search(), pkgsWithPrefix)
        pkgsToRm <- grep(pkgsToRm, pattern = .corePackagesGrep, invert = TRUE, value = TRUE)
        whRm <- seq_along(pkgsToRm)
      } else {
        whRm <- which(pkgPositions > min(which(whNotAtTop)))
        pkgsToRm <- pkgs[whRm]
      }

      if (length(whRm) > 0) {
        # i.e,. ones that need reordering
        suppressWarnings(
          lapply(unique(gsub(pkgsToRm, pattern = "package:", replacement = "")[whRm]), function(pack) {
            try(detach(paste0("package:", pack), character.only = TRUE), silent = TRUE)
          })
        )
      }
      #if (!removeOthers) {
      if (length(whAdd)) {
        suppressMessages(
          lapply(rev(pkgs[whAdd]), function(pack) {
            try(attachNamespace(pack), silent = TRUE)
          })
        )
      }
      #}
    }
  }
}

#' @keywords internal
.corePackages <- c(".GlobalEnv", "Autoloads", "SpaDES.core", "base", "grDevices",
                   "rstudio", "devtools_shims",
                   "methods", "utils", "graphics", "datasets", "stats", "testthat") # nolint
.corePackagesGrep <- paste(.corePackages, collapse = "|")

# .pkgEnv$corePackagesVec <- unlist(strsplit(.corePackagesGrep, split = "\\|"))
.corePackagesVec <- c(.corePackages[(1:2)], paste0("package:", .corePackages[-(1:2)]))

#' `tryCatch` that keeps warnings, errors and value (result)
#'
#' From <https://stackoverflow.com/a/24569739/3890027>
#'
#' @keywords internal
#' @rdname tryCatch
.tryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- e
      NULL
    }), warning = function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value = value, warning = warn, error = err)
}

#' All equal method for `simList` objects
#'
#' This function removes a few attributes that are added internally
#' by \pkg{SpaDES.core} and are not relevant to the `all.equal`.
#' One key element removed is any time stamps, as these are guaranteed to be different.
#'
#' @inheritParams base::all.equal
#'
#' @return See [base::all.equal()]
#'
#' @export
all.equal.simList <- function(target, current, ...) {
  attr(target, ".Cache")$newCache <- NULL
  attr(current, ".Cache")$newCache <- NULL
  attr(target, "removedObjs") <- NULL
  attr(current, "removedObjs") <- NULL

  if (length(target@completed))
    completed(target) <- completed(target, times = FALSE)
  if (length(current@completed))
    completed(current) <- completed(current, times = FALSE)

  # remove all objects starting with ._ in the simList@.xData
  objNamesTarget <- ls(envir = envir(target), all.names = TRUE, pattern = "^._")
  objNamesCurrent <- ls(envir = envir(current), all.names = TRUE, pattern = "^._")
  objsTarget <- mget(objNamesTarget, envir = envir(target))
  objsCurrent <- mget(objNamesCurrent, envir = envir(current))
  on.exit({
    # put them back on.exit
    list2env(objsTarget, envir = envir(target))
    list2env(objsCurrent, envir = envir(current))
  })
  rm(list = objNamesTarget, envir = envir(target))
  rm(list = objNamesCurrent, envir = envir(current))
  # suppressWarnings(rm("._startClockTime", envir = envir(target)))
  # suppressWarnings(rm("._startClockTime", envir = envir(current)))
  # suppressWarnings(rm("._firstEventClockTime", envir = envir(target)))
  # suppressWarnings(rm("._firstEventClockTime", envir = envir(current)))
  # suppressWarnings(rm(".timestamp", envir = envir(target)))
  # suppressWarnings(rm(".timestamp", envir = envir(current)))

  target1 <- .wrap(target, cachePath = getwd()) # deals with SpatVector/SpatRaster etc.
  current1 <- .wrap(current, cachePath = getwd()) # deals with SpatVector/SpatRaster etc.
  all.equal.default(target1, current1, check.environment = FALSE)
}

needInstall <- function(
    pkg = "methods",
    minVersion = NULL,
    messageStart = paste0(pkg, if (!is.null(minVersion)) paste0("(>=", minVersion, ")"),
                          " is required. Try: ")) {
  need <- FALSE
  if (!requireNamespace(pkg, quietly = TRUE)) {
    need <- TRUE
  } else {
    if (isTRUE(packageVersion(pkg) < minVersion))
      need <- TRUE
  }
  if (need) {
    stop(messageStart,
         "install.packages('", pkg, "')")
  }
}


.moduleNameNoUnderscore <- function(mod) gsub("_", ".", basename(mod))
