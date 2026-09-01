#' Parse and extract module metadata
#'
#' @inheritParams spades
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the `spades.modulePath` option.
#' @param defineModuleListItems A vector of metadata entries to return values
#'   about.
#'
#' @return A list of module metadata, matching the structure in [defineModule()].
#'
#' For a **parent module** -- one that declares `childModules` -- a parent
#' declares no `parameters`, `inputObjects`, `outputObjects` or `reqdPkgs` of
#' its own, so each of those entries is instead a **named list holding one
#' entry per child**, named by child module. Nothing is merged: each child
#' keeps its own table with its own columns, which differ between modules.
#' Nested parents resolve through to the modules that have no children of their
#' own. A module with no children is returned unchanged.
#'
#' @author Alex Chubaty
#' @export
#' @include simulation-simInit.R
#' @rdname moduleMetadata
#' @seealso [defineModule()]
#'
#' @example inst/examples/example_moduleMetadata.R
#'
setGeneric("moduleMetadata", function(sim, module, path = getOption("spades.modulePath", NULL),
                                      defineModuleListItems = c(
                                        "name", "description", "keywords", "childModules", "authors",
                                        "version", "spatialExtent", "timeframe", "timeunit", "citation",
                                        "documentation", "reqdPkgs", "parameters", "inputObjects", "outputObjects"
                                      )) {
  standardGeneric("moduleMetadata")
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(sim = "missing", module = "character", path = "character"),
  definition = function(module, path, defineModuleListItems) {
    filename <- paste(path, "/", module, "/", module, ".R", sep = "")
    if (!any(file.exists(filename))) {
      stop(paste(filename, "does not exist. This was created by putting",
                 "modulePath with the module name as a folder and filename.",
                 "Please correct the modulePath or module name in the simInit() call."))
    } else {
      ## check which of the module paths the file exists in -- use that file and path below
      id <- which(file.exists(filename))
      filename <- filename[id]
      path <- path[id]
    }

    ## store metadata as list
    opts <- options(spades.moduleCodeChecks = FALSE, spades.dotInputObjects = FALSE,
                    reproducible.useCache = FALSE, spades.loadReqdPkgs = FALSE)
    on.exit(options(opts))

    # There are 2 ways to get module metadata; read them directly or call `simInit`
    #   to do it it. Calling `simInit` allows us to use the internal tools built
    #   within `simInit`; BUT, it also means it parses the whole module. Normally
    #   that is only functions, but old SpaDES modules (e.g., LCC2005 and family)
    #   have a line stopifnot(packageVersion("SpaDES") >= "1.2.0.9009"),
    #   which fails using the simInit approach, so need the .parseModulePartial
    suppressMessages({
      sim <- try(simInit(modules = module, paths = list(modulePath = path)), silent = TRUE)
    }) # any failure will just pass to next try

    if (!is(sim, "try-error")) {
      metadata <- moduleMetadata(sim)[defineModuleListItems]
    } else {
      metadata <- lapply(defineModuleListItems, function(xx) {
        pmp <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                                   defineModuleElement = xx)
        out2 <- suppressMessages(try(eval(pmp), silent = TRUE))
        if (is(out2, "try-error")) {
          inner2 <- lapply(pmp, function(yyy) {
            # pmp is whole rbind statement
            out4 <- try(eval(yyy), silent = TRUE)
            if (is(out4, "try-error")) {
              yyy <- lapply(yyy, function(yyyyy) {
                # yyy is whole defineParameter statement
                out5 <- try(eval(yyyyy), silent = TRUE)
                if (is(out5, "try-error")) yyyyy <- deparse(yyyyy)
                return(yyyyy)
              })
            }
            if (is.list(yyy)) yyy <- as.call(yyy)
            return(yyy)
          })

          out2 <- as.call(inner2)
        }
        # Remove extra spaces
        aa <- capture.output(type = "message", {bb <- eval(out2)})
        return(bb)
      })
      names(metadata) <- defineModuleListItems
      # #metadata <- eval(parse(text = x)) # can't be used because can't evaluate start(sim)
      metadata <- rmExtraSpacesEOLList(metadata)
    }

    ## A parent module declares no parameters, inputs or outputs of its own, so
    ## querying one used to return empty tables. Report its children's instead.
    metadata <- .parentMetadataFromChildren(metadata, module, path,
                                            defineModuleListItems)

    return(metadata)
})

## Read a module's `childModules` straight from its .R file, without needing the
## module to parse or `simInit()` to succeed. character(0) when it has none.
.childModules <- function(module, path) {
  f <- file.path(path, module, paste0(module, ".R"))
  if (!file.exists(f)) return(character())
  kids <- tryCatch(
    as.character(unlist(.parseModulePartial(filename = f,
                                            defineModuleElement = "childModules"))),
    error = function(e) character()
  )
  kids[!is.na(kids) & nzchar(kids)]
}

## Depth-first expansion of a parent's children down to the modules that have no
## children of their own. `seen` stops a cyclic `childModules` entry looping.
.leafModules <- function(module, path, seen = character()) {
  if (module %in% seen) return(character())
  kids <- .childModules(module, path)
  if (length(kids) == 0) return(module)
  unlist(lapply(kids, .leafModules, path = path, seen = c(seen, module)))
}

## For a parent module, fill the (empty) metadata entries with its children's,
## as one named entry per child. Nothing is merged: each child keeps its own
## table with its own columns, which differ between modules (only some declare
## `sourceURL` on inputObjects, for example).
.parentMetadataFromChildren <- function(metadata, module, path, defineModuleListItems) {
  kids <- .childModules(module, path)
  if (length(kids) == 0) return(metadata)

  ## simInit() expands a parent into its children, so the parent's own row -- and
  ## with it childModules -- can come back empty; take it from the file instead
  if ("childModules" %in% defineModuleListItems && length(unlist(metadata[["childModules"]])) == 0)
    metadata[["childModules"]] <- kids

  leaves <- unique(setdiff(.leafModules(module, path), module))
  if (length(leaves) == 0) return(metadata)

  ## A child that cannot be parsed must not take the parent's query down with
  ## it: old SpaDES modules (LCC2005 and family) carry a top-level
  ## `stopifnot(packageVersion("SpaDES") >= ...)` that errors when SpaDES is not
  ## installed. Such a child is left out rather than propagated.
  mds <- lapply(leaves, function(m)
    tryCatch(suppressWarnings(moduleMetadata(module = m, path = path,
                                             defineModuleListItems = defineModuleListItems)),
             error = function(e) NULL))
  names(mds) <- leaves
  mds <- mds[!vapply(mds, is.null, FUN.VALUE = logical(1))]
  if (length(mds) == 0) return(metadata)

  for (item in intersect(c("parameters", "inputObjects", "outputObjects", "reqdPkgs"),
                         defineModuleListItems))
    metadata[[item]] <- lapply(mds, `[[`, item)

  metadata
}

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(sim = "missing", module = "character", path = "missing"),
  definition = function(module, defineModuleListItems) {
    path <- checkModulePath()

    moduleMetadata(module = module, path = getOption("spades.modulePath"),
                   defineModuleListItems = defineModuleListItems)
})

#' @export
#' @rdname moduleMetadata
setMethod(
  "moduleMetadata",
  signature = c(sim = "ANY", module = "ANY", path = "ANY"),
  definition = function(sim, module, path, defineModuleListItems) {
    if (is.character(sim)) {
      message("Assuming sim is a module name")
      if (missing(path)) {
        metadataList <- moduleMetadata(module = sim, defineModuleListItems = defineModuleListItems)
      } else {
        metadataList <- moduleMetadata(module = sim, path = path, defineModuleListItems = defineModuleListItems)
      }

    } else {

      if (!missing(path)) message("path not used with sim provided")
      if (missing(module)) {
        module <- unlist(modules(sim))
      }
      numModules <- length(module)

      metadata <- sim@depends@dependencies[module]
      sn <- slotNames(".moduleDeps")
      names(sn) <- sn
      metadataList <- lapply(metadata, function(mod) {
        lapply(sn, function(element) {
          slot(mod, name = element)
        })
      })
      metadataList <- lapply(metadataList, function(moduleMetadata) {
        rmExtraSpacesEOLList(moduleMetadata)
      })
      if (numModules == 1)
        metadataList <- metadataList[[module]]
    }

    return(metadataList)
})

################################################################################
#' Parse and extract a module's version
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the `spades.modulePath` option.
#'
#' @inheritParams spades
#' @inheritParams .parseModulePartial
#'
#' @return `numeric_version` indicating the module's version.
#'
#' @author Alex Chubaty
#' @export
#' @include simulation-simInit.R
#' @rdname moduleVersion
#' @seealso [moduleMetadata()]
#'
#' @example inst/examples/example_moduleVersion.R
#'
setGeneric("moduleVersion", function(module, path, sim, envir = NULL) {
  standardGeneric("moduleVersion")
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "character", sim = "missing", envir = "ANY"),
  definition = function(module, path, envir) {
    v <- .parseModulePartial(filename = file.path(path, module, paste0(module, ".R")),
                             defineModuleElement = "version", envir = envir)
    if (is.null(names(v))) {
      as.numeric_version(v) ## SpaDES < 1.3.1.9044
    } else {
      as.numeric_version(v[[module]]) ## SpaDES >= 1.3.1.9044
    }
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "missing", sim = "missing", envir = "ANY"),
  definition = function(module, envir) {
    path <- checkModulePath()

    moduleVersion(module = module, path = getOption("spades.modulePath"), envir = envir)
})

#' @export
#' @rdname moduleVersion
setMethod(
  "moduleVersion",
  signature = c(module = "character", path = "missing", sim = "simList", envir = "ANY"),
  definition = function(module, sim, envir) {
    path <- checkModulePath()

    v <- .parseModulePartial(sim = sim, modules = list(module),
                             defineModuleElement = "version", envir = envir) |>
      (function(x) x[[module]])()

    if (is.null(names(v))) {
      as.numeric_version(v) ## SpaDES < 1.3.1.9044
    } else {
      as.numeric_version(v[[module]])  ## SpaDES >= 1.3.1.9044
    }
})

################################################################################
#' Extract a module's parameters, inputs, outputs, or required packages
#'
#' These are more or less wrappers around `moduleMetadata`, with the exception
#' that extraneous spaces and End-Of-Line characters will be removed from the
#' `desc` arguments in `defineParameters`, `defineInputs`, and
#' `defineOutputs`
#'
#' @param module Character string. Your module's name.
#'
#' @param path   Character string specifying the file path to modules directory.
#'               Default is to use the `spades.modulePath` option.
#'
#' @return `data.frame`
#'
#' @author Alex Chubaty
#' @export
#' @rdname moduleParamsInputsOuputs
#' @seealso [moduleMetadata()]
#'
#' @example inst/examples/example_moduleParamsInputsOuputs.R
#'
setGeneric("moduleParams", function(module, path) {
  standardGeneric("moduleParams")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setMethod(
  "moduleParams",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    md <- suppressWarnings(moduleMetadata(module = module, path = path))

    # remove spaces and EOL
    .cleanDescCol(md[["parameters"]], "paramDesc")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setGeneric("moduleInputs", function(module, path) {
  standardGeneric("moduleInputs")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setMethod(
  "moduleInputs",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    md <- suppressWarnings(moduleMetadata(module = module, path = path))

    # remove spaces and EOL
    .cleanDescCol(md[["inputObjects"]], "desc")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setGeneric("moduleOutputs", function(module, path) {
  standardGeneric("moduleOutputs")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setMethod(
  "moduleOutputs",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    md <- suppressWarnings(moduleMetadata(module = module, path = path))

    # remove spaces and EOL
    .cleanDescCol(md[["outputObjects"]], "desc")
})

#' @export
#' @rdname moduleParamsInputsOuputs
setGeneric("moduleReqdPkgs", function(module, path) {
  standardGeneric("moduleReqdPkgs")
})

#' @export
#' @importFrom Require extractPkgName extractVersionNumber
#' @rdname moduleParamsInputsOuputs
setMethod(
  "moduleReqdPkgs",
  signature = c(module = "character", path = "character"),
  definition = function(module, path) {
    md <- suppressWarnings(moduleMetadata(module = module, path = path))
    pkgs <- unique(unlist(md[["reqdPkgs"]], use.names = FALSE))
    if (length(pkgs) == 0)
      return(data.frame(packageName = character(), minVersion = character()))

    data.frame(packageName = extractPkgName(pkgs),
               minVersion = replaceNAwithEmpty(extractVersionNumber(pkgs)))
})

## extractVersionNumber() gives NA when a package is declared without one; an
## empty string reads better in a rendered table.
replaceNAwithEmpty <- function(x) {
  x[is.na(x)] <- ""
  x
}

## A metadata entry is a data.frame for an ordinary module, and a named list of
## data.frames -- one per child -- for a parent. Clean `col` in either shape.
.cleanDescCol <- function(x, col) {
  if (is.data.frame(x)) {
    x[[col]] <- rmExtraSpacesEOL(x[[col]])
    x
  } else if (is.list(x)) {
    lapply(x, .cleanDescCol, col = col)
  } else {
    x
  }
}

rmExtraSpacesEOL <- function(x) gsub(" +|[ *\n]+", " ", x)

rmExtraSpacesEOLList <- function(xx) {
  toRmESEOL <- grepl(c("parameters|inputObjects|outputObjects"), names(xx))
  xx[toRmESEOL] <- lapply(xx[toRmESEOL], function(elem) {
    if (any(grepl("desc", tolower(names(elem))))) {
      whCol <- grep("desc", tolower(names(elem)))
      elem[[whCol]] <- rmExtraSpacesEOL(elem[[whCol]])
      elem
    } else {
      elem
    }
  })

  ## used to diagnose long parameter descriptions that trigger insertion of `\n`
  #id1 <- which(names(xx) == "parameters")
  #id2 <- which(names(xx[[id1]]) == "paramDesc")
  #grep("\n", xx[[id1]][[id2]], value = TRUE)

  xx
}

rmExtraSpacesEOLCollapse <- function(lis, useOnlyUnnamed = TRUE) {
  # moreDesc <- list(...)
  if (isTRUE(useOnlyUnnamed))
    if (!is.null(names(lis))) {
      lis <- lis[!nzchar(names(lis))]
    }
  lis <- unlist(lis)
  lis <- paste(lis, collapse = "")
  lis <- rmExtraSpacesEOL(lis)
}
