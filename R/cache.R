if (!isGeneric(".robustDigest")) {
  setGeneric(
    ".robustDigest",
    function(object, objects, compareRasterFileLength = 1e6, algo = "xxhash64") {
      standardGeneric(".robustDigest")
  })
}

#' \code{.robustDigest} for \code{simList} class objects
#'
#' This is intended to be used within the \code{Cache} function, but can be
#' used to evaluate what a \code{simList} would look like once it is
#' converted to a repeatably digestible object.
#'
#' See \code{\link[reproducible]{robustDigest}}. This method strips out stuff
#' from a simList class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems,
#' or machines. This will likely still not allow identical digest
#' results across R versions.
#'
#' @inheritParams reproducible::.robustDigest
#'
#' @author Eliot Mcintire
#' @exportMethod .robustDigest
#' @importFrom fastdigest fastdigest
#' @importFrom reproducible asPath .robustDigest .sortDotsUnderscoreFirst .orderDotsUnderscoreFirst
#' @importMethodsFrom reproducible .robustDigest
#' @include simList-class.R
#' @aliases Cache
#' @rdname robustDigest
#' @seealso \code{\link[reproducible]{robustDigest}}
#'
setMethod(
  ".robustDigest",
  signature = "simList",
  definition = function(object, objects, compareRasterFileLength, algo,
                        digestPathContent, classOptions) {
    outerObjs <- ls(object@.envir, all.names = TRUE)
    moduleEnvirs <- mget(outerObjs[outerObjs %in% unlist(modules(object))], envir = object@.envir)
    moduleObjs <- lapply(moduleEnvirs, function(me) ls(me, all.names = TRUE))
    allObjsInSimList <- append(list(".envir" = outerObjs), moduleObjs)
    allEnvsInSimList <- append(list(object@.envir), moduleEnvirs)

    ord <- .orderDotsUnderscoreFirst(allObjsInSimList)
    allObjsInSimList <- allObjsInSimList[ord]
    allEnvsInSimList <- allEnvsInSimList[ord]

    isObjectEmpty <- if (!missing(objects)) {
      if (!is.null(objects)) {
        FALSE
      } else {
        TRUE
      }
    } else {
      TRUE
    }
    if (!isObjectEmpty) {
      # objects may be provided in a namespaced format: modName:objName --
      # e.g., coming from .parseModule
      objects1 <- strsplit(objects, split = ":")
      lens <- unlist(lapply(objects1, length))
      objects1ByMod <- unlist(lapply(objects1[lens > 1], function(x) x[1]))
      mods <- unique(objects1ByMod)
      objects2 <- lapply(mods, function(mod) {
        unlist(lapply(objects1[lens > 1][objects1ByMod == mod], function(x) x[[2]]))
      })
      names(objects2) <- mods
      objects <- append(list(".envir" = unlist(objects1[lens == 1])), objects2)
    } else {
      objects <- allObjsInSimList
    }
    envirHash <- lapply(seq(allObjsInSimList), function(objs) {
      objectsToDigest <- sort(allObjsInSimList[[objs]], method = "radix")
      objectsToDigest <- objectsToDigest[objectsToDigest %in%
                                           objects[[names(allObjsInSimList)[objs]]]]
      .robustDigest(mget(objectsToDigest, envir = allEnvsInSimList[[objs]]),
                    digestPathContent = digestPathContent,
                    compareRasterFileLength = compareRasterFileLength)
    })
    names(envirHash) <- names(allObjsInSimList)
    lens <- unlist(lapply(envirHash, function(x) length(x) > 0))
    envirHash <- envirHash[lens]

    # Copy all parts except environment, clear that, then convert to list
    object <- Copy(object, objects = FALSE, queues = FALSE)
    object@.envir <- new.env()
    object <- as(object, "simList_")
    # Replace the .list slot with the hashes of the slots
    object@.list <- envirHash

    # Remove paths (i.e., dirs) as they are not relevant -- it is only the files that are relevant
    #  i.e., if the same file is located in a different place, that is ok
    object@paths <- list()
    #object@paths <- .robustDigest(lapply(object@paths, asPath),
    #                              digestPathContent = digestPathContent)

    # don't cache contents of output because file may already exist
    object@outputs$file <- basename(object@outputs$file)
    object@inputs$file <- unlist(.robustDigest(object@inputs$file,
                                               digestPathContent = digestPathContent,
                                               compareRasterFileLength = compareRasterFileLength))
    deps <- object@depends@dependencies
    for (i in seq_along(deps)) {
      if (!is.null(deps[[i]])) {
        object@depends@dependencies[[i]] <- lapply(
          slotNames(object@depends@dependencies[[i]]), function(x) {
          slot(object@depends@dependencies[[i]], x)
        })
        names(object@depends@dependencies[[i]]) <- slotNames(deps[[i]])
        object@depends@dependencies[[i]][["timeframe"]] <- as.Date(deps[[i]]@timeframe)
      }
    }

    # Sort the params and .list with dots first, to allow Linux and Windows to be compatible
    if (!is.null(classOptions$params)) if (length(classOptions$params)) {
      object@params <- list(classOptions$params)
      names(object@params) <- classOptions$modules
    }
    if (!is.null(classOptions$modules)) if (length(classOptions$modules)) {
      object@modules <- list(classOptions$modules)
      object@depends@dependencies <- object@depends@dependencies[classOptions$modules]
    }
    object@params <- lapply(object@params, function(x) .sortDotsUnderscoreFirst(x))
    object@params <- .sortDotsUnderscoreFirst(object@params)

    nonDotList <- grep(".list", slotNames(object), invert = TRUE, value = TRUE)
    obj <- list()
    obj$.list <- object@.list

    obj[nonDotList] <- lapply(nonDotList, function(x) fastdigest(slot(object, x)))
    if (!is.null(classOptions$events))
      if (FALSE %in% classOptions$events) obj$events <- NULL
    if (!is.null(classOptions$current))
      if (FALSE %in% classOptions$current) obj$current <- NULL
    if (!is.null(classOptions$completed))
      if (FALSE %in% classOptions$completed) obj$completed <- NULL
    if (!is.null(classOptions$simtimes))
      if (FALSE %in% classOptions$simtimes) obj$simtimes <- NULL

    obj
  })

if (!isGeneric(".tagsByClass")) {
  setGeneric(".tagsByClass", function(object) {
    standardGeneric(".tagsByClass")
  })
}

#' tagsByClass for simList class objects
#'
#' See \code{\link[reproducible]{.tagsByClass}}. Adds current \code{moduleName},
#' \code{eventType}, \code{eventTime}, and \code{function:spades} as userTags
#'
#' @inheritParams reproducible::.tagsByClass
#'
#' @author Eliot McIntire
#' @exportMethod .tagsByClass
#' @importFrom reproducible .tagsByClass
#' @importMethodsFrom reproducible .tagsByClass
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.tagsByClass}}
#' @rdname tagsByClass
#'
setMethod(
  ".tagsByClass",
  signature = "simList",
  definition = function(object) {
    cur <- object@current
    if (NROW(cur)) {
      userTags <- c(
        paste0("module:", cur$moduleName),
        paste0("eventType:", cur$eventType),
        paste0("eventTime:", cur$eventTime),
        paste0("function:spades")
      ) # add this because it will be an outer function, if there are events occurring
    } else {
      scalls <- sys.calls()
      parseModuleFrameNum <- grep(scalls, pattern = "(^.parseModule)")[2]
      if (!is.na(parseModuleFrameNum)) {
        inObj <- grepl(scalls, pattern = ".inputObjects")
        if (any(!is.na(inObj))) {
          userTags <- c("function:.inputObjects")
          userTags1 <- tryCatch(paste0("module:", get("m", sys.frame(parseModuleFrameNum))),
                                error = function(x) NULL)
          userTags <- c(userTags, userTags1)
        }
      } else {
        userTags <- NULL
      }
    }
    userTags
  })

if (!isGeneric(".cacheMessage")) {
  setGeneric(".cacheMessage", function(object, functionName) {
    standardGeneric(".cacheMessage")
  })
}

#' cacheMessage for simList class objects
#'
#' See \code{\link[reproducible]{.cacheMessage}}.
#'
#' @importFrom reproducible .cacheMessage
#' @importMethodsFrom reproducible .cacheMessage
#' @inheritParams reproducible::.cacheMessage
#' @rdname cacheMessage
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.cacheMessage}}
#' @exportMethod .cacheMessage
setMethod(
  ".cacheMessage",
  signature = "simList",
  definition = function(object, functionName) {
    cur <- current(object)
    if (NROW(cur)) {
      whichCached <- grep(".useCache", object@params)
      useCacheVals <- lapply(whichCached, function(x) {
        object@params[[x]]$.useCache
      })

      whCurrent <- match(cur$moduleName, names(object@params)[whichCached])
      if (isTRUE(useCacheVals[[whCurrent]])) {
        cat(crayon::blue("  Using cached copy of", cur$moduleName, "module\n"))
      } else {
        cat(crayon::blue("  Using cached copy of", cur$eventType, "event in",
                         cur$moduleName, "module\n"))
      }
    } else {
      .cacheMessage(NULL, functionName)
    }
  })

#########################################################
if (!isGeneric(".checkCacheRepo")) {
  setGeneric(".checkCacheRepo", function(object, create = FALSE) {
    standardGeneric(".checkCacheRepo")
  })
}

#' checkCacheRepo for simList class objects
#'
#' See \code{\link[reproducible]{.checkCacheRepo}}.
#'
#' @importFrom reproducible .checkCacheRepo
#' @importMethodsFrom reproducible .checkCacheRepo
#' @inheritParams reproducible::.checkCacheRepo
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.checkCacheRepo}}
#' @exportMethod .checkCacheRepo
#' @export
#' @rdname checkCacheRepo
setMethod(
  ".checkCacheRepo",
  signature = "list",
  definition = function(object, create) {

    object <- .findSimList(object)
    whSimList <- unlist(lapply(object, is, "simList"))

    if (any(whSimList)) {
      # just take the first simList, if there are >1
      cacheRepo <- object[whSimList][[1]]@paths$cachePath
    } else {
      doEventFrameNum <- grep(sys.calls(), pattern = "(^doEvent)|(^.parseModule)")[2]
      if (!is.na(doEventFrameNum)) {
        sim <- get("sim", envir = sys.frame(doEventFrameNum))
        cacheRepo <- sim@paths$cachePath
      } else {
        cacheRepo <- .getOption("spades.cachePath")
        #checkPath(cacheRepo, create = TRUE) #SpaDES dependency
      }
    }
    checkPath(path = cacheRepo, create = create)
  })

if (!isGeneric(".prepareOutput")) {
  setGeneric(".prepareOutput", function(object) {
    standardGeneric(".prepareOutput")
  })
}

##########################################
#' prepareOutput for simList class objects
#'
#' See \code{\link[reproducible]{.prepareOutput}}.
#'
#' @importFrom reproducible .prepareOutput
#' @importMethodsFrom reproducible .prepareOutput
#' @inheritParams reproducible::.prepareOutput
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.prepareOutput}}
#' @exportMethod .prepareOutput
#' @export
#' @rdname prepareOutput
setMethod(
  ".prepareOutput",
  signature = "simList",
  definition = function(object, cacheRepo, ...) {
    tmpl <- list(...)
    tmpl <- .findSimList(tmpl)
    # only take first simList -- may be a problem:
    whSimList <- which(unlist(lapply(tmpl, is, "simList")))[1]
    origEnv <- tmpl[[whSimList[1]]]@.envir
    isListOfSimLists <- if (is.list(object)) {
      if (is(object[[1]], "simList")) TRUE else FALSE
    } else {
      FALSE
    }

    if (isListOfSimLists) {
      object2 <- list()
      for (i in seq_along(object)) {
        # need to keep the list(...) slots ...
        # i.e., Caching of simLists is mostly about objects in .envir
        object2[[i]] <- Copy(tmpl[[whSimList]], objects = FALSE)
        object2[[i]]@.envir <- object[[i]]@.envir
        object2[[i]]@completed <- object[[i]]@completed
        object2[[i]]@simtimes <- object[[i]]@simtimes
        object2[[i]]@current <- object[[i]]@current
        object2[[i]]@events <- object[[i]]@events

        lsOrigEnv <- ls(origEnv, all.names = TRUE)
        keepFromOrig <- !(lsOrigEnv %in% ls(object2[[i]]@.envir, all.names = TRUE))
        # list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv),
        #          envir = object2[[i]]@.envir)
        list2env(mget(lsOrigEnv[keepFromOrig], envir = tmpl[[whSimList]]@.envir),
                 envir = object2[[i]]@.envir)
      }
    } else {
      # need to keep the tmpl slots ...
      # i.e., Caching of simLists is mostly about objects in .envir
      object2 <- Copy(tmpl[[whSimList]], objects = FALSE)
      object2@.envir <- object@.envir
      object2@completed <- object@completed
      if (NROW(current(object2)) == 0) {
        # this is usually a spades call, i.e., not an event or module doEvent call
        object2@events <- object@events
        object2@simtimes <- object@simtimes
      } else {
        # if this is FALSE, it means that events were added by the event
        if (!isTRUE(all.equal(object@events, object2@events)))
          object2@events <- do.call(unique, args = list(append(object@events, object2@events)))
          #object2@events <- unique(rbindlist(list(object@events, object2@events)))
      }
      object2@current <- object@current

      lsOrigEnv <- ls(origEnv, all.names = TRUE)
      keepFromOrig <- !(lsOrigEnv %in% ls(object2@.envir, all.names = TRUE))
      list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv), envir = object2@.envir)
    }
    if (!is.null(attr(object, "removedObjs"))) {
      if (length(attr(object, "removedObjs"))) {
        rm(list = attr(object, "removedObjs"), envir = object2@.envir)
      }
    }

    attr(object2, "tags") <- attr(object, "tags")
    attr(object2, "call") <- attr(object, "call")
    attr(object2, "function") <- attr(object, "function")

    return(object2)
})

if (!isGeneric(".preDigestByClass")) {
  setGeneric(".preDigestByClass", function(object) {
    standardGeneric(".preDigestByClass")
  })
}

################################################################################
#' Pre-digesting method for \code{simList}
#'
#' Takes a snapshot of simList objects.
#'
#' See \code{\link[reproducible]{.preDigestByClass}}.
#'
#' @author Eliot McIntire
#' @export
#' @exportMethod .preDigestByClass
#' @importFrom reproducible .preDigestByClass
#' @importMethodsFrom reproducible .preDigestByClass
#' @inheritParams reproducible::.preDigestByClass
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.preDigestByClass}}
#' @rdname preDigestByClass
setMethod(
  ".preDigestByClass",
  signature = "simList",
  definition = function(object) {
    obj <- ls(object@.envir, all.names = TRUE)
    return(obj)
})

if (!isGeneric(".addTagsToOutput")) {
  setGeneric(".addTagsToOutput", function(object, outputObjects, FUN) {
    standardGeneric(".addTagsToOutput")
  })
}

#' addTagsToOutput for simList class objects
#'
#' See \code{\link[reproducible]{.addTagsToOutput}}.
#'
#' @inheritParams reproducible::.addTagsToOutput
#'
#' @author Eliot McIntire
#' @exportMethod .addTagsToOutput
#' @export
#' @importFrom reproducible .addTagsToOutput
#' @importMethodsFrom reproducible .addTagsToOutput
#' @include simList-class.R
#' @rdname addTagsToOutput
#' @seealso \code{\link[reproducible]{.addTagsToOutput}}
#'
setMethod(
  ".addTagsToOutput",
  signature = "simList",
  definition = function(object, outputObjects, FUN, preDigestByClass) {
    if (!is.null(outputObjects)) {
      outputToSave <- object
      outputToSave@.envir <- new.env()
      # Some objects are conditionally produced from a module's outputObject
      whExist <- outputObjects %in% ls(object@.envir, all.names = TRUE)
      list2env(mget(outputObjects[whExist], envir = object@.envir), envir = outputToSave@.envir)
      attr(outputToSave, "tags") <- attr(object, "tags")
      attr(outputToSave, "call") <- attr(object, "call")
      if (isS4(FUN))
        attr(outputToSave, "function") <- attr(object, "function")
    } else {
      outputToSave <- object
    }

    # Some objects are removed from a simList
    if (!is.null(preDigestByClass)) {
      lsInObj <- ls(object@.envir, all.names = TRUE)
      removedObjs <- unlist(preDigestByClass)[!(unlist(preDigestByClass) %in% lsInObj)]
      attr(outputToSave, "removedObjs") <- removedObjs
    }

    outputToSave
})

if (!isGeneric(".objSizeInclEnviros")) {
  setGeneric(".objSizeInclEnviros", function(object) {
    standardGeneric(".objSizeInclEnviros")
  })
}

#' objSizeInclEnviros for simList class objects
#'
#' See \code{\link[reproducible]{.objSizeInclEnviros}}.
#'
#' @importFrom reproducible .objSizeInclEnviros
#' @importFrom utils object.size
#' @importMethodsFrom reproducible .objSizeInclEnviros
#' @inheritParams reproducible::.objSizeInclEnviros
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.objSizeInclEnviros}}
#' @exportMethod .objSizeInclEnviros
#' @export
#' @rdname objSizeInclEnviros
setMethod(
  ".objSizeInclEnviros",
  signature = "simList",
  definition = function(object) {
    object.size(as.list(object@.envir, all.names = TRUE)) + object.size(object)
})


#' Find simList in a nested list
#'
#' THis is recursive, so it will find the all simLists even if they are deeply nested.
#'
#' @param x any object, used here only when it is a list with at least one
#'        \code{simList} in it
#' @rdname findSimList
.findSimList <- function(x) {
  if (is.list(x)) {
    out <- lapply(x, .findSimList)
    out <- unlist(out, recursive = TRUE, use.names = FALSE) # get rid of NULL
    if (is.null(out)) out <- list(NULL)
  } else {
    out <- inherits(x, "simList")
    if (out) {
      out <- x
    } else {
      out <- NULL
    }
  }
  return(out)
}

