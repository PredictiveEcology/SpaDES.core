if (!isGeneric(".robustDigest")) {
  setGeneric(
    ".robustDigest",
    function(object, .objects, length = Inf, algo = "xxhash64", ...) {
      standardGeneric(".robustDigest")
    }
  )
}

#' \code{.robustDigest} for \code{simList} objects
#'
#' This is intended to be used within the \code{Cache} function, but can be used to evaluate what
#' a \code{simList} would look like once it is converted to a repeatably digestible object.
#'
#' See \code{\link[reproducible]{robustDigest}}.
#' This method strips out stuff from a \code{simList} class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems, or machines.
#' This will likely still not allow identical digest results across R versions.
#'
#' @inheritParams reproducible::.robustDigest
#'
#' @aliases Cache
#' @author Eliot McIntire
#' @exportMethod .robustDigest
#' @importFrom fastdigest fastdigest
#' @importFrom reproducible asPath  .orderDotsUnderscoreFirst .robustDigest .sortDotsUnderscoreFirst
#' @importMethodsFrom reproducible .robustDigest
#' @include simList-class.R
#' @rdname robustDigest
#' @seealso \code{\link[reproducible]{robustDigest}}
setMethod(
  ".robustDigest",
  signature = "simList",
  definition = function(object, .objects, length, algo, quick, classOptions) {

    curMod <- currentModule(object)

    outerObjs <- ls(object@.xData, all.names = TRUE)
    moduleEnvirs <- mget(outerObjs[outerObjs %in% unlist(modules(object))],
                         envir = object@.xData)
    moduleObjs <- lapply(moduleEnvirs, function(me) ls(me, all.names = TRUE))
    allObjsInSimList <- append(list(".xData" = outerObjs), moduleObjs)
    allEnvsInSimList <- append(list(object@.xData), moduleEnvirs)

    ord <- .orderDotsUnderscoreFirst(allObjsInSimList)
    allObjsInSimList <- allObjsInSimList[ord]
    allEnvsInSimList <- allEnvsInSimList[ord]
    names(allEnvsInSimList) <- names(allObjsInSimList)

    isObjectEmpty <- if (!missing(.objects)) {
      if (!is.null(.objects)) {
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
      objects1 <- strsplit(.objects, split = ":")
      lens <- unlist(lapply(objects1, length))
      objects1ByMod <- unlist(lapply(objects1[lens > 1], function(x) x[1]))
      mods <- unique(objects1ByMod)
      objects2 <- lapply(mods, function(mod) {
        unlist(lapply(objects1[lens > 1][objects1ByMod == mod], function(x) x[[2]]))
      })
      names(objects2) <- mods
      .objects <- append(list(".xData" = unlist(objects1[lens == 1])), objects2)
    } else {
      .objects <- allObjsInSimList
    }
    envirHash <- Map(objs = allObjsInSimList, name = names(allObjsInSimList),
                     function(objs, name) {
                       objs <- objs[!objs %in% c("._parsedData", "._sourceFilename", "._simRndString", "mod")]
                       objectsToDigest <- sort(objs, method = "radix")
                       objectsToDigest <- objectsToDigest[objectsToDigest %in%
                                                            .objects[[name]]]
                       .robustDigest(mget(objectsToDigest, envir = allEnvsInSimList[[name]]),
                                     quick = quick,
                                     length = length)
                     })
    #names(envirHash) <- names(allObjsInSimList)
    lens <- unlist(lapply(envirHash, function(x) length(x) > 0))
    envirHash <- envirHash[lens]

    # Copy all parts except environment, clear that, then convert to list
    objectTmp <- object
    object <- Copy(object, objects = FALSE, queues = FALSE)
    object <- as(object, "simList_")
    # Replace the .list slot with the hashes of the slots
    object@.Data <- envirHash

    # Remove paths (i.e., dirs) as they are not relevant -- it is only the files that are relevant
    #  i.e., if the same file is located in a different place, that is ok
    object@paths <- list()

    # don't cache contents of output because file may already exist
    object@outputs$file <- basename(object@outputs$file)
    if (NROW(object@inputs))
      object@inputs$file <- unlist(.robustDigest(object@inputs$file, quick = quick, length = length)) #nolint
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

    # if this call is within a single module, only keep module-specific params
    if (length(curMod) > 0) {
      object@params <- object@params[curMod]
    }
    object@params <- lapply(object@params, function(x) .sortDotsUnderscoreFirst(x))
    object@params <- .sortDotsUnderscoreFirst(object@params)

    nonDotList <- grep(".list", slotNames(object), invert = TRUE, value = TRUE)
    obj <- list()
    obj$.list <- object@.Data
    if (length(obj$.list)) {
      obj$.list[[1]]$._startClockTime <- NULL
      obj$.list[[1]]$._timestamp <- NULL
      obj$.list[[1]]$._simRndString <- NULL
    }

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

#' \code{.tagsByClass} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.tagsByClass}}. Adds current \code{moduleName},
#' \code{eventType}, \code{eventTime}, and \code{function:spades} as \code{userTags}.
#'
#' @inheritParams reproducible::.tagsByClass
#'
#' @author Eliot McIntire
#' @exportMethod .tagsByClass
#' @importFrom reproducible .tagsByClass
#' @importFrom reproducible .grepSysCalls
#' @importMethodsFrom reproducible .tagsByClass
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.tagsByClass}}
#' @rdname tagsByClass
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
      parseModuleFrameNum <- .grepSysCalls(scalls, "^.parseModule")[2]
      if (!is.na(parseModuleFrameNum)) {
        inObj <- .grepSysCalls(scalls, pattern = "^.inputObjects")
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
  setGeneric(".cacheMessage", function(object, functionName, fromMemoise) {
    standardGeneric(".cacheMessage")
  })
}

#' \code{.cacheMessage} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.cacheMessage}}.
#'
#' @exportMethod .cacheMessage
#' @importFrom crayon blue
#' @importFrom reproducible .cacheMessage
#' @importMethodsFrom reproducible .cacheMessage
#' @inheritParams reproducible::.cacheMessage
#' @include simList-class.R
#' @rdname cacheMessage
#' @seealso \code{\link[reproducible]{.cacheMessage}}
setMethod(
  ".cacheMessage",
  signature = "simList",
  definition = function(object, functionName,
                        fromMemoise = getOption("reproducible.useMemoise", TRUE)) {
    cur <- current(object)
    if (NROW(cur)) {
      whichCached <- grep(".useCache", object@params)
      useCacheVals <- lapply(whichCached, function(x) {
        object@params[[x]]$.useCache
      })

      whCurrent <- match(cur$moduleName, names(object@params)[whichCached])
      if (is.na(fromMemoise)) fromMemoise <- FALSE
      fromWhere <- c("cached", "memoised")[fromMemoise + 1]
      if (isTRUE(useCacheVals[[whCurrent]])) {
        if (isTRUE(fromMemoise)) {
          cat(crayon::blue("  Loading memoised copy of", cur$moduleName, "module\n"))
        } else if (!is.na(fromMemoise)) {
          cat(crayon::blue("  Using cached copy of", cur$moduleName, "module\n",
                           "adding to memoised copy\n"))
        } else {
          cat(crayon::blue("  Using ", fromWhere," copy of", cur$moduleName, "module\n"))
        }
      } else {
        if (isTRUE(fromMemoise)) {
          cat(crayon::blue("  Using memoised copy of", cur$eventType, "event in",
                           cur$moduleName, "module\n"))

        } else if (!is.na(fromMemoise)) {
          cat(crayon::blue("  Using cached copy of", cur$eventType, "event in",
                           cur$moduleName, "module. ",
                           if (fromMemoise) "Adding to memoised copy.",
                           "\n"))
        } else {
          cat(crayon::blue("  Using ", fromWhere," copy of", cur$eventType, "event in",
                           cur$moduleName, "module\n"))
        }

      }
    } else {
      .cacheMessage(NULL, functionName, fromMemoise = fromMemoise)
    }
})

if (!isGeneric(".checkCacheRepo")) {
  setGeneric(".checkCacheRepo", function(object, create = FALSE) {
    standardGeneric(".checkCacheRepo")
  })
}

#' \code{.checkCacheRepo} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.checkCacheRepo}}.
#'
#' @inheritParams reproducible::.checkCacheRepo
#'
#' @export
#' @exportMethod .checkCacheRepo
#' @importFrom reproducible .checkCacheRepo .grepSysCalls
#' @importMethodsFrom reproducible .checkCacheRepo
#' @include simList-class.R
#' @rdname checkCacheRepo
#' @seealso \code{\link[reproducible]{.checkCacheRepo}}
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
      doEventFrameNum <- .grepSysCalls(sys.calls(), "(^doEvent)|(^.parseModule)")[2]

      if (!is.na(doEventFrameNum)) {
        sim <- get("sim", envir = sys.frame(doEventFrameNum))
        cacheRepo <- sim@paths$cachePath
      } else {
        cacheRepo <- .getOption("reproducible.cachePath")
        #checkPath(cacheRepo, create = TRUE) #SpaDES dependency
      }
    }
    checkPath(path = cacheRepo, create = create)
})

if (!isGeneric(".addChangedAttr")) {
  setGeneric(".addChangedAttr", function(object, preDigest, origArguments, ...) {
    standardGeneric(".addChangedAttr")
  })
}

#' \code{.addChangedAttr} for \code{simList} objects
#'
#' This will evaluate which elements in the \code{simList} object changed following
#' this Cached function call. It will add a named character string as an
#' attribute \code{attr(x, ".Cache")$changed}, indicating which ones changed.
#' When this function is subsequently called again, only these changed objects
#' will be returned. All other \code{simList} objects will remain unchanged.
#'
#' @inheritParams reproducible::.addChangedAttr
#'
#' @seealso \code{\link[reproducible]{.addChangedAttr}}.
#'
#' @export
#' @exportMethod .addChangedAttr
#' @importFrom reproducible .addChangedAttr .setSubAttrInList
#' @importMethodsFrom reproducible .addChangedAttr
#' @include simList-class.R
#' @rdname addChangedAttr
#' @seealso \code{\link[reproducible]{.addChangedAttr}}
setMethod(
  ".addChangedAttr",
  signature = "simList",
  definition = function(object, preDigest, origArguments, ...) {
    dots <- list(...)
    whSimList <- which(unlist(lapply(origArguments, is, "simList")))[1]

    # remove the "newCache" attribute, which is irrelevant for digest
    if (!is.null(attr(object, ".Cache")$newCache)) {
      .setSubAttrInList(object, ".Cache", "newCache", NULL)
      #attr(object, ".Cache")$newCache <- NULL

      if (!identical(attr(object, ".Cache")$newCache, NULL))
        stop("attributes on the cache object are not correct - 4")
    }

    postDigest <- .robustDigest(object, .objects = dots$.objects,
                                length = dots$length,
                                algo = dots$algo,
                                quick = dots$quick,
                                classOptions = dots$classOptions)

    changed <- if (length(postDigest$.list)) {
      internalSimList <- unlist(lapply(preDigest[[whSimList]]$.list,
                                       function(x) !any(startsWith(names(x), "doEvent"))))
      whSimList2 <- if (is.null(internalSimList)) {
        1
      } else {
        # this can be wrongly of length > 1 -- unclear why, but should be safe to take 1st
        which(internalSimList)[1]
      }

      isNewObj <- !names(postDigest$.list[[whSimList2]]) %in%
        names(preDigest[[whSimList]]$.list[[whSimList2]])
      newObjs <- names(postDigest$.list[[whSimList2]])[isNewObj]
      newObjs <- newObjs[!startsWith(newObjs, "._")]
      existingObjs <- names(postDigest$.list[[whSimList2]])[!isNewObj]
      post <- lapply(postDigest$.list[[whSimList2]][existingObjs], fastdigest::fastdigest)
      pre <- lapply(preDigest[[whSimList]]$.list[[whSimList2]][existingObjs], fastdigest::fastdigest)
      changedObjs <- names(post[!(unlist(post) %in% unlist(pre))])
      c(newObjs, changedObjs)
    } else {
      character()
    }

    .setSubAttrInList(object, ".Cache", "changed", changed)
    #attr(object, ".Cache")$changed <- changed
    if (!identical(attr(object, ".Cache")$changed, changed))
      stop("attributes on the cache object are not correct - 5")

    object
})

if (!isGeneric(".preDigestByClass")) {
  setGeneric(".preDigestByClass", function(object) {
    standardGeneric(".preDigestByClass")
  })
}

if (!isGeneric(".prepareOutput")) {
  setGeneric(".prepareOutput", function(object) {
    standardGeneric(".prepareOutput")
  })
}

#' \code{.prepareOutput} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.prepareOutput}}.
#'
#' @inheritParams reproducible::.prepareOutput
#'
#' @export
#' @exportMethod .prepareOutput
#' @include simList-class.R
#' @importFrom data.table setattr
#' @importFrom reproducible .prepareOutput
#' @importMethodsFrom reproducible .prepareOutput
#' @rdname prepareOutput
#' @seealso \code{\link[reproducible]{.prepareOutput}}
setMethod(
  ".prepareOutput",
  signature = "simList",
  definition = function(object, cacheRepo, ...) {
    tmpl <- list(...)
    tmpl <- .findSimList(tmpl)
    # only take first simList -- may be a problem:
    whSimList <- which(unlist(lapply(tmpl, is, "simList")))[1]
    simListInput <- !isTRUE(is.na(whSimList))
    if (simListInput) {
      origEnv <- tmpl[[whSimList]]@.xData

      isListOfSimLists <- if (is.list(object)) {
        if (is(object[[1]], "simList")) TRUE else FALSE
      } else {
        FALSE
      }

      if (isListOfSimLists) {
        object2 <- list()
        for (i in seq_along(object)) {
          # need to keep the list(...) slots ...
          # i.e., Caching of simLists is mostly about objects in .xData
          object2[[i]] <- Copy(tmpl[[whSimList]], objects = FALSE)
          object2[[i]]@.xData <- object[[i]]@.xData
          object2[[i]]@completed <- object[[i]]@completed
          object2[[i]]@simtimes <- object[[i]]@simtimes
          object2[[i]]@current <- object[[i]]@current
          object2[[i]]@events <- object[[i]]@events

          lsOrigEnv <- ls(origEnv, all.names = TRUE)
          keepFromOrig <- !(lsOrigEnv %in% ls(object2[[i]]@.xData, all.names = TRUE))
          # list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv),
          #          envir = object2[[i]]@.xData)
          list2env(mget(lsOrigEnv[keepFromOrig], envir = tmpl[[whSimList]]@.xData),
                   envir = object2[[i]]@.xData)
        }
      } else {
        # need to keep the tmpl slots ...
        # i.e., Caching of simLists is mostly about objects in .xData
        #   makes soft copy of all objects, i.e., they have the identical objects, which are pointers only
        object2 <- Copy(tmpl[[whSimList]], objects = FALSE)

        currModules <- currentModule(tmpl[[whSimList]])
        # Convert to numeric index, as some modules don't have names
        hasCurrModule <- match(currModules, modules(tmpl[[whSimList]]))
        if (length(currModules) == 0) currModules <- modules(tmpl[[whSimList]])

        createOutputs <- if (length(hasCurrModule)) {
          tmpl[[whSimList]]@depends@dependencies[[hasCurrModule]]@outputObjects$objectName
        } else {
          aa <- lapply(tmpl[[whSimList]]@depends@dependencies, function(dep)
            dep@outputObjects$objectName)
          unique(unlist(aa))
        }
        createOutputs <- na.omit(createOutputs)

        # add the environments for each module - allow local objects
        createOutputs <- c(createOutputs, currModules)

        # take only the ones that the file changed, based on attr(object, ".Cache")$changed
        changedOutputs <- createOutputs[createOutputs %in% attr(object, ".Cache")$changed]

        expectsInputs <- if (length(hasCurrModule)) {
          tmpl[[whSimList]]@depends@dependencies[[hasCurrModule]]@inputObjects$objectName
        } else {
          aa <- lapply(tmpl[[whSimList]]@depends@dependencies, function(dep)
            dep@inputObjects$objectName)
          unique(unlist(aa))
        }

        # Copy all objects from createOutputs only -- all others take from tmpl[[whSimList]]
        lsObjectEnv <- ls(object@.xData, all.names = TRUE)
        list2env(mget(lsObjectEnv[lsObjectEnv %in% changedOutputs | lsObjectEnv %in% expectsInputs],
                      envir = object@.xData), envir = object2@.xData)
        if (length(object2@current) == 0) {
          ## means it is not in a spades call
          object2@completed <- object@completed
        }
        if (NROW(current(object2)) == 0) {
          # this is usually a spades call, i.e., not an event or module doEvent call
          object2@events <- object@events
          object2@simtimes <- object@simtimes
        } else {
          # if this is FALSE, it means that events were added by the event
          eventsAddedByThisModule <- events(object)$moduleName == current(object2)$moduleName
          if (isTRUE(any(eventsAddedByThisModule))) {
            if (!isTRUE(all.equal(object@events, object2@events))) {
              b <- object@events
              b <- lapply(b, function(x) {x[["order"]] <- 2; x})

              d <- object2@events
              d <- lapply(d, function(x) {x[["order"]] <- 1; x})


              a <- do.call(unique,
                           args = list(append(b[eventsAddedByThisModule], d)))

              # a <- do.call(unique,
              #              args = list(append(object@events[eventsAddedByThisModule], object2@events)))
              a1 <- rbindlist(a)
              f1 <- a[order(a1$eventTime, a1$eventPriority, a1$order)]
              object2@events <- lapply(f1, function(f) {f$order <- NULL; f})
              # object2@events <- do.call(unique,
              #                           args = list(append(object@events[eventsAddedByThisModule], object2@events)))
            }
          }
          #object2@events <- unique(rbindlist(list(object@events, object2@events)))
        }
        object2@current <- object@current

        # This is for objects that are not in the return environment yet because they are unrelated to the
        #   current module -- these need to be copied over
        lsOrigEnv <- ls(origEnv, all.names = TRUE)
        keepFromOrig <- !(lsOrigEnv %in% ls(object2@.xData, all.names = TRUE))
        list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv), envir = object2@.xData)
      }
      if (!is.null(attr(object, "removedObjs"))) {
        if (length(attr(object, "removedObjs"))) {
          rm(list = attr(object, "removedObjs"), envir = object2@.xData)
        }
      }

      attrsToGrab <- setdiff(names(attributes(object)), names(attributes(object2)))
      for (atts in attrsToGrab) {
        setattr(object2, atts, attr(object, atts))
        #attr(object2, atts) <- attr(object, atts)
        if (!identical(attr(object2, atts), attr(object, atts)))
          stop("attributes on the cache object are not correct - 6")
      }

      return(object2)
    } else {
      return(object)
    }
})

#' Pre-digesting method for \code{simList}
#'
#' Takes a snapshot of \code{simList} objects.
#'
#' See \code{\link[reproducible]{.preDigestByClass}}.
#'
#' @inheritParams reproducible::.preDigestByClass
#'
#' @author Eliot McIntire
#' @export
#' @exportMethod .preDigestByClass
#' @importFrom reproducible .preDigestByClass
#' @importMethodsFrom reproducible .preDigestByClass
#' @include simList-class.R
#' @rdname preDigestByClass
#' @seealso \code{\link[reproducible]{.preDigestByClass}}
setMethod(
  ".preDigestByClass",
  signature = "simList",
  definition = function(object) {
    obj <- ls(object@.xData, all.names = TRUE)
    return(obj)
})

if (!isGeneric(".addTagsToOutput")) {
  setGeneric(".addTagsToOutput", function(object, outputObjects, FUN) {
    standardGeneric(".addTagsToOutput")
  })
}

#' \code{.addTagsToOutput} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.addTagsToOutput}}.
#'
#' @inheritParams reproducible::.addTagsToOutput
#'
#' @author Eliot McIntire
#' @exportMethod .addTagsToOutput
#' @export
#' @importFrom data.table setattr
#' @importFrom reproducible .addTagsToOutput
#' @importMethodsFrom reproducible .addTagsToOutput
#' @include simList-class.R
#' @rdname addTagsToOutput
#' @seealso \code{\link[reproducible]{.addTagsToOutput}}
setMethod(
  ".addTagsToOutput",
  signature = "simList",
  definition = function(object, outputObjects, FUN, preDigestByClass) {
    if (!is.null(outputObjects)) {
      outputToSave <- object
      outputToSave@.xData <- new.env()
      outputToSave@.envir <- outputToSave@.xData
      # Some objects are conditionally produced from a module's outputObject
      whExist <- outputObjects %in% ls(object@.xData, all.names = TRUE)
      list2env(mget(outputObjects[whExist], envir = object@.xData), envir = outputToSave@.xData)

      setattr(outputToSave, "tags", attr(object, "tags"))
      setattr(outputToSave, "call", attr(object, "call"))
      #attr(outputToSave, "tags") <- attr(object, "tags")
      #attr(outputToSave, "call") <- attr(object, "call")
      if (isS4(FUN)) {
        setattr(outputToSave, "function", attr(object, "function"))
        # attr(outputToSave, "function") <- attr(object, "function")
      }
      if (!identical(attr(outputToSave, "tags"), attr(object, "tags")))
        stop("attributes on the cache object are not correct - 1")
      if (!identical(attr(outputToSave, "call"), attr(object, "call")))
        stop("attributes on the cache object are not correct - 2")
    } else {
      outputToSave <- object
    }

    # Some objects are removed from a simList
    if (!is.null(preDigestByClass)) {
      lsInObj <- ls(object@.xData, all.names = TRUE)
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

#' \code{.objSizeInclEnviros} for \code{simList} objects
#'
#' See \code{\link[reproducible]{.objSizeInclEnviros}}.
#'
#' @inheritParams reproducible::.objSizeInclEnviros
#'
#' @export
#' @exportMethod .objSizeInclEnviros
#' @importFrom reproducible .objSizeInclEnviros
#' @importFrom utils object.size
#' @importMethodsFrom reproducible .objSizeInclEnviros
#' @include simList-class.R
#' @rdname objSizeInclEnviros
#' @seealso \code{\link[reproducible]{.objSizeInclEnviros}}
setMethod(
  ".objSizeInclEnviros",
  signature = "simList",
  definition = function(object) {
    object.size(as.list(object@.xData, all.names = TRUE)) + object.size(object)
})

#' Find \code{simList} in a nested list
#'
#' This is recursive, so it will find the all \code{simList}s even if they are deeply nested.
#'
#' @param x any object, used here only when it is a list with at least one
#'        \code{simList} in it
#'
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

if (!exists("objSize")) {
  objSize <- function(x, quick, enclosingEnvs, .prevEnvs, ...) UseMethod("objSize")
}

#' Object size for \code{simList}
#'
#' Recursively, runs \code{\link[reproducible]{objSize}} on the \code{simList} environment,
#' so it estimates the correct size of functions stored there (e.g., with their enclosing
#' environments) plus, it adds all other "normal" elements of the \code{simList}, e.g.,
#' \code{object.size(completed(sim))}.
#'
#' @export
#' @importFrom reproducible objSize
#' @inheritParams reproducible::objSize
#'
#' @examples
#' a <- simInit(objects = list(d = 1:10, b = 2:20))
#' objSize(a)
#' object.size(a)
objSize.simList <- function(x, quick = getOption("reproducible.quick", FALSE),
                            enclosingEnvs = TRUE, .prevEnvirs = list(), ...) {
  xObjName <- deparse(substitute(x))
  aa <- objSize(x@.xData, quick = quick, ...)
  bb <- as(x, "simList_")
  bb@.Data <- list()
  bbOs <- list(simList = object.size(bb))
  aa <- append(aa, bbOs)
  return(aa)
}

#' Make \code{simList} correctly work with \code{memoise}
#'
#' Because of the environment slot, \code{simList} objects don't correctly
#' memoise a \code{simList}.
#' This method for \code{simList} converts the object to a \code{simList_} first.
#'
#' @inheritParams reproducible::makeMemoisable
#'
#' @return A \code{simList_} object or a \code{simList}, in the case
#' of \code{unmakeMemoisable}.
#'
#' @importFrom reproducible makeMemoisable
#' @include simList-class.R
#' @rdname makeMemoisable
#' @seealso \code{\link[reproducible]{makeMemoisable}}
#' @export
makeMemoisable.simList <- function(x) {
  as(x, "simList_")
}

#' @export
#' @importFrom reproducible unmakeMemoisable
#' @inheritParams reproducible::unmakeMemoisable
#' @rdname makeMemoisable
unmakeMemoisable.simList_ <- function(x) {
  as(x, "simList")
}

#' Attach missing attributes from x to y
#'
#' This is an internal helper.
#'
#' @param x an object with attributes
#' @param y an object with attributes
#'
#'
#' @importFrom data.table setattr
#' @keywords internal
#' @rdname keepAttrs
.keepAttrs <- function(x, y, omitAttrs = c(".envir", ".list", ".xData", ".Data")) {
  keepAttrs <- setdiff(names(attributes(x)), names(attributes(y)))
  keepAttrs <- setdiff(keepAttrs, omitAttrs)
  keepAttrs <- keepAttrs[keepAttrs != "names"]

  for (att in keepAttrs) {
    setattr(y, att, attr(x, att))
    if (!identical(attr(y, att), attr(x, att)))
      stop("attributes on the cache object are not correct - 3")

    #attr(y, att) <- attr(x, att)
  }
  return(y)
}
