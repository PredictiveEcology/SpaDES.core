if (!isGeneric(".robustDigest")) {
  setGeneric(
    ".robustDigest",
    function(object, .objects, length = Inf, algo = "xxhash64", ...) {
      standardGeneric(".robustDigest")
    }
  )
}

#' `.robustDigest` for `simList` objects
#'
#' This is intended to be used within the `Cache` function, but can be used to evaluate what
#' a `simList` would look like once it is converted to a repeatably digestible object.
#'
#' See [reproducible::.robustDigest()].
#' This method strips out stuff from a `simList` class object that would make it otherwise not
#' reproducibly digestible between sessions, operating systems, or machines.
#' This will likely still not allow identical digest results across R versions.
#'
#' @inheritParams reproducible::.robustDigest
#'
#' @aliases Cache
#' @author Eliot McIntire
#' @exportMethod .robustDigest
#' @importFrom Require modifyList2
#' @importFrom reproducible asPath .orderDotsUnderscoreFirst .robustDigest .sortDotsUnderscoreFirst
#' @importMethodsFrom reproducible .robustDigest
#' @include simList-class.R
#' @rdname robustDigest
#' @seealso [reproducible::.robustDigest()]
setMethod(
  ".robustDigest",
  signature = "simList",
  definition = function(object, .objects, length, algo, quick, classOptions) {

    # browser(expr = exists("._robustDigest_1"))

    curMod <- currentModule(object)

    outerObjs <- ls(object@.xData, all.names = TRUE)
    moduleObjs <- ls(object@.xData$.mods, all.names = TRUE)
    moduleEnvirs <- mget(moduleObjs[moduleObjs %in% unlist(modules(object))],
                         envir = object@.xData$.mods)
    moduleObjs <- lapply(moduleEnvirs, function(me) ls(me, all.names = TRUE))
    allObjsInSimList <- append(list(".xData" = outerObjs), moduleObjs)
    allObjsInSimList$.xData <- allObjsInSimList$.xData[!allObjsInSimList$.xData %in% ".mods"]
    allEnvsInSimList <- append(list(.xData = object@.xData), moduleEnvirs)

    ord1 <- .orderDotsUnderscoreFirst(allObjsInSimList)
    ord2 <- .orderDotsUnderscoreFirst(names(allEnvsInSimList))
    allObjsInSimList <- allObjsInSimList[ord1]
    allEnvsInSimList <- allEnvsInSimList[ord2]
    # names(allEnvsInSimList) <- names(allObjsInSimList)

    isObjectEmpty <- if (!missing(.objects)) {
      if (!is.null(.objects)) {
        FALSE
      } else {
        TRUE
      }
    } else {
      TRUE
    }
    # browser(expr = exists("._robustDigest_2"))
    if (!isObjectEmpty) {
      # objects may be provided in a namespaced format: modName:objName --
      # e.g., coming from .parseModule
      objectsMods <- grep("\\.mods\\$", .objects, value = TRUE)
      objectsMods <- gsub("\\.mods\\$", "", objectsMods)
      names(objectsMods) <- objectsMods
      objects1ByModWhole <- lapply(objectsMods, function(mod) ls(envir = object@.xData$.mods[[mod]]))

      .objects <- grep("\\.mods\\$", .objects, value = TRUE, invert = TRUE)
      objects1 <- strsplit(.objects, split = ":")
      lens <- unlist(lapply(objects1, length))
      objects1ByMod <- unlist(lapply(objects1[lens > 1], function(x) x[1]))
      mods <- unique(objects1ByMod)
      objects2 <- lapply(mods, function(mod) {
        unlist(lapply(objects1[lens > 1][objects1ByMod == mod], function(x) x[[2]]))
      })
      names(objects2) <- mods
      .objects <- append(list(".xData" = unlist(objects1[lens == 1])), objects2)
      if (length(objects1ByModWhole))
        .objects <- suppressWarnings(modifyList2(.objects, objects1ByModWhole))
    } else {
      .objects <- allObjsInSimList
    }
    envirHash <- Map(objs = allObjsInSimList, name = names(allObjsInSimList),
                     function(objs, name) {
                       # browser(expr = exists("._robustDigest_5"))
                       dotUnderscoreObjs <- objs[startsWith(objs, "._")]
                       objs <- objs[!objs %in% c(dotUnderscoreObjs, "mod", "Par")]
                       objs <- objs[objs %in% .objects[[name]]]
                       if (length(objs) > 1) {
                         objs <- sort(objs, method = "radix")
                       }
                       out <- if (length(objs) > 0) {
                         a <- mget(objs, envir = allEnvsInSimList[[name]])
                         nonZero <- unlist(lapply(a, function(x) length(x) > 0))
                         .robustDigest(a[nonZero],
                                       quick = !isFALSE(quick), # can be character or TRUE --> TRUE
                                       length = length, classOptions = classOptions) # need classOptions
                       } else {
                         list()
                       }
                     })

    #names(envirHash) <- names(allObjsInSimList)
    lens <- unlist(lapply(envirHash, function(x) length(x) > 0))
    envirHash <- envirHash[lens]

    # demote .mods objects into .xData
    eh <- envirHash[names(envirHash)[names(envirHash) %in% names(moduleEnvirs)]]
    envirHash$.xData[names(eh)] <- eh
    envirHash[names(eh)] <- NULL

    # browser(expr = exists("._robustDigest_3"))
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
      omitParams <- c(".showSimilar", ".useCache")
      object@params <- object@params[curMod]
      object@params[[curMod]] <- object@params[[curMod]][!names(object@params[[curMod]]) %in% omitParams]
    }
    object@params <- lapply(object@params, function(x) .sortDotsUnderscoreFirst(x))
    object@params <- .sortDotsUnderscoreFirst(object@params)

    nonDotList <- grep(".list|.Data", slotNames(object), invert = TRUE, value = TRUE)
    obj <- list()
    obj$.list <- object@.Data
    if (length(obj$.list)) {
      objNames <- names(obj$.list[[1]])
      dotUnderscoreObjs <- objNames[startsWith(objNames, "._")]
      obj$.list[[1]][dotUnderscoreObjs] <- NULL

      # Now deal with ._ objects inside each module's environment
      objNamesInner <- lapply(obj$.list[[1]][], names)
      namesObjNamesInner <- names(objNamesInner)
      names(namesObjNamesInner) <- namesObjNamesInner
      nestedDotUnderscoreObjs <- lapply(namesObjNamesInner,
                                        function(x) {
                                          if (!is.null(objNamesInner[[x]]))
                                            objNamesInner[[x]][startsWith(objNamesInner[[x]], "._")]
                                          })
      nestedDotUnderscoreObjs <- nestedDotUnderscoreObjs[names(unlist(nestedDotUnderscoreObjs, recursive = FALSE))]
      noneToRm <- unlist(lapply(nestedDotUnderscoreObjs, function(x) length(x) == 0))
      nestedDotUnderscoreObjs[noneToRm] <- NULL
      obj$.list[[1]][names(nestedDotUnderscoreObjs)] <-
        Map(na = obj$.list[[1]][names(nestedDotUnderscoreObjs)],
            nduo = nestedDotUnderscoreObjs, function(na, nduo) {
              na[nduo] <- NULL
              na
      })
    }

    # outputs --> should not be treated like inputs; if they change, it is OK, so just outputs as a data.frame,
    #   not files
    nonDotListNoOutputs <- setdiff(nonDotList, "outputs")
    obj[nonDotListNoOutputs] <- lapply(nonDotListNoOutputs, function(x) .robustDigest(slot(object, x), algo = algo))
    obj["outputs"] <- .robustDigest(object@outputs, quick = TRUE)
    if (!is.null(classOptions$events))
      if (FALSE %in% classOptions$events) obj$events <- NULL
    if (!is.null(classOptions$current))
      if (FALSE %in% classOptions$current) obj$current <- NULL
    if (!is.null(classOptions$completed))
      if (FALSE %in% classOptions$completed) obj$completed <- NULL
    if (!is.null(classOptions$simtimes))
      if (FALSE %in% classOptions$simtimes) obj$simtimes <- NULL
    # browser(expr = exists("._robustDigest_4"))
    obj
})

if (!isGeneric(".tagsByClass")) {
  setGeneric(".tagsByClass", function(object) {
    standardGeneric(".tagsByClass")
  })
}

#' `.tagsByClass` for `simList` objects
#'
#' See [reproducible::.tagsByClass]. Adds current `moduleName`,
#' `eventType`, `eventTime`, and `function:spades` as `userTags`.
#'
#' @inheritParams reproducible::.tagsByClass
#'
#' @author Eliot McIntire
#' @exportMethod .tagsByClass
#' @importFrom reproducible .tagsByClass
#' @importFrom reproducible .grepSysCalls
#' @importMethodsFrom reproducible .tagsByClass
#' @include simList-class.R
#' @seealso [reproducible::.tagsByClass]
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

#' `.cacheMessage` for `simList` objects
#'
#' See [reproducible::.cacheMessage].
#'
#' @exportMethod .cacheMessage
#' @importFrom crayon blue
#' @importFrom reproducible .cacheMessage
#' @importMethodsFrom reproducible .cacheMessage
#' @inheritParams reproducible::.cacheMessage
#' @include simList-class.R
#' @rdname cacheMessage
#' @seealso [reproducible::.cacheMessage]
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
          message(crayon::blue("  Loading memoised copy of", cur$moduleName, "module\n"))
        } else if (!is.na(fromMemoise)) {
          message(crayon::blue("     loaded cached copy of", cur$moduleName, "module\n",
                           "adding to memoised copy\n"))
        } else {
          message(crayon::blue("     loaded ", fromWhere," copy of", cur$moduleName, "module\n"))
        }
      } else {
        if (isTRUE(fromMemoise)) {
          message(crayon::blue("     loaded memoised copy of", cur$eventType, "event in",
                           cur$moduleName, "module\n"))

        } else if (!is.na(fromMemoise)) {
          message(crayon::blue("     loaded cached copy of", cur$eventType, "event in",
                           cur$moduleName, "module. ",
                           if (fromMemoise) "Adding to memoised copy.",
                           "\n"))
        } else {
          message(crayon::blue("     loaded ", fromWhere," copy of", cur$eventType, "event in",
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

#' `.checkCacheRepo` for `simList` objects
#'
#' See [reproducible::.checkCacheRepo].
#'
#' @inheritParams reproducible::.checkCacheRepo
#'
#' @export
#' @exportMethod .checkCacheRepo
#' @importFrom reproducible .checkCacheRepo .grepSysCalls
#' @importMethodsFrom reproducible .checkCacheRepo
#' @include simList-class.R
#' @rdname checkCacheRepo
#' @seealso [reproducible::.checkCacheRepo]
setMethod(
  ".checkCacheRepo",
  signature = "list",
  definition = function(object, create = FALSE) {
    object <- .findSimList(object)
    whSimList <- unlist(lapply(object, is, "simList"))

    if (any(whSimList)) {
      # just take the first simList, if there are >1
      cachePath <- object[whSimList][[1]]@paths$cachePath
    } else {
      doEventFrameNum <- .grepSysCalls(sys.calls(), "(^doEvent)|(^.parseModule)")[2]

      if (!is.na(doEventFrameNum)) {
        sim <- get("sim", envir = sys.frame(doEventFrameNum))
        cachePath <- sim@paths$cachePath
      } else {
        cachePath <- .getOption("reproducible.cachePath")
        #checkPath(cachePath, create = TRUE) #SpaDES dependency
      }
    }
    checkPath(path = cachePath, create = create)
})

if (!isGeneric(".addChangedAttr")) {
  setGeneric(".addChangedAttr", function(object, preDigest, origArguments, ...) {
    standardGeneric(".addChangedAttr")
  })
}

#' `.addChangedAttr` for `simList` objects
#'
#' This will evaluate which elements in the `simList` object changed following
#' this Cached function call. It will add a named character string as an
#' attribute `attr(x, ".Cache")$changed`, indicating which ones changed.
#' When this function is subsequently called again, only these changed objects
#' will be returned. All other `simList` objects will remain unchanged.
#'
#' @inheritParams reproducible::.addChangedAttr
#'
#' @seealso [reproducible::.addChangedAttr].
#'
#' @export
#' @exportMethod .addChangedAttr
#' @importFrom reproducible .addChangedAttr .setSubAttrInList
#' @importMethodsFrom reproducible .addChangedAttr
#' @include simList-class.R
#' @rdname addChangedAttr
#' @seealso [reproducible::.addChangedAttr]
setMethod(
  ".addChangedAttr",
  signature = "simList",
  definition = function(object, preDigest, origArguments, ...) {
    dots <- list(...)
    whSimList <- which(unlist(lapply(origArguments, is, "simList")))[1]

    # browser(expr = exists("._addChangedAttr_5"))
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
      whSimList2 <- if (is.null(internalSimList) || isFALSE(internalSimList)) {
        1
      } else {
        # this can be wrongly of length > 1 -- unclear why, but should be safe to take 1st
        which(internalSimList)[1]
      }

      isNewObj <- !names(postDigest$.list[[whSimList2]]) %in%
        names(preDigest[[whSimList]]$.list[[whSimList2]])
      if (sum(isNewObj)) {
        newObjs <- names(postDigest$.list[[whSimList2]])[isNewObj]
        newObjs <- newObjs[!startsWith(newObjs, "._")]
        existingObjs <- names(postDigest$.list[[whSimList2]])[!isNewObj]
        post <- lapply(postDigest$.list[[whSimList2]][existingObjs], .robustDigest)
        pre <- lapply(preDigest[[whSimList]]$.list[[whSimList2]][existingObjs], .robustDigest)
        # browser()
        changedObjs <- setdiffNamedRecursive(post, pre)
        # changedObjs <- names(unlist(post)[!(unlist(post) %in% unlist(pre))])
        changed <- append(list(changedObjs = newObjs), changedObjs)
      } else {
        changed <- character()
      }
      changed
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

#' @importFrom Require setdiffNamed
setdiffNamedRecursive <- function(l1, l2, missingFill) {
  l1Different <- Require::setdiffNamed(l1, l2)
  if (length(l1Different)) {
    areList <- unlist(lapply(l1, is.list))
    if (any(areList)) {
      l1Different <- Map(nl1 = names(l1Different), function(nl1) {
        if (nl1 %in% names(l2)) {
          setdiffNamedRecursive(l1Different[[nl1]], l2[[nl1]])
        } else {
          l1Different
        }
      })
    }
  }
  l1Different
}

#' `.prepareOutput` for `simList` objects
#'
#' See [reproducible::.prepareOutput].
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
#' @seealso [reproducible::.prepareOutput]
setMethod(
  ".prepareOutput",
  signature = "simList",
  definition = function(object, cachePath, ...) {
    simFromCache <- object # rename for internal purposes
    simPre <- list(...)
    simPre <- .findSimList(simPre)
    # only take first simList -- may be a problem:
    whSimList <- which(unlist(lapply(simPre, is, "simList")))[1]
    simListInput <- !isTRUE(is.na(whSimList))
    if (simListInput) {
      origEnv <- simPre[[whSimList]]@.xData

      isListOfSimLists <- if (is.list(simFromCache)) {
        if (is(simFromCache[[1]], "simList")) TRUE else FALSE
      } else {
        FALSE
      }

      if (isListOfSimLists) {
        simPost <- list()
        for (i in seq_along(simFromCache)) {
          stop("It looks like there are more than one simList in the Cached objects; ",
               "Cache does not correctly deal with this currently.")
          # need to keep the list(...) slots ...
          # i.e., Caching of simLists is mostly about objects in .xData
          simPost[[i]] <- Copy(simPre[[whSimList]], objects = FALSE)
          simPost[[i]]@.xData <- simFromCache[[i]]@.xData
          simPost[[i]]@completed <- simFromCache[[i]]@completed
          simPost[[i]]@simtimes <- simFromCache[[i]]@simtimes
          simPost[[i]]@current <- simFromCache[[i]]@current
          simPost[[i]]@events <- simFromCache[[i]]@events

          lsOrigEnv <- ls(origEnv, all.names = TRUE)
          keepFromOrig <- !(lsOrigEnv %in% ls(simPost[[i]]@.xData, all.names = TRUE))
          # list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv),
          #          envir = simPost[[i]]@.xData)
          list2env(mget(lsOrigEnv[keepFromOrig], envir = simPre[[whSimList]]@.xData),
                   envir = simPost[[i]]@.xData)
        }
      } else {
        # Setup some things to use throughout
        currModules <- currentModule(simPre[[whSimList]])

        # Step 1 -- copy the non-simEnv slots
        simPost <- Copy(simPre[[whSimList]], objects = FALSE)


        # Step 2 -- copy the objects that are in simPre to simPost
        # objsInPre <- ls(simPre[[whSimList]]@.xData, all.names = TRUE)
        # objsInPre <- grep("^\\._", objsInPre, value = TRUE, invert = TRUE)
        # This needs to have different environments, i.e., like what Copy does
        # list2env(mget(objsInPre, envir = simPre[[whSimList]]@.xData), envir = simPost@.xData)

        # Step 2 -- figure out where to get objects in simEnv from -- preSim or simRecoveredFromCache
        # Convert to numeric index, as some modules don't have names

        # hasCurrModule <- match(currModules, modules(simPre[[whSimList]]))

        lsObjectEnv <- ls(simFromCache@.xData, all.names = TRUE)
        if (!is.null(simFromCache@.xData$.mods))
          changedModEnvObjs <- ls(simFromCache@.xData$.mods, all.names = TRUE)

        deps <- simPre[[whSimList]]@depends@dependencies
        namesAllMods <- names(deps)
        if (!is.null(namesAllMods)) { # i.e., no modules exist in the simList
          hasCurrModule <- match(currModules, names(deps))
          if (length(currModules) == 0) currModules <- namesAllMods

          createOutputs <- if (length(hasCurrModule)) {
            deps[[hasCurrModule]]@outputObjects$objectName
          } else {
            aa <- lapply(deps, function(dep) dep@outputObjects$objectName)
            unique(unlist(aa))
          }
          createOutputs <- na.omit(createOutputs)

          # add the environments for each module - allow local objects
          createOutputs <- c(createOutputs, currModules)

          # take only the ones that the file changed, based on attr(simFromCache, ".Cache")$changed
          changedOutputs <- createOutputs[createOutputs %in% attr(simFromCache, ".Cache")$changed$changedObjs]

          expectsInputs <- if (length(hasCurrModule)) {
            deps[[hasCurrModule]]@inputObjects$objectName
          } else {
            aa <- lapply(deps, function(dep)
              dep@inputObjects$objectName)
            unique(unlist(aa))
          }
          lsObjectEnv <- lsObjectEnv[lsObjectEnv %in% changedOutputs | lsObjectEnv %in% expectsInputs]
          if (!is.null(simFromCache@.xData$.mods)) {
            privateObjectsInModules <- attr(simFromCache, ".Cache")$changed
            objsWithChangeInners <- setdiff(names(privateObjectsInModules), "changedObjs")
            changedModEnvObjs <- privateObjectsInModules[objsWithChangeInners]
          }
        }

        # Copy all objects from createOutputs only -- all others take from simPre[[whSimList]]
        list2env(mget(lsObjectEnv, envir = simFromCache@.xData), envir = simPost@.xData)

        # Deal with .mods objects
        if (!is.null(simFromCache@.xData$.mods)) {
          sames <- list()
          # These are the unchanged objects
          for (modNam in currModules) {
            objs <-
              setdiffNamedRecursive(as.list(simPre[[1]]$.mods[[modNam]]$.objects, all.names = T), changedModEnvObjs[[modNam]]$.objects)
            list2env(objs, simPost$.mods[[modNam]]$.objects)
          }
          # Now changed objects
          if (length(changedModEnvObjs)) {
            Map(nam = names(changedModEnvObjs), objs = changedModEnvObjs, function(nam, objs) {
              objNames <- names(objs$.objects)
              list2env(mget(objNames, envir = simFromCache@.xData$.mods[[nam]][[".objects"]]),
                       envir = simPost@.xData$.mods[[nam]][[".objects"]])
            })
            # override everything first -- this includes .objects -- take from Cache
            # list2env(mget(changedModEnvObjs, envir = simFromCache@.xData$.mods), envir = simPost@.xData$.mods)
            # BUT functions are so lightweight that they should always return current
            if (length(currModules)) {
              lapply(currModules, function(currModule) {
                currMods <- simPre[[whSimList]]@.xData$.mods[[currModule]]
                objsInModuleActive <- ls(currMods, all.names = TRUE)
                dontCopyObjs <- c(".objects", "mod", "Par") # take these from the Cached copy (made 3 lines above)
                objsInModuleActive <- setdiff(objsInModuleActive, dontCopyObjs)
                if (length(objsInModuleActive))
                  list2env(mget(objsInModuleActive, envir = simPre[[whSimList]]@.xData$.mods[[currModule]]),
                           envir = simPost@.xData$.mods[[currModule]])
              })

            }
          }
          makeSimListActiveBindings(simPost)
        }


        if (length(simPost@current) == 0) {
          ## means it is not in a spades call
          simPost@completed <- simFromCache@completed
        }
        if (NROW(current(simPost)) == 0) {
          # this is usually a spades call, i.e., not an event or module doEvent call
          simPost@events <- simFromCache@events
          simPost@simtimes <- simFromCache@simtimes
        } else {
          # if this is FALSE, it means that events were added by the event
          eventsAddedByThisModule <- events(simFromCache)$moduleName == current(simPost)$moduleName
          if (isTRUE(any(eventsAddedByThisModule))) {
            if (!isTRUE(all.equal(simFromCache@events, simPost@events))) {
              b <- simFromCache@events
              b <- lapply(b, function(x) {x[["order"]] <- 2; x})

              d <- simPost@events
              d <- lapply(d, function(x) {x[["order"]] <- 1; x})


              a <- do.call(unique,
                           args = alist(append(b[eventsAddedByThisModule], d)))

              # a <- do.call(unique,
              #              args = list(append(simFromCache@events[eventsAddedByThisModule], simPost@events)))
              a1 <- rbindlist(a)
              f1 <- a[order(a1$eventTime, a1$eventPriority, a1$order)]
              simPost@events <- lapply(f1, function(f) {f$order <- NULL; f})
              # simPost@events <- do.call(unique,
              #                           args = list(append(simFromCache@events[eventsAddedByThisModule], simPost@events)))
            }
          }
          #simPost@events <- unique(rbindlist(list(simFromCache@events, simPost@events)))
        }
        simPost@current <- simFromCache@current

        # This is for objects that are not in the return environment yet because they are unrelated to the
        #   current module -- these need to be copied over
        lsOrigEnv <- ls(origEnv, all.names = TRUE)
        keepFromOrig <- !(lsOrigEnv %in% ls(simPost@.xData, all.names = TRUE))
        list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv), envir = simPost@.xData)

        # Deal with .mods
        lsOrigModsEnv <- ls(origEnv$.mods, all.names = TRUE)
        keepFromModsOrig <- !(lsOrigModsEnv %in% ls(simPost@.xData$.mods, all.names = TRUE))
        list2env(mget(lsOrigModsEnv[keepFromModsOrig], envir = origEnv$.mods), envir = simPost@.xData$.mods)

        if (exists("objectSynonyms", envir = simPost@.xData)) {
          objSyns <- lapply(attr(simPost$objectSynonyms, "bindings"), function(x) unname(unlist(x)))
          # must remove the "other ones" first
          objNonCanonical <- unlist(lapply(objSyns, function(objs) objs[-1]))
          objNonCanonicalExist <- unlist(lapply(objNonCanonical, exists, envir = simPost@.xData))
          if (any(objNonCanonicalExist))
            rm(list = objNonCanonical[objNonCanonicalExist], envir = simPost@.xData)
          suppressMessages(objectSynonyms(synonyms = objSyns, envir = simPost@.xData))
        }

      }
      if (!is.null(attr(simFromCache, "removedObjs"))) {
        if (length(attr(simFromCache, "removedObjs"))) {
          rm(list = attr(simFromCache, "removedObjs"), envir = simPost@.xData)
        }
      }

      attrsToGrab <- setdiff(names(attributes(simFromCache)), names(attributes(simPost)))
      for (atts in attrsToGrab) {
        setattr(simPost, atts, attr(simFromCache, atts))
        #attr(simPost, atts) <- attr(simFromCache, atts)
        if (!identical(attr(simPost, atts), attr(simFromCache, atts)))
          stop("attributes on the cache simFromCache are not correct - 6")
      }

      return(simPost)
    } else {
      return(simFromCache)
    }
})

#' Pre-digesting method for `simList`
#'
#' Takes a snapshot of `simList` objects.
#'
#' See [reproducible::.preDigestByClass].
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
#' @seealso [reproducible::.preDigestByClass]
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

#' `.addTagsToOutput` for `simList` objects
#'
#' See [reproducible::.addTagsToOutput].
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
#' @seealso [reproducible::.addTagsToOutput]
setMethod(
  ".addTagsToOutput",
  signature = "simList",
  definition = function(object, outputObjects, FUN, preDigestByClass) {
    if (!is.null(outputObjects)) {
      # browser(expr = exists("._addTagsToOutput_2"))
      outputToSave <- object
      outputToSave@.xData <- new.env(parent = emptyenv())
      outputToSave@.xData$.mods <- new.env(parent = asNamespace("SpaDES.core"))
      outputToSave@.envir <- outputToSave@.xData

      # Some objects are conditionally produced from a module's outputObject
      lsEnv <- ls(object@.xData, all.names = TRUE)
      whExist <- outputObjects %in% lsEnv
      list2env(mget(outputObjects[whExist], envir = object@.xData), envir = outputToSave@.xData)

      # Deal with .mods
      outputObjectsMods <- grep(".mods", outputObjects, value = TRUE)
      outputObjectsMods <- gsub("(.*)\\$", "", outputObjectsMods)
      list2env(mget(outputObjectsMods, envir = object@.xData$.mods),
               envir = outputToSave@.xData$.mods)

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


#' Find `simList` in a nested list
#'
#' This is recursive, so it will find the all `simList`s even if they are deeply nested.
#'
#' @param x any object, used here only when it is a list with at least one
#'        `simList` in it
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

#' Object size for `simList`
#'
#' Recursively, runs [reproducible::objSize()] on the `simList` environment,
#' so it estimates the correct size of functions stored there (e.g., with their enclosing
#' environments) plus, it adds all other "normal" elements of the `simList`, e.g.,
#' `objSize(completed(sim))`.
#' The output is structured into 2 elements: the sim environment and all its objects,
#' and the other slots in the `simList` (e.g., events, completed, modules, etc.).
#' The returned object also has an attribute, "total", which shows the total size.
#'
#' @importFrom reproducible objSize
#' @importFrom lobstr obj_size
#' @inheritParams reproducible::objSize
#' @export
#'
#' @examples
#' a <- simInit(objects = list(d = 1:10, b = 2:20))
#' objSize(a)
#' utils::object.size(a)
objSize.simList <- function(x, quick = TRUE, ...) {

  varName <- deparse(substitute(x))
  aa <- objSize(x@.xData, quick = quick, ...)

  simSlots <- grep("^\\.envir$|^\\.xData$", slotNames(x), value = TRUE, invert = TRUE)
  names(simSlots) <- simSlots
  otherParts <- objSize(lapply(simSlots, function(slotNam) slot(x, slotNam)), quick = quick, ...)

  total <- obj_size(x, quick = TRUE)
  if (!quick)
    attr(total, "objSizes") <- list(sim = attr(aa, "objSize"),
                                    other = attr(otherParts, "objSize"))

  return(total)
}

#' Make `simList` correctly work with `memoise`
#'
#' Because of the environment slot, `simList` objects don't correctly
#' memoise a `simList`.
#' This method for `simList` converts the object to a `simList_` first.
#'
#' @inheritParams reproducible::makeMemoisable
#'
#' @return A `simList_` object or a `simList`, in the case
#' of `unmakeMemoisable`.
#'
#' @importFrom reproducible makeMemoisable
#' @include simList-class.R
#' @rdname makeMemoisable
#' @seealso [reproducible::makeMemoisable()]
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

if (!isGeneric("clearCache")) {
  setGeneric(
    "clearCache",
    function(x, userTags = character(), after = NULL, before = NULL,
             ask = getOption("reproducible.ask"),
             useCloud = FALSE,
             cloudFolderID = NULL, ...) {
      standardGeneric("clearCache")
    }
  )
}

#' `clearCache` for `simList` objects
#'
#' This will take the `cachePath(object)` and pass
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`.
#' @param drv an object that inherits from `DBIDriver`, or an existing
#'            `DBIConnection` object (in order to clone an existing connection).
#' @inheritParams reproducible::clearCache
#'
#' @export
#' @importFrom reproducible clearCache
#' @importMethodsFrom reproducible clearCache
#' @rdname clearCache
setMethod(
  "clearCache",
  signature = "simList",
  definition = function(x, userTags, after = NULL, before = NULL, ask, useCloud = FALSE,
                        cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
                        drv = getOption("reproducible.drv", RSQLite::SQLite()),
                        conn = getOption("reproducible.conn", NULL), ...) {
    x <- x@paths$cachePath
    clearCache(x = x, userTags = userTags, after = after, before = before,
               ask = ask, useCloud = useCloud,
               cloudFolderID = cloudFolderID,
               ...)
})

if (!isGeneric("showCache")) {
  setGeneric("showCache", function(x, userTags = character(),
                                   after = NULL, before = NULL, ...) {
    standardGeneric("showCache")
  })
}

#' `showCache` for `simList` objects
#'
#' This will take the `cachePath(object)` and pass
#' @export
#'
#' @importFrom reproducible showCache
#' @importMethodsFrom reproducible showCache
#' @rdname clearCache
setMethod(
  "showCache",
  signature = "simList",
  definition = function(x, userTags, after = NULL, before = NULL, ...) {
    x <- x@paths$cachePath
    showCache(x = x, userTags = userTags, after = after, before = before, ...)
})

if (!isGeneric("keepCache")) {
  setGeneric("keepCache", function(x, userTags = character(),
                                   after = NULL, before = NULL, ...) {
    standardGeneric("keepCache")
  })
}

#' `keepCache` for `simList` objects
#'
#' This will take the `cachePath(object)` and pass
#' @export
#'
#' @importFrom reproducible keepCache
#' @importMethodsFrom reproducible keepCache
#' @rdname clearCache
setMethod(
  "keepCache",
  signature = "simList",
  definition = function(x, userTags, after = NULL, before = NULL, ...) {
    x <- x@paths$cachePath
    keepCache(x = x, userTags = userTags, after = after, before = before,
               ...)
})
