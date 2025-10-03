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
#' @importFrom Require modifyList2 invertList
#' @importFrom reproducible asPath .orderDotsUnderscoreFirst .robustDigest .sortDotsUnderscoreFirst
#' @importMethodsFrom reproducible .robustDigest
#' @include simList-class.R
#' @rdname robustDigest
#' @seealso [reproducible::.robustDigest()]
setMethod(
  ".robustDigest",
  signature = "simList",
  definition = function(object, .objects, length, algo = "xxhash64", quick, classOptions) {
    # browser(expr = exists("._robustDigest_1"))
    curMod <- currentModule(object)

    outerObjs <- ls(object@.xData, all.names = TRUE)
    moduleFunctions <- ls(object@.xData[[dotMods]], all.names = TRUE) # module names
    moduleFunctionEnvir <- mget(moduleFunctions[moduleFunctions %in% unlist(modules(object))],
                                envir = object@.xData[[dotMods]])  # module environments
    moduleFunctions <- lapply(moduleFunctionEnvir, function(me) ls(me, all.names = TRUE)) # obj names in .mods

    # Eliot addition May 2025 for new dotObjs
    allObjsInSimList <- list(".xData" = outerObjs)
    allEnvsInSimList <- list(".xData" = object@.xData)
    if (!is.null(object@.xData[[dotObjs]])) {
      moduleObjects <- ls(object@.xData[[dotObjs]], all.names = TRUE) # module names
      moduleObjEnvirs <- mget(moduleObjects[moduleObjects %in% unlist(modules(object))],
                              envir = object@.xData[[dotObjs]])  # module environments
      moduleObjects <- lapply(moduleObjEnvirs, function(me) ls(me, all.names = TRUE)) # obj names in .mods

      modFunsAndObjs <- append(list(moduleFunctions) |> setNames(.moduleFunctionsNam),
                               list(moduleObjects) |> setNames(.moduleObjectsNam))
      modEnvirFunsAndObjs <- append(list(moduleFunctionEnvir) |> setNames(.moduleFunctionsNam),
                                    list(moduleObjEnvirs) |> setNames(.moduleObjectsNam))
      allObjsInSimList <- modifyList2(allObjsInSimList, modFunsAndObjs)
      # allObjsInSimList <- append(append(list(".xData" = outerObjs), moduleFunctions),
      #                            moduleObjects |> setNames(paste0(dotObjs, "_", names(moduleObjects))))
      allObjsInSimList$.xData <- allObjsInSimList$.xData[!allObjsInSimList$.xData %in% dotObjsAndMods]
      # allEnvsInSimList <- append(append(list(.xData = object@.xData), moduleFunctionEnvir),
      #                                   moduleObjEnvirs |> setNames(paste0(dotObjs, "_", names(moduleObjEnvirs))))
      allEnvsInSimList <- modifyList2(allEnvsInSimList, modEnvirFunsAndObjs)

      # ord1 <- .orderDotsUnderscoreFirst(allObjsInSimList)
      # ord2 <- .orderDotsUnderscoreFirst(names(allEnvsInSimList))
      # allObjsInSimList <- allObjsInSimList[ord1]
      # allEnvsInSimList <- allEnvsInSimList[ord2]
 # Was conflicting development branch clode
#    moduleObjs <- ls(object@.xData$.mods, all.names = TRUE)
#    moduleEnvirs <- mget(
#      moduleObjs[moduleObjs %in% unlist(modules(object))],
#      envir = object@.xData$.mods
#    )
#    moduleObjs <- lapply(moduleEnvirs, function(me) ls(me, all.names = TRUE))
#    allObjsInSimList <- append(list(".xData" = outerObjs), moduleObjs)
#    allObjsInSimList$.xData <- allObjsInSimList$.xData[!allObjsInSimList$.xData %in% ".mods"]
#    allEnvsInSimList <- append(list(.xData = object@.xData), moduleEnvirs)
#
#    ord1 <- .orderDotsUnderscoreFirst(allObjsInSimList)
#    ord2 <- .orderDotsUnderscoreFirst(names(allEnvsInSimList))
#    allObjsInSimList <- allObjsInSimList[ord1]
#    allEnvsInSimList <- allEnvsInSimList[ord2]
#    # names(allEnvsInSimList) <- names(allObjsInSimList)

    }
    allObjsInSimList <- sortInner(allObjsInSimList)
    allEnvsInSimList <- sortInner(allEnvsInSimList)

    # if ("fireSense_dataPrepFit" %in% curMod) {
    #   # after .addChangedAttr --> .objects is a list with `moduleFunctions` and `moduleObjects` ... with `.robustDigest` alone, it doesn't
    #   aaaa <<- 1; on.exit(rm(aaaa, envir = .GlobalEnv))
    # }
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
      .objects <- buildDotObjectsList(object, .objects)
    } else {
      .objects <- allObjsInSimList
    }
    envirHash <- Map(objs = allObjsInSimList, xDataDotModDotObj = names(allObjsInSimList),
                     function(objs, xDataDotModDotObj) {

                       if (is.list(objs)) {
                         out <- Map(obj = objs, moduleNam = names(objs), function(obj, moduleNam) {
                           if (!is.null(.objects[[xDataDotModDotObj]][[moduleNam]]) && length(obj)) {
                             digestEnviros(obj, .objects[[xDataDotModDotObj]][[moduleNam]],
                                           allEnvsInSimList[[xDataDotModDotObj]][[moduleNam]], algo, quick, length, classOptions)
                           }
                         })

                       } else {
                         out <- digestEnviros(objs, .objects[[".xData"]], allEnvsInSimList[[".xData"]], algo, quick, length, classOptions)
                       }
                       out
                     })

    envirHash <- rmLength0Recursive(envirHash)
    envirHash <- upgradeModsToXdata(envirHash, upgradeModsToXdata, moduleFunctionEnvir)

    if (FALSE) {
      eh <- envirHash[names(envirHash)[names(envirHash) %in% names(moduleFunctionEnvir)]]
      envirHash$.xData[names(eh)] <- eh
      envirHash[names(eh)] <- NULL
    }

    # browser(expr = exists("._robustDigest_3"))
    ## Copy all parts except environment, clear that, then convert to list
    objectTmp <- object
    object <- Copy(object, objects = FALSE, queues = FALSE)
    object <- as(object, "simList_")
    ## Replace the .list slot with the hashes of the slots
    object@.Data <- envirHash

    ## Remove paths (i.e., dirs) as they are not relevant -- it is only the files that are relevant
    ##  i.e., if the same file is located in a different place, that is ok
    object@paths <- list()

    # Remove event queue from digest. The queue will be put back into the sim at the end (.prepareOutput),
    #    but it doesn't matter what it is for digesting
    # object@events <- list()

    # don't cache contents of output because file may already exist
    if (NROW(object@outputs)) {
      object@outputs$file <- basename(object@outputs$file)
      object@outputs$file <- tools::file_path_sans_ext(object@outputs$file) # could be qs or rds; doesn't matter for Cache
    }

    deps <- object@depends@dependencies
    for (i in seq_along(deps)) {
      if (!is.null(deps[[i]])) {
        object@depends@dependencies[[i]] <- lapply(
          slotNames(object@depends@dependencies[[i]]),
          function(x) {
            slot(object@depends@dependencies[[i]], x)
          }
        )
        names(object@depends@dependencies[[i]]) <- slotNames(deps[[i]])
        object@depends@dependencies[[i]][["timeframe"]] <- as.Date(deps[[i]]@timeframe)
      }
    }

    if (!is.null(classOptions$.globals)) {
      newGlobals <- object@params$.globals
    }

    if (!is.null(classOptions$modules)) {
      if (length(classOptions$modules)) {
        object@modules <- list(classOptions$modules)
        object@depends@dependencies <- object@depends@dependencies[classOptions$modules]
      }
    }

    # Inputs
    if (NROW(object@inputs)) {
      # Only include objects that are in the `inputs` slot that this module uses
      if (length(curMod)) { # If it is a simInitAndSpades call, it doesn't have a curMod
        expectsInputs <- deps[[curMod]]@inputObjects$objectName
        object@inputs <- object@inputs[object@inputs$objectName %in% expectsInputs,]
      }
      if (NROW(object@inputs)) { # previous line may have removed row(s) from object@inputs, leaving potentially zero
        object@inputs$file <- unlist(.robustDigest(object@inputs$file, algo = algo, quick = quick, length = length)) #nolint
      }
    }

    # Params
    # if this call is within a single module, only keep module-specific params
    if (length(curMod) > 0) {
      omitParams <- c(".showSimilar", ".useCache")
      object@params <- object@params[curMod]
      object@params[[curMod]] <- object@params[[curMod]][
        !names(object@params[[curMod]]) %in% omitParams
      ]
    }
    object@params <- lapply(object@params, function(x) .sortDotsUnderscoreFirst(x))
    object@params <- .sortDotsUnderscoreFirst(object@params)

    # globals
    if (!is.null(classOptions$.globals)) {
      object@params <- append(list(.globals = newGlobals), object@params)
    }

    nonDotList <- grep(".list|.Data", slotNames(object), invert = TRUE, value = TRUE)
    obj <- list()
    obj$.list <- object@.Data
    if (length(obj$.list)) {
      # Now deal with ._ objects inside each module's environment
      obj <- rmDotUnderscoresInModules(obj, names)

    }

    # outputs --> should not be treated like inputs; if they change, it is OK, so just outputs as a data.frame,
    #   not files
    nonDotListNoOutputs <- outputsRmDontNeedForCache(nonDotList, "outputs")
    # nonDotListNoOutputs <- setdiff(nonDotList, "outputs")
    dependsSeparate <- setdiff(nonDotListNoOutputs, "depends")
    obj[dependsSeparate] <- lapply(dependsSeparate, function(x) {
      .robustDigest(slot(object, x), algo = algo)
    })
    dependsFirst <- obj[["depends"]] <- list()
    if (!isTRUE(all(sapply(object@depends@dependencies, is.null)))) {
      for (ii in c("inputObjects", "outputObjects", "parameters")) {
        dependsFirst[[ii]] <-
          .robustDigest(lapply(object@depends@dependencies,
                               function(mo) {
                                 mo[[ii]][, grep("desc$", colnames(mo[[ii]]), value = TRUE, invert = TRUE)]
                               }), algo = algo)
      }

      obj[["depends"]] <- invertList(dependsFirst)
    }

    # outputs -- we only care if it was an output from this module
    if (length(curMod) > 0) {
      outputsFromThisMod <- object@depends@dependencies[[curMod]]$outputObjects$objectName
      object@outputs <- object@outputs[object@outputs$objectName %in% outputsFromThisMod,]
    }

    otherDependsToDig <- c("childModules", "loadOrder", "reqdPkgs",
                           "spatialExtent", "timeframe", "timeunit", "version")
    dependsSecond <-
      .robustDigest(lapply(object@depends@dependencies,
                           function(mo) {
                             mo[otherDependsToDig]
                           } ), algo = algo)

    obj[["depends"]] <- modifyList2(obj[["depends"]], dependsSecond)
    # obj[["depends"]] <- .robustDigest(object@depends@dependencies, algo = algo)
    obj <- .sortDotsUnderscoreFirst(obj)
    obj["outputs"] <- .robustDigest(object@outputs[, c("objectName", "saveTime", "file", "arguments")],
                                    quick = TRUE, algo = algo)
    if (!is.null(classOptions$depends)) { # this is used for Cache(.inputObjects(...))
      keep <- intersect(names(obj$depends[[curMod]]), classOptions$depends)
      obj$depends[[curMod]] <- obj$depends[[curMod]][keep]
    }

    if (!is.null(classOptions$events)) {
      if (FALSE %in% classOptions$events) obj$events <- NULL
    }
    if (!is.null(classOptions$current)) {
      if (FALSE %in% classOptions$current) obj$current <- NULL
    }
    if (!is.null(classOptions$completed)) {
      if (FALSE %in% classOptions$completed) obj$completed <- NULL
    }
    if (!is.null(classOptions$simtimes)) {
      if (FALSE %in% classOptions$simtimes) obj$simtimes <- NULL
    }
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
#' See [reproducible::.tagsByClass()]. Adds current `moduleName`,
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
        paste0("otherFunctions:spades")
      ) # add this because it will be an outer function, if there are events occurring
    } else {
      scalls <- sys.calls()
      parseModuleFrameNum <- .grepSysCalls(scalls, "^.parseModule")[2]
      if (!is.na(parseModuleFrameNum)) {
        inObj <- .grepSysCalls(scalls, pattern = "^.inputObjects")
        if (any(!is.na(inObj))) {
          userTags <- c("otherFunctions:.inputObjects")
          userTags1 <- tryCatch(
            paste0("module:", get("m", sys.frame(parseModuleFrameNum))),
            error = function(x) NULL
          )
          userTags <- c(userTags, userTags1)
        }
      } else {
        userTags <- NULL
      }
    }
    userTags
  })

if (!isGeneric(".cacheMessage")) {
  setGeneric(".cacheMessage", function(object, functionName, fromMemoise, verbose) {
    standardGeneric(".cacheMessage")
  })
}

#' `.cacheMessage` for `simList` objects
#'
#' See [reproducible::.cacheMessage()].
#'
#' @exportMethod .cacheMessage
#' @importFrom cli col_blue
#' @importFrom reproducible .cacheMessage messageCache
#' @importMethodsFrom reproducible .cacheMessage
#' @inheritParams reproducible::.cacheMessage
#' @include simList-class.R
#' @rdname cacheMessage
#' @seealso [reproducible::.cacheMessage]
setMethod(
  ".cacheMessage",
  signature = "simList",
  definition = function(
    object,
    functionName,
    fromMemoise = getOption("reproducible.useMemoise", TRUE),
    verbose = getOption("reproducible.verbose")
  ) {
    cur <- current(object)
    .cacheMessage(NULL, functionName, fromMemoise = fromMemoise, verbose = verbose)

    if (NROW(cur)) {
      whichCached <- grep(".useCache", object@params)
      useCacheVals <- lapply(whichCached, function(x) {
        object@params[[x]]$.useCache
      })

      whCurrent <- match(cur$moduleName, names(object@params)[whichCached])
      if (is.na(fromMemoise)) {
        fromMemoise <- FALSE
      }
      fromWhere <- c("cached", "memoised")[fromMemoise + 1]
      # if (isTRUE(useCacheVals[[whCurrent]])) {
      #   if (isTRUE(fromMemoise)) {
      #     message(cli::col_blue("  Loading memoised copy of", cur$moduleName, "module"))
      #   } else if (!is.na(fromMemoise)) {
      #     message(cli::col_blue("     loaded cached copy of", cur$moduleName, "module"),
      #             "\n        ",
      #             cli::col_blue(.message$AddingToMemoised))
      #   } else {
      #     message(cli::col_blue("     loaded ", fromWhere," copy of", cur$moduleName, "module"))
      #   }
      # } else {
      messageCache(
        .message$HangingIndent,
        "for ",
        cur$eventType,
        " event in ",
        cur$moduleName,
        " module",
        verbose = verbose
      )

      # }
    } else {
      messageCache(.message$HangingIndent, "from ", cur$moduleName, " module", verbose = verbose)
    }
  })

if (!isGeneric(".checkCacheRepo")) {
  setGeneric(".checkCacheRepo", function(object, create = FALSE) {
    standardGeneric(".checkCacheRepo")
  })
}

#' `.checkCacheRepo` for `simList` objects
#'
#' See [reproducible::.checkCacheRepo()].
#'
#' @inheritParams reproducible::.checkCacheRepo
#'
#' @return character string representing a directory path to the cache repo
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
      ## just take the first simList, if there are >1
      cachePath <- object[whSimList][[1]]@paths$cachePath
    } else {
      doEventFrameNum <- .grepSysCalls(sys.calls(), "(^doEvent)|(^.parseModule)")[2]

      if (!is.na(doEventFrameNum)) {
        sim <- get("sim", envir = sys.frame(doEventFrameNum))
        cachePath <- sim@paths$cachePath
      } else {
        cachePath <- .getOption("reproducible.cachePath")
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
#' @return returns the object with attribute added
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
    whSimList <- names(whSimList)

    # remove the "newCache" attribute, which is irrelevant for digest
    if (!is.null(attr(object, ".Cache")$newCache)) {
      object <- .setSubAttrInList(object, ".Cache", "newCache", NULL)

      if (!identical(attr(object, ".Cache")$newCache, NULL)) {
        stop("attributes on the cache object are not correct - 4")
      }
    }

    postDigest <- .robustDigest(object, .objects = dots[[.objectsArg]],
                                length = dots$length,
                                algo = dots$algo,
                                quick = dots$quick,
                                classOptions = dots$classOptions)
    changed <- if (length(postDigest$.list)) {
      internalSimList <- unlist(lapply(preDigest[[whSimList]]$.list, function(x) {
        !any(startsWith(names(x), "doEvent"))
      }))
      whSimList2 <- if (is.null(internalSimList) || isFALSE(internalSimList)) {
        1
      } else {
        # this can be wrongly of length > 1 -- unclear why, but should be safe to take 1st
        which(internalSimList)[1]
      }

      # remove all functions from the module environment; they aren't allowed to be redefined within a function
      if (length(preDigest[[whSimList]]$.list)) {
        out <- setdiffNamedRecursive(postDigest$.list[[whSimList2]],
                                     preDigest[[whSimList]]$.list[[whSimList2]])
        if (length(out) == 0) {
          out <- Map(modNam = names(postDigest$.list[[whSimList2]]), function(modNam) {
            list()
          })
        }
      } else {
        out <- postDigest$.list[[whSimList2]]
      }
      modulesInObject <- modules(object)
      for (modNam in modulesInObject) {
        isModElement <- names(out) == modNam
        if (any(isModElement)) {
          isDotObjects <- names(out[isModElement][[modNam]]) == .objectsSlot
          if (any(!isDotObjects)) # removes functions
            out[isModElement][[modNam]][!isDotObjects] <- NULL
        }
      }

      # remove empty elements, but keep module names with list
      changedObjs <- out[lengths(out) > 0 | (names(out) %in% modulesInObject)]

      changed <- changedObjs
      if (!any(modulesInObject %in% names(changed)) && NROW(object@current)) { # NROW object@current is for Caching of sim, pre-module running
        currMod <- object@current[[2]]
        changed <- append(changed, list(list()) |> setNames(currMod))
      }
      changed
    } else {
      character()
    }

    object <- .setSubAttrInList(object, ".Cache", "changed", changed)
    #attr(object, ".Cache")$changed <- changed
    if (!identical(attr(object, ".Cache")$changed, changed)) {
      stop("attributes on the cache object are not correct - 5")
    }

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
    areList <- unlist(lapply(l1Different, is, "list"))
    if (any(areList)) {
      l1Different[areList] <- Map(nl1 = names(l1Different)[areList], function(nl1) {
        if (nl1 %in% names(l2)) {
          setdiffNamedRecursive(l1Different[[nl1]], l2[[nl1]])
        } else {
          l1Different[[nl1]]
        }
      })
    }
    if (any(!areList)) {
      l1Different[!areList] <- Require::setdiffNamed(l1Different[!areList], l2)
    }
  } else {
    l1Different <- Require::setdiffNamed(l1, l2)
  }
  l1Different
}

#' `.prepareOutput` for `simList` objects
#'
#' See [reproducible::.prepareOutput()].
#'
#' @inheritParams reproducible::.prepareOutput
#'
#' @return the modified `object`
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
    # if ("Biomass_borealDataPrep" %in% currentModule(object)) browser()
    simFromCache <- object # rename for internal purposes
    hasDotObjs <- !is.null(simFromCache@.xData[[dotObjs]])
    if (hasDotObjs %in% FALSE) {
      messageColoured(colour = "red", "This object is using the old mod object structure; it must be \n",
                   "deleted. Rerunning it now ... ")
      clearCache(x = cachePath, cacheId = cacheId(object), ask = FALSE)
      return(invisible(reproducible:::.returnNothing))
    }

    simPre <- list(...)
    simPre <- .findSimList(simPre)
    ## only take first simList -- may be a problem:
    whSimList <- which(unlist(lapply(simPre, is, "simList")))[1]
    simListInput <- !isTRUE(is.na(whSimList))

    if (simListInput) {
      simPreOrigEnv <- simPre[[whSimList]]@.xData

      isListOfSimLists <- if (is.list(simFromCache)) {
        if (is(simFromCache[[1]], "simList")) TRUE else FALSE
      } else {
        FALSE
      }

      if (isListOfSimLists) {
        simPost <- list()
        for (i in seq_along(simFromCache)) {
          stop(
            "It looks like there are more than one simList in the Cached objects; ",
            "Cache does not correctly deal with this currently."
          )
          ## need to keep the list(...) slots ...
          ## i.e., Caching of simLists is mostly about objects in .xData
          simPost[[i]] <- Copy(simPre[[whSimList]], objects = FALSE)
          simPost[[i]]@.xData <- simFromCache[[i]]@.xData
          simPost[[i]]@completed <- simFromCache[[i]]@completed
          simPost[[i]]@simtimes <- simFromCache[[i]]@simtimes
          simPost[[i]]@current <- simFromCache[[i]]@current
          simPost[[i]]@events <- simFromCache[[i]]@events

          lsSimPreOrigEnv <- ls(simPreOrigEnv, all.names = TRUE)
          keepFromOrig <- !(lsSimPreOrigEnv %in% ls(simPost[[i]]@.xData, all.names = TRUE))
          # list2env(mget(lsSimPreOrigEnv[keepFromOrig], envir = simPreOrigEnv),
          #          envir = simPost[[i]]@.xData)
          list2env(
            mget(lsSimPreOrigEnv[keepFromOrig], envir = simPre[[whSimList]]@.xData),
            envir = simPost[[i]]@.xData
          )
        }
      } else {

        # Setup some things to use throughout
        currModules <- currentModule(simPre[[whSimList]])

        ## Step 1 -- copy the non-simEnv slots
        simPost <- Copy(simPre[[whSimList]], objects = FALSE)

        ## This was unnecessary if the parameters never change; but they can
        ##  -- but draw from Cache only from this module -- other modules may have
        ##     changed during this simInit/spades call --> don't want their cached copies
        ##   UNLESS they changed via the updateParamsFromGlobals mechanism!! Then DO want
        ##   cached copy of other modules
        simPost@params[currModules] <- simFromCache@params[currModules]
        if (FALSE) {
          ## Eliot Dec 2023 -- removed this because it is already occuring during simInit;
          ##  also, user may have passed individual values for individual modules that should not
          ##  be overridden by globals
          anyNewGlobals <- setdiffNamed(simFromCache@params$.globals, simPost@params$.globals)
          if (length(anyNewGlobals)) {
            suppressMessages(
              simPost@params <- updateParamsSlotFromGlobals(simPost@params, simFromCache@params)
            )
          }
        }

        ## Step 2 -- copy the objects that are in simPre to simPost
        # objsInPre <- ls(simPre[[whSimList]]@.xData, all.names = TRUE)
        # objsInPre <- grep("^\\._", objsInPre, value = TRUE, invert = TRUE)
        ## This needs to have different environments, i.e., like what Copy does
        # list2env(mget(objsInPre, envir = simPre[[whSimList]]@.xData), envir = simPost@.xData)

        ## Step 2 -- figure out where to get objects in simEnv from -- preSim or simRecoveredFromCache
        ## Convert to numeric index, as some modules don't have names

        # hasCurrModule <- match(currModules, modules(simPre[[whSimList]]))

        lsObjectEnv <- ls(simFromCache@.xData, all.names = TRUE)
        # changedModEnv <- list()
        # for (dotType in dotObjsAndMods) {
        #   if (!is.null(simFromCache@.xData[[dotType]]))
        #     changedModEnv[[dotType]] <- ls(simFromCache@.xData[[dotType]], all.names = TRUE)
        # }
        if (!is.null(simFromCache@.xData[[dotObjs]]))
          changedModEnvObjs <- ls(simFromCache@.xData[[dotObjs]], all.names = TRUE)

        deps <- simPre[[whSimList]]@depends@dependencies
        namesAllMods <- names(deps)
        if (!is.null(namesAllMods)) { # i.e., are there modules in the simList
          hasCurrModule <- match(currModules, names(deps))
          if (length(currModules) == 0) {
            currModules <- namesAllMods
          }

          changedObjs <- attr(simFromCache, ".Cache")$changed
          lsObjectEnv <- lsObjectsChanged(lsObjectEnv, changedObjs,
                                          hasCurrModule, currModules, deps)
          hasDotObjs <- !is.null(simFromCache@.xData[[dotObjs]])
          changedModEnvObjs <- lsModObjectsChanged(namesAllMods, changedObjs, hasDotObjs)

        }

        # Copy all objects from createOutputs only -- all others take from simPre[[whSimList]]
        list2env(mget(lsObjectEnv, envir = simFromCache@.xData), envir = simPost@.xData)

        otherModules <- setdiff(namesAllMods, currModules)
        # Need to pull all things from "other modules" i.e., functions and .objects etc. from non currModules
        if (length(currModules)) {
          lapply(currModules, function(currModule) {
            copyModuleSpecificFunsAndObjs(simPre[[whSimList]], simPost, currModule)
          })
        }
        if (length(otherModules)) {
          lapply(otherModules, function(otherModule) {
            copyModuleSpecificFunsAndObjs(simPre[[whSimList]], simPost, otherModule)
          })
        }

        # Deal with .mods objects
        if (!is.null(simFromCache@.xData[[dotMods]])) {
          # These are the unchanged objects
          for (modNam in currModules) {
            modObjsInList <- as.list(simPre[[1]][[dotMods]][[modNam]], all.names = TRUE)
            if (length(modObjsInList)) {
              objs <- try(
                setdiffNamedRecursive(modObjsInList,
                                      changedModEnvObjs[[modNam]])
              )
              if (is(objs, "try-error")) browser()
              if (length(objs))
                list2env(objs, simPost[[dotMods]][[modNam]])
            }
          }
          # Now changed objects
          if (length(unlist(changedModEnvObjs))) {
            Map(nam = names(changedModEnvObjs), objs = changedModEnvObjs, function(nam, objs) {
              objNames <- names(objs[[.objectsSlot]]) # used to be "names(...)" -- but don't want `._` objs
              objNames <- grep("^._.+", objNames, value = TRUE, invert = TRUE)
              if (!is.null(objNames))
                list2env(mget(objNames, envir = simFromCache@.xData[[dotObjs]][[nam]]),
                         envir = simPost@.xData[[dotObjs]][[nam]])
            })
            # override everything first -- this includes .objects -- take from Cache
            # list2env(mget(changedModEnvObjs, envir = simFromCache@.xData[[dotMods]]), envir = simPost@.xData[[dotMods]])
            # BUT functions are so lightweight that they should always return current
          }
        }
        makeSimListActiveBindings(simPost)

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

          esfc <- events(simFromCache)
          cur <- current(simFromCache)

          # anti-join to find new ones
          eventsAddedByThisModuleDT <- esfc[!events(simPost), on = colnames(esfc)]

          eventsAddedByThisModule <- esfc$moduleName %in% currModules # can only add itself

          if (NROW(eventsAddedByThisModuleDT)) {
            # if (!isTRUE(all.equal(simFromCache@events, simPost@events))) {
              b <- simFromCache@events
              b <- lapply(b, function(x) {
                x[["order"]] <- 2
                x
              })

              d <- simPost@events
              d <- lapply(d, function(x) {
                x[["order"]] <- 1
                x
              })

              a <- do.call(unique, args = alist(append(b[eventsAddedByThisModule], d)))
              if (length(a)) {

                # a <- do.call(unique,
                #              args = list(append(simFromCache@events[eventsAddedByThisModule], simPost@events)))
                a1 <- rbindlist(a)
                # f1 <- if (NROW(a1)) a[order(a1$eventTime, a1$eventPriority, a1$order)] else a1
                f1 <- a[order(a1$eventTime, a1$eventPriority, a1$order)]
                simPost@events <- lapply(f1, function(f) {
                  f$order <- NULL
                  f
                })
              }
              # simPost@events <- do.call(unique,
              #                           args = list(append(simFromCache@events[eventsAddedByThisModule], simPost@events)))
            # }
          }
          #simPost@events <- unique(rbindlist(list(simFromCache@events, simPost@events)))
        }
        simPost@current <- simFromCache@current

        # Outputs -- there may have been outputs added by another module that should be recovered
        if (length(currModules) > 0) {
          outputsFromTheseMods <- lapply(currModules, function(cmod) {
            object@depends@dependencies[[cmod]]@outputObjects$objectName
          })
          outputsFromTheseMods <- unlist(outputsFromTheseMods)

          ooo <- rbindlist(list(
            simPost@outputs, object@outputs[!object@outputs$objectName %in% outputsFromTheseMods,]),
            use.names = TRUE, fill = TRUE)
          allowedColumnsForUnique <- (sapply(ooo, is, "AsIs") | sapply(ooo, is, "list")) %in% FALSE
          simPost@outputs <- unique(ooo, by = names(ooo)[allowedColumnsForUnique])

        }


        # This is for objects that are not in the return environment yet because they are unrelated to the
        #   current module -- these need to be copied over
        lsSimPreOrigEnv <- ls(simPreOrigEnv, all.names = TRUE)
        keepFromOrig <- !(lsSimPreOrigEnv %in% ls(simPost@.xData, all.names = TRUE))
        list2env(mget(lsSimPreOrigEnv[keepFromOrig], envir = simPreOrigEnv), envir = simPost@.xData)

        # Deal with .mods
        # lsOrigModsEnv <- ls(simPreOrigEnv[[dotMods]], all.names = TRUE)
        # keepFromModsOrig <- !(lsOrigModsEnv %in% ls(simPost@.xData[[dotMods]], all.names = TRUE))
        # list2env(mget(lsOrigModsEnv[keepFromModsOrig], envir = simPreOrigEnv[[dotMods]]), envir = simPost@.xData[[dotMods]])

        if (exists(".objectSynonyms", envir = simPost@.xData)) {
          # objSyns <- lapply(attr(simPost$.objectSynonyms, "bindings"), function(x) unname(unlist(x)))
          objSyns <- lapply(simPost$.objectSynonyms, function(x) unname(unlist(x)))
          # must remove the "other ones" first
          objNonCanonical <- unlist(lapply(objSyns, function(objs) objs[-1]))
          objNonCanonicalExist <- unlist(lapply(objNonCanonical, exists, envir = simPost@.xData))
          if (any(objNonCanonicalExist)) {
            rm(list = objNonCanonical[objNonCanonicalExist], envir = simPost@.xData)
          }
          suppressMessages(objectSynonyms(synonyms = objSyns, envir = simPost@.xData))
        }
      }
      if (!is.null(attr(simFromCache, "removedObjs"))) {
        if (length(attr(simFromCache, "removedObjs"))) {
          rm(list = attr(simFromCache, "removedObjs"), envir = simPost@.xData)
        }
      }

      # Need the .Cache attributes from the recovered simFromCache
      attr(simPost, ".Cache") <- attr(simFromCache, ".Cache")

      attrsToGrab <- setdiff(names(attributes(simFromCache)), names(attributes(simPost)))
      for (atts in attrsToGrab) {
        setattr(simPost, atts, attr(simFromCache, atts))
        #attr(simPost, atts) <- attr(simFromCache, atts)
        if (!identical(attr(simPost, atts), attr(simFromCache, atts))) {
          stop("attributes on the cache simFromCache are not correct - 6")
        }
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
#' See [reproducible::.preDigestByClass()].
#'
#' @inheritParams reproducible::.preDigestByClass
#'
#' @return character vector corresponding to the names of objects stored in the `.xData` slot
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
#' See [reproducible::.addTagsToOutput()].
#'
#' @inheritParams reproducible::.addTagsToOutput
#'
#' @return modified `object`, with attributes added
#'
#' @author Eliot McIntire
#' @exportMethod .addTagsToOutput
#' @export
#' @importFrom data.table setattr
#' @importFrom reproducible .addTagsToOutput
#' @importMethodsFrom reproducible .addTagsToOutput
#' @include simList-class.R
#' @rdname addTagsToOutput
setMethod(
  ".addTagsToOutput",
  signature = "simList",
  definition = function(object, outputObjects, FUN, preDigestByClass) {
    if (!is.null(outputObjects)) {
      # browser(expr = exists("._addTagsToOutput_2"))
      outputToSave <- object
      outputToSave@.xData <- new.env(parent = emptyenv())
      outputToSave@.xData[[dotMods]] <- new.env(parent = asNamespace("SpaDES.core"))
      outputToSave@.envir <- outputToSave@.xData

      # Some objects are conditionally produced from a module's outputObject
      lsEnv <- ls(object@.xData, all.names = TRUE)
      whExist <- outputObjects %in% lsEnv
      list2env(mget(outputObjects[whExist], envir = object@.xData), envir = outputToSave@.xData)

      # Deal with .mods
      outputObjectsMods <- grep(".mods", outputObjects, value = TRUE)
      outputObjectsMods <- gsub("(.*)\\$", "", outputObjectsMods)
      list2env(mget(outputObjectsMods, envir = object@.xData[[dotMods]]),
               envir = outputToSave@.xData[[dotMods]])

      setattr(outputToSave, "tags", attr(object, "tags"))
      setattr(outputToSave, "call", attr(object, "call"))
      #attr(outputToSave, "tags") <- attr(object, "tags")
      #attr(outputToSave, "call") <- attr(object, "call")
      if (isS4(FUN)) {
        setattr(outputToSave, "function", attr(object, "function"))
        # attr(outputToSave, "function") <- attr(object, "function")
      }
      if (!identical(attr(outputToSave, "tags"), attr(object, "tags"))) {
        stop("attributes on the cache object are not correct - 1")
      }
      if (!identical(attr(outputToSave, "call"), attr(object, "call"))) {
        stop("attributes on the cache object are not correct - 2")
      }
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
#' The output is structured into 2 elements: the `sim` environment and all its objects,
#' and the other slots in the `simList` (e.g., events, completed, modules, etc.).
#' The returned object also has an attribute, "total", which shows the total size.
#'
#' @importFrom reproducible objSize .objSizeWithTry
#' @importFrom lobstr obj_size
#' @inheritParams reproducible::objSize
#'
#' @return an estimate of the size of the object, in bytes.
#'
#' @export
#' @examples
#' a <- simInit(objects = list(d = 1:10, b = 2:20))
#' objSize(a)
#' utils::object.size(a)
objSize.simList <- function(x, quick = FALSE, recursive = FALSE, ...) {

  total <- .objSizeWithTry(x)
  # total <- try(obj_size(x, quick = TRUE), silent = TRUE) # failing due to lobstr issue #72
  if (!is(total, "try-error") && isTRUE(recursive)) {
    aa <- objSize(x@.xData, quick = quick, recursive = recursive, ...)

    simSlots <- grep("^\\.envir$|^\\.xData$", slotNames(x), value = TRUE, invert = TRUE)
    names(simSlots) <- simSlots
    otherParts <- objSize(lapply(simSlots, function(slotNam) slot(x, slotNam)), quick = quick, ...)

    # if (!quick)
    attr(total, "objSize") <- list(sim = attr(aa, "objSize"),
                                   other = attr(otherParts, "objSize"))
    # attr(total, "objSize") <- sum(unlist(attr(aa, "objSize")), unlist(attr(otherParts, "objSize")))
    # class(attr(total, "objSize")) <- "lobstr_bytes"

  } # else {
  #   total <- NA
  # }

  return(total)
}

#' Methods for `.wrap` and `.unwrap`
#'
#'
#' @return The same object as passed into the function, but dealt with so that it can be
#' saved to disk.
#'
#' @param conn A `DBIConnection` object, as returned by `dbConnect()`.
#' @param drv an object that inherits from `DBIDriver`, or an existing
#'            `DBIConnection` object (in order to clone an existing connection).
#' @inheritParams reproducible::clearCache
#' @inheritParams reproducible::.wrap
#' @importFrom reproducible .wrap
#' @include simList-class.R
#' @export
#' @rdname dealWithClass
.wrap.simList <- function(obj, cachePath, preDigest, drv = getOption("reproducible.drv", NULL),
                          conn = getOption("reproducible.conn", NULL),
                          verbose = getOption("reproducible.verbose"),
                          outputObjects = NULL, cacheId,
                          ...) {

  # Copy everything (including . and ._) that is NOT a main object -- objects are the potentially very large things
  modules <- TRUE
  if (!is.null(outputObjects)) {
    grepForMods <- "^.mods"
    isAModule <- grep(grepForMods, outputObjects)
    modules <- gsub(paste0("^.mods", "\\$"), "", outputObjects[isAModule])
  }
  objTmp <- Copy(obj, objects = 2, modules = modules, drv = drv, conn = conn, verbose = verbose)

  # Remove Par and mod active bindings --> these shouldn't be .wrap'd
  modulesInSim <- ls(objTmp[[dotMods]])
  for (mo in modulesInSim) {
    try(rm(list = c("Par", "mod"), envir = objTmp[[dotMods]][[mo]]))
  }

  # .wrap the metadata ... i.e,. @depends
  objTmp <- .wrapOrUnwrapSimListAts(objTmp, .wrap)

  # Need to wrap the objects in e.g., .mods for e.g., mod objects that might be e.g., SpatVector
  objTmp[[dotMods]] <- .wrap(objTmp[[dotMods]], cachePath = cachePath, drv = drv, conn = conn, verbose = verbose)
  objTmp[[dotObjs]] <- .wrap(objTmp[[dotObjs]], cachePath = cachePath, drv = drv, conn = conn, verbose = verbose)
  # Deal with the potentially large things -- convert to list -- not a copy
  obj2 <- as.list(obj, all.names = FALSE) # don't copy the . or ._ objects, already done
  # Now the individual objects
  out <- .wrap(obj2, cachePath = cachePath, outputObjects = outputObjects, cacheId = cacheId,
               drv = drv, conn = conn, verbose = verbose, ...)

  # for (objName in names(out)) obj[[objName]] <- NULL
  list2env(out, envir = envir(objTmp))
  objTmp
}

.wrapOrUnwrapSimListAts <- function(obj, wrapOrUnwrap = .wrap) {
  sns <- slotNames(obj)
  sns <- sns[!startsWith(sns, ".")]
  for (sn in sns) {
    slot(obj, sn) <- wrapOrUnwrap(slot(obj, sn))
  }
  obj
}

#' @export
#' @inheritParams reproducible::.unwrap
#' @importFrom reproducible .unwrap
#' @rdname dealWithClass
.wrap..simDeps <- function(obj, ...) {
  wrapAndUnwrapAtDepends(obj, .wrap)
}

#' @export
#' @inheritParams reproducible::.unwrap
#' @importFrom reproducible .unwrap
#' @rdname dealWithClass
.unwrap..simDeps <- function(obj, ...) {
  wrapAndUnwrapAtDepends(obj, .unwrap)
}

wrapAndUnwrapAtDepends <- function(obj, wrapOrUnwrap = .wrap) {
  sn <- "dependencies"
  deps <- slot(obj, sn)
  fn <- wrapOrUnwrap
  for (mod in names(deps)) {
    deps[[mod]] <- fn(deps[[mod]])
  }
  slot(obj, sn) <- deps
  obj
}

#' @export
#' @inheritParams reproducible::.unwrap
#' @importFrom reproducible .unwrap
#' @rdname dealWithClass
.wrap..moduleDeps <- function(obj, ...) {
  wrapAndUnwrapDotMmoduleDeps(obj, .wrap)
}

#' @export
#' @inheritParams reproducible::.unwrap
#' @importFrom reproducible .unwrap
#' @rdname dealWithClass
.unwrap..moduleDeps <- function(obj, ...) {
  wrapAndUnwrapDotMmoduleDeps(obj, .unwrap)
}

wrapAndUnwrapDotMmoduleDeps <- function(deps, wrapOrUnwrap = .wrap) {
  snsInner <- slotNames(deps)
  fn <- wrapOrUnwrap
  for (snInner in snsInner) {
    slot(deps, snInner) <- fn(slot(deps, snInner))
  }
  deps
}

#' @export
#' @inheritParams reproducible::.unwrap
#' @importFrom reproducible .unwrap
#' @rdname dealWithClass
.unwrap.simList <- function(
  obj,
  cachePath,
  cacheId,
  drv = getOption("reproducible.drv", NULL),
  conn = getOption("reproducible.conn", NULL),
  ...
) {
  ## the as.list doesn't get everything. But with a simList, this is OK; rest will stay
  obj[[dotMods]] <- .unwrap(obj[[dotMods]], cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn, ...)
  obj[[dotObjs]] <- .unwrap(obj[[dotObjs]], cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn, ...)
  objList <- as.list(obj, all.names = TRUE) # don't overwrite everything, just the ones in the list part

  outList <- .unwrap(objList, cachePath = cachePath, cacheId = cacheId, drv = drv, conn = conn, ...)
  list2env(outList, envir = envir(obj))

  # .unwrap the metadata ... i.e,. @depends
  obj <- .wrapOrUnwrapSimListAts(obj, wrapOrUnwrap = .unwrap)

  obj
}

#' Make `simList` correctly work with `memoise`
#'
#' Because of the environment slot, `simList` objects don't correctly
#' memoise a `simList`.
#' This method for `simList` converts the object to a `simList_` first.
#'
#' @inheritParams reproducible::makeMemoisable
#'
#' @return A `simList_` object or a `simList`, in the case of `unmakeMemoisable`.
#'
#' @export
#' @importFrom reproducible makeMemoisable
#' @include simList-class.R
#' @rdname makeMemoisable
#' @seealso [reproducible::makeMemoisable()]
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

#' Attach missing attributes from `x` to `y`
#'
#' This is an internal helper.
#'
#' @param x an object with attributes
#' @param y an object with attributes
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
    if (!identical(attr(y, att), attr(x, att))) {
      stop("attributes on the cache object are not correct - 3")
    }

    #attr(y, att) <- attr(x, att)
  }
  return(y)
}

if (!isGeneric("clearCache")) {
  setGeneric(
    "clearCache",
    function(
      x,
      userTags = character(),
      after = NULL,
      before = NULL,
      ask = getOption("reproducible.ask"),
      useCloud = FALSE,
      cloudFolderID = NULL,
      ...
    ) {
      standardGeneric("clearCache")
    }
  )
}

#' `clearCache` for `simList` objects
#'
#' This will take the `cachePath(object)` and pass
#'
#' @inheritParams .wrap.simList
#'
#' @inheritParams reproducible::clearCache
#'
#' @return A `data.table` object showing the subset of items in the cache, located at `cachePath`
#' of the `sim` object, if `sim` is provided, or located in `cachePath`.
#' For `clearCache` (invoked for its side effect of clearing objects matching `userTags`, or those
#' between `after` or `before`), the returned `data.table` shows the removed items (invisibly).
#'
#' @export
#' @importFrom reproducible clearCache
#' @importMethodsFrom reproducible clearCache
#' @rdname clearCache
setMethod(
  "clearCache",
  signature = "simList",
  definition = function(
    x,
    userTags,
    after = NULL,
    before = NULL,
    ask,
    useCloud = FALSE,
    cloudFolderID = getOption("reproducible.cloudFolderID", NULL),
    drv = getOption("reproducible.drv", NULL),
    conn = getOption("reproducible.conn", NULL),
    ...
  ) {
    x <- x@paths$cachePath
    clearCache(x = x, userTags = userTags, after = after, before = before,
               ask = ask, useCloud = useCloud,
               cloudFolderID = cloudFolderID,
               ...)
  })

if (!isGeneric("showCache")) {
  setGeneric("showCache", function(x, userTags = character(), after = NULL, before = NULL, ...) {
    standardGeneric("showCache")
  })
}

#' `showCache` for `simList` objects
#'
#' @export
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
  setGeneric("keepCache", function(x, userTags = character(), after = NULL, before = NULL, ...) {
    standardGeneric("keepCache")
  })
}

#' `keepCache` for `simList` objects
#'
#' @export
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


sortInner <- function(l, useNames = FALSE) {
    l <- Map(inner = l, function(inner) {
      if (is.list(inner) || is.character(inner))  {
        ord1 <- .orderDotsUnderscoreFirst(inner)
        if (!(identical(ord1, seq_len(length(inner)))))
          inner <- inner[ord1]
      } else {
        en <- environmentName(inner)
        ord1 <- .orderDotsUnderscoreFirst(en)
        if (!(identical(ord1, seq_along(en))))
          inner <- inner[ord1]
      }
      inner
    }
    )
  # }
  l
}


digestEnviros <- function(objs, .objects, envToLookIn, algo, quick, length, classOptions) {
  dotUnderscoreObjs <- objs[startsWith(objs, "._")]
  objs <- objs[!objs %in% c(dotUnderscoreObjs, modAndParAB)]
  objs <- objs[objs %in% .objects]
  if (length(objs) > 1) {
    objs <- sort(objs, method = "radix")
  }
  out <- if (length(objs) > 0) {
    a <- mget(objs, envir = envToLookIn)
    nonZero <- lengths(a) > 0
    commonObjs <- intersect(.objects, names(a[nonZero]))
    # nonZero <- unlist(lapply(a, function(x) length(x) > 0))
    .robustDigest(a[nonZero], algo = algo, .objects = commonObjs,
                  quick = !isFALSE(quick), # can be character or TRUE --> TRUE
                  length = length, classOptions = classOptions) # need classOptions
  } else {
    list()
  }
  out
}


rmLength0Recursive <- function(envirHash, recurse = 1) {
  if (is.list(envirHash) && recurse > 0) {
    out <- lapply(envirHash, rmLength0Recursive, recurse = 0)
    out <- out[lengths(out) > 0]
  } else {
    lens <- lengths(envirHash) > 0
    # lens <- unlist(lapply(envirHash, function(x) length(x) > 0))
    out <- envirHash[lens]
  }
  out
}


# upgradeModsToXdata <- function(envirHash, moduleFunctionEnvir, recurse = 1)
upgradeModsToXdata <- function(envirHash, upgradeModsToXdata, moduleFunctionEnvir, recurse = 1) {
  if (is.list(envirHash) && recurse > 0) {
    eh <- lapply(envirHash, upgradeModsToXdata, moduleFunctionEnvir = moduleFunctionEnvir, recurse = 0)
    if (length(eh)) {
      en2Vals <- unlist(lapply(eh, names))
      en2Nams <- lapply(eh, names)
      en2Nams <- rep(names(en2Nams), lengths(eh))

      #if (!identical(.moduleFunctionsNam, unique(en2Nams))) {
      envirHashNew <- mapply(USE.NAMES = FALSE, nam = en2Nams, val = en2Vals, function(nam, val) {
        # dotVal <- paste0(".", val)
        dotVal <- val
        if (identical(nam, .moduleObjectsNam)) {
          list(list(envirHash[[nam]][[val]]) |> setNames(.objectsSlot)) |> setNames(dotVal)
        } else if (identical(nam, .moduleFunctionsNam)) {
          list(list(envirHash[[nam]][[val]]) |> setNames(.moduleFunctionsNam)) |> setNames(dotVal)
        } else {
          envirHash[[nam]][val]
        }
      })
      nehn <- names(envirHashNew)
      dups <- duplicated(nehn)
      if (any(dups)) {
        # Need to merge them into one list element per module
        uniqNams <- unique(nehn)
        envirHashNew2 <- list()
        for (un in uniqNams) {
          wh <- which(un == nehn)
          envirHashNew2 <- append(envirHashNew2, list(Reduce(modifyList, envirHashNew[wh])) |> setNames(un))
        }
        envirHashNew <- envirHashNew2
        # wh <- which(nehn %in% nehn[dups])
        # val <- nehn[dups]
        # envirHashNew <- unlist(envirHashNew[wh], recursive = FALSE)
        # names(envirHashNew) <- gsub(paste0(val, "."), "", x = names(envirHashNew))
        # envirHashNew <- list(envirHashNew) |> setNames(val)
      }
      envirHash$.xData <- append(envirHash$.xData, envirHashNew)
      if (any(names(en2Nams) %in% names(envirHash)))
        envirHash[names(en2Nams)] <- NULL

      # They have been promoted
      if (!is.null(envirHash[[.moduleFunctionsNam]]))
        envirHash[[.moduleFunctionsNam]] <- NULL
      if (!is.null(envirHash[[.moduleObjectsNam]]))
        envirHash[[.moduleObjectsNam]] <- NULL

      eh <- envirHash

      #}
    }
  } else {
    eh <- envirHash[names(envirHash)[names(envirHash) %in% names(moduleFunctionEnvir)]]
  }
  eh
}




copyModuleSpecificFunsAndObjs <- function(simPreWhSimList, simPost, currModule) {
  for (dotType in dotObjsAndMods) { # need functions
    currMods <- simPreWhSimList@.xData[[dotType]][[currModule]]
    objsInModuleActive <- ls(currMods, all.names = TRUE)
    dontCopyObjs <- c(.objectsSlot, "mod", "Par") # take these from the Cached copy (made 3 lines above)
    objsInModuleActive <- setdiff(objsInModuleActive, dontCopyObjs)
    if (length(objsInModuleActive))
      list2env(mget(objsInModuleActive, envir = simPreWhSimList@.xData[[dotType]][[currModule]]),
               envir = simPost@.xData[[dotType]][[currModule]])
  }
}


rmDotUnderscoresInModules <- function(obj, names) {
  objNames <- names(obj$.list[[1]])
  dotUnderscoreObjs <- objNames[startsWith(objNames, "._")]
  obj$.list[[1]][dotUnderscoreObjs] <- NULL

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
  obj
}


buildDotObjectsList <- function(object, .objects) {
  # objects may be provided in a namespaced format: modName:objName --
  # e.g., coming from .parseModule
  objectsMods <- grep("\\.mods\\$", .objects, value = TRUE)
  objectsMods <- gsub("\\.mods\\$", "", objectsMods)
  names(objectsMods) <- objectsMods
  objects1ByModWhole <- lapply(objectsMods, function(mod) ls(envir = object@.xData[[dotMods]][[mod]], all.names = TRUE))
  objects1ByModObjectWhole <- lapply(objectsMods, function(mod) ls(envir = object@.xData[[dotObjs]][[mod]], all.names = TRUE))

  .objects <- grep("\\.mods\\$", .objects, value = TRUE, invert = TRUE)
  objects1 <- strsplit(.objects, split = ":")
  lens <- unlist(lapply(objects1, length))
  objects1ByMod <- unlist(lapply(objects1[lens > 1], function(x) x[1])) # these are e.g., module::function
  mods <- unique(objects1ByMod)
  objects2 <- lapply(mods, function(mod) {
    unlist(lapply(objects1[lens > 1][objects1ByMod == mod], function(x) x[[2]]))
  })
  names(objects2) <- mods
  .objects <- append(list(".xData" = unlist(objects1[lens == 1])), objects2)

  # depending on how .objects came into this function, it may still have list(.xData = ..., moduleName = ...)
  mods <- unlist(unname(modules(object)))
  modNamesInDotObjs <- intersect(mods, names(.objects))
  if (length(modNamesInDotObjs)) {
    .objects3 <- Map(dotType = dotObjsAndMods, function(dotType) {
      Map(modNam = modNamesInDotObjs, function(modNam) {
        intersect(.objects[[modNam]], names(object@.xData[[dotType]][[modNam]]))
      })
    })
    objects1ByModWhole <- .objects3[[dotMods]]
    objects1ByModObjectWhole <- .objects3[[dotObjs]]
    .objects <- .objects[".xData"] # they are now in objects1ByModWhole and objects1ByModObjectsWhole
  }

  # Now add correct names to create length 3 .objects
  if (length(objects1ByModWhole))
    .objects <- suppressWarnings(modifyList2(.objects, list(objects1ByModWhole) |> setNames(.moduleFunctionsNam)))
  if (length(objects1ByModObjectWhole))
    .objects <- suppressWarnings(append(.objects, list(objects1ByModObjectWhole) |> setNames(.moduleObjectsNam)))

  .objects
}

# changedFromCreatesOutputs <- function(hasCurrModule, deps, currModules, simFromCache) {
lsObjectsChanged <- function(lsObjectEnv, changedObjs, hasCurrModule,
                             currModules, deps) {
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
  changedOutputs <- createOutputs[createOutputs %in% names(changedObjs)]

  # Basically, inputs shouldn't be returned, except for .inputObjects ... but more
  #   generally, we will be only be returning those that are changed anyway, which
  #   should be none if the metadata are correct.
  expectsInputs <- if (length(hasCurrModule)) {
    deps[[hasCurrModule]]@inputObjects$objectName
  } else {
    aa <- lapply(deps, function(dep)
      dep@inputObjects$objectName)
    unique(unlist(aa))
  }
  changedInputs <- expectsInputs[expectsInputs %in% names(changedObjs)]

  dotObjects <- startsWith(lsObjectEnv, ".")
  dotObjectsChanged <- dotObjects %in% TRUE & lsObjectEnv %in% names(changedObjs)

  # lsObjectEnv --> this should only be objects that can be outputted, not expectsInputs
  lsObjectEnv[lsObjectEnv %in% changedOutputs | lsObjectEnv %in% changedInputs |
                dotObjectsChanged %in% TRUE]
}

# }

lsModObjectsChanged <- function(namesAllMods, changedObjs, hasDotObjs) {
  changedModEnvObjs <- character()
  if (hasDotObjs) {
    # privateObjectsInModules <- attr(simFromCache, ".Cache")$changed
    # objsWithChangeInners <- intersect(namesAllMods, names(privateObjectsInModules))
    # changedModEnvObjs <- privateObjectsInModules[objsWithChangeInners]
    objsWithChangeInners <- intersect(namesAllMods, names(changedObjs)) # this is the whole "module" level; not individual objs
    changedModEnvObjs <- changedObjs[objsWithChangeInners]
  }
  changedModEnvObjs
}


#' Convenience wrapper around `clearCache` for SpaDES events
#'
#' This will clear only the event- and module-level caching that is triggered
#' using a module parameter, `.useCache`.
#'
#' @inheritParams reproducible::clearCache
#' @param dryRun logical. If `FALSE`, the default, then the function will deleted
#'   entries in the Cache. If `TRUE`, the function will identify which events and .inputObjects
#'   will be deleted, without deleting them.
#' @export
#' @returns A list of individual `clearCache` outputs, one for each event that was
#'   cleared.
clearCacheEventsOnly <- function(ask = TRUE,
                                 x = getOption("reproducible.cachePath"), dryRun = FALSE,
                                 verbose = getOption("reproducible.verbose")) {
  sc <- showCache(x, verbose = verbose)
  grepDoEventOrDotInputObjects <- quote(grepl("function", tagKey) & (grepl(".inputObjects", tagValue) |
                                                                       grepl("doEvent", tagValue)))
  cacheIds <- unique(sc[eval(grepDoEventOrDotInputObjects)]$cacheId)
  if (isTRUE(dryRun))
    messageVerbose(verbose = verbose, "dryRun = TRUE, no clearing...")

  rr <- lapply(cacheIds, function(y) {
    df <- sc[cacheId == y & eval(grepDoEventOrDotInputObjects)]
    mess <- paste0(df$tagValue)
    if (isTRUE(dryRun))
      mess <- paste0("Would remove: ", mess)
    messageVerbose(verbose = verbose, mess)
    if (isFALSE(dryRun))
      clearCache(cacheId = y, ask = ask, verbose = verbose - 1)
  })
}

outputsRmDontNeedForCache <- function(nonDotList, whichOutputs) {
  # setdiff(nonDotList, "outputs")
  setdiff(nonDotList, whichOutputs)
}
