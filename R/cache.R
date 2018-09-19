if (!isGeneric(".robustDigest")) {
  setGeneric(
    ".robustDigest",
    function(object, objects, length = 1e6, algo = "xxhash64") {
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
#' @author Eliot McIntire
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
  definition = function(object, objects, length, algo,
                        quick, classOptions) {
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
                    quick = quick,
                    length = length)
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

    # don't cache contents of output because file may already exist
    object@outputs$file <- basename(object@outputs$file)
    if (NROW(object@inputs))
      object@inputs$file <- unlist(.robustDigest(object@inputs$file,
                                                 quick = quick,
                                                 length = length))
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
    obj$.list$.envir$._startClockTime <- NULL
    obj$.list$.envir$.timestamp <- NULL

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
#' @importFrom reproducible .grepSysCalls
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
  definition = function(object, functionName,
                        fromMemoise = getOption("reproducible.useMemoise", TRUE)) {
    cur <- current(object)
    if (NROW(cur)) {
      whichCached <- grep(".useCache", object@params)
      useCacheVals <- lapply(whichCached, function(x) {
        object@params[[x]]$.useCache
      })

      whCurrent <- match(cur$moduleName, names(object@params)[whichCached])
      fromWhere <- c("cached", "memoised")[fromMemoise + 1]
      if (isTRUE(useCacheVals[[whCurrent]])) {
        if (isTRUE(fromMemoise)) {
          cat(crayon::blue("  Loading memoised copy of", cur$moduleName, "module\n"))
        } else if (!is.na(fromMemoise)){
          cat(crayon::blue("  Using cached copy of", cur$moduleName, "module\n",
                           "adding to memoised copy"))
        } else {
          cat(crayon::blue("  Using ", fromWhere," copy of", cur$moduleName, "module\n"))
        }
      } else {
        if (isTRUE(fromMemoise)) {
          cat(crayon::blue("  Using memoised copy of", cur$eventType, "event in",
                           cur$moduleName, "module\n"))

        } else if (!is.na(fromMemoise)){
          cat(crayon::blue("  Using cached copy of", cur$eventType, "event in",
                           cur$moduleName, "module. Adding to memoised copy.\n"))
        } else {
          cat(crayon::blue("  Using ", fromWhere," copy of", cur$eventType, "event in",
                           cur$moduleName, "module\n"))
        }

      }
    } else {
      .cacheMessage(NULL, functionName, fromMemoise = fromMemoise)
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
#' @export
#' @exportMethod .checkCacheRepo
#' @importFrom reproducible .checkCacheRepo
#' @importFrom reproducible .grepSysCalls
#' @importMethodsFrom reproducible .checkCacheRepo
#' @include simList-class.R
#' @inheritParams reproducible::.checkCacheRepo
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
        cacheRepo <- .getOption("spades.cachePath")
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

##########################################
#' \code{.addChangedAttr} for simList class objects
#'
#' This will evaluate which elements in the simList object changed following
#' this Cached function call. It will add a named character string as an
#' attribute \code{attr(x, ".Cache")$changed}, indicating which ones changed.
#' When this function is subsequently called again, only these changed objects
#' will be returned. All other \code{simList} objects will remain unchanged.
#'
#' @seealso \code{\link[reproducible]{.addChangedAttr}}.
#'
#' @importFrom reproducible .addChangedAttr
#' @importMethodsFrom reproducible .addChangedAttr
#' @inheritParams reproducible::.addChangedAttr
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{.addChangedAttr}}
#' @exportMethod .addChangedAttr
#' @export
#' @rdname addChangedAttr
setMethod(
  ".addChangedAttr",
  signature = "simList",
  definition = function(object, preDigest, origArguments, ...) {
    dots <- list(...)
    whSimList <- which(unlist(lapply(origArguments, is, "simList")))[1]
    # remove the "newCache" attribute, which is irrelevant for digest
    if (!is.null(attr(object, ".Cache")$newCache)) attr(object, ".Cache")$newCache <- NULL
    postDigest <-
      .robustDigest(object, objects = dots$objects,
                    length = dots$length,
                    algo = dots$algo,
                    quick = dots$quick,
                    classOptions = dots$classOptions)
    isNewObj <- !names(postDigest$.list$.envir) %in% names(preDigest[[whSimList]]$.list$.envir)
    newObjs <- names(postDigest$.list$.envir)[isNewObj]
    existingObjs <- names(postDigest$.list$.envir)[!isNewObj]
    post <- lapply(postDigest$.list$.envir[existingObjs], fastdigest::fastdigest)
    pre <- lapply(preDigest[[whSimList]]$.list$.envir[existingObjs], fastdigest::fastdigest)
    changedObjs <- names(post[!(unlist(post) %in% unlist(pre))])
    attr(object, ".Cache")$changed <- c(newObjs, changedObjs)
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

##########################################
#' \code{.prepareOutput} for simList
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
    simListInput <- !isTRUE(is.na(whSimList))
    if (simListInput) {
      origEnv <- tmpl[[whSimList]]@.envir

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
        #   makes soft copy of all objects, i.e., they have the identical objects, which are pointers only
        object2 <- Copy(tmpl[[whSimList]], objects = FALSE)

        hasCurrModule <- currentModule(tmpl[[whSimList]])
        # Convert to numeric index, as some modules don't have names
        hasCurrModule <- match(hasCurrModule, modules(tmpl[[whSimList]]))

        createOutputs <- if (length(hasCurrModule)) {
          tmpl[[whSimList]]@depends@dependencies[[hasCurrModule]]@outputObjects$objectName
        } else {
          aa <- lapply(tmpl[[whSimList]]@depends@dependencies, function(dep)
            dep@outputObjects$objectName)
          unique(unlist(aa))
        }
        createOutputs <- na.omit(createOutputs)
        # take only the ones that the file changed, based on attr(object, ".Cache")$changed
        createOutputs <- createOutputs[createOutputs %in% attr(object, ".Cache")$changed]

        expectsInputs <- if (length(hasCurrModule)) {
          tmpl[[whSimList]]@depends@dependencies[[hasCurrModule]]@inputObjects$objectName
        } else {
          aa <- lapply(tmpl[[whSimList]]@depends@dependencies, function(dep)
            dep@inputObjects$objectName)
          unique(unlist(aa))
        }

        # Copy all objects from createOutputs only -- all others take from tmpl[[whSimList]]
        lsObjectEnv <- ls(object@.envir, all.names = TRUE)
        list2env(mget(lsObjectEnv[lsObjectEnv %in% createOutputs | lsObjectEnv %in% expectsInputs], envir = object@.envir), envir = object2@.envir)
        if (length(object2@current)==0) { # means it is not in a spades call
          # numCompleted <- if (length(object2@completed)) {
          #   length(unlist(object2@completed, recursive = FALSE))/(length(object2@completed[[1]]))
          # } else {
          #   0
          # }
          object2@completed <- object@completed
        }
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

        # This is for objects that are not in the return environment yet because they are unrelated to the
        #   current module -- these need to be copied over
        lsOrigEnv <- ls(origEnv, all.names = TRUE)
        keepFromOrig <- !(lsOrigEnv %in% ls(object2@.envir, all.names = TRUE))
        list2env(mget(lsOrigEnv[keepFromOrig], envir = origEnv), envir = object2@.envir)
      }
      if (!is.null(attr(object, "removedObjs"))) {
        if (length(attr(object, "removedObjs"))) {
          rm(list = attr(object, "removedObjs"), envir = object2@.envir)
        }
      }

      attrsToGrab <- setdiff(names(attributes(object)), names(attributes(object2)))
      for(atts in attrsToGrab) {
        attr(object2, atts) <- attr(object, atts)
      }

      # attr(object2, "tags") <- attr(object, "tags")
      # attr(object2, "call") <- attr(object, "call")
      # attr(object2, "function") <- attr(object, "function")
      #
      return(object2)
    } else {
      return(object)
    }
  })

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
#' This is recursive, so it will find the all simLists even if they are deeply nested.
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

if (!exists("objSize")) {
  objSize <- function(..., quick) UseMethod("objSize")
}


#' Object size for simLists
#'
#' Recursively, runs \code{object\.size} on the simList environment.
#' Currently, this will not assess object.size of the other elements
#'
#' @inheritParams reproducible::objSize
#' @importFrom reproducible objSize
#' @export
#' @examples
#' a <- simInit(objects = list(d = 1:10, b = 2:20))
#' objSize(a)
#' object.size(a)
objSize.simList <- function(x, quick = getOption("reproducible.quick", FALSE)) {
  xObjName <- deparse(substitute(x))
  aa <- objSize(x@.envir, quick = quick)
  bb <- as(x, "simList_")
  bb@.list <- list()
  bbOs <- list(simList = object.size(bb))
  aa <- append(aa, bbOs)
  return(aa)
}



#' Make simList correctly work with memoise
#'
#' Because of the environment slot, \code{simList} objects don't
#' correctly memoise a \code{simList}. This method for
#' \code{simList} converts the object to a \code{simList_} first.
#'
#' @return A \code{simList_} object or a \code{simList}, in the case
#' of \code{unmakeMemoiseable}.
#'
#' @importFrom reproducible makeMemoiseable
#' @inheritParams reproducible::makeMemoiseable
#' @rdname makeMemoiseable
#' @include simList-class.R
#' @seealso \code{\link[reproducible]{makeMemoiseable}}
#' @export
makeMemoiseable.simList <- function(x) {
  as(x, "simList_")
}

#' @importFrom reproducible unmakeMemoiseable
#' @inheritParams reproducible::unmakeMemoiseable
#' @export
#' @rdname makeMemoiseable
unmakeMemoiseable.simList_ <- function(x) {
  as(x, "simList")
}

#' Attach missing attributes from x to y
#'
#' This is an internal helper.
#'
#' @keywords internal
#' @param x an object with attributes
#' @param y an object with attributes
#' @rdname keepAttrs
.keepAttrs <- function(x, y, omitAttrs = c(".envir", ".list")) {
  keepAttrs <- setdiff(names(attributes(x)), names(attributes(y)))
  keepAttrs <- setdiff(keepAttrs, omitAttrs)
  for (att in keepAttrs)
    attr(y, att) <- attr(x, att)
  return(y)
}
