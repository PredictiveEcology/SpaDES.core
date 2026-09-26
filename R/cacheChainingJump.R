## cacheChaining: skip ahead over a run of cached events ---------------------------------------
##
## cacheChainingSetup() finds, from the tags on the previous event's cache entry, the entry the
## current event can be recovered from without digesting the simList. The chain is recorded one
## link at a time (cacheChainingPost()), so it can also be FOLLOWED one link at a time: from that
## entry's own tags to the entry of the event that came after it, and so on. When every link
## checks out, the run lands directly on the last entry and loads each object once, from the
## last event that produced it, instead of loading and re-assembling the simList at every step.
##
## What is checked per skipped event (.chainWalk()):
##   * the module's code and parameters -- `digestNonObjects`, the same digest the chain already
##     keys on. Functions are still digested at every event.
##   * its expectsInputs that were NOT produced by an earlier event of the chain: their current
##     digest must equal the `preDigest` the entry recorded for them. Objects produced inside the
##     chain are fixed by the chain itself and are not re-digested -- that is the saving. A novel
##     object supplied at simInit to a later module therefore stops the jump at that module even
##     though everything before it chained.
##   * the entry's file still exists. A deleted entry means the event has to run.
## A jump never crosses between `.inputObjects` and the event queue, and, as before, a chain
## stops at the first uncached event.

## The `current` event list for an event that is not (yet) current.
.chainCur <- function(sim, module, event, eventTime = NA_real_) {
  ## in seconds, like the event queue -- not start(sim) / time(sim), which are in the simList's unit
  if (is.na(eventTime))
    eventTime <- if (identical(event, ".inputObjects")) sim@simtimes[["start"]] else sim@simtimes[["current"]]
  list(eventTime = eventTime, moduleName = module, eventType = event, eventPriority = .normal())
}

## What an event can have produced: its module's `createsOutput`s; for `.inputObjects` also the
## module's `expectsInput`s, which is what it exists to fill in (and what that Cache call lists
## as `outputObjects`).
.chainOutputs <- function(sim, module, event) {
  dep <- sim@depends@dependencies[[module]]
  if (is.null(dep)) return(character())
  out <- dep@outputObjects$objectName
  if (identical(event, ".inputObjects")) out <- c(dep@inputObjects$objectName, out)
  unique(as.character(na.omit(out)))
}

## Rebuild, for any (module, event), the `nonObjects` that .runEvent() / .runModuleInputObjects()
## give cacheChainingSetup() -- without running anything. Must stay identical to those two
## call sites: a difference does not break anything, it only means the jump never engages.
.chainNonObjects <- function(sim, module, event) {
  fnEnv <- sim@.xData[[dotMods]][[module]]
  dep <- sim@depends@dependencies[[module]]
  if (is.null(fnEnv) || is.null(dep)) return(NULL)
  modParamsFull <- sim@params[[module]]
  modParams <- modParamsFull[!names(modParamsFull) %in% paramsDontCacheOn]
  if (identical(event, ".inputObjects")) {
    if (!.isNamespaced(sim, module)) return(NULL) # the legacy (non-namespaced) digest is not mirrored
    objs <- paste(module, .fnsReachableFrom(".inputObjects", fnEnv), sep = ":")
    dependsSlots <- outputsRmDontNeedForCache(metadataToDigest, "outputObjects")
    classOptions <- classOptionsForCache(events = FALSE, modParams, dependsSlots, module)
  } else {
    notFns <- c(".inputObjects", "mod", "Par", ".objects")
    fns <- setdiff(ls(fnEnv, all.names = TRUE), notFns)
    objsInMod <- setdiff(ls(sim@.xData[[dotObjs]][[module]], all.names = TRUE), notFns)
    objs <- c(ls(sim@.xData, all.names = TRUE, pattern = module),
              paste0(attr(fnEnv, "name"), ":", fns),
              paste0(attr(fnEnv, "name"), ":", objsInMod),
              na.omit(dep@inputObjects$objectName))
    classOptions <- classOptionsForCache(events = event, paramsWoKnowns = modParams,
                                         dependsSlots = metadataToDigest, mBase = module)
  }
  extraCacheArgs <- sim@params[[module]][[._txtDotUseCacheArgs]][[event]]
  if (!is.list(extraCacheArgs)) extraCacheArgs <- list()
  isCalls <- vapply(extraCacheArgs, is.call, logical(1))
  if (any(isCalls)) {
    ## quoted entries (e.g. `quote(P(sim)$.useCloud)`) resolve against the module they belong to
    simTmp <- sim
    slot(simTmp, "current", check = FALSE) <- .chainCur(sim, module, event)
    env <- list2env(list(sim = simTmp, cur = simTmp@current), parent = environment())
    extraCacheArgs[isCalls] <- lapply(extraCacheArgs[isCalls], eval, envir = env)
  }
  nonObjectsForCacheChaining(objs, fnEnv, classOptions, extraCacheArgs = extraCacheArgs)
}

## The digest the chain keys on (`digestNonObjects`): module code, parameters, metadata slots
## and keyed `.useCacheArgs`. Two normalisations make it the same at record time and when the
## chain is walked, where the same values are rebuilt from tags:
##   * functions are digested as their deparsed text, not as closures -- a module function's
##     enclosing environment is the module environment, whose contents drift during a run;
##   * atomic elements lose their names -- `cur[["moduleName"]]` arrives as a named scalar, and
##     `setDT()` in cacheChainingSetup() strips those names by reference from the shared vector,
##     so the same element digested differently before and after `df` was built.
.chainDigest <- function(nonObjects) {
  nonObjects <- lapply(nonObjects, function(x) {
    if (is.function(x)) paste(deparse(x), collapse = "\n") else if (is.atomic(x)) unname(x) else x
  })
  reproducible::CacheDigest(nonObjects)$outputHash
}

.chainEntryTags <- function(cacheId, cachePath) {
  sc <- try(showCacheFast(cacheId = cacheId, cachePath = cachePath), silent = TRUE)
  if (is(sc, "try-error") || !NROW(sc)) NULL else sc
}

## The chains recorded on one entry: one row per postCacheId, columns prevCache,
## digestNonObjects, module, event, lastEventDetails, postCacheId. Flattening every tag into a
## single row would collapse them (duplicate names get mangled, rows pair positionally). The same
## link can also have been recorded more than once under one postCacheId -- by a version whose
## digest differs -- and only its newest recording counts: otherwise its repeated names are mangled
## the same way and the join sees the oldest, which then never matches again.
.chainSuccessors <- function(sc) {
  ccVals <- sc[startsWith(sc$tagKey, "cacheChaining")]
  if (!NROW(ccVals)) return(NULL)
  if ("createdDate" %in% names(ccVals)) ccVals <- ccVals[order(ccVals$createdDate)]
  spli <- strsplit(ccVals$tagKey, "_")
  nams <- vapply(spli, function(x) x[[2]], character(1))
  postCacheIds <- vapply(spli, function(x) x[[3]], character(1))
  newest <- !duplicated(paste(postCacheIds, nams), fromLast = TRUE)
  ccVals <- ccVals[newest]; nams <- nams[newest]; postCacheIds <- postCacheIds[newest]
  rbindlist(
    lapply(split(seq_along(nams), postCacheIds), function(ix)
      as.data.frame(as.list(ccVals$tagValue[ix]) |> setNames(nams[ix]))),
    use.names = TRUE, fill = TRUE)
}

## The expectsInputs of `module` that the chain did not produce must be what they were when the
## entry was written: compare their digest now with the entry's `preDigest` tags
## (`sim..list.<object>:<hash>`). An object not yet in the simList may still be waiting in the
## user-supplied `objects` of simInit; it is digested from there.
.chainExternalInputsMatch <- function(sim, module, postTags, produced, userObjects = NULL) {
  dep <- sim@depends@dependencies[[module]]
  ext <- setdiff(as.character(na.omit(dep@inputObjects$objectName)), produced)
  if (!length(ext)) return(TRUE)
  pre <- postTags$tagValue[postTags$tagKey == "preDigest"]
  pre <- pre[startsWith(pre, "sim..list.")]
  recorded <- sub("^[^:]+:", "", pre)
  names(recorded) <- sub("^sim\\.\\.list\\.", "", sub(":.*$", "", pre))
  for (o in ext) {
    val <- if (exists(o, envir = sim@.xData, inherits = FALSE)) {
      get(o, envir = sim@.xData, inherits = FALSE)
    } else {
      userObjects[[o]]
    }
    if (is.null(val)) {
      if (o %in% names(recorded)) return(FALSE)
      next
    }
    if (!o %in% names(recorded)) return(FALSE)
    ## the same call, with the same defaults, that Cache() makes per object of a simList
    now <- .robustDigest(val, length = getOption("reproducible.length", Inf), algo = "xxhash64",
                         quick = getOption("reproducible.quick", FALSE))
    if (!identical(unname(now), unname(recorded[[o]]))) return(FALSE)
  }
  TRUE
}

## Follow the chain forward from `cacheId`, the entry the current (module, event) is about to be
## recovered from. Returns the events that can be skipped, in order -- a data.table with
## cacheId, module, event, eventTime -- or NULL.
.chainWalk <- function(sim, cacheId, module, event, cachePath, produced = NULL, userObjects = NULL,
                       controls = list(), verbose = getOption("reproducible.verbose")) {
  phaseIO <- identical(event, ".inputObjects")
  produced <- union(produced, .chainOutputs(sim, module, event))
  led <- paste(module, event)
  steps <- list()
  seen <- cacheId
  cur <- cacheId
  ## a .stopAfter barrier on the event being recovered: nothing may be skipped past it
  if (.chainStopsAfter(controls, module, event)) return(NULL)
  repeat {
    sc <- .chainEntryTags(cur, cachePath)
    if (is.null(sc)) break
    cand <- .chainSuccessors(sc)
    if (is.null(cand) || !lastEventDetails %in% colnames(cand)) break
    ## plain-vector indexing: inside `[.data.table`, `lastEventDetails` would resolve to the column
    keep <- cand[[lastEventDetails]] == led & (cand[["event"]] == ".inputObjects") == phaseIO &
      !cand[["postCacheId"]] %in% seen
    cand <- cand[which(keep), ]
    hit <- NULL
    for (r in seq_len(NROW(cand))) {
      row <- as.list(cand[r])
      nonObjects <- .chainNonObjects(sim, row$module, row$event)
      if (is.null(nonObjects)) next
      if (!identical(.chainDigest(nonObjects), row$digestNonObjects)) next
      postTags <- .chainEntryTags(row$postCacheId, cachePath)
      if (is.null(postTags)) next
      if (!any(file.exists(reproducible::CacheStoredFile(cachePath, row$postCacheId)))) next
      if (!.chainExternalInputsMatch(sim, row$module, postTags, produced, userObjects)) next
      et <- postTags$tagValue[postTags$tagKey == "eventTime"]
      hit <- list(cacheId = row$postCacheId, module = row$module, event = row$event,
                  eventTime = .chainTagTime(et, sim))
      break
    }
    if (is.null(hit)) break
    ## A skipped event never passes through doEvent(), where the barriers, the `events` whitelist
    ##   and end(sim) are tested. An event any of them would act on has to run as itself.
    if (.chainBlocked(controls, sim, hit)) break
    steps[[length(steps) + 1L]] <- hit
    produced <- union(produced, .chainOutputs(sim, hit$module, hit$event))
    seen <- c(seen, hit$cacheId)
    cur <- hit$cacheId
    led <- paste(hit$module, hit$event)
  }
  if (length(steps)) rbindlist(steps) else NULL
}

## The eventTime tag on an event entry is written in the simList's time unit (`time(sim)`, in
## .runEvent()), whereas the queue, current(sim) and completed(sim) hold times in seconds.
.chainTagTime <- function(et, sim) {
  x <- if (length(et)) suppressWarnings(as.numeric(et[[1]])) else NA_real_
  if (is.na(x)) return(NA_real_)
  as.numeric(convertTimeunit(structure(x, unit = timeunit(sim)), "second", sim@.xData))
}

## `controls` is what doEvent() passes down: `events` (the whitelist) and `eventsBeforeAfter`
## (the barriers). .stopAfter on the event a jump starts from: doEvent() tests it on that event.
.chainStopsAfter <- function(controls, module, event) {
  ba <- controls$eventsBeforeAfter
  length(ba) > 0L && .matchesEventSpec(list(moduleName = module, eventType = event), ba$after)
}

## May `hit` be skipped? Not if a barrier names it (doEvent() tests .stopAfter on its own `cur`, the
## event the jump started from, so a barrier on a later event would never fire), not if the
## whitelist excludes it, and not if it is past end(sim) or its time is unknown.
.chainBlocked <- function(controls, sim, hit) {
  ev <- list(moduleName = hit$module, eventType = hit$event)
  ba <- controls$eventsBeforeAfter
  if (length(ba) && (.matchesEventSpec(ev, ba$before) || .matchesEventSpec(ev, ba$after))) return(TRUE)
  if (!is.null(controls$events) && isListedEvent(list(ev), controls$events) == 0L) return(TRUE)
  if (!identical(hit$event, ".inputObjects") &&
      (is.na(hit$eventTime) || hit$eventTime > sim@simtimes[["end"]])) return(TRUE)
  FALSE
}

## `jump` (cacheChainingSetup()): one row per event recovered by the jump, in order -- row 1 is
## the current event's own entry, the last row the entry the Cache() call is pointed at.
##
## Before that call: make the last event the current one, so .unwrap.simList() merges the entry
## as if that event had just run.
.chainJumpPrepare <- function(sim, jump, verbose = getOption("reproducible.verbose")) {
  n <- NROW(jump)
  attr(sim, "cacheChainingJump") <- jump
  slot(sim, "current", check = FALSE) <- .chainCur(sim, jump$module[n], jump$event[n], jump$eventTime[n])
  messageCache("Using cacheChaining ... skipping ahead over ", n - 1L, " cached event",
               if (n > 2L) "s", " to ", jump$module[n], " ", jump$event[n], verbose = verbose)
  skipped <- seq_len(n - 1L)
  messageCache(paste(sprintf("%s. %s %s  (%s)", formatC(skipped, width = nchar(n - 1L)),
                              jump$module[skipped], jump$event[skipped], jump$cacheId[skipped]),
                      collapse = "\n"), verbose = verbose)
  sim
}

## After it: the last entry only holds its own module's outputs. Objects produced by the skipped
## events in between come from the last entry that produced each of them; objects a later step
## overwrote are never loaded. User-supplied objects a skipped `.inputObjects` would have placed
## in the simList are placed first, so an entry's copy wins where both exist.
.chainJumpFinish <- function(sim, jump, cachePath, userObjects = NULL,
                             verbose = getOption("reproducible.verbose")) {
  n <- NROW(jump)
  if (length(userObjects)) {
    for (i in seq_len(n)) {
      if (!identical(jump$event[i], ".inputObjects")) next
      theirs <- intersect(names(userObjects), .chainOutputs(sim, jump$module[i], jump$event[i]))
      if (length(theirs)) list2env(userObjects[theirs], envir = sim@.xData)
    }
  }
  later <- .chainOutputs(sim, jump$module[n], jump$event[n])
  modsDone <- jump$module[n]
  for (i in rev(seq_len(n - 1L))) {
    want <- setdiff(.chainOutputs(sim, jump$module[i], jump$event[i]), later)
    needMod <- !jump$module[i] %in% modsDone
    if (length(want) || needMod) {
      simI <- try(reproducible::loadFromCache(cachePath = cachePath, cacheId = jump$cacheId[i],
                                              verbose = -1), silent = TRUE)
      if (is(simI, "simList")) {
        have <- intersect(want, ls(simI@.xData, all.names = TRUE))
        if (length(have))
          list2env(mget(have, envir = simI@.xData), envir = sim@.xData)
        if (needMod) {
          mo <- simI@.xData[[dotObjs]][[jump$module[i]]]
          moTo <- sim@.xData[[dotObjs]][[jump$module[i]]]
          if (is.environment(mo) && is.environment(moTo)) {
            objNames <- grep("^\\._", ls(mo, all.names = TRUE), value = TRUE, invert = TRUE)
            objNames <- setdiff(objNames, c("mod", "Par"))
            if (length(objNames)) list2env(mget(objNames, envir = mo), envir = moTo)
          }
        }
      } else {
        messageCache("cacheChaining: could not load skipped entry ", jump$cacheId[i], " (",
                     jump$module[i], " ", jump$event[i], "); its outputs are not restored",
                     verbose = verbose)
      }
    }
    later <- union(later, .chainOutputs(sim, jump$module[i], jump$event[i]))
    modsDone <- union(modsDone, jump$module[i])
  }
  attr(sim, "cacheChainingJump") <- NULL
  ## doEvent() records the current event itself; these are the ones it would not know about
  attr(sim, "cacheChainingJumped") <- jump[-1L, ]
  sim
}

## The event a cached step should be reported as, once a jump may have moved it.
.chainLast <- function(jump, module, event) {
  if (is.null(jump)) return(list(module = module, event = event))
  n <- NROW(jump)
  list(module = jump$module[n], event = jump$event[n])
}
