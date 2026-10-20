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
##   * the event queue. The queue after a jump is the LIVE queue, never the one stored in the entry it
##     lands on -- an entry is shared by runs with different module sets and different queues. Each
##     entry carries the delta its own event made to the queue (the `eventQueueDelta` tag, written when
##     the event ran; see .chainQueueDelta()). The walk replays those deltas onto the live queue link
##     by link, and a link is only skipped if its event is the head of the queue as replayed so far. An
##     entry without a delta (saved before deltas were recorded) stops the walk.
## A jump never crosses between `.inputObjects` and the event queue, and, as before, a chain
## stops at the first uncached event.

## The queue delta of one event: what it added to the queue and what it removed, comparing the queue
## just before it ran with the queue just after. Multiset-correct: two identical events count twice.
## The first line is a format marker (so an event that changed nothing is still a recorded delta); the
## second is the event's own eventTime in seconds (the `eventTime` tag is in the simList's unit at the
## time, which a later run may not share). Then one line per event, `+` or `-`, eventTime (17
## significant digits, seconds, as queued), eventPriority, module, eventType, tab separated.
.chainEvKey <- function(e) {
  sprintf("%.17g\t%.17g\t%s\t%s", as.numeric(e[["eventTime"]]), as.numeric(e[["eventPriority"]]),
          e[["moduleName"]], e[["eventType"]])
}

.chainQueueKeys <- function(q) {
  k <- vapply(q, .chainEvKey, character(1))
  if (length(k)) paste0(k, "#", stats::ave(seq_along(k), k, FUN = seq_along)) else k
}

.chainQueueDelta <- function(pre, post, eventTime) {
  kPre <- .chainQueueKeys(pre)
  kPost <- .chainQueueKeys(post)
  added <- vapply(post[!kPost %in% kPre], .chainEvKey, character(1))
  removed <- vapply(pre[!kPre %in% kPost], .chainEvKey, character(1))
  paste(c(.chainDeltaMarker, sprintf("@\t%.17g", as.numeric(eventTime)), if (length(added)) paste0("+\t", added),
          if (length(removed)) paste0("-\t", removed)), collapse = "\n")
}

## The delta to record for the event that just went through Cache(): only when it actually ran (a new
## cache entry) and no jump was taken. NULL otherwise.
.chainDeltaIfRan <- function(sim, cacheIt, chaining, eventsPreCall, eventTime) {
  if (!isTRUE(cacheIt) || !is.null(chaining$jump)) return(NULL)
  if (!attr(sim, ".Cache")$newCache %in% TRUE) return(NULL)
  .chainQueueDelta(eventsPreCall, sim@events, eventTime)
}

## The same, for the outputs rows the event added (`.chainOutText()`).
.chainOutTextIfRan <- function(sim, cacheIt, chaining, outputsPreCall) {
  if (!isTRUE(cacheIt) || !is.null(chaining$jump)) return(NULL)
  if (!attr(sim, ".Cache")$newCache %in% TRUE) return(NULL)
  .chainOutText(outputsPreCall, sim@outputs)
}

.chainDeltaTag <- "eventQueueDelta"

## The object synonyms of an entry, stored with it (the `eventObjectSynonyms` tag) so a jump can give
## the live simList the synonyms a skipped event added without loading that entry. A marker line, then
## one line per synonym group (the names, tab separated); the marker alone is an event with none.
.chainSynTag <- "eventObjectSynonyms"
.chainSynMarker <- "objSyn1"

## How many outputs(sim) rows the event added (by the key a cache hit merges on, .mergeCachedOutputs()),
## stored as the `eventOutputsAdded` tag on every new entry. A hit restores only those rows
## (.chainOutAddedRows()); a jump loads only the skipped entries that added some (or have no tag): an
## outputs table can hold long `arguments` lists, so the rows themselves are not copied into a tag.
.chainOutTag <- "eventOutputsAdded"
.chainOutMarker <- "outAdd1"

.chainOutText <- function(pre, post) {
  n <- NROW(.mergeCachedOutputs(pre, post)) - NROW(pre)
  paste(.chainOutMarker, max(n, 0L), sep = "\t")
}

## How many outputs rows the entry's event added; NA if it has no record (saved before the tag existed).
.chainReadOutAddedN <- function(sc) {
  txt <- if (!is.null(sc)) sc$tagValue[sc$tagKey == .chainOutTag]
  if (!length(txt)) return(NA_real_)
  parts <- strsplit(txt[[length(txt)]], "\t", fixed = TRUE)[[1]]
  if (!identical(parts[1], .chainOutMarker)) return(NA_real_)
  as.numeric(parts[2])
}

## TRUE if the entry's event added outputs rows; NA if it has no record.
.chainReadOutAdded <- function(sc) .chainReadOutAddedN(sc) > 0

## The outputs(sim) rows an entry's event added, out of the whole table stored in the entry. That table
## is the saving run's: it also holds rows that run already had (an earlier module's Plots() row, a
## user's `outputs` argument), which are not a later run's to restore -- a row saved at the saving run's
## start made the save module schedule before start(sim) when a run with a later start hit the entry.
## Rows are appended (outputsAppend()), so the event's own are the last `n`, `n` from the tag. An entry
## without the tag gives all its rows, as every hit did before the tag existed.
.chainOutAddedRows <- function(cachedOutputs, sc) {
  n <- .chainReadOutAddedN(sc)
  if (is.na(n) || n >= NROW(cachedOutputs)) return(cachedOutputs)
  utils::tail(cachedOutputs, n)
}

.chainSynText <- function(sim) {
  syns <- envir(sim)[[objSynName]]
  paste(c(.chainSynMarker, vapply(syns, function(g) paste(unname(unlist(g)), collapse = "\t"), character(1))),
        collapse = "\n")
}

## The groups recorded in an entry's tags, as a list of character vectors; NULL if it has no record
## (an entry saved before the tag existed), which is not the same as an empty list.
.chainReadSyns <- function(sc) {
  txt <- sc$tagValue[sc$tagKey == .chainSynTag]
  if (!length(txt)) return(NULL)
  lines <- strsplit(txt[[length(txt)]], "\n", fixed = TRUE)[[1]]
  if (!identical(lines[1], .chainSynMarker)) return(NULL)
  strsplit(lines[-1L], "\t", fixed = TRUE)
}

## Give `sim` the synonym groups `groups` that it does not have yet, as a cache hit would
## (.prepareOutput()): a name that stands for another object is removed first.
.chainApplySyns <- function(sim, groups) {
  have <- lapply(envir(sim)[[objSynName]], function(g) unname(unlist(g)))
  groups <- unique(groups)
  groups <- groups[!vapply(groups, function(g) any(vapply(have, identical, logical(1), g)), logical(1))]
  if (!length(groups)) return(invisible(sim))
  nonCanonical <- unlist(lapply(groups, `[`, -1L))
  isPlain <- vapply(nonCanonical, function(o) exists(o, envir = sim@.xData, inherits = FALSE) &&
                      !bindingIsActive(o, sim@.xData), logical(1))
  if (any(isPlain)) rm(list = nonCanonical[isPlain], envir = sim@.xData)
  suppressMessages(objectSynonyms(synonyms = groups, envir = sim@.xData))
  invisible(sim)
}
.chainDeltaMarker <- "queueDelta1"

## The delta recorded in an entry's tags, as list(add, remove) of queue events; NULL if it has none.
.chainReadDelta <- function(sc) {
  txt <- sc$tagValue[sc$tagKey == .chainDeltaTag]
  if (!length(txt)) return(NULL)
  lines <- strsplit(txt[[length(txt)]], "\n", fixed = TRUE)[[1]]
  if (!identical(lines[1], .chainDeltaMarker)) return(NULL)
  toEvents <- function(l) lapply(strsplit(l, "\t", fixed = TRUE), function(x)
    list(eventTime = structure(as.numeric(x[2]), unit = "second"), moduleName = x[4],
         eventType = x[5], eventPriority = as.numeric(x[3])))
  own <- lines[startsWith(lines, "@")]
  list(time = if (length(own)) as.numeric(sub("^@\t", "", own[[1]])) else NA_real_,
       add = toEvents(lines[startsWith(lines, "+")]), remove = toEvents(lines[startsWith(lines, "-")]))
}

## The delta of an entry that has none recorded. An `.inputObjects` event schedules nothing and runs
## at start(sim), so its delta is what .chainQueueDelta() would have recorded: empty, with its time.
## Any other event could have scheduled anything: NULL.
.chainImpliedDelta <- function(sim, event) {
  if (!identical(event, ".inputObjects")) return(NULL)
  list(time = as.numeric(sim@simtimes[["start"]]), add = list(), remove = list())
}

## Replay a delta onto a queue: drop the events it removed (one each), then add the ones it added
## after the queued events at equal time and priority, as scheduleEvent() does.
.chainApplyDelta <- function(q, delta) {
  for (e in delta$remove) {
    i <- match(.chainEvKey(e), vapply(q, .chainEvKey, character(1)))
    if (!is.na(i)) q <- q[-i]
  }
  if (length(delta$add)) {
    q <- c(q, delta$add)
    q <- q[order(vapply(q, function(e) as.numeric(e[["eventTime"]]), numeric(1)),
                 vapply(q, function(e) as.numeric(e[["eventPriority"]]), numeric(1)))]
  }
  q
}

## Is `hit` the event at the head of queue `q`? `.inputObjects` events all sit at start(sim).
.chainIsHead <- function(q, hit) {
  if (!length(q)) return(FALSE)
  e <- q[[1]]
  if (!identical(unname(as.character(e[["moduleName"]])), hit$module) ||
      !identical(unname(as.character(e[["eventType"]])), hit$event)) return(FALSE)
  if (identical(hit$event, ".inputObjects")) return(TRUE)
  isTRUE(abs(as.numeric(e[["eventTime"]]) - hit$eventTime) <= 1e-9 * max(1, abs(hit$eventTime)))
}

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

## A module's declared `createsOutput`s. Used for the outputs(sim) rows a skipped entry may contribute
## (`.chainJumpFinish()`): rows named for the module's own outputs come from its own entry.
.chainCreates <- function(sim, module) {
  dep <- sim@depends@dependencies[[module]]
  if (is.null(dep)) return(character())
  unique(as.character(na.omit(dep@outputObjects$objectName)))
}

## What `entry`, a module's own cache entry, actually restores to the live simList: of the objects
## the module declares (`expectsInput`s and `createsOutput`s), those its event CHANGED
## (`attr(entry, ".Cache")$changed`). `.prepareOutput()` (R/cache.R, `lsObjectsChanged()`) carries no
## other object out of a loaded entry: not an `expectsInput` the event merely read, and not a
## `createsOutput` it declares but did not write (Biomass_core declares `speciesLayers`, which
## Biomass_borealDataPrep's `.inputObjects` makes). In an ordinary (non-jumped) run this is
## invisible -- the live simList already has such an object, supplied by whichever event produced
## it, running just before. A jump skips that producing event, so what each entry restores has to
## be known exactly: used to decide what a LATER step in a jump has already (re)written --
## `.chainJumpFinish()`'s `later` -- never for `.chainOutputs()`'s other job of tracking what the
## chain as a whole has produced. Deciding it from the declaration instead lost the object.
.chainRestored <- function(sim, entry, module) {
  dep <- sim@depends@dependencies[[module]]
  if (is.null(dep)) return(character())
  declared <- unique(as.character(na.omit(c(dep@inputObjects$objectName, dep@outputObjects$objectName))))
  intersect(declared, names(attr(entry, ".Cache")$changed))
}

## Rebuild, for any (module, event), the `nonObjects` that .runEvent() / .runModuleInputObjects()
## give cacheChainingSetup() -- without running anything. Must stay identical to those two
## call sites: a difference does not break anything, it only means the jump never engages.
.chainNonObjects <- function(sim, module, event) {
  fnEnv <- sim@.xData[[dotMods]][[module]]
  dep <- sim@depends@dependencies[[module]]
  if (is.null(fnEnv) || is.null(dep)) return(NULL)
  modParamsFull <- sim@params[[module]]
  modParams <- modParamsFull[!names(modParamsFull) %in% .paramsNotCachedOn(sim, module)]
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
  nonObjectsForCacheChaining(objs, fnEnv, classOptions, dep, extraCacheArgs = extraCacheArgs)
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

## An entry's tags. In the file backend a tag write rewrites the whole tag file under a lock, and the
## reader of showCacheFast() takes none, so it can see the file between one tag write and the next, or
## part-written: read under the writer's lock. (A DBI backend answers with one SELECT, already consistent.)
## NULL when they cannot be read, which means no jump.
.chainEntryTags <- function(cacheId, cachePath = getOption("reproducible.cachePath")) {
  read <- function() showCacheFast(cacheId = cacheId, cachePath = cachePath)
  sc <- try(if (useDBI()) read() else withLockFile(CacheTagLockFile(cachePath, cacheId), read()),
            silent = TRUE)
  if (is(sc, "try-error") || !NROW(sc)) NULL else sc
}

## A link (previous entry -> this event, recorded on the previous entry) is ONE tag, `eventChainLink_<postCacheId>`,
## so that one write, under the tag lock, records all of it. prevCache and postCacheId are the entry and
## the key. Its value is a marker and the other fields, tab separated, as for the tags above.
## It must not start with "cacheChaining": versions before this one read every such tag as a column of
## the old six-tag format and stop on a link that has fewer.
.chainLinkKeyPrefix <- "eventChainLink_"
.chainLinkMarker <- "chainLink1"
lastEventDetails <- "lastEventDetails"
.chainLinkFields <- c("digestNonObjects", "module", "event", lastEventDetails)
.chainLinkCols <- c("prevCache", .chainLinkFields, "postCacheId")

.chainLinkTagKey <- function(postCacheId) paste0(.chainLinkKeyPrefix, postCacheId)

.chainLinkText <- function(df)
  paste(c(.chainLinkMarker, vapply(.chainLinkFields, function(f) as.character(df[[f]]), character(1))),
        collapse = "\t")

## The one-tag links in `ccVals` (tags of one entry), as a data.frame of the columns of .chainLinkCols;
## a tag whose value is not a link is dropped.
.chainNewLinks <- function(ccVals) {
  ccVals <- ccVals[startsWith(ccVals$tagKey, .chainLinkKeyPrefix)]
  parts <- strsplit(ccVals$tagValue, "\t", fixed = TRUE)
  ok <- vapply(parts, function(x) length(x) == length(.chainLinkFields) + 1L &&
                 identical(x[[1]], .chainLinkMarker), logical(1))
  if (!any(ok)) return(NULL)
  parts <- parts[ok]; ccVals <- ccVals[ok]
  out <- as.data.frame(lapply(seq_along(.chainLinkFields), function(i)
    vapply(parts, `[[`, character(1), i + 1L)) |> setNames(.chainLinkFields))
  out$prevCache <- paste0("cacheId:", ccVals$cacheId)
  out$postCacheId <- substring(ccVals$tagKey, nchar(.chainLinkKeyPrefix) + 1L)
  out[, .chainLinkCols, drop = FALSE]
}

## The chains recorded on one entry: one row per postCacheId, columns .chainLinkCols. Flattening every
## tag into a single row would collapse them (duplicate names get mangled, rows pair positionally). The same
## link can also have been recorded more than once under one postCacheId -- by a version whose
## digest differs -- and only its newest recording counts: otherwise its repeated names are mangled
## the same way and the join sees the oldest, which then never matches again.
## Links recorded before they were one tag are six tags, `cacheChaining_<column>_<postCacheId>`, written
## one at a time; a group of them that lacks a column (read while being written, or left by an
## interrupted writer) is not a link and is dropped. NULL if there is no link.
.chainSuccessors <- function(sc) {
  if ("createdDate" %in% names(sc)) sc <- sc[order(sc$createdDate)]
  out <- rbindlist(list(.chainOldLinks(sc[startsWith(sc$tagKey, "cacheChaining")]),
                        .chainNewLinks(sc[startsWith(sc$tagKey, .chainLinkKeyPrefix)])), use.names = TRUE)
  ## newest last, so a one-tag link beats a six-tag one, and a later recording beats an earlier
  if (NROW(out)) out[!duplicated(out$postCacheId, fromLast = TRUE)] else NULL
}

.chainOldLinks <- function(ccVals) {
  if (!NROW(ccVals)) return(NULL)
  spli <- strsplit(ccVals$tagKey, "_")
  nams <- vapply(spli, function(x) x[[2]], character(1))
  postCacheIds <- vapply(spli, function(x) x[[3]], character(1))
  newest <- !duplicated(paste(postCacheIds, nams), fromLast = TRUE)
  ccVals <- ccVals[newest]; nams <- nams[newest]; postCacheIds <- postCacheIds[newest]
  groups <- split(seq_along(nams), postCacheIds)
  groups <- groups[vapply(groups, function(ix) all(.chainLinkCols %in% nams[ix]), logical(1))]
  if (!length(groups)) return(NULL)
  rbindlist(
    lapply(groups, function(ix)
      as.data.frame(as.list(ccVals$tagValue[ix]) |> setNames(nams[ix]))[, .chainLinkCols, drop = FALSE]),
    use.names = TRUE)
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
    ## the same call, with the same defaults, that Cache() makes per object of a simList
    now <- .robustDigest(val, length = getOption("reproducible.length", Inf), algo = "xxhash64",
                         quick = getOption("reproducible.quick", FALSE))
    if (is.list(now)) {
      ## Cache() records a list-valued object element by element (`sim..list.<object>.<element>`,
      ## an unnamed element as `sim..list.<object>`), never necessarily under the bare object name.
      ## Flatten the digest as reproducible does (unlist(), and metadata_define_preEval() drops the
      ## numbers unlist() adds), and compare as multisets: tag order is not guaranteed, names repeat.
      key <- function(nm, h) sort(paste0(sub("[[:digit:]]{1,5}$", "", nm), ":", h), method = "radix")
      rec <- recorded[names(recorded) == o | startsWith(names(recorded), paste0(o, "."))]
      if (!length(rec)) return(FALSE)
      now <- unlist(stats::setNames(list(now), o))
      if (!identical(key(names(now), now), key(names(rec), rec))) return(FALSE)
      next
    }
    if (!o %in% names(recorded)) return(FALSE)
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
  ## The live queue, as it will be once each event walked over has run: the event being recovered is
  ##   already off it; replay its delta, then each skipped event's.
  sc <- .chainEntryTags(cur, cachePath)
  delta <- if (is.null(sc)) NULL else .chainReadDelta(sc)
  if (is.null(delta) && !is.null(sc)) delta <- .chainImpliedDelta(sim, event)
  if (is.null(delta)) return(NULL)
  q <- .chainApplyDelta(sim@events, delta)
  repeat {
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
      ## only an event that is next in the live queue may be skipped, and only if its entry says what
      ##   it did to the queue; the time it ran at is the one it recorded, in seconds
      delta <- .chainReadDelta(postTags)
      if (is.null(delta)) delta <- .chainImpliedDelta(sim, row$event)
      if (is.null(delta) || is.na(delta$time)) next
      cHit <- list(cacheId = row$postCacheId, module = row$module, event = row$event,
                   eventTime = delta$time)
      if (!.chainIsHead(q, cHit)) next
      hit <- cHit
      break
    }
    if (is.null(hit)) break
    ## A skipped event never passes through doEvent(), where the barriers, the `events` whitelist
    ##   and end(sim) are tested. An event any of them would act on has to run as itself.
    if (.chainBlocked(controls, sim, hit)) break
    steps[[length(steps) + 1L]] <- hit
    q <- .chainApplyDelta(q[-1L], delta)
    produced <- union(produced, .chainOutputs(sim, hit$module, hit$event))
    seen <- c(seen, hit$cacheId)
    cur <- hit$cacheId
    sc <- postTags
    led <- paste(hit$module, hit$event)
  }
  if (length(steps)) {
    res <- rbindlist(steps)
    attr(res, "queue") <- q
    res
  } else {
    NULL
  }
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
  ## the landing entry's Cache() hit replays its URLs into the ledger with the context set here, so
  ## it must be the landing event's, not that of the event the jump started from
  .updateUrlLogExtra(sim)
  ## every event the jump restored, the one it lands on included; none of them runs again
  restored <- seq_len(n)
  messageCache("cacheChaining: restored ", n, " cached events in one step", verbose = verbose)
  messageCache(paste(sprintf("%s. %s %s  (%s)", formatC(restored, width = nchar(n)),
                              jump$module[restored], jump$event[restored], jump$cacheId[restored]),
                      collapse = "\n"), verbose = verbose)
  sim
}

## After it: the last entry only restores the objects its own event changed (`.chainRestored()`;
## `.prepareOutput()`'s restore is keyed on `attr(simFromCache, ".Cache")$changed`, so neither an
## `expectsInput` it merely read nor a `createsOutput` it declares but did not write is in it).
## Objects produced by the skipped events in between come from the last entry that produced each
## of them; objects a later step changed are never loaded from an earlier one. User-supplied objects
## a skipped `.inputObjects` would have placed in the simList are placed first, so an entry's copy
## wins where both exist.
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
  ## every skipped event's downloads, with that event's module + event (the landing entry, row
  ## n, was replayed by its own Cache() hit); see .replayUrlLog()
  for (i in seq_len(n - 1L))
    .replayUrlLog(sim, cachePath, jump$cacheId[i], jump$module[i], jump$event[i])
  ## the landing entry's own hit has just been merged: `sim` carries that entry's `.Cache` attribute
  later <- .chainRestored(sim, sim, jump$module[n])
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
        later <- union(later, .chainRestored(sim, simI, jump$module[i]))
      } else {
        messageCache("cacheChaining: could not load skipped entry ", jump$cacheId[i], " (",
                     jump$module[i], " ", jump$event[i], "); its outputs are not restored",
                     verbose = verbose)
      }
    }
    modsDone <- union(modsDone, jump$module[i])
  }
  ## The synonyms each skipped event added, in the order they ran. A hit restores its entry's synonyms
  ##   (.prepareOutput()); the landing entry may not carry a skipped event's (it can have been saved
  ##   after they were lost), so they are applied here, once the objects they name are in place. An
  ##   entry without the tag is loaded to read them.
  ##   The outputs(sim) rows each added are merged the same way, as a hit merges them: the rows its event
  ##   added (.chainOutAddedRows()) except those named for its own module's outputs. Only an entry that
  ##   added rows is loaded.
  for (i in seq_len(n - 1L)) {
    sc <- .chainEntryTags(jump$cacheId[i], cachePath)
    groups <- if (!is.null(sc)) .chainReadSyns(sc)
    addedOut <- if (!is.null(sc)) .chainReadOutAdded(sc) else NA
    simI <- NULL
    if (is.null(groups) || !identical(addedOut, FALSE)) {
      simI <- try(reproducible::loadFromCache(cachePath = cachePath, cacheId = jump$cacheId[i],
                                              verbose = -1), silent = TRUE)
      if (!is(simI, "simList")) simI <- NULL
    }
    if (is.null(groups) && !is.null(simI))
      groups <- lapply(simI@.xData[[objSynName]], function(g) unname(unlist(g)))
    if (length(groups)) .chainApplySyns(sim, groups)
    if (!is.null(simI) && !identical(addedOut, FALSE)) {
      own <- .chainCreates(sim, jump$module[i])
      added <- .chainOutAddedRows(simI@outputs, sc)
      slot(sim, "outputs", check = FALSE) <- .mergeCachedOutputs(
        sim@outputs, added[!added$objectName %in% own, ])
    }
  }
  attr(sim, "cacheChainingJump") <- NULL
  ## the live queue with every restored event taken off and what each scheduled put on (.chainWalk())
  slot(sim, "events", check = FALSE) <- attr(jump, "queue")
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
