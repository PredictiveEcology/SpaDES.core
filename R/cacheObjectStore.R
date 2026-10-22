## Object store for event cache entries: options(spades.cacheObjectStore = TRUE) ------------------
##
## An event's (or `.inputObjects`') cache entry holds only the objects its event changed -- the ones
## a hit restores (.objectsToRestore()) -- and each of those not inline but as a reference to a file
## `<cachePath>/objects/<hash>.<qs2|rds>`. The hash is of the file's bytes (rlang::hash_file(),
## XXH3 128 bit), so two entries with an identical object share one file. The `.robustDigest()`
## that `changed` is computed from is not used: it rounds numbers (`reproducible.digestDigits`) and
## digests only file metadata for a file-backed object, so equal digests do not mean equal objects.
##
## A file-backed object (a `SpatRaster` on disk, ...) stays in the entry, as before: its file is
## saved beside the entry by reproducible, under the entry's cacheId, and goes with the entry.
##
## Loading an entry (.unwrap.simList()) binds a promise for each reference, so an object is read
## when something uses it. The promise's code is `.objStoreForce(<handle>)`; the handle (an
## environment) records the file and whether it was read. .copyBindings() moves an unread one to
## another environment as a promise on the same handle, so .prepareOutput() and a cacheChaining jump
## do not read it either; anything that takes the values (`mget()`, `as.list()`, a digest, `Copy()`,
## `saveSimList()`) reads it.
##
## The entry records each file it refers to in an `objectStore` tag. reproducible::clearCache() does
## not know about the files; clearCacheObjectStore() deletes the ones no entry refers to.

.objStoreDir <- "objects"
.objStoreTagKey <- "objectStore"
.objStoreRefClass <- "spadesObjStoreRef"
.objStoreHandleClass <- "spadesObjStoreHandle"

## Is the object store used for this save? Only for an event or `.inputObjects` entry (one with a
##   current event and the module's `outputObjects`); a whole simInit()/spades() entry stays complete.
.objStoreUse <- function(sim, outputObjects, cachePath) {
  isTRUE(getOption("spades.cacheObjectStore", FALSE)) && !is.null(outputObjects) &&
    NROW(sim@current) > 0 && !is.null(cachePath) && !is.null(attr(sim, ".Cache")$changed)
}

.objStoreExt <- function() {
  if (identical(getOption("reproducible.cacheSaveFormat"), "rds")) "rds" else "qs2"
}

## The objects `names` of `envir`, as an entry keeps them: each in the store, as a reference, but a
##   file-backed object or a NULL (kept as it is). An object a hit restored from this store and nothing
##   has read since is not read: its reference is reused.
.objStorePutNames <- function(names, envir, cachePath) {
  dir <- file.path(cachePath, .objStoreDir)
  ext <- .objStoreExt()
  objs <- setNames(vector("list", length(names)), names)
  for (nm in names) {
    h <- .objStoreHandle(nm, envir)
    if (.objStorePending(h) && identical(h$cachePath, cachePath) && file.exists(file.path(dir, h$key))) {
      .objStoreTouch(file.path(dir, h$key))
      objs[nm] <- list(.objStoreRef(h$key))
      next
    }
    obj <- get(nm, envir = envir, inherits = FALSE)
    objs[nm] <- list(if (is.null(obj) || any(nzchar(Filenames(obj, allowMultiple = TRUE)))) obj else
      .objStorePut(.wrap(obj, filebackedPath = cachePath), dir, ext))
  }
  objs
}

.objStoreRef <- function(key) structure(key, class = .objStoreRefClass)

## An entry about to be saved refers to `file`, which exists already: a new modification time, so
##   clearCacheObjectStore(minAge) does not delete it before the entry (and its tags) are saved.
.objStoreTouch <- function(file) Sys.setFileTime(file, Sys.time())

## Write one (wrapped) object, unless a file with the same bytes is there already. The file is written
##   under a temporary name in the same directory, then renamed: a reader never sees a partial file,
##   and two writers of the same object leave the same bytes.
.objStorePut <- function(obj, dir, ext) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(".tmp-", tmpdir = dir, fileext = paste0(".", ext))
  on.exit(unlink(tmp))
  .saveOneLazy(obj, tmp, ext)
  key <- paste0(rlang::hash_file(tmp), ".", ext)
  target <- file.path(dir, key)
  if (file.exists(target)) {
    .objStoreTouch(target)
  } else if (!file.rename(tmp, target) && !file.exists(target)) {
    stop("Could not write ", target)
  }
  .objStoreRef(key)
}

.objStoreNewHandle <- function(key, cachePath, name) {
  h <- new.env(parent = emptyenv())
  h$key <- as.character(key)
  h$name <- name
  h$cachePath <- cachePath
  h$loaded <- FALSE
  class(h) <- .objStoreHandleClass
  h
}

## The promise code: read the object once per handle, whichever binding is used first.
.objStoreForce <- function(h) {
  if (!isTRUE(h$loaded)) {
    f <- file.path(h$cachePath, .objStoreDir, h$key)
    if (!file.exists(f))
      stop("Cannot read `", h$name, "`: its cache object file ", f, " is missing (deleted?). ",
           "Clear the cache entries that restored it and run again.")
    h$value <- .unwrap(.readOneLazy(f), filebackedPath = h$cachePath)
    h$loaded <- TRUE
  }
  h$value
}

.objStoreDelay <- function(name, h, envir) {
  do.call(delayedAssign, list(name, call(".objStoreForce", h), eval.env = asNamespace("SpaDES.core"),
                              assign.env = envir))
}

## The handle of `name` in `envir` if it is bound to an object-store promise, else NULL. Neither
##   reads the object nor calls an active binding.
.objStoreHandle <- function(name, envir) {
  if (bindingIsActive(name, envir)) return(NULL)
  code <- do.call(substitute, list(as.name(name), envir))
  if (is.call(code) && length(code) == 2L && identical(code[[1]], quote(.objStoreForce)) &&
      inherits(code[[2]], .objStoreHandleClass))
    code[[2]]
}

## Is `h` (from .objStoreHandle()) an object not read yet?
.objStorePending <- function(h) !is.null(h) && !isTRUE(h$loaded)

## Is `name` in `envir` an object-store object not read yet? Such an object is neither NULL nor
##   file-backed (.objStorePutNames()), and was checked when its event first ran.
.objStoreIsPending <- function(name, envir) .objStorePending(.objStoreHandle(name, envir))

## In a loaded entry, replace each reference by a promise.
.objStoreAttach <- function(envir, cachePath) {
  for (nm in ls(envir, all.names = TRUE)) {
    if (!is.null(.objStoreHandle(nm, envir))) next # attached already (a memoised entry)
    if (bindingIsActive(nm, envir)) next
    val <- get(nm, envir = envir, inherits = FALSE)
    if (inherits(val, .objStoreRefClass)) .objStoreDelay(nm, .objStoreNewHandle(val, cachePath, nm), envir)
  }
}

## `list2env(mget(names, envir = from), envir = to)`, except that an object-store object not read
##   yet is moved as a promise, so it is still not read.
.copyBindings <- function(names, from, to) {
  if (!length(names)) return(invisible(to))
  handles <- lapply(names, .objStoreHandle, envir = from)
  pending <- vapply(handles, .objStorePending, logical(1))
  for (i in which(pending)) .objStoreDelay(names[i], handles[[i]], to)
  if (any(!pending)) list2env(mget(names[!pending], envir = from), envir = to)
  invisible(to)
}

#' `Filenames` for `simList` objects
#'
#' The files behind the file-backed objects in a `simList`, as [reproducible::Filenames()] gives
#' for an environment, without reading an object that a cache hit restored and nothing has used
#' yet (`options(spades.cacheObjectStore = TRUE)`; such an object is never file-backed).
#' The environment method takes `as.list()`, which reads them all, and reproducible calls
#' `Filenames()` on every memoised cache hit.
#'
#' @param obj A `simList`.
#' @inheritParams reproducible::Filenames
#' @return As [reproducible::Filenames()].
#' @exportMethod Filenames
#' @importFrom reproducible Filenames
#' @importMethodsFrom reproducible Filenames
#' @include simList-class.R
#' @rdname Filenames
setMethod("Filenames", signature = "simList", definition = function(obj, allowMultiple = TRUE,
                                                                     returnList = FALSE) {
  ## the other objects, in the order as.list(<the simList>) has them, so the result is ordered as
  ##   before: as.list() of an unhashed environment gives the bindings in reverse order of assignment
  nms <- ls(obj@.xData, sorted = FALSE)
  nms <- nms[!vapply(nms, .objStoreIsPending, logical(1), envir = obj@.xData)]
  objs <- new.env(hash = FALSE, parent = emptyenv())
  for (nm in rev(nms)) assign(nm, get(nm, envir = obj@.xData, inherits = FALSE), envir = objs)
  Filenames(objs, allowMultiple = allowMultiple, returnList = returnList)
})

## The `objectStore` tags of an entry: one per file it refers to.
.objStoreTags <- function(objs) {
  isRef <- vapply(objs, inherits, logical(1), .objStoreRefClass)
  if (any(isRef)) paste0(.objStoreTagKey, ":", unlist(objs[isRef], use.names = FALSE))
}

#' Delete the cache object files no cache entry refers to
#'
#' With `options(spades.cacheObjectStore = TRUE)`, an event's cache entry keeps the objects its
#' event changed in files under `file.path(x, "objects")`, shared by every entry with an identical
#' object. [reproducible::clearCache()] does not delete these files (yet): after it, run this to
#' delete the ones no remaining entry refers to (an entry's `objectStore` tags name its files), and
#' any temporary file a writer left behind.
#'
#' @param x A cache path.
#' @param minAge Seconds. A file modified more recently than this is kept even if no entry refers to
#'   it: another process may have just written it, for an entry it has not saved yet.
#' @param verbose Passed to [reproducible::showCache()] and used for the message.
#'
#' @return The files deleted, invisibly.
#' @export
#' @seealso [spadesOptions()]
clearCacheObjectStore <- function(x = getOption("reproducible.cachePath"), minAge = 3600,
                                  verbose = getOption("reproducible.verbose")) {
  dir <- file.path(x, .objStoreDir)
  ## all.files: a `.tmp-` file left by a writer that stopped is never referred to either
  files <- dir(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(files)) return(invisible(character()))
  sc <- showCache(x, verbose = verbose - 1)
  used <- sc$tagValue[sc$tagKey == .objStoreTagKey]
  age <- as.numeric(difftime(Sys.time(), file.mtime(files), units = "secs"))
  gone <- files[!basename(files) %in% used & age >= minAge]
  unlink(gone)
  messageVerbose("clearCacheObjectStore: deleted ", length(gone), " of ", length(files),
                 " object files", verbose = verbose)
  invisible(gone)
}
