## ---- caching a whole simInit(), spades() or simInitAndSpades() call -------------------------
##
## `cache = TRUE` (or "local", or "portable") on any of the three, or `.useCache` with the same
## values in `...`. Wrapping the call in Cache() from outside is not enough: there `modules` are
## only folder names, so an edit to a module's code is not in the key and the stale result comes
## back. Here the key adds the content of every module code file (child modules and non-R files
## included). A module's `reqdPkgs` are in its code file, so a change to them reruns too.
##
## "local": the result is stored as any cached simList is; its input and output files stay where
##   they are, so the entry only works on this machine.
## "portable": the result is saved with saveSimList(files = TRUE), its inputs and outputs included,
##   to an archive in the cache folder; a cache hit unpacks it with loadSimList() into the working
##   directory (the project). Paths are not in the key, so the entry works on another machine.

._txtPortableRun <- "spades.portableRun"

## The mode asked for by `cache`, or by `.useCache` in `...`: FALSE, "local" or "portable"
.wholeRunCacheMode <- function(cache = NULL, .useCache = NULL) {
  norm <- function(x, arg) {
    if (is.null(x) || isFALSE(x)) return(FALSE)
    if (isTRUE(x)) return("local")
    if (is.character(x) && length(x) == 1L && x %in% c("local", "portable")) return(x)
    stop("`", arg, "` must be TRUE, FALSE, \"local\" or \"portable\"", call. = FALSE)
  }
  mode <- norm(cache, "cache")
  modeDots <- norm(.useCache, ".useCache")
  if (!is.null(cache) && !is.null(.useCache) && !identical(mode, modeDots))
    stop("`cache` and `.useCache` disagree; give only one", call. = FALSE)
  if (is.null(cache)) modeDots else mode
}

## Content of the modules' code files, named by path relative to the module folder's parent, so
## the key does not depend on where the project is
.moduleCodeDigest <- function(modulePath, modules) {
  modulePath <- path.expand(modulePath)
  files <- moduleCodeFiles(list(modulePath = modulePath), unlist(modules, use.names = FALSE),
                           childModules = TRUE, nonR = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  sums <- unname(tools::md5sum(files))
  rel <- files
  for (mp in modulePath) rel <- sub(paste0("^", mp, "/"), "", rel, fixed = FALSE)
  ord <- order(rel)
  setNames(sums[ord], rel[ord])
}

## Run `fnName` (simInit, spades or simInitAndSpades) with `args` through Cache(). `untracked`
## are arguments that do not change the result (debug, progress, notOlderThan). `paths`, `outputs`,
## `modules` and `params` enter the key as a simList's slots do (.simArgsForDigest()), so e.g. paths
## are not in it. `inputs` are: unlike in a simList, the objects they name are not loaded yet. `sim`
## is the simList of a spades() call: its digest already holds the module code, so none is added.
.cacheWholeRun <- function(fnName, args, mode, untracked = list()) {
  portable <- identical(mode, "portable")
  extra <- list(mode = mode)
  if (fnName != "spades") {
    paths <- args$paths
    if (is.null(paths) || is.null(paths$modulePath)) paths <- modifyList(as.list(getPaths()), as.list(paths))
    extra$code <- .moduleCodeDigest(paths$modulePath, args$modules)
  }
  simArgs <- intersect(names(args), c("paths", "outputs", "modules", "params"))
  extra$simArgs <- .simArgsForDigest(args[simArgs])
  untracked[simArgs] <- args[simArgs]
  args[simArgs] <- NULL
  sim <- Cache(.wholeRun(fnName = fnName, args = args, untracked = untracked, portable = portable),
               omitArgs = "untracked", .cacheExtra = extra, .functionName = fnName,
               notOlderThan = untracked$notOlderThan)
  attr(sim, ._txtPortableRun) <- NULL
  if (.loadedFromCache(sim)) .warnMissingOutputs(sim)
  sim
}

## What Cache() runs: the call itself; a portable result is marked so .wrap.simList() saves it
## with its files
.wholeRun <- function(fnName, args, untracked, portable) {
  sim <- doCallSafe(fnName, c(args, untracked))
  if (portable) attr(sim, ._txtPortableRun) <- TRUE
  sim
}

## A cached simList comes back without its output files when they were deleted after the run
.warnMissingOutputs <- function(sim) {
  os <- outputs(sim)
  fns <- if (NROW(os)) os$file[os$saved %in% TRUE] else character()
  missing <- fns[!file.exists(fns)]
  if (length(missing))
    warning("The cached simList is returned, but these output files it saved are no longer on disk:\n  ",
            paste(missing, collapse = "\n  "), call. = FALSE)
}

## The cache entry of a portable run: the archive saveSimList() wrote, in the cache's storage
## folder, named by the cache id (so clearCache() removes it with the entry). The tag lets the
## cloud cache find the file.
.wrapPortableRun <- function(obj, filebackedPath, cacheId, verbose) {
  attr(obj, ._txtPortableRun) <- NULL
  dir <- reproducible::CacheStorageDir(filebackedPath)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveSimList(obj, file.path(dir, paste0(cacheId, "_portable.qs2")), projectPath = getwd(),
              outputs = TRUE, inputs = TRUE, files = TRUE, verbose = verbose)
  archive <- dir(dir, pattern = paste0("^", cacheId, "_portable\\."))
  structure(list(archive = archive), class = "spadesPortableRun",
            tags = paste0("filenamesInCache:", archive))
}

#' @export
#' @rdname dealWithClass
.unwrap.spadesPortableRun <- function(obj, filebackedPath = getOption("reproducible.cachePath"),
                                      cacheId = NULL, ..., cachePath) {
  filebackedPath <- .filebackedPath(filebackedPath, missing(filebackedPath), cachePath, missing(cachePath))
  loadSimList(file.path(reproducible::CacheStorageDir(filebackedPath), obj$archive[1L]), projectPath = getwd())
}
