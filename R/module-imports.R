#' Package imports for a module's function environment
#'
#' When `options(spades.reqdPkgsAttach = FALSE)`, a module's `reqdPkgs` are not
#' attached to the search path. Instead each module's function environment gets a
#' parent "imports" environment, between the module environment and the
#' `SpaDES.core` namespace, holding the exports of that module's `reqdPkgs`
#' (and of the packages they `Depends` on, as attaching would also have made
#' visible). It is built as R builds a package's imports: bindings, not copies,
#' so lazy-load promises are not forced. When two packages export the same name,
#' the one listed later in `reqdPkgs` wins, as it would when attaching.
#'
#' @param reqdPkgs Character vector of package specifications, in the order the
#'   module lists them (anything `Require::extractPkgName()` accepts).
#' @param module Module name, used only to name the environment.
#' @return An environment: the imports environment, or the `SpaDES.core`
#'   namespace when the option is `TRUE` or the module has nothing to import.
#'   Identical package vectors share one environment.
#' @keywords internal
#' @rdname moduleImportsEnv
.moduleImportsEnv <- function(reqdPkgs, module = "") {
  spadesNS <- asNamespace("SpaDES.core")
  if (isTRUE(getOption("spades.reqdPkgsAttach", TRUE))) return(spadesNS)

  pkgs <- .importPkgOrder(reqdPkgs)
  if (!length(pkgs)) return(spadesNS)
  nss <- lapply(pkgs, asNamespace)

  key <- paste(pkgs, collapse = ",")
  env <- .moduleImportsCache[[key]]
  ## a package reloaded in this session (e.g., pkgload) has a new namespace
  if (!is.null(env) && identical(attr(env, "namespaces"), nss)) return(env)

  env <- new.env(parent = spadesNS)
  for (ns in nss) {
    nms <- names(getNamespaceInfo(ns, "exports"))
    importIntoEnv(env, nms, ns, nms)
    .importLazyData(env, getNamespaceInfo(ns, "lazydata")) # found on the search path when attached
  }
  attr(env, "name") <- paste0("imports:", module)
  attr(env, "namespaces") <- nss
  assign(key, env, envir = .moduleImportsCache)
  env
}

## The packages to import, in increasing priority: each package's `Depends`
## (transitively) before it, then the package itself, first occurrence kept.
## Packages that are not installed, that `spades.reqdPkgsDontLoad` excludes, or
## that are attached by default, are dropped.
.importPkgOrder <- function(reqdPkgs) {
  if (inherits(reqdPkgs, "try-error") || !is.character(unlist(reqdPkgs))) return(character())
  pkgs <- unique(Require::extractPkgName(unlist(reqdPkgs)))
  pkgs <- setdiff(reqdPkgsDontLoad(pkgs), "SpaDES.core")
  out <- character()
  add <- function(p) {
    if (p %in% out || p %in% .defaultPkgs || !requireNamespace(p, quietly = TRUE)) return()
    for (d in .pkgDepends(p)) add(d)
    out <<- c(out, p)
  }
  for (p in pkgs) add(p)
  out
}

.defaultPkgs <- c("R", "base", "methods", "datasets", "utils", "grDevices", "graphics", "stats")

.pkgDepends <- function(pkg) {
  desc <- file.path(getNamespaceInfo(pkg, "path"), "DESCRIPTION")
  dep <- if (file.exists(desc)) read.dcf(desc, fields = "Depends")[[1]] else NA_character_
  if (is.na(dep)) return(character())
  Require::extractPkgName(trimws(strsplit(dep, ",")[[1]]))
}

## The reqdPkgs a module declares, in declared order, from a simList's metadata
.moduleReqdPkgs <- function(sim, module) {
  deps <- sim@depends@dependencies[[module]]
  if (is.null(deps)) character() else unlist(deps@reqdPkgs)
}

## Point an existing module environment at the imports for `reqdPkgs`
.setModuleImports <- function(modEnv, reqdPkgs, module) {
  imports <- .moduleImportsEnv(reqdPkgs, module)
  if (!identical(parent.env(modEnv), imports)) parent.env(modEnv) <- imports
  modEnv
}

## Bind a package's lazy data (datasets) into `env`, still unevaluated
.importLazyData <- function(env, lazy) {
  for (nm in names(lazy))
    local(delayedAssign(nm, get(nm, envir = lazy), assign.env = env),
          envir = list2env(list(nm = nm, lazy = lazy, env = env)))
}
