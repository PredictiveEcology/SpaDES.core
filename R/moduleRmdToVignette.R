#' Add a module's documentation `.Rmd` to its package rendition as a vignette
#'
#' A module documents itself in `<module>.Rmd`, written from the [newModule()]
#' template. To publish it as an article of a \pkg{pkgdown} site built from the
#' package rendition that [convertToPackage()] makes, it has to move into
#' `vignettes/`, and three things in the template assume it does not:
#'
#' \itemize{
#'   \item **the module path.** The template's R code -- the `subtitle` and
#'         authors line, and the `moduleInputs()`, `moduleParams()` and
#'         `moduleOutputs()` tables -- passes the module path as `".."`, relative
#'         to the `.Rmd`. From `vignettes/` that is `"../.."`. Only R code (code
#'         chunks and inline `` `r ` `` expressions) is rewritten: the template's
#'         prose also shows `".."`, as advice to the reader, and is left alone;
#'   \item **the output format.** The template renders with
#'         `bookdown::html_document2`, whose numbered table cross-references
#'         (`\@ref(tab:...)`) \pkg{pkgdown} would otherwise replace with its own
#'         format and leave unresolved. `pkgdown: as_is: true` is added to the YAML
#'         header, unless the header already has a `pkgdown` entry;
#'   \item **the `citations/` and `figures/` directories**, which the header's
#'         `bibliography` and the text refer to by relative path. They are copied
#'         into `vignettes/`.
#' }
#'
#' `\pkg{pkgdown}` builds from an *installed* package, so install the package
#' rendition before `pkgdown::build_site(install = FALSE)`.
#'
#' @param module Character string of module name, without path.
#' @param pkgPath Character string. The package rendition of the module, as
#'   returned by `convertToPackage(module, destinationPath = )`.
#'
#' @return Invisibly, the path of the vignette written,
#'   `file.path(pkgPath, "vignettes", paste0(module, ".Rmd"))`.
#'
#' @seealso [convertToPackage()]
#' @export
#' @examples
#' if (requireNamespace("ggplot2") && requireNamespace("pkgload")) {
#'   tmpdir <- tempdir2()
#'   newModule("myModule", tmpdir, open = FALSE)
#'   pkg <- convertToPackage("myModule", path = tmpdir, destinationPath = tempfile())
#'   moduleRmdToVignette("myModule", pkg)
#' }
#' \dontrun{
#' install.packages(pkg, repos = NULL, type = "source")
#' pkgdown::build_site(pkg, install = FALSE)
#' }
moduleRmdToVignette <- function(module, pkgPath) {
  rmd <- file.path(pkgPath, paste0(module, ".Rmd"))
  if (!file.exists(rmd))
    stop("moduleRmdToVignette(): ", rmd, " does not exist")
  lines <- readLines(rmd, warn = FALSE)

  delim <- which(lines == "---")
  if (length(delim) < 2 || any(nzchar(trimws(lines[seq_len(delim[1] - 1)]))))
    stop("moduleRmdToVignette(): ", rmd, " has no YAML header")

  repoint <- function(x) gsub("([\"'])\\.\\.\\1", "\\1../..\\1", x, perl = TRUE)
  inChunk <- FALSE
  for (i in seq_along(lines)) {
    if (!inChunk && grepl("^\\s*```+\\s*\\{r", lines[i])) {
      inChunk <- TRUE
    } else if (inChunk && grepl("^\\s*```+\\s*$", lines[i])) {
      inChunk <- FALSE
    } else if (inChunk) {
      lines[i] <- repoint(lines[i])
    } else {
      inline <- gregexpr("`r [^`]*`", lines[i])
      regmatches(lines[i], inline) <- lapply(regmatches(lines[i], inline), repoint)
    }
  }

  header <- if (delim[2] > delim[1] + 1) seq(delim[1] + 1, delim[2] - 1) else integer(0)
  if (!any(grepl("^pkgdown\\s*:", lines[header])))
    lines <- append(lines, c("pkgdown:", "  as_is: true"), after = delim[2] - 1)

  vignettes <- file.path(pkgPath, "vignettes")
  dir.create(vignettes, showWarnings = FALSE)
  for (d in c("citations", "figures"))
    if (dir.exists(file.path(pkgPath, d)))
      file.copy(file.path(pkgPath, d), vignettes, recursive = TRUE)

  out <- file.path(vignettes, paste0(module, ".Rmd"))
  writeLines(lines, out)
  invisible(out)
}
