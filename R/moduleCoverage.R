################################################################################
#' Calculate module coverage of unit tests
#'
#' Calculate the test coverage by unit tests for the module and its functions.
#'
#' @param mod  Character string. The module's name. Default is `basename(getwd())`
#'
#' @param modulePath  Character string. The path to the module directory
#'              (default is "..", i.e., one level up from working directory).
#'
#' @return Return a list of two coverage objects and two data.table objects.
#' The two coverage objects are named `moduleCoverage` and `functionCoverage`.
#' The `moduleCoverage` object contains the percent value of unit test coverage
#' for the module.
#' The `functionCoverage` object contains percentage values for unit test
#' coverage for each function defined in the module.
#' Please use `covr::report()` to view the coverage information.
#' Two data.tables give the information of all the tested and untested functions
#' in the module.
#'
#' @note When running this function, the test files must be strictly placed in
#' the \file{tests/testthat/} directory under module path.
#' To automatically generate this folder, please set `unitTests = TRUE`
#' when creating a new module using [newModule()].
#' To accurately test your module, the test filename must follow the format
#' `test-functionName.R`.
#'
#' @seealso [newModule()].
#'
#' @author Yong Luo
#' @importFrom data.table data.table
#' @importFrom reproducible .file.move
#' @include simList-class.R
#' @param ... Passed to [covr::package_coverage()].
#'
#' @rdname moduleCoverage
#' @export
moduleCoverage <- function(mod, modulePath = "..", ...) {
  if (!requireNamespace("covr", quietly = TRUE) || !requireNamespace("testthat", quietly = TRUE))
    stop("moduleCoverage needs testthat and covr; install.packages(c('testthat', 'covr'))")

  if (missing(mod))
    mod <- basename(getwd())

  # Build the package rendition somewhere disposable: convertToPackage() is not
  # reversible, and measuring coverage must not rewrite the user's module.
  pkg <- convertToPackage(mod, path = modulePath, buildDocuments = TRUE,
                          destinationPath = tempfile("moduleCoverage"))
  on.exit(unlink(dirname(pkg), recursive = TRUE), add = TRUE)

  cov <- covr::package_coverage(pkg, ...)

  # Coverage lands on R/READONLYFromMainModuleFile.R, a generated file that exists
  # in no repository -- so, unmapped, a coverage report names a file nobody can
  # open and shows nothing for the module file people actually edit.
  .remapModuleCoverage(cov,
                       generatedFile = file.path(pkg, "R", "READONLYFromMainModuleFile.R"),
                       moduleFile = file.path(modulePath, mod, paste0(mod, ".R")))
}

#' Map coverage on the generated module source back onto the module file
#'
#' [convertToPackage()] copies the module's functions into
#' `R/READONLYFromMainModuleFile.R`, which is a header followed by the module
#' file with the `defineModule()` block removed. Coverage collected on that file
#' therefore has to be shifted twice to name a real location: once for the
#' header, and once for the removed block.
#'
#' @param cov A `coverage` object from \pkg{covr}.
#' @param generatedFile Path to `R/READONLYFromMainModuleFile.R`.
#' @param moduleFile Path to the module's `<module>.R`.
#'
#' @return `cov`, with traces from `generatedFile` renamed and renumbered to
#'   `moduleFile`. Traces from other files are returned untouched.
#' @keywords internal
#' @rdname remapModuleCoverage
.remapModuleCoverage <- function(cov, generatedFile, moduleFile) {
  if (!file.exists(generatedFile))
    return(cov)

  gen <- readLines(generatedFile, warn = FALSE)
  header <- grep("^#%", gen)
  header <- if (length(header)) seq_len(max(header[header <= length(gen)])) else integer(0)
  nHeader <- length(header)

  rm <- grep("^#% removedLines: ", gen[header], value = TRUE)
  if (length(rm) != 1L)
    return(cov) # nothing authoritative to map with; leave the coverage alone
  rmRange <- as.integer(strsplit(sub("^#% removedLines: ", "", rm), "-", fixed = TRUE)[[1L]])
  removedStart <- rmRange[[1L]]
  removedN <- rmRange[[2L]] - rmRange[[1L]] + 1L

  # generated line -> line in the module file
  toModuleLine <- function(k) {
    j <- k - nHeader                      # index within the retained module lines
    ifelse(j < removedStart, j, j + removedN)
  }

  # Forward slashes throughout: `covr:::to_relative_path()` strips `root` followed
  # by a "/", so a backslashed Windows path never matches its own root and covr
  # reports the whole absolute path -- which is what would reach codecov.
  genNorm <- normalizePath(generatedFile, winslash = "/", mustWork = FALSE)
  modSrcFile <- srcfile(normalizePath(moduleFile, winslash = "/", mustWork = FALSE))

  isGen <- vapply(cov, function(x) {
    identical(normalizePath(attr(x[["srcref"]], "srcfile")[["filename"]],
                            winslash = "/", mustWork = FALSE), genNorm)
  }, logical(1))
  if (!any(isGen))
    return(cov)

  cov[isGen] <- lapply(cov[isGen], function(x) {
    sr <- as.integer(x[["srcref"]])
    # srcref layout: first_line, first_byte, last_line, last_byte,
    #                first_column, last_column, first_parsed, last_parsed
    sr[c(1L, 3L, 7L, 8L)] <- as.integer(toModuleLine(sr[c(1L, 3L, 7L, 8L)]))
    x[["srcref"]] <- structure(sr, srcfile = modSrcFile, class = "srcref")
    x
  })
  names(cov)[isGen] <- vapply(cov[isGen], function(x) {
    paste(c(basename(moduleFile), as.integer(x[["srcref"]])), collapse = ":")
  }, character(1))

  # covr reports every filename relative to `root` (`covr:::display_name()`, used
  # by `as.data.frame.coverage()` and so by `to_cobertura()` and `codecov()`), and
  # `to_cobertura()` takes its <sources> from `attr(cov, "package")$path`. Both are
  # still the throwaway directory the rendition was built in, so leaving them would
  # upload absolute temp paths that match nothing in the repository. For a SpaDES
  # module the repository root is the module directory itself, with <module>.R at
  # the top of it, which is what a coverage service needs to see.
  moduleRoot <- normalizePath(dirname(moduleFile), winslash = "/", mustWork = FALSE)
  attr(cov, "root") <- moduleRoot
  if (!is.null(attr(cov, "package")))
    attr(cov, "package")$path <- moduleRoot
  cov
}

