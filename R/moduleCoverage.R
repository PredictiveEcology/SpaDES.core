################################################################################
#' Calculate module coverage of unit tests
#'
#' Calculate the test coverage by unit tests for the module and its functions.
#'
#' @param name  Character string. The module's name.
#'
#' @param path  Character string. The path to the module directory
#'              (default is the current working directory).
#'
#' @return Return a list of two coverage objects and two data.table objects.
#' The two coverage objects are named `moduleCoverage` and `functionCoverage`.
#' The `moduleCoverage` object contains the percent value of unit test coverage
#' for the module.
#' The `functionCoverage` object contains percentage values for unit test
#' coverage for each function defined in the module.
#' Please use \code{\link[covr]{report}} to view the coverage information.
#' Two data.tables give the information of all the tested and untested functions
#' in the module.
#'
#' @note When running this function, the test files must be strictly placed in
#' the \file{tests/testthat/} directory under module path.
#' To automatically generate this folder, please set \code{unitTests = TRUE}
#' when creating a new module using \code{\link{newModule}}.
#' To accurately test your module, the test filename must follow the format
#' \code{test-functionName.R}.
#'
#' @seealso \code{\link{newModule}}.
#'
#' @author Yong Luo
#' @export
#' @importFrom data.table data.table
#' @include simList-class.R
#' @rdname moduleCoverage
#'
#' @examples
#' \dontrun{
#'   tmpdir <- file.path(tempdir(), "coverage")
#'   modulePath <- file.path(tmpdir, "Modules") %>% checkPath(create = TRUE)
#'   moduleName <- "forestAge" # sample module to test
#'   downloadModule(name = moduleName, path = modulePath) # download sample module
#'   testResults <- moduleCoverage(name = moduleName, path = modulePath)
#'   report(testResults$moduleCoverage)
#'   report(testResults$functionCoverage)
#'   unlink(tmpdir, recursive = TRUE)
#'   mc1 <- moduleCoverage("Biomass_core", modulePath = "..")
#' }
moduleCoverage <- function(mod, modulePath = "..") {
  if (requireNamespace("testthat")) {
    # require("testthat")
    tmpFile <- "R/tmpDeleteMeForCoverageOnly.R"
    modFileNam <- file.path(modulePath, mod, paste0(mod, ".R"))
    b <- parse(file = modFileNam)
    defModLine <- grep("defineModule", b)
    tf <- tempfile(fileext = ".R")
    file.move(modFileNam, tf)
    on.exit(file.move(tf, modFileNam, overwrite = TRUE), add = TRUE)
    cat(do.call(c, lapply(b[-defModLine], function(x) format(x))),
        file = tmpFile, sep = "\n")
    cat(do.call(c, lapply(b[defModLine], function(x) format(x))),
        file = modFileNam, sep = "\n")
    on.exit(unlink(tmpFile), add = TRUE)
    covr::file_coverage(source_files = dir("R", full.names = TRUE),
                  test_files = dir("tests/testthat", full.names = TRUE) )
  } else {
    stop("moduleCoverage doesn't work without testthat and covr; install.packages(c('testthat', 'covr'))")
  }
}
