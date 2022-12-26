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
#' Please use [covr::report()] to view the coverage information.
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
#' @rdname moduleCoverage
#'
#' @examples
#' \dontrun{
#'   tmpdir <- file.path(tempdir(), "coverage")
#'   modulePath <- file.path(tmpdir, "Modules") %>% checkPath(create = TRUE)
#'   moduleName <- "forestAge" # sample module to test
#'   downloadModule(name = moduleName, path = modulePath) # download sample module
#'   testResults <- moduleCoverage(mod = moduleName, modulePath = modulePath)
#'   report(testResults$moduleCoverage)
#'   report(testResults$functionCoverage)
#'   unlink(tmpdir, recursive = TRUE)
#'   mc1 <- moduleCoverage("Biomass_core", modulePath = "..")
#' }
moduleCoverage <- function(mod, modulePath = "..") {
  stop("This is a stub that is not intended for use")
  if (requireNamespace("testthat")) {
    if (is.null(getOption("testthat.progress.max_fails"))) {
      options(testthat.progress.max_fails = Inf)
    }

    if (missing(mod))
      mod <- basename(getwd())


    # this is the trigger that causes 2 behaviours to occur
    #   inside `simInit` and `spades`
    opts <- options("spades.covr" = mod, "spades.covr2" = TRUE)

    on.exit(options(opts))
    .pkgEnv$._covr <- list()

    # Copy all functions new file in R subfolder
    tmpFile <- paste0("R/",mod,"_main.R")
    modFileNam <- file.path(modulePath, mod, paste0(mod, ".R"))
    b <- parse(file = modFileNam)
    defModLine <- grep("defineModule", b)
    tf <- tempfile(fileext = ".R")
    .file.move(modFileNam, tf)
    on.exit(.file.move(tf, modFileNam, overwrite = TRUE), add = TRUE)
    cat(do.call(c, lapply(b[-defModLine], function(x) format(x))),
        file = tmpFile, sep = "\n")
    cat(do.call(c, lapply(b[defModLine], function(x) format(x))),
        file = modFileNam, sep = "\n")
    on.exit(unlink(tmpFile), add = TRUE)

    test_files <- dir(file.path(modulePath, mod, "tests", "testthat"), full.names = TRUE)


    # run test files

    ################
    options(opts)
    test_files <- dir(file.path("tests", "testthat"), full.names = TRUE)
    bb <- covr::file_coverage(source_files = checkPath(dir("R", full.names = TRUE, pattern = "\\.R")),
                              test_files = grep("Ward", test_files, value = TRUE) )
    #################


    ignore <- lapply(test_files, source)

    covr <- do.call(c, .pkgEnv$._covr)
    class(covr) <-  "coverage"

    options(opts)

    # Now do tests all 2nd time, but this time testing unique function calls without `spades` or `simInit`
    # test_files <- dir(file.path(modulePath, mod, "tests"), pattern = ".R$", full.names = TRUE)
    bb <- covr::file_coverage(source_files = checkPath(dir("R", full.names = TRUE)),
                        test_files = test_files )

    # Need to update file names of the bb so that they are the same as the covr
    #bb2 <- bb
    #covr2 <- covr
    # names(bb) <- gsub(paste0(mod, ".R"), "tmpDeleteMeForCoverageOnly", names(bb))
    # bbChar <- sapply(bb[grep(basename(tmpFile), names(bb))], function(x) as.character(x$srcref))
    # covrChar <- sapply(covr[grep("Biomass_core", names(covr))], function(x) as.character(x$srcref))
    # mm <- match(unname(bbChar), unname(covrChar))
    # whNoNA <- which(!is.na(mm));
    # bb[whNoNA] <- covr[mm[whNoNA]]
    # names(bb)[whNoNA] <- names(covr[mm[whNoNA]])

    covr2 <- c(covr, bb)
    class(covr2) <-  "coverage"
    return(covr2)
  } else {
    stop("moduleCoverage doesn't work without testthat and covr; install.packages(c('testthat', 'covr'))")
  }
}

