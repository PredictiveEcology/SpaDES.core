################################################################################
#' Open a file for editing
#'
#' RStudio's `file.edit` behaves differently than `utils::file.edit`.
#' The workaround is to have the user manually open the file if they are using RStudio.
#'
#' @param file  Character string giving the file path to open.
#'
#' @return  Invoked for its side effect of opening a file for editing.
#'
#' @author Alex Chubaty
#' @importFrom utils file.edit
#' @keywords internal
#' @rdname fileEdit
#'
.fileEdit <- function(file) {
  if (Sys.getenv("RSTUDIO") == "1") {
    file <- gsub(file, pattern = "\\./", replacement = "")
    file.show(file)
    message("Using RStudio; may need to open manually e.g., with file.edit or file.show")#,
#            paste0("file.edit('", file, "')")
#    )
  } else {
    file.edit(file)
  }
  message(paste0("file.edit('", file, "')"))
}

################################################################################
#' Create new module from template
#'
#' Generate a skeleton for a new SpaDES module, a template for a
#' documentation file, a citation file, a license file, a \file{README.md} file,
#' and a folder that contains unit tests information.
#' The `newModuleDocumentation` will not generate the module file, but will
#' create the other files.
#'
#' All files will be created within a subdirectory named `name` within the
#' `path`:
#'
#' \preformatted{
#'   <path>/
#'     |_ <name>/
#'     |_ R/               # contains additional module R scripts
#'     |_ data/            # directory for all included data
#'       |_ CHECKSUMS.txt  # contains checksums for data files
#'     |_ tests/           # contains unit tests for module code
#'     |_ citation.bib     # bibtex citation for the module
#'     |_ LICENSE          # describes module's legal usage
#'     |_ README.md        # provide overview of key aspects
#'     |_ <name>.R         # module code file (incl. metadata)
#'     |_ <name>.Rmd       # documentation, usage info, etc.
#' }
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param ...   Additional arguments. Currently, only the following are supported:\cr\cr
#' \describe{
#'   \item{`children`}{Required when `type = "parent"`. A character vector
#'   specifying the names of child modules.}
#'   \item{`open`}{Logical. Should the new module file be opened after creation?
#'   Default `TRUE`.}
#'   \item{`type`}{Character string specifying one of `"child"` (default),
#'   or `"parent"`.}
#'   \item{`unitTests`}{Logical. Should the new module include unit test files?
#'   Default `TRUE`. Unit testing relies on the \pkg{testthat} package.}
#'   \item{`useGitHub`}{Logical. Is module development happening on GitHub?
#'   Default `TRUE`.}
#' }
#'
#' @return Nothing is returned. The new module file is created at
#' \file{path/name.R}, as well as ancillary files for documentation, citation,
#' \file{LICENSE}, \file{README}, and \file{tests} directory.
#'
#' @note On Windows there is currently a bug in RStudio that prevents the editor
#' from opening when `file.edit` is called.
#' Similarly, in RStudio on macOS, there is an issue opening files where they
#' are opened in an overlaid window rather than a new tab.
#' `file.edit` does work if the user types it at the command prompt.
#' A message with the correct lines to copy and paste is provided.
#'
#' @author Alex Chubaty and Eliot McIntire
#' @export
#' @family module creation helpers
#' @rdname newModule
#'
#' @examples
#' \donttest{
#'   tmpdir <- tempdir2("exampleNewModule")
#'   ## create a "myModule" module in the "modules" subdirectory.
#'   newModule("myModule", tmpdir)
#'
#'   ## create a new parent module in the "modules" subdirectory.
#'   newModule("myParentModule", tmpdir, type = "parent", children = c("child1", "child2"))
#'   unlink(tmpdir, recursive = TRUE)
#' }
#'
setGeneric("newModule", function(name, path, ...) {
  standardGeneric("newModule")
})

#' @export
#' @rdname newModule
#' @importFrom reproducible checkPath
setMethod(
  "newModule",
  signature = c(name = "character", path = "character"),
  definition = function(name, path, ...) {
    args <- list(...)

    stopifnot((names(args) %in% c("children", "open", "type", "unitTests", "useGitHub")))

    children <- args$children
    open <- args$open
    type <- args$type
    unitTests <- args$unitTests
    useGitHub <- args$useGitHub

    # define defaults for ... args
    if (is.null(children)) children <- NA_character_
    if (is.null(open)) open <- interactive()
    if (is.null(type)) type <- "child"
    if (is.null(unitTests)) unitTests <- TRUE
    if (is.null(useGitHub)) useGitHub <- TRUE

    stopifnot(
      is(children, "character"),
      is(open, "logical") || any(endsWith(tolower(open), c("r", "rmd"))),
      is(type, "character"),
      type %in% c("child", "parent"),
      is(unitTests, "logical"),
      is(useGitHub, "logical")
    )

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    dataPath <- file.path(nestedPath, "data") %>% checkPath(create = TRUE)
    RPath <- file.path(nestedPath, "R") %>% checkPath(create = TRUE)

    ## empty data checksum file
    cat("", file = file.path(dataPath, "CHECKSUMS.txt"))

    if (isTRUE(useGitHub)) {
      ## basic .gitignore file for module data
      gitignoreTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "data-gitignore.template"))
      writeLines(whisker.render(gitignoreTemplate), file.path(dataPath, ".gitignore"))
    }

    ## module code file
    newModuleCode(name = name, path = path, open = isTRUE(open) || endsWith(tolower(open), "r"),
                  type = type, children = children)

    if (type == "child" && unitTests) {
      newModuleTests(name = name, path = path, open = !isFALSE(open), useGitHub = useGitHub)
    }

    ### Make R Markdown file for module documentation
    newModuleDocumentation(name = name, path = path,
                           open = isTRUE(open) || endsWith(tolower(open), "rmd"),
                           type = type, children = children)
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing"),
  definition = function(name, ...) {
    newModule(name = name, path = getOption("spades.modulePath"), ...)
})

################################################################################
#' Create new module code file
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default `TRUE` in an interactive session.
#'
#' @param type  Character string specifying one of `"child"` (default),
#'              or `"parent"`.
#'
#' @param children   Required when `type = "parent"`. A character vector
#'                   specifying the names of child modules.
#'
#' @return Nothing is returned. Invoked for its side effect of creating new module code files.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname newModuleCode
setGeneric("newModuleCode", function(name, path, open, type, children) {
  standardGeneric("newModuleCode")
})

#' @export
#' @family module creation helpers
#' @importFrom crayon bold green yellow
#' @importFrom reproducible checkPath
#' @importFrom whisker whisker.render
#' @rdname newModuleCode
# igraph exports %>% from magrittr
setMethod(
  "newModuleCode",
  signature = c(name = "character", path = "character", open = "logical",
                type = "character", children = "character"),
  definition = function(name, path, open, type, children) {
    stopifnot(type %in% c("child", "parent"))

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameR <- file.path(nestedPath, paste0(name, ".R"))

    children_char <- if (any(is.na(children)) || length(children) == 0L) {
      "character(0)"
    } else {
      capture.output(dput(children))
    }

    version <- list()
    version[[name]] <- moduleDefaults[["version"]]
    if (type == "parent")
      lapply(children, function(x) version[[x]] <<- "0.0.1")

    SpaDES.core.version <- as.character(utils::packageVersion("SpaDES.core"))
    DESCtxt <- readLines(system.file("DESCRIPTION", package = "SpaDES.core"))
    if (any(grepl("GithubRepo", DESCtxt))) {
      Grepo <- gsub(".+: ", "", grep("GithubRepo", DESCtxt, value = TRUE))
      Guser <- gsub(".+: ", "", grep("GithubUsername", DESCtxt, value = TRUE))
      Gref <- gsub(".+: ", "", grep("GithubRef", DESCtxt, value = TRUE))
      SpaDES.core.pkgName <- paste0(Guser, "/", Grepo, "@", Gref)
    } else {
      SpaDES.core.pkgName <- "SpaDES.core"
    }
    SpaDES.core.Fullname <- paste0(SpaDES.core.pkgName, " (>= ", SpaDES.core.version, ")")

    modulePartialMeta <- list(
      reqdPkgs = deparse1(append(list(SpaDES.core.Fullname), moduleDefaults[["reqdPkgs"]]),
                          collapse = "")
    )
    modulePartialMetaTemplate <- readLines(file.path(.pkgEnv[["templatePath"]],
                                                     "modulePartialMeta.R.template"))
    otherMetadata <- if (type == "child") {
      whisker.render(modulePartialMetaTemplate, modulePartialMeta)
    } else {
      paste("## this is a parent module and as such does not have any",
            "reqdPkgs, parameters, inputObjects, nor outputObjects.")
    }

    modulePartialEvents <- list(
      name = name,
      name_char = deparse(name)
    )
    moduleEventsTemplate <- readLines(file.path(.pkgEnv[["templatePath"]],
                                                "modulePartialEvents.R.template"))
    moduleEvents <- if (type == "child") {
      whisker.render(moduleEventsTemplate, modulePartialEvents)
    } else {
      "## this is a parent module and as such does not have any events."
    }

    moduleData <- list(
      authors = deparse(moduleDefaults[["authors"]], width.cutoff = 500),
      children = children_char,
      citation = deparse(moduleDefaults[["citation"]]),
      description = deparse(moduleDefaults[["description"]]),
      events = moduleEvents,
      keywords = deparse(moduleDefaults[["keywords"]]),
      name = deparse(name),
      otherMetadata = otherMetadata,
      RmdName = deparse(paste0(name, ".Rmd")),
      timeframe = deparse(moduleDefaults[["timeframe"]]),
      timeunit = deparse(moduleDefaults[["timeunit"]]),
      type = type,
      versions = deparse(version)
    )
    moduleTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "module.R.template"))
    writeLines(whisker.render(moduleTemplate, moduleData), filenameR)

    ## help the user with next steps
    message(crayon::bold(paste(
      crayon::green("New module"),
      crayon::yellow(name),
      crayon::green("created at"),
      crayon::yellow(dirname(nestedPath))
    )))
    message(crayon::green("* edit module code in", crayon::yellow(paste0(name, ".R"))))
    message(crayon::green("* write tests for your module code in", crayon::yellow("tests/")))
    message(crayon::green("* describe and document your module in", crayon::yellow(paste0(name, ".Rmd"))))
    message(crayon::green("* tell others how to cite your module by editing", crayon::yellow("citation.bib")))
    message(crayon::green("* choose a license for your module; see", crayon::yellow("LICENSE.md")))

    if (isTRUE(open)) {
      openModules(name, nestedPath)
    }
})

#' Create new module documentation
#'
#' @inheritParams newModuleCode
#'
#' @return Nothing is returned. Invoked for its side effect of creating new module code files.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @importFrom reproducible checkPath
#' @export
#' @family module creation helpers
#' @rdname newModuleDocumentation
#'
setGeneric("newModuleDocumentation", function(name, path, open, type, children) {
  standardGeneric("newModuleDocumentation")
})

#' @export
#' @rdname newModuleDocumentation
setMethod(
  "newModuleDocumentation",
  signature = c(name = "character", path = "character", open = "logical",
                type = "character", children = "character"),
  definition = function(name, path, open, type, children) {
    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) %>% checkPath(create = TRUE)
    filenameRmd <- file.path(nestedPath, paste0(name, ".Rmd"))
    filenameCitation <- file.path(nestedPath, "citation.bib")
    filenameLICENSE <- file.path(nestedPath, "LICENSE.md")
    filenameNEWS <- file.path(nestedPath, "NEWS.md")
    filenameREADME <- file.path(nestedPath, "README.md")

    moduleRmd <- list(
      author = Sys.getenv("USER"),
      date = format(Sys.Date(), "%d %B %Y"),
      name = name,
      name2 = gsub("_", "-", name),  ## for chunk names
      path = path
    )
    moduleRmdTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "module.Rmd.template"))
    writeLines(whisker.render(moduleRmdTemplate, moduleRmd), filenameRmd)

    ### Make citation.bib file
    moduleCite <- list(
      author = paste(paste(moduleDefaults[["authors"]]$given, collapse = " "),
                     moduleDefaults[["authors"]]$family), ## need to use `$` here
      name = name,
      year = format(Sys.Date(), "%Y")
    )
    moduleCiteTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "citation.bib.template"))
    writeLines(whisker.render(moduleCiteTemplate, moduleCite), filenameCitation)

    ### Make LICENSE file
    licenseTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "LICENSE.template"))
    writeLines(whisker.render(licenseTemplate), filenameLICENSE)

    ### Make NEWS file
    newsTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "NEWS.template"))
    writeLines(whisker.render(newsTemplate, moduleRmd), filenameNEWS)

    ### Make README file
    filenameMd <- paste0(tools::file_path_sans_ext(filenameRmd), ".md")
    success <- tryCatch({
      linkOrCopy(filenameMd, filenameREADME, verbose = FALSE)
    }, error = function(e) FALSE)
    if (isFALSE(success)) {
      ReadmeTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "README.template"))
      writeLines(whisker.render(ReadmeTemplate, moduleRmd), filenameREADME)
    }

    if (open) {
      openModules(basename(filenameRmd), nestedPath)
    }

    return(invisible(NULL))
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "logical"),
          definition = function(name, open) {
            newModuleDocumentation(name = name, path = ".", open = open)
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "character", open = "missing"),
          definition = function(name, path) {
            newModuleDocumentation(name = name, path = path, open = interactive())
})

#' @export
#' @rdname newModuleDocumentation
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing", open = "missing"),
          definition = function(name) {
            newModuleDocumentation(name = name, path = ".", open = interactive())
})

#' Use GitHub actions for automated module checking
#'
#' See corresponding vignette for more information.
#'
#' @param name module name
#' @param path module path
#'
#' @return Invoked for its side effect of creating new GitHub Actions workflow files.
#'
#' @export
#' @importFrom reproducible checkPath
#' @importFrom whisker whisker.render
use_gha <- function(name, path) {
  ghActionPath <- checkPath(file.path(path, name, ".github", "workflows"), create = TRUE)

  moduleRmdYaml <- list(name = name)
  renderModuleRmdYamlTemplate <- readLines(file.path(.pkgEnv[["templatePath"]],
                                                     "render-module-rmd.yaml.template"))
  writeLines(whisker.render(renderModuleRmdYamlTemplate, moduleRmdYaml),
             file.path(ghActionPath, "render-module-rmd.yaml"))
  write("*.html\n", file = file.path(path, name, ".github", ".gitignore"), append = TRUE)
}

#' Create template testing structures for new modules
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param open  Logical. Should the new module file be opened after creation?
#'              Default `TRUE` in an interactive session.
#'
#' @param useGitHub Logical indicating whether GitHub will be used.
#'                  If `TRUE` (default), creates suitable configuration files (e.g.,
#'                  \file{.gitignore}) and configures basic GitHub actions for module code checking.
#'
#' @return Nothing is returned. Invoked for its side effect of creating new module test files.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @importFrom reproducible checkPath
#' @export
#' @family module creation helpers
#' @rdname newModuleTests
#'
setGeneric("newModuleTests", function(name, path, open, useGitHub) {
  standardGeneric("newModuleTests")
})

#' @export
#' @rdname newModuleTests
setMethod(
  "newModuleTests",
  signature = c(name = "character", path = "character", open = "logical", useGitHub = "logical"),
  definition = function(name, path, open, useGitHub) {
    if (!requireNamespace("testthat", quietly = TRUE)) {
      warning('The `testthat` package is required to run unit tests on modules.')
    }
    path <- checkPath(path, create = TRUE)
    testthatDir <- file.path(path, name, "tests", "testthat") %>% checkPath(create = TRUE)
    testDir <- dirname(testthatDir)

    ## create basic local testing structure based on testthat
    ## -- see ?testthat
    unitTests <- list(name = name, path = path)
    unitTestsR <- file.path(testDir, "unitTests.R") # source this to run all tests
    testTemplate <- file.path(testthatDir, "test-template.R") # template for user-defined tests

    moduleUnitTestTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "unitTests.R.template"))
    writeLines(whisker.render(moduleUnitTestTemplate, unitTests), unitTestsR)

    moduleUnitTestTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "test-template.R.template"))
    writeLines(whisker.render(moduleUnitTestTemplate, unitTests), testTemplate)

    ## create basic testing infrastructure using GitHub Actions
    ## -- see ?usethis::use_github_action and https://github.com/r-lib/actions/tree/master/examples
    if (isTRUE(useGitHub)) {
      use_gha(name, path)
    }
})

#' Open all modules nested within a base directory
#'
#' This is just a convenience wrapper for opening several modules at once, recursively.
#' A module is defined as any file that ends in `.R` or `.r` and has a
#' directory name identical to its filename. Thus, this must be case sensitive.
#'
#' @param name  Character vector with names of modules to open. If missing, then
#'              all modules will be opened within the basedir.
#'
#' @param path  Character string of length 1. The base directory within which
#'              there are only module subdirectories.
#'
#' @return Nothing is returned. All file are open via `file.edit`.
#'
#' @note On Windows there is currently a bug in RStudio that prevents the editor
#' from opening when `file.edit` is called. `file.edit` does work if the
#' user types it at the command prompt. A message with the correct lines to copy
#' and paste is provided.
#'
#' @author Eliot McIntire
#' @export
#' @importFrom reproducible checkPath
#' @rdname openModules
#'
#' @examples
#' \donttest{
#' if (interactive())
#'   openModules("modules")
#' }
#'
setGeneric("openModules", function(name, path) {
  standardGeneric("openModules")
})

#' @export
#' @rdname openModules
setMethod(
  "openModules",
  signature = c(name = "character", path = "character"),
  definition = function(name, path) {
    basedir <- checkPath(path, create = FALSE)
    fileExtension <- tools::file_ext(name)
    if (length(unique(fileExtension)) > 1) {
      stop("Can only open one file type at a time.")
    }
    ncharFileExt <- unlist(lapply(fileExtension, nchar))
    origDir <- getwd()
    setwd(basedir)
    if (any(name == "all")) {
      Rfiles <- dir(pattern = "[\\.][Rr]$", recursive = TRUE, full.names = TRUE)
    } else if (all(ncharFileExt > 0) & all(fileExtension != "R")) {
      Rfiles <- dir(pattern = name, recursive = TRUE, full.names = TRUE)
      Rfiles <- Rfiles[unlist(lapply(name, function(n) grep(pattern = n, Rfiles)))]
    } else {
      Rfiles <- dir(pattern = "[\\.][Rr]$", recursive = TRUE, full.names = TRUE)
      Rfiles <- Rfiles[unlist(lapply(name, function(n) grep(pattern = n, Rfiles)))]
    }
    # remove tests
    hasTests <- grep(pattern = "tests", Rfiles)
    if (length(hasTests) > 0) Rfiles <- Rfiles[-hasTests]

    onlyModuleRFile <- unlist(lapply(file.path(name, name), function(n) {
      grep(pattern = n, Rfiles)
    }))
    if (length(onlyModuleRFile) > 0) Rfiles <- Rfiles[onlyModuleRFile]

    # Open Rmd file also
    # RfileRmd <- dir(pattern = paste0(name, ".[Rr]md$"), recursive = TRUE, full.names = TRUE)

    # Rfiles <- c(Rfiles, RfileRmd)
    Rfiles <- Rfiles[grep(pattern = "[/\\\\]", Rfiles)]
    Rfiles <- Rfiles[sapply(strsplit(Rfiles,"[/\\\\\\.]"), function(x) any(duplicated(x)))]

    lapply(file.path(basedir, Rfiles), .fileEdit)
    setwd(origDir)
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "missing", path = "missing"),
          definition = function() {
            openModules(name = "all", path = ".")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "missing", path = "character"),
          definition = function(path) {
            openModules(name = "all", path = path)
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "character", path = "missing"),
          definition = function(name) {
            openModules(name = name, path = ".")
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "simList", path = "missing"),
          definition = function(name) {
            mods <- unlist(modules(name))
            openModules(name = mods, path = modulePath(name))
})

#' Create a copy of an existing module
#'
#' @param from  The name of the module to copy.
#'
#' @param to    The name of the copy.
#'
#' @param path  The path to a local module directory. Defaults to the path set by
#'              the `spades.modulePath` option. See [setPaths()].
#'
#' @param ...   Additional arguments to `file.copy`, e.g., `overwrite = TRUE`.
#'
#' @return Invisible logical indicating success (`TRUE`) or failure (`FALSE`).
#'
#' @author Alex Chubaty
#' @export
#' @rdname copyModule
setGeneric("copyModule", function(from, to, path, ...) {
  standardGeneric("copyModule")
})

#' @export
#' @rdname copyModule
setMethod(
  "copyModule",
  signature = c(from = "character", to = "character", path = "character"),
  definition = function(from, to, path, ...) {
    if (!dir.exists(to)) {
      dir.create(file.path(path, to))
      dir.create(file.path(path, to, "data"))
      dir.create(file.path(path, to, "tests"))
      dir.create(file.path(path, to, "tests", "testthat"))
    }

    files <- dir(file.path(path, from), full.names = TRUE, recursive = TRUE)

    ## files in base dir
    ids <- which(basename(dirname(files)) == from)
    result <- file.copy(from = files[ids],
                        to = file.path(path, to), ...)
    result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".R")),
                                    to = file.path(path, to, paste0(to, ".R"))))
    result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".Rmd")),
                                    to = file.path(path, to, paste0(to, ".Rmd"))))
    if (file.exists(file.path(path, to, paste0(from, ".pdf")))) {
      result <- c(result, file.rename(from = file.path(path, to, paste0(from, ".pdf")),
                                      to = file.path(path, to, paste0(to, ".pdf"))))
    }

    ## files in "data" dir
    ids <- which(basename(dirname(files)) == "data")
    if (length(ids) > 0) {
      result <- c(result, file.copy(from = files[ids],
                        to = file.path(path, to, "data"), ...))
    }

    ## files in "tests" dir
    ids <- which(basename(dirname(files)) == "test")
    if (length(ids) > 0) {
      result <- c(result, file.copy(from = files[ids],
                                    to = file.path(path, to, "tests"), ...))
    }

    ## files in "testthat" subdir
    ids <- which(basename(dirname(files)) == "testthat")
    if (length(ids) > 0) {
      result <- c(result, file.copy(from = files[ids],
                                    to = file.path(path, to, "tests", "testthat"), ...))
    }

    if (!all(result)) warning("some module files could not be copied.")

    return(invisible(all(result)))
})

#' @export
#' @rdname copyModule
setMethod("copyModule",
          signature = c(from = "character", to = "character", path = "missing"),
          definition = function(from, to, ...) {
            copyModule(from, to, path = getOption('spades.modulePath'), ...)
})

#' Create a zip archive of a module subdirectory
#'
#' The most common use of this would be from a "modules" directory, rather than
#' inside a given module.
#'
#' @param name    Character string giving the module name.
#' @param path    A file path to a directory containing the module subdirectory.
#' @param version The module version.
#' @param data    Logical. If `TRUE`, then the data subdirectory will be included in the zip.
#'                Default is `FALSE`.
#' @param ...     Additional arguments to [zip()]:
#'                e.g., add `"-q"` using `flags="-q -r9X"`
#'                (the default flags are `"-r9X"`).
#'
#' @return Nothing is returned. Invoked for its side effect of zipping module files.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @importFrom reproducible checkPath
#' @importFrom utils zip
#' @rdname zipModule
#'
setGeneric("zipModule", function(name, path, version, data = FALSE, ...) {
  standardGeneric("zipModule")
})

#' @export
#' @rdname zipModule
setMethod(
  "zipModule",
  signature = c(name = "character", path = "character", version = "character"),
  definition = function(name, path, version, data, ...) {
    dots <- list(...)

    path <- checkPath(path, create = FALSE)
    callingWd <- getwd()
    on.exit(setwd(callingWd), add = TRUE)
    setwd(path)
    zipFileName <- paste0(name, "_", version, ".zip")
    message(crayon::green(paste("Zipping module into zip file:", zipFileName)), sep = "")

    allFiles <- dir(path = file.path(name), recursive = TRUE, full.names = TRUE)

    # filter out 'moduleName_*.zip' from results
    allFiles <- grep(paste0(name, "_+.+.zip"), allFiles, value = TRUE, invert = TRUE)

    if (!data) {
      # filter out all data file but keep the 'CHECKSUMS.txt' file
      allFiles <- grep(file.path(name, "data"),  allFiles, invert = TRUE, value = TRUE)
      allFiles <- sort(c(allFiles, file.path(name, "data", "CHECKSUMS.txt")))
    }

    tryOut <- try(zip(zipFileName, files = allFiles, ...))
    if (is(tryOut, "try-error") || isTRUE(tryOut == 127)) {
      if (Sys.info()["sysname"] == "Windows") {
        if (is.null(dots$zip) & all(Sys.getenv(c("R_ZIPCMD", "zip")) %in% ""))
          stop("External zip command paths missing.\nAdd 'zip = \"path/to/zip.exe\"' specifying path to zip.exe")
      }
    }
    file.copy(zipFileName, to = paste0(name, "/", zipFileName), overwrite = TRUE)
    file.remove(zipFileName)
})

#' @rdname zipModule
#' @export
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "character"),
          definition = function(name, version, data, ...) {
            zipModule(name = name, path = "..", version = version, data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "missing"),
          definition = function(name, data, ...) {
            zipModule(name = name, path = "..", data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "character", version = "missing"),
          definition = function(name, path, data, ...) {
            vers <- moduleVersion(name, path) %>% as.character()
            zipModule(name = name, path = path, version = vers, data = data, ...)
})
