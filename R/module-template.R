#' @keywords internal
openIsRequested <- function(open, suff) {
  isTRUE(open) || endsWith(tolower(open), suff)
}

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
.fileEdit <- function(file) {
  if (Sys.getenv("RSTUDIO") == "1") {
    file <- gsub(file, pattern = "\\./", replacement = "")
    file.show(file)
    message("Using RStudio; may need to open manually e.g., with file.edit or file.show")
  } else {
    file.edit(file)
  }
  message(paste0("file.edit('", file, "')"))
}

#' Create new module from template
#'
#' Generate a skeleton for a new SpaDES module, a template for a
#' documentation file, a citation file, a license file, a \file{README.md} file,
#' and a folder that contains unit tests information. `newModule` is largely a
#' wrapper around `newModuleCode` and `newModuleDocumentation`.
#' `newModuleCode` will not generate the module code.
#' `newModuleDocumentation` will create the other files.
#'
#' All files will be created within a subdirectory named `name` within the `path`:
#'
#' ```
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
#' ```
#'
#' @param name  Character string specifying the name of the new module.
#'
#' @param path  Character string. Subdirectory in which to place the new module code file.
#'              The default is the current working directory.
#'
#' @param ...   Additional arguments. Currently, these can be either named
#'    function definitions (which will be added to the `simList`) or one or
#'    more of the following:\cr\cr
#' \describe{
#'   \item{`children`}{Required when `type = "parent"`. A character vector
#'   specifying the names of child modules.}
#'   \item{`open`}{Logical. Should the new module file be opened after creation?
#'   Default `TRUE`.}
#'   \item{`type`}{Character string specifying one of `"child"` (default),
#'   or `"parent"`.}
#' }
#' For `newModule` can also be:\cr\cr
#' \describe{
#'   \item{`unitTests`}{Logical. Should the new module include unit test files?
#'   Default `TRUE`. Unit testing relies on the \pkg{testthat} package.}
#'   \item{`useGitHub`}{Logical. Is module development happening on GitHub?
#'   Default `TRUE`.}
#' }
#'
#' @param events A list of named expressions, each of which is surrounded by `{ }`.
#'   A user can specify events here, instead of accepting the default `doEvent` function
#'   that comes with the module template. If this is specified, all events must
#'   be specified, i.e., it will not inherit partially from the template `doEvent.<moduleName>`.
#'   See example.
#' @param envir An environment where objects being passed to `newModule` can be found.
#'   Default `parent.frame()`, which should be fine for most cases.
#'
#' @return NULL (invisibly). The new module file is created at
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
#' if (requireNamespace("ggplot2")) {
#'   # We can also specify events and functions in `newModule`; it will still get all
#'   #   functions that are not specified from the module template (e.g., plotFun below)
#'   nm <- "test"
#'   modulePath <- Require::tempdir2()
#'   newModule(nm, path = modulePath, open = FALSE,
#'             events = list(
#'               init = {
#'                   sim <- Init(sim)                            # finds definition below
#'                   sim <- scheduleEvent(sim, start(sim) + 1,
#'                                        eventType = "plot")
#'                 },
#'               plot = {
#'                   plotFun(sim)                                # finds the templated plotFun
#'                   sim <- scheduleEvent(sim, time(sim) + 1,
#'                                        eventType = "plot")
#'                 }
#'               ,
#'             ),
#'             Init = function(sim) { # replaces Init definition from template
#'               sim$a <- 1
#'               return(sim)
#'             }
#'   )
#'   out <- simInitAndSpades(module = nm, paths = list(modulePath = modulePath))
#'   # clean up
#'   unlink(dir(modulePath, pattern = nm, full.names = TRUE), recursive = TRUE)
#' }
#'
#'
#'
setGeneric("newModule", function(name, path, ..., events = NULL, envir = parent.frame()) {
  standardGeneric("newModule")
})

#' @export
#' @rdname newModule
#' @importFrom reproducible checkPath
setMethod(
  "newModule",
  signature = c(name = "character", path = "character"),
  definition = function(name, path, ..., events, envir) {
    events <- substitute(events)

    # This if for methods that passed to here
    if (is.null(names(events)))
      events <- eval(events, parent.frame())

    argsFull <- substitute(list(...))
    # if (is.null(names(argsFull)))
    #   argsFull <- eval(argsFull, parent.frame())

    argsNames <- ...names()

    simpleArgsHere <- intersect(argsNames, simpleArgs)
    args <- eval(as.list(argsFull[simpleArgsHere]))
    args <- lapply(args, eval, envir = envir) # Things like T --> TRUE

    argsOther <- as.list(argsFull[setdiff(argsNames, simpleArgsHere)])
    if (any(sapply(argsOther, is.null))) {
      a <- argsFull[-1][!nzchar(names(argsFull[-1]))]
      if (any(grepl("<-", a[[1]][[1]])))
        stop("Did you use `<-` operator instead of `=` for arguments?")
    }
    # args <- list(...)

    stopifnot(all(names(args) %in% simpleArgs))

    ## define defaults for ... args
    if (is.null(args$children)) args$children <- NA_character_
    if (is.null(args$open)) {
      args$open <- open.user <- interactive()
    } else {
      open.user <- args$open
    }
    if (is.null(args$type)) args$type <- "child"
    if (is.null(args$unitTests)) args$unitTests <- TRUE
    if (is.null(args$useGitHub)) args$useGitHub <- TRUE

    stopifnot(
      is(args$children, "character"),
      is(args$open, "logical") || any(endsWith(tolower(args$open), c("r", "rmd"))),
      is(args$type, "character"),
      args$type %in% c("child", "parent"),
      is(args$unitTests, "logical"),
      is(args$useGitHub, "logical")
    )

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) |> checkPath(create = TRUE)
    dataPath <- file.path(nestedPath, "data") |> checkPath(create = TRUE)
    RPath <- file.path(nestedPath, "R") |> checkPath(create = TRUE)

    ## empty data checksum file
    cat("", file = file.path(dataPath, "CHECKSUMS.txt"))

    if (isTRUE(args$useGitHub)) {
      ## basic .gitignore file for module data
      gitignoreTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "data-gitignore.template"))
      writeLines(whisker.render(gitignoreTemplate), file.path(dataPath, ".gitignore"))
    }

    ## module code file
    args$open <- openIsRequested(open.user, suff = "r")
    do.call(newModuleCode, append(alist(name = name, path = path, events = events),
                                  append(args, argsOther)))

    if (!args$open) {
      message("Main module file is: ", normalizePath(file.path(path, name, paste0(name, ".R"))))
    }

    if (args$type == "child" && args$unitTests) {
      newModuleTests(name = name, path = path, open = !isFALSE(args$open), useGitHub = args$useGitHub)
    }

    ### Make R Markdown file for module documentation
    args$open <- openIsRequested(open.user, suff = "rmd")
    newModuleDocumentation(name = name, path = path,
                           open = args$open, type = args$type, children = args$children)
    if (!args$open) {
      message("Main documentation file is: ", normalizePath(file.path(path, name, paste0(name, ".Rmd"))))
    }

    return(invisible(NULL))
})

#' @export
#' @rdname newModule
setMethod(
  "newModule",
  signature = c(name = "character", path = "missing"),
  definition = function(name, ..., events = NULL, envir = parent.frame()) {

    # Take "." if not set, but it could be set by user with setPaths(modulePath = ...)
    path <- checkModulePath()
    events <- substitute(events)
    newModule(name = name, path = path, ..., events = events, envir = envir)
})

#' @return `newModuleCode` is invoked for its side effect of creating new module code files.
#'
#' @author Eliot McIntire and Alex Chubaty
#' @export
#' @rdname newModule
setGeneric("newModuleCode", function(name, path, ..., events) {
  standardGeneric("newModuleCode")
})

#' @export
#' @family module creation helpers
#' @importFrom cli col_green col_yellow style_bold
#' @importFrom reproducible checkPath
#' @importFrom whisker whisker.render
#' @rdname newModule
setMethod(
  "newModuleCode",
  signature = c(name = "character", path = "character"),
  definition = function(name, path, ..., events) {
    argsFull <- list(...)
    argsNames <- ...names()

    simpleArgsHere <- intersect(argsNames, simpleArgs)
    args <- eval(as.list(argsFull[simpleArgsHere]))

    stopifnot(all(names(args) %in% simpleArgs))

    ## define defaults for ... args
    if (is.null(args$children)) args$children <- NA_character_
    if (is.null(args$open)) {
      args$open <- open.user <- interactive()
    } else {
      open.user <- args$open
    }
    if (is.null(args$type)) args$type <- "child"

    stopifnot(args$type %in% c("child", "parent"))

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) |> checkPath(create = TRUE)
    filenameR <- file.path(nestedPath, paste0(name, ".R"))

    children_char <- if (any(is.na(args$children)) || length(args$children) == 0L) {
      "character(0)"
    } else {
      capture.output(dput(args$children))
    }

    version <- list()
    version[[name]] <- moduleDefaults[["version"]]
    if (args$type == "parent")
      lapply(args$children, function(x) version[[x]] <<- "0.0.1")

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
    otherMetadata <- if (args$type == "child") {
      whisker.render(modulePartialMetaTemplate, modulePartialMeta)
    } else {
      paste("## this is a parent module and as such does not have any",
            "reqdPkgs, parameters, inputObjects, nor outputObjects.")
    }

    modulePartialEvents <- list(
      name = name,
      name_char = deparse(name)
    )

    templDE <- "doEvent"
    templs <- c("Init", "Save", "plotFn", "Event1", "Event2", ".inputObjects", "ggplotFn")
    moduleTmplTxt <- list()
    for (templ in c(templDE, templs)) {
      moduleTmplTxt[[templ]] <- readLines(file.path(.pkgEnv[["templatePath"]],
                                                    paste0("module", templ, ".R.template")))
    }

    if (!missing(events) && !is.null(events) && !(length(events) == 0)) {
      evs <- names(events)[nzchar(names(events))]
      ord <- c(evs, templs)
      if (length(evs)) {
        moduleTmplTxt[["doEvent"]] <- NULL
        for (n in evs) {
          #   defineEvent(code = events[[n]], eventName = n, moduleName = name)

          eventFnName <-  makeEventFn(name, n)
          moduleTmplTxt[[n]] <- defineEventFnMaker(events[[n]], eventFnName)
        }
      } else {
        stop("events must be a list of named elements, where each name is the event name")
      }

      ord <- intersect(ord, names(moduleTmplTxt))
      moduleTmplTxt <- moduleTmplTxt[ord]

      # b <- lapply(events, substitute)

      if (!is.null(...names())) {
        other <- ...names()[nzchar(...names())]
        other <- setdiff(other, simpleArgs)
        ord <- c(templDE, evs, other, templs)
        if (length(other)) {
          for (n in other) {
            ## defineEventFnMaker(events[[n]], eventFnName)
            moduleTmplTxt[[n]] <- paste0(n, " <- ", paste(format(argsFull[[n]]), collapse = "\n"))
          }
        }

        ord <- intersect(ord, names(moduleTmplTxt))
        moduleTmplTxt <- moduleTmplTxt[ord]
      }
    }

    moduleEventsTemplate <- unlist(moduleTmplTxt, use.names = FALSE)

    moduleEvents <- if (args$type == "child") {
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
      type = args$type,
      versions = deparse(version)
    )
    moduleTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "module.R.template"))
    writeLines(whisker.render(moduleTemplate, moduleData), filenameR)

    ## help the user with next steps
    message(cli::style_bold(paste(
      cli::col_green("New module"),
      cli::col_yellow(name),
      cli::col_green("created at"),
      cli::col_yellow(dirname(nestedPath))
    )))
    message(paste(
      cli::col_green("* edit module code in"), cli::col_yellow(paste0(name, ".R"))
    ))
    message(paste(
      cli::col_green("* write tests for your module code in"), cli::col_yellow("tests/")
    ))
    message(paste(
      cli::col_green("* describe and document your module in"), cli::col_yellow(paste0(name, ".Rmd"))
    ))
    message(paste(
      cli::col_green("* tell others how to cite your module by editing"), cli::col_yellow("citation.bib")
    ))
    message(paste(
      cli::col_green("* choose a license for your module; see"), cli::col_yellow("LICENSE.md")
    ))

    if (isTRUE(args$open)) {
      openModules(name, nestedPath)
    }

    return(invisible(NULL))
})

#' @return `newModuleDocumentation` is nvoked for its side effect of
#'   creating new module documentation files.
#'
#' @importFrom reproducible checkPath
#' @export
#' @family module creation helpers
#' @rdname newModule
#'
setGeneric("newModuleDocumentation", function(name, path, ...) {
  standardGeneric("newModuleDocumentation")
})

#' @export
#' @rdname newModule
setMethod(
  "newModuleDocumentation",
  signature = c(name = "character", path = "character"),
  definition = function(name, path, ...) {
    argsFull <- list(...)
    argsNames <- ...names()
    simpleArgsHere <- intersect(argsNames, simpleArgs)
    args <- eval(as.list(argsFull[simpleArgsHere]))

    stopifnot(all(names(args) %in% simpleArgs))

    ## define defaults for ... args
    if (is.null(args$children)) args$children <- NA_character_
    if (is.null(args$open)) args$open <- interactive()
    if (is.null(args$type)) args$type <- "child"

    stopifnot(args$type %in% c("child", "parent"))

    path <- checkPath(path, create = TRUE)
    nestedPath <- file.path(path, name) |> checkPath(create = TRUE)
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

    if (args$open) {
      openModules(basename(filenameRmd), nestedPath)
    }

    return(invisible(NULL))
})

#' @export
#' @rdname newModule
setMethod("newModuleDocumentation",
          signature = c(name = "character", path = "missing"),
          definition = function(name, ...) {
            path <- checkModulePath()

            newModuleDocumentation(name = name, path = path, ...)
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
#' @return NULL (invisibly). Invoked for its side effect of creating new module test files.
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
    testthatDir <- file.path(path, name, "tests", "testthat") |> checkPath(create = TRUE)
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

    return(invisible(NULL))
})

#' Open all modules nested within a base directory
#'
#' This is just a convenience wrapper for opening several modules at once, recursively.
#' A module is defined as any file that ends in `.R` or `.r` and has a
#' directory name identical to its filename. Thus, this must be case sensitive.
#'
#' @param name  Character vector with names of modules to open. If missing, then
#'              all modules will be opened within the base directory.
#'
#' @param path  Character string of length 1. The base directory within which
#'              there are only module subdirectories.
#'
#' @return NULL (invisibly). All file are open via `file.edit`.
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

    return(invisible(NULL))
})

#' @export
#' @rdname openModules
setMethod("openModules",
          signature = c(name = "missing", path = "missing"),
          definition = function() {
            path <- checkModulePath()

            openModules(name = "all", path = path)
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
            path <- checkModulePath()

            openModules(name = name, path = path)
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
            path <- checkModulePath()

            copyModule(from, to, path = getOption("spades.modulePath"), ...)
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
#' @return NULL (invisibly). Invoked for its side effect of zipping module files.
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
    message(cli::col_green(paste("Zipping module into zip file:", zipFileName)), sep = "")

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

    return(invisible(NULL))
})

#' @rdname zipModule
#' @export
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "character"),
          definition = function(name, version, data, ...) {
            path <- checkModulePath()

            zipModule(name = name, path = "..", version = version, data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "missing", version = "missing"),
          definition = function(name, data, ...) {
            path <- checkModulePath()

            zipModule(name = name, path = "..", data = data, ...)
})

#' @export
#' @rdname zipModule
setMethod("zipModule",
          signature = c(name = "character", path = "character", version = "missing"),
          definition = function(name, path, data, ...) {
            vers <- moduleVersion(name, path) |> as.character()
            zipModule(name = name, path = path, version = vers, data = data, ...)
})

simpleArgs <- c("children", "open", "type", "unitTests", "useGitHub")


#' Uses "." if getPath not set
#'
#' Will compare default in spadesOptions to getPaths ... these will be same if use
#' has not set them. For such case, use ".". They will be different if the user has
#' used `setPaths`. If that is the case, then use `getPaths()[["modulePath"]]`
checkModulePath <- function() {
  gp <- getPaths()
  mp1 <- normalizePath(gp[["modulePath"]], winslash = "/", mustWork = FALSE)
  mp2 <- normalizePath(spadesOptions()[["spades.modulePath"]], winslash = "/", mustWork = FALSE)
  path <- if (identical(mp1, mp2)) "." else gp[["modulePath"]]
  path
}
