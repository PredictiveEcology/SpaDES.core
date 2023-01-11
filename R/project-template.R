#' Create new SpaDES project
#'
#' Initialize a project with subdirectories \file{cache/}, \file{modules/},
#' \file{inputs/}, \file{outputs/}, and `setPaths` accordingly.
#' If invoked from Rstudio, will also create a new Rstudio project file.
#'
#' @param name project name (name of project directory)
#' @param path path to directory in which to create the project directory
#' @param open  Logical. Should the new project file be opened after creation?
#'              Default `TRUE` in an interactive session.
#'
#' @export
#' @rdname newProject
#'
#' @examples
#' myProjDir <- newProject("myProject", tempdir())
#'
#' dir.exists(file.path(myProjDir, "cache"))
#' dir.exists(file.path(myProjDir, "inputs"))
#' dir.exists(file.path(myProjDir, "modules"))
#' dir.exists(file.path(myProjDir, "outputs"))
#' unlink(myProjDir, recursive = TRUE) ## cleanup
setGeneric("newProject", function(name, path, open) {
  standardGeneric("newProject")
})

#' @export
#' @rdname newProject
#' @importFrom reproducible checkPath
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", open = "logical"),
  definition = function(name, path, open) {
    checkPath(path, create = TRUE)
    projDir <- checkPath(file.path(path, name), create = TRUE)

    setPaths(
      cachePath = checkPath(file.path(path, name, "cache"), create = TRUE),
      inputPath = checkPath(file.path(path, name, "inputs"), create = TRUE),
      modulePath = checkPath(file.path(path, name, "modules"), create = TRUE),
      outputPath = checkPath(file.path(path, name, "outputs"), create = TRUE)#,
      #rasterPath = checkPath(file.path(dirname(tempdir()), "scratch", name), create = TRUE)
    )

    if (interactive() && Sys.getenv("RSTUDIO") == "1") {
      if (requireNamespace("rstudioapi", quietly = TRUE))
        rstudioapi::initializeProject(path = projDir)
    }

    newProjectCode(name, path, open = open)

    return(projDir)
})

#' @export
#' @rdname newProject
#' @importFrom reproducible checkPath
setMethod(
  "newProject",
  signature = c(name = "character", path = "character", open = "missing"),
  definition = function(name, path, open) {
    newProject(name, path, open = interactive())
})

#' Create new module code file
#'
#' @inheritParams newProject
#'
#' @author Alex Chubaty
#' @export
#' @rdname newProjectCode
#'
setGeneric("newProjectCode", function(name, path, open) {
  standardGeneric("newProjectCode")
})

#' @export
#' @importFrom reproducible checkPath
#' @importFrom whisker whisker.render
#' @rdname newProjectCode
setMethod(
  "newProjectCode",
  signature = c(name = "character", path = "character", open = "logical"),
  definition = function(name, path, open = interactive()) {
    nestedPath <- checkPath(file.path(path, name), create = TRUE)
    filenameR <- file.path(nestedPath, paste0(name, "-project.R"))

    projectData <- list()
    projectTemplate <- readLines(file.path(.pkgEnv[["templatePath"]], "project.R.template"))
    writeLines(whisker.render(projectTemplate, projectData), filenameR)

    if (open) .fileEdit(filenameR)
})
