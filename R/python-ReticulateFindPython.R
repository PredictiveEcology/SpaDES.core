
#' Reticulate Find Python
#'
#' Use `reticulate` to find or install Python to meet version requirements.
#' Download and use the \href{https://pypi.org/project/pyenv-win/}{pyenv-win}
#' Python version management tool from Github if necessary.
#'
#' This function was created to bypass the requirement for Git to be installed
#' when \code{\link[reticulate]{install_python}} is called on a Windows computer
#' without pyenv-win already installed.
#' If Git is available, `reticulate` will clone the
#' \href{https://github.com/pyenv-win/pyenv-win}{pyenv-win Github repository}
#' and use the tool to install Python.
#' If Git is not available, this function will instead download the tool
#' directly from the repository and make it available to `reticulate`.
#'
#' @param version character. Python version or a comma separated list of version constraints.
#' See \code{\link[reticulate]{virtualenv_starter}} for more details.
#' @param versionInstall character. Version to install if suitable version not found.
#' Required if `version` is a list of version constraints.
#' @param pyenvRoot character. Path of location for install of the pyenv-win tool.
#' Defaults to the R user data directory.
#' @param pyenvOnly logical. Exclude versions not within a pyenv install directory.
#' @param useGit logical. Allow `reticulate` to clone pyenv-win if Git is available.
#' @param prompt logical. Prompt user to approve download of pyenv-win tool.
#'
#' @return character. Path to Python interpreter.
#' @export
#'
#' @importFrom tools R_user_dir
ReticulateFindPython <- function(version, versionInstall = version, pyenvRoot = NULL,
                                 pyenvOnly = FALSE, useGit = TRUE, prompt = FALSE){

  if (!requireNamespace("reticulate", quietly = TRUE)) stop(
    "The package \"reticulate\" is required to use Python")

  # Set pyenv-win location
  if (is.null(pyenvRoot)) pyenvRoot <- R_user_dir("SpaDES.core")

  # Get path to Python interpreter
  pyInterp <- reticulate_python_path(version, pyenvRoot = pyenvRoot, pyenvOnly = pyenvOnly)

  # If found: return
  if (!is.null(pyInterp)) return(pyInterp)

  # If not found: install Python
  if (isWindows()){

    reticulate_install_python_windows(
      versionInstall, pyenvRoot = pyenvRoot, useGit = useGit, prompt = prompt)

  }else{

    reticulate::install_python(versionInstall)
  }

  # Return path to interpreter
  reticulate_python_path(version, pyenvRoot = pyenvRoot, pyenvOnly = pyenvOnly)
}


#' Reticulate: Return Python interpreter path
#'
#' Get path to a Python interpreter including installs at a given pyenv-win location.
#'
#' @param version character. Python version or a comma separated list of version constraints.
#' See \code{\link[reticulate]{virtualenv_starter}} for more details.
#' @param pyenvRoot character. Path to directory containing pyenv-win tool.
#' @param pyenvOnly logical. Exclude versions not installed by pyenv.
#'
#' @return character or NULL. If found, a path to Python interpreter.
#'
#' @importFrom tools R_user_dir
#' @keywords internal
reticulate_python_path <- function(version = NULL, pyenvRoot = NULL, pyenvOnly = FALSE){

  if (!requireNamespace("reticulate", quietly = TRUE)) stop(
    "The package \"reticulate\" is required to use Python")

  # Get paths to Python interpreters in known locations
  pyPaths <- reticulate::virtualenv_starter(version = version, all = TRUE)

  # Search pyenv-win installs
  if (isWindows()){

    # Set pyenv-win location
    if (is.null(pyenvRoot)) pyenvRoot <- R_user_dir("SpaDES.core")
    pyenvDir <- file.path(pyenvRoot, "pyenv-win")

    if (file.exists(pyenvDir)){

      if (!requireNamespace("withr", quietly = TRUE)) stop("The package \"withr\" is required")

      withr::local_envvar(
        c(PYENV      = file.path(pyenvDir, "pyenv-win", fsep = "/"),
          PYENV_ROOT = file.path(pyenvDir, "pyenv-win", fsep = "/"),
          PYENV_HOME = file.path(pyenvDir, "pyenv-win", fsep = "/")
        ))
      withr::local_path(
        c(file.path(pyenvDir, "pyenv-win", "bin",   fsep = "/"),
          file.path(pyenvDir, "pyenv-win", "shims", fsep = "/")),
        action = "prefix")

      pyPaths <- rbind(
        pyPaths,
        reticulate::virtualenv_starter(version = version, all = TRUE)
      )
    }
  }

  if (pyenvOnly & nrow(pyPaths) > 0){
    pyPaths <- pyPaths[sapply(pyPaths$path, function(path){
      any(c(".pyenv", "pyenv", ".pyenv-win", "pyenv-win") %in%
            strsplit(normalizePath(path, winslash = "/"), "/")[[1]])
    }),]
  }

  # Choose highest version
  if (nrow(pyPaths) > 1){
    pyPaths <- pyPaths[pyPaths$version == max(pyPaths$version),]
  }

  # Return path or NULL
  if (nrow(pyPaths) > 0) return(pyPaths[["path"]][[1]])
}


#' Reticulate: Install Python on Windows
#'
#' Download and use the \href{https://pypi.org/project/pyenv-win/}{pyenv-win}
#' Python version management tool from Github if necessary.
#'
#' @param version character. Python version string.
#' @param pyenvRoot character. Path of location for install of the pyenv-win tool.
#' Defaults to the R user data directory.
#' @param useGit logical. Allow `reticulate` to clone pyenv-win if Git is available.
#' @param prompt logical. Prompt user to approve download of pyenv-win tool.
#'
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip
#' @keywords internal
reticulate_install_python_windows <- function(
    version = NULL, pyenvRoot = NULL, useGit = TRUE, prompt = interactive()){

  if (!requireNamespace("reticulate", quietly = TRUE)) stop(
    "The package \"reticulate\" is required to use Python")

  if (!isWindows()) stop("Windows OS required")

  # Set pyenv-win location
  if (is.null(pyenvRoot)) pyenvRoot <- R_user_dir("SpaDES.core")
  pyenvDir <- file.path(pyenvRoot, "pyenv-win")

  if (!file.exists(pyenvDir)){

    # Check if Git is available on system
    reqAvailable <- c(
      git = ifelse(!useGit, FALSE, suppressWarnings(tryCatch({
        system("git --version", intern = TRUE)
        TRUE
      }, error = function(e) FALSE)))
    )

    # If Git not available: check if pyenv is available
    if (!reqAvailable[["git"]]){

      reqAvailable[["pyenv"]] <- suppressWarnings(tryCatch({
        system("pyenv --version", intern = TRUE)
        TRUE
      }, error = function(e) FALSE))
    }

    # If neither Git or pyenv is available: install pyenv-win directly from Github
    if (!any(reqAvailable)){

      dlPyenv <- TRUE

      if (prompt){

        ans <- readline("Type Y to download the pyenv-win tool for managing Python installations ")

        if (!identical(trimws(tolower(ans)), "y")){

          dlPyenv <- FALSE

          warning("reticulate may not be able install Python without pyenv-win")
        }
      }

      if (dlPyenv){

        # Create temporary directory
        tempDir <- tempfile()
        dir.create(tempDir)
        on.exit(unlink(tempDir, recursive = TRUE))

        # Download URL
        tempZip <- file.path(tempDir, "master.zip")
        download.file("https://github.com/pyenv-win/pyenv-win/archive/master.zip",
                      destfile = tempZip, quiet = TRUE)

        # Unzip and move to destination path
        unzip(tempZip, exdir = tempDir)

        dir.create(pyenvRoot, recursive = TRUE, showWarnings = FALSE)
        tryCatch(
          file.rename(file.path(tempDir, "pyenv-win-master"), pyenvDir),
          warning = function(w) stop(w, call. = FALSE))
      }
    }
  }

  # Add pyenv-win to environmental variables
  if (file.exists(pyenvDir)){

    if (!requireNamespace("withr", quietly = TRUE)) stop("The package \"withr\" is required")

    withr::local_envvar(
      c(PYENV      = file.path(pyenvDir, "pyenv-win", fsep = "/"),
        PYENV_ROOT = file.path(pyenvDir, "pyenv-win", fsep = "/"),
        PYENV_HOME = file.path(pyenvDir, "pyenv-win", fsep = "/")
      ))
    withr::local_path(
      c(file.path(pyenvDir, "pyenv-win", "bin",   fsep = "/"),
        file.path(pyenvDir, "pyenv-win", "shims", fsep = "/")),
      action = "prefix")
  }

  # Install Python
  tryCatch(

    if (is.null(version)){
      reticulate::install_python() # Let reticulate decide which version to install
    }else{
      reticulate::install_python(version = version)
    },

    error = function(e) stop(
      "Python installation failed. Python can be installed directly from python.org/downloads",
      "\n", e$message,
      call. = FALSE))
}

