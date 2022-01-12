#' Convert standard module code into an R package
#'
#' This will move all functions that are not already in an \code{.R} file
#' in the \code{R} folder into that folder, one function per file, including the
#' \code{doEvent.xxx} function. It will not
#' touch any other functions already in the \code{"R"} folder. It will also create
#' and fill a minimal \code{DESCRIPTION} file. This will leave the `defineModule`
#' function call as the only code in the main module file. This \code{defineModule}
#' and a \code{doEvent.xxx} are the only 2 elements that are required for an R
#' package to be considered a SpaDES module. With these changes, the module should
#' still function normally (still experimental), but will be able to act like an
#' R package, e.g., for writing function documentation, using the \code{testthat}
#' infrastructure, etc.
#'
#' @export
#' @param module Character string of module name, without path
#' @param path Character string of modulePath. Defaults to
#'   \code{getOption("spades.modulePath")}
convertToPackage <- function(module = NULL, path = getOption("spades.modulePath")) {
  e <- new.env(parent = asNamespace("SpaDES.core"))
  mainModuleFile <- file.path(path, unlist(module), paste0(unlist(module), ".R"))
  aa <- parse(mainModuleFile)
  defModule <- grepl("^defineModule", aa)
  whDefModule <- which(defModule)
  whNotDefModule <- which(!defModule)

  fileNames <- lapply(whNotDefModule, function(element) {
    fn <- aa[[element]][[2]]

    filePath <- file.path(dirname(mainModuleFile), "R", paste0(gsub("\\.", "", fn), ".R"))
    if (isTRUE(grepl("^doEvent", fn))) {
      if (!any(grepl("@export", aa[[element]][[3]])))
        cat("#' @export", file = filePath, sep = "\n", append = FALSE)
    }

    cat(format(aa[[element]]), file = filePath, sep = "\n", append = TRUE)
  })
  cat(format(aa[[whDefModule]]), file = mainModuleFile, sep = "\n")
  md <- aa[[whDefModule]][[3]]
  d <- list()
  d$Package <- .moduleNameNoUnderscore(module)
  d$Type <- "Package"

  d$Title <- md$name
  d$Description <- md$description
  d$Version <- as.character(eval(md$version[[2]]))
  d$Date <- Sys.Date()
  d$Authors <- md$authors
  d$Authors <- c(paste0("  ", format(d$Authors)[1]), format(d$Authors)[-1])
  deps <- unlist(eval(md$reqdPkgs))
  versionNumb <- Require::extractVersionNumber(deps)
  hasVersionNumb <- !is.na(versionNumb)
  inequality <- paste0("(", gsub("(.+)\\((.+)\\)", "\\2", deps[hasVersionNumb]), ")")
  d$Depends <- Require::extractPkgName(deps)
  d$Depends[hasVersionNumb] <- paste(d$Depends[hasVersionNumb], inequality)

  dFile <- file.path(dirname(mainModuleFile), "DESCRIPTION")

  cat(paste("Package:", d$Package), file = dFile, sep = "\n")
  cat(paste("Type:", d$Type), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Title:", d$Title), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Version:", d$Version), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Description:", d$Description), file = dFile, sep = "\n", append = TRUE)
  cat(paste("Date:", d$Date), file = dFile, sep = "\n", append = TRUE)
  cat(c("Authors@R:  ", format(d$Authors)), file = dFile, sep = "\n", append = TRUE)
  cat(c("Depends:", paste("   ", d$Depends, collapse = ",\n")), sep = "\n", file = dFile, append = TRUE)
  cat("Encoding: UTF-8", sep = "\n", file = dFile, append = TRUE)

  return(invisible())

}
