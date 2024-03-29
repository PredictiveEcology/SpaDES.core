% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simList-accessors.R
\name{paths}
\alias{paths}
\alias{simList-accessors-paths}
\alias{paths,simList-method}
\alias{paths<-}
\alias{paths<-,simList-method}
\alias{cachePath}
\alias{cachePath,simList-method}
\alias{cachePath<-}
\alias{cachePath<-,simList-method}
\alias{inputPath}
\alias{inputPath,simList-method}
\alias{inputPath<-}
\alias{inputPath<-,simList-method}
\alias{outputPath}
\alias{outputPath,simList-method}
\alias{outputPath<-}
\alias{outputPath<-,simList-method}
\alias{figurePath}
\alias{figurePath,simList-method}
\alias{logPath}
\alias{logPath,simList-method}
\alias{modulePath}
\alias{modulePath,simList-method}
\alias{modulePath<-}
\alias{modulePath<-,simList-method}
\alias{scratchPath}
\alias{scratchPath,simList-method}
\alias{scratchPath<-}
\alias{scratchPath<-,simList-method}
\alias{rasterPath}
\alias{rasterPath,simList-method}
\alias{rasterPath<-}
\alias{rasterPath<-,simList-method}
\alias{terraPath}
\alias{terraPath,simList-method}
\alias{terraPath<-}
\alias{terraPath<-,simList-method}
\alias{dataPath}
\alias{dataPath,simList-method}
\title{Specify paths for modules, inputs, outputs, and temporary rasters}
\usage{
paths(sim)

\S4method{paths}{simList}(sim)

paths(sim) <- value

\S4method{paths}{simList}(sim) <- value

cachePath(sim)

\S4method{cachePath}{simList}(sim)

cachePath(sim) <- value

\S4method{cachePath}{simList}(sim) <- value

inputPath(sim)

\S4method{inputPath}{simList}(sim)

inputPath(sim) <- value

\S4method{inputPath}{simList}(sim) <- value

outputPath(sim)

\S4method{outputPath}{simList}(sim)

outputPath(sim) <- value

\S4method{outputPath}{simList}(sim) <- value

figurePath(sim)

\S4method{figurePath}{simList}(sim)

logPath(sim)

\S4method{logPath}{simList}(sim)

modulePath(sim, module)

\S4method{modulePath}{simList}(sim, module)

modulePath(sim) <- value

\S4method{modulePath}{simList}(sim) <- value

scratchPath(sim)

\S4method{scratchPath}{simList}(sim)

scratchPath(sim) <- value

\S4method{scratchPath}{simList}(sim) <- value

rasterPath(sim)

\S4method{rasterPath}{simList}(sim)

rasterPath(sim) <- value

\S4method{rasterPath}{simList}(sim) <- value

terraPath(sim)

\S4method{terraPath}{simList}(sim)

terraPath(sim) <- value

\S4method{terraPath}{simList}(sim) <- value

dataPath(sim)

\S4method{dataPath}{simList}(sim)
}
\arguments{
\item{sim}{A \code{simList} object from which to extract element(s) or in which to replace element(s).}

\item{value}{The parameter value to be set (in the corresponding \code{module} and \code{param}).}

\item{module}{The optional character string of the module(s) whose
paths are desired. If omitted, will return all module paths,
if more than one exist.}
}
\value{
Returns or sets the value of the slot from the \code{simList} object.
}
\description{
Accessor functions for the \code{paths} slot in a \code{simList} object.

\code{dataPath} will return \code{file.path(modulePath(sim), currentModule(sim), "data")}.
\code{dataPath}, like \code{currentModule},is namespaced. This means that when
it is used inside a module, then it will return \emph{that model-specific} information.
For instance, if used inside a module called \code{"movingAgent"},
then \code{currentModule(sim)}
will return \code{"movingAgent"}, and \code{dataPath(sim)} will return
\code{file.path(modulePath(sim), "movingAgent", "data")}
}
\details{
These are ways to add or access the file paths used by \code{\link[=spades]{spades()}}.
There are five file paths: \code{cachePath}, \code{modulePath},
\code{inputPath}, \code{outputPath}, and \code{rasterPath}.
Each has a function to get or set the value in a \code{simList} object.
If no paths are specified, the defaults are as follows:

\itemize{
\item \code{cachePath}: \code{getOption("reproducible.cachePath")};
\item \code{inputPath}: \code{getOption("spades.modulePath")};
\item \code{modulePath}: \code{getOption("spades.inputPath")};
\item \code{outputPath}: \code{getOption("spades.outputPath")};
\item \code{rasterPath}: \code{file.path(getOption("spades.scratchPath"), "raster")};
\item \code{scratchPath}: \code{getOption("spades.scratchPath")};
\item \code{terraPath}: \code{file.path(getOption("spades.scratchPath"), "terra")}
}
}
\seealso{
\link{SpaDES.core-package}, specifically the section 1.2.4 on Simulation Paths.

Other functions to access elements of a 'simList' object: 
\code{\link{.addDepends}()},
\code{\link{checkpointFile}()},
\code{\link{envir}()},
\code{\link{events}()},
\code{\link{globals}()},
\code{\link{inputs}()},
\code{\link{modules}()},
\code{\link{objs}()},
\code{\link{packages}()},
\code{\link{params}()},
\code{\link{progressInterval}()},
\code{\link{times}()}
}
\concept{functions to access elements of a 'simList' object}
