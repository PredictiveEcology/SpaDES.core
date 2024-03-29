% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simList-accessors.R
\name{objs}
\alias{objs}
\alias{simList-accessors-objects}
\alias{objs,simList-method}
\alias{objs<-}
\alias{objs<-,simList-method}
\alias{moduleObjects}
\alias{findObjects}
\title{Extract or replace an object from the simulation environment}
\usage{
objs(sim, ...)

\S4method{objs}{simList}(sim, ...)

objs(sim) <- value

\S4method{objs}{simList}(sim) <- value

moduleObjects(sim, module, path)

findObjects(objects, sim, module, path)
}
\arguments{
\item{sim}{A \code{simList} object from which to extract element(s) or in which to replace element(s).}

\item{...}{passed to \code{ls}}

\item{value}{objects to assign to the \code{simList}}

\item{module}{Character vector of module name(s)}

\item{path}{The path to the module., i.e., the \code{modulePath}.
Only relevant if \code{sim} not supplied.}

\item{objects}{A character vector of length >= 1 with name(s) of objects to look
for in the metadata. This is used in a \code{grep}, meaning it will do partial
matching (e.g., \code{"studyArea"} will find \code{"studyArea"} and \code{"studyAreaLarge"}).
User can use regular expressions.}
}
\value{
Returns or sets a list of objects in the \code{simList} environment.

\code{moduleObjects} returns a data.table with 4 columns, \code{module}, \code{objectName}, \code{type}, and \code{desc},
pulled directly from the object metadata in the \code{createsOutputs} and \code{expectsInputs}. These
will be determined either from a \code{simList} or from the module source code.

\code{findObjects} returns a data.table similar to \code{moduleObjects}, but with only the
objects provided by \code{objects}.
}
\description{
The \code{[[} and \code{$} operators provide "shortcuts" for accessing
objects in the simulation environment.
I.e., instead of using \code{envir(sim)$object} or \code{envir(sim)[["object"]]},
one can simply use \code{sim$object} or \code{sim[["object"]]}.
}
\details{
\code{objs} can take \code{...} arguments passed to \code{ls},
allowing, e.g. \code{all.names=TRUE}
\verb{objs<-} requires takes a named list of values to be assigned in
the simulation environment.
}
\examples{
# findObjects
path <- getSampleModules(tempdir())
findObjects(path = path, module = dir(path), objects = "caribou")
}
\seealso{
\link{SpaDES.core-package}, specifically the section 1.2.1 on Simulation Parameters.

Other functions to access elements of a 'simList' object: 
\code{\link{.addDepends}()},
\code{\link{checkpointFile}()},
\code{\link{envir}()},
\code{\link{events}()},
\code{\link{globals}()},
\code{\link{inputs}()},
\code{\link{modules}()},
\code{\link{packages}()},
\code{\link{params}()},
\code{\link{paths}()},
\code{\link{progressInterval}()},
\code{\link{times}()}
}
\concept{functions to access elements of a 'simList' object}
