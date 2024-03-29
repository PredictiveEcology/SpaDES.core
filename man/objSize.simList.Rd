% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache.R
\name{objSize.simList}
\alias{objSize.simList}
\title{Object size for \code{simList}}
\usage{
\method{objSize}{simList}(x, quick = TRUE, ...)
}
\arguments{
\item{x}{An object}

\item{quick}{Logical. If \code{FALSE}, then an attribute, "objSize" will be added to
the returned value, with each of the elements' object size returned also.}

\item{...}{Additional arguments (currently unused), enables backwards compatible use.}
}
\value{
an estimate of the size of the object, in bytes.
}
\description{
Recursively, runs \code{\link[reproducible:objSize]{reproducible::objSize()}} on the \code{simList} environment,
so it estimates the correct size of functions stored there (e.g., with their enclosing
environments) plus, it adds all other "normal" elements of the \code{simList}, e.g.,
\code{objSize(completed(sim))}.
The output is structured into 2 elements: the \code{sim} environment and all its objects,
and the other slots in the \code{simList} (e.g., events, completed, modules, etc.).
The returned object also has an attribute, "total", which shows the total size.
}
\examples{
a <- simInit(objects = list(d = 1:10, b = 2:20))
objSize(a)
utils::object.size(a)
}
