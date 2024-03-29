% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.R
\name{checkParams}
\alias{checkParams}
\alias{checkParams,simList,list-method}
\title{Check use and existence of parameters passed to simulation.}
\usage{
checkParams(sim, coreParams, ...)

\S4method{checkParams}{simList,list}(sim, coreParams, ...)
}
\arguments{
\item{sim}{A \code{simList} simulation object.}

\item{coreParams}{List of default core parameters.}

\item{...}{Additional arguments. Not implemented.}
}
\value{
Invisibly return \code{TRUE} indicating object exists; \code{FALSE} if not.
Sensible messages are produced identifying missing parameters.
}
\description{
Checks that all parameters passed are used in a module,
and that all parameters used in a module are passed.
}
\author{
Alex Chubaty
}
