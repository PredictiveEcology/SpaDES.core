% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simList-accessors.R
\name{globals}
\alias{globals}
\alias{globals,simList-method}
\alias{globals<-}
\alias{globals<-,simList-method}
\alias{G}
\alias{G,simList-method}
\alias{G<-}
\alias{G<-,simList-method}
\title{Get and set global simulation parameters}
\usage{
globals(sim)

\S4method{globals}{simList}(sim)

globals(sim) <- value

\S4method{globals}{simList}(sim) <- value

G(sim)

\S4method{G}{simList}(sim)

G(sim) <- value

\S4method{G}{simList}(sim) <- value
}
\arguments{
\item{sim}{A \code{simList} object from which to extract element(s) or in which to replace element(s).}

\item{value}{The parameter value to be set (in the corresponding \code{module} and \code{param}).}
}
\description{
\code{globals}, and the alias \code{G}, accesses or sets the "globals"
in the \code{simList}. This currently is not an explicit slot in the \code{simList},
but it is a \code{.globals} element in the \code{params} slot of the \code{simList}.
}
\seealso{
\link{SpaDES.core-package}, specifically the section 1.2.1 on Simulation Parameters.

Other functions to access elements of a 'simList' object: 
\code{\link{.addDepends}()},
\code{\link{checkpointFile}()},
\code{\link{envir}()},
\code{\link{events}()},
\code{\link{inputs}()},
\code{\link{modules}()},
\code{\link{objs}()},
\code{\link{packages}()},
\code{\link{params}()},
\code{\link{paths}()},
\code{\link{progressInterval}()},
\code{\link{times}()}
}
\concept{functions to access elements of a 'simList' object}
