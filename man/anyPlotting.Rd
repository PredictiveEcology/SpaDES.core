% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plots.R
\name{anyPlotting}
\alias{anyPlotting}
\title{Test whether there should be any plotting from \code{.plots} module parameter}
\usage{
anyPlotting(.plots)
}
\arguments{
\item{.plots}{Usually will be the \code{P(sim)$.plots} is used within
a module.}
}
\value{
logical of length 1
}
\description{
This will do all the various tests needed to determine whether
plotting of one sort or another will occur.
Testing any of the types as listed in \code{\link[=Plots]{Plots()}} argument \code{types}.
Only the first 3 letters of the type are required.
}
