% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memory.R
\name{memoryUseThisSession}
\alias{memoryUseThisSession}
\alias{memoryUse}
\title{Estimate memory used with \code{system("ps")}}
\usage{
memoryUseThisSession(thisPid)

memoryUse(sim, max = TRUE)
}
\arguments{
\item{thisPid}{Numeric or integer, the \code{PID} of the process.
If omitted, it will be found with \code{Sys.getpid()}.}

\item{sim}{A completed \code{simList}}

\item{max}{Logical. If TRUE, then it the return value will be summarized by
module/event, showing the maximum memory used. If \code{FALSE}, then
the raw memory used during each event will be shown.}
}
\value{
estimated memory use in MiB

\code{data.table} summarizing the estimated memory use (in MiB) for each event type,
for each module, during the simulation.
}
\description{
This will give a slightly different estimate than \code{pryr::mem_used}, which uses \code{gc()} internally.
The purpose of this function is to allow continuous monitoring, external to the R session.
Normally, this is run in a different session.

This will only work if the user has specified before running the \code{spades} call,
set the interval, in seconds, that \code{ps} is run.
E.g., \code{options("spades.memoryUseInterval" = 0.5)}, will assess memory use every 0.5 seconds.
The default is \code{0}, meaning no interval, "off".
}
\note{
The suggested \code{future} and \code{future.callr} packages must be available.
}
\seealso{
The \code{vignette("iv-modules")}
}
