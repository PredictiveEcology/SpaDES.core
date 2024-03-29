% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save.R
\name{simFile}
\alias{simFile}
\title{Generate simulation file name}
\usage{
simFile(name, path, time = NULL, ext = "rds")
}
\arguments{
\item{name}{Object name (e.g., \code{"mySimOut"})}

\item{path}{Directory location in where the file will be located (e.g., an \code{outputPath}).}

\item{time}{Optional simulation time to use as filename suffix. Default \code{NULL}.}

\item{ext}{The file extension to use (default \code{"rds"}).}
}
\value{
character string giving a file path for a simulation file
}
\description{
Assists with saving and retrieving simulations (e.g., with \code{saveSimList} and \code{loadSimList}).
}
