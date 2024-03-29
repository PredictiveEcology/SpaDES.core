% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suppliedElsewhere.R
\name{suppliedElsewhere}
\alias{suppliedElsewhere}
\title{Assess whether an object has or will be supplied from elsewhere}
\usage{
suppliedElsewhere(
  object,
  sim,
  where = c("sim", "user", "initEvent"),
  returnWhere = FALSE
)
}
\arguments{
\item{object}{Character vector}

\item{sim}{A \code{simList} in which to evaluated whether the object is supplied elsewhere}

\item{where}{Character vector with one to three of \code{"sim"}, \code{"user"}, or \code{"initEvent"}.
Default is all three. Partial matching is used. See details.}

\item{returnWhere}{Logical, default \code{FALSE}, whether the vector of length
3 logical should be returned, or a logical of length one}
}
\value{
logical
}
\description{
When loading objects into a \code{simList}, especially during the
\code{simInit} call, and inside the \code{.inputObjects} functions of modules,
it is often useful to know if an object in question will or has been
by the user via the \code{inputs} or \code{objects} arguments, or by another
module's \code{.inputObjects} while preparing its expected inputs (via
\code{expectsInputs} in metadata), or if it will be supplied by another
module during its \code{"init"} event. In all these cases, it may not
be necessary for a given module to load any default value for its \code{expectsInputs}.
This function can be used as a check to determine whether the module needs
to proceed in getting and assigning its default value.
}
\details{
\code{where} indicates which of three places to search, either \code{"sim"} i.e.,
the \code{simList}, which would be equivalent to \verb{is.null(sim\\$objName)}, or
\code{"user"} which would be supplied by the user in the \code{simInit} function
call via \code{outputs} or \code{inputs} (equivalent to
\verb{(!('defaultColor' \\\%in\\\% sim$.userSuppliedObjNames))}),
or \code{"initEvent"}, which would test whether a module that gets loaded \strong{before}
the present one \strong{will} create it as part of its outputs (i.e., as indicated by
\code{createsOutputs} in that module's metadata). There is a caveat to this test,
however; if that other event also has the object as an \code{expectsInput}, then
it would fail this test, as it \emph{also} needs it as an input.
This final one (\code{"initEvent"}) does not explicitly test that the object will be created
in the "init" event, only that it is in the outputs of that module, and that it is a module
that is loaded prior to this one.
}
\examples{
mySim <- simInit()
suppliedElsewhere("test", mySim) # FALSE

# supplied in the simList
mySim$test <- 1
suppliedElsewhere("test", mySim) # TRUE
test <- 1

# supplied from user at simInit time -- note, this object would eventually get into the simList
#   but the user supplied values come *after* the module's .inputObjects, so
#   a basic is.null(sim$test) would return TRUE even though the user supplied test
mySim <- simInit(objects = list("test" = test))
suppliedElsewhere("test", mySim) # TRUE

\donttest{
# Example with prepInputs
# Put chunks like this in your .inputObjects
if (!suppliedElsewhere("test", mySim))
  sim$test <- Cache(prepInputs, "raster.tif", "downloadedArchive.zip",
                    destinationPath = dataPath(sim), studyArea = sim$studyArea,
                    rasterToMatch = sim$otherRasterTemplate, overwrite = TRUE)
}
}
