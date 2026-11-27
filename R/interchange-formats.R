#' Interchange formats for exchanging objects with non-R models
#'
#' The on-disk format a non-R process should use to read or write an object of
#' a given `objectClass`, as declared in a module's [expectsInput()] or
#' [createsOutput()] metadata.
#'
#' @details
#'
#' A module's metadata declares an object's class, e.g. `"SpatRaster"`. That
#' tells a module author in R what they will receive, but it says nothing to a
#' model written in Python, Julia or C++ about how to receive it: two modules
#' can agree on an object's name and class and still fail to exchange it,
#' because nothing says whether a raster crosses as a GeoTIFF or a NetCDF, or a
#' table as Parquet or CSV.
#'
#' This registry is that missing agreement, in one place, so that each pair of
#' modules does not have to invent its own. It is plain data rather than code,
#' shipped as `inst/extdata/interchange-formats.csv`, so a non-R process can
#' read it directly without calling R:
#'
#' ```
#' csv.DictReader(l for l in open(path) if not l.startswith("#"))
#' ```
#'
#' The columns are:
#'
#' \tabular{ll}{
#'   `objectClass` \tab The class, as declared in module metadata.\cr
#'   `ext` \tab The file extension to write and read.\cr
#'   `driver` \tab The GDAL driver, where one applies.\cr
#'   `mapped` \tab A zero-copy alternative, for handoffs where the cost of a
#'      copy matters more than universality. An optimisation, not a
#'      replacement for `ext`.\cr
#'   `notes` \tab Constraints worth knowing before relying on the format.\cr
#' }
#'
#' Nothing in SpaDES.core reads or enforces this; it is a convention that
#' module authors on both sides of a language boundary can point at.
#'
#' @param objectClass Optional character vector of classes to look up. If
#'   missing, the whole registry is returned. An unrecognised class returns a
#'   row of `NA`s.
#'
#' @return A `data.frame` with one row per class, and the columns described
#'   above.
#'
#' @export
#' @seealso [expectsInput()], [createsOutput()], [moduleMetadata()]
#'
#' @examples
#' interchangeFormats()                      # the whole registry
#' interchangeFormats("SpatRaster")$ext      # "tif"
interchangeFormats <- function(objectClass) {
  f <- system.file("extdata", "interchange-formats.csv", package = "SpaDES.core")
  reg <- utils::read.csv(f, comment.char = "#", stringsAsFactors = FALSE)
  if (!missing(objectClass)) {
    reg <- reg[match(objectClass, reg$objectClass), , drop = FALSE]
    rownames(reg) <- NULL
  }
  reg
}
