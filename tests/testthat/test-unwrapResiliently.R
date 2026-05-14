## Tests for .unwrapResiliently — load-side mirror of .wrapResiliently.
## A file-backed object whose backing file is missing at load time must
## be replaced with NULL + a warning, not abort the entire load.

test_that(".unwrapResiliently NULLs unwrappable objects with warning", {
  skip_if_not_installed("terra")
  skip_if_not_installed("reproducible")

  simPaths <- list(cachePath = tempdir(), inputPath = tempdir(),
                   outputPath = tempdir(), modulePath = tempdir(),
                   scratchPath = tempdir(), terraPath = tempdir())

  tf <- tempfile(fileext = ".tif")
  r  <- terra::rast(matrix(1:9, nrow = 3))
  terra::writeRaster(r, tf, overwrite = TRUE)
  rOnDisk <- terra::rast(tf)

  wrapped <- reproducible::.wrap(rOnDisk, cachePath = NULL, paths = simPaths)
  skip_if(is.null(attr(wrapped, "tags")),
          "reproducible::.wrap did not produce tagged output")

  sim <- new("simList")
  sim@.xData[["myRast"]] <- wrapped

  unlink(tf)  # cause load-side .unwrap to fail

  expect_warning(
    out <- SpaDES.core:::.unwrapResiliently(sim, simPaths = simPaths),
    "could not unwrap 'myRast'"
  )
  expect_null(out@.xData[["myRast"]])
})

test_that(".unwrapResiliently leaves unwrappable-free objects alone", {
  sim <- new("simList")
  sim@.xData[["plain"]] <- 42L          # no tags -> skip
  sim@.xData[["str"]]   <- "hello"      # no tags -> skip

  out <- SpaDES.core:::.unwrapResiliently(sim, simPaths = list())
  expect_equal(out@.xData[["plain"]], 42L)
  expect_equal(out@.xData[["str"]], "hello")
})
