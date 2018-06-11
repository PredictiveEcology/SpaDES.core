test_that("downloadData downloads and unzips module data", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules")
  datadir <- file.path(tmpdir, m, "data")
  reproducible::checkPath(datadir, create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  filenames <- c("DEM.tif", "habitatQuality.tif")
  Rversion <- numeric_version(paste0(R.version$major, ".", R.version$minor))
  if (Rversion > "3.4.2") { ## TODO: need o test on earlier versions too!
    chksums <- structure(
      list(
        file = structure(1:2, .Label = c("DEM.tif", "habitatQuality.tif"), class = "factor"),
        checksum = structure(1:2, .Label = c("181f66ffb4580194", "f21251dcdf23dde0"), class = "factor")
      ),
      .Names = c("file", "checksum"),
      class = "data.frame", row.names = c(NA, -2L)
    )
    moduleDir <- file.path(tmpdir, "test")
    dataDir <- file.path(moduleDir, "data")
    write.table(chksums, file = file.path(dataDir, "CHECKSUMS.txt") )
    expectsInputs <- data.frame(
      objectName = c("DEM", "habitatQuality"),
      objectClass = "RasterLayer",
      sourceURL = c("https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif",
                    "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/habitatQuality.tif"),
      stringsAsFactors = FALSE
    )

    reproducible::checkPath(dataDir, create = TRUE)

    #f <- downloadModule(m, tmpdir, quiet = TRUE)
    t1 <- system.time(downloadData(m, tmpdir, quiet = FALSE, urls = expectsInputs$sourceURL))
    result <- checksums(m, tmpdir)$result
    expect_true(all(file.exists(file.path(datadir, filenames))))
    expect_true(all(result == "OK"))

    # shouldn't need a redownload because file exists
    t2 <- system.time(downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL))
    expect_true(t1[3] > t2[3]) # compare elapsed times

    # if one file is missing, will fill in correctly
    unlink(file.path(datadir, filenames)[1])
    downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL)
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there, but one is incorrectly named
    file.rename(from = file.path(datadir, filenames[1]),
                to = file.path(datadir, "test.tif"))
    downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL) # renames the file back to expected
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there with correct names, but wrong content
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    if (require(rgdal, quietly = TRUE)) {
      on.exit(detach("package:rgdal"), add = TRUE)
      ras <- raster(file.path(datadir, filenames[2]))
      ras[4] <- maxValue(ras) + 1
      writeRaster(ras, filename = file.path(datadir, filenames[2]), overwrite = TRUE)
      downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL)
      expect_true(all(file.exists(file.path(datadir, filenames))))
    }
  }
})
