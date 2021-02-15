test_that("downloadData downloads and unzips module data", {
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  testInitOut <- testInit(smcc = FALSE, opts = list(reproducible.inputPaths = NULL))
  on.exit(testOnExit(testInitOut), add = TRUE)

  m <- "test"
  datadir <- file.path(tmpdir, m, "data") %>% checkPath(create = TRUE)

  filenames <- c("DEM.tif", "habitatQuality.tif")
  Rversion <- getRversion()
  if (Rversion > "3.4.2") {
    # write checksums
    chksums <- structure(
      list(
        file = structure(1:2, .Label = c("DEM.tif", "habitatQuality.tif"), class = "factor"),
        checksum = structure(1:2, .Label = c("77c56d42fecac5b1", "f21251dcdf23dde0"), class = "factor")
      ),
      .Names = c("file", "checksum"),
      class = "data.frame", row.names = c(NA, -2L)
    )
    write.table(chksums[order(chksums$file),], file = file.path(datadir, "CHECKSUMS.txt"))

    expectsInputs <- data.frame(
      objectName = c("DEM", "habitatQuality"),
      objectClass = "RasterLayer",
      sourceURL = c(
        "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/DEM.tif",
        "https://raw.githubusercontent.com/PredictiveEcology/quickPlot/master/inst/maps/habitatQuality.tif"
      ),
      stringsAsFactors = FALSE
    )

    a <- capture.output({
      t1 <- system.time(downloadData(m, tmpdir, quiet = FALSE, urls = expectsInputs$sourceURL,
                                     files = c("DEM.tif", "habitatQuality.tif")))
    })
    result <- checksums(m, tmpdir)$result
    expect_true(all(file.exists(file.path(datadir, filenames))))
    expect_true(all(result == "OK"))

    # shouldn't need a redownload because file exists
    a <- capture.output({
      t2 <- system.time(downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL,
                                     files = c("DEM.tif", "habitatQuality.tif")))
    })
    expect_true(t1[3] > t2[3]) # compare elapsed times

    # if one file is missing, will fill in correctly
    unlink(file.path(datadir, filenames)[1])
    a <- capture.output(downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL))
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there, but one is incorrectly named
    file.rename(from = file.path(datadir, filenames[1]),
                to = file.path(datadir, "test.tif"))
    a <- capture.output(downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL)) # renames the file back to expected
    expect_true(all(file.exists(file.path(datadir, filenames))))

    # if files are there with correct names, but wrong content
    library(raster); on.exit(detach("package:raster"), add = TRUE)
    if (require(rgdal, quietly = TRUE)) {
      on.exit(detach("package:rgdal"), add = TRUE)
      ras <- raster(file.path(datadir, filenames[2]))
      ras[5] <- maxValue(ras) + 1
      suppressWarnings({
        ## TODO: remove suppressWarnings after raster package fixes/updates
        writeRaster(ras, filename = file.path(datadir, filenames[2]), overwrite = TRUE)
      })
      a <- capture.output({
        dwnload <- downloadData(m, tmpdir, quiet = TRUE, urls = expectsInputs$sourceURL, overwrite = TRUE, purge = 7)
      })
      expect_true(all(dwnload$result %in% "OK"))
      expect_true(all(file.exists(file.path(datadir, filenames))))
    }
  }
})
