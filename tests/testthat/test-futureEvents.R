test_that("test futureEvents", {
  skip("Future testing is ")
  #testInitOut <- testInit(smcc = FALSE, opts = list("reproducible.useMemoise" = FALSE,
  #                                                  "spades.useFuture" = TRUE))
  tmpdir <- tempdir()
  #on.exit({
  #  testOnExit(testInitOut)
  #}, add = TRUE)

  modPath <- system.file("sampleModules", package = "SpaDES.core")

  mods <- c("caribouMovement", "randomLandscapes", "fireSpread")
  # Example of changing parameter values
  mySim <- simInit(
    times = list(start = 0.0, end = 3.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init", .showSimilar = TRUE)
    ),
    modules = mods,
    paths = list(modulePath = modPath,
                 outputPath = tmpdir,
                 cachePath = tmpdir),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

#  mySim@depends@dependencies$caribouMovement@timeunit <- "year"

  sims <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = FALSE)

})
