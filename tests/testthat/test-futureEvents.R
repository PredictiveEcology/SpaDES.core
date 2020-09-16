test_that("test futureEvents", {
  if (interactive()) {
    testInitOut <- testInit( smcc = FALSE, libraries = "future",
                            opts = list("reproducible.useMemoise" = FALSE,
                                                      "spades.useFuture" = TRUE))
    tmpdir <- tempdir()
    modPath <- system.file("sampleModules", package = "SpaDES.core")
    newModule("test", path = modPath)
    on.exit(unlink(file.path(modPath, "test"), recursive = TRUE), add = TRUE)
    mods1 <- c("caribouMovement", "fireSpread", "test")
    for (mod in mods1) {
      f1 <- file.path(modPath, mod, paste0(mod, ".R"))
      f2 <- file.path(modPath, mod, paste0(mod, "Orig.R"))
      file.copy(f1, f2)
      ll <- readLines(f1)
      lin <- grep("Burn\\(sim\\)|Move\\(sim\\)|plotFun\\(sim\\)", ll)
      newModCode <- c(ll[seq(lin-1)], "    system.time(for (i in 1:3e6) rnorm(10))",
                      if(mod == "test") "sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, 'test', 'plot')",
                      ll[lin:length(ll)])
      writeLines(newModCode, con = f1)
    }
    on.exit({
      for (mod in mods1) {
        f1 <- file.path(modPath, mod, paste0(mod, ".R"))
        f2 <- file.path(modPath, mod, paste0(mod, "Orig.R"))
        file.copy(f2, f1, overwrite = TRUE)
        unlink(f2)
      }
    },
    add = TRUE)


    future::plan(future::multiprocess(workers = 3))

    mods <- c("caribouMovement", "randomLandscapes", "fireSpread", "test")
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

    mySim@params$test$.plotInitialTime <- 0
    mySim@params$test$.plotInterval <- 1

    options("spades.useFuture" = TRUE)
    set.seed(1)
    simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
    options("spades.useFuture" = FALSE)
    set.seed(1)
    simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
    expect_true(isTRUE(all.equal(completed(simsFALSE), completed(simsTRUE))))

    mySim@depends@dependencies$caribouMovement@timeunit <- "year"
    options("spades.useFuture" = TRUE)
    set.seed(1)
    simsTRUE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)
    options("spades.useFuture" = FALSE)
    set.seed(1)
    simsFALSE <- spades(Copy(mySim), notOlderThan = Sys.time(), debug = TRUE)

    s2 <- completed(simsFALSE)[, 1:3]
    data.table::setorderv(s2, c("eventTime", "moduleName", "eventType"))
    s1 <- completed(simsTRUE)[, 1:3]
    data.table::setorderv(s1, c("eventTime", "moduleName", "eventType"))
    expect_true(isTRUE(all.equal(s2, s1)))
  }

})
