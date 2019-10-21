test_that("experiment2 does not work correctly", {
#  skip("Not ready")
  testInitOut <- testInit(c("raster", "future.callr", "future"), smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)


  # options(mc.cores = 2)
  plan("callr", workers = 2)
  # Example of changing parameter values
  mySim1 <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(10)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  mySim2 <- simInit(
    times = list(start = 0.0, end = 2.0, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(20)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c("landscape", "caribou"),
                         stringsAsFactors = FALSE)
  )

  # Create an experiment - here, 2 x 2 x 2 (2 levels of 2 params in fireSpread,
  #    and 2 levels of 1 param in caribouMovement)
  #caribouNums <- c(100, 1000)
  #experimentParams <- list(
  #  caribouMovement = list(N = caribouNums)
  #)
  # Test Caching
  cap0 <- capture_output(mess <- capture_messages(spades(Copy(mySim1), debug = 2)))
  expect_true(sum(grepl("cached", cap0))==0)
  cap1 <- capture.output(mess <- capture_messages(sims <- experiment2(mySim1, mySim2)))
  expect_true(sum(grepl("cached", cap1))==2) # b/c they are at the same time. If sequential, one would be memoised
  cap <- capture.output(mess <- capture_messages(sims <- experiment2(mySim1, mySim2,
                                                                     mySim1)))
  expect_true(sum(grepl("cached", cap))==2) # these are not same session as previous, so can't memoise
  expect_true(sum(grepl("memoised", cap))==1) # 2 were old, plus 1 was a redo in one of the workers


  # Test replication
  mySim1Orig <- Copy(mySim1)
  mySim2Orig <- Copy(mySim2)

  cap1 <- capture.output(mess <- capture_messages(
    sims <- experiment2(mySim1, mySim2, replicates = c(2,3))
  ))
  # Test don't need to use Copy
  expect_true(isTRUE(all.equal(mySim1Orig, mySim1))) # can't use identical -- envs are different

  # Test replication -- can be a vector of replicates
  expect_true(length(ls(sims)) == 5)
  expect_true(sum(grepl("^1", sort(ls(sims)))) == 2)
  expect_true(sum(grepl("^2", sort(ls(sims)))) == 3)
  expect_true(sum(grepl("rep1$", sort(ls(sims)))) == 2)
  expect_true(sum(grepl("rep2$", sort(ls(sims)))) == 2)
  expect_false(identical(sims$`1_rep1`$caribou$x1, sims$`1_rep2`$caribou$x1))
  expect_false(identical(sims$`1_rep1`$caribou$x1, sims$`2_rep2`$caribou$x1))
  expect_false(identical(sims$`1_rep1`$caribou$x1, sims$`2_rep1`$caribou$x1))



})
#
#   expt <- load(file.path(tmpdir, "experiment.RData")) %>% get() # Loads an object named experiment
#   exptDesign <- expt$expDesign
#   exptVals <- expt$expVals
#
#   expect_equal(NROW(exptDesign), 4)
#   expect_equal(exptVals[exptVals$module == "caribouMovement", "val"] %>% unlist(),
#                c(rep(caribouNums, each = 2)))
#   expect_equal(exptVals$modules %>% unique(),
#                "randomLandscapes,fireSpread,caribouMovement")
#   expect_equal(NROW(attr(sims, "experiment")$expDesign), NROW(exptDesign))
#
#   # test that experimental design object is indeed what is in the sims object
#   mods <- sapply(strsplit(names(exptDesign)[-(4:5)], split = "\\."), function(x) x[[1]])
#   params <- sapply(strsplit(names(exptDesign)[-(4:5)], split = "\\."), function(x) x[[2]])
#   out2 <- lapply(seq_along(mods), function(y) {
#     out <- lapply(seq_len(NROW(exptDesign)), function(x) {
#       expect_equivalent(0, params(sims[[x]])[[mods[y]]][[params[[y]]]] -
#                           exptVals %>% dplyr::filter(module == mods[[y]] &
#                                                        param == params[[y]] &
#                                                        expLevel == x) %>%
#                           dplyr::select(val) %>% unlist())
#     })
#   })
#
#   sims <- experiment(mySimFull, replicates = 3)
#   expt <- load(file.path(tmpdir, "experiment.RData")) %>% get() # Loads an object named experiment
#   exptDesign <- expt$expDesign
#   exptVals <- expt$expVals
#   out <- lapply(seq_along(sims), function(x) {
#     expect_equal(outputs(sims[[x]])$saved, c(TRUE, TRUE))
#     expect_equal(
#       outputs(sims[[x]])$file,
#       file.path(normPath(tmpdir), paste0("rep", x),
#                 paste0(c("landscape", "caribou"), "_year2.rds")) %>%
#         normPath()
#     )
#   })
#
#   ### Test inputs - first, have to make the input map
#   mySimRL <- simInit(
#     times = list(start = 0.0, end = 0.1, timeunit = "year"),
#     params = list(
#       .globals = list(stackName = "landscape"),
#       # Turn off interactive plotting
#       randomLandscapes = list(.plotInitialTime = NA)
#     ),
#     modules = list("randomLandscapes"),
#     paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#                  outputPath = file.path(tmpdir, "landscapeMaps1")),
#     outputs = data.frame(objectName = "landscape", saveTime = 0, stringsAsFactors = FALSE)
#   )
#   sims2 <- experiment(mySimRL, replicate = 2)
#
#   mySimNoRL <- simInit(
#     times = list(start = 0.0, end = 2.0, timeunit = "year"),
#     params = list(
#       .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
#       # Turn off interactive plotting
#       fireSpread = list(.plotInitialTime = NA),
#       caribouMovement = list(.plotInitialTime = NA)
#     ),
#     modules = list("fireSpread", "caribouMovement"),
#     paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
#                  outputPath = tmpdir),
#     # Save final state of landscape and caribou
#     outputs = data.frame(objectName = c("landscape", "caribou"), stringsAsFactors = FALSE)
#   )
#   landscapeFiles <- dir(outputPath(mySimRL), pattern = "landscape_year0", recursive = TRUE,
#                         full.names = TRUE)
#   set.seed(1232)
#   sims <- experiment(mySimNoRL, replicates = 2,
#                      inputs = lapply(landscapeFiles, function(filenames) {
#                        data.frame(file = filenames, loadTime = 0,
#                                   objectName = "landscape", stringsAsFactors = FALSE)
#                      })
#   )
#
#   # Make sure these are using the same, identical input maps
#   expect_true(identical(sims[[1]]$landscape$habitatQuality, sims[[3]]$landscape$habitatQuality))
#   expect_true(identical(sims[[2]]$landscape$habitatQuality, sims[[4]]$landscape$habitatQuality))
#   # Make sure there are two different input maps (i.e,. the inverse of the above test)
#   expect_false(identical(sims[[2]]$landscape$habitatQuality, sims[[3]]$landscape$habitatQuality))
#
#   # Make sure random number generator is working. These start with the same maps, but should end up different
#   expect_false(identical(sims[[2]]$landscape$Fires, sims[[4]]$landscape$Fires))
#   expect_false(identical(sims[[1]]$landscape$Fires, sims[[3]]$landscape$Fires))
#
#   # Test clearSimEnv argument... i.e., clearing of the final objects
#   expect_equal(length(ls(sims[[1]])), 5)
#   set.seed(1232)
#   sims2 <- experiment(mySimNoRL, replicates = 2, clearSimEnv = TRUE,
#                       inputs = lapply(landscapeFiles, function(filenames) {
#                         data.frame(file = filenames, loadTime = 0,
#                                    objectName = "landscape", stringsAsFactors = FALSE)
#                       })
#   )
#   # This version has no objects
#   expect_equal(length(ls(sims2[[1]])), 0)
#
#   # Test that the only difference is their objects, which we can pass back in manually
#   list2env(mget(ls(sims[[1]]), envir = envir(sims[[1]])), envir = envir(sims2[[1]]))
#   expect_equal(sims[[1]], sims2[[1]])
#
#   # Test object passing in
#   experimentObj <- list(landscape = lapply(landscapeFiles, readRDS) %>%
#                           setNames(paste0("landscape", 1:2)))
#   # Pass in this list of landscape objects
#   set.seed(1232)
#   sims3 <- experiment(mySimNoRL, objects = experimentObj)
#   # Compare simulations that had objects read from disk with objects passed via objects arg
#   expect_equal(sims3[[1]]$landscape, sims[[1]]$landscape)
#   expect_equal(sims3[[2]]$landscape, sims[[2]]$landscape)
# })
#
