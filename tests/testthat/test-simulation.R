test_that("simulation runs with simInit and spades", {
  library(igraph); on.exit(detach("package:igraph"), add = TRUE)

  set.seed(42)

  times <- list(start = 0.0, end = 10, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  mySim <- simInit(times, params, modules, objects = list(), paths) %>%
    spades(debug = FALSE)

  # simtime
  expect_equivalent(time(mySim), 10.0)
  expect_equivalent(start(mySim), 0.0)
  expect_equivalent(end(mySim), 10.0)

  # sim results ## NOTE upcoming version of RandomFields completely changes the values!!!
  #if (utils::packageVersion("RandomFields") >= "3.1.20") {
    burnedLast <- c(1725, 126, 816, 2136, 1836, 825, 1381, 1507, 1509, 1624)

    pos_x <- c(1.05944046146596, 44.4530163547364, 45.8793618542196,
               -1.31587161139159, 17.8640418602947, 48.4840937544703, 10.7535089382283,
               -24.6253107925863, -48.5108426726095, -7.76764412474417, 34.7851382149642,
               9.83572522829692, 41.9505922251921, -9.07555369460079, -39.676662136738,
               21.6185984474158, 46.4701522375152, -0.883315154385187, 28.0911201823354,
               -13.847369533562, -49.7727146829549, -16.2936810258016, -33.9335427084662,
               -8.47926857699624, -17.5993028014645, -33.0461085829127, 24.8750808532409,
               -43.5077879392023, -16.9752324398488, 16.7694485345211, -8.91563038316473,
               -48.3650161814635, 27.0749755681643, 34.0994637648402, -49.1648592745971,
               -29.9712621173336, 49.5667313041262, 21.3612681252904, -21.3445953954488,
               24.8211683871312, 23.4173005016891, -29.9699855919438, -49.958594066106,
               -17.825402853539, 30.0307948992073, 29.6380142083718, 49.6506417101735,
               -44.1750870723644, -32.8472885698896, 12.7607439556149, -15.7741569225653,
               -41.5405111886502, -34.8179542704289, -38.3885023832212, 4.60137973293355,
               -22.6440580206797, -0.213427106250919, -2.29987838737896, -21.081170253662,
               38.454598692853, 15.1416694346051, -43.728082307768, -13.0723350036922,
               28.0846481560228, -27.3096141757915, 45.7054711281168, 9.16013996497286,
               9.62565605939507, -16.0461041502597, -35.5546251289444, -24.9716699563686,
               49.8465322556856, 10.8257847829093, -34.2753709391232, -10.4856247161767,
               48.8397137324941, 16.7101178052949, -35.4992391966538, -49.0238384344422,
               47.597755712123, -20.1128595601079, 44.2571096499243, -34.507735468028,
               35.4557823272802, 49.5489237085567, 31.1401253978472, 25.3007340967458,
               6.45953149953921, -46.7962501020211, 1.03386467153074, -24.1707710493782,
               35.2616940703965, -1.12871674368546, 46.1499998989781, 30.4491021819093,
               -26.5440703974598, -39.6645892713107, -28.1460804729089, -41.6606455063026,
               -7.39377404125834)

    pos_y <- c(-15.8993972461616, -8.96685346602316, 25.2653183350009,
               -39.0714808439679, -32.9641113081288, 19.5922174069934, 14.8674025804159,
               38.2616003081126, -8.16583605869974, 17.8252525894824, 8.21593603120944,
               -36.3379915713033, 29.7329787318616, 42.0827238488804, -20.2386772454794,
               35.7556518838588, 5.55152952583172, 27.7482179433487, 37.4124616940735,
               -42.4068608457502, -22.1109081515469, 10.2089709119964, -3.45245440515526,
               -12.0062733527654, 5.19230996990487, -27.1646572653389, -26.8632992341179,
               12.4455696770408, 39.5029725546056, -40.8904798063864, 13.4903138848752,
               -9.33150580154056, 12.266246498016, 27.8095419155339, 8.94181503604101,
               24.4028888129788, -44.3392208269988, 36.7910379389471, -45.1228359607758,
               -35.9317653139032, 6.31296435991845, 20.9073748145245, -16.3575394819696,
               22.4740052505691, 36.7314942719821, -3.77070330422793, -30.2931909279016,
               16.583785813993, 38.7946206773652, -12.6087368305901, -3.99514927265182,
               15.0189478879167, 27.1011443151554, -19.4368853267691, -6.71379469116036,
               18.2862630593746, -40.2802146105095, -15.6248800198687, 32.2775346771305,
               31.4803155819967, 45.0553762113152, 25.7675846881672, -49.8075671509248,
               -6.86962262435439, 14.4530466734969, -5.96936453527461, 3.86969432620594,
               -35.3152672431326, -15.5041801097397, -36.2079856152726, -10.8110700378683,
               -48.6952585997799, -30.7297095345783, -42.2974065446223, 7.53790808198442,
               -9.89011549737592, 30.0073720455366, 48.1503933132591, -32.8646234493861,
               -46.9746807507946, -21.8590265307828, -2.9748266389963, 2.86405420190562,
               -43.7889401248308, 10.0365696344108, 44.4795579080847, -48.3616605982961,
               1.2228017579147, 4.04249242242624, 33.1075803768298, 4.64441718295845,
               -47.8216259071557, 33.265393363792, 40.0764181461184, 42.4561064648432,
               20.6604237095649, -41.4412266891093, 9.96154914243285, 33.2776014337331,
               12.9988550096717)
  # } else {
  #   burnedLast <- c(1680, 1485, 607, 1079, 1041, 605, 871, 1097, 495, 1253)
  #
  #   pos_x <- c(
  #     -36.9964594878183, 2.32458656754471, 30.12442850383, -18.6640229184998,
  #     -33.8885207519506, -29.5427930908878, -21.0095214973779, -6.42587067738999,
  #     -5.07663946918247, 1.51100547592112, 40.1425992497513, -40.0937640741396,
  #     8.4223912961982, -28.2165542018589, -23.0437975158775, 18.294604333076,
  #     -16.7308043319382, -8.47854059870558, 39.0343113708618, -39.3309621495378,
  #     -3.85775142502581, 49.0817194947727, -17.9722065247698, 4.55173210595375,
  #     -41.0519469414064, 34.8930158926542, -32.589908783801, -40.5387963961559,
  #     34.2194237750273, -33.5059996554471, 41.7214409962314, -33.4975937591762,
  #     -16.7534188285129, -3.15227992913145, 23.6589792431533, 28.0041852235432,
  #     43.3963008307616, 15.6253468583625, -24.338203383424, 38.3698431106254,
  #     -8.95270144561675, 33.179905218285, -32.412733860245, 35.8000608503506,
  #     32.1821466587538, -5.44818140354121, 34.3391843159336, -1.11939974258066,
  #     -29.9411578108715, -8.4957082608383, 38.4884378359319, -44.6014019956853,
  #     9.52576850503965, -29.621391887535, -3.42226265233396, -48.0427755267367,
  #     -48.0453925992576, 31.3356691703378, 39.4370108644243, -23.0450600175721,
  #     -29.9676867807934, -14.984320542406, 13.0752058955144, 11.8531604473534,
  #     -24.9697318775997, -25.3771063203708, 46.0558653502021, 27.1583913421414,
  #     47.0730137699115, -43.2589915711839, 34.287440364578, -16.462781379318,
  #     13.8025952614028, 7.98145114328579, 12.6617467700004, 8.6529212125842,
  #     42.9266926532228, -29.6405295858366, 35.0331975539472, 22.782823127803,
  #     -33.6543726032234, -9.20996524772551, 6.86344094726754, -42.0192917407262,
  #     41.097913592971, 9.11335978963641, 35.7427369785196, 1.39434498353882,
  #     25.6514918771879, -25.7249927901813, 46.5803504144238, -46.9702484362805,
  #     30.3177776221904, 11.1446124689632, 46.5656200410542, 42.6072217133873,
  #     -26.5944387764709, 30.3706355061775, -39.2953280003378, -6.61414387806415
#   )
#
  #   pos_y <- c(
  #     11.2259177001642, -48.2749285467045, -19.2693150119305, -30.1955629122566,
  #     -5.16274098908813, 19.7263653977621, -18.3682115225133, 11.738367415035,
  #     38.9967732966357, 2.71795967614925, -44.7225009655404, -32.6275290676451,
  #     -14.7686749801842, 25.2266058813999, 43.367709252435, 40.625866000042,
  #     22.0714768248821, -24.1975134747756, -24.2623383782744, -12.5286480447744,
  #     48.122230154392, -44.8308278925359, 37.0642433839201, -15.6006743461834,
  #     -28.265637676497, 8.5281444890346, 37.3382373812721, 30.2225238433243,
  #     -26.9394661142134, 29.4716759594875, -43.5670094907836, -47.8084247612384,
  #     -48.1749534451242, -45.1487680482165, -30.8568223487393, -39.0221779181183,
  #     -15.806612632621, 9.37725216531152, -40.2347020337625, 19.3362330671687,
  #     10.0653991235251, -24.599586469326, -35.8913143320209, 35.6277170387067,
  #     35.5608652339012, -36.7887704649442, -5.75447090913782, 1.01447582952702,
  #     12.9836719196659, -18.5773516046411, -46.6496786531966, -22.4229092556123,
  #     35.9359685267436, 45.2300691122058, 25.6432668008571, 41.3834115900633,
  #     -13.4068360644368, -26.1410488438946, -47.4071373813623, -32.2387795954974,
  #     -35.7842792759393, -7.70426791095162, 36.0018667789867, 30.1562830869532,
  #     41.6895906080711, 47.8520336608373, 3.57737369062482, -39.6190773865091,
  #     5.37402201117737, -37.3107079155598, -34.1742440396536, -19.2538176414933,
  #     -48.6308373737092, -26.392445100473, -41.039612082825, -46.6119580572095,
  #     -16.844154891972, -26.0741751607543, -31.4374762519959, -6.44047912640615,
  #     44.8723281786276, -43.6527354527293, 37.8112535875202, -22.5498526193275,
  #     25.9669403382768, 1.16676526552681, -30.6497237706147, 32.5573215765397,
  #     15.4977777956788, 20.7609180017956, 31.0985715416653, -34.7622261504206,
  #     18.6274139594293, 43.0727309557879, -18.0134694585456, 39.5790564364162,
  #     12.5097325118235, -31.4932896470479, 37.7529892755605, -30.960625150814
#   )
#   }

  expect_equal(mySim$npixelsburned, burnedLast)
  expect_equivalent(mySim$caribou$x, pos_x)
  expect_equivalent(mySim$caribou$y, pos_y)
})

test_that("spades calls with different signatures don't work", {
  library(igraph)
  library(reproducible)

  tmpdir <- file.path(tempdir(), "test_Plot1") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  a <- simInit()
  a1 <- Copy(a)
  expect_output(spades(a, debug = TRUE), "eventTime")
  expect_silent(spades(a, debug = FALSE))
  expect_silent(spades(a, debug = FALSE, .plotInitialTime = NA))
  expect_silent(spades(a, debug = FALSE, .saveInitialTime = NA))
  expect_output(spades(a, debug = TRUE, .plotInitialTime = NA), "eventTime")
  expect_output(spades(a, debug = TRUE, .saveInitialTime = NA), "eventTime")
  expect_equivalent(capture_output(spades(a, debug = "current", .plotInitialTime = NA)),
                    capture_output(spades(a, debug = TRUE, .plotInitialTime = NA)))

  expect_output(spades(a, debug = c("current", "events"), .plotInitialTime = NA),
                "This is the current event")
  expect_output(spades(a, debug = c("current", "events"), .plotInitialTime = NA),
                "moduleName")
  expect_output(spades(a, debug = "simList", .plotInitialTime = NA),
                "Completed Events")

  if (interactive()) {
    expect_output(spades(a, progress = "text", debug = TRUE), "10%")
    expect_output(spades(a, progress = "text", debug = TRUE), "20%")
    expect_output(spades(a, progress = "text"), "..........| 100%")
  }
  expect_silent(spades(a, debug = FALSE, progress = FALSE))
  expect_silent(spades(a, debug = FALSE, progress = "rr"))

  paths(a)$cachePath <- file.path(tempdir(), "cache") %>% checkPath(create = TRUE)
  a <- Copy(a1)
  expect_output(spades(a, cache = TRUE, debug = TRUE, notOlderThan = Sys.time()), "eventTime")
  expect_true(all(dir(paths(a)$cachePath) == c("backpack.db", "gallery")))
  file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))

  # test for system time ... in this case, the first time through loop is slow
  #   because of writing cache to disk, not because of spades being slow.
  #   SimList is empty.

  set.seed(42)

  times <- list(start = 0.0, end = 0, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(nx = 20, ny = 20)
  )
  modules <- list("randomLandscapes", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  for (i in 1:2) {
    a <- simInit(times, params, modules, paths = paths)
    paths(a)$cachePath <- file.path(tempdir(), "cache") %>% checkPath(create = TRUE)
    assign(paste0("st", i), system.time(spades(a, cache = TRUE, .plotInitialTime = NA)))
  }
  expect_gt(st1[1], st2[1])
  file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))
})

test_that("simInit with R subfolder scripts", {
  library(igraph)
  library(reproducible)

  tmpdir <- file.path(tempdir(), "test_timeunits") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  newModule("child1", ".", open = FALSE)
  cat(file = file.path("child1", "R", "script.R"),
      "a <- function(poiuoiu) {
          poiuoiu + 1
      }", sep = "\n")
  mySim <- simInit(modules = "child1", paths = list(modulePath = tmpdir))
  expect_true(sum(grepl(unlist(lapply(ls(mySim@.envir, all.names = TRUE), function(x) {
    if (is.environment(mySim@.envir[[x]])) ls(envir = mySim@.envir[[x]], all.names = TRUE)
  })), pattern = "^a$")) == 1)
  expect_true(mySim@.envir$child1$a(2) == 3) # Fns
})

test_that("simulation runs with simInit with duplicate modules named", {
  library(igraph); on.exit(detach("package:igraph"), add = TRUE)

  set.seed(42)

  times <- list(start = 0.0, end = 10, timeunit = "year")
  params <- list(
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE)
  )
  modules <- list("randomLandscapes", "randomLandscapes", "caribouMovement")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))

  expect_true(any(grepl(capture_messages(
    mySim <- simInit(times, params, modules, objects = list(), paths)
  ), pattern = "Duplicate module")))
  expect_true(length(modules(mySim)) != length(modules))
  expect_true(length(modules(mySim)) == length(unique(modules)))
})


test_that("simulation runs with simInit with duplicate modules named", {
  skip("benchmarking DES")

  library(dplyr)

  tmpdir <- file.path(tempdir(), "testBenchmarking")
  checkPath(tmpdir, create = TRUE)
  setwd(tmpdir)
  on.exit(unlink(tmpdir, force = TRUE, recursive = TRUE))
  newModule("test", tmpdir, open = FALSE)

  sim <- simInit()

  cat(file = file.path(tmpdir, "test", "test.R"),'
  defineModule(sim, list(
    name = "test",
    description = "insert module description here",
    keywords = c("insert key words here"),
    authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    childModules = character(0),
    version = list(SpaDES.core = "0.1.0", test = "0.0.1"),
    spatialExtent = raster::extent(rep(NA_real_, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list("citation.bib"),
    documentation = list("README.txt", "test.Rmd"),
    reqdPkgs = list(),
    parameters = rbind(
    ),
    inputObjects = bind_rows(
    ),
    outputObjects = bind_rows(
    )
  ))

  doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
    switch(
      eventType,
      init = {
        sim <- scheduleEvent(sim, time(sim)+1, "test", "event1")
      },
      event1 = {
        sim <- scheduleEvent(sim, time(sim)+1, "test", "event1")
      })
    return(invisible(sim))
  }
  ', fill = TRUE)

  library(igraph)

  moduleDir <- file.path(tmpdir)
  inputDir <- file.path(moduleDir, "inputs") %>% reproducible::checkPath(create = TRUE)
  outputDir <- file.path(moduleDir, "outputs")
  cacheDir <- file.path(outputDir, "cache")
  times <- list(start = 0, end = 5000)
  parameters <- list(
  )
  modules <- list("test")
  objects <- list()
  paths <- list(
    cachePath = cacheDir,
    modulePath = moduleDir,
    inputPath = inputDir,
    outputPath = outputDir
  )

  #options("spades.nCompleted" = 500)
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   objects = objects, paths = paths)


  # was 10.2 seconds -- currently 4.2 seconds or so --> June 29, 2018 is 1.25 seconds
  #system.time({spades(mySim, debug = FALSE)})
  options("spades.keepCompleted" = TRUE)
  microbenchmark::microbenchmark(times = 10, {spades(mySim, debug = FALSE)})
  options("spades.keepCompleted" = FALSE)
  microbenchmark::microbenchmark(times = 10, {spades(mySim, debug = FALSE)})
  #profvis::profvis({spades(mySim, debug = FALSE)})
})


test_that("conflicting function types", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_conflictingFns") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  m <- "child4"
  newModule(m, tmpdir, open = FALSE)
  fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
  xxx <- readLines(fileName)
  lineWithInit <- grep(xxx, pattern = "^Init")


  xxx1 <- gsub(xxx, pattern = 'plotFun', replacement = 'Plot') # nolint
  cat(xxx1, file = fileName, sep = "\n")
  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 "Plot is defined")

  # do functions like raster::levels
  cat(xxx[1:lineWithInit], "
      library(raster)
      poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
      poiuoiu <- poiuoiu
      poiuoiu <- scale(poiuoiu)
      poiuoiu <- ratify(poiuoiu)
      rat <- raster::levels(poiuoiu)[[1]]

      levels(poiuoiu) <- rat
      ",
              xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))

  fullMessage <- c("the following function\\(s\\) is used that",
                   "raster::scale", "scale")
  expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
  nonMessage <- c("raster::levels", "levels")
  expect_false(all(unlist(lapply(nonMessage, function(x) any(grepl(mm, pattern = x))))))

  cat(xxx[1:lineWithInit], "
      library(raster)
      poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
      poiuoiu <- scale(poiuoiu)
      ",
      xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 "raster::scale")

  ###
  cat(xxx[1:lineWithInit], "
      library(raster)
      poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
      poiuoiu <- raster::scale(poiuoiu)
      sim$poiuoiu <- poiuoiu
      ",
      xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 "poiuoiu is assigned")

  cat(xxx[1:(lineWithInit - 1)], "
      a <- function(x) {
         b <- b + 1
      }
      ",
      xxx[(lineWithInit):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 "a: parameter")

  xxx1 <- gsub(xxx, pattern = "\\.plotInitialTime", replacement = "value")
  xxx1 <- gsub(xxx1, pattern = "NA, NA, NA", replacement = "'hi', NA, NA")

  cat(xxx1[1:lineWithInit], "
      a <- sim$b
      d <- sim$d
      f <- sim[['f']]
      f <- sim[[P(sim)$value]]
      poiuoiu <- sim@.envir$d1
      qwerqwer <- sim@.envir[['test']]
      sim$g <- f
      sim@.envir$g1 <- f
      return(list(a, d, f, sim))
      ",
      xxx1[(lineWithInit+1):length(xxx1)], sep = "\n", fill = FALSE, file = fileName)

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))

  fullMessage <- c("defineParameter: 'value' is not of specified type 'numeric'",
                   "defineParameter: 'plotInterval' is not of specified type 'numeric'",
                   "defineParameter: 'saveInitialTime' is not of specified type 'numeric'",
                   "defineParameter: 'saveInterval' is not of specified type 'numeric'",
                   "child4: module code: Init: local variable.*qwerqwer.*assigned but may not be used",
                   "Running inputObjects for child4", "child4: module code: Init: local variable.*poiuoiu.*assigned but may not be used",
                   "child4: outputObjects: g, g1 are assigned to sim inside Init, but are not declared in outputObjects",
                   "child4: inputObjects: b, d, f, hi, d1, test are used from sim inside Init, but are not declared in inputObjects"
  )

  mm <- cleanMessage(mm)
  expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
  # cat(paste("################################################"), file = tempfile(), append = FALSE)
  # for (x in seq(fullMessage)) {
  #   lineNum <- "444"
  #   theGrepEach <- grepl(mm, pattern = fullMessage[x])
  #   theGrep <- any(theGrepEach)
  #   if (!theGrep) {
  #     cat(paste("\nline ", lineNum, theGrep, fullMessage[x], "\n              ", paste(mm, collapse = "\n               "), collapse = ""), file = tempfile(), append = TRUE)
  #   }
  #   expect_true(theGrep)
  # }

  cat(xxx[1:lineWithInit], "
      sim$child4 <- 1
      ",
      xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_error(simInit(paths = list(modulePath = tmpdir), modules = m),
               c(paste0(m, ": You have created an object")))

  # declared in inputObjects
  lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  cat(xxx[1:(lineWithInputObjects-1)], "
      expectsInput('a', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
               c(paste0(m, ": module code: a is declared in inputObjects")))

  # declared in outputObjects
  lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  cat(xxx[1:(lineWithOutputObjects-1)], "
      createsOutput('b', 'numeric', '')
      ",
      xxx[(lineWithOutputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
               c(paste0(m, ": module code: b is declared in outputObjects")))

  cat(xxx[1:(lineWithInputObjects-1)], "
      expectsInput('a', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):(lineWithOutputObjects-1)],
      "
      createsOutput('b', 'numeric', '')
      ",
      xxx[(lineWithOutputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  expect_true(all(grepl(mm,
    pattern = c(paste0(m, ": module code: b is declared in outputObjects|child4: module code: a is declared in inputObjects|Running .input")))))

  # assign to sim for functions like scheduleEvent
  lineWithScheduleEvent <- grep(xxx, pattern = "scheduleEvent")[1]
  xxx1 <- xxx
  xxx1[lineWithScheduleEvent] <- sub(xxx[lineWithScheduleEvent], pattern = "sim <- scheduleEvent", replacement = "scheduleEvent")
  cat(xxx1, sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: scheduleEvent inside doEvent.child4 must")))

  # Return sim in doEvent
  patt <- "return\\(invisible\\(sim\\)\\)"
  lineWithReturnSim <- grep(xxx, pattern = patt)[1]
  xxx1 <- xxx
  xxx1[lineWithReturnSim] <- sub(xxx[lineWithReturnSim], pattern = patt,
                                     replacement = "return(invisible())")
  cat(xxx1, sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: doEvent.",m," must return")))


  lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  lineWithDotInputObjects <- grep(xxx, pattern = "\\.inputObjects")
  cat(xxx[1:(lineWithInputObjects-1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei2', 'numeric', '', ''),
      expectsInput('ei3', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):(lineWithOutputObjects-1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co2', 'numeric', ''),
      createsOutput('co3', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx[(lineWithOutputObjects+1):lineWithInit], "
      a <- sim$b
      sim$g <- f
      holy(sim$co4) <- f
      moly(sim$aaa) <- f
      fff <- sim$ei2
      fff <- sim$co3
      sim$co1 <- 123
      xx <- c(1,2)
      xx[sim$ei4] <- NA
      ",
      xxx[(lineWithInit+1):lineWithDotInputObjects], "
      a <- sim$b
      sim$g <- 1
      sim$ei1 <- 4
      fff <- sim$ei1
      fff <- sim$co3
      sim$co1 <- 123
      aaa <- sim$.userSuppliedObjNames # in the ignoreObjects
      ",
      xxx[(lineWithDotInputObjects+1):length(xxx)],
      sep = "\n", fill = FALSE, file = fileName)

  fullMessage <- c("Running inputObjects for child4", "child4: module code: co2, co3 are declared in outputObjects, but are not assigned in the module",
                   "child4: module code: ei2, ei3, ei4 are declared in inputObjects, but no default\\(s\\) are provided in inputObjects",
                   "child4: module code: ei3 is declared in inputObjects, but is not used in the module",
                   "child4: module code: inputObjects: local variable.*a.*assigned but may not be used",
                   "child4: module code: inputObjects: local variable.*fff.*assigned but may not be used",
                   "child4: module code: Init: local variable.*a.*assigned but may not be used",
                   "child4: module code: Init: local variable.*fff.*assigned but may not be used",
                   "child4: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in outputObjects",
                   "child4: inputObjects: g, co1 are assigned to sim inside inputObjects, but are not declared in inputObjects",
                   "child4: inputObjects: b, aaa are used from sim inside Init, but are not declared in inputObjects",
                   "child4: inputObjects: b, co3 are used from sim inside inputObjects, but are not declared in inputObjects"
  )

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  mm <- cleanMessage(mm)
  expect_true(all(unlist(lapply(fullMessage, function(x) any(grepl(mm, pattern = x))))))
  # for (x in seq(fullMessage)) {
  #   lineNum <- "566"
  #   theGrepEach <- grepl(mm, pattern = fullMessage[x])
  #   theGrep <- any(theGrepEach)
  #   if (!theGrep) {
  #     cat(paste("\nline ", lineNum, theGrep, fullMessage[x], "\n              ", paste(mm, collapse = "\n               "), collapse = ""), file = tempfile(), append = TRUE)
  #   }
  #   expect_true(theGrep)
  # }


})


test_that("scheduleEvent with NA logical in a non-standard parameter", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_conflictingFns") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  m <- "test"
  newModule(m, tmpdir, open = FALSE)
  fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
  xxx <- readLines(fileName)
  #lineWithInit <- grep(xxx, pattern = "^Init")

  xxx1 <- gsub(xxx, pattern = '.plotInitialTime', replacement = '.plotInitialTim') # nolint
  xxx2 <- gsub(",$", grep(".plotInitialTim\\>", xxx1, value = TRUE)[1], replacement = "")
  xxx3 <- parse(text = xxx2)
  # show that it is logical
  expect_true(is.logical(eval(xxx3)$default[[1]]))

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  expect_true(all(unlist(lapply(c("Running .inputObjects", "module code appears clean"),
                                function(x) any(grepl(mm, pattern = x))))))

})

test_that("messaging with multiple modules", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_conflictingFns") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  m1 <- "test"
  m2 <- "test2"
  m3 <- "test3"
  m4 <- "test4"
  m <- c(m1, m2, m3, m4)
  newModule(m1, tmpdir, open = FALSE)
  newModule(m2, tmpdir, open = FALSE)
  newModule(m3, tmpdir, open = FALSE)
  newModule(m4, tmpdir, open = FALSE)
  #lapply(m, newModule, tmpdir, open = FALSE)
  fileNames <- file.path(dir(tmpdir), paste0(dir(tmpdir),".R"))
  xxx <- lapply(fileNames, readLines)
  set.seed(113)


  lineWithInit <- grep(xxx[[1]], pattern = "^Init")
  lineWithInputObjects <- grep(xxx[[1]], pattern = " expectsInput")
  lineWithOutputObjects <- grep(xxx[[1]], pattern = " createsOutput")
  lineWithDotInputObjects <- grep(xxx[[1]], pattern = "\\.inputObjects")

  xxx1 <- list()
  #lapply(seq(m), function(yy) sample(c("character", "numeric", "logical"), size = 3, replace = TRUE))
  xxx1[[1]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
                   "\\.plotInitialTime\", \"character\", 1", xxx[[1]])
  xxx1[[1]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
                   "\\.saveInitialTime\", \"character\", FALSE", xxx1[[1]])
  xxx1[[1]] <- gsub("\\.saveInterval\", \"numeric\", NA",
                   "\\testtime\", \"logical\", NA_real_", xxx1[[1]])

  xxx1[[2]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
                   "\\.plotInitialTime\", \"character\", TRUE", xxx[[2]])
  xxx1[[2]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
                   "\\.saveInitialTime\", \"character\", 'c'", xxx1[[2]])
  xxx1[[2]] <- gsub("\\.saveInterval\", \"numeric\", NA",
                   "\\testtime\", \"character\", NA_real_", xxx1[[2]])

  xxx1[[3]] <- gsub("\\.plotInitialTime\", \"numeric\", NA",
                   "\\.plotInitialTime\", \"character\", 1", xxx[[3]])
  xxx1[[3]] <- gsub("\\.saveInitialTime\", \"numeric\", NA",
                   "\\hello\", \"character\", 1", xxx1[[3]])
  xxx1[[3]] <- gsub("\\.saveInterval\", \"numeric\", NA",
                   "\\testtime\", \"logical\", NA_real_", xxx1[[3]])
  xxx1[[4]] <- xxx[[4]] # clean one

  cat(xxx1[[1]][1:(lineWithInputObjects-1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei2', 'numeric', '', ''),
      expectsInput('ei3', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx1[[1]][(lineWithInputObjects+1):(lineWithOutputObjects-1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co2', 'numeric', ''),
      createsOutput('co3', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx1[[1]][(lineWithOutputObjects+1):lineWithInit], "
      a <- sim$b
      sim$g <- f
      holy(sim$co4) <- f
      moly(sim$aaa) <- f
      fff <- sim$ei2
      fff <- sim$co3
      sim$co1 <- 123
      xx <- c(1,2)
      xx[sim$ei4] <- NA
      ",
      xxx1[[1]][(lineWithInit+1):lineWithDotInputObjects], "
      a <- sim$b
      sim$g <- 1
      sim$ei1 <- 4
      fff <- sim$ei1
      fff <- sim$co3
      sim$co1 <- 123
      ",
      xxx1[[1]][(lineWithDotInputObjects+1):length(xxx1[[1]])],
      sep = "\n", fill = FALSE, file = fileNames[1])


  cat(xxx1[[2]][1:(lineWithInputObjects-1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx1[[2]][(lineWithInputObjects+1):(lineWithOutputObjects-1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx1[[2]][(lineWithOutputObjects+1):lineWithInit], "
      a <- sim$b
      xx <- c(1,2)
      xx[sim$ei4] <- NA
      ",
      xxx1[[2]][(lineWithInit+1):lineWithDotInputObjects], "
      a <- sim$b
      sim$co1 <- 123
      ",
      xxx1[[2]][(lineWithDotInputObjects+1):length(xxx1[[2]])],
      sep = "\n", fill = FALSE, file = fileNames[2])

  fullMessage <- c("defineParameter: 'plotInitialTime' is not of specified type 'character'",
                   "defineParameter: 'saveInitialTime' is not of specified type 'character'",
                   "Running inputObjects for test", "test: module code: co2, co3 are declared in outputObjects, but are not assigned in the module",
                   "test: module code: ei2, ei3, ei4 are declared in inputObjects, but no default\\(s\\) are provided in inputObjects",
                   "test: module code: ei3 is declared in inputObjects, but is not used in the module",
                   "test: module code: inputObjects: local variable.*a.*assigned but may not be used",
                   "test: module code: inputObjects: local variable.*fff.*assigned but may not be used",
                   "test: module code: Init: local variable.*a.*assigned but may not be used",
                   "test: module code: Init: local variable.*fff.*assigned but may not be used",
                   "test: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in outputObjects",
                   "test: inputObjects: g, co1 are assigned to sim inside inputObjects, but are not declared in inputObjects",
                   "test: inputObjects: b, aaa are used from sim inside Init, but are not declared in inputObjects",
                   "test: inputObjects: b, co3 are used from sim inside inputObjects, but are not declared in inputObjects",
                   "defineParameter: 'plotInitialTime' is not of specified type 'character'",
                   "Running inputObjects for test2", "test2: module code: co1, co4 are declared in outputObjects, but are not assigned in the module",
                   "test2: module code: ei1, ei4 are declared in inputObjects, but no default\\(s\\) are provided in inputObjects",
                   "test2: module code: ei1 is declared in inputObjects, but is not used in the module",
                   "test2: module code: inputObjects: local variable.*a.*assigned but may not be used",
                   "test2: module code: Init: local variable.*a.*assigned but may not be used",
                   "test2: inputObjects: co1 is assigned to sim inside inputObjects, but is not declared in inputObjects",
                   "test2: inputObjects: b is used from sim inside Init, but is not declared in inputObjects",
                   "test2: inputObjects: b is used from sim inside inputObjects, but is not declared in inputObjects",
                   "defineParameter: 'plotInitialTime' is not of specified type 'character'",
                   "defineParameter: 'hello' is not of specified type 'character'",
                   "Running inputObjects for test3", "test3: module code appears clean",
                   "Running inputObjects for test4", "test4: module code appears clean"
  )

  for(y in 3:4) {
    cat(xxx1[[y]], sep = "\n", fill = FALSE, file = fileNames[y])
  }

  mm1 <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = as.list(m)))
  mm1 <- cleanMessage(mm1)
  expect_true(all(unlist(lapply(fullMessage,
                                function(x) any(grepl(mm1, pattern = x))))))
  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = as.list(m)))
  mm <- cleanMessage(mm)
})


test_that("Module code checking -- pipe with matrix product with backtick & data.table", {
  library(igraph)
  tmpdir <- file.path(tempdir(), "test_conflictingFns") %>% checkPath(create = TRUE)
  cwd <- getwd()
  setwd(tmpdir)

  on.exit({
    detach("package:igraph")
    setwd(cwd)
    unlink(tmpdir, recursive = TRUE)
  }, add = TRUE)

  m <- "child4"
  newModule(m, tmpdir, open = FALSE)
  fileName <- file.path(m, paste0(m, ".R"))#child4/child4.R"
  xxx <- readLines(fileName)
  lineWithInit <- grep(xxx, pattern = "^Init")
  xxx1 <- xxx
  cat(xxx[1:lineWithInit], "
    checksums1 <- structure(list(result = c('OK', 'OK'),
                                           expectedFile = c('Land_Cover_2010_TIFF.zip','NA_LandCover_2010_25haMMU.tif'),
                                           actualFile = c('Land_Cover_2010_TIFF.zip', 'NA_LandCover_2010_25haMMU.tif'),
                                           checksum.x = c('f4f647d11f5ce109', '6b74878f59de5ea9'),
                                           checksum.y = c('f4f647d11f5ce109', '6b74878f59de5ea9'),
                                           algorithm.x = c('xxhash64', 'xxhash64'),
                                           algorithm.y = c('xxhash64', 'xxhash64'),
                                           renamed = c(NA, NA),
                                           module = c('simplifyLCCVeg',  'simplifyLCCVeg')),
                                      .Names = c('result', 'expectedFile', 'actualFile',
                                                 'checksum.x', 'checksum.y', 'algorithm.x', 'algorithm.y', 'renamed',
                                                 'module'),
                                      row.names = c(NA, -2L),
                                      class = c('grouped_df', 'tbl_df', 'tbl', 'data.frame'),
                                      vars = 'expectedFile',
                                      indices = list(0L, 1L),
                                      group_sizes = c(1L, 1L),
                                      biggest_group_size = 1L,
                                      labels = structure(list(expectedFile = c('Land_Cover_2010_TIFF.zip', 'NA_LandCover_2010_25haMMU.tif')),
                                                         .Names = 'expectedFile',
                                                         row.names = c(NA, -2L),
                                                         class = 'data.frame', vars = 'expectedFile'))

    result1 <- checksums1[checksums1$expectedFile == 'NA_LandCover_2010_25haMMU.tif',]$result

    sim$bvcx <- matrix(1:2) %>% `%*%` (2:3)
    sim$bvcx2 <- matrix(1:2) %>% \"%*%\" (2:3)
    sim$b <- matrix(1:2) %>% t()

    sim$a <- 1
    ",
      xxx[(lineWithInit+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))
  mm <- cleanMessage(mm)

  fullMessage1 <- c("Running inputObjects for child4",
                   "child4: module code: Init: local variable.*result1.*assigned but may not be used ",
                   "child4: outputObjects: bvcx, bvcx2, b, a are assigned to sim inside Init, but are not declared in outputObjects")
  fullMessageNonInteractive <- c("Running inputObjects for child4",
                    "child4: module code: Init",cantCodeCheckMessage,"'sim\\$bvcx <- matrix.*",#possibly at .*147",
                    "child4: module code: Init",cantCodeCheckMessage,"'sim\\$bvcx2 <- matrix.*",#possibly at .*148",
                    "child4: module code: Init: local variable.*result1.*assigned but may not be used",
                    "child4: outputObjects: b, a are assigned to sim inside Init, but are not declared in outputObjects"
  )
  test1 <- all(unlist(lapply(fullMessage1, function(x) any(grepl(mm, pattern = x)))))
  test2 <- all(unlist(lapply(fullMessageNonInteractive, function(x) any(grepl(mm, pattern = x)))))
  # if (grepl( "emcintir", Sys.info()["user"])) {
  #   tmpFilename = "c:/Eliot/tmp/test1.txt"
  #
  #   cat("################### test1\n", file = tmpFilename, append = FALSE)
  #   cat(paste(collapse = " ", lapply(fullMessage1, function(x) any(grepl(mm, pattern = x)))), file = tmpFilename, append = TRUE)
  #   cat("\n################### test2\n", file = tmpFilename, append = TRUE)
  #   cat(paste(collapse = " ", lapply(fullMessageNonInteractive, function(x) any(grepl(mm, pattern = x)))), file = tmpFilename, append = TRUE)
  #   cat("\n################### fullMessage1\n", file = tmpFilename, append = TRUE)
  #   cat(paste(collapse = "\n", fullMessage1), file = tmpFilename, append = TRUE)
  #   cat("\n################### fullMessageNonInteractive\n", file = tmpFilename, append = TRUE)
  #   cat(paste(collapse = "\n", fullMessageNonInteractive), file = tmpFilename, append = TRUE)
  #   cat("\n###################  mm\n", file = tmpFilename, append = TRUE)
  #   cat(paste(collapse = "\n", mm), file = tmpFilename, append = TRUE)
  # }
  expect_true(test1 || test2)

})
