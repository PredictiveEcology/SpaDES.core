test_that("simulation runs with simInit and spades", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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

  ## simtime
  expect_equivalent(time(mySim), 10.0)
  expect_equivalent(start(mySim), 0.0)
  expect_equivalent(end(mySim), 10.0)

  ## sim results
  ## NOTE: version 3.3.4 of RandomFields completely changed the values!!!
  ## TODO: figure out why R-devel is completely different

  if (getRversion() < "3.6.0") {
    burnedLast <- c(1725, 126, 816, 2136, 1836, 825, 1381, 1507, 1509, 1624)

    pos_x <- c(3.67198762961861, 43.5696708785149, 47.2305734618245, 3.60246886799683,
               20.2341208513449, -47.8103385360554, 16.549667617437, -21.4073219999126,
               -47.2769911726926, -6.90873327788373, 36.835560427439, 12.9374835943128,
               40.5674117042784, -11.7361472823519, -41.5713366897425, 34.5473813500302,
               48.7716328246997, 1.17105329929266, 35.5846164245956, -7.86561428907069,
               -46.3823959510007, -15.5702817526754, -30.8126668166424, -10.8010597558361,
               -21.621652166683, -34.9716907591623, 25.5631391945506, -39.4952612681468,
               -25.1246681360093, 21.622935348448, -14.4866699835992, -49.0314824104314,
               29.5110481868387, 29.2980035604069, -43.4047992834982, -29.9034739927016,
               -47.4019699552285, 16.4433227782692, -14.9724014492504, 29.3395340026156,
               13.6774863068155, -34.9815939568256, -49.2326424452628, -24.63099179726,
               29.5125886276067, 23.458819195019, -49.267195519905, -38.9791254541313,
               -28.2266566260833, 13.5885336184307, -20.6503350281957, -33.8303967733853,
               -24.1745353595507, -31.3420623391095, 6.61947623399171, -21.1950814939711,
               -1.33380687943018, -3.75424367990195, -14.1989815964155, 39.9509364652245,
               11.3294250869671, -45.6319461016064, -20.4925846467192, 33.5846108424744,
               -27.8641689486461, 49.2799565507116, 10.6300284406548, 9.82084260223184,
               -14.0267505658531, -38.7461668664082, -17.4223198195779, -42.0410983042247,
               13.116781230502, -30.2946890723426, -7.24177892193424, -48.5130451079312,
               17.1057771797333, -34.2704160400112, 45.5029029338103, -45.8001752305177,
               -17.2400741380727, 44.8468809094991, -30.2109608623335, 36.1406259127961,
               49.5797105154377, 37.9715152273499, 28.9228258650546, 0.0171507589597368,
               -49.8120291449472, -0.98955203861027, -23.0873331875497, 37.1384151375969,
               -5.71780586016212, 43.5434737540987, 30.4242728959723, -26.1180246775251,
               -41.1416482549562, -27.6357211052713, -44.4316026481829, -5.95912959330009)

    pos_y <- c(-15.001761220824, -5.35080605403711, 19.5077777843759, -36.397145230444,
               -33.6423155958808, 15.5654887730343, 15.1161302639245, 39.6292863891733,
               -6.33467507909924, 18.7170224485725, 9.9439345128018, -35.6163117424963,
               30.3189149133229, 36.3302747888531, -20.5841744054804, 35.0724288881633,
               6.89301514649688, 26.6513337958583, 40.7971294972673, -42.5921974005888,
               -25.1956144258027, 8.07885129937992, -4.96023665998145, -17.0580144244664,
               6.70757582479894, -30.0547962733918, -26.9514004855529, 9.51961127097033,
               45.8924961557304, -35.064160676, 15.9600129744977, -11.0340528019363,
               15.0513989967463, 28.2511337198897, 12.6024406421523, 29.6186430588117,
               -39.7428219762265, 35.4932893119247, -43.7241362328051, -37.9005457120927,
               12.0957051751919, 23.8288477776908, -19.2354379455293, 23.8879129871873,
               33.7143251941822, -3.12072701146833, -31.6235585354086, 16.7528980653008,
               36.7097153167129, -11.7295242023283, -6.94989772244249, 13.9367056129548,
               28.4679184711446, -15.8080671318301, -3.55494838851763, 14.9708684826552,
               -38.8792113697679, -15.9961364230978, 37.0641490103275, 37.8956868303261,
               47.326711092004, 26.553518278515, -49.1534947969543, -10.7774399881724,
               15.8927912475565, -6.9736291044513, 3.01107884790719, -33.4855281463676,
               -11.9831588943045, -38.8181249029193, -6.62110647477371, 45.9736674749952,
               -34.1942437078733, -40.9635623302871, 5.17629246277392, -6.68267453641167,
               29.1209898564489, 45.3398473666519, -30.5369457698111, -42.9088551558613,
               -19.1027196578503, -4.02030489693142, 2.12143116091905, -44.4144553066295,
               10.7604128113847, -49.9396648839783, 45.0920147869853, -0.0581486970932161,
               9.09784601154538, 35.1429818255256, 1.65204980292364, -46.334526877774,
               32.4348917623925, 38.1377734360978, 44.4057542471017, 19.6370212341059,
               -47.8588704211158, 12.5778638346433, 27.527048666893, 7.68312730236396)
  } else {
    ## development version produces different results
    burnedLast <- c(1709, 545, 1055, 1172, 1207, 723, 2017, 1611, 1348, 1775)

    pos_x <- c(-32.0147358840541, -36.6807333239312, 36.9599246236279, -49.1427239464679,
               -12.3717481613493, 1.549995261422, -29.5540251992733, 45.6628705203289,
               40.2149952394638, 46.026430937904, -36.5247110708436, -12.3882735123318,
               22.9200817240235, -28.5192058188812, 2.42813593067285, 0.978402961981786,
               9.62303025248077, -24.2483647055001, -6.62154607781263, -34.7205685320196,
               -16.2417149545597, 15.6945638231324, -26.0836501201885, -1.43426412367929,
               -23.1254804157888, 23.3910906907021, 41.2635265888731, 21.105009781434,
               49.3775442374953, 33.4239309506703, -29.4751468744909, 1.72474914804072,
               -8.97788305384604, -23.7774034252138, -45.4917855789254, 42.8374359196147,
               6.52473707472846, 17.1338446343558, -5.51124209279867, -46.374143381615,
               19.8657253531249, -42.7546027167513, 15.0246135210187, -26.5382947485011,
               -31.9033746236977, -22.3014148770238, 4.62708930716146, -15.9853954841212,
               43.7609836458805, -21.1477261478464, -10.8019912678679, -14.9098585223619,
               34.4437570584511, 36.9703190472271, -48.3232218599662, 13.7647596215917,
               -24.4925919090681, 37.1940412359918, -27.4666386567011, 36.4082152674992,
               2.98201350035863, -24.5018854133219, 17.4403908766274, -19.3832165721584,
               4.26326638082313, -39.2765047935536, 35.6915513907935, -19.1040414194675,
               24.6033714721212, 48.6787778859825, 14.4024675884235, 19.1023531228051,
               -4.81383884154367, -30.0253692350584, 16.7471978438968, 27.2374120486856,
               -43.8349397650265, -31.3223358314743, 15.8187001177445, -15.2473576691829,
               25.4644284571478, 8.23721814213037, 27.2736774253095, 44.0318010499479,
               -47.4766734417228, 32.2920640690994, -28.1553590423564, -36.708160014371,
               24.2897273432036, 13.4070118698388, -39.0357978447186, 28.7441677522237,
               45.850207026505, -35.3054084105622, 31.902189127751, -48.7310104328324,
               43.7053887130289, 20.739589064343, 44.5061034783714, -30.581665487695
    )
    pos_y <- c(-40.8374937549455, -1.77407047690452, 27.4205596092746, -5.2659196647588,
               -0.398387246347298, -9.8699245787057, -44.1107141009149, -44.0626088528403,
               -32.3221058509319, 46.4311217225135, -13.4547045255636, -28.5882922018316,
               -24.8270876101612, 49.7702225161138, 16.9445351147954, -26.0993425858616,
               28.1485442288767, -1.16137370610496, -48.2715291823916, 30.0454696684257,
               -0.519078123157968, 10.6872711213086, 20.5146752155688, 19.1873725161782,
               -3.2544938630978, -47.7430736924847, -5.10142230623757, -49.4867294147209,
               3.1607171444729, -26.4031958423312, 5.49383577051489, 5.2288124452874,
               -30.5040240074296, -20.2005487866558, 10.3239668691001, -46.7385151654818,
               -49.1426725752601, -2.1743243579597, 12.3139010066894, -33.8547213140177,
               -48.1008009222837, -29.9889849426229, 47.1384793220961, 47.2097804758614,
               19.638130959238, -12.0294310784439, -13.530589022465, -45.8592686166851,
               30.5949010327554, -19.2218134496256, 49.734031281965, -21.5711964695992,
               -0.810489621001885, -49.6648513784812, 45.3194693546655, 38.6834772856,
               -35.2641005483873, -18.2627951228366, -36.4417428555604, -7.83375300437855,
               9.58655873962374, -21.6533567782094, 35.7206583836036, -9.18796458370498,
               47.3812404962028, 19.0919040872498, -11.0203843747802, -21.4106348850377,
               -42.9235851680988, -42.6388751027297, -26.3172831548664, 26.2895106877418,
               -32.4288432501506, 6.98620984566215, -9.90144334379338, 21.5616737043562,
               35.7302843325972, -16.2798639295043, 16.5901838219654, 9.6338008841535,
               33.5897751064701, 26.7225783735572, 40.2572238444122, -23.5918870166261,
               -39.2637914958258, 30.8518719668662, -43.1441767947326, -39.0136203922306,
               -20.8968365217442, -42.315958931679, 13.9422955510082, 13.0515276234654,
               45.37807834991, 31.3935885758781, -34.3600554608326, -5.40001822413458,
               -10.6915955872229, 18.3693792201844, 36.9593007041215, 0.828185330538979
    )
  }

  expect_equal(mySim$npixelsburned, burnedLast)
  expect_equivalent(mySim$caribou$x, pos_x)
  expect_equivalent(mySim$caribou$y, pos_y)
})

test_that("spades calls with different signatures don't work", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  a <- simInit()
  a1 <- Copy(a)
  opts <- options(spades.saveSimOnExit = FALSE)
  expect_output(spades(a, debug = TRUE), "eventTime")
  expect_silent(spades(a, debug = FALSE))
  expect_silent(spades(a, debug = FALSE, .plotInitialTime = NA))
  expect_silent(spades(a, debug = FALSE, .saveInitialTime = NA))
  opts <- options(opts)
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
  opts <- options(spades.saveSimOnExit = FALSE)
  expect_silent(spades(a, debug = FALSE, progress = FALSE))
  expect_silent(spades(a, debug = FALSE, progress = "rr"))
  opts <- options(opts)

  paths(a)$cachePath <- file.path(tempdir(), "cache") %>% checkPath(create = TRUE)
  a <- Copy(a1)
  expect_output(spades(a, cache = TRUE, debug = TRUE, notOlderThan = Sys.time()), "eventTime")
  expect_true(all(c("backpack.db", "gallery") %in% dir(paths(a)$cachePath)))
  file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))

  # test for system time ... in this case, the first time through loop is slow
  #   because of writing cache to disk, not because of spades being slow.
  #   simList is empty.

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
  #expect_gt(st1[1], st2[1]) ## no longer true on R >= 3.5.1 ??
  file.remove(dir(paths(a)$cachePath, full.names = TRUE, recursive = TRUE))
})

test_that("simInit with R subfolder scripts", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  newModule("child1", ".", open = FALSE)
  cat(file = file.path("child1", "R", "script.R"),
      "a <- function(poiuoiu) {
      poiuoiu + 1
}", sep = "\n")
  mySim <- simInit(modules = "child1", paths = list(modulePath = tmpdir))
  expect_true(sum(grepl(unlist(lapply(ls(mySim@.xData, all.names = TRUE), function(x) {
    if (is.environment(mySim@.xData[[x]])) ls(envir = mySim@.xData[[x]], all.names = TRUE)
  })), pattern = "^a$")) == 1)
  expect_true(mySim@.xData$child1$a(2) == 3) # Fns
  })

test_that("simulation runs with simInit with duplicate modules named", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

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

  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  newModule("test", tmpdir, open = FALSE)
  newModule("test2", tmpdir, open = FALSE)

  sim <- simInit()

  # Sept 18 2018 -- Changed to use "seconds" -- better comparison with simple loop
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
      timeunit = "second",
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
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 1, "test", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      ', fill = TRUE)

  cat(file = file.path(tmpdir, "test2", "test2.R"),'
      defineModule(sim, list(
      name = "test2",
      description = "insert module description here",
      keywords = c("insert key words here"),
      authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
      childModules = character(0),
      version = list(SpaDES.core = "0.1.0", test2 = "0.0.1"),
      spatialExtent = raster::extent(rep(NA_real_, 4)),
      timeframe = as.POSIXlt(c(NA, NA)),
      timeunit = "second",
      citation = list("citation.bib"),
      documentation = list("README.txt", "test2.Rmd"),
      reqdPkgs = list(),
      parameters = rbind(
      ),
      inputObjects = bind_rows(
      ),
      outputObjects = bind_rows(
      )
      ))

      doEvent.test2 = function(sim, eventTime, eventType, debug = FALSE) {
      switch(
      eventType,
      init = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
      },
      event1 = {
      sim <- scheduleEvent(sim, sim@simtimes[["current"]] + 2, "test2", "event1", .skipChecks = TRUE)
      })
      return(invisible(sim))
      }
      ', fill = TRUE)

  N <- 5000

  moduleDir <- file.path(tmpdir)
  inputDir <- file.path(moduleDir, "inputs") %>% reproducible::checkPath(create = TRUE)
  outputDir <- file.path(moduleDir, "outputs")
  cacheDir <- file.path(outputDir, "cache")
  times <- list(start = 0, end = N)
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

  nTimes <- 20

  #######################
  # Tested on laptop
  #######################
  # laptop was 10.2 seconds -- currently 4.2 seconds or so --> June 29, 2018 is 1.06 seconds
  # laptop New with "seconds" -- Sept 21, 2018 is 0.492 seconds --> 98 microseconds/event
  # laptop New with "seconds" -- Nov 26, 2018 is 0.458 seconds --> 92 microseconds/event!
  # Windows Desktop -- slower -- Nov 26, 2018 0.730 Seconds --> 148 microseconds/event!
  # Linux Server -- slower -- Nov 26, 2018 0.795 Seconds --> 159 microseconds/event!
  # BorealCloud Server -- slower -- Nov 26, 2018 0.972 Seconds --> 194 microseconds/event!
  # laptop -- May 25, 2019 0.603 Seconds --> 120 microseconds/event!
  # laptop with new completed as environment -- May 25, 2019 0.357 Seconds --> 71 microseconds/event!
  options("spades.keepCompleted" = TRUE)
  microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)})

  # Turn off completed list
  #  Changed to use "seconds" -- better comparison with simple loop
  # Old times using "year"  -- June 29, 2018 is 0.775 seconds, Sept 19, 2018 0.809 seconds
  #                         -- This is 161 microseconds per event
  # New times using "second" -- Sept 19, 2018 0.244 Seconds --> 49 microseconds/event
  # New times using "second" -- Nov 26, 2018 0.192 Seconds --> 38 microseconds/event!
  # Windows Desktop -- slower -- Nov 26, 2018 0.348 Seconds --> 70 microseconds/event!
  # Linux Server -- slower -- Nov 26, 2018 0.461 Seconds --> 92 microseconds/event!
  # BorealCloud Server -- slower -- Nov 26, 2018 0.282 Seconds --> 56 microseconds/event!
  # With many new "exists"
  # laptop -- May 25, 2019 0.264 Seconds --> 53 microseconds/event!
  options("spades.keepCompleted" = FALSE)
  (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
  #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})

  a <- 0
  a3 <- microbenchmark::microbenchmark(
    for (i in 1:N) {
      a <- a + 1
    }
  )

  summary(a2)[, "median"]/summary(a3)[, "median"]

  ########################################
  # With 2 modules, therefore sorting
  ########################################
  modules <- list("test", "test2")
  mySim <- simInit(times = times, params = parameters, modules = modules,
                   objects = objects, paths = paths)

  nTimes <- 10
  # Turn off completed list
  # New times using "second" -- Nov 26, 2018 0.443 Seconds --> 59 microseconds/event, even with sorting
  options("spades.keepCompleted" = FALSE)
  (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
  #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})

  # New times using "second" -- Nov 26, 2018 0.443 Seconds --> 130 microseconds/event, even with sorting
  options("spades.keepCompleted" = TRUE)
  (a2 <- microbenchmark::microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
})

test_that("conflicting function types", {
  testInitOut <- testInit(smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  m <- "child4"
  newModule(m, tmpdir, open = FALSE)
  fileName <- file.path(m, paste0(m, ".R")) # child4/child4.R"
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
      poiuoiu <- sim@.xData$d1
      qwerqwer <- sim@.xData[['test']]
      sim$g <- f
      sim@.xData$g1 <- f
      return(list(a, d, f, sim))
      ",
      xxx1[(lineWithInit+1):length(xxx1)], sep = "\n", fill = FALSE, file = fileName)

  mm <- capture_messages(simInit(paths = list(modulePath = tmpdir), modules = m))

  fullMessage <- c("defineParameter: 'value' is not of specified type 'numeric'",
                   "defineParameter: 'plotInterval' is not of specified type 'numeric'",
                   "defineParameter: 'saveInitialTime' is not of specified type 'numeric'",
                   "defineParameter: 'saveInterval' is not of specified type 'numeric'",
                   "child4: module code: Init: local variable.*qwerqwer.*assigned but may not be used",
                   "Running .inputObjects for child4", "child4: module code: Init: local variable.*poiuoiu.*assigned but may not be used",
                   "child4: outputObjects: g, g1 are assigned to sim inside Init, but are not declared in metadata outputObjects",
                   "child4: inputObjects: b, d, f, hi, d1, test are used from sim inside Init, but are not declared in metadata inputObjects"
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

  # declared in metadata inputObjects
  lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  cat(xxx[1:(lineWithInputObjects-1)], "
      expectsInput('a', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: a is declared in metadata inputObjects")))

  # declared in metadata outputObjects
  lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  cat(xxx[1:(lineWithOutputObjects-1)], "
      createsOutput('b', 'numeric', '')
      ",
      xxx[(lineWithOutputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: b is declared in metadata outputObjects")))

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
                        pattern = c(paste0(m, ": module code: b is declared in metadata outputObjects|",
                                           m, ": module code: a is declared in metadata inputObjects|",
                                           "Running .inputObjects|",
                                           "Setting:|Paths set to:|",
                                           m, ": using dataPath")))))

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
  lineWithDotInputObjects <- grep(xxx, pattern = "\\.inputObjects")[1]
  cat(xxx[1:(lineWithInputObjects-1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei2', 'numeric', '', ''),
      expectsInput('ei3', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co2', 'numeric', ''),
      createsOutput('co3', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx[(lineWithOutputObjects + 1):lineWithInit], "
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
      xxx[(lineWithInit + 1):lineWithDotInputObjects], "
      a <- sim$b
      sim$g <- 1
      sim$ei1 <- 4
      fff <- sim$ei1
      fff <- sim$co3
      sim$co1 <- 123
      aaa <- sim$.userSuppliedObjNames # in the ignoreObjects
      ",
      xxx[(lineWithDotInputObjects + 1):length(xxx)],
      sep = "\n", fill = FALSE, file = fileName)

  fullMessage <- c(
    "Running .inputObjects for child4",
    "child4: module code: co2, co3 are declared in metadata outputObjects, but are not assigned in the module",
    "child4: module code: ei2, ei3, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
    "child4: module code: ei3 is declared in metadata inputObjects, but is not used in the module",
    "child4: module code: .inputObjects: local variable.*a.*assigned but may not be used",
    "child4: module code: .inputObjects: local variable.*fff.*assigned but may not be used",
    "child4: module code: Init: local variable.*a.*assigned but may not be used",
    "child4: module code: Init: local variable.*fff.*assigned but may not be used",
    "child4: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in metadata outputObjects",
    "child4: inputObjects: g, co1 are assigned to sim inside .inputObjects, but are not declared in metadata inputObjects",
    "child4: inputObjects: b, aaa are used from sim inside Init, but are not declared in metadata inputObjects",
    "child4: inputObjects: b, co3 are used from sim inside .inputObjects, but are not declared in metadata inputObjects"
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
  testInitOut <- testInit(smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
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
  testInitOut <- testInit(smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
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
  fileNames <- file.path(tmpdir, m, paste0(m,".R"))
  xxx <- lapply(fileNames, readLines)
  set.seed(113)

  lineWithInit <- grep(xxx[[1]], pattern = "^Init")
  lineWithInputObjects <- grep(xxx[[1]], pattern = " expectsInput")
  lineWithOutputObjects <- grep(xxx[[1]], pattern = " createsOutput")
  lineWithDotInputObjects <- grep(xxx[[1]], pattern = "\\.inputObjects")[1]

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

  fullMessage <- c(
    "defineParameter: 'plotInitialTime' is not of specified type 'character'",
    "defineParameter: 'saveInitialTime' is not of specified type 'character'",
    "Running .inputObjects for test",
    "test: module code: co2, co3 are declared in metadata outputObjects, but are not assigned in the module",
    "test: module code: ei2, ei3, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
    "test: module code: ei3 is declared in metadata inputObjects, but is not used in the module",
    "test: module code: .inputObjects: local variable.*a.*assigned but may not be used",
    "test: module code: .inputObjects: local variable.*fff.*assigned but may not be used",
    "test: module code: Init: local variable.*a.*assigned but may not be used",
    "test: module code: Init: local variable.*fff.*assigned but may not be used",
    "test: outputObjects: g, aaa are assigned to sim inside Init, but are not declared in metadata outputObjects",
    "test: inputObjects: g, co1 are assigned to sim inside .inputObjects, but are not declared in metadata inputObjects",
    "test: inputObjects: b, aaa are used from sim inside Init, but are not declared in metadata inputObjects",
    "test: inputObjects: b, co3 are used from sim inside .inputObjects, but are not declared in metadata inputObjects",
    "defineParameter: 'plotInitialTime' is not of specified type 'character'",
    "Running .inputObjects for test2",
    "test2: module code: co1, co4 are declared in metadata outputObjects, but are not assigned in the module",
    "test2: module code: ei1, ei4 are declared in metadata inputObjects, but no default\\(s\\) are provided in .inputObjects",
    "test2: module code: ei1 is declared in metadata inputObjects, but is not used in the module",
    "test2: module code: .inputObjects: local variable.*a.*assigned but may not be used",
    "test2: module code: Init: local variable.*a.*assigned but may not be used",
    "test2: inputObjects: co1 is assigned to sim inside .inputObjects, but is not declared in metadata inputObjects",
    "test2: inputObjects: b is used from sim inside Init, but is not declared in metadata inputObjects",
    "test2: inputObjects: b is used from sim inside .inputObjects, but is not declared in metadata inputObjects",
    "defineParameter: 'plotInitialTime' is not of specified type 'character'",
    "defineParameter: 'hello' is not of specified type 'character'",
    "Running .inputObjects for test3",
    "test3: module code appears clean",
    "Running .inputObjects for test4",
    "test4: module code appears clean"
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
  testInitOut <- testInit(smcc = TRUE)
  on.exit({
    testOnExit(testInitOut)
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

  fullMessage1 <- c(
    "Running .inputObjects for child4",
    "child4: module code: Init: local variable.*result1.*assigned but may not be used ",
    "child4: outputObjects: bvcx, bvcx2, b, a are assigned to sim inside Init, but are not declared in metadata outputObjects")
  fullMessageNonInteractive <- c(
    "Running .inputObjects for child4",
    "child4: module code: Init", cantCodeCheckMessage, "'sim\\$bvcx <- matrix.*",#possibly at .*147",
    "child4: module code: Init", cantCodeCheckMessage, "'sim\\$bvcx2 <- matrix.*",#possibly at .*148",
    "child4: module code: Init: local variable.*result1.*assigned but may not be used",
    "child4: outputObjects: b, a are assigned to sim inside Init, but are not declared in metadata outputObjects"
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

test_that("simInitAndSpades", {
  testInitOut <- testInit(opts = list("spades.moduleCodeChecks" = FALSE))
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  set.seed(42)

  times <- list(start = 0.0, end = 0, timeunit = "year")
  params <- list(
    .globals = list(burnStats = "npixelsburned", stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA, torus = TRUE),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = system.file("sampleModules", package = "SpaDES.core"))
  set.seed(123)
  mySim <- simInitAndSpades(times = times, params = params,
                            modules = modules, objects = list(), paths = paths, debug = FALSE)

  set.seed(123)
  mySim2 <- simInit(times = times, params = params,
                    modules = modules, objects = list(), paths = paths) %>%
    spades(debug = FALSE)

  expect_true(all.equal(mySim, mySim2))

  set.seed(123)
  mySim <- simInitAndExperiment(times = times, params = params,
                                modules = modules, objects = list(), paths = paths, debug = FALSE)

  set.seed(123)
  mySim2 <- simInit(times = times, params = params,
                    modules = modules, objects = list(), paths = paths) %>%
    experiment(debug = FALSE)

  expect_true(all.equal(mySim, mySim2))

})

test_that("scheduleEvent with invalid values for eventTime", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  s <- simInit(times = list(start = 1, end = 10))
  expect_error(s <- scheduleEvent(s, eventTime = -1, eventType = "test1", moduleName = "test"))
  expect_warning(s <- scheduleEvent(s, eventTime = numeric(), eventType = "test1", moduleName = "test"))
  expect_error(s <- scheduleEvent(s, eventTime = 0, eventType = "test1", moduleName = "test"))


})
