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

  if (as.numeric_version(paste0(R.version$major, ".", R.version$minor)) < "3.6.0") {
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
    burnedLast <- c(1623, 201, 723, 2036, 1561, 739, 1836, 2024, 1798, 1339)

    pos_x <- c(13.0532707978136, -32.8923908957165, 27.3969797389589, 38.9968311591861,
               -29.7376281316361, 17.6101067673529, -18.0977082950375, 24.5274747285482,
               -31.7189715804585, 7.381959991136, -43.9760214431076, -21.6972114900354,
               -49.5278149392391, 2.43127242195319, 16.0134685436804, -10.8459282001756,
               25.2180607454783, -1.7723351523618, -18.250212230032, -7.71846859821378,
               13.3820348997256, -4.38164903104301, -20.9431540816844, -36.998943651721,
               -17.8923147847315, -28.1370858241664, 5.61463577954896, -10.8043913485292,
               31.8712545591271, 5.64559917365374, -27.3013571395529, 5.65249374187498,
               -10.8303539587148, 13.4035832312236, -16.000923747874, -19.7411280170716,
               -49.9816367287749, -37.6135951927579, 2.66195570061529, 39.0068015622127,
               -37.8160698182155, 31.4230204779793, -17.2974671375295, -41.3562749525678,
               -38.9645106419225, 2.96198358254623, -40.7250620074311, 27.9062680492077,
               10.8307589967548, -21.8711284922136, -16.6113099460494, -13.4123971497708,
               -23.7813281135169, 5.59680019423666, -22.8738222538764, -4.61301507814081,
               -0.544052309611232, -39.7755277571212, -2.13053883162479, -37.591781844244,
               34.1633684717672, 29.0069972464264, 24.1316007358375, -25.9864222887071,
               -49.8242275699278, -48.9598672554151, -6.15892154315306, 1.16337971379826,
               -47.6223466826543, 38.6937425706247, 34.0439646466938, 17.3981885305444,
               28.3409608399292, 15.6170752733262, -3.53143594952176, -25.3309561688557,
               -15.5283151206724, -37.9411587563038, 20.7166031267508, 11.2837696021034,
               -34.0286000372693, 7.46434995962331, 20.9157207240955, 32.0145672661506,
               12.2843224379444, 35.9057920317743, 22.9722398004589, -26.5263089349701,
               19.4029956656467, -44.1004690935732, 17.5475666457743, 37.5821163849668,
               -14.358284978876, -31.946161198529, -14.6964305387228, -16.5479970400639,
               -48.722373418505, -29.0573994002683, 13.3363571862685, 45.5692245103506)

    pos_y <- c(-46.5761429420037, 38.2439863553609, 20.0261965063628, -21.4606618074102,
               -9.51059989449662, 14.4401700414302, 44.9147442190373, 11.2682450186758,
               24.0685938108325, -24.870962431481, -8.01146882859514, -9.96302774286448,
               3.29769285370759, -35.5205034224527, 7.96879139066004, -7.55249637477033,
               16.9858126076091, 37.5808111768344, 2.23821912140651, 28.3798053445325,
               -28.3136130644296, 16.4478023949398, -5.98824704902006, 9.24586778625189,
               -1.09542384738207, 36.9944727921547, 46.1910898146572, 40.7505950274036,
               49.7607619375239, -30.4505336358037, -28.4034895267469, -8.72345277836496,
               27.7076565721677, 41.9951376238924, 8.85561141012658, -3.04245837269701,
               -45.1137050244705, -40.5414982009767, -24.6727879893299, -45.2919860455611,
               -42.5112055155485, -13.1917543276661, -40.850073457265, -41.8302677196561,
               -27.7712219343686, -16.4281590103097, -27.1531853802137, 16.9457103838765,
               -6.29062196558142, -3.4357193317133, -24.2484089091365, -38.5749361086974,
               -13.1314930799806, -10.9645824097158, 32.4045508393872, 26.8735110113556,
               -6.28434405084627, 38.0979862917681, -27.1293303090933, 7.81747455074677,
               5.20099196556176, -11.5937542112238, 27.3199537994373, -47.2988889910877,
               -0.19224290434223, -12.4145475414155, 6.23372665793533, 29.0736458708029,
               -38.4879231566294, 8.61233866657297, -31.8099809709908, 2.41597419902637,
               40.8106545598371, -2.1559204259618, 49.6630367101775, 20.4182148709363,
               -48.8304953691509, 4.61626877232501, -12.8116808925648, 42.6054316290405,
               14.9962140339475, 46.7919320532188, 33.5183430261066, 35.5936219189923,
               21.5804760101296, 19.4874951556404, -45.8661534840926, -17.6352639537173,
               -47.2695257201565, -31.0717200543392, -19.8878369452885, -20.9349856677383,
               -36.2764092980074, 17.8241404162551, -42.5037175043944, 5.31469358813471,
               27.5892762266628, -36.0062106861055, -32.2175737208069, -23.7712900249536)
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
  # laptop with new completed as environment -- May 25, 2019 0.390 Seconds --> 78 microseconds/event!
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
  # laptop -- May 25, 2019 0.305 Seconds --> 61 microseconds/event!
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
