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

  # if (getRversion() < "3.6.0") {
  #   burnedLast <- c(1820, 1261, 2505, 1226, 892, 1512, 177, 438, 1479, 2064)
  #
  #   pos_x <- c(-22.7683666102722, 17.3555473143824, 45.0748979828491, -5.71543012245525,
  #              13.2860560329844, 1.34663419261703, 5.56532508096136, -45.0473927181248,
  #              -6.12977742301295, 5.08353969485077, 1.14174598331352, 37.7293792298175,
  #              -11.4142708285661, 45.1995049811249, 35.6157446642305, -21.656682621034,
  #              33.1449575095831, -10.6257527612957, -4.90251118510999, -41.4817632705445,
  #              31.9982045841794, -20.8749666963955, 14.7252700472079, -47.9949991444413,
  #              -29.1204974753759, 38.0331108219756, -35.0895276324787, 43.1589266432416,
  #              13.5963520604849, 2.36755340534138, -44.6274745898925, 8.89407643552825,
  #              14.4298741157073, -21.203657965455, -16.8983481482313, -39.441211751588,
  #              49.5486343402659, -15.7398166786901, -18.6641206027285, -46.1763830867225,
  #              12.3094167704489, 43.7384837668099, 19.9829255603587, -13.2752861970067,
  #              18.6340683196464, -39.9479633110077, 41.8291939251269, -16.6326282587702,
  #              -2.18201487402304, 6.0976747294475, 30.3879492102144, -5.35735514888578,
  #              21.6175235698125, -15.9417878868747, 47.1080062573359, -12.5927965245563,
  #              47.8245173781132, 29.926963425077, 21.5659627850468, 44.1340804006678,
  #              14.6441731717331, 22.9229735617647, -1.32050967115406, 22.2977401107245,
  #              -28.9168187197, 20.8029115163069, 22.7249018256671, 17.748006461159,
  #              -33.8908790901827, -13.81919011449, -4.55332941010478, -1.38242942638633,
  #              -9.31163096796901, 23.9354259046485, -36.900008412641, 21.5282661476207,
  #              22.7827264760937, 18.6360219905709, 6.56800266357888, 35.5297325186964,
  #              3.46529346243958, 16.6785857712921, -43.1966569764224, -19.1233995938329,
  #              13.5404913610291, -39.9009741460952, 18.0632016364486, -26.6087599396678,
  #              23.5589855648471, 40.2178920799335, 21.4796085517192, 11.5609279003851,
  #              47.2688304466519, -39.7610863533586, 38.0013322778655, -18.8312897542215,
  #              17.3284152152884, -43.3994535227433, -33.1406051583927, 28.7000094532591)
  #
  #   pos_y <- c(-24.304450206001, 5.24221170482942, -49.1206493974155, 22.8088673720581,
  #              15.994290908485, 25.6942377432507, 8.43622417462334, -30.5574862809077,
  #              42.5134631277559, -12.7208098652815, 3.12484766276381, 7.52247455911817,
  #              -5.35384862627406, 6.27081368183644, 20.7386282804314, 1.18567034172845,
  #              -35.6123536370302, 22.4291660372664, 0.954448750233389, -11.2685927512153,
  #              -25.97775324536, -27.5958951796571, -1.6268673450532, -3.02534423831433,
  #              -5.30339206385986, -30.2676465663823, -9.50112294103683, -42.8094213806217,
  #              12.7125257979874, 2.96121946880695, 35.4927370318243, 11.2853004027962,
  #              41.0069151890822, 7.66696065562701, 11.1859355441145, -1.8661444427233,
  #              -24.3571712228047, 44.8671064351402, 37.4990594412021, 20.2890592010447,
  #              -11.2387846020943, -7.98082411156742, -25.5141855096053, 28.3732629847582,
  #              -0.769809478844543, 37.2903791868844, -12.6974049030286, -15.6468829286525,
  #              -12.219209290255, -16.5229257405488, -15.2566782908342, -46.1682834327737,
  #              41.9056400482951, 31.5462433661389, 46.9217214073241, 26.8859475613353,
  #              -44.8722456199625, -30.0987491081832, 49.6956237067509, -22.6129205307865,
  #              16.5122154465785, -10.3999534036642, 11.0667644128902, 41.17672575666,
  #              21.3502980053056, 13.9229610016491, -33.8292618081412, 14.9492772863686,
  #              -30.2699491871322, 11.8184529680194, -33.5127657841321, 23.6408590141811,
  #              44.5360637754437, -42.3509148870208, -35.8025385024339, 12.8543488323667,
  #              -5.947306808355, -41.5599012241941, 29.4362511561944, -46.0088544479594,
  #              -3.80217930654341, 18.0451508378053, 32.7984949131119, 5.29501177584519,
  #              41.3173119584381, 25.4483139738277, 34.3835776043239, 25.5306288396947,
  #              5.62911441156129, 26.7130262655009, 11.6908365728822, 17.0301090073792,
  #              -0.683088707243385, 12.2663824226089, -16.647557643728, -44.2784312576946,
  #              -42.9691504616704, 6.51024514052493, 35.6612465429568, 48.4079013317619)
  # } else {
    ## development version produces different results
    burnedLast <- c(1622, 315, 702, 1221, 1792, 1072, 1437, 540, 736, 2063)

    pos_x <- c(45.3135454929626, -6.24768388010582, 44.8244177052508, 41.5418134694892,
               -48.6737022013069, -25.4510010665777, 9.97302965845389, 7.54073394735897,
               46.6807815240342, -5.72675643676041, 9.53364172691089, 32.4075089175158,
               3.92068352949394, -22.1644909596656, 4.92620043565161, -37.0099729504007,
               -41.5107527961036, 11.9681792251008, 24.9207722378767, -0.610280519346723,
               -10.5347956900002, -5.69458810828205, -15.4205535464277, 30.6742572630065,
               42.9537617101028, 21.2817380815315, -20.2746864016489, -13.1791335831457,
               -30.8370803284051, -36.8134210025304, 25.523856846821, 36.8161152634668,
               -41.6623106479343, -35.7733293027008, -31.9230791854198, 43.5904361664519,
               -41.9648189361, 9.08199461243247, -24.9136438427857, -32.5055281714404,
               -13.5081393712363, 34.6714903617347, 24.0298962766724, -21.9139532582008,
               -12.9465546669247, -43.4645903313617, 43.0949034751186, 0.969478070936411,
               25.0289161247854, 10.878421447172, 45.2278615812835, 37.3167111529381,
               35.8589878791028, -38.4298564060075, 8.07500568770769, 0.854373302059521,
               -42.0137752605589, 47.5335841301193, 38.6766404019634, -28.5295342060636,
               -15.9111898998201, -16.6938172440575, -26.0788427199152, 0.672348283687384,
               -5.92912256489739, -45.4176256760058, 22.3025167624911, -23.1098105948377,
               2.01214441393652, 28.8294763671351, -48.6379517526463, 41.0782780524804,
               -47.7112885884925, -23.5749673530765, 20.9568504090471, -36.0970126174757,
               -34.0783312178965, -3.40257911742975, -0.0302472237898357, -43.8582017326135,
               3.88254121298579, 13.3222016989665, -11.7270342154779, 34.3238157214522,
               30.3126472054117, 17.896598754322, -5.8689816610119, 40.8307910127854,
               27.6340017883336, -22.9017717409522, 28.5587155376361, 26.279484997513,
               34.504558297299, 8.61584322806688, 10.3056776562736, 23.1096758486237,
               -21.1736851164793, 6.12105202818987, -13.4018289622821, 43.9104026729703
    )
    pos_y <- c(31.740914255583, -40.6209317093019, -35.223998778074, 0.330387226183163,
               -30.2542976485152, 9.13525117674168, 23.3788519556616, 11.4468008540356,
               15.7491649024705, -39.8035205566601, -38.7226738656964, -35.9999461588219,
               37.9229310071172, -28.290793073822, -10.8561738538821, -10.0457045425668,
               26.9720458504925, -38.2142252789072, 7.95988910041123, 34.4888540113826,
               -19.4928128723217, 1.04211777782832, -6.3749933883286, 46.0909030607061,
               -17.9991706854972, -24.5528778276568, 11.9653824507767, 33.0515590318069,
               -22.5635049943451, -13.8056202161169, 18.6430503800567, 49.5042422360717,
               28.2327941504677, -23.5389439771081, -12.5236362770098, 17.6035639652873,
               11.903895091931, 31.6186176954857, -6.52369392686975, -8.68036391871772,
               -25.0528296177676, 45.0166618061142, 45.8521550760685, -1.33671783951888,
               40.5706834568383, 21.7568829049721, -29.9522736193872, 38.5436698159674,
               18.3039829248301, -45.0427737549665, -39.6667289289839, -13.379163553718,
               4.53458247219081, 18.3746091221627, 19.6389620408606, 18.6610525797329,
               34.4689631302232, -36.3877380941283, 1.3498467801286, 12.7630994173697,
               19.9439065141578, 28.0419405279047, 4.15737287233868, 20.1470950038561,
               15.2508251946458, -18.6986811387597, 41.6363536749384, 9.55976430466935,
               24.7212757005479, 8.71683009017259, 10.8699125375096, 23.966354407982,
               8.54341488030077, 35.7867368568469, -34.8409505347251, 5.32716908115744,
               -25.6365264970226, -41.6783509791431, 13.8168081737344, 43.4546409286252,
               -20.2834249581727, 24.9619458463728, -16.0397091255677, 15.1738621367286,
               3.76137173761458, 24.3441480182552, 48.2218297111432, -18.1749743801169,
               43.8103508308043, -0.275838362356964, -47.0084702648021, 22.6482338617792,
               -4.25713037639701, -0.302661247220549, 22.0859846432585, -17.2975164777434,
               44.5587288889833, -17.8906362097366, 33.3869492141604, 45.9253458310934
    )
  # }

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
  library(microbenchmark)
  microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)})

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
  (a2 <- microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
  #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})

  a <- 0
  a3 <- microbenchmark(
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
  (a2 <- microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
  #profvis::profvis({for (i in 1:10) spades(mySim, debug = FALSE)})

  # New times using "second" -- Nov 26, 2018 0.443 Seconds --> 130 microseconds/event, even with sorting
  options("spades.keepCompleted" = TRUE)
  (a2 <- microbenchmark(times = nTimes, {spades(mySim, debug = FALSE)}))
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
      xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

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
      xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 "raster::scale")

  ###
  cat(xxx[1:lineWithInit], "
      library(raster)
      poiuoiu <- raster(extent(0,10,0,10), vals = rep(1:2, length.out = 100))
      poiuoiu <- raster::scale(poiuoiu)
      sim$poiuoiu <- poiuoiu
      ",
      xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

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
      xxx1[(lineWithInit + 1):length(xxx1)], sep = "\n", fill = FALSE, file = fileName)

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
      xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_error(simInit(paths = list(modulePath = tmpdir), modules = m),
               c(paste0(m, ": You have created an object")))

  # declared in metadata inputObjects
  lineWithInputObjects <- grep(xxx, pattern = " expectsInput")
  cat(xxx[1:(lineWithInputObjects - 1)], "
      expectsInput('a', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: a is declared in metadata inputObjects")))

  # declared in metadata outputObjects
  lineWithOutputObjects <- grep(xxx, pattern = " createsOutput")
  cat(xxx[1:(lineWithOutputObjects - 1)], "
      createsOutput('b', 'numeric', '')
      ",
      xxx[(lineWithInputObjects + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

  expect_message(simInit(paths = list(modulePath = tmpdir), modules = m),
                 c(paste0(m, ": module code: b is declared in metadata outputObjects")))

  cat(xxx[1:(lineWithInputObjects - 1)], "
      expectsInput('a', 'numeric', '', '')
      ",
      xxx[(lineWithInputObjects+1):(lineWithOutputObjects - 1)],
      "
      createsOutput('b', 'numeric', '')
      ",
      xxx[(lineWithInputObjects + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

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
  cat(xxx[1:(lineWithInputObjects - 1)], "
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
  xxx2a <- grep(".plotInitialTim\\>", xxx1, value = TRUE)[1]
  xxx2b <- gsub(",$", grep("time interval between plot", xxx1, value = TRUE), replacement = "")
  xxx3 <- parse(text = paste(xxx2a, xxx2b))
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

  cat(xxx1[[1]][1:(lineWithInputObjects - 1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei2', 'numeric', '', ''),
      expectsInput('ei3', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx1[[1]][(lineWithInputObjects + 1):(lineWithOutputObjects - 1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co2', 'numeric', ''),
      createsOutput('co3', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx1[[1]][(lineWithInputObjects + 1):lineWithInit], "
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
      xxx1[[1]][(lineWithInit + 1):lineWithDotInputObjects], "
      a <- sim$b
      sim$g <- 1
      sim$ei1 <- 4
      fff <- sim$ei1
      fff <- sim$co3
      sim$co1 <- 123
      ",
      xxx1[[1]][(lineWithDotInputObjects + 1):length(xxx1[[1]])],
      sep = "\n", fill = FALSE, file = fileNames[1])


  cat(xxx1[[2]][1:(lineWithInputObjects - 1)], "
      expectsInput('ei1', 'numeric', '', ''),
      expectsInput('ei4', 'numeric', '', '')
      ",
      xxx1[[2]][(lineWithInputObjects+1):(lineWithOutputObjects - 1)], "
      createsOutput('co1', 'numeric', ''),
      createsOutput('co4', 'numeric', '')
      ",
      xxx1[[2]][(lineWithInputObjects + 1):lineWithInit], "
      a <- sim$b
      xx <- c(1,2)
      xx[sim$ei4] <- NA
      ",
      xxx1[[2]][(lineWithInit + 1):lineWithDotInputObjects], "
      a <- sim$b
      sim$co1 <- 123
      ",
      xxx1[[2]][(lineWithDotInputObjects + 1):length(xxx1[[2]])],
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

  for (y in 3:4) {
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
      xxx[(lineWithInit + 1):length(xxx)], sep = "\n", fill = FALSE, file = fileName)

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
})

test_that("scheduleEvent with invalid values for eventTime", {
  testInitOut <- testInit()
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)
  s <- simInit(times = list(start = 1, end = 10))
  expect_error({
    s <- scheduleEvent(s, eventTime = -1, eventType = "test1", moduleName = "test")
  })
  expect_warning({
    s <- scheduleEvent(s, eventTime = numeric(), eventType = "test1", moduleName = "test")
  })
  expect_error({
    s <- scheduleEvent(s, eventTime = 0, eventType = "test1", moduleName = "test")
  })
})
