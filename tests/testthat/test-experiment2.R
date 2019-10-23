test_that("experiment2 does not work correctly", {
  #if (!interactive())
  skip_on_cran()
  skip_on_appveyor()
  testInitOut <- testInit(c("raster", "future.callr", "future", "ggplot2"), smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)


  endTime <- 5
  # Example of changing parameter values
  mySim1 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
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
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  mySim2 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
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
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  mySim3 <- simInit(
    times = list(start = 0.0, end = endTime, timeunit = "year"),
    params = list(
      .globals = list(stackName = "landscape", burnStats = "nPixelsBurned"),
      # Turn off interactive plotting
      fireSpread = list(.plotInitialTime = NA, spreadprob = c(0.2), nFires = c(30)),
      caribouMovement = list(.plotInitialTime = NA),
      randomLandscapes = list(.plotInitialTime = NA, .useCache = "init")
    ),
    modules = list("randomLandscapes", "fireSpread", "caribouMovement"),
    paths = list(modulePath = system.file("sampleModules", package = "SpaDES.core"),
                 outputPath = tmpdir,
                 cachePath = tmpCache),
    # Save final state of landscape and caribou
    outputs = data.frame(objectName = c(rep("landscape", endTime), "caribou", "caribou"),
                         saveTimes = c(seq_len(endTime), unique(c(ceiling(endTime/2),endTime))),
                         stringsAsFactors = FALSE)
  )

  for (pl in c("sequential", "multiprocess", "callr")) {
    cat(" -- testing future plan when", pl, "                ")
    warn <- capture_warnings(plan(pl, workers = 2)) # just about "workers" not defined in "sequential"
    # Test Caching
    cap0 <- capture_output(mess <- capture_messages(spades(Copy(mySim1), debug = 2)))
    expect_true(sum(grepl("cached", cap0))==0)
    cap1 <- capture.output(mess <- capture_messages(sims <- experiment2(mySim1, mySim2)))
    expects <- if (is(plan(), "sequential")) 1 else 2 # sequential has no concurrent spades
    expect_true(sum(grepl("cached", cap1))==expects) # b/c they are at the same time. If sequential, one would be memoised
    cap <- capture.output(mess <- capture_messages(sims <- experiment2(mySim1, mySim2,
                                                                       mySim1)))
    expects <- if (is(plan(), "callr")) c(2,1) else c(0,3) # uses a new session each call
    expect_true(sum(grepl("cached", cap))==expects[1]) # these are not same session as previous, so can't memoise
    expect_true(sum(grepl("memoised", cap))==expects[2]) # 2 were old, plus 1 was a redo in one of the workers


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
  }

  stStart <- list()
  stEnd <- list()
  for (pl in c("sequential")) {
    #for (pl in c("sequential", "multiprocess", "callr")) {
    stStart[[pl]] <- Sys.time()
    cat(" -- testing future plan when", pl, "                ")
    warn <- capture_warnings(plan(pl, workers = 2)) # just about "workers" not defined in "sequential"
    cap1 <- capture.output(mess <- capture_messages(
      sims <- experiment2(sim1 = mySim1, sim2 = mySim2, sim3 = mySim3,
                          replicates = 3, useCache = FALSE)
    ))
    stEnd[[pl]] <- Sys.time()
  }
  lapply(names(stStart), function(x) print(stEnd[[x]] - stStart[[x]]))

  expect_true(is(sims, "simLists"))
  mess <- capture.output(sims)
  expect_true(sum(grepl("3 simLists", mess)) == 1)

  df1 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))))
  df2 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned", NCaribou = "length(caribou$x1)"))
  expect_true(identical(df1, df2))

  df1 <- as.data.table(sims, byRep = TRUE,
                       vals = c("nPixelsBurned", NCaribou = quote(length(caribou$x1))),
                       objectsFromOutputs = c("caribou"))


  df1 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned"))

  measure.cols <- grep("nPixelsBurned", names(df1), value = TRUE)
  df1Short <- data.table::melt(df1, measure.vars = measure.cols,
                               variable.name = "year", variable.factor = FALSE)
  # df1Short[, year := as.numeric(gsub(".*V([[:digit:]])", "\\1", df1Short$year))]
  df1Short[, year := as.numeric(unlist(lapply(strsplit(year, split = "\\.V"), function(x) x[2])))]

  if (interactive()) {
    p<- ggplot(df1Short, aes(x=year, y=value, group=simList, color=simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)

    print(p)
  }
  # with an unevaluated string
  df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = "length(caribou$x1)"))
  caribouColName <- grep("NCaribou", colnames(df1), value = TRUE)
  expect_true(length(caribouColName) == 1)

  if (interactive()) {
    p<- ggplot(df1, aes_string(x="simList", y=caribouColName, group="simList", color="simList")) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)
  }


  df1 <- as.data.table(sims, byRep = TRUE,
                       vals = c(meanFireSize = quote(mean(table(landscape$Fires[])[-1]))),
                       objectsFromOutputs = c("landscape"))
  if (interactive()) {
    # with an unevaluated string
    p<- ggplot(df1, aes(x=simList, y=meanFireSize, group=simList, color=simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)

    p <- ggplot(df1, aes(x=saveTime, y=meanFireSize, group=simList, color=simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p)

  }

  fn <- quote({
    landscape$Fires[landscape$Fires[]==0] <- NA;
    a <- boundaries(landscape$Fires, type = "inner");
    a[landscape$Fires[] > 0 & a[] == 1] <- landscape$Fires[landscape$Fires[] > 0 & a[] == 1];
    peri <- table(a[]);
    area <- table(landscape$Fires[]);
    keep <- match(names(area),names(peri));
    mean(peri[keep]/area)
  })

  df1 <- as.data.table(sims, byRep = TRUE,
                       vals = c(perimToArea = fn,
                                meanFireSize = quote(mean(table(landscape$Fires[])[-1]))),
                       objectsFromOutputs = c("landscape"))
  if (interactive()) {
    # with an unevaluated string
    p <- ggplot(df1, aes(x=saveTime, y=perimToArea, group=simList, color=simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    warn <- capture_warnings(print(p))
  }
})
