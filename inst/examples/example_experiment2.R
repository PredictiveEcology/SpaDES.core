\dontrun{

  # Make 3 simLists -- set up scenarios
  endTime <- 5
  tmpdir <- file.path(tempdir(), "testing")
  tmpCache <- file.path(tempdir(), "testingCache")
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

  # Run experiment
  sims <- experiment2(sim1 = mySim1, sim2 = mySim2, sim3 = mySim3,
                      replicates = 3, useCache = FALSE)

  ### If spades already run -- can manually add to a simLists object
  # simsManual <- new("simLists")
  # simsManual$sim1_rep1 <- sims$sim1_rep1

  # Convert to data.table so can do stuff with
  # Just pull out a variable from the simLists -- simplest case
  df1 <- as.data.table(sims, byRep = TRUE, vals = c("nPixelsBurned"))

  measure.cols <- grep("nPixelsBurned", names(df1), value = TRUE)
  df1Short <- data.table::melt(df1, measure.vars = measure.cols, variable.name = "year")
  df1Short[, year := as.numeric(gsub(".*V([[:digit:]])", "\\1", df1Short$year))]
  library(ggplot2)
  p<- ggplot(df1Short, aes(x=year, y=value, group=simList, color=simList)) +
    stat_summary(geom = "point", fun.y = mean) +
    stat_summary(geom = "line", fun.y = mean) +
    stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)

  print(p)

  # A quoted function -- do not prefix objects with 'sim' -- next two lines are identical
  df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = quote(length(caribou$x1))))
  df1 <- as.data.table(sims, byRep = TRUE, vals = list(NCaribou = "length(caribou$x1)"))

  p<- ggplot(df1, aes(x=simList, y=NCaribou, group=simList, color=simList)) +
    stat_summary(geom = "point", fun.y = mean) +
    stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
  print(p)

  # A much more complicated object to calculate -- an estimate of perimeter to area ratio of fires
  library(raster)
  perimToAreaRatioFn <- quote({
    landscape$Fires[landscape$Fires[]==0] <- NA;
    a <- boundaries(landscape$Fires, type = "inner");
    a[landscape$Fires[] > 0 & a[] == 1] <- landscape$Fires[landscape$Fires[] > 0 & a[] == 1];
    peri <- table(a[]);
    area <- table(landscape$Fires[]);
    keep <- match(names(area),names(peri));
    mean(peri[keep]/area)
  })

  df1 <- as.data.table(sims, byRep = TRUE,
                       vals = c(perimToArea = perimToAreaRatioFn,
                                meanFireSize = quote(mean(table(landscape$Fires[])[-1]))),
                       objectsFromOutputs = c("landscape")) # need to get landscape obj from disk
  if (interactive()) {
    # with an unevaluated string
    p <- ggplot(df1, aes(x=saveTime, y=perimToArea, group=simList, color=simList)) +
      stat_summary(geom = "point", fun.y = mean) +
      stat_summary(geom = "line", fun.y = mean) +
      stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2)
    print(p) # may have NAs, giving warning, if perimeter calculation couldn't be completed
  }


} # end /dontrun
