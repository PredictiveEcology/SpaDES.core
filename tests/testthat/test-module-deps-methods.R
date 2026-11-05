test_that("defineModule correctly handles different inputs", {
  testInit("terra", smcc = FALSE)

  tmp <- simInit()
  tmp1 <- simInit()

  # check empty metadata
  x0 <- list()
  expect_warning(defineModule(tmp, x0))
  expect_equivalent(suppressWarnings(defineModule(tmp, x0)),
                   suppressWarnings(defineModule(tmp1, .emptyMetadata())))

  # check each element in metadata
  x1 <- list(
    name = "testModule",
    description = "this is a test.",
    keywords = c("test"),
    childModules = character(),
    authors = c(person(c("Alex", "M"), "Chubaty",
                       email = "alex.chubaty@gmail.com",
                       role = c("aut", "cre"))),
    version = list(testModule = "0.0.0.9000"),
    spatialExtent = terra::ext(rep(0, 4)),
    timeframe = as.POSIXlt(c(NA, NA)),
    timeunit = "year",
    citation = list(),
    documentation = list(),
    reqdPkgs = list("grid", "terra", "sf"),
    parameters = rbind(
      defineParameter("dummyVal", "numeric", 1.0, NA, NA, "vague description
                      with spaces")
    ),
    inputObjects = bindrows(
      expectsInput(objectName = "testInput", objectClass = "list", sourceURL = "", desc = NA_character_),
      expectsInput(objectName = "testInput2", objectClass = "list", sourceURL = "", desc = "another vague
                   description with spaces")
    ),
    outputObjects = bindrows(
      createsOutput(objectName = "testOutput", objectClass = "list", desc = NA_character_),
      createsOutput(objectName = "testOutput", objectClass = "list", desc = "another vague
                    description with spaces for outputs   another space")
    )
  )

  # This is now corrected automatically
  expect_false(any(unlist(lapply(x1, function(v) grepl("  |\n", v)))))
  x1 <- rmExtraSpacesEOLList(x1)
  expect_false(any(unlist(lapply(x1, function(v) grepl("  |\n", v)))))

  ## check name
  x2 <- x1
  x2$name <- list("testModule") # not a character
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check description
  x2 <- x1
  x2$description <- list("this is a test.") # not a character vector
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check keywords
  x2 <- x1
  x2$keywords <- list("test") # not a character vector
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check authors
  x2 <- x1
  x2$authors <- "not a person class"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check version
  x2 <- x1
  x2$version <- "0.0.0.9000"
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check spatialExtent
  x2 <- x1
  x2$spatialExtent <- NA
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check timeframe
  x2 <- x1
  x2$timeframe <- NA
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check timeunit
  x2 <- x1
  x2$timeunit <- NA
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check citation
  x2 <- x1
  x2$citation <- character() # not a list
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check reqdPkgs
  x2 <- x1
  x2$reqdPkgs <- c("grid", "terra", "sf") # not a list
  expect_equivalent(defineModule(tmp, x1), defineModule(tmp1, x2))

  ## check parameters
  x2 <- x1
  x2$parameters <- "not a data.frame"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check inputObjects
  x2 <- x1
  x2$inputObjects <- "not a data.frame"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed

  ## check authors
  x2 <- x1
  x2$outputObjects <- "not a person class"
  expect_true({defineModule(tmp, x2); TRUE}) # if error, then TRUE not eval'ed
})

test_that("depsEdgeList and depsGraph work", {
  testInit(sampleModReqdPkgs)

  times <- list(start = 0.0, end = 10)
  npb <- "nPixelsBurned"
  params <- list(
    .globals = list(burnStats = npb, stackName = "landscape"),
    randomLandscapes = list(.plotInitialTime = NA, .plotInterval = NA),
    caribouMovement = list(.plotInitialTime = NA, .plotInterval = NA),
    fireSpread = list(.plotInitialTime = NA, .plotInterval = NA)
  )
  modules <- list("randomLandscapes", "caribouMovement", "fireSpread")
  paths <- list(modulePath = getSampleModules(tmpdir))

  mySim <- simInit(times, params, modules, paths = paths)

  # depsEdgeList
  el <- depsEdgeList(mySim)
  el_from <- c("_INPUT_", "randomLandscapes", "randomLandscapes",
               "fireSpread", "fireSpread", "fireSpread",
               "caribouMovement")
  el_to <- c("randomLandscapes", "fireSpread", "caribouMovement",
             "fireSpread", "fireSpread", "caribouMovement",
             "caribouMovement")
  el_objName <- c("inRAM", "landscape", "landscape", "landscape",
                  npb, "landscape",
                  "caribou")
  el_objClass <- c("logical", "SpatRaster", "SpatRaster",
                   "SpatRaster", "numeric", "SpatRaster",
                   "SpatVector")

  expect_is(el, "data.table")
  expect_equal(names(el), c("from", "to", "objName", "objClass", "fromOrd", "toOrd"))
  expect_equal(el$from, el_from)
  expect_equal(el$to, el_to)
  expect_equal(el$objName, el_objName)
  expect_equal(el$objClass, el_objClass)

  # .depsPruneEdges
  p <- .depsPruneEdges(el)
  p_from <- c("_INPUT_", "randomLandscapes", "randomLandscapes", "fireSpread")
  p_to <- c("randomLandscapes", "fireSpread", "caribouMovement", "caribouMovement")
  p_objName <- c("inRAM", "landscape", "landscape", "landscape")
  p_objClass <- c("logical", "SpatRaster", "SpatRaster", "SpatRaster")
  fromOrd <- p_from
  toOrd <- p_to
  p_ <- data.table::data.table(
    from = p_from, to = p_to,
    objName = p_objName, objClass = p_objClass,
    fromOrd = replace(p_from, p_from == "_INPUT_", NA), toOrd = p_to
  )

  expect_is(p, "data.table")
  expect_equivalent(p, p_)

  # depsGraph
  expect_is(depsGraph(mySim), "igraph")
})

test_that("3 levels of parent and child modules load and show correctly", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  testInit("terra", smcc = FALSE)

  suppressMessages({
    newModule("grandpar1", tmpdir, type = "parent",
              children = c("child1", "child2", "par1", "par2"), open = FALSE)
    newModule("par1", tmpdir, type = "parent", children = c("child4", "child3"), open = FALSE)
    newModule("par2", tmpdir, type = "parent", children = c("child5", "child6"), open = FALSE)
    newModule("child1", tmpdir, open = FALSE)
    newModule("child2", tmpdir, open = FALSE)
    newModule("child3", tmpdir, open = FALSE)
    newModule("child4", tmpdir, open = FALSE)
    newModule("child5", tmpdir, open = FALSE)
    newModule("child6", tmpdir, open = FALSE)
  })

  fileName <- "child2/child2.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "day"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "child3/child3.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "week"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "child5/child5.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "second"')
  cat(xxx1, file = fileName, sep = "\n")

  fileName <- "par1/par1.R"
  xxx <- readLines(fileName)
  xxx1 <- gsub(xxx, pattern = 'timeunit = "year"', replacement = 'timeunit = "month"')
  cat(xxx1, file = fileName, sep = "\n")

  ## cluster_optimal() needs igraph built with GLPK (not the glpsol program). How many
  ## communities it finds differs by igraph version and platform, so only the structure is tested.
  if (is(try(igraph::cluster_optimal(igraph::make_graph(~ A - B)), silent = TRUE), "try-error"))
    skip("igraph was built without GLPK")
  mySim <- simInit(modules = list("grandpar1"), paths = list(modulePath = tmpdir))
  mg <- moduleGraph(mySim, FALSE)
  expect_true(is(mg, "list"))
  expect_true(is(mg$graph, "igraph"))
  expect_true(is(mg$communities, "communities"))
  expect_setequal(basename(mg$communities$names), c("_INPUT_", "grandpar1", "par1", "par2", paste0("child", 1:6)))
  expect_true(all(lengths(igraph::communities(mg$communities)) > 0))
})

test_that("Test cleaning up of desc in createsOutputs, expectsInputs, defineParameters", {
  testInit("terra", smcc = FALSE)

  aList <- list()
  aList[[1]] <- expectsInput("ROCList", "list", sourceURL = NA,
               "Hi ", "Ho ", "its off
              to work we go", otherCol = "lala")
  aList[[2]] <- createsOutput("ROCList", "list", # sourceURL = NA,
                              "Hi ", "Ho ", "its off
              to work we go", otherCol = "lala")
  aList[[3]] <- defineParameter("ROCList", "list", NA, NA, NA, # sourceURL = NA,
                                "Hi ", "Ho ", "its off
              to work we go", otherCol = "lala")

  tests <- Map(a = aList,
               nam = c("expectsInput", "createsOutput", "defineParameter"),
               function(a, nam) {
                 expect_true(is(a, "data.frame"))
                 cn <- colnames(a)
                 cn <- tolower(gsub("param", "", cn))
                 actuallyIs <- tolower(sort(c("...", cn)))
                 shouldBe <- tolower(sort(c(formalArgs(get(nam)))))
                 if (!grepl("Param", nam))
                   shouldBe <- sort(c(shouldBe, "othercol"))
                 expect_true(identical(actuallyIs, shouldBe))
                 desc <- a[[grep("desc", tolower(colnames(a)))]]
                 expect_false(grepl("  ", desc))
                 expect_false(grepl("\n", desc))
               })
})

test_that("spatialExtent of a module dependency is a live SpatExtent", {
  ## the `.moduleDeps` prototype extent is created at package *build* time, so its external
  ## pointer is dead in the installed package; `defineModule()` must supply its own.
  ## terra >= 1.9-34 errors "external pointer is not valid" on such an object
  testInit(sampleModReqdPkgs, smcc = FALSE)

  mySim <- simInit(times = list(start = 0.0, end = 1.0, timeunit = "year"),
                   modules = list("caribouMovement"),
                   paths = list(modulePath = getSampleModules(tmpdir)))
  ex <- mySim@depends@dependencies$caribouMovement@spatialExtent
  expect_true(isExtents(ex))
  expect_false(identical(format(ex@pntr$.pointer), "<pointer: (nil)>"))
  expect_equal(as.numeric(terra::xmin(ex)), 0)

  ## a module that supplies no extent still gets a live default
  expect_false(isExtents(getClassDef(".moduleDeps", package = "SpaDES.core")@prototype@spatialExtent))
  x <- suppressWarnings(defineModule(simInit(), list(name = "noExtent")))
  ex0 <- x@depends@dependencies[[1]]@spatialExtent
  expect_true(isExtents(ex0))
  expect_false(identical(format(ex0@pntr$.pointer), "<pointer: (nil)>"))
})
