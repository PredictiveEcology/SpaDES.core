## A parent module declares no parameters, inputs or outputs of its own, so
## querying one used to hand back empty tables. It now reports its children's,
## as one named entry per child -- nothing is merged, because children need not
## declare the same columns.

## Build a module tree under `path`: two leaves, a parent over them, and a
## grandparent over that parent.
makeModuleTree <- function(path) {
  for (m in c("a1", "a2"))
    suppressMessages(newModule(m, path, open = FALSE, unitTests = FALSE))
  suppressMessages(newModule("mid", path, open = FALSE, unitTests = FALSE,
                             children = c("a1", "a2"), type = "parent"))
  suppressMessages(newModule("top", path, open = FALSE, unitTests = FALSE,
                             children = "mid", type = "parent"))
  path
}

test_that("moduleMetadata on a parent reports one entry per child", {
  testInit()

  mp <- makeModuleTree(file.path(tempdir(), "parentMeta"))
  p <- moduleParams("mid", mp)

  expect_type(p, "list")
  expect_false(is.data.frame(p))
  expect_setequal(names(p), c("a1", "a2"))
  expect_s3_class(p$a1, "data.frame")
  expect_true(".useCache" %in% p$a1$paramName)
})

test_that("moduleMetadata on a parent reports its childModules", {
  testInit()

  mp <- makeModuleTree(file.path(tempdir(), "parentMeta2"))

  expect_setequal(unlist(moduleMetadata(module = "mid", path = mp)$childModules),
                  c("a1", "a2"))
  expect_identical(unlist(moduleMetadata(module = "top", path = mp)$childModules),
                   "mid")
})

test_that("a grandparent resolves through to the leaf modules", {
  testInit()

  mp <- makeModuleTree(file.path(tempdir(), "parentMeta3"))

  expect_setequal(SpaDES.core:::.leafModules("top", mp), c("a1", "a2"))
  expect_setequal(names(moduleParams("top", mp)), c("a1", "a2"))
  expect_setequal(names(moduleInputs("top", mp)), c("a1", "a2"))
  expect_setequal(names(moduleOutputs("top", mp)), c("a1", "a2"))
  expect_setequal(names(moduleMetadata(module = "top", path = mp)$reqdPkgs),
                  c("a1", "a2"))
})

test_that("a leaf module's metadata is unchanged -- still a data.frame", {
  testInit()

  mp <- makeModuleTree(file.path(tempdir(), "parentMeta5"))

  expect_s3_class(moduleParams("a1", mp), "data.frame")
  expect_s3_class(moduleInputs("a1", mp), "data.frame")
  expect_identical(unlist(moduleMetadata(module = "a1", path = mp)$childModules),
                   character(0))
})

test_that("children with different metadata columns are each kept intact", {
  testInit(sampleModReqdPkgs)

  ## the sample modules are a real parent (SpaDES_sampleModules) whose children
  ## do NOT share columns: fireSpread's outputObjects carries an extra `other`
  path <- getSampleModules(tempdir())
  o <- moduleOutputs("SpaDES_sampleModules", path)

  expect_setequal(names(o), c("caribouMovement", "fireSpread", "randomLandscapes"))
  expect_true("other" %in% names(o$fireSpread))
  expect_false("other" %in% names(o$caribouMovement))
})

test_that("moduleObjects attributes a parent's objects to the child that declares them", {
  testInit(sampleModReqdPkgs)

  path <- getSampleModules(tempdir())
  d <- moduleObjects(path = path, module = dir(path))

  ## the parent itself declares nothing, so it should not appear ...
  expect_false("SpaDES_sampleModules" %in% d$module)
  ## ... and its children should
  expect_true(all(c("caribouMovement", "fireSpread", "randomLandscapes") %in% d$module))

  ## the documented findObjects() example must run
  expect_no_error(fo <- findObjects(path = path, module = dir(path), objects = "caribou"))
  expect_true(all(fo$module == "caribouMovement"))
})

test_that("a cyclic childModules entry does not loop forever", {
  testInit()

  mp <- file.path(tempdir(), "parentCycle")
  for (m in c("c1", "c2"))
    suppressMessages(newModule(m, mp, open = FALSE, unitTests = FALSE,
                               children = character(0)))
  for (pair in list(c("c1", "c2"), c("c2", "c1"))) {
    f <- file.path(mp, pair[1], paste0(pair[1], ".R"))
    txt <- readLines(f)
    txt <- sub("childModules = character\\(0\\)",
               paste0('childModules = "', pair[2], '"'), txt)
    writeLines(txt, f)
  }

  expect_no_error(res <- SpaDES.core:::.leafModules("c1", mp))
  expect_type(res, "character")
})

test_that("a child that cannot be parsed does not break the parent's query", {
  testInit()

  mp <- file.path(tempdir(), "parentBadKid")
  for (m in c("good", "bad"))
    suppressMessages(newModule(m, mp, open = FALSE, unitTests = FALSE))
  suppressMessages(newModule("papa", mp, open = FALSE, unitTests = FALSE,
                             children = c("good", "bad"), type = "parent"))

  ## reproduce the LCC2005 shape: a top-level guard on a package that is absent
  f <- file.path(mp, "bad", "bad.R")
  writeLines(c('stopifnot(packageVersion("aPackageThatIsNotInstalled") >= "1.0.0")',
               readLines(f)), f)

  expect_no_error(p <- moduleParams("papa", mp))
  ## the parseable child is still reported ...
  expect_true("good" %in% names(p))
  expect_s3_class(p$good, "data.frame")
  ## ... and the broken one is simply left out
  expect_false("bad" %in% names(p))
})
