test_that("code units list reads, updates, writes and parameters in run order", {
  testInit()
  ef <- moduleEventFlow("fireMod", dirname(efFixtureFire(tmpdir)))
  u <- SpaDES.core:::.ef_codeUnits(ef)
  ids <- vapply(u, `[[`, "", "id")
  expect_identical(ids, c(".inputObjects", "init", "Burn", "summarySingle", "summaryMulti"))
  names(u) <- ids
  expect_identical(u$.inputObjects[c("reads", "writes")],
                   list(reads = character(), writes = "flammableMap"))
  expect_true("fireReturnInterval" %in% intersect(u$init$reads, u$init$writes)) # an input: kept
  expect_identical(u$Burn[c("reads", "writes", "params")],
                   list(reads = "flammableMap", writes = "burnMap", params = "fireTimestep"))
  expect_identical(u$summaryMulti$mode, "multi")
})

test_that("a read of an object the same code first creates is not a read", {
  testInit()
  efModule(tmpdir, "rwMod", outputs = 'createsOutput("tab", "data.frame", "")', code =
    'doEvent.rwMod <- function(sim, eventTime, eventType) {
       switch(eventType, init = { sim$tab <- data.frame(a = 1); sim$tab$b <- sim$tab$a })
       invisible(sim)
     }')
  u <- SpaDES.core:::.ef_codeUnits(moduleEventFlow("rwMod", tmpdir))
  expect_identical(u[[1]][c("reads", "writes")], list(reads = character(), writes = "tab"))
})

test_that("moduleObjects(byEvent = TRUE) lists writers and readers with defaults and modes", {
  testInit()
  mo <- moduleObjects(module = "fireMod", path = dirname(efFixtureFire(tmpdir)), byEvent = TRUE)
  expect_identical(names(mo), c("module", "objectName", "group", "writtenBy", "readBy"))
  row <- function(o) mo[mo$objectName == o, ]
  expect_identical(row("flammableMap")$writtenBy, ".inputObjects (default)")
  expect_identical(row("burnMap")$writtenBy, "Burn (single)")
  expect_identical(row("burnMap")$readBy, "summarySingle (single), summaryMulti (multi)")
  expect_identical(row("fireReturnInterval")$group, "Inputs the module also updates")
})

## a parent module: defineModule() with childModules and no inputObjects or outputObjects
efParent <- function(dir, name, children) {
  modDir <- file.path(dir, name)
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "defineModule(sim, list(",
    paste0("  name = \"", name, "\","),
    "  timeunit = \"year\",",
    paste0("  childModules = c(", paste0("\"", children, "\"", collapse = ", "), ")"),
    "))"
  ), file.path(modDir, paste0(name, ".R")))
  modDir
}

test_that("moduleEventFlow() reads a parent module, with no events", {
  testInit()
  efParent(tmpdir, "parentMod", c("kidA", "kidB"))
  ef <- moduleEventFlow("parentMod", tmpdir)
  expect_length(ef$default$events, 0)
  expect_identical(ef$read$meta$inputs, character())
  expect_identical(ef$read$meta$outputs, character())
})

test_that("moduleObjects(byEvent = TRUE) on a parent returns its children's rows", {
  testInit()
  for (k in c("kidA", "kidB")) {
    efModule(tmpdir, k, outputs = sprintf('createsOutput("%sOut", "data.frame", "")', k), code =
      sprintf('doEvent.%s <- function(sim, eventTime, eventType) {
         switch(eventType, init = { sim$%sOut <- data.frame(a = 1) })
         invisible(sim)
       }', k, k))
  }
  efParent(tmpdir, "parentMod", c("kidA", "kidB"))
  mo <- moduleObjects(module = "parentMod", path = tmpdir, byEvent = TRUE)
  expect_identical(names(mo), c("module", "objectName", "group", "writtenBy", "readBy"))
  expect_identical(mo$module, c("kidA", "kidB"))
  expect_identical(mo$objectName, c("kidAOut", "kidBOut"))

  ## a parent whose children cannot be read gives an empty table with the same columns
  efParent(tmpdir, "lonely", "noSuchKid")
  e <- moduleObjects(module = "lonely", path = tmpdir, byEvent = TRUE)
  expect_identical(nrow(e), 0L)
  expect_identical(names(e), c("module", "objectName", "group", "writtenBy", "readBy"))
})
