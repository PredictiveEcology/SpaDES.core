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
