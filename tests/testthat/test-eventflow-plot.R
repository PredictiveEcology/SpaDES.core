test_that("eventDiagram(module =, path =) writes a PNG at the text width and under a page tall", {
  testInit(c("ggplot2", "ragg", "systemfonts", "png"))
  f <- file.path(tmpdir, "flow.png")
  out <- eventDiagram(module = "fireMod", path = dirname(efFixtureFire(tmpdir)), file = f)
  img <- png::readPNG(f)
  expect_equal(ncol(img), round(4.65 * 300), tolerance = 1, scale = 1)
  expect_lt(out$height, 7)
})

test_that("context draws other modules' events; an uninstalled font falls back", {
  testInit(c("ggplot2", "ragg", "systemfonts"))
  withr::local_options(spades.eventFlowFont = "NoSuchFontFamilyXYZ")
  vd <- efFixtureVeg(tmpdir)
  fd <- efFixtureFire(tmpdir)
  out <- eventDiagram(module = "vegMod", path = dirname(vd), file = file.path(tmpdir, "v.png"),
                      context = fd)
  expect_true(file.exists(file.path(tmpdir, "v.png")))
  expect_true("Burn" %in% out$rows$label[out$rows$kind == "context"])
})

test_that("includeModuleEventFlow writes into the staging directory when staged", {
  testInit(c("ggplot2", "ragg", "systemfonts", "knitr"))
  ## setting the old options back would leave the staging directory set, and a later
  ## knit (test-module-template.R) would write its figures there
  oldOut <- knitr::opts_knit$get("output.dir")
  withr::defer(knitr::opts_knit$set(SpaDES.docs.stageDir = NULL, output.dir = oldOut))
  knitr::opts_knit$set(SpaDES.docs.stageDir = "staged/fireMod", output.dir = tmpdir)
  ef <- moduleEventFlow("fireMod", dirname(efFixtureFire(tmpdir)))
  inc <- includeModuleEventFlow(ef, "figures/fireMod_eventFlow.png")
  expect_true(file.exists(file.path(tmpdir, "staged/fireMod/figures/fireMod_eventFlow.png")))
  expect_identical(as.character(inc), "staged/fireMod/figures/fireMod_eventFlow.png")
})

test_that("a failed read becomes a note, not an error", {
  testInit("knitr")
  e <- tryCatch(moduleEventFlow("nope", tmpdir), error = function(e) e)
  expect_match(as.character(includeModuleEventFlow(e, "x.png")), "Event flow not available")
  efModule(tmpdir, "parentMod", code = "")
  expect_match(as.character(includeModuleEventFlow(moduleEventFlow("parentMod", tmpdir), "p.png")),
               "no events")
})

test_that(".ef_font() defaults to Noto Sans and falls back to sans when it is missing", {
  testInit(c("systemfonts"))
  withr::local_options(spades.eventFlowFont = "NoSuchFontFamilyXYZ")
  expect_identical(.ef_font(), "sans")
  withr::local_options(spades.eventFlowFont = NULL)
  if ("Noto Sans" %in% systemfonts::system_fonts()$family) {
    expect_identical(.ef_font(), "Noto Sans")
  } else {
    expect_identical(.ef_font(), "sans")
  }
})

test_that("a note is one line, with its asterisks escaped", {
  testInit("knitr")
  note <- as.character(SpaDES.core:::.ef_note(simpleError("unexpected '*'\n2: a *b\n   ^")))
  expect_identical(note, "*Event flow not available: unexpected '\\*' 2: a \\*b ^*")
})

test_that("includeModuleEventFlow says a module has no events only when it has none", {
  testInit(c("ggplot2", "ragg", "systemfonts", "knitr"))
  ## init schedules nothing and calls no module function
  efModule(tmpdir, "initOnly", code =
    'doEvent.initOnly <- function(sim, eventTime, eventType) {
       switch(eventType, init = { sim$x <- 1 })
       invisible(sim)
     }')
  f <- file.path(tmpdir, "initOnly.png")
  inc <- includeModuleEventFlow(moduleEventFlow("initOnly", tmpdir), f)
  expect_false(grepl("no events", as.character(inc)))
  expect_true(file.exists(f))
  ## .inputObjects is not an event
  efModule(tmpdir, "ioOnly", code = ".inputObjects <- function(sim) sim")
  io <- includeModuleEventFlow(moduleEventFlow("ioOnly", tmpdir), file.path(tmpdir, "io.png"))
  expect_match(as.character(io), "no events")
})
