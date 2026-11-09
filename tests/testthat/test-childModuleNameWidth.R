## Log lines prefix messages with a shortened module name. The width must make the
## children of a parent module distinct too, when only the parent is given to simInit().

test_that("simInit widens the module-name width for the children of a parent module", {
  tmpdir <- withr::local_tempdir()
  mp <- file.path(tmpdir, "modules")
  writeTestModule(mp, "fireSense", children = c("fireSense_ELFs", "fireSense_burn"))
  writeTestModule(mp, "fireSense_ELFs")
  writeTestModule(mp, "fireSense_burn")
  withr::local_options(spades.messagingNumCharsModule = 21)

  sim <- simInit(times = list(start = 0, end = 1), modules = "fireSense",
                 paths = list(modulePath = mp, outputPath = file.path(tmpdir, "out")))

  nc <- getOption("spades.messagingNumCharsModule") - loggingMessagePrefixLength
  kids <- c("fireSense_ELFs", "fireSense_burn")
  stripped <- vapply(kids, moduleNameStripped, character(1), numCharsMax = nc)
  expect_false(anyDuplicated(stripped) > 0)
})
