test_that("loadPkgs stops, naming the package, when a reqdPkg cannot be attached", {
  withr::local_options(list(spades.useRequire = FALSE, spades.reqdPkgsAttach = TRUE,
                            spades.reqdPkgsDontLoad = NULL))
  expect_error(suppressWarnings(loadPkgs(list("notAnInstalledPkgXyz", "stats"))),
               "notAnInstalledPkgXyz")
})

test_that("loadPkgs attaches attachable reqdPkgs and does nothing extra when attach is FALSE", {
  withr::local_options(list(spades.useRequire = FALSE, spades.reqdPkgsDontLoad = NULL))
  withr::local_options(list(spades.reqdPkgsAttach = TRUE))
  skip_if(paste0("package:tools") %in% search(), "tools is already attached")
  withr::defer(detach("package:tools", unload = FALSE))
  expect_no_error(loadPkgs(list("tools")))
  expect_true("package:tools" %in% search())
  withr::local_options(list(spades.reqdPkgsAttach = FALSE))
  expect_no_error(loadPkgs(list("notAnInstalledPkgXyz")))
})
