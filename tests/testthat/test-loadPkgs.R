test_that("loadPkgs stops, naming the package, when an installed reqdPkg is not attached", {
  skip_if("package:tools" %in% search(), "tools is already attached")
  ## tools is installed but not attached: what a reqdPkg looks like after require() fails on it,
  ## e.g., its namespace is loaded from another library at another version and cannot be unloaded
  expect_error(stopIfNotAttached(c("tools", "stats"), c(tools = "cannot be unloaded")),
               "tools: cannot be unloaded")
})

test_that("loadPkgs keeps only require()'s warning for a reqdPkg that is not installed", {
  withr::local_options(list(spades.useRequire = FALSE, spades.reqdPkgsAttach = TRUE,
                            spades.reqdPkgsDontLoad = NULL))
  expect_warning(loadPkgs(list("notAnInstalledPkgXyz", "stats")), "notAnInstalledPkgXyz")
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
