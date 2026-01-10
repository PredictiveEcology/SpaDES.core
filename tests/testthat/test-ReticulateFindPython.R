if (interactive()) library(testthat)

test_that("ReticulateFindPython installs pyenv-win and Python on Windows", {

  skip_if_not(identical(.Platform$OS.type, "windows"))

  testInit(libraries = "reticulate")

  # Skip if Python is already installed
  skip_if_not(is.null(reticulate::virtualenv_starter("3.10")))

  # Install Python
  pyInterp <- ReticulateFindPython(
    version = "3.10", pyenvRoot = tmpdir,
    pyenvOnly = TRUE, useGit = FALSE, prompt = FALSE)

  expect_equal(length(pyInterp), 1)
  expect_true(file.exists(pyInterp))

  # pyenv-win should have been installed in tmpdir
  expect_match(
    normalizePath(pyInterp, winslash = "/"),
    normalizePath(tmpdir,   winslash = "/"),
    fixed = TRUE)

  # Check that re-running returns the same path
  pyInterp2 <- ReticulateFindPython(
    version = "3.10", pyenvRoot = tmpdir,
    pyenvOnly = TRUE, useGit = FALSE, prompt = FALSE)

  expect_identical(pyInterp, pyInterp2)

})

