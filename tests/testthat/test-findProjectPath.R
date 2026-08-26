## findProjectPath() walks up for an RStudio project or a git repository,
## falling back on the starting directory.

test_that("findProjectPath returns a project root from a subdirectory", {
  testInit()

  proj <- checkPath(file.path(tmpdir, "proj"), create = TRUE)
  sub <- checkPath(file.path(proj, "R", "deeper"), create = TRUE)
  writeLines("Version: 1.0", file.path(proj, "proj.Rproj"))

  expect_identical(normPath(findProjectPath(sub)), normPath(proj))
  expect_identical(normPath(findProjectPath(proj)), normPath(proj))
})

test_that("findProjectPath recognises a git repository", {
  testInit()

  proj <- checkPath(file.path(tmpdir, "gitproj"), create = TRUE)
  checkPath(file.path(proj, ".git"), create = TRUE)
  sub <- checkPath(file.path(proj, "a", "b"), create = TRUE)

  expect_identical(normPath(findProjectPath(sub)), normPath(proj))
})

test_that("findProjectPath recognises a git worktree/submodule .git file", {
  testInit()

  proj <- checkPath(file.path(tmpdir, "wt"), create = TRUE)
  writeLines("gitdir: /elsewhere/.git/worktrees/wt", file.path(proj, ".git"))
  sub <- checkPath(file.path(proj, "inner"), create = TRUE)

  expect_identical(normPath(findProjectPath(sub)), normPath(proj))
})

test_that("findProjectPath ignores an .Rproj file that is not one", {
  testInit()

  ## rprojroot requires the first line to be `Version: `
  proj <- checkPath(file.path(tmpdir, "notproj"), create = TRUE)
  writeLines("this is not an Rproj file", file.path(proj, "notproj.Rproj"))
  sub <- checkPath(file.path(proj, "sub"), create = TRUE)

  expect_identical(normPath(findProjectPath(sub)), normPath(sub))
})

test_that("findProjectPath falls back on the starting directory", {
  testInit()

  ## no .Rproj and no .git anywhere above -> the path itself
  bare <- checkPath(file.path(tempdir(), paste0("bare", .rndstr(1))), create = TRUE)
  withr::defer(unlink(bare, recursive = TRUE))

  expect_identical(normPath(findProjectPath(bare)), normPath(bare))
})

test_that("findProjectPath defaults to the working directory", {
  testInit()

  ## testInit() sets the working directory to tmpdir
  expect_identical(normPath(findProjectPath()), normPath(findProjectPath(getwd())))
})

test_that("findProjectPath does not return the start directory unconditionally", {
  testInit()

  ## Regression: the original implementation OR'd rprojroot::from_wd into the
  ## criteria. Its test function is `function(path) TRUE`, so it matched the
  ## starting directory immediately and short-circuited the search -- making
  ## this return getwd() every time and never finding a project root at all.
  skip_if_not_installed("rprojroot")

  proj <- checkPath(file.path(tmpdir, "regress"), create = TRUE)
  writeLines("Version: 1.0", file.path(proj, "regress.Rproj"))
  sub <- checkPath(file.path(proj, "deep", "deeper"), create = TRUE)

  expect_false(identical(normPath(findProjectPath(sub)), normPath(sub)))
  expect_identical(normPath(findProjectPath(sub)), normPath(proj))
})

test_that("findProjectPath works without rprojroot installed", {
  testInit()

  ## rprojroot is in Suggests, so the function must degrade to the fallback
  ## rather than error when it is absent
  proj <- checkPath(file.path(tmpdir, "nopkg"), create = TRUE)
  writeLines("Version: 1.0", file.path(proj, "nopkg.Rproj"))
  sub <- checkPath(file.path(proj, "sub"), create = TRUE)

  ## mock the package's own seam rather than base::requireNamespace(), which
  ## would be replaced process-wide and can take unrelated code with it
  testthat::with_mocked_bindings(
    .hasRprojroot = function() FALSE,
    {
      expect_identical(normPath(findProjectPath(sub)), normPath(sub))
    }
  )
})
