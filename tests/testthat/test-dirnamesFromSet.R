## dirnamesFromSet(): the rep directory names occurring in a set of output
## paths, restricted to the requested ids. Purely textual -- touches no disk.

test_that("dirnamesFromSet returns only ids that occur in the paths", {
  files <- c("out/rep1/burnMap.tif", "out/rep3/burnMap.tif")

  expect_identical(dirnamesFromSet(files, set = 1:4), c("rep1", "rep3"))
})

test_that("dirnamesFromSet drops ids that were not asked for", {
  files <- c("out/rep1/a.tif", "out/rep7/a.tif")

  expect_identical(dirnamesFromSet(files, set = 1:3), "rep1")
})

test_that("dirnamesFromSet sorts numerically, not as character", {
  files <- file.path("out", paste0("rep", c(2, 10, 1)), "a.tif")

  ## character sort would give rep1, rep10, rep2
  expect_identical(dirnamesFromSet(files, set = 1:10), c("rep1", "rep2", "rep10"))
})

test_that("dirnamesFromSet de-duplicates repeated ids", {
  files <- c("out/rep1/a.tif", "out/rep1/b.tif", "out/rep1/c.tif")

  expect_identical(dirnamesFromSet(files, set = 1:2), "rep1")
})

test_that("dirnamesFromSet assumes all ids present when there are no files", {
  expect_identical(dirnamesFromSet(character(0), set = 1:3), c("rep1", "rep2", "rep3"))
  expect_identical(dirnamesFromSet(NULL, set = 1:2), c("rep1", "rep2"))
})

test_that("dirnamesFromSet does not zero-pad the ids it fabricates", {
  ## the directories on disk are rep1 .. rep10, never rep01
  expect_identical(dirnamesFromSet(character(0), set = 1L), "rep1")
})

test_that("dirnamesFromSet ignores non-matching paths without warning", {
  files <- c("out/rep2/a.tif", "out/notARep/a.tif", "somethingElse.tif")

  expect_silent(res <- dirnamesFromSet(files, set = 1:3))
  expect_identical(res, "rep2")
})

test_that("dirnamesFromSet honours a different prefix", {
  files <- c("out/run5/a.tif", "out/run6/a.tif")

  expect_identical(dirnamesFromSet(files, set = 5:6, prefix = "run"), c("run5", "run6"))
})

test_that("dirnamesFromSet returns full directory paths when leafOnly is FALSE", {
  files <- c("out/rep1/burnMap.tif", "out/rep10/burnMap.tif", "out/rep2/x.tif")

  expect_identical(dirnamesFromSet(files, set = 1:10, leafOnly = FALSE),
                   c("out/rep1", "out/rep2", "out/rep10"))
})

test_that("dirnamesFromSet de-duplicates directories when leafOnly is FALSE", {
  files <- c("out/rep1/a.tif", "out/rep1/b.tif")

  expect_identical(dirnamesFromSet(files, set = 1:2, leafOnly = FALSE), "out/rep1")
})

test_that("dirnamesFromSet keeps differing parents distinct when leafOnly is FALSE", {
  files <- c("scenA/rep1/a.tif", "scenB/rep2/a.tif")

  expect_identical(dirnamesFromSet(files, set = 1:2, leafOnly = FALSE),
                   c("scenA/rep1", "scenB/rep2"))
})

test_that("dirnamesFromSet errors on leafOnly = FALSE with no files", {
  expect_error(dirnamesFromSet(character(0), set = 1:2, leafOnly = FALSE),
               "requires `files`")
})
