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

test_that("dirnamesFromSet returns the rep directory, not a subfolder, when leafOnly is FALSE", {
  ## outputs(sim) lists files in subfolders of a rep directory too
  files <- c("out/rep1/figures/plot.png", "out/rep1/climate/MDC.tif", "out/rep1/burnMap.tif",
             "out/rep2/figures/plot.png")

  expect_identical(dirnamesFromSet(files, set = 1:2, leafOnly = FALSE), c("out/rep1", "out/rep2"))
})

test_that("dirnamesFromSet returns zero-padded folder names as they are on disk", {
  files <- c("/a/rep10/x.tif", "/a/rep02/y.tif", "/a/rep01/x.tif", "/a/rep03/z.tif")

  expect_identical(dirnamesFromSet(files, set = c(1, 2, 10)), c("rep01", "rep02", "rep10"))
})

test_that("dirnamesFromSet lists a padded folder once however many files it holds", {
  files <- c("/a/rep01/x.tif", "/a/rep01/y.tif", "/a/rep02/y.tif", "/a/rep01/z.tif")

  expect_identical(dirnamesFromSet(files, set = 1:2), c("rep01", "rep02"))
})

test_that("dirnamesFromSet keeps padding when files are in subfolders of the rep directory", {
  files <- c("/o/rep01/figures/p.png", "/o/rep01/a.tif", "/o/rep02/climate/c.vrt")

  expect_identical(dirnamesFromSet(files, set = 1:2), c("rep01", "rep02"))
  expect_identical(dirnamesFromSet(files, set = 1:2, leafOnly = FALSE), c("/o/rep01", "/o/rep02"))
})

test_that("dirnamesFromSet keeps whatever padding is on disk", {
  files <- c("/o/rep001/a.tif", "/o/rep2/a.tif")

  expect_identical(dirnamesFromSet(files, set = 1:2), c("rep001", "rep2"))
})

test_that("dirnamesFromSet unpadded output is the same for subfolders as for plain files", {
  files <- c("out/rep2/figures/p.png", "out/rep1/a.tif", "out/rep10/b.tif", "out/rep1/figures/p.png")

  expect_identical(dirnamesFromSet(files, set = 1:10), c("rep1", "rep2", "rep10"))
  expect_identical(dirnamesFromSet(files, set = 1:10, leafOnly = FALSE),
                   c("out/rep1", "out/rep2", "out/rep10"))
})

## bare-number replicate folders (SpaDES.project::pathBuild() with the field label off)

test_that("dirnamesFromSet finds bare-number replicate folders when no prefixed ones exist", {
  files <- c("/o/6.4/NRV/16/burnMap.tif", "/o/6.4/NRV/2/burnMap.tif", "/o/6.4/NRV/10/x.tif")

  expect_identical(dirnamesFromSet(files, set = 1:20), c("2", "10", "16"))
  expect_identical(dirnamesFromSet(files, set = 1:20, leafOnly = FALSE),
                   c("/o/6.4/NRV/2", "/o/6.4/NRV/10", "/o/6.4/NRV/16"))
})

test_that("dirnamesFromSet picks the replicate, not an SSP number above it", {
  files <- c("/mnt/fast/outputs/6.4/NRV/370/16/burnMap_year3020.tif")

  expect_identical(dirnamesFromSet(files, set = 16L), "16")
  expect_identical(dirnamesFromSet(files, set = 16L, leafOnly = FALSE),
                   "/mnt/fast/outputs/6.4/NRV/370/16")
})

test_that("dirnamesFromSet gives the bare replicate folder for files in a numeric subfolder", {
  files <- c("/o/NRV/370/16/2020/x.tif", "/o/NRV/370/16/y.tif")

  expect_identical(dirnamesFromSet(files, set = 1:20), "16")
  expect_identical(dirnamesFromSet(files, set = 1:20, leafOnly = FALSE), "/o/NRV/370/16")
})

test_that("dirnamesFromSet ignores bare-number folders when prefixed ones exist", {
  files <- c("/o/370/rep1/a.tif", "/o/370/rep3/a.tif", "/o/16/b.tif", "/o/2/b.tif")

  expect_identical(dirnamesFromSet(files, set = 1:20), c("rep1", "rep3"))
  expect_identical(dirnamesFromSet(files, set = 1:20, leafOnly = FALSE),
                   c("/o/370/rep1", "/o/370/rep3"))
})

test_that("dirnamesFromSet returns nothing when neither layout matches", {
  expect_identical(dirnamesFromSet(c("/o/a/x.tif", "/o/99/x.tif"), set = 1:3), character(0))
})
