## repFiles(): the file of each replicate, matched exactly so rep1 does not pick up rep10's file.

test_that("repFiles takes each replicate's file from `files`, matching directory and name exactly", {
  files <- c("out/rep1/burnMap.tif", "out/rep10/burnMap.tif", "out/rep1/fire.csv", "out/rep10/fire.csv")
  dirs <- dirnamesFromSet(files, set = c(1, 10), leafOnly = FALSE)
  expect_identical(repFiles(dirs, "burnMap.tif", files),
                   c(`1` = "out/rep1/burnMap.tif", `10` = "out/rep10/burnMap.tif"))
  expect_identical(unname(repFiles("out/rep1", "fire.csv", files)), "out/rep1/fire.csv")
})

test_that("repFiles stops when a replicate has no such file, or more than one", {
  files <- c("out/rep1/burnMap.tif", "out/rep2/other.tif")
  expect_error(repFiles(c("out/rep1", "out/rep2"), "burnMap.tif", files), "in out/rep2 in `files`; found 0")
  expect_error(repFiles("out/rep1", "burnMap.tif", c(files, files[1])), "found 2")
})

test_that("repFiles builds the paths from the directories without `files`", {
  expect_identical(repFiles(c(`1` = "out/rep01", `2` = "out/rep02"), "a.tif"),
                   c(`1` = "out/rep01/a.tif", `2` = "out/rep02/a.tif"))
  expect_identical(repFiles(character(0), "a.tif"), character(0))
})

test_that("repFiles matches folders whose path mixes \\ and / separators", {
  ## a Windows tempdir() holds "\" and file.path() appends "/rep1"; dirname() of the files there
  ## gives "/" throughout on Windows, so comparing it with the folder as written matched nothing
  d <- c(`1` = "C:\\tmp\\x/rep1", `10` = "C:\\tmp\\x/rep10")
  files <- c(file.path(d, "burnMap.tif"), file.path(d, "fire.csv"))
  expect_identical(repFiles(d, "burnMap.tif", files),
                   c(`1` = "C:\\tmp\\x/rep1/burnMap.tif", `10` = "C:\\tmp\\x/rep10/burnMap.tif"))
  dirs <- dirnamesFromSet(files, set = c(1, 10), leafOnly = FALSE)
  expect_identical(dirs, d)
  expect_identical(repFiles(dirs, "fire.csv", files),
                   c(`1` = "C:\\tmp\\x/rep1/fire.csv", `10` = "C:\\tmp\\x/rep10/fire.csv"))
})

test_that("repFiles matches the folders dirnamesFromSet finds in backslash-only paths", {
  files <- c("C:\\tmp\\x\\rep1\\burnMap.tif", "C:\\tmp\\x\\rep10\\burnMap.tif", "C:\\tmp\\x\\rep1\\fire.csv")
  dirs <- dirnamesFromSet(files, set = c(1, 10), leafOnly = FALSE)
  expect_identical(repFiles(dirs, "burnMap.tif", files), c(`1` = files[1], `10` = files[2]))
})

test_that("repFiles matches a folder written with a doubled or a trailing slash", {
  files <- c("out/rep1/burnMap.tif", "out/rep2/burnMap.tif", "out//rep3/burnMap.tif")
  expect_identical(repFiles(c(`1` = "out//rep1", `2` = "out/rep2/", `3` = "out/rep3"), "burnMap.tif", files),
                   c(`1` = "out/rep1/burnMap.tif", `2` = "out/rep2/burnMap.tif", `3` = "out//rep3/burnMap.tif"))
})
