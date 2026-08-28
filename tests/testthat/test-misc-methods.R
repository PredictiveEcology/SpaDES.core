## Small helpers in misc-methods.R: classFilter(), rndstr(), the empty
## input/output file tables, moduleCodeFiles() and the defunct updateList().

test_that("classFilter keeps only objects of the included class", {
  testInit()

  e <- new.env()
  e$aNumber <- 1.5
  e$aCharacter <- "x"
  e$aList <- list(1, 2)

  out <- classFilter(ls(e), include = "numeric", exclude = NA_character_, envir = e)

  expect_identical(out, "aNumber")
})

test_that("classFilter treats integers as numeric", {
  testInit()

  e <- new.env()
  e$anInteger <- 1L
  e$aCharacter <- "x"

  out <- classFilter(ls(e), include = "numeric", exclude = NA_character_, envir = e)

  expect_identical(out, "anInteger")
})

test_that("classFilter drops objects of the excluded class", {
  testInit()

  e <- new.env()
  e$aNumber <- 1.5
  e$anInteger <- 1L

  out <- classFilter(ls(e), include = "numeric", exclude = "integer", envir = e)

  expect_identical(out, "aNumber")
})

test_that("classFilter returns nothing when no object matches", {
  testInit()

  e <- new.env()
  e$aCharacter <- "x"

  expect_length(classFilter(ls(e), include = "numeric", exclude = NA_character_,
                            envir = e), 0L)
})

test_that("rndstr produces n strings of the requested length", {
  testInit()

  out <- rndstr(n = 4, len = 6)

  expect_length(out, 4L)
  expect_true(all(nchar(out) == 6L))
  expect_type(out, "character")
})

test_that("rndstr honours characterFirst", {
  testInit()

  set.seed(123)
  withFirst <- rndstr(n = 20, len = 5, characterFirst = TRUE)
  expect_true(all(grepl("^[A-Za-z]", withFirst)))

  ## characterFirst = FALSE may start with a digit or a letter, so just check
  ## the shape holds
  noFirst <- rndstr(n = 20, len = 5, characterFirst = FALSE)
  expect_true(all(nchar(noFirst) == 5L))
})

test_that("rndstr defaults n and len when either is missing", {
  testInit()

  expect_length(rndstr(n = 3), 3L)
  expect_true(all(nchar(rndstr(len = 4)) == 4L))
  expect_length(rndstr(), 1L)
})

test_that("rndstr rejects non-positive n or len", {
  testInit()

  expect_error(rndstr(n = 0, len = 3), "requires n > 0 and len > 0")
  expect_error(rndstr(n = 3, len = 0), "requires n > 0 and len > 0")
})

test_that("rndstr is reproducible under a fixed seed", {
  testInit()

  set.seed(42); a <- rndstr(n = 5, len = 8)
  set.seed(42); b <- rndstr(n = 5, len = 8)

  expect_identical(a, b)
})

test_that(".fileTableIn returns an empty input table with the expected columns", {
  testInit()

  ft <- SpaDES.core:::.fileTableIn()

  expect_s3_class(ft, "data.frame")
  expect_identical(NROW(ft), 0L)
  expect_true(all(c("file", "fun", "package", "objectName", "loadTime", "loaded") %in%
                    names(ft)))
})

test_that(".fileTableOut returns an empty output table with the expected columns", {
  testInit()

  ft <- SpaDES.core:::.fileTableOut()

  expect_s3_class(ft, "data.frame")
  expect_identical(NROW(ft), 0L)
  expect_true(all(c("file", "fun", "package", "objectName", "saveTime", "saved") %in%
                    names(ft)))
})

test_that("the cached empty input table matches a fresh one", {
  testInit()

  expect_identical(SpaDES.core:::.fileTableInCols, colnames(SpaDES.core:::.fileTableIn()))
})

test_that("moduleCodeFiles finds a module's R files", {
  skip_on_cran()
  testInit(sampleModReqdPkgs)

  mp <- getSampleModules(tmpdir)
  files <- SpaDES.core:::moduleCodeFiles(list(modulePath = mp), "randomLandscapes")

  expect_type(files, "character")
  expect_true(length(files) > 0)
  expect_true(any(grepl("randomLandscapes\\.R$", files)))
})

test_that("updateList is defunct in favour of Require::modifyList2", {
  testInit()

  expect_error(SpaDES.core:::updateList(list(a = 1), list(b = 2)), "defunct")
})
