test_that("downloadModule downloads and unzips a single module", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  library(igraph); on.exit(detach("package:igraph"), add = TRUE)

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    f <- downloadModule(m, tmpdir, quiet = TRUE)[[1]] %>% unlist() %>% basename()

    f_expected <- c("LICENSE", "README.txt", "citation.bib", "CHECKSUMS.txt",
                    "test.R", "test.Rmd")

    expect_gt(length(f), 0)
    expect_gt(length(file.path(tmpdir)), 0)
    expect_gt(length(file.path(tmpdir, m)), 0)
    expect_true(all(f %in% f_expected))
  }
})

test_that("downloadModule downloads and unzips a parent module", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  library(igraph); on.exit(detach("package:igraph"), add = TRUE)

  m <- "LCC2005"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    f <- downloadModule(m, tmpdir, quiet = TRUE)[[1]] %>% unlist() %>% as.character()
    d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

    d_expected <- moduleMetadata(module = "LCC2005", path = tmpdir)$childModules %>%
      c(m, "data", "testthat") %>% sort()

    expect_equal(length(f), 43)
    expect_equal(d, d_expected)
  }
})


test_that("downloadModule can overwrite existing modules", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "LccToBeaconsReclassify"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE)

  original_f <- file.path(tmpdir, m) %>%
    list.files(., full.names = TRUE, pattern = "[.]R$") %>%
    file.info()

  expect_error(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE))

  downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = TRUE)

  new_f <- file.path(tmpdir, m) %>%
    list.files(., full.names = TRUE, pattern = "[.]R$") %>%
    file.info()

  expect_true(original_f$mtime < new_f$mtime)
})

test_that("downloadModule does not fail when data URLs cannot be accessed", {
  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "forestAge"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    f <- downloadModule(m, tmpdir, quiet = TRUE, data = TRUE)[[1]] %>% unlist() %>% as.character()
    d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

    d_expected <- moduleMetadata(module = m, path = tmpdir)$childModules %>%
      c(m, "data", "testthat") %>%
      sort()

    expect_equal(d, d_expected)
  }
})
