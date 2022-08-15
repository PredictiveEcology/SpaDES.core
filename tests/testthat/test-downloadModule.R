test_that("downloadModule downloads and unzips a single module", {
  skip_on_cran()
  skip_if_not_installed("httr")

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  testInitOut <- testInit("raster", smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  m <- "test"

  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }
  f <- f$value[[1]] %>% unlist() %>% as.character() %>% basename()

  f_expected <- c("LICENSE", "README.md", "citation.bib", "CHECKSUMS.txt",
                  "test.R", "test.Rmd")

  expect_gt(length(f), 0)
  expect_gt(length(file.path(tmpdir)), 0)
  expect_gt(length(file.path(tmpdir, m)), 0)
  expect_true(all(f %in% f_expected))
})

test_that("downloadModule downloads and unzips a parent module", {
  skip_on_cran()
  skip_if_not_installed("httr")

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl")
  }

  testInitOut <- testInit("raster", smcc = FALSE)
  on.exit({
    testOnExit(testInitOut)
  }, add = TRUE)

  m <- "LCC2005"

  ## f <- downloadModule(m, tmpdir, quiet = TRUE)[[1]] %>% unlist() %>% as.character()
  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }
  f <- f$value[[1]] %>% unlist() %>% as.character()

  d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

  d_expected <- moduleMetadata(module = "LCC2005", path = tmpdir)$childModules %>%
    c(m, "data", "testthat") %>% sort()

  valToCompare <- 45 # if (.Platform$OS.type == "unix" || isWindows()) 45 else 43
  expect_equal(length(f), valToCompare)
  expect_equal(d, d_expected)
})

test_that("downloadModule can overwrite existing modules", {
  skip_on_cran()

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "LccToBeaconsReclassify"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }

  original_f <- file.path(tmpdir, m) %>%
    list.files(., full.names = TRUE, pattern = "[.]R$") %>%
    file.info()

  expect_error(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE))


  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = TRUE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }

  new_f <- file.path(tmpdir, m) %>%
    list.files(., full.names = TRUE, pattern = "[.]R$") %>%
    file.info()

  expect_true(original_f$mtime < new_f$mtime)
})

test_that("downloadModule does not fail when data URLs cannot be accessed", {
  skip_on_cran()
  skip_if_not_installed("httr")

  if (identical(Sys.getenv("TRAVIS"), "true") &&
      tolower(Sys.info()[["sysname"]]) == "darwin") skip("On Travis OSX")

  if (Sys.info()["sysname"] == "Windows") {
    options(download.file.method = "auto")
  } else {
    options(download.file.method = "curl", download.file.extra = "-L")
  }

  m <- "test"
  tmpdir <- file.path(tempdir(), "modules") %>% checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  if (paste0(R.version$major, ".", R.version$minor) > "3.4.2") {
    f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = TRUE))
    if (!is.null(f$error)) {
      if (grepl("Forbidden", f$error)) {
        skip("Forbidden HTTP 403 on GitHub during downloadModule")
      }
    }
    f <- f$value[[1]] %>% unlist() %>% as.character()
    d <- f %>% dirname() %>% basename() %>% unique() %>% sort()

    d_expected <- sort(c(m, "data"))

    expect_equal(d, d_expected)
  }
})
