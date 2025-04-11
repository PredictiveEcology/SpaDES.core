test_that("downloadModule downloads and unzips a single module", {
  skip_on_cran()

  opts <- list(reproducible.inputPaths = NULL)
  if (isWindows()) {
    opts <- append(opts, list(download.file.method = "auto"))
  } else {
    opts <- append(opts, list(download.file.method = "curl", download.file.extra = "-L"))
  }

  testInit(opts = opts, "httr")

  m <- "test"

  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }
  f <- f$value[[1]] |> unlist() |> as.character() |> basename()

  f_expected <- c("LICENSE", "README.md", "citation.bib", "CHECKSUMS.txt",
                  "test.R", "test.Rmd")

  expect_gt(length(f), 0)
  expect_gt(length(file.path(tmpdir)), 0)
  expect_gt(length(file.path(tmpdir, m)), 0)
  expect_true(all(f %in% f_expected))
})

test_that("downloadModule downloads and unzips a parent module", {
  skip_on_cran()

  testInit(c("terra", "httr"), smcc = FALSE)
  m <- "LCC2005"

  ## f <- downloadModule(m, tmpdir, quiet = TRUE)[[1]] |> unlist() |> as.character()
  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE))
  dirTD <- dir(tmpdir, recursive = TRUE)
  gotAllMods <- all(unlist(Map(yy = c("caribouMovementLcc", "cropReprojectLccAge", "fireSpreadLcc",
           "forestAge", "forestSuccessionBeacons", "LCC2005", "LccToBeaconsReclassify"),
         function(yy) any(grepl(yy, x = dirTD)))))
  if (!isTRUE(gotAllMods)) skip("Download didn't work correctly; likely no GITHUB_PAT")
  if (length(dirTD) < 16) skip("Download didn't work correctly; likely no GITHUB_PAT")
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }
  f <- f$value[[1]] |> unlist() |> as.character()

  d <- f |> dirname() |> basename() |> unique() |> sort()

  d_expected <- moduleMetadata(module = "LCC2005", path = tmpdir)$childModules |>
    c(m, "data", "testthat") |> sort()

  valToCompare <- 45 # if (.Platform$OS.type == "unix" || isWindows()) 45 else 43
  expect_equal(length(f), valToCompare)
  expect_equal(d, d_expected)
})

test_that("downloadModule can overwrite existing modules", {
  skip_on_cran()
  opts <- list(reproducible.inputPaths = NULL)
  if (isWindows()) {
    opts <- append(opts, list(download.file.method = "auto"))
  } else {
    opts <- append(opts, list(download.file.method = "curl", download.file.extra = "-L"))
  }
  testInit("httr")

  m <- "LccToBeaconsReclassify"
  tmpdir <- file.path(tempdir(), "modules") |> checkPath(create = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }

  original_f <- file.path(tmpdir, m) |>
    list.files(full.names = TRUE, pattern = "[.]R$") |>
    file.info()

  errs <- capture_error(
    warns <- capture_warnings(
      downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = FALSE))
  )
  if (!is.null(getGitCredsToken())) {
    expect_match(paste(warns, collapse = "_"), all = FALSE, fixed = FALSE, regexp = "not overwriting")
  } else {
    expect_match(paste(errs, collapse = "_"), all = FALSE, fixed = FALSE, regexp = "overwrite is FALSE")
  }




  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = FALSE, overwrite = TRUE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
  }

  new_f <- file.path(tmpdir, m) |>
    list.files(full.names = TRUE, pattern = "[.]R$") |>
    file.info()

  expect_true(original_f$mtime < new_f$mtime)
})

test_that("downloadModule does not fail when data URLs cannot be accessed", {
  skip_on_cran()
  opts <- list(reproducible.inputPaths = NULL, "reproducible.verbose" = TRUE)
  if (isWindows()) {
    opts <- append(opts, list(download.file.method = "auto"))
  } else {
    opts <- append(opts, list(download.file.method = "curl", download.file.extra = "-L"))
  }
  testInit(c("httr2"), opts = opts)

  m <- "test"

  skipMessReGoogledrive <-
    "Need a newer version of reproducible for downloadData for non-googledrive urls"
  if (packageVersion("reproducible") <= "1.2.16")
    skip(skipMessReGoogledrive)
  f <- .tryCatch(downloadModule(m, tmpdir, quiet = TRUE, data = TRUE))
  if (!is.null(f$error)) {
    if (grepl("Forbidden", f$error)) {
      skip("Forbidden HTTP 403 on GitHub during downloadModule")
    }
    if (grepl("no package called", f$error)) {
      skip(skipMessReGoogledrive)
    }
  }
  f <- f[[1]] |> unlist() |> as.character()
  d <- f |> dirname() |> basename() |> unique() |> sort()

  d_expected <- sort(c(m, "data"))

  expect_equal(d, d_expected)
})
