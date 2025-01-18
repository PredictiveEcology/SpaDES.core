library(testthat)
withr::local_options(spades.debug = FALSE)

## run all tests using different combinations of env vars
if (nzchar(Sys.getenv("NOT_CRAN")) && as.logical(Sys.getenv("NOT_CRAN"))) {
  withr::local_options(spades.useBox = TRUE)
  # Sys.setenv(R_REPRODUCIBLE_USE_DBI = "false")
  test_check("SpaDES.core")

  withr::local_options(spades.useBox = FALSE)
  test_check("SpaDES.core")
} else {
  test_check("SpaDES.core")
}

