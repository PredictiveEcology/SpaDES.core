## Some code path in the test suite transiently creates a directory at
## `tests/testthat/NA/file<hex>` (a downstream of path massaging where a
## value resolves to the literal string "NA"; not traced to a single
## offending dir.create() call -- likely a C-level filesystem op in a
## Suggests-stack package). The dirs are always empty by the time the
## test run finishes (`R CMD build` reports them as
## "Removed empty directory"). Wipe them at both ends of the test
## session so they never linger in the working tree.
##
## (Belt-and-braces: tests/testthat/.gitignore already excludes `NA/`
## from being committed.)
unlink("NA", recursive = TRUE, force = TRUE)
withr::defer(unlink("NA", recursive = TRUE, force = TRUE),
             envir = testthat::teardown_env())
