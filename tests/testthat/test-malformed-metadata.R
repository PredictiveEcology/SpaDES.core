## Tests for the malformed-metadata detector registry (R/module-malformed.R).
##
## Each fixture in tests/testthat/fixtures/malformed/<name>/<name>.R is a
## deliberately-broken (or, for the `clean` fixture, well-formed) module.
## We confirm that `checkModuleMetadata()` returns the right finding(s) and
## that `.checkMalformedMetadata()` throws the same.

fixtureFile <- function(name) {
  testthat::test_path("fixtures", "malformed", name, paste0(name, ".R"))
}

test_that("clean fixture: no findings, no error", {
  f <- fixtureFile("clean")
  expect_equal(length(checkModuleMetadata(f, stopOnFirst = FALSE)), 0L)
  expect_silent(SpaDES.core:::.checkMalformedMetadata(f))
})

test_that("missingComma: detected as a missing trailing comma between metadata rows", {
  f <- fixtureFile("missingComma")
  findings <- checkModuleMetadata(f, stopOnFirst = FALSE)
  expect_true(any(grepl("missing_comma_between_metadata_rows", findings)))
  expect_true(any(grepl("missing a trailing comma|missing.*comma", findings)))
  expect_error(SpaDES.core:::.checkMalformedMetadata(f),
               "trailing comma|missing.*comma")
})

test_that("trailingComma: detected as a trailing comma in defineModule()", {
  f <- fixtureFile("trailingComma")
  findings <- checkModuleMetadata(f, stopOnFirst = FALSE)
  ## Either the trailing-comma detector OR the missing-comma one may
  ## fire first depending on how R reports the parse error -- accept either,
  ## but require at least one finding.
  expect_gt(length(findings), 0L)
  expect_error(SpaDES.core:::.checkMalformedMetadata(f))
})

test_that("unquotedParam: bare symbol caught as a non-string parameter name", {
  f <- fixtureFile("unquotedParam")
  findings <- checkModuleMetadata(f, stopOnFirst = FALSE)
  expect_true(any(grepl("param_name_not_quoted", findings)))
  expect_true(any(grepl("alpha", findings)))
  expect_error(SpaDES.core:::.checkMalformedMetadata(f),
               "param_name_not_quoted")
})

test_that("unquotedObjName: bare symbol caught as a non-string objectName", {
  f <- fixtureFile("unquotedObjName")
  findings <- checkModuleMetadata(f, stopOnFirst = FALSE)
  expect_true(any(grepl("obj_name_not_quoted", findings)))
  expect_true(any(grepl("myInput", findings)))
  expect_error(SpaDES.core:::.checkMalformedMetadata(f),
               "obj_name_not_quoted")
})

test_that("registry can be extended with a new detector", {
  ## Adding a fake detector to the registry should make it fire (verifies
  ## the modular pattern: no surgery beyond appending to the list).
  newId <- "fake_always_fires"
  saved <- SpaDES.core:::.CC_MALFORMED_CHECKS
  on.exit(assignInNamespace(".CC_MALFORMED_CHECKS", saved, ns = "SpaDES.core"),
          add = TRUE)
  extended <- saved
  extended[[newId]] <- function(input) "synthetic-always-fires"
  assignInNamespace(".CC_MALFORMED_CHECKS", extended, ns = "SpaDES.core")

  f <- fixtureFile("clean")
  findings <- checkModuleMetadata(f, stopOnFirst = FALSE)
  expect_true(any(grepl(newId, findings)))
  expect_true(any(grepl("synthetic-always-fires", findings)))
})
