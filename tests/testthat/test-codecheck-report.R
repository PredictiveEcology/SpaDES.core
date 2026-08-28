## .cc_report() turns a findings data.frame into grouped, printed tables and
## returns the findings (with a `group` column) invisibly.

mkFinding <- function(...) SpaDES.core:::.cc_finding(...)
ccReport <- function(...) SpaDES.core:::.cc_report(...)
emptyFindings <- function() SpaDES.core:::.cc_emptyFindings()

## cli does not write these tables to stdout, so expect_output() sees nothing.
## Under testthat cli signals them as message conditions, so capture those.
## Widen cli first so long lines are not wrapped mid-match.
ccOut <- function(...) {
  withr::with_options(list(cli.width = 300),
                      paste(testthat::capture_messages(ccReport(...)),
                            collapse = "\n"))
}

test_that(".cc_report reports clean code when there are no findings", {
  testInit()

  expect_message(ccReport(emptyFindings(), module = "myMod"),
                 "myMod: module code appears clean")
})

test_that(".cc_report falls back to a placeholder when no module name is given", {
  testInit()

  expect_message(ccReport(emptyFindings()), "\\(module\\): module code appears clean")
})

test_that(".cc_report stays silent on clean code when quiet", {
  testInit()

  expect_no_message(ccReport(emptyFindings(), module = "myMod", quiet = TRUE))
})

test_that(".cc_report returns the findings invisibly", {
  testInit()

  f <- emptyFindings()
  expect_invisible(ccReport(f, module = "myMod", quiet = TRUE))
})

test_that(".cc_report tags each finding with its rule group", {
  testInit()

  f <- rbind(
    mkFinding("out_declared_unused", "warning", module = "m", name = "a",
              message = "declared but unused"),
    mkFinding("param_used_undeclared", "note", module = "m", name = "p",
              message = "used but undeclared")
  )

  out <- ccReport(f, module = "m", quiet = TRUE)

  expect_true("group" %in% names(out))
  expect_identical(out$group, c("outputObjects", "parameters"))
})

test_that(".cc_report groups unknown rule ids under 'other'", {
  testInit()

  f <- mkFinding("no_such_rule_id", "note", module = "m", message = "hm")

  out <- ccReport(f, module = "m", quiet = TRUE)

  expect_identical(out$group, "other")
})

test_that(".cc_report prints the module name, message and rule id", {
  testInit()

  f <- mkFinding("out_declared_unused", "warning", module = "myMod", name = "a",
                 message = "declared but never assigned", file = "myMod.R",
                 line = 12L, col = 3L)

  txt <- ccOut(f, module = "myMod")
  expect_match(txt, "myMod")
  expect_match(txt, "declared but never assigned")
  expect_match(txt, "out_declared_unused", fixed = TRUE)
})

test_that(".cc_report prints a file:line:col location when there is one", {
  testInit()

  f <- mkFinding("out_declared_unused", "warning", module = "myMod", name = "a",
                 message = "some problem", file = "myMod.R", line = 12L, col = 3L)

  expect_match(ccOut(f, module = "myMod"), "myMod.R:12:3", fixed = TRUE)
})

test_that(".cc_report omits the location when the line is unknown", {
  testInit()

  f <- mkFinding("out_declared_unused", "warning", module = "myMod", name = "a",
                 message = "some problem")

  expect_false(grepl(":NA:", ccOut(f, module = "myMod"), fixed = TRUE))
})

test_that(".cc_report prints the suggestion when one is supplied", {
  testInit()

  f <- mkFinding("in_no_default", "note", module = "myMod", name = "a",
                 message = "no default", suggestion = "add a default value")

  expect_match(ccOut(f, module = "myMod"), "add a default value")
})

test_that(".cc_report renders every severity tag", {
  testInit()

  for (sev in c("error", "warning", "note", "info")) {
    f <- mkFinding("out_declared_unused", sev, module = "myMod", name = "a",
                   message = paste("a", sev, "finding"))
    expect_match(ccOut(f, module = "myMod"), paste("a", sev, "finding"))
  }
})

test_that(".cc_report collapses findings that differ only by object name", {
  testInit()

  ## same rule, same wording apart from the object's own name -> one header,
  ## then one line per hit
  f <- rbind(
    mkFinding("in_declared_unused", "note", module = "myMod", name = "alpha",
              message = "input alpha is declared but unused",
              suggestion = "remove alpha", file = "myMod.R", line = 3L, col = 1L),
    mkFinding("in_declared_unused", "note", module = "myMod", name = "beta",
              message = "input beta is declared but unused",
              suggestion = "remove beta", file = "myMod.R", line = 9L, col = 1L)
  )

  txt <- ccOut(f, module = "myMod")

  ## the shared header uses the <name> placeholder ...
  expect_match(txt, "<name>", fixed = TRUE)
  ## ... and both individual objects still appear, with their own locations
  expect_match(txt, "alpha")
  expect_match(txt, "beta")
  expect_match(txt, "myMod.R:3:1", fixed = TRUE)
  expect_match(txt, "myMod.R:9:1", fixed = TRUE)
})

test_that(".cc_report separates findings from different groups", {
  testInit()

  f <- rbind(
    mkFinding("out_declared_unused", "warning", module = "myMod", name = "a",
              message = "an output problem"),
    mkFinding("param_used_undeclared", "note", module = "myMod", name = "p",
              message = "a parameter problem")
  )

  txt <- ccOut(f, module = "myMod")

  expect_match(txt, "outputObjects")
  expect_match(txt, "parameters")
  expect_match(txt, "an output problem")
  expect_match(txt, "a parameter problem")
})

test_that(".cc_report uses the findings' own module when none is passed", {
  testInit()

  f <- mkFinding("out_declared_unused", "warning", module = "modFromFinding",
                 name = "a", message = "some problem")

  expect_match(ccOut(f), "modFromFinding")
})
