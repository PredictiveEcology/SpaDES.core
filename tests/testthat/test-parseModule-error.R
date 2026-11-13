test_that("a defineModule() error is reported as itself, without \"object 'mess' not found\"", {
  testInit(smcc = FALSE)
  newModule("badParam", tmpdir, open = FALSE)
  f <- file.path(tmpdir, "badParam", "badParam.R")
  code <- readLines(f)
  i <- grep('defineParameter(".plots"', code, fixed = TRUE)[1]
  code[i] <- sub('defineParameter(".plots"', 'defineParameter("boom", "numeric", stop("boom in defineModule"), NA, NA, ""),\n    defineParameter(".plots"',
                 code[i], fixed = TRUE)
  writeLines(code, f)
  msgs <- capture.output(type = "message",
                         err <- tryCatch(simInit(modules = "badParam", paths = list(modulePath = tmpdir)),
                                         error = function(e) e))
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "boom in defineModule")
  expect_false(any(grepl("object 'mess' not found", msgs, fixed = TRUE)))
})
