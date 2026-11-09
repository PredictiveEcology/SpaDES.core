test_that("an error in a defineModule parameter default names the module and the real message", {
  tmp <- withr::local_tempdir()
  newModule("badDefault", tmp, open = FALSE)
  f <- file.path(tmp, "badDefault", "badDefault.R")
  txt <- readLines(f)
  i <- grep("defineParameter\\(\".plotInitialTime\"", txt)[1]
  txt[i] <- sub("defineParameter\\(\".plotInitialTime\", \"numeric\", [^,]+,",
                "defineParameter(\".plotInitialTime\", \"numeric\", stop(\"boom\"),", txt[i])
  writeLines(txt, f)
  err <- tryCatch(
    suppressWarnings(simInit(modules = "badDefault", paths = list(modulePath = tmp))),
    error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "badDefault")
  expect_match(err, "boom")
  expect_no_match(err, "out@depends|no applicable method")
})
