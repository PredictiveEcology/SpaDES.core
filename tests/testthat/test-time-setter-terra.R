test_that("`time<-` works after terra is attached", {
  skip_if_not_installed("terra")
  # terra defines its own `time<-` generic; SpaDES.core's method must live on that generic
  if (!"package:terra" %in% search()) {
    suppressPackageStartupMessages(library("terra", character.only = TRUE))
    on.exit(detach("package:terra", unload = FALSE), add = TRUE)
  }
  sim <- simInit(times = list(start = 0, end = 10))
  time(sim) <- 3
  expect_equal(as.numeric(time(sim)), 3)
  # same generic from either package
  expect_identical(get("time<-", asNamespace("SpaDES.core")), get("time<-", asNamespace("terra")))
})
