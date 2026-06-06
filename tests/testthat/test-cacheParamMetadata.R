test_that(".inputObjects/event cacheId ignores the parameter-definition table", {
  skip_on_cran()
  ## The parameter *definition* table (defineParameter defaults/min/max/class) is
  ## metadata, not a computation input -- only resolved param *values* should affect
  ## the cacheId. It must not enter the digest, else env-derived defaults or
  ## platform-dependent metadata digesting (row order, string encoding, version skew)
  ## split the cacheId across machines/OSs and break (cloud) cache sharing.
  testInit(opts = list(spades.loadReqdPkgs = FALSE, spades.moduleCodeChecks = FALSE,
                       reproducible.useMemoise = FALSE))
  modDir <- file.path(tmpdir, "m"); dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  cat(file = file.path(modDir, "m.R"), sep = "", '
defineModule(sim, list(name = "m", description = "", keywords = "", authors = person("A","B"),
  childModules = character(0), version = list(m = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(),
  parameters = rbind(
    defineParameter("alpha", "numeric", 1, 0, 10, "a"),
    defineParameter("beta",  "numeric", 2, 0, 10, "b"),
    defineParameter(".useCache", "character", ".inputObjects", NA, NA, "c")),
  inputObjects = bindrows(), outputObjects = bindrows()))
doEvent.m <- function(sim, eventTime, eventType, debug = FALSE) { switch(eventType, init = {}); invisible(sim) }
')
  s <- suppressMessages(simInit(modules = "m", paths = list(modulePath = tmpdir),
                                times = list(start = 0, end = 1)))
  p <- s@depends@dependencies[["m"]]@parameters
  d1 <- reproducible::CacheDigest(s)$outputHash
  ## mutate the parameter-definition table the way platforms differ on it
  ## (row order here; encoding/defaults are analogous) -- cacheId must not move:
  s@depends@dependencies[["m"]]@parameters <- p[rev(seq_len(nrow(p))), ]
  d2 <- reproducible::CacheDigest(s)$outputHash
  expect_identical(d1, d2)
})
