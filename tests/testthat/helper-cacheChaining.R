## The "restored ..." message, on one line: the console wraps it, and cli adds a prefix per line.
restoredMsgs <- function(m) {
  x <- grep("cacheChaining: restored", m, value = TRUE)
  trimws(gsub("\\s+", " ", gsub("[^[:alnum:][:space:]]*[[:space:]]*[[:alpha:]]{3}[0-9]+ [0-9:]+ \\S+\\s+:\\S+\\s+", " ", x)))
}

## The events a jump restored, in order, from its numbered list ("  2. jC init  (<cacheId>)").
restoredList <- function(m) {
  m <- unlist(strsplit(m, "\n", fixed = TRUE)) # the numbered list is one message, one line per event
  x <- grep("^\\s*[0-9]+\\. \\S+ \\S+\\s+\\([0-9a-f]+\\)\\s*$", sub("^[[:alpha:]]{3}[0-9]+ [0-9:]+ \\S+\\s+:\\S+\\s+", "", m), value = TRUE)
  trimws(sub("^\\s*[0-9]+\\. (\\S+ \\S+)\\s+\\(.*$", "\\1", x))
}

## Shared by test-cacheChainingJump.R and test-cacheChainingJumpModObjects.R: a module with a cached
## `init` that runs `initBody` and schedules a `grow` event, the options the chaining messages need,
## a test_that() that runs with memoise off and on, and the chaining messages of a run.
## Four modules, each with a cached `init` that writes its own output from the previous module's,
## then schedules a `grow` event (so the queue after the chain is not trivial). modB also reads
## `ext`, an object the user supplies at simInit: the "external input" the jump must re-check.
mkJumpMod <- function(mp, name, inObjs, outObjs, initBody, useCache = c(".inputObjects", "init"),
                      params = 'defineParameter(".useCache", "character", NA, NA, NA, "")') {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  spatialExtent = terra::ext(rep(0, 4)), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list("citation.bib"), documentation = list(),
  reqdPkgs = list(),
  parameters = rbind(%s),
  inputObjects = %s, outputObjects = %s))

doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
    init = { %s; sim <- scheduleEvent(sim, time(sim) + 1, "%s", "grow") },
    grow = { sim$grown_%s <- time(sim) })
  return(invisible(sim))
}

.inputObjects <- function(sim) { return(invisible(sim)) }
', name, name, params, inObjs, outObjs, name, initBody, name, name), file.path(d, paste0(name, ".R")))
  invisible(name)
}

## `spades.debug = TRUE` + `reproducible.verbose = 1` so the chaining messages are emitted
## (as in test-cacheChaining.R's liveness test); `debug` is therefore not passed here.
jumpOpts <- list(reproducible.useMemoise = FALSE, spades.debug = TRUE, reproducible.verbose = 1)

## Every jump test runs with memoise off and on: with it on, the jump's loadFromCache() and the
## plain Cache() hits share one memoise environment, and that once broke a run.
jumpTest <- function(desc, code) {
  code <- substitute(code)
  env <- parent.frame() # the test file's environment, where its fixtures are defined
  for (memo in c(FALSE, TRUE)) {
    body <- bquote({
      jumpOpts <- modifyList(jumpOpts, list(reproducible.useMemoise = .(memo)))
      .(code)
    })
    eval(bquote(test_that(.(paste0(desc, " [memoise ", if (memo) "on" else "off", "]")), .(body))), envir = env)
  }
}

jumpMessages <- function(expr) {
  m <- capture_messages(expr)
  list(jumps = restoredMsgs(m),
       chains = grep("Using cacheChaining", m, value = TRUE),
       raw = m)
}

