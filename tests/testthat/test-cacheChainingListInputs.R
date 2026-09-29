## .chainExternalInputsMatch() compares an expectsInput the chain did not produce with the digest
## the entry recorded in its `preDigest` tags. Cache() records a list-valued object element by
## element (`sim..list.<object>.<element>`), never under the bare object name, so a module with a
## list-valued input never chained even when nothing had changed.

## `preDigest` tags as Cache() writes them for a simList holding `lst`: one per element, named
## `sim..list.lst.<element>` (`sim..list.lst` for an unnamed element), valued by that element's digest.
tagsFor <- function(hashes, names = names(hashes)) {
  data.frame(tagKey = "preDigest", tagValue = paste0("sim..list.", names, ":", unlist(hashes)))
}
dig <- function(x) .robustDigest(x, length = Inf, algo = "xxhash64", quick = FALSE)

test_that(".chainExternalInputsMatch() matches a list-valued input recorded element by element", {
  testInit("data.table", opts = list())
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  d <- file.path(mp, "lm"); dir.create(d)
  writeLines('
defineModule(sim, list(name = "lm", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(lm = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list("citation.bib"),
  documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = bindrows(expectsInput("lst", "list", "")), outputObjects = bindrows(createsOutput("o", "numeric", ""))))
doEvent.lm <- function(sim, eventTime, eventType, debug = FALSE) return(invisible(sim))
', file.path(d, "lm.R"))
  lst <- list(a = data.table::data.table(x = 1:3), b = data.table::data.table(y = 4:6),
              c = list(z = 1, w = "q"))
  sim <- simInit(modules = "lm", paths = list(modulePath = mp), objects = list(lst = lst))
  tags <- tagsFor(list(dig(lst$a), dig(lst$b), dig(lst$c$z), dig(lst$c$w)),
                  c("lst.a", "lst.b", "lst.c.z", "lst.c.w"))
  ## the tag order is not guaranteed
  shuffled <- tags[c(3, 1, 4, 2), ]

  check <- function(value, postTags = tags) {
    sim$lst <- value
    .chainExternalInputsMatch(sim, "lm", postTags, produced = character(0))
  }
  expect_true(check(lst))
  expect_true(check(lst, shuffled))
  changed <- lst; changed$b <- data.table::data.table(y = 4:7)
  expect_false(check(changed))
  changedNested <- lst; changedNested$c$z <- 2
  expect_false(check(changedNested))
  expect_false(check(lst[c("a", "b")])) # an element removed
  expect_false(check(c(lst, list(d = 1)))) # an element added
  ## recorded under neither form: not recorded
  expect_false(check(lst, tags[0, ]))

  ## unnamed elements are all recorded as `sim..list.lst`: repeated names compare as multisets
  dts <- list(data.table::data.table(x = 1), data.table::data.table(x = 2), data.table::data.table(x = 3))
  rt <- tagsFor(lapply(dts, dig), rep("lst", 3))
  expect_true(check(dts, rt))
  expect_true(check(dts, rt[c(3, 1, 2), ]))
  expect_false(check(dts[1:2], rt))
  expect_false(check(dts[c(1, 2, 2)], rt))
})

mkListMod <- function(mp, name, inObjs, outObjs, initBody) {
  d <- file.path(mp, name)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines(sprintf('
defineModule(sim, list(name = "%s", description = "", keywords = "",
  authors = person(c("A"), "B", email = "a@b.com", role = c("aut", "cre")),
  childModules = character(0), version = list(%s = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)), timeunit = "year", citation = list("citation.bib"),
  documentation = list(), reqdPkgs = list(),
  parameters = rbind(defineParameter(".useCache", "character", NA, NA, NA, "")),
  inputObjects = %s, outputObjects = %s))
doEvent.%s <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType, init = { %s })
  return(invisible(sim))
}
', name, name, inObjs, outObjs, name, initBody), file.path(d, paste0(name, ".R")))
}

test_that("cacheChaining chains over a module with a list-valued input, and not once an element changes", {
  skip_on_cran()
  testInit("data.table", opts = list(reproducible.useMemoise = FALSE, spades.debug = TRUE,
                                     reproducible.verbose = 1))
  mp <- file.path(tmpdir, "mods"); dir.create(mp, showWarnings = FALSE)
  mkListMod(mp, "lA", "bindrows(expectsInput('unsupplied0', 'numeric', ''))",
            "bindrows(createsOutput('a', 'numeric', ''))", "sim$a <- 1")
  mkListMod(mp, "lB", "bindrows(expectsInput('a', 'numeric', ''), expectsInput('lst', 'list', ''))",
            "bindrows(createsOutput('b', 'numeric', ''))",
            "sim$b <- sim$a + sum(vapply(sim$lst, function(x) sum(x$v), numeric(1)))")
  mkListMod(mp, "lC", "bindrows(expectsInput('b', 'numeric', ''))",
            "bindrows(createsOutput('cc', 'numeric', ''))", "sim$cc <- sim$b * 2")
  cp <- file.path(tmpdir, "cache")
  lst <- list(one = data.table::data.table(v = 1:2), two = data.table::data.table(v = 3:4))
  run <- function(lst) {
    withr::local_options(spades.cacheChaining = TRUE)
    msgs <- capture_messages(s <- simInitAndSpades(
      times = list(start = 1, end = 1),
      params = list(lA = list(.useCache = "init"), lB = list(.useCache = "init"),
                    lC = list(.useCache = "init")),
      objects = list(lst = lst), modules = list("lA", "lB", "lC"),
      paths = list(modulePath = mp, cachePath = cp)))
    list(sim = s, msgs = msgs)
  }
  run(lst) # records the chain
  warm <- run(lst)
  expect_false(any(grepl("has changed; not chaining", warm$msgs)))
  jump <- restoredMsgs(warm$msgs)
  expect_length(jump, 1L)
  ## the message lists every restored event, the one it lands on included
  expect_match(jump, "restored 2 events from the cache \\(lB init, lC init\\); continuing with the next scheduled event")
  expect_equal(warm$sim$cc, 2 * (1 + 10))

  lst2 <- lst; lst2$two <- data.table::data.table(v = 30:40)
  changed <- run(lst2)
  expect_true(any(grepl("an input of lB that the chain did not produce has changed; not chaining", changed$msgs)))
  expect_length(restoredMsgs(changed$msgs), 0L)
  expect_equal(changed$sim$cc, 2 * (1 + 3 + sum(30:40)))
})
