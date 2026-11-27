## A module's loadOrder that contradicts an object dependency (B creates an object A expects, but A's loadOrder says it
## goes before B) used to make .depsLoadOrder() drop every module's loadOrder and sort on object dependencies alone.
## Seen in fireSense: fireSense_dataPrepPredict creates ignitionFitRTM as a fallback, fireSense_ignitionFit expects it and
## declares `before = "fireSense_dataPrepPredict"`; Biomass_speciesParameters' `before = "Biomass_core"` was then lost.

test_that("a contradicting loadOrder overrides that object dependency and every other loadOrder is kept", {
  testInit(smcc = FALSE, debug = FALSE, opts = list(spades.moduleCodeChecks = FALSE))
  mod <- function(name, ins = character(), outs = character(), loadOrder = "list()") {
    newModule(name, tmpdir, open = FALSE)
    cat(file = file.path(tmpdir, name, paste0(name, ".R")), sprintf('
defineModule(sim, list(name = "%1$s", description = "", keywords = "", authors = person("a", "b"),
  childModules = character(0), version = list(%1$s = "0.0.1"), timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year", citation = list(), documentation = list(), reqdPkgs = list(), loadOrder = %2$s,
  parameters = rbind(defineParameter(".useCache", "logical", FALSE, NA, NA, "")),
  inputObjects = bindrows(%3$s), outputObjects = bindrows(%4$s)))
doEvent.%1$s <- function(sim, eventTime, eventType, debug = FALSE) sim
', name, loadOrder,
      paste(sprintf('expectsInput("%s", "ANY", "")', ins), collapse = ", "),
      paste(sprintf('createsOutput("%s", "ANY", "")', outs), collapse = ", ")), fill = TRUE)
  }
  mod("modData", outs = "sp")                                     # as Biomass_borealDataPrep
  mod("modCore", ins = "sp", outs = "sp")                         # as Biomass_core: both create sp, so that edge is pruned
  mod("modParam", loadOrder = 'list(after = "modData", before = "modCore")') # as Biomass_speciesParameters: loadOrder only
  mod("modPrep", outs = "rtm")                                    # as fireSense_dataPrepPredict (rtm as a fallback)
  mod("modFit", ins = "rtm", loadOrder = 'list(before = "modPrep")') # as fireSense_ignitionFit

  msgs <- character()
  s <- withCallingHandlers(
    simInit(times = list(start = 0, end = 1), paths = list(modulePath = tmpdir),
            modules = c("modCore", "modParam", "modData", "modPrep", "modFit")),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  o <- unlist(modules(s))
  expect_lt(match("modData", o), match("modParam", o))
  expect_lt(match("modParam", o), match("modCore", o))
  expect_lt(match("modFit", o), match("modPrep", o))
  expect_false(any(grepl("Could not automatically determine module order", msgs)))
  expect_true(any(grepl("loadOrder metadata overrides 1 object dependency: modPrep -> modFit \\(rtm\\)", msgs)))
})
