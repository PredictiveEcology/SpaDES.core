## Fixture modules for the event-flow tests. They are written to disk and read
## statically; nothing in them is ever evaluated, so they need not run.

## Write `<dir>/<name>/<name>.R`: a defineModule() call followed by `code`.
## `params`, `inputs` and `outputs` are text spliced into bindrows(...), and
## `timeunit` is the text of the time unit.
efModule <- function(dir, name, params = "", inputs = "", outputs = "", code,
                     timeunit = "\"year\"") {
  modDir <- file.path(dir, name)
  dir.create(modDir, recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "defineModule(sim, list(",
    paste0("  name = \"", name, "\","),
    paste0("  timeunit = ", timeunit, ","),
    paste0("  parameters = bindrows(", params, "),"),
    paste0("  inputObjects = bindrows(", inputs, "),"),
    paste0("  outputObjects = bindrows(", outputs, ")"),
    "))",
    "",
    code
  )
  writeLines(lines, file.path(modDir, paste0(name, ".R")))
  modDir
}

efFixtureFire <- function(dir) {
  efModule(
    dir, "fireMod",
    params = paste(
      "defineParameter(\"mode\", \"character\", \"single\", NA, NA, \"run mode\"),",
      "defineParameter(\"fireTimestep\", \"integer\", 1L, NA, NA, \"fire interval\"),",
      "defineParameter(\"burnInitialTime\", \"numeric\", start(sim, \"year\") + 1, NA, NA, \"first burn\"),",
      "defineParameter(\".saveInitialTime\", \"numeric\", NA, NA, NA, \"first save\")"
    ),
    inputs = paste(
      "expectsInput(\"flammableMap\", \"SpatRaster\", \"flammable pixels\"),",
      "expectsInput(\"fireReturnInterval\", \"SpatRaster\", \"fire return interval\")"
    ),
    outputs = paste(
      "createsOutput(\"burnMap\", \"SpatRaster\", \"burned pixels\"),",
      "createsOutput(\"fireReturnInterval\", \"SpatRaster\", \"fire return interval\"),",
      "createsOutput(\"friSummary\", \"data.table\", \"summary\")"
    ),
    code = c(
      "doEvent.fireMod <- function(sim, eventTime, eventType, priority) {",
      "  if (eventType == \"init\") {",
      "    sim <- Init(sim)",
      "    if (P(sim)$mode == \"single\") {",
      "      sim <- scheduleEvent(sim, P(sim)$burnInitialTime, \"fireMod\", \"Burn\", 2.5)",
      "      sim <- scheduleEvent(sim, end(sim), \"fireMod\", \"summarySingle\")",
      "    } else if (P(sim)$mode == \"multi\") {",
      "      sim <- scheduleEvent(sim, start(sim), \"fireMod\", \"summaryMulti\")",
      "    }",
      "    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, \"fireMod\", \"save\")",
      "  } else if (eventType == \"Burn\") {",
      "    sim <- Burn(sim)",
      "    sim <- scheduleEvent(sim, time(sim) + P(sim)$fireTimestep, \"fireMod\", \"Burn\", 2.5)",
      "  } else if (eventType == \"summarySingle\") {",
      "    sim$friSummary <- summarise(sim$burnMap)",
      "  } else if (eventType == \"summaryMulti\") {",
      "    sim$friSummary <- summarise(sim$burnMap)",
      "  } else if (eventType == \"save\") {",
      "  }",
      "  return(invisible(sim))",
      "}",
      "",
      "Init <- compiler::cmpfun(function(sim) {",
      "  sim$fireReturnInterval <- sim$fireReturnInterval * 1",
      "  sim",
      "})",
      "",
      "Burn <- function(sim) {",
      "  sim$burnMap <- burn(sim$flammableMap, P(sim)$fireTimestep)",
      "  sim",
      "}",
      "",
      ".inputObjects <- function(sim) {",
      "  if (!suppliedElsewhere(\"flammableMap\", sim)) sim$flammableMap <- dummy()",
      "  sim",
      "}"
    )
  )
}

efFixtureVeg <- function(dir) {
  efModule(
    dir, "vegMod",
    params = paste(
      "defineParameter(\"calcSummary\", \"character\", \"end\", NA, NA, \"when to summarise\"),",
      "defineParameter(\"successionTimestep\", \"numeric\", 10, NA, NA, \"succession interval\")"
    ),
    code = c(
      "doEvent.vegMod <- function(sim, eventTime, eventType, priority) {",
      "  dispPriority <- 5",
      "  summPriority <- list(start = 4, end = 8)[P(sim)$calcSummary]",
      "  switch(",
      "    eventType,",
      "    init = {",
      "      if (!is.null(summPriority$start)) {",
      "        sim <- scheduleEvent(sim, start(sim), \"vegMod\", \"summaryStart\", summPriority$start)",
      "      }",
      "      sim <- scheduleEvent(sim, start(sim) + P(sim)$successionTimestep, \"vegMod\", \"Dispersal\", dispPriority)",
      "      if (nrow(sim$cohortData) > 0) {",
      "        sim <- scheduleEvent(sim, start(sim), \"vegMod\", \"Growth\", 6)",
      "      }",
      "    },",
      "    Dispersal = {",
      "      sim <- scheduleEvent(sim, time(sim) + P(sim)$successionTimestep, \"vegMod\", \"Dispersal\", dispPriority)",
      "    },",
      "    Growth = ,",
      "    Grow2 = {",
      "      sim <- scheduleEvent(sim, time(sim) + 1, \"vegMod\", \"Growth\", 6)",
      "    },",
      "    summaryStart = {},",
      "    orphan = {},",
      "    warning(\"unknown\")",
      "  )",
      "  return(invisible(sim))",
      "}"
    )
  )
}

efFixtureEvents <- function(dir) {
  efModule(
    dir, "evMod",
    code = c(
      "doEvent.evMod.init <- function(sim, eventTime, eventType, priority) {",
      "  sim <- scheduleEvent(sim, start(sim) + 1, \"evMod\", \"step\")",
      "  sim",
      "}",
      "",
      "doEvent.evMod.step <- function(sim, eventTime, eventType, priority) {",
      "  sim <- scheduleEvent(sim, time(sim) + 1, \"evMod\", \"step\")",
      "  sim",
      "}"
    )
  )
}
